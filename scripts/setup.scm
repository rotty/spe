#!r6rs
(import (rnrs base)
        (rnrs unicode)
        (rnrs lists)
        (rnrs control)
        (rnrs exceptions)
        (rnrs conditions)
        (rnrs io simple)
        (rnrs io ports)
        (rnrs programs)
        (rnrs eval)
        (spe setup utils))

(define *silent* #f)

(define *implementations* '(ikarus mzscheme larceny ypsilon))

;; entry point
(define (main args)
  (let ((action (string->symbol (cadr args)))
        (impl (string->symbol (caddr args))))
    (case action
      ((compile)
       (run impl
            (make-library-compiler impl
                                   (cadddr args)
                                   `((rnrs base)
                                     (spe setup ,(symbol-append impl '- 'compiler))))
            (lambda (sys-path lib-name filespec) #f)))
      ((symlinks)
       (when (eq? impl 'larceny)
         (set! *silent* #t))  ;; larceny has (current-error-port) returning stdout!!
       (run impl (make-library-symlink-lister impl) include-file-symlink-lister))
      (else (error #f "invalid action" action)))))

;; Read in a list of .sls files from stdin and call `process-library'
;; for each of them, filtering out implementation-specific files that
;; are not for the provided implementation.
(define (run implementation library-action include-action)
  (for-each (lambda (filename)
              (let ((parts (string-split filename "/")))
                (process-library (string-append (car parts) "/" (cadr parts))
                                 (string-join (cddr parts) "/")
                                 library-action
                                 include-action)))
            (filter-libfilenames implementation (read-lines (current-input-port)))))

(define (filter-libfilenames implementation filenames)
  (define (impl-suffix impl)
    (string-append "." (symbol->string impl) ".sls"))
  (let ((our-suffix (impl-suffix implementation)))
    (let loop ((result '()) (fnames filenames))
      (if (null? fnames)
          result ; order doesn't matter
          (cond ((string-suffix? (car fnames) our-suffix)
                 (loop (cons (car fnames) result) (cdr fnames)))
                ((exists (lambda (impl)
                           (string-suffix? (car fnames) (impl-suffix impl)))
                         *implementations*) ; other implementation?
                 (loop result (cdr fnames))) ; -> ignore
                ((member (string-append
                          (substring (car fnames) 0 (- (string-length (car fnames)) 4))
                          our-suffix)
                         filenames) ; implementation specific file in input?
                 (loop result (cdr fnames))) ; -> ignore
                (else
                 (loop (cons (car fnames) result) (cdr fnames))))))))

;;
;; implementation-specifics
;;
(define (libname-converter impl)
  (case impl
    ((larceny) values)
    ((ikarus ypsilon) (lambda (libname) (escape-libname libname values)))
    ((mzscheme) pltify-libname)
    (else
     (error 'libname-converter "not implemented for this implementation" impl))))

(define (make-library-symlink-lister impl)
  (let ((conv (libname-converter impl)))
    (lambda (sys-path filename libname form)
      (let ((libname (conv libname)))
        (println (make-link-target sys-path (length (filter symbol? libname)) filename)
                 " "
                 (libname->path libname))))))

(define (escape-symbol symbol maybe-upcase)
  (string->symbol
   (apply string-append (map (lambda (c)
                               (case c
                                 ((#\*) (string #\% #\2 (maybe-upcase #\a)))
                                 (else (string c))))
                             (string->list (symbol->string symbol))))))

(define (escape-libname libname maybe-upcase)
  (map (lambda (part)
         (cond ((symbol? part) (escape-symbol part maybe-upcase))
               (else part)))
       libname))

(define (pltify-libname libname)
  (escape-libname (if (= (length libname) 1)
                      (append libname '(main))
                      libname)
                  values))

(define (basename path)
  (cond ((string-index-right path #\/)
         => (lambda (i)
              (substring path (+ i 1) (string-length path))))
        (else
         path)))

(define (dirname path)
  (cond ((string-index-right path #\/)
         => (lambda (i)
              (substring path 0 i)))
        (else
         path)))

(define (string-index-right s c)
  (let loop ((i (- (string-length s) 1)))
    (cond ((<= i 0)
           #f)
          ((char=? c (string-ref s i))
           i)
          (else
           (loop (- i 1))))))
;;
;; actions and action constructors
;;

(define (include-file-symlink-lister sys-path lib-name form)
  (let ((lib-dir (dirname sys-path)))
    
    (define (output ddepth target linkname)
      (println (make-link-target lib-dir (+ ddepth 1) target) " " linkname))
  
    (case (car form)
      ((include-file)
       (for-each (lambda (filespec)
                   (let ((filename (filespec->path filespec ".scm")))
                     (output (filespec-ddepth filespec)
                             filename
                             filename)))
                 (cdr form)))
      ((include/resolve)
       (output (filespec-ddepth (cdr form))
               (resolvespec->path (cdr form))
               (resolvespec->path (cdr form)))))))


(define (make-library-compiler impl target-dir import-specs)
  (let ((compiled-libraries '())
        (env (apply environment import-specs))
        (conv (libname-converter impl)))
    (lambda (sys-path filename lib-name form)
      (let ((import-form (cadddr form)))
        (define (compile-lib! lib-name imported-libs)
          (cond ((not (member lib-name compiled-libraries))
                 (for-each
                  (lambda (lib-name)
                    (let ((import-form
                           (cadddr
                            (call-with-input-file
                                (string-append target-dir "/" (libname->path (conv lib-name)))
                              read))))
                      (compile-lib! lib-name (extract-imported-libs import-form))))
                  imported-libs)

                 (message "compiling " lib-name)
                 (eval `(with-working-directory ,target-dir
                          (compile-library ',lib-name))
                       env)
                 (set! compiled-libraries (cons lib-name compiled-libraries)))))
        (compile-lib! lib-name (extract-imported-libs import-form))))))

(define (read-library port)
  (let ((first-line (get-line port)))
    (cond
     ((string-prefix? first-line ";;;(library ")
      (read (open-string-input-port
             (substring first-line 3 (string-length first-line)))))
     (else
      (set-port-position! port 0)
      (read port)))))

(define (process-library sys-path filename library-action include-action)
  (let ((form (guard (c
                      ((error? c)
                       (message "error while processing library in " sys-path "/" filename ":")
                       (if (message-condition? c)
                           (message (condition-message c))
                           (message "no error message available"))
                       'error))
                (call-with-input-file (string-append sys-path "/" filename)
                  read-library))))
    (cond
     ((eq? form 'error)
      #f)
     (else
      (let ((lib-name (cadr form))
            (include-forms (filter (lambda (form)
                                     (memq (car form) '(include-file include/resolve)))
                                   (cddddr form))))
        (library-action sys-path filename lib-name form)
        (for-each (lambda (iform)
                    (include-action sys-path lib-name iform))
                  include-forms))))))

;;
;; helpers
;;

(define (extract-imported-libs import-form)
  (define (import-set-lib is)
    (cond ((pair? is)
           (case (car is)
             ((library) (cadr is))
             ((only except prefix rename) (import-set-lib (cadr is)))
             (else is)))))
  (filter
   (lambda (lib-name)
     (memq (car lib-name) '(xitomatl spells sxml texinfo stexidoc)))
   (map (lambda (import-spec)
          (cond ((and (pair? import-spec)
                      (eq? (car import-spec) 'for))
                 (import-set-lib (cadr import-spec)))
                (else (import-set-lib import-spec))))
        (cdr import-form))))

(define (make-link-target sys-path n filename)
  (if (= n 0)
      (string-append "../"  sys-path "/" filename)
      (string-append "../"  (string-join (make-list n "..") "/")  "/" sys-path "/" filename)))

(define (make-list len . maybe-elt)
  (let ((elt (cond ((null? maybe-elt) #f) ; Default value
		   ((null? (cdr maybe-elt)) (car maybe-elt))
		   (else (error 'make-list "Too many arguments"
				(cons len maybe-elt))))))
    (do ((i len (- i 1))
	 (ans '() (cons elt ans)))
	((<= i 0) ans))))

(define (resolvespec->path spec)
  (let ((dirname (string-join (car spec) "/")))
    (if (= (string-length dirname) 0)
        (cadr spec)
        (string-append dirname "/" (cadr spec)))))

(define (filespec->path name ext)
  (cond ((symbol? name) (string-append (symbol->string name) ext))
        ((pair? name)
         (string-append
          (if (pair? (car name))
              (string-join (map symbol->string (car name)) "/")
              (symbol->string (car name)))
          "/"
          (symbol->string (cadr name))
          ext))
        ((string? name) name)
        (else (error 'filespec->path "invalid filespec"))))

(define (string-count s c)
  (do ((i 0 (+ i 1))
       (count 0 (+ count (if (char=? c (string-ref s i)) 1 0))))
      ((>= i (string-length s)) count)))

(define (filespec-ddepth filespec)
  (cond ((symbol? filespec)
         0)
        ((pair? filespec)
         (if (pair? (car filespec))
             (length (car filespec))
             1))
        ((string? filespec)
         (string-count filespec #\/))
        (else
         (error 'filespec-ddepth "invalid filespec" filespec))))

(define (symbol-append . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define (string->forms s)
  (let ((port (open-string-input-port s)))
    (let loop ((forms '()))
      (let ((form (read port)))
        (if (eof-object? form)
            (reverse forms)
            (loop (cons form forms)))))))

(define (read-lines port)
  (let loop ((result '()))
    (let ((line (get-line (current-input-port))))
      (if (eof-object? line)
          (reverse result)
          (loop (cons line result))))))

(define (println . args)
  (for-each display args) (newline))

(define (message . args)
  (unless *silent*
    (for-each (lambda (arg) (display arg (current-error-port))) args)
    (newline (current-error-port))))

(main (command-line))
