#!r6rs
(import (rnrs base)
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

;; entry point
(define (main args)
  (define (run library-action include-action)
    (for-each (lambda (lst)
                (process-library (car lst) (cadr lst) library-action include-action))
              (read)))
  (let ((action (string->symbol (cadr args)))
        (impl (string->symbol (caddr args))))
    (case action
      ((compile)
       (run (make-library-compiler (cadddr args)
                                             `((rnrs base)
                                               (spe setup ,(symbol-append impl '- 'compiler))))
                      (lambda (sys-path lib-name filespec) #f)))
      ((symlinks)
       (when (eq? impl 'larceny)
         (set! *silent* #t))  ;; larceny has (current-error-port) returning stdout!!
       (run (make-library-symlink-lister impl) include-file-symlink-lister))
      (else (error #f "invalid action" action)))))

;;
;; implementation-specifics
;; 
(define (make-library-symlink-lister impl)
  (case impl
    ((ikarus larceny)
     (lambda (sys-path filename lib-name form)
       (println (make-link-target sys-path (length (filter symbol? lib-name)) filename)
                " "
                (libname->path lib-name))))
    ((mzscheme)
     (lambda (sys-path filename lib-name form)
       (let ((lib-name (if (= (length lib-name) 1)
                           (append lib-name '(main))
                           lib-name)))
         (println (make-link-target sys-path (length (filter symbol? lib-name)) filename)
                  " "
                  (libname->path lib-name)))))
    (else
     (error #f "unsupported implementation"))))

;;
;; actions and action constructors
;;

(define (include-file-symlink-lister sys-path lib-name form)
  (define (output ddepth target linkname)
    (println (make-link-target sys-path (+ ddepth 1) target) " " linkname))
  
  (case (car form)
    ((include)
     (for-each (lambda (filespec)
                 (let ((filename (filespec->path filespec ".scm")))
                   (output (filespec-ddepth filespec) filename filename)))
               (cdr form)))
    ((include/resolve)
     (output (filespec-ddepth (cdr form))
             (resolvespec->path (cons (cdadr form) (cddr form)))
             (resolvespec->path (cdr form))))))


(define (make-library-compiler target-dir import-specs)
  (let ((compiled-libraries '())
        (env (apply environment import-specs)))
    (lambda (sys-path filename lib-name form)
      (let ((import-form (cadddr form)))
        (define (compile-lib! lib-name imported-libs)
          (cond ((not (member lib-name compiled-libraries))
                 (for-each
                  (lambda (lib-name)
                    (let ((import-form (cadddr
                                        (call-with-input-file
                                            (string-append target-dir "/" (libname->path lib-name))
                                          read))))
                      (compile-lib! lib-name (extract-imported-libs import-form))))
                  imported-libs)

                 (message "compiling " lib-name)
                 (eval `(with-working-directory ,target-dir
                          (compile-library ',lib-name))
                       env)
                 (set! compiled-libraries (cons lib-name compiled-libraries)))))
        (compile-lib! lib-name (extract-imported-libs import-form))))))

(define (process-library sys-path filename library-action include-action)
  (let ((form (guard (c
                      ((error? c)
                       (message "error while processing library in " filename ":")
                       (if (message-condition? c)
                           (message (condition-message c))
                           (message "no error message available"))
                       'error))
                (call-with-input-file (string-append sys-path "/" filename) read))))
    (unless (eq? form 'error)
      (let ((lib-name (cadr form))
            (include-forms (filter (lambda (form)
                                     (memq (car form) '(include include/resolve)))
                                   (cddddr form))))
        (library-action sys-path filename lib-name form)
        (for-each (lambda (iform)
                    (include-action sys-path lib-name iform))
                  include-forms)))))

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

(define (println . args)
  (for-each display args) (newline))

(define (message . args)
  (unless *silent*
    (for-each (lambda (arg) (display arg (current-error-port))) args)
    (newline (current-error-port))))

(main (command-line))
