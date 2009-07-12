#!r6rs
(import (rnrs))

;; entry point
(define (main args)
  (let ((action (string->symbol (cadr args))))
    (case action
      ((symlinks)
       (for-each system-symlink-lister
                 (read-lines (current-input-port))))
      (else
       (error #f "invalid action" action)))))

(define (system-symlink-lister sys-path)
  (let ((sys-def-fname (string-append sys-path "/sys-def.scm")))
    (if (file-exists? sys-def-fname)
        (let* ((sys-def (call-with-input-file sys-def-fname read))
               (r6rs-libs (and (pair? sys-def)
                               (eq? (car sys-def) 'define-system)
                               (assq 'r6rs-libraries (cddr sys-def)))))
          (if r6rs-libs
              (print-system-links sys-path (cdr r6rs-libs))
              (print-default-system-links sys-path)))
        (print-default-system-links sys-path))))

(define (print-default-system-links sys-path)
  (println sys-path " " (basename sys-path)))

(define (print-system-links sys-path specs)
  (for-each
   (lambda (spec)
     (cond ((or (string? spec) (symbol? spec))
            (print-link sys-path spec spec))
           ((pair? spec)
            (print-link sys-path (car spec) (cdr spec)))
           (else
            (error 'print-system-links "invalid spec" spec))))
   specs))

(define (filespec->path name)
  (cond ((symbol? name) (symbol->string name))
        ((pair? name)
         (string-join (map ->string name) "/"))
        ((string? name) name)
        (else (error 'filespec->path "invalid filespec"))))

(define (filespec-ddepth filespec)
  (cond ((symbol? filespec)
         0)
        ((pair? filespec)
         (- (length filespec) 1))
        ((string? filespec)
         (string-count filespec #\/))
        (else
         (error 'filespec-ddepth "invalid filespec" filespec))))

(define (print-link sys-path target name)
  (let ((n-up (filespec-ddepth name)))
    (if (= n-up 0)
        (println sys-path "/" (filespec->path target)
                 " "
                 (filespec->path name))
        (println (string-join (make-list n-up "..") "/")
                 "/" sys-path "/" (filespec->path target)
                 " "
                 (filespec->path name)))))


;;; Utilities

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

(define (make-list len . maybe-elt)
  (let ((elt (cond ((null? maybe-elt) #f) ; Default value
		   ((null? (cdr maybe-elt)) (car maybe-elt))
		   (else (error 'make-list "Too many arguments"
				(cons len maybe-elt))))))
    (do ((i len (- i 1))
	 (ans '() (cons elt ans)))
	((<= i 0) ans))))

(define (string-count s c)
  (do ((i 0 (+ i 1))
       (count 0 (+ count (if (char=? c (string-ref s i)) 1 0))))
      ((>= i (string-length s)) count)))

(define (->string x)
  (cond ((symbol? x) (symbol->string x))
        ((string? x) x)
        (else        (error '->string "cannot coerce" x))))

(define (string-index-right s c)
  (let loop ((i (- (string-length s) 1)))
    (cond ((<= i 0)
           #f)
          ((char=? c (string-ref s i))
           i)
          (else
           (loop (- i 1))))))

(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ((result '()) (lst lst))
        (if (null? lst)
            (apply string-append (cdr (reverse result)))
            (loop (cons (car lst) (cons sep result))
                  (cdr lst))))))

(define (read-lines port)
  (let loop ((result '()))
    (let ((line (get-line (current-input-port))))
      (if (eof-object? line)
          (reverse result)
          (loop (cons line result))))))

(define (println . args)
  (for-each display args) (newline))


;;; Go!
(main (command-line))
