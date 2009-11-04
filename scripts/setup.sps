#!r6rs
(import (rnrs))

;; entry point
(define (main args)
  (let ((action (string->symbol (cadr args))))
    (case action
      ((symlinks)
       (for-each package-symlink-lister
                 (read-lines (current-input-port))))
      (else
       (error #f "invalid action" action)))))

(define (package-symlink-lister pkg-path)
  (let ((pkg-def-fname (string-append pkg-path "/pkg-list.scm")))
    (if (file-exists? pkg-def-fname)
        (let* ((package-form (call-with-input-file pkg-def-fname read))
               (libraries (and (pair? package-form)
                               (eq? (car package-form) 'package)
                               (assq 'libraries (cddr package-form)))))
          (if libraries
              (print-package-links pkg-path (cdr libraries))
              (print-default-package-links pkg-path)))
        (print-default-package-links pkg-path))))

(define (print-default-package-links pkg-path)
  (print-link pkg-path " " (basename pkg-path)))

(define (print-package-links pkg-path specs)
  (for-each
   (lambda (spec)
     (cond ((and (list? spec)
                 (= 3 (length spec))
                 (eq? '-> (cadr spec)))
            (print-link pkg-path (car spec) (caddr spec)))
           (else
            (print-link pkg-path spec spec))))
   specs))

(define (filespec->path spec)
  (cond ((string? spec)
         (list spec))
        (else
         (let loop ((spec spec)
                    (processed '()))
           (cond ((null? spec)
                  (reverse processed))
                 ((symbol? spec)
                  (loop '() processed))
                 ((and (pair? spec)
                       (string? (car spec)))
                  (loop (cdr spec)
                        (cons (car spec) processed)))
                 (else
                  (error 'filespec->path "unsupported filespec" spec)))))))

(define (path->namestring path)
  (string-join path "/"))

(define (filespec->namestring spec)
  (path->namestring (filespec->path spec)))

(define (print-link pkg-path target name)
  (let* ((path (filespec->path name))
         (n-up (- (length path) 1)))
    (if (= n-up 0)
        (println pkg-path "/" (filespec->namestring target)
                 " "
                 (filespec->namestring name))
        (println (string-join (make-list n-up "..") "/")
                 "/" pkg-path "/" (filespec->namestring target)
                 " "
                 (filespec->namestring name)))))


;;; Utilities

(define (basename namestring)
  (cond ((string-index-right namestring #\/)
         => (lambda (i)
              (substring namestring (+ i 1) (string-length namestring))))
        (else
         namestring)))

(define (dirname namestring)
  (cond ((string-index-right namestring #\/)
         => (lambda (i)
              (substring namestring 0 i)))
        (else
         namestring)))

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
