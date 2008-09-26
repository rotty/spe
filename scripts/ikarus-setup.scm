(import (rnrs base)
        (rnrs lists)
        (rnrs control)
        (rnrs io simple))

(define (main)
  (for-each (lambda (lst)
              (call-with-input-file (cadr lst) 
                (lambda (port)
                  (process-library (car lst) (cadr lst) (read port)))))
            (read)))

(define (process-library sys-path filename form)
  (let ((lib-name (cadr form))
        (include-forms (filter (lambda (form)
                                 (eq? (car form) 'include))
                               (cddddr form))))
    (for-each display (list (make-link-target sys-path
                                              (length lib-name)
                                              (string-append "libraries/" (libname->path lib-name)))
                            " "
                            (libname->path lib-name)
                            #\newline))
    (for-each (lambda (iform)
                (for-each (lambda (filespec)
                            (process-include-file sys-path lib-name filespec))
                          (cdr iform)))
              include-forms)))

(define (process-include-file sys-path lib-name filespec)
  (let ((filename (filespec->path filespec ".scm")))
    (for-each display (list (make-link-target sys-path (+ (filespec-ddepth filespec) 1) filename)
                            " "
                            filename
                            #\newline))))

(define (make-link-target sys-path n filename)
  (string-append "../"  (string-join (make-list n "..") "/")  "/" sys-path "/" filename))

(define (make-list len . maybe-elt)
  (let ((elt (cond ((null? maybe-elt) #f) ; Default value
		   ((null? (cdr maybe-elt)) (car maybe-elt))
		   (else (error "Too many arguments to MAKE-LIST"
				(cons len maybe-elt))))))
    (do ((i len (- i 1))
	 (ans '() (cons elt ans)))
	((<= i 0) ans))))

(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ((result '()) (lst lst))
        (if (null? lst)
            (apply string-append (cdr (reverse result)))
            (loop (cons (car lst) (cons sep result))
                  (cdr lst))))))

(define (filespec->path name ext)
  (cond ((symbol? name) (string-append (symbol->string name) ext))
        ((pair? name) (string-append
                       (if (pair? (car name))
                           (string-join (map symbol->string (car name)) "/")
                           (symbol->string (car name)))
                       "/"
                       (symbol->string (cadr name))
                       ext))
        (else (error "invalid filespec" name))))

(define (filespec-ddepth filespec)
  (cond ((symbol? filespec)
         0)
        ((pair? filespec)
         (if (pair? (car filespec))
             (length (car filespec))
             1))
        (else
         (error "invalid filespec" filespec))))

(define (libname->path lib-name)
  (filespec->path (if (= (length lib-name) 1)
                      (car lib-name)
                      lib-name)
                  ".sls"))

(main)