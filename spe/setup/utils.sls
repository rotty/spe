#!r6rs
(library (spe setup utils)
  (export string-join libname->path)
  (import (rnrs base)
          (rnrs lists))

  
  (define (libname->path lib-name)
    (string-append (string-join (map symbol->string (filter symbol? lib-name)) "/")
                   ".sls"))

  (define (string-join lst sep)
    (if (null? lst)
        ""
        (let loop ((result '()) (lst lst))
          (if (null? lst)
              (apply string-append (cdr (reverse result)))
              (loop (cons (car lst) (cons sep result))
                    (cdr lst))))))
  
  )
