#!r6rs
(library (spe setup utils)
  (export string-join string-split string-prefix? string-suffix? libname->path)
  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic fixnums)
          (rnrs mutable-pairs)
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

  (define (string-prefix? s1 s2)
    (and (>= (string-length s1)
             (string-length s2))
         (let ((ls1 (string-length s1))
               (ls2 (string-length s2)))
           (string=? (substring s1 0 ls2) s2))))

  (define (string-suffix? s1 s2)
    (and (>= (string-length s1)
             (string-length s2))
         (let ((ls1 (string-length s1))
               (ls2 (string-length s2)))
           (string=? (substring s1 (- ls1 ls2) ls1) s2))))

  (define string-split
    ;; Taken from Chicken Scheme
    (case-lambda
      ((str) (string-split str "\t\n\r " #f))
      ((str delim-strs) (string-split str delim-strs #f))
      ((str delim-strs keep-empty?)
       (unless (string? str)
         (assertion-violation 'string-split "not a string" str))
       (unless (string? delim-strs)
         (assertion-violation 'string-split "not a string" delim-strs))
       (let ((strlen (string-length str))
             (dellen (string-length delim-strs)) 
             (first #f))
         (define (add from to last)
           (let ((node (cons (substring str from to) '())))
             (if first
                 (set-cdr! last node)
                 (set! first node) ) 
             node))
         (let loop ((i 0) (last #f) (from 0))
           (cond ((fx>=? i strlen)
                  (when (or (fx>? i from) keep-empty?) (add from i last))
                  (or first '()) )
                 (else
                  (let ((c (string-ref str i)))
                    (let scan ((j 0))
                      (cond ((fx>=? j dellen) (loop (fx+ i 1) last from))
                            ((eq? c (string-ref delim-strs j))
                             (let ((i2 (fx+ i 1)))
                               (if (or (fx>? i from) keep-empty?)
                                   (loop i2 (add from i last) i2)
                                   (loop i2 last i2))))
                            (else (scan (fx+ j 1)))))))))))))


  )
