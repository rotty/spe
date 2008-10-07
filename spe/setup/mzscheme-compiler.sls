#lang scheme

(provide compile-library
         with-working-directory)

(require (lib "spe/setup/utils.sls")

         (prefix-in r6rs: (lib "r6rs/lang/reader"))
         compiler/cm)

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory path body ...)
     (parameterize ((current-directory path))
       body ...))))

(define r6rs-read-syntax
  (case-lambda
    (() (r6rs-read-syntax (object-name (current-input-port))))
    ((name) (r6rs-read-syntax name (current-input-port)))
    ((name port)
     (datum->syntax #f (r6rs:read-syntax name port #'r6rs 1 0 1)))))

(define (compile-file src)
  (parameterize ((manager-compile-notify-handler
                  (lambda (p)
                    (printf " (Compiling ~a)\n" p))))
    (managed-compile-zo src r6rs-read-syntax)))

(define (compile-library libname)
  (compile-file (libname->path libname)))

(with-working-directory "targets/mzscheme"
  (compile-file "spells/define-values.sls"))
