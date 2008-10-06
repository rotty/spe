(library (spe setup larceny-compiler)
  (export compile-library
          with-working-directory)
  (import (rnrs base)
          (spe setup utils)
          
          (primitives parameterize current-directory)
          (prefix (larceny compiler) l:))

  (define-syntax with-working-directory
    (syntax-rules ()
      ((with-working-directory path body ...)
       (parameterize ((current-directory path))
         body ...))))

  (define (compile-library libname)
    (l:compile-library (libname->path libname))))
