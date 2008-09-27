(library (spe boot filesys)
  (export with-working-directory)
  (import (rnrs base)
          (rnrs bytevectors)
          (primitives foreign-procedure
                      ffi/asciiz->string))
  
  (define set-working-directory!
    (let ((chdir (foreign-procedure "chdir" '(string) 'int)))
      (lambda (newdir)
        (if (not (zero? (chdir newdir)))
            (error "cd: " newdir " is not a valid directory name.")))))

  (define working-directory
    (let ((getcwd (foreign-procedure "getcwd" '(boxed int) 'int)))
      (lambda ()
        (let ((s (make-bytevector 1024)))
          (getcwd s 1024)
          (ffi/asciiz->string s)))))

  (define-syntax with-working-directory
    (syntax-rules ()
      ((with-working-directory dir body ...)
       (let ((wd (working-directory)))
         (dynamic-wind
           (lambda () (set-working-directory! dir))
           (lambda () body ...)
           (lambda () (set-working-directory! wd))))))))

