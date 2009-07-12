;;; build.sps --- Build script for SPE

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (rnrs programs)
        (rnrs eval)
        (except (srfi :1 lists) for-each map)
        (srfi :39 parameters)
        (spells match)
        (spells pathname)
        (spells filesys)
        (spells misc)
        (spells logging)
        (spells tracing)
        (conjure base)
        (conjure task-lib)
        (conjure dsl))

(register-builtin-tasks)

(configure-logger '(conjure)
                  `((threshold info)
                    (handlers
                     ,(lambda (entry)
                        (default-log-formatter entry
                          (current-output-port))))))

(define default-conj-environment
  (environment '(rnrs)
               '(srfi :39 parameters)
               '(conjure dsl)))

(define product-dir (x->pathname '(("r6rs-libs"))))

(define (system-buildproc sys-name sys-dir forms)
  (lambda (step)
    (define (do-eval forms env)
      (with-project (<project> 'new sys-name
                               '()
                               `((source-dir ,(pathname-join
                                               ((step 'project) 'source-dir)
                                               sys-dir))))
        (lambda ()
          (eval `(let () ,@forms) env)
          ((current-project) 'build-rec))))
    (for-each display (list "* Building system " sys-name "\n"))
    (match forms
      ((('import import-specs ___) body0 body ___)
       (do-eval (cons body0 body) (apply environment import-specs)))
      ((body0 body ___)
       (do-eval (cons body0 body) default-conj-environment))
      (_
       #f))))

(define (system-task-name sym)
  (string->symbol (string-append "system/" (symbol->string sym))))

(define (alist-rhsides alist key)
  (append-map (lambda (entry)
                (if (eq? (car entry) key)
                    (cdr entry)
                    '()))
              alist))

(define (sys-defs->tasks pathname)
  (call-with-input-file (x->namestring pathname)
    (lambda (port)
      (filter-map
       (lambda (form)
         (match form
           (('define-system name clauses ___)
            (let ((deps (map system-task-name
                             (alist-rhsides clauses 'dependencies)))
                  (forms (alist-rhsides clauses 'conjure)))
              (<ordinary-task>
               'new
               (system-task-name name)
               '()
               `((depends ,@deps)
                 (proc ,(system-buildproc name
                                          (pathname-with-file pathname #f)
                                          forms))))))
           (_
            #f)))
       (port->sexps port)))))

(define (port->sexps port)
  (unfold eof-object? values (lambda (seed) (read port)) (read port)))

(define-project spe-project
    ((product-dir product-dir))
  (directory-fold
   '(("systems"))
   (lambda (pathname state)
     (let ((sys-def (pathname-join (pathname-as-directory pathname) "sys-def.scm")))
       (when (file-exists? sys-def)
         (for-each (lambda (task) ((current-project) 'add-task task))
                   (sys-defs->tasks sys-def))))
     #f)
   #f))

(spe-project 'invoke (cdr (command-line)))

;; Local Variables:
;; scheme-indent-styles: (conjure-dsl)
;; End:
