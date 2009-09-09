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
        (spells fmt)
        (spells match)
        (spells pathname)
        (spells filesys)
        (spells misc)
        (spells logging)
        (spells tracing)
        (prometheus)
        (conjure utils)
        (conjure base)
        (conjure task-lib)
        (conjure dsl))

(register-builtin-tasks)

(set-logger-properties!
 root-logger
 `((threshold info)
   (handlers
    ,(lambda (entry)
       (default-log-formatter entry (current-output-port))))))

(define log/spe (make-fmt-log '(spe build)))

(define default-conj-environment
  (environment '(rnrs)
               '(srfi :39 parameters)
               '(conjure dsl)))

(define product-dir (x->pathname '(("r6rs-libs"))))

(define (make-triggered-delegator obj triggers thunk)
  (let ((clone (obj 'clone)))
    (define (trigger-method msg)
      (lambda (self resend . args)
        (thunk)
        (for-each (lambda (trigger)
                    (self 'delete-slot! trigger))
                  triggers)
        (apply resend #f msg args)))
    (for-each (lambda (trigger)
                (clone 'add-method-slot! trigger (trigger-method trigger)))
              triggers)
    clone))

(define (make-system-project parent sys-name sys-dir forms)
  (let ((project (<project> 'new sys-name
                            '()
                            `((source-dir ,(pathname-join
                                            (parent 'source-dir)
                                            sys-dir))))))
    (make-triggered-delegator
     project
     '(construct-step)
     (lambda ()
       (with-project project
         (lambda ()
           (match forms
             ((('import import-specs ___) body0 body ___)
              (eval `(let () ,@(cons body0 body)) (apply environment import-specs)))
             ((body0 body ___)
              (eval `(let () ,@(cons body0 body)) default-conj-environment))
             (_
              #f))))))))

(define (system-task-name sym)
  (string->symbol (string-append "system/" (symbol->string sym))))

(define (alist-rhsides alist key)
  (append-map (lambda (entry)
                (if (eq? (car entry) key)
                    (cdr entry)
                    '()))
              alist))

(define (sys-defs->projects pathname parent)
  (call-with-input-file (x->namestring pathname)
    (lambda (port)
      (filter-map
       (lambda (form)
         (match form
           (('define-system name clauses ___)
            (let ((project (make-system-project
                            parent
                            (system-task-name name)
                            (pathname-with-file pathname #f)
                            (alist-rhsides clauses 'conjure))))
              (modify-object! project
                (dependencies (map system-task-name
                                   (alist-rhsides clauses 'dependencies))))
              project))
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
                   (sys-defs->projects sys-def (current-project)))))
     #f)
   #f))

(spe-project 'invoke (cdr (command-line)))

;; Local Variables:
;; scheme-indent-styles: (conjure-dsl (match 1) (modify-object! 1) (object 1))
;; End:
