;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;;
;;; This file is part of Guile-Git.
;;;
;;; Guile-Git is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-Git is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-Git.  If not, see <http://www.gnu.org/licenses/>.

(define-module (git revwalker)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git oid)
  #:use-module (git structs)
  #:use-module (git types)
  #:export (revwalk-hide!
            revwalk-new
            revwalk-next!
            revwalk-push!
            revwalk-push-range!
            revwalk-reset!))


;;; Revision Walker https://libgit2.github.com/libgit2/#HEAD/group/revwalk

(define %revwalk-free (libgit2->pointer "git_revwalk_free"))

(define revwalk-new
  (let ((proc (libgit2->procedure* "git_revwalk_new" `(* *))))
    (lambda (repository)
      "Create a new revision walker for REPOSITORY and return it."
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository))
        (set-pointer-finalizer! (dereference-pointer out) %revwalk-free)
        (pointer->revwalk (dereference-pointer out))))))

(define revwalk-hide!
  (let ((proc (libgit2->procedure int "git_revwalk_hide" `(* *))))
    (lambda (walker oid)
      "Hide OID from the revision WALKER."
      (proc (revwalk->pointer walker) (oid->pointer oid)))))

(define revwalk-push!
  (let ((proc (libgit2->procedure int "git_revwalk_push" `(* *))))
    (lambda (walker oid)
      "Push OID to the revision WALKER."
      (proc (revwalk->pointer walker) (oid->pointer oid)))))

(define revwalk-push-range!
  (let ((proc (libgit2->procedure int "git_revwalk_push_range" `(* *))))
    (lambda (walker range)
      "Push and hide the respective endpoints of the given RANGE."
      (proc (revwalk->pointer walker) (string->pointer range)))))

(define revwalk-next!
  (let ((proc (libgit2->procedure int "git_revwalk_next" `(* *))))
    (lambda (walker)
      "Retrieve the next OID in the revision WALKER. Returns #f if there are no more commits."
      (let ((oid (make-oid-pointer)))
        (if (zero? (proc oid (revwalk->pointer walker)))
            (pointer->oid oid)
            #f)))))

(define revwalk-reset!
  (let ((proc (libgit2->procedure int "git_revwalk_reset" `(*))))
    (lambda (walker)
      "Reset the revision WALKER to the initial state."
      (proc (revwalk->pointer walker)))))


;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/revwalk/git_revwalk_sorting
