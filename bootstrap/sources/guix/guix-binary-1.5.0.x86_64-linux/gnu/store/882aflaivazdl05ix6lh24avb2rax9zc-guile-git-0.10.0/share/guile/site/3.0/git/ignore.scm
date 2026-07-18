;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2021 Fredrik Salomonsson <plattfot@posteo.net>
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

(define-module (git ignore)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (ignored-file?))

;;; ignore https://libgit2.github.com/libgit2/#HEAD/group/ignore
(define ignored-file?
  (let ((proc (libgit2->procedure* "git_ignore_path_is_ignored" '(* * *))))
    (lambda (repository file)
      "Return true if FILE would be ignored according to current ignore
rules, regardless of whether the file is already in the index or committed to
the repository."
      (let ((ignored (make-int-pointer)))
        (proc ignored
              (repository->pointer repository)
              (string->pointer file))
        (= (pointer->int ignored) 1)))))
