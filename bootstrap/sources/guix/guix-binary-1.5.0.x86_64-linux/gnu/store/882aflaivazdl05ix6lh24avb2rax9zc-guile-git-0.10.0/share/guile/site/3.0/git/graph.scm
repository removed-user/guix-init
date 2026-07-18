;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2025 45mg <45mg.writes@gmail.com>
;;; Copyright © 2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git graph)
  #:use-module (git bindings)
  #:use-module (git errors)
  #:use-module (git oid)
  #:use-module (git structs)
  #:use-module (git types)
  #:use-module (system foreign)
  #:export (graph-ahead-behind
            graph-descendant?))

(define graph-ahead-behind
  (let ((proc (libgit2->procedure* "git_graph_ahead_behind" '(* * * * *))))
    (lambda (repository local upstream)
      "Return two values: the number of unique commits \"ahead\" in UPSTREAM,
and the number of unique commits \"behind\" in LOCAL, where UPSTREAM and
LOCAL are OIDs."
      (let ((ahead (make-size_t-pointer))
            (behind (make-size_t-pointer)))
        (proc ahead behind
              (repository->pointer repository)
              (oid->pointer local)
              (oid->pointer upstream))
        (values (pointer->size_t ahead) (pointer->size_t behind))))))

(define graph-descendant?
  (let ((proc (libgit2->procedure int "git_graph_descendant_of" '(* * *))))
    (lambda (repository commit ancestor)
      "Return whether COMMIT is a descendant of ANCESTOR (both OIDs) in
REPOSITORY.  (Note that a commit is not considered a descendant of itself.)"
      (let ((ret (proc (repository->pointer repository)
                       (oid->pointer commit)
                       (oid->pointer ancestor))))
        (cond ((= ret 1) #t)
              ((= ret 0) #f)
              (else (raise-git-error ret)))))))

;; TODO: git_graph_reachable_from_any
