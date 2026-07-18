;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (git checkout)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git fetch)
  #:use-module (git structs)
  #:use-module (git types)
  #:use-module (git repository)
  #:export (checkout-head
            checkout-index
            checkout-tree
            make-checkout-options))

;;; checkout https://libgit2.github.com/libgit2/#HEAD/group/checkout

(define CHECKOUT-OPTIONS-VERSION 1)

(define make-checkout-options
  (let ((proc (libgit2->procedure* "git_checkout_init_options"
                                   `(* ,unsigned-int))))
    (lambda ()
      (let ((checkout-options (make-checkout-options-bytestructure)))
        (proc (checkout-options->pointer checkout-options) CHECKOUT-OPTIONS-VERSION)
        checkout-options))))

(define checkout-head
  (let ((proc (libgit2->procedure* "git_checkout_head" '(* *))))
    (lambda* (repository #:optional (checkout-options (make-checkout-options)))
      (proc (repository->pointer repository)
            (checkout-options->pointer checkout-options)))))

(define checkout-index
  (let ((proc (libgit2->procedure* "git_checkout_index" '(* * *))))
    (lambda* (repository index
                         #:optional (checkout-options (make-checkout-options)))
      (proc (repository->pointer repository)
	    (index->pointer index)
	    (checkout-options->pointer checkout-options)))))

(define checkout-tree
  (let ((proc (libgit2->procedure* "git_checkout_tree" `(* * *))))
    (lambda* (repository treeish
                         #:optional (checkout-options (make-checkout-options)))
      (proc (repository->pointer repository)
	    (object->pointer treeish)
	    (checkout-options->pointer checkout-options)))))
