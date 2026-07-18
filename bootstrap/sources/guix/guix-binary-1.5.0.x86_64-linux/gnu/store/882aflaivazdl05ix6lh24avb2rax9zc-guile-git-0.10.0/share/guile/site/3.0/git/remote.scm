;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2021, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (git remote)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git fetch)
  #:use-module (git structs)
  #:use-module (git types)
  #:export (remote-name
            remote-url
            remote-set-url!
            remote-lookup
            remote-fetch
            remote-create-anonymous
            remote-create-detached
            remote-connected?
            remote-connect
	    remote-connect/detached
            remote-disconnect
            remote-ls
            remote-default-branch))

(define %remote-free (libgit2->pointer "git_remote_free"))

(define (pointer->remote! pointer)
  (set-pointer-finalizer! pointer %remote-free)
  (pointer->remote pointer))

(define remote-name
  (let ((proc (libgit2->procedure '* "git_remote_name" '(*))))
    (lambda (remote)
      (pointer->string (proc (remote->pointer remote))))))

(define remote-url
  (let ((proc (libgit2->procedure '* "git_remote_url" '(*))))
    (lambda (remote)
      "Return the URL of REMOTE, a \"remote\" as returned by
'remote-lookup'."
      (pointer->string (proc (remote->pointer remote))))))

(define remote-set-url!
  (let ((proc (libgit2->procedure* "git_remote_set_url" '(* * *))))
    (lambda (repository remote url)
      "Change the URL of REMOTE, a string, to URL."
      (proc (repository->pointer repository) (string->pointer remote)
            (string->pointer url)))))

(define remote-lookup
  (let ((proc (libgit2->procedure* "git_remote_lookup" '(* * *))))
    (lambda* (repository remote-name)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer remote-name))
        (pointer->remote! (dereference-pointer out))))))

(define remote-create-anonymous
  (let ((proc (libgit2->procedure* "git_remote_create_anonymous" '(* * *))))
    (lambda* (repository url)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer url))
        (pointer->remote! (dereference-pointer out))))))

(define remote-create-detached
  (let ((proc (libgit2->procedure* "git_remote_create_detached" '(* *))))
    (lambda* (url)
      "Return an in-memory remote for URL without a connected local repository."
      (let ((out (make-double-pointer)))
        (proc out
              (string->pointer url))
        (pointer->remote! (dereference-pointer out))))))

(define remote-connected?
  (let ((proc (libgit2->procedure int "git_remote_connected" '(*))))
    (lambda* (remote)
      (case (proc (remote->pointer remote))
        ((1) #t)
        (else #f)))))

(define GIT_DIRECTION_FETCH 0)

(define remote-connect
  ;; TODO: calling this on detached remotes causes segfaults,
  ;; hence the remote-connect/detached work-around
  (let ((proc (libgit2->procedure* "git_remote_connect" `(* ,int * * * )))) ;; XXX: actual types
    (lambda* (remote)
      (let ((remote-callbacks (make-remote-callbacks)))
        (set-remote-callbacks-version! remote-callbacks 1)
        (proc (remote->pointer remote)
              GIT_DIRECTION_FETCH
              (remote-callbacks->pointer remote-callbacks)
              %null-pointer
              %null-pointer)))))

(define remote-connect/detached
  (let ((proc (libgit2->procedure* "git_remote_connect" `(* ,int * * * )))) ;; XXX: actual types
    (lambda* (remote)
      "Connect the detached remote."
      (proc (remote->pointer remote)
            GIT_DIRECTION_FETCH
            %null-pointer
            %null-pointer
            %null-pointer))))

(define remote-disconnect
  (let ((proc (libgit2->procedure void "git_remote_disconnect" '(*))))
    (lambda (remote)
      (proc (remote->pointer remote)))))

(define remote-ls
  (let ((proc (libgit2->procedure* "git_remote_ls" '(* * *))))
    (lambda* (remote)
      (let ((out (make-double-pointer))
            (size-ptr (make-size_t-pointer)))
        (proc out size-ptr
              (remote->pointer remote))
        (pointer->remote-head-list (dereference-pointer out)
                                   (pointer->size_t size-ptr))))))

(define remote-fetch
  (let ((proc (libgit2->procedure* "git_remote_fetch" '(* * * *))))
    (lambda* (remote #:key
                     (reflog-message "")
                     (fetch-options (make-fetch-options))
                     (refspecs '()))
      "Fetch the content of REMOTE, an opaque libgit2 remote object
associated to a repository, into its associated repository.

REFLOG-MESSAGE is the message to insert into the reflogs.
FETCH-OPTIONS is a data structure used to tweak the way the remote is
fetched.

REFSPECS is a list of strings that contain symbolic references defined
in the remote to be fetched. If it is set to the empty list, only the
remote HEAD will be fetched."
      (proc (remote->pointer remote)
            (strarray->pointer
             (string-list->strarray refspecs))
            (fetch-options->pointer fetch-options)
            (string->pointer reflog-message)))))

(define remote-default-branch
  (let ((proc (libgit2->procedure* "git_remote_default_branch" '(* *))))
    (lambda (remote)
      "Return the name of REMOTE's default branch.

The default branch of a repository is the branch which HEAD points to.  If
the remote does not support reporting this information directly, it performs
the guess as git does; that is, if there are multiple branches which point to
the same commit, the first one is chosen.  If the master branch is a
candidate, it wins.

This procedure must be called after connecting REMOTE."
      (let ((buffer (make-buffer)))
        (proc buffer (remote->pointer remote))
        (buffer-content/string buffer)))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/reset/git_reset_default
