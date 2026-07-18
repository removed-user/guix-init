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

(define-module (git notes)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git errors)
  #:use-module (git oid)
  #:use-module (git structs)
  #:use-module (git types)
  #:use-module (ice-9 match)
  #:export (note-author
            note-commiter
            note-id
            note-message
            note-read
            note-create))

;;; https://libgit2.github.com/libgit2/#HEAD/group/note

(define note-author
  (let ((proc (libgit2->procedure '* "git_note_author" '(*))))
    (lambda (note)
      "Return the author of NOTE as a pointer to the signature."
      (pointer->signature (proc (note->pointer note))))))

(define note-committer
  (let ((proc (libgit2->procedure '* "git_note_committer" '(*))))
    (lambda (note)
      "Return the committer of NOTE as a pointer to the signature."
      (pointer->signature (proc (note->pointer note))))))

(define note-id
  (let ((proc (libgit2->procedure '* "git_note_id" '(*))))
    (lambda (note)
      "Return the object ID of the note as an OID."
      (pointer->oid (proc (note->pointer note))))))

(define note-message
  (let ((proc (libgit2->procedure '* "git_note_message" '(*))))
    (lambda (note)
      "Return the message of NOTE as a string."
      (pointer->string (proc (note->pointer note))))))

(define note-read
  (let ((proc (libgit2->procedure* "git_note_read" `(* * * *))))
    (lambda* (repository oid #:optional notes-ref)
      "Read the note for the given OID in the specified NOTES-REF within REPOSITORY.
       Returns the note object or #f if no note is found."
      (catch 'git-error
        (lambda ()
          (let* ((out (make-double-pointer)))
            (proc out
                  (repository->pointer repository)
                  (if (or (not notes-ref) (null? notes-ref))
                      %null-pointer
                      (string->pointer notes-ref))
                  (oid->pointer oid))
            (let ((note (pointer->note (dereference-pointer out))))
              (set-pointer-finalizer! (note->pointer note)
                                      (libgit2->pointer "git_note_free"))
              note)))
        (lambda (key error . rest)
          ;; For convenience return #f in the common case.
          (if (= GIT_ENOTFOUND (git-error-code error))
              #f
              (apply throw key error rest)))))))

(define note-create
  (let ((proc (libgit2->procedure int "git_note_create" `(* * * * * * * ,int))))
    (lambda* (repository author committer oid note-content
                         #:optional notes-ref (force? 0))
      "Create a note for the specified OID in the given NOTES-REF within REPOSITORY.
       AUTHOR and COMMITTER are the git signatures for the operation.
       NOTE-CONTENT is the message of the note.
       If FORCE? is non-zero, overwrite existing notes."
      (let* ((out (make-oid-pointer))
             (result (proc out
                           (repository->pointer repository)
                           (if (or (not notes-ref) (null? notes-ref))
                               %null-pointer
                               (string->pointer notes-ref))
                           (signature->pointer author)
                           (signature->pointer committer)
                           (oid->pointer oid)
                           (string->pointer note-content)
                           force?)))
        (if (zero? result)
            (pointer->oid out)
            (throw 'git-error result))))))
