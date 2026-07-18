;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
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

(define-module (git diff)
  #:use-module (system foreign)
  #:use-module (git errors)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (DIFF-OPTIONS-VERSION
            GIT-SUBMODULE-IGNORE-UNSPECIFIED
            GIT-SUBMODULE-IGNORE-NONE
            GIT-SUBMODULE-IGNORE-UNTRACKED
            GIT-SUBMODULE-IGNORE-DIRTY
            GIT-SUBMODULE-IGNORE-ALL
            GIT-DIFF-FORMAT-PATCH
            GIT-DIFF-FORMAT-PATCH-HEADER
            GIT-DIFF-FORMAT-RAW
            GIT-DIFF-FORMAT-NAME-ONLY
            GIT-DIFF-FORMAT-NAME-STATUS
            GIT-DIFF-FORMAT-PATCH-ID
            GIT-DELTA-UNMODIFIED
            GIT-DELTA-ADDED
            GIT-DELTA-DELETED
            GIT-DELTA-MODIFIED
            GIT-DELTA-RENAMED
            GIT-DELTA-COPIED
            GIT-DELTA-IGNORED
            GIT-DELTA-UNTRACKED
            GIT-DELTA-TYPECHANGE
            GIT-DELTA-UNREADABLE
            GIT-DELTA-CONFLICTED
            
            make-diff-options
            diff-index-to-index
            diff-index-to-workdir
            diff-tree-to-index
            diff-tree-to-tree
            diff-tree-to-workdir
            diff-print
            diff->string
            diff-foreach
            diff-fold))

;;; https://libgit2.org/libgit2/#HEAD/group/diff
(define DIFF-OPTIONS-VERSION 1)

(define GIT-SUBMODULE-IGNORE-UNSPECIFIED -1)
(define GIT-SUBMODULE-IGNORE-NONE 1)
(define GIT-SUBMODULE-IGNORE-UNTRACKED 2)
(define GIT-SUBMODULE-IGNORE-DIRTY 3)
(define GIT-SUBMODULE-IGNORE-ALL 4)

(define GIT-DIFF-FORMAT-PATCH 1)        ;; full git diff
(define GIT-DIFF-FORMAT-PATCH-HEADER 2) ;; just the file headers of patch
(define GIT-DIFF-FORMAT-RAW 3)          ;; like git diff --raw
(define GIT-DIFF-FORMAT-NAME-ONLY 4)    ;; like git diff --name-only
(define GIT-DIFF-FORMAT-NAME-STATUS 5)  ;; like git diff --name-status
(define GIT-DIFF-FORMAT-PATCH-ID 6)     ;; git diff as used by git patch-id

(define GIT-DELTA-UNMODIFIED 0)  ;; no changes
(define GIT-DELTA-ADDED 1)       ;; entry does not exist in old version
(define GIT-DELTA-DELETED 2)     ;; entry does not exist in new version
(define GIT-DELTA-MODIFIED 3)    ;; entry content changed between old and new
(define GIT-DELTA-RENAMED 4)     ;; entry was renamed between old and new
(define GIT-DELTA-COPIED 5)      ;; entry was copied from another old entry
(define GIT-DELTA-IGNORED 6)     ;; entry is ignored item in workdir
(define GIT-DELTA-UNTRACKED 7)   ;; entry is untracked item in workdir
(define GIT-DELTA-TYPECHANGE 8)  ;; type of entry changed between old and new
(define GIT-DELTA-UNREADABLE 9)  ;; entry is unreadable
(define GIT-DELTA-CONFLICTED 10) ;; entry in the index is conflicted

(define make-diff-options
  ;; Return a <diff-options> structure for use with DIFF procedures.
  (let ((proc (libgit2->procedure* "git_diff_options_init"
                                   `(* ,unsigned-int))))
    (lambda* (#:key (version DIFF-OPTIONS-VERSION))
      (let ((diff-options (make-diff-options-bytestructure)))
        (proc (diff-options->pointer diff-options) version)
        diff-options))))

(define* diff-index-to-index
  (let ((proc (libgit2->procedure* "git_diff_index_to_index" '(* * * * *))))
    (lambda* (repository old-index new-index #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (index->pointer old-index)
              (index->pointer new-index)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-index-to-workdir
  (let ((proc (libgit2->procedure* "git_diff_index_to_workdir" '(* * * *))))
    (lambda* (repository index #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (index->pointer index)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-tree-to-index
  (let ((proc (libgit2->procedure* "git_diff_tree_to_index" '(* * * * *))))
    (lambda* (repository old-tree index #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (tree->pointer old-tree)
              (index->pointer index)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-tree-to-tree
  (let ((proc (libgit2->procedure* "git_diff_tree_to_tree" '(* * * * *))))
    (lambda* (repository old-tree new-tree #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (tree->pointer old-tree)
              (tree->pointer new-tree)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-tree-to-workdir
  (let ((proc (libgit2->procedure* "git_diff_tree_to_workdir" '(* * * * ))))
    (lambda* (repository old-tree #:optional (options (make-diff-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (tree->pointer old-tree)
              (diff-options->pointer options))
        (pointer->diff (dereference-pointer out))))))

(define* diff-print
  (let ((proc (libgit2->procedure* "git_diff_print" `(* ,int * *))))
    (lambda* (diff callback #:optional (format GIT-DIFF-FORMAT-PATCH))
      ;; Returning a non-zero value from the callbacks will terminate the
      ;; iteration and return the non-zero value to the caller.
      (let ((callback* (procedure->pointer int
                                           (lambda (delta hunk line _)
                                             (callback
                                               (pointer->diff-delta delta)
                                               (pointer->diff-hunk hunk)
                                               (pointer->diff-line line)))
                                           (list '* '* '* '*))))
        (proc (diff->pointer diff) format callback* %null-pointer)))))

(define* diff->string
  (let ((proc (libgit2->procedure* "git_diff_to_buf" `(* * ,int))))
    (lambda* (diff #:optional (format GIT-DIFF-FORMAT-PATCH))
      (let ((out (make-buffer)))
        (proc out (diff->pointer diff) format)
        (buffer-content/string out)))))

(define* diff-foreach
  (let ((proc (libgit2->procedure* "git_diff_foreach" '(* * * * * *))))
    (lambda* (diff file-cb binary-cb hunk-cb line-cb)
      ;; Returning a non-zero value from any of the callbacks will terminate
      ;; the iteration and return the value to the user.
      (let ((file-cb* (procedure->pointer int
                                          (lambda (delta progress _)
                                            (file-cb
                                              (pointer->diff-delta delta)
                                              progress))
                                          (list '* float '*)))
            (binary-cb* (procedure->pointer int
                                            (lambda (delta binary _)
                                              (binary-cb
                                                (pointer->diff-delta delta)
                                                (pointer->diff-binary binary)))
                                            (list '* '* '*)))
            (hunk-cb* (procedure->pointer int
                                          (lambda (delta hunk _)
                                            (hunk-cb
                                              (pointer->diff-delta delta)
                                              (pointer->diff-hunk hunk)))
                                          (list '* '* '*)))
            (line-cb* (procedure->pointer int
                                          (lambda (delta hunk line _)
                                            (line-cb
                                              (pointer->diff-delta delta)
                                              (pointer->diff-hunk hunk)
                                              (pointer->diff-line line)))
                                          (list '* '* '* '*))))
        (proc (diff->pointer diff) file-cb* binary-cb* hunk-cb* line-cb* %null-pointer)))))

(define (diff-fold file-proc binary-proc hunk-proc line-proc knil diff)
  (let ((out knil))
    (diff-foreach
      diff
      (lambda (delta progress)
        (set! out (file-proc delta progress out))
        0)
      (lambda (delta binary)
        (set! out (binary-proc delta binary out))
        0)
      (lambda (delta hunk)
        (set! out (hunk-proc delta hunk out))
        0)
      (lambda (delta hunk line)
        (set! out (line-proc delta hunk line out))
        0))
    out))
