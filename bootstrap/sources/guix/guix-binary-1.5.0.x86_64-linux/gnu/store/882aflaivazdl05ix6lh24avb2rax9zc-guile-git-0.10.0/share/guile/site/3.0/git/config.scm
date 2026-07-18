;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021, 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git config)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git structs)
  #:use-module (git types)
  #:export (config-foreach
            config-fold
            config-get-entry
            set-config-string
            set-config-integer
            set-config-boolean))

;;; config https://libgit2.github.com/libgit2/#HEAD/group/config

(define %config-entry-free!
  (libgit2->procedure void "git_config_entry_free" '(*)))

(define config-foreach
  (let ((proc (libgit2->procedure* "git_config_foreach" '(* * *)))
        (wrap (lambda (callback)
                (lambda (ptr _)
                  ;; Note: do *not* call %CONFIG-ENTRY-FREE! on PTR since PTR
                  ;; is documented as being valid only for the duration of
                  ;; the iteration.
                  (callback (pointer->config-entry ptr))))))
    (lambda (config callback)
      "Iterate over all the entries of CONFIG, passing each config entry to
CALLBACK, a one-argument procedure.  The result is unspecified."
      (let ((callback* (procedure->pointer int (wrap callback)
                                           (list '* '*))))
        (proc (config->pointer config) callback* %null-pointer)))))

(define (config-fold proc knil config)
  "Fold over the entries of CONFIG and return the result.  For each entry,
PROC is passed the entry and the previous result, starting from KNIL."
  (let ((out knil))
    (config-foreach
      config
      (lambda (entry)
        (set! out (proc entry out))
        0))
    out))

(define config-get-entry
  (let ((proc (libgit2->procedure* "git_config_get_entry" '(* * *))))
    (lambda (config name)
      "Return the entry NAME for CONFIG, where NAME is a string such as
\"core.bare\" or \"remote.origin.url\".  Raise an exception if NAME was not
found."
      ;; FIXME: Return #f upon GIT_ENOTFOUND?
      (let ((out (make-double-pointer)))
        (proc out (config->pointer config) (string->pointer name))
        (let* ((ptr   (dereference-pointer out))
               (entry (pointer->config-entry ptr)))
          ;; It's our responsibility to free PTR.
          (%config-entry-free! ptr)
          entry)))))

(define set-config-string
  (let ((proc (libgit2->procedure* "git_config_set_string" '(* * *))))
    (lambda (config key value)
      "Record the KEY/VALUE association in CONFIG and in the corresponding
config file, where VALUE is a string."
      (proc (config->pointer config)
            (string->pointer key "UTF-8") (string->pointer value "UTF-8")))))

(define set-config-integer
  (let ((proc32 (libgit2->procedure* "git_config_set_int32" `(* * ,int32)))
        (proc64 (libgit2->procedure* "git_config_set_int64" `(* * ,int64))))
    (lambda (config key value)
      "Record the KEY/VALUE association in CONFIG and in the corresponding
config file, where VALUE is an integer."
      (let ((proc (if (>= value (expt 2 32)) proc64 proc32)))
        (proc (config->pointer config)
              (string->pointer key "UTF-8") value)))))

(define set-config-boolean
  (let ((proc (libgit2->procedure* "git_config_set_bool" `(* * ,int))))
    (lambda (config key value)
      "Record the KEY/VALUE association in CONFIG and in the corresponding
config file, where VALUE is a Boolean."
      (proc (config->pointer config)
            (string->pointer key "UTF-8") (if value 1 0)))))
