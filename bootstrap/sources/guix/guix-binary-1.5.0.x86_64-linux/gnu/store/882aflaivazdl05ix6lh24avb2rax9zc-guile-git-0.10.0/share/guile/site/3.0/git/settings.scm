;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 André Batista <nandre@riseup.net>
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

(define-module (git settings)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git configuration)
  #:export (owner-validation?
            set-owner-validation!
            set-tls-certificate-locations!
            user-agent
            set-user-agent!
            user-agent-product
            set-user-agent-product!
            home-directory
            set-home-directory!
            server-connection-timeout
            set-server-connection-timeout!
            server-timeout
            set-server-timeout!))

;; 'git_libgit2_opt_t' enum defined in <git2/common.h>.
(define GIT_OPT_GET_MWINDOW_SIZE 0)
(define GIT_OPT_SET_MWINDOW_SIZE 1)
(define GIT_OPT_GET_MWINDOW_MAPPED_LIMIT 2)
(define GIT_OPT_SET_MWINDOW_MAPPED_LIMIT 3)
(define GIT_OPT_GET_SEARCH_PATH 4)
(define GIT_OPT_SET_SEARCH_PATH 5)
(define GIT_OPT_SET_CACHE_OBJECT_LIMIT 6)
(define GIT_OPT_SET_CACHE_MAX_SIZE 7)
(define GIT_OPT_ENABLE_CACHING 8)
(define GIT_OPT_GET_CACHED_MEMORY 9)
(define GIT_OPT_GET_TEMPLATE_PATH 10)
(define GIT_OPT_SET_TEMPLATE_PATH 11)
(define GIT_OPT_SET_SSL_CERT_LOCATIONS 12)
(define GIT_OPT_SET_USER_AGENT 13)
(define GIT_OPT_ENABLE_STRICT_OBJECT_CREATION 14)
(define GIT_OPT_ENABLE_STRICT_SYMBOLIC_REF_CREATION 15)
(define GIT_OPT_SET_SSL_CIPHERS 16)
(define GIT_OPT_GET_USER_AGENT 17)
(define GIT_OPT_ENABLE_OFS_DELTA 18)
(define GIT_OPT_ENABLE_FSYNC_GITDIR 19)
(define GIT_OPT_GET_WINDOWS_SHAREMODE 20)
(define GIT_OPT_SET_WINDOWS_SHAREMODE 21)
(define GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION 22)
(define GIT_OPT_SET_ALLOCATOR 23)
(define GIT_OPT_ENABLE_UNSAVED_INDEX_SAFETY 24)
(define GIT_OPT_GET_PACK_MAX_OBJECTS 25)
(define GIT_OPT_SET_PACK_MAX_OBJECTS 26)
(define GIT_OPT_DISABLE_PACK_KEEP_FILE_CHECKS 27)
(define GIT_OPT_ENABLE_HTTP_EXPECT_CONTINUE 28)
(define GIT_OPT_GET_MWINDOW_FILE_LIMIT 29)
(define GIT_OPT_SET_MWINDOW_FILE_LIMIT 30)
(define GIT_OPT_SET_ODB_PACKED_PRIORITY 31)
(define GIT_OPT_SET_ODB_LOOSE_PRIORITY 32)
(define GIT_OPT_GET_EXTENSIONS 33)
(define GIT_OPT_SET_EXTENSIONS 34)
(define GIT_OPT_GET_OWNER_VALIDATION 35)
(define GIT_OPT_SET_OWNER_VALIDATION 36)
(define GIT_OPT_GET_HOMEDIR 37)
(define GIT_OPT_SET_HOMEDIR 38)
(define GIT_OPT_SET_SERVER_CONNECT_TIMEOUT 39)    ;note: two "SET" in a row
(define GIT_OPT_GET_SERVER_CONNECT_TIMEOUT 40)
(define GIT_OPT_SET_SERVER_TIMEOUT 41)
(define GIT_OPT_GET_SERVER_TIMEOUT 42)
(define GIT_OPT_SET_USER_AGENT_PRODUCT 43)
(define GIT_OPT_GET_USER_AGENT_PRODUCT 44)

(define integer-option
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int '*))))
    (lambda (option)
      "Return the value of OPTION, a 'GIT_OPT_GET_' flag, as an integer."
      (let ((out (make-int-pointer)))
        (proc option out)
        (pointer->int out)))))

(define set-integer-option!
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int int))))
    (lambda (option value)
      "Set OPTION, a 'GIT_OPT_SET_' flag, to VALUE."
      (proc option value))))

(define string-option
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int '*))))
    (lambda (option)
      "Return the value of OPTION, a 'GIT_OPT_GET_' flag, as a string."
      (let ((out (make-buffer)))
        (proc option out)
        (let ((str (buffer-content/string out)))
          (free-buffer out)
          str)))))

(define set-string-option!
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int '*))))
    (lambda* (option value #:key (false-is-null? #f))
      "Set the value of OPTION, a 'GIT_OPT_SET_' flag, to VALUE, a string.
When FALSE-IS-NULL? is true and VALUE is #f, convert it to the NULL pointer."
      (proc option (if (and (not value) false-is-null?)
                       %null-pointer
                       (string->pointer value))))))

(define (owner-validation?)
  "Return true if owner validation is enabled."
  (not (zero? (integer-option GIT_OPT_GET_OWNER_VALIDATION))))

(define (set-owner-validation! owner-validation?)
  "Enable/disable owner validation checks.  When enabled, raise an error
when a repository directory is not owned by the current user.  See
CVE-2022-24765."
  (set-integer-option! GIT_OPT_SET_OWNER_VALIDATION
                       (if owner-validation? 1 0)))

(define set-tls-certificate-locations!
  (let ((proc (libgit2->procedure* "git_libgit2_opts" (list int '* '*))))
    (lambda* (directory #:optional file)
      "Search for TLS certificates under FILE (a certificate bundle) or under
DIRECTORY (a directory containing one file per certificate, with \"hash
symlinks\" as created by OpenSSL's 'c_rehash').  Either can be #f but not both.
This is used when transferring from a repository over HTTPS."
      (proc GIT_OPT_SET_SSL_CERT_LOCATIONS
            (if file (string->pointer file) %null-pointer)
            (if directory (string->pointer directory) %null-pointer)))))

(define (set-user-agent! user-agent)
  "Append USER-AGENT to the 'User-Agent' HTTP header."
  (set-string-option! GIT_OPT_SET_USER_AGENT user-agent))

(define (user-agent)
  "Return the value of the 'User-Agent' header."
  (string-option GIT_OPT_GET_USER_AGENT))

(define (set-user-agent-product! product)
  "Use PRODUCT as the product portion of the User-Agent header.
This defaults to \"git/2.0\", for compatibility with other Git clients.  It
is recommended to keep this as \"git/VERSION\" for compatibility with servers
that do user-agent detection.

Set to the empty string to not send any user-agent string, or set to #f to
restore the default."
  (set-string-option! GIT_OPT_SET_USER_AGENT_PRODUCT product
                      #:false-is-null? #t))

(define (user-agent-product)
  "Return the value of the 'User-Agent' product header."
  (string-option GIT_OPT_GET_USER_AGENT_PRODUCT))

(define (set-home-directory! directory)
  "Use DIRECTORY as the user home directory used for file lookups."
  (set-string-option! GIT_OPT_SET_HOMEDIR directory))

(define (home-directory)
  "Return the current user home directory, as it will be used for file
lookups."
  (string-option GIT_OPT_GET_HOMEDIR))

(define (server-connection-timeout)
  "Return the server connection timeout in milliseconds; zero indicates using
the system default."
  (if %have-GIT_OPT_SET_SERVER_CONNECT_TIMEOUT?
      (integer-option GIT_OPT_GET_SERVER_CONNECT_TIMEOUT)
      0))

(define (set-server-connection-timeout! timeout)
  "Attempt connections to a remote server for up to TIMEOUT, expressed in
milliseconds.  Use the system default when TIMEOUT is 0.

Note that this may not be able to be configured longer than the system
default, typically 75 seconds.

This procedure has no effect when using libgit2 < 1.7."
  (when %have-GIT_OPT_SET_SERVER_CONNECT_TIMEOUT?
    (set-integer-option! GIT_OPT_SET_SERVER_CONNECT_TIMEOUT timeout)))

(define (server-timeout)
  "Return the timeout (in milliseconds) for reading from and writing to a remote server."
  (if %have-GIT_OPT_SET_SERVER_CONNECT_TIMEOUT?
      (integer-option GIT_OPT_GET_SERVER_TIMEOUT)
      0))

(define (set-server-timeout! timeout)
  "Wait up to TIMEOUT milliseconds when reading from or writing to a remote
server.  Use the system default when TIMEOUT is 0.

This procedure has no effect when using libgit2 < 1.7."
  (when %have-GIT_OPT_SET_SERVER_CONNECT_TIMEOUT?
    (set-integer-option! GIT_OPT_SET_SERVER_TIMEOUT timeout)))
