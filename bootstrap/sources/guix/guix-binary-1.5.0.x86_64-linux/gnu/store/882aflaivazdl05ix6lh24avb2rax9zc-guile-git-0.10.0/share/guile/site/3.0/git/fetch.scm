;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Marius Bakke <marius@devup.no>
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

(define-module (git fetch)
  #:use-module (system foreign)
  #:use-module (git auth)
  #:use-module (git bindings)
  #:use-module (git cred)
  #:use-module (git structs)
  #:use-module (git types)
  #:use-module (srfi srfi-26)

  #:export (make-fetch-options
            fetch-init-options   ;deprecated!
            set-fetch-auth-with-ssh-agent!
            set-fetch-auth-with-ssh-key!
            set-fetch-auth-with-default-ssh-key!))

(define FETCH-OPTIONS-VERSION 1)

(define make-fetch-options
  (let ((proc (libgit2->procedure* "git_fetch_init_options"
                                   `(* ,unsigned-int))))
    (lambda* (#:optional auth-method
              #:key
              proxy-url (proxy-type (if proxy-url 'specified 'none))
              transfer-progress)
      "Return a <fetch-options> record.  When AUTH-METHOD is true, it must be
an object as returned by '%make-auth-ssh-agent' or
'%make-auth-ssh-credentials'.  When TRANSFER-PROGRESS is true, it must be a
one-argument procedure.  TRANSFER-PROGRESS is called periodically and passed
an <indexer-progress> record; when TRANSFER-PROGRESS returns #false,
transfers are canceled.

When PROXY-URL is true, it is the URL of an HTTP/HTTPS proxy to use.
PROXY-TYPE is one of 'none, 'specified, or 'auto.  The default is 'specified
when PROXY-URL is true and 'none when PROXY-URL is false.  Setting it to
'auto enables proxy detection based on the Git configuration."
      (let ((fetch-options (make-fetch-options-bytestructure)))
        (proc (fetch-options->pointer fetch-options) FETCH-OPTIONS-VERSION)

        (cond
         ((auth-ssh-credentials? auth-method)
          (set-fetch-auth-with-ssh-key! fetch-options auth-method))
         ((auth-ssh-agent? auth-method)
          (set-fetch-auth-with-ssh-agent! fetch-options)))

        (set-fetch-options-proxy-type! fetch-options proxy-type)
        (when proxy-url
          (set-fetch-options-proxy-url! fetch-options proxy-url))

        (when transfer-progress
          (set-fetch-options-transfer-progress! fetch-options
                                                transfer-progress))

        fetch-options))))

(define fetch-init-options
  ;; Deprecated alias for compatibility with 0.2.
  make-fetch-options)

(define (set-fetch-auth-callback fetch-options callback)
  (let ((callbacks (fetch-options-remote-callbacks fetch-options)))
    (set-remote-callbacks-credentials! callbacks
                                       (pointer-address callback))))

(define (current-user-name)
  "Return the current user name."
  ;; Note: 'getlogin', which relies on 'getutent', doesn't work inside Guix
  ;; build environments.
  (or (getlogin)
      (and=> (false-if-exception (getpwuid (getuid)))
             passwd:name)
      (getenv "LOGNAME")
      (getenv "USER")))

(define (set-fetch-auth-with-ssh-agent! fetch-options)
  (set-fetch-auth-callback
   fetch-options
   (cred-acquire-cb
    (lambda (cred url username allowed payload)
      (let ((username (if (eq? username %null-pointer)
                          ""
                          (pointer->string username))))
        (cond
         ;; If no username were specified in URL, we will be asked for
         ;; one. Try with the current user login.
         ((= allowed CREDTYPE-SSH-USERNAME)
          (cred-username-new cred (current-user-name)))
         (else
          (cred-ssh-key-from-agent cred username))))))))

(define* (set-fetch-auth-with-ssh-key! fetch-options
                                       auth-ssh-credentials)
  (set-fetch-auth-callback
   fetch-options
   (cred-acquire-cb
    (lambda (cred url username allowed payload)
      (cond
       ;; Same as above.
       ((= allowed CREDTYPE-SSH-USERNAME)
        (cred-username-new cred (current-user-name)))
       (else
        (let* ((pri-key-file
                (auth-ssh-credentials-private-key auth-ssh-credentials))
               (pub-key-file
                (auth-ssh-credentials-public-key auth-ssh-credentials))
               (username (if (eq? username %null-pointer)
                             ""
                             (pointer->string username))))
          (cred-ssh-key-new cred
                            username
                            pub-key-file
                            pri-key-file
                            ""))) )))))

(define (set-fetch-options-transfer-progress! fetch-options
                                              transfer-progress)
  (let ((callbacks (fetch-options-remote-callbacks fetch-options)))
    (set-remote-callbacks-transfer-progress! callbacks transfer-progress)))

(define (set-fetch-options-proxy-type! fetch-options type)
  (let ((proxy (fetch-options-proxy-options fetch-options)))
    (set-proxy-options-type! proxy type)))

(define (set-fetch-options-proxy-url! fetch-options url)
  (let ((proxy (fetch-options-proxy-options fetch-options)))
    (set-proxy-options-url! proxy url)))
