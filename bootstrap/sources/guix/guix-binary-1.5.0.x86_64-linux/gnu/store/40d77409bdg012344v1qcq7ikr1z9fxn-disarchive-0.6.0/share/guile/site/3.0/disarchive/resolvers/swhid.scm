;;; Disarchive
;;; Copyright © 2020, 2021 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Disarchive.
;;;
;;; Disarchive is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Disarchive is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Disarchive.  If not, see <http://www.gnu.org/licenses/>.

(define-module (disarchive resolvers swhid)
  #:use-module (disarchive config)
  #:use-module (disarchive git-hash)
  #:use-module (disarchive resolvers)
  #:use-module (disarchive utils)
  #:use-module (gcrypt base16)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-2)
  #:export (swhid-resolver))

(define (file-swhid-address filename)
  (string-append "swh:1:dir:"
                 (bytevector->base16-string
                  (git-hash-directory filename))))

(define (resolve-swhid-address address output)
  ;; XXX: This is a hack to avoid a circular reference between Guix
  ;; and Disarchive.  We could copy in the Software Heritage module or
  ;; pull it out of Guix or...?
  (and-let* ((module (resolve-module '(guix swh) #:ensure #f))
             (vault-fetch (module-ref module 'vault-fetch)))
    (match (string-split address #\:)
      (("swh" "1" "dir" address)
       (let* ((in (vault-fetch address 'directory))
              (_ (mkdir-p output))
              ;; XXX: This assumes that Gzip can be found from $PATH.
              (out (open-pipe* OPEN_WRITE %tar
                               "-C" output
                               "--strip-components=1"
                               "-xzf" "-")))
         (dump-port-all in out)
         (close-port in)
         (close-pipe out)))
      (_ (error "Invalid SWHID" address)))))

(define serialize-swhid-address identity)

(define deserialize-swhid-address identity)

(define swhid-resolver
  (make-resolver 'swhid file-swhid-address resolve-swhid-address
                 serialize-swhid-address deserialize-swhid-address))
