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

(define-module (disarchive resolvers)
  #:use-module (disarchive logging)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<resolver>
            make-resolver
            resolver?
            resolver-name
            resolver-file-address
            resolver-resolve-address
            resolver-serialize-address
            resolver-deserialize-address

            file-addresses
            resolve-address
            serialize-address
            deserialize-address
            resolve-addresses
            %resolve-addresses))

(define-record-type <resolver>
  (make-resolver name file-address resolve-address
                 serialize-address deserialize-address)
  resolver?
  (name resolver-name)
  (file-address resolver-file-address)
  (resolve-address resolver-resolve-address)
  (serialize-address resolver-serialize-address)
  (deserialize-address resolver-deserialize-address))

(define (name->resolver name)
  (let ((module `(disarchive resolvers ,name)))
    (module-ref (resolve-interface module)
                (symbol-append name '-resolver))))

(define %resolvers
  (delay (map name->resolver
              '(swhid))))

(define (lookup-resolver name)
  (find (lambda (resolver)
          (eq? (resolver-name resolver) name))
        (force %resolvers)))

(define (resolve-address address output)
  (match-let* (((name payload) address)
               (resolver (lookup-resolver name)))
    ((resolver-resolve-address resolver) payload output)))

(define (file-addresses filename)
  (map (lambda (resolver)
         `(,(resolver-name resolver)
           ,((resolver-file-address resolver) filename)))
       (force %resolvers)))

(define (serialize-address address)
  (match-let* (((name payload) address)
               (resolver (lookup-resolver name)))
    `(,name ,((resolver-serialize-address resolver) payload))))

(define (deserialize-address obj)
  (match-let* (((name payload-obj) obj)
               (resolver (lookup-resolver name)))
    `(,name ,((resolver-deserialize-address resolver) payload-obj))))

(define (resolve-addresses addresses output)
  (let ((count (length addresses)))
    (if (= count 1)
        (message "Checking 1 address")
        (message "Checking ~a addresses" count)))
  (any (lambda (address)
         (match address
           ((name _) (start-message "  ~a... " name)))
         (if (resolve-address address output)
             (begin (message "yes!") #t)
             (begin (message "no" #f))))
       addresses))

;; In the future, 'resolve-addresses' could be the default resolver,
;; but right now we only know how to resolve SWHIDs via Guix.  In an
;; effort to avoid Guix as a dependency, the default resolver just
;; fails, with the expectation that clients will provide their own
;; resolver.
(define %resolve-addresses
  (make-parameter (const #f)))
