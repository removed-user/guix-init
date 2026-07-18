;;; Disarchive
;;; Copyright © 2020-2022 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive)
  #:use-module (disarchive assemblers)
  #:use-module (disarchive config)
  #:use-module (disarchive digests)
  #:use-module (disarchive disassemblers)
  #:use-module (disarchive logging)
  #:use-module (disarchive resolvers)
  #:use-module (disarchive utils)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-34)
  #:re-export (%disarchive-log-port)
  #:export (specification->blueprint
            disarchive-assemble
            disarchive-disassemble))

;;; Commentary:
;;;
;;; This module provides a high-level interface into Disarchive.
;;;
;;; Code:

(define (wrap-blueprint serial)
  `(disarchive
    (version 0)
    ,serial))

(define (unwrap-blueprint obj)
  (match obj
    (('disarchive ('version 0) serial) serial)
    (_ (error "Invalid Disarchive wrapper"))))

(define (specification->blueprint specification)
  (match specification
    ((? string?) (specification->blueprint
                  (call-with-input-file specification read)))
    ((? port?) (specification->blueprint (read specification)))
    (_ (deserialize-blueprint (unwrap-blueprint specification)))))

(define* (disarchive-assemble specification out
                              #:key (resolver (%resolve-addresses)))
  "Assemble the archive described by SPECIFICATION and write it to OUT.
SPECIFICATION can be either a value returned by 'disarchive-disassemble'
or a filename or port that contains such a value.  If OUT is a filename,
the result will be written there.  Otherwise, OUT must be an output
port.

If RESOLVER is set, it will be used to resolve directory references.
It must be a two-argument procedure that takes a list of addresses
that refer to the same content and the name of an output directory.
The RESOLVE procedure needs to obtain that content and write it to the
given output directory."
  (let ((blueprint (specification->blueprint specification)))
    (call-with-temporary-directory
     (lambda (workspace)
       (guard (exn ((assembly-error? exn)
                    (when (exception-with-message? exn)
                      (message (exception-message exn)))
                    #f))
         (parameterize ((%resolve-addresses resolver))
           (assemble blueprint workspace))
         (let* ((digest (blueprint-digest blueprint))
                (result (digest->filename digest workspace)))
           (match out
             ((? output-port?)
              (message "Writing result to output port")
              (call-with-input-file result
                (lambda (port)
                  (dump-port-all port out))))
             ((? string?)
              (message "Copying result to ~a" out)
              (copy-file result out))
             (_ (scm-error 'wrong-type-arg "disarchive-assemble"
                           "Wrong type (expecting string or port): ~A"
                           (list out) (list out)))))
         #t)))))

(define* (disarchive-disassemble filename #:optional
                                 (algorithm (hash-algorithm sha256))
                                 #:key name)
  "Disassemble FILENAME into a Disarchive specification.  If ALGORITHM
is set, use it instead of the default (SHA-256).  Normally, the
filename is used for the specification name.  If this is wrong, it can
be corrected explicitly with NAME."
  (call-with-temporary-directory
   (lambda (workspace)
     (parameterize ((%disarchive-directory-cache workspace))
       (let ((blueprint (disassemble filename algorithm #:name name)))
         (message "Finished disassembly of ~a" filename)
         (start-message "Checking that it can be assembled... ")
         (without-logging
          (assemble blueprint workspace))
         (message "ok")
         (let ((serial (serialize-blueprint blueprint)))
           (start-message "Checking that it can be deserialized... ")
           (let ((blueprint* (without-logging
                              (deserialize-blueprint serial))))
             (if (equal? blueprint blueprint*)
                 (message "ok")
                 (error "the deserialized value differs from the original")))
           (wrap-blueprint serial)))))))
