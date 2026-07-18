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

(define-module (disarchive assemblers directory-ref)
  #:use-module (disarchive assemblers)
  #:use-module (disarchive config)
  #:use-module (disarchive digests)
  #:use-module (disarchive disassemblers)
  #:use-module (disarchive resolvers)
  #:use-module (disarchive logging)
  #:use-module (disarchive utils)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<directory-ref>
            make-directory-ref
            directory-ref?
            directory-ref-name
            directory-ref-addresses
            directory-ref-digest

            serialize-directory-ref
            serialized-directory-ref?
            deserialize-directory-ref

            directory-ref-file?
            disassemble-directory-ref

            directory-ref-assembler
            directory-ref-disassembler))

;;; Commentary:
;;;
;;; This module provides the means to construct a reference to a
;;; directory and, given that reference, restore the original
;;; directory.  Each reference contains a list of addresses that each
;;; represent the directory (i.e., content addressing).  There are
;;; many methods of hashing a directory and we store and use as many
;;; as we are able to.  See '(disarchive resolvers)' for more about
;;; the addresses supported here.
;;;
;;; Code:


;; Data

(define-record-type <directory-ref>
  (make-directory-ref name addresses digest)
  directory-ref?
  (name directory-ref-name)             ; string
  (addresses directory-ref-addresses)   ; list
  (digest directory-ref-digest))        ; <digest>

(define (serialize-directory-ref directory)
  (match-let ((($ <directory-ref> name addresses digest) directory))
    `(directory-ref
      (version 0)
      (name ,name)
      (addresses ,@(map serialize-address addresses))
      (digest ,(digest->sexp digest)))))

(define (serialized-directory-ref? sexp)
  (match sexp
    (('directory-ref _ ...) #t)
    (_ #f)))

(define (deserialize-directory-ref sexp)
  (match sexp
    (('directory-ref
      ('version 0)
      ('name name)
      ('addresses . address-sexps)
      ('digest digest-sexp))
     (let ((addresses (map deserialize-address address-sexps))
           (digest (sexp->digest digest-sexp)))
       (make-directory-ref name addresses digest)))
    (_ #f)))


;; Assembly

(define (assemble-directory-ref directory workspace)
  (match-let* ((($ <directory-ref> name addresses digest) directory)
               (output (digest->filename digest workspace)))
    (message "Assembling the directory ~a" name)
    (or (let* ((cache (%disarchive-directory-cache))
               (local (and cache (digest->filename digest cache))))
          (and local (directory-exists? local)
               (message "Found directory in cache: ~a" local)
               (copy-recursively local output
                                 #:log (%make-void-port "w"))))
        ((%resolve-addresses) addresses output)
        (assembly-error "Could not resolve directory reference"))))


;; Disassembly

(define (directory-ref-file? filename st)
  (eq? (stat:type st) 'directory))

(define* (disassemble-directory-ref directory #:optional
                                    (algorithm (hash-algorithm sha256))
                                    #:key (name (basename directory)))
  (message "Disassembling the directory ~a" name)
  (let* ((addresses (file-addresses directory))
         (digest (file-digest directory algorithm))
         (cache (%disarchive-directory-cache))
         (local (and cache (digest->filename digest cache))))
    (when (and local (not (directory-exists? local)))
      (message "Saving directory in cache: ~a" local)
      (mkdir-p local)
      (copy-recursively directory local #:log (%make-void-port "w")))
    (make-directory-ref name addresses digest)))


;; Interfaces

(define directory-ref-assembler
  (make-assembler directory-ref?
                  directory-ref-name
                  directory-ref-digest
                  (const '())
                  serialize-directory-ref
                  serialized-directory-ref?
                  deserialize-directory-ref
                  assemble-directory-ref))

(define directory-ref-disassembler
  (make-disassembler directory-ref-file?
                     disassemble-directory-ref))
