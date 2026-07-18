;;; Disarchive
;;; Copyright © 2020, 2023 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive disassemblers)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (<disassembler>
            make-disassembler
            disassembler?
            disassembler-x-file?
            disassembler-disassemble-x
            disassemble))

;;; Commentary:
;;;
;;; This module provides a generalized interface for disassemblers.  A
;;; disassembler is a procedure that takes a filename and disassembles
;;; that file into its metadata and a reference to its data.
;;;
;;; Code:

(define-record-type <disassembler>
  (make-disassembler x-file? disassemble-x)
  disassembler?
  (x-file? disassembler-x-file?)
  (disassemble-x disassembler-disassemble-x))

(define (name->disassembler name)
  (let ((module `(disarchive assemblers ,name)))
    (module-ref (resolve-interface module)
                (symbol-append name '-disassembler))))

(define %disassemblers
  (delay (map name->disassembler
              '(gzip-member
                xz-file
                bzip2-stream
                tarball
                directory-ref))))

(define (file-disassembler filename)
  "Get the disassembler for the file named FILENAME."
  (define st (stat filename))
  (or (find (lambda (dasm)
              ((disassembler-x-file? dasm) filename st))
            (force %disassemblers))
      (error "No disassembler for file")))

(define* (disassemble filename #:optional
                      (algorithm (hash-algorithm sha256))
                      #:key name)
  (match-let ((($ <disassembler> _ disassemble-x)
               (file-disassembler filename)))
    (apply disassemble-x filename algorithm
           (if name `(#:name ,name) '()))))
