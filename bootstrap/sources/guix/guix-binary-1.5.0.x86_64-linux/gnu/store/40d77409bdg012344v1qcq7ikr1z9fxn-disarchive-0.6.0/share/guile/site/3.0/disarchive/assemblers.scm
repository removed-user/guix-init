;;; Disarchive
;;; Copyright © 2020, 2021, 2023 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive assemblers)
  #:use-module (disarchive digests)
  #:use-module (disarchive logging)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (<assembler>
            make-assembler
            assembler?
            assembler-x?
            assembler-x-name
            assembler-x-digest
            assembler-x-inputs
            assembler-serialize-x
            assembler-serialized-x?
            assembler-deserialize-x
            assembler-assemble-x

            &assembly-error
            make-assembly-error
            assembly-error?
            assembly-error

            blueprint-name
            blueprint-digest
            blueprint-inputs
            serialize-blueprint
            deserialize-blueprint
            assemble))

;;; Commentary:
;;;
;;; This module provides a generalized interface for blueprints.  A
;;; blueprint is an object that describes how to produce an output
;;; that matches its digest.  Blueprints can also be serialized and
;;; deserialized.
;;;
;;; Code:

(define-record-type <assembler>
  (make-assembler x? x-name x-digest x-inputs
                  serialize-x serialized-x? deserialize-x
                  assemble-x)
  assembler?
  (x? assembler-x?)
  (x-name assembler-x-name)
  (x-digest assembler-x-digest)
  (x-inputs assembler-x-inputs)
  (serialize-x assembler-serialize-x)
  (serialized-x? assembler-serialized-x?)
  (deserialize-x assembler-deserialize-x)
  (assemble-x assembler-assemble-x))

(define-exception-type &assembly-error &error
  make-assembly-error
  assembly-error?)

(define-syntax-rule (assembly-error msg)
  (raise-exception (make-exception (make-assembly-error)
                                   (make-exception-with-message msg))))

(define (name->assembler name)
  (let ((module `(disarchive assemblers ,name)))
    (module-ref (resolve-interface module)
                (symbol-append name '-assembler))))

(define %assemblers
  (delay (map name->assembler
              '(gzip-member
                xz-file
                bzip2-stream
                tarball
                directory-ref))))

(define (blueprint-assembler blueprint)
  "Get the assembler for BLUEPRINT."
  (or (find (lambda (asm) ((assembler-x? asm) blueprint))
            (force %assemblers))
      (error "No assembler for blueprint")))

(define (serialized-assembler sexp)
  "Get the assembler for SEXP."
  (or (find (lambda (asm) ((assembler-serialized-x? asm) sexp))
            (force %assemblers))
      (error "No assembler for serialized object")))

(define (blueprint-name blueprint)
  "Get the name of BLUEPRINT."
  (match-let ((($ <assembler> x? x-name _ _ _ _ _ _)
               (blueprint-assembler blueprint)))
    (x-name blueprint)))

(define (blueprint-digest blueprint)
  "Get the digest of BLUEPRINT."
  (match-let ((($ <assembler> x? _ x-digest _ _ _ _ _)
               (blueprint-assembler blueprint)))
    (x-digest blueprint)))

(define (blueprint-inputs blueprint)
  "Get the inputs of BLUEPRINT."
  (match-let ((($ <assembler> x? _ _ x-inputs _ _ _ _)
               (blueprint-assembler blueprint)))
    (x-inputs blueprint)))

(define (serialize-blueprint blueprint)
  "Serialize BLUEPRINT."
  (match-let ((($ <assembler> x? _ _ _ serialize-x _ _ _)
               (blueprint-assembler blueprint)))
    (serialize-x blueprint)))

(define (deserialize-blueprint sexp)
  "Deserialize SEXP into a blueprint."
  (match-let ((($ <assembler> _ _ _ _ _ serialized-x? deserialize-x _)
               (serialized-assembler sexp)))
    (deserialize-x sexp)))

(define* (assemble blueprint workspace #:key (verify? #t))
  (match-let ((($ <assembler> x? x-name _ _ _ _ _ assemble-x)
               (blueprint-assembler blueprint)))
    (let* ((name (x-name blueprint))
           (digest (blueprint-digest blueprint))
           (out (digest->filename digest workspace)))
      (unless (and (file-exists? out) (file-digest? out digest))
        (for-each (cut assemble <> workspace)
                  (blueprint-inputs blueprint))
        (assemble-x blueprint workspace)
        (when verify?
          (start-message "Checking ~a digest... " name)
          (if (file-digest? out digest)
              (message "ok")
              (begin
                (message "fail")
                (assembly-error "Output is incorrect"))))))))
