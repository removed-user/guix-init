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

(define-module (disarchive scripts disassemble)
  #:use-module (disarchive)
  #:use-module (disarchive config)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (disassemble-main))

(define options-grammar
  '((name (single-char #\n) (value #t))
    (output (single-char #\o) (value #t))
    (help (single-char #\h))
    (version)))

(define help-message "\
Usage: disarchive disassemble [OPTION]... FILE
Disassemble the archive FILE into its metadata specification.

Options:
  -n, --name=NAME    use NAME as the basename of the archive
  -o, --output=FILE  write the specification to FILE
  -h, --help         display this help and exit
  --version          display version information and exit
")

(define short-help-message "\
Run 'disarchive disassmeble --help' for usage instructions.
")

(define (getopt-long* . args)
  (catch 'quit
    (lambda ()
      (apply getopt-long args))
    (lambda (_ status)
      (display short-help-message (current-error-port))
      (exit status))))

(define (disassemble-main . args)
  (%disarchive-log-port (current-error-port))
  (let ((options (getopt-long* args options-grammar)))
    (when (option-ref options 'help #f)
      (display help-message)
      (exit EXIT_SUCCESS))
    (when (option-ref options 'version #f)
      (display version-message)
      (exit EXIT_SUCCESS))
    (match (option-ref options '() '())
      ((filename)
       (let ((port (or (and=> (option-ref options 'output #f)
                              open-output-file)
                       (current-output-port))))
         (match (option-ref options 'name #f)
           (#f (pretty-print (disarchive-disassemble filename) port))
           (name (pretty-print (disarchive-disassemble filename
                                                       #:name name)
                               port))))
       (exit EXIT_SUCCESS))
      (_
       (with-output-to-port (current-error-port)
         (lambda ()
           (display "disarchive disassemble: No input file specified\n")
           (display short-help-message)
           (exit EXIT_FAILURE)))))))
