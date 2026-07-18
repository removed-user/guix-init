;;; guile-gcrypt --- crypto tooling for guile
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of guile-gcrypt.
;;;
;;; guile-gcrypt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; guile-gcrypt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with guile-gcrypt.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gcrypt internal)
  #:use-module (gcrypt package-config)
  #:use-module (system foreign)
  #:export (libgcrypt->pointer
            libgcrypt->procedure

            define-enumerate-type
            define-lookup-procedure

            gcrypt-version))

;;; Code:
;;;
;;; This module provides tools for internal use.  The API of this module may
;;; change anytime; you should not rely on it.  Loading this module
;;; initializes Libgcrypt as a side effect.
;;;
;;; Comment:

(define (libgcrypt->pointer name)
  "Return a pointer to symbol FUNC in libgcrypt."
  (catch #t
    (lambda ()
      (dynamic-func name (dynamic-link %libgcrypt)))
    (lambda args
      (lambda _
        (throw 'system-error name  "~A" (list (strerror ENOSYS))
               (list ENOSYS))))))

(define (libgcrypt->procedure return name params)
  "Return a pointer to symbol FUNC in libgcrypt."
  (catch #t
    (lambda ()
      (let ((ptr (dynamic-func name (dynamic-link %libgcrypt))))
        ;; The #:return-errno? facility was introduced in Guile 2.0.12.
        (pointer->procedure return ptr params
                            #:return-errno? #t)))
    (lambda args
      (lambda _
        (throw 'system-error name  "~A" (list (strerror ENOSYS))
               (list ENOSYS))))))

(define-syntax-rule (define-enumerate-type name->integer symbol->integer
                      integer->symbol
                      (name id) ...)
  (begin
    (define-syntax name->integer
      (syntax-rules (name ...)
        "Return hash algorithm NAME."
        ((_ name) id) ...))

    (define symbol->integer
      (let ((alist '((name . id) ...)))
        (lambda (symbol)
          "Look up SYMBOL and return the corresponding integer or #f if it
could not be found."
          (assq-ref alist symbol))))

    (define-lookup-procedure integer->symbol
      "Return the name (a symbol) corresponding to the given integer value."
      (id name) ...)))

(define-syntax define-lookup-procedure
  (lambda (s)
    "Define LOOKUP as a procedure that maps an integer to its corresponding
value in O(1)."
    (syntax-case s ()
      ((_ lookup docstring (index value) ...)
       (let* ((values (map cons
                           (syntax->datum #'(index ...))
                           #'(value ...)))
              (min    (apply min (syntax->datum #'(index ...))))
              (max    (apply max (syntax->datum #'(index ...))))
              (array  (let loop ((i max)
                                 (result '()))
                        (if (< i min)
                            result
                            (loop (- i 1)
                                  (cons (or (assv-ref values i) *unspecified*)
                                        result))))))
         #`(define lookup
             ;; Allocate a big sparse vector.
             (let ((values '#(#,@array)))
               (lambda (integer)
                 docstring
                 (and (<= integer #,max) (>= integer #,min)
                      (let ((result (vector-ref values (- integer #,min))))
                        (if (unspecified? result)
                            #f
                            result)))))))))))

(define gcrypt-version
  ;; According to the manual, this function must be called before any other,
  ;; and it's not clear whether it can be called more than once.  So call it
  ;; right here from the top level.  During cross-compilation, the call to
  ;; PROC fails with a 'system-error exception; catch it.
  (let* ((proc    (libgcrypt->procedure '* "gcry_check_version" '(*)))
         (version (catch 'system-error
                    (lambda ()
                      (pointer->string (proc %null-pointer)))
                    (const ""))))
    (lambda ()
      "Return the version number of libgcrypt as a string."
      version)))
