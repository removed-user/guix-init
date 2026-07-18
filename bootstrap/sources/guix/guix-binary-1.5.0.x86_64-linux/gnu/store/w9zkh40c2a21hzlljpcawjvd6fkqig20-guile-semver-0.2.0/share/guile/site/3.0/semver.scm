;;; Guile-SemVer
;;; Copyright 2019 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Guile-SemVer.
;;;
;;; Guile-SemVer is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-SemVer is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-SemVer.  If not, see <https://www.gnu.org/licenses/>.

(define-module (semver)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (semver partial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (<semver>
            make-semver
            semver?
            semver-major
            semver-minor
            semver-patch
            semver-pre
            semver-build
            string->semver
            semver->string
            semver=?
            semver<?
            semver>?
            semver<=?
            semver>=?))

;;; Commentary:
;;;
;;; This module is all about the representation and comparison of
;;; Semantic Versions (SemVers).  Hopefully everything follows the
;;; standard at <https://semver.org/spec/v2.0.0.html>.
;;;
;;; Code:

(define-record-type <semver>
  (%make-semver major minor patch revision pre build)
  semver?
  (major semver-major)
  (minor semver-minor)
  (patch semver-patch)
  (revision semver-revision)
  (pre semver-pre)
  (build semver-build))

(define* (make-semver #:optional (major 0) (minor 0) (patch 0) (revision 0)
                      #:key (pre '()) (build '()))
  "Make a SemVer with major version MAJOR, minor version MINOR, and
patch version PATCH (all of which must be nonnegative integers).  The
key PRE can be set to a list of identifiers to include a pre-release
version.  The key BUILD can be set to a list of identifiers to include
build metadata.  A list of identifiers is a list made of nonegative
integers and strings containing only ASCII alphanumeric characters and
hyphens."
  (%make-semver major minor patch revision pre build))

(define (string->semver str)
  "Return the SemVer represented by the string STR.  If STR is not a
syntactically valid notation for a SemVer, then `string->semver'
returns `#f'."
  (match (string->partial-semver str)
    ;; nuget has MAJOR.MINOR.PATCH.REVISION with REVISION defaulting to 0 if missing.
    ;; See <https://learn.microsoft.com/en-us/nuget/concepts/package-versioning?tabs=semver20sort>.
    (((? number? major) (? number? minor) (? number? patch) (? number? revision)
      (pre-parts ...) (build-parts ...))
     (make-semver major minor patch revision #:pre pre-parts #:build build-parts))
    (((? number? major) (? number? minor) (? number? patch)
      (pre-parts ...) (build-parts ...))
     (make-semver major minor patch #:pre pre-parts #:build build-parts))
    (_ #f)))

(define (semver->string sv)
  "Return a string holding the external representation of the SemVer
SV."
  (define (part->string part)
    (match part
      ((? number?) (number->string part))
      ((? string?) part)))
  (match-let* ((($ <semver> major minor patch revision pre build) sv)
               (pre (string-join (map part->string pre) "."))
               (build (string-join (map part->string build) ".")))
    (string-append
     (string-join (map number->string (list major minor patch)) ".")
     (if (= revision 0)
         ""
         (string-append "." (number->string revision)))
     (if (string-null? pre) "" (string-append "-" pre))
     (if (string-null? build) "" (string-append "+" build)))))

(set-record-type-printer!
 <semver>
 (lambda (semver port)
   (format port "#<semver ~a>" (semver->string semver))))

(define (semver=? sv1 sv2)
  "Return `#f' if SV1 and SV2 are not equal, a true value otherwise."
  (match-let ((($ <semver> major1 minor1 patch1 revision1 pre1 _) sv1)
              (($ <semver> major2 minor2 patch2 revision2 pre2 _) sv2))
    (and (= major1 major2)
         (= minor1 minor2)
         (= patch1 patch2)
         (= revision1 revision2)
         (list= equal? pre1 pre2)))) ; TODO: case insensitive, apparently.

(define-syntax-rule (%make-list<-or-list<= doc equal-value)
  (lambda (elt= elt< xs ys)
    doc
    (let loop ((xs xs) (ys ys))
      (match xs
        (() (if (null? ys) equal-value #t))
        ((x . xs)
         (match ys
           (() #f)
           ((y . ys)
            (cond
             ((elt< x y) #t)
             ((elt= x y) (loop xs ys))
             (else #f)))))))))

(define list<
  (%make-list<-or-list<=
   "Return `#f' if XS is greater than or equal to YS, a true value
otherwise.  The two lists are compared lexicographically, using ELT=
to test element equality and ELT< to compare elements."
   #f))

(define list<=
  (%make-list<-or-list<=
   "Return `#f' if XS is greater than YS, a true value otherwise.  The
two lists are compared lexicographically, using ELT= to test element
equality and ELT< to compare elements."
   #t))

(define (part<? a b)
  (match (cons a b)
    (((? number?) . (? number?)) (< a b))
    (((? number?) . (? string?)) #t)
    (((? string?) . (? number?)) #f)
    (((? string?) . (? string?)) (string<? a b))))

(define-syntax-rule (%make-semver<-or-semver<=? doc pre-compare)
  (lambda (sv1 sv2)
    doc
    (match-let* ((($ <semver> major1 minor1 patch1 revision1 pre1 _) sv1)
                 (($ <semver> major2 minor2 patch2 revision2 pre2 _) sv2)
                 (numbers1 (list major1 minor1 patch1 revision1))
                 (numbers2 (list major2 minor2 patch2 revision2)))
      (or (list< = < numbers1 numbers2)
          (and (list= = numbers1 numbers2)
               (pre-compare pre1 pre2))))))

(define semver<?
  (let ((pre<? (lambda (a b)
                 (cond
                  ((null? a) #f)
                  ((null? b) #t)
                  (else (list< equal? part<? a b))))))
    (%make-semver<-or-semver<=?
     "Return `#f' if SV1 is greater than or equal to SV2, a true value
otherwise."
     pre<?)))

(define semver<=?
  (let ((pre<=? (lambda (a b)
                  (cond
                   ((null? a) (null? b))
                   ((null? b) #t)
                   (else (list<= equal? part<? a b))))))
    (%make-semver<-or-semver<=?
     "Return `#f' if SV1 is greater than SV2, a true value otherwise."
     pre<=?)))

(define (semver>? sv1 sv2)
  "Return `#f' if SV1 is less than or equal to SV2, a true value
otherwise."
  (not (semver<=? sv1 sv2)))

(define (semver>=? sv1 sv2)
  "Return `#f' if SV1 is less than SV2, a true value otherwise."
  (not (semver<? sv1 sv2)))

