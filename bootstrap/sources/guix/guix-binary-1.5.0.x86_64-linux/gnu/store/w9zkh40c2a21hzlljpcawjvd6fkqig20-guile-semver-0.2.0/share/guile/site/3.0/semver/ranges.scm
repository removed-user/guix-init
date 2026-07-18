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

(define-module (semver ranges)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg simplify-tree)
  #:use-module (semver)
  #:use-module (semver partial)
  #:use-module (srfi srfi-1)
  #:export (semver-slice
            semver-range
            *semver-range-empty*
            *semver-range-any*
            semver-range-contains?
            string->semver-range))

;;; Commentary:
;;;
;;; This module deals with ranges of Semantic Versions (SemVers).  The
;;; semantics and external represntation of SemVer ranges comes from
;;; NodeJS Package Manager, and is described at
;;; <https://github.com/npm/node-semver>.
;;;
;;; Code:

(define* (semver-slice #:optional (major '*) (minor '*) (patch '*) (revision '*)
                       #:key (pre '()) (build '()))
  "Create a set of SemVers (a \"slice\") using wildcards."
  (cond
   ((eq? major '*) '*)
   ((eq? minor '*) (cons (make-semver major)
                         (make-semver (1+ major))))
   ((eq? patch '*) (cons (make-semver major minor)
                         (make-semver major (1+ minor))))
   ((eq? revision '*) (cons (make-semver major minor patch)
                            (make-semver major minor (1+ patch))))
   (else (make-semver major minor patch revision #:pre pre #:build build))))

(define *comparators-none* `((< ,(make-semver))))

(define *comparators-any* '())

(define (comparators slice)
  "Convert SLICE into a list of comparators."
  (match slice
    ('* *comparators-any*)
    ((? semver?) `((= ,slice)))
    (((? semver? a) . (? semver? b))
     `((>= ,a) (< ,b)))))

(define *ineq-table*
  `((< ,*comparators-none* ,car <)
    (> ,*comparators-none* ,cdr >=)
    (<= ,*comparators-any* ,cdr <=)
    (>= ,*comparators-any* ,car >=)))

(define (ineq-comparators ineq slice)
  "Return the list of comparators that represents all SemVers that
satisfy the inequaility relation INEQ with SLICE.  The relation INEQ
must be one of the following symbols: `<', `>', `<=', or `>='."
  (match-let (((star selector rator) (assoc-ref *ineq-table* ineq)))
    (match slice
      ('* star)
      ((? semver?) `((,ineq ,slice)))
      (((? semver?) . (? semver?))
       `((,rator ,(selector slice)))))))

(define (semver-next-minor sv)
  "Increment the minor-level of SV and reset its patch level to zero."
  (match-let ((($ <semver> major minor patch _ _) sv))
    (make-semver major (1+ minor))))

(define (tilde-comparators slice)
  "If SLICE is a sinlge SemVer, return the list of comparators that
represents all SemVers that only differ from SLICE by patch-level.
Otherwise, convert SLICE to a list of comparators (this is the same as
calling `(comparators SLICE)')."
  (match slice
    ((? semver?)
     `((>= ,slice) (< ,(semver-next-minor slice))))
    (_ (comparators slice))))

(define (semver-incr-nonzero sv)
  "Increment the first nonzero digit of SV."
  (define nonzero? (negate zero?))
  (match-let ((($ <semver> major minor patch _ _) sv))
    (cond
     ((nonzero? major) (make-semver (1+ major)))
     ((nonzero? minor) (make-semver major (1+ minor)))
     ((nonzero? patch) (make-semver major minor (1+ patch))))))

(define (caret-comparators slice)
  "If SLICE is a sinlge SemVer, return the list of comparators that
represents all SemVers that have the same left-most nonzero digit as
SLICE.  Otherwise, convert SLICE to a list of comparators (this is the
same as calling `(comparators SLICE)')."
  (define (nonzero-semver? sv)
    (and (semver? sv) (not (semver=? sv (make-semver 0 0 0)))))

  (match slice
    ((? nonzero-semver?)
     `((>= ,slice) (< ,(semver-incr-nonzero slice))))
    (((? nonzero-semver? sv1) . (? semver? sv2))
     `((>= ,sv1) (< ,(semver-incr-nonzero sv1))))
    (_ (comparators slice))))

(define (hyphen-comparators slice1 slice2)
  "Return the list of comparators that represents all SemVers between
and including SLICE1 and SLICE2."
  (append (match slice1
            ('* *comparators-any*)
            ((? semver?) `((>= ,slice1)))
            (((? semver? sv) . (? semver?)) `((>= ,sv))))
          (match slice2
            ('* *comparators-any*)
            ((? semver?) `((<= ,slice2)))
            (((? semver?) . (? semver? sv)) `((< ,sv))))))

(define (primitive->comparators primitive)
  "Convert PRIMITIVE to a list of comparators.  Here, PRIMITIVE is an
S-Expression short-hand for the comparators constructors.  For
example, `'(~ SLICE)' is converted using `(tilde-comparators SLICE)'."
  (match primitive
    (('= slice) (comparators slice))
    (('~ slice) (tilde-comparators slice))
    (('^ slice) (caret-comparators slice))
    ((ineq slice) (ineq-comparators ineq slice))
    (('- slice1 slice2) (hyphen-comparators slice1 slice2))
    (_ (comparators primitive))))

(define (semver-range . primitive-lists)
  "Construct a SemVer range from PRIMITIVE-LISTS, which is a list of
lists of primitives.

The syntax of PRIMITIVE-LISTS should be

  <primitive-lists> ::= (<primitive-list> ..1)
  <primitive-list>  ::= (<primitive> ...)
  <primitive>       ::= <slice>
                      | (= <slice>)
                      | (< <slice>)
                      | (> <slice>)
                      | (<= <slice>)
                      | (>= <slice>)
                      | (~ <slice>)
                      | (^ <slice>)
                      | (- <slice> <slice>)
  <slice>           ::= *
                      | (? semver?)
                      | ((? semver?) . (? semver?))"
  (map (lambda (primitives)
         (append-map primitive->comparators primitives))
       primitive-lists))

(define *semver-range-empty*
  (list *comparators-none*))

(define *semver-range-any*
  (list *comparators-any*))

(define *semver-relations*
  `((= . ,semver=?)
    (< . ,semver<?)
    (<= . ,semver<=?)
    (> . ,semver>?)
    (>= . ,semver>=?)))

(define (match-comparator comparator semver)
  "Check if COMPARATOR is satisfied by SEMVER."
  (match-let* (((operator reference-semver) comparator)
               (proc (assq-ref *semver-relations* operator)))
    (proc semver reference-semver)))

(define (same-major-minor-patch? sv1 sv2)
  "Check if SV1 and SV2 have the same values (respectively) for their
major, minor, and patch version components."
  (match-let ((($ <semver> major1 minor1 patch1 _ _) sv1)
              (($ <semver> major2 minor2 patch2 _ _) sv2))
    (every = (list major1 minor1 patch1) (list major2 minor2 patch2))))

(define* (semver-range-contains? semver-range semver
                                 #:key include-prerelease?)
  "Check if SEMVER-RANGE contains SEMVER."
  (any (lambda (comparators)
         (and (every (lambda (comparator)
                       (match-comparator comparator semver))
                     comparators)
              (or include-prerelease?
                  (null? (semver-pre semver))
                  (any (match-lambda
                         ((_ csemver)
                          (and (same-major-minor-patch? semver csemver)
                               (not (null? (semver-pre csemver))))))
                       comparators))))
       semver-range))

(define (semver-range-intersection svr1 svr2)
  "Compute the intersection of the two SemVer ranges SVR1 and SVR2."
  (append-map (lambda (comparators1)
                (map (lambda (comparators2)
                       (append comparators1 comparators2))
                     svr2))
              svr1))

(define* (string-split/string s1 s2 #:optional
                              (start1 0) (end1 (string-length s1))
                              (start2 0) (end2 (string-length s2)))
  "Split S1 into parts delimited by S2."
  (let loop ((start start1) (acc '()))
    (match (string-contains s1 s2 start end1 start2 end2)
      (#f (reverse! (cons (substring s1 start end1) acc)))
      (ix (loop (+ ix (- end2 start2))
                (cons (substring s1 start ix) acc))))))

(define (string-split/1 s char_pred)
  "Split S1 at the first character that satisfies CHAR_PRED (a
character, character set, or predicate).  If there is no character
that satisfies CHAR_PRED, return S unchanged."
  (match (string-index s char_pred)
    (#f s)
    (ix (list (substring s 0 ix) (substring s (1+ ix))))))

(define (string->semver-range str)
  "Return the SemVer range represented by the string STR.  If STR is
not a syntactically valid notation for a SemVer range, then
`string->semver-range' returns `#f'."

  ;; Make sure that no symbol is a prefix of another that appears later!
  (define operators '(<= >= = < > ~> ~ ^))

  (define operator-strings (map symbol->string operators))

  (define operator-chars
    (apply char-set-union (map string->char-set operator-strings)))

  (define (string->semver-slice str)
    (match (string->partial-semver str)
      ((major minor patch revision (pre-parts ...) (build-parts ...))
       (semver-slice major minor patch revision #:pre pre-parts #:build build-parts))
      (#f #f)))

  (define (string->comparator str)
    (or (any (lambda (op)
               (let* ((key (symbol->string op))
                      (len (string-length key)))
                 (and (string-prefix? key str)
                      (and=> (string->semver-slice (substring str len))
                             (lambda (svs)
                               (list (if (eq? op '~>) '~ op) svs))))))
             operators)
        (string->semver-slice str)))

  ;; This goes beyond what the documentation calls a valid range.
  ;; However, some packages in the NPM registry put spaces after
  ;; operators, so we have to deal with it.
  (define (split-comparator-set str)
    (define (is-operator? x)
      (member x operator-strings))

    (define (resolve-operators op1 op2)
      (let ((op1-special? (member op1 '("~" "~>" "^")))
            (op2-special? (member op2 '("~" "~>" "^"))))
        (cond
         ((and op1-special? op2-special?) #f)
         (op1-special? op1)
         (else op2))))

    (define non-delimiters (char-set-complement (char-set #\space #\,)))

    (let loop ((xs (string-tokenize str non-delimiters)) (acc '()))
      (match xs
        (((? is-operator? op1) (? is-operator? op2) . rest)
         (and=> (resolve-operators op1 op2)
                (lambda (op) (loop (cons op rest) acc))))
        (((? is-operator? op1) x . rest)
         (match (find (lambda (op) (string-prefix? op x)) operator-strings)
           (#f (loop rest (cons (string-append op1 x) acc)))
           (op2 (and=> (resolve-operators op1 op2)
                       (lambda (op)
                         (let ((x-rest (substring x (string-length op2))))
                           (loop rest (cons (string-append op x-rest)
                                            acc))))))))
        ((x . rest) (loop rest (cons x acc)))
        (() (reverse! acc)))))

  (define (string->comparator-set str)
    (match (string-contains str " - ")
      (#f (if (string-null? str)
              *comparators-any*
              (let* ((comparator-strings (split-comparator-set str))
                     (x (and comparator-strings
                             (map string->comparator comparator-strings))))
                (and x (every values x) x))))
      (ix (let ((c1 (string->comparator (substring str 0 ix)))
                (c2 (string->comparator (substring str (+ ix 3)))))
            (and c1 c2
                 (match c1
                   ((op svs) (list c1 c2))
                   (_ (match c2
                        ((op svs) (list c1 c2))
                        (_ `((- ,c1 ,c2)))))))))))

  (let ((x (map (compose string->comparator-set string-trim-both)
                (string-split/string str "||"))))
    (and (every values x)
         (apply semver-range x))))
