;;; Disarchive
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (disarchive git-hash)
  #:use-module (disarchive utils)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (git-hash-file
            git-hash-directory))

;;; Commentary:
;;;
;;; This module provides functions for hashing files and directories in
;;; the style of Git.  The hope is that these hashes can be used to find
;;; data in the Software Heritage archive.  Hence, the standard of
;;; correctness is that the results match Software Heritage.
;;;
;;; Code:

(define (write-git-hash-header port type size)
  "Construct a Git hash header from TYPE and SIZE, and write it to
PORT."
  (display type port)
  (display #\space port)
  (display size port)
  (display #\nul port))

(define* (git-hash-blob bv #:optional
                        (algorithm (hash-algorithm sha1)))
  "Compute the Git hash of BV.  If ALGORITHM is set, compute hashes
using ALGORITHM.  Otherwise, use SHA-1."
  (let ((out get-hash (open-hash-port algorithm)))
    (write-git-hash-header out "blob" (bytevector-length bv))
    (put-bytevector out bv)
    (force-output out)
    (get-hash)))

(define* (git-hash-file* filename st #:optional
                         (algorithm (hash-algorithm sha1)))
  "Compute the Git hash of FILENAME (a regular file).  The ST
parameter must be the object returned by '(stat FILENAME)'.  If
ALGORITHM is set, compute hashes using ALGORITHM.  Otherwise, use
SHA-1."
  (let ((out get-hash (open-hash-port algorithm)))
    (write-git-hash-header out "blob" (number->string (stat:size st)))
    (call-with-input-file filename (cut dump-port-all <> out))
    (force-output out)
    (get-hash)))

(define* (git-hash-file filename #:optional
                        (algorithm (hash-algorithm sha1)))
  "Compute the Git hash of FILENAME (a regular file).  If ALGORITHM is
set, compute hashes using ALGORITHM.  Otherwise, use SHA-1."
  (git-hash-file* filename (stat filename) algorithm))

(define (make-tree-node mode name hash)
  "Serialize the bytevectors MODE, NAME, and HASH into a Git tree node."
  (let* ((name-offset (+ (bytevector-length mode) 1))
         (hash-offset (+ name-offset (bytevector-length name) 1))
         (node (make-bytevector (+ hash-offset (bytevector-length hash)))))
    (bytevector-copy! mode 0 node 0 (bytevector-length mode))
    (bytevector-u8-set! node (1- name-offset) #x20)
    (bytevector-copy! name 0 node name-offset (bytevector-length name))
    (bytevector-u8-set! node (1- hash-offset) 0)
    (bytevector-copy! hash 0 node hash-offset (bytevector-length hash))
    node))

(define (%read-tree-node filename algorithm select?)
  "Read the file at FILENAME and turn it into a Git tree node.  The file
may be a regular file, directory, or symlink.  Hashes will be computed
using ALGORITHM."
  (let ((st (lstat filename))
        (name (basename filename)))
    (and (select? filename st)
         (case (stat:type st)
           ((regular)
            (make-tree-node (if (zero? (bit-extract (stat:perms st) 6 7))
                                (string->utf8 "100644")
                                (string->utf8 "100755"))
                            (string->utf8 name)
                            (git-hash-file* filename st algorithm)))
           ((directory)
            (make-tree-node (string->utf8 "40000")
                            (string->utf8 name)
                            (git-hash-directory filename algorithm)))
           ((symlink)
            (make-tree-node (string->utf8 "120000")
                            (string->utf8 name)
                            (git-hash-blob (string->utf8 (readlink filename))
                                           algorithm)))))))

;; XXX: Guile 3 seems to fail when optimizing the call from
;; 'git-hash-directory' to 'read-tree-node'.  It decides not to use
;; 'read-tree-node' and just calls 'git-hash-directory' (recursively)
;; instead.  Since 'read-tree-node' does a lot of useful work,
;; everything breaks when Guile 3 does this.  The following indirection
;; tricks the compiler into doing the right thing.
(define read-tree-node (car (list %read-tree-node)))

(define (tree-node-directory? node)
  "Check if NODE is a directory tree node."
  (let loop ((k 0) (digits '(#x34 #x30 #x30 #x30 #x30)))
    (match digits
      (() #t)
      ((digit . rest)
       (and (= (bytevector-u8-ref node k) digit) (loop (1+ k) rest))))))

(define (tree-node-name-index node)
  "Get the index of the name field of NODE."
  (let loop ((k 0))
    (if (= (bytevector-u8-ref node k) #x20)
        (1+ k)
        (loop (1+ k)))))

(define (tree-node-hash-index node)
  "Get the index of the hash field of NODE."
  (let loop ((k 0))
    (if (= (bytevector-u8-ref node k) 0)
        (1+ k)
        (loop (1+ k)))))

(define (display-tree-node node)
  "Write a representation of NODE to the current output port."
  (let* ((name-index (tree-node-name-index node))
         (mode (make-bytevector (1- name-index)))
         (hash-index (tree-node-hash-index node))
         (name (make-bytevector (- hash-index name-index 1)))
         (hash (make-bytevector (- (bytevector-length node) hash-index))))
    (bytevector-copy! node 0 mode 0 (bytevector-length mode))
    (bytevector-copy! node name-index name 0 (bytevector-length name))
    (bytevector-copy! node hash-index hash 0 (bytevector-length hash))
    (display (utf8->string mode))
    (display " ")
    (display (utf8->string name))
    (display " ")
    (display ((@ (guix base16) bytevector->base16-string) hash))
    (newline)))

(define (tree-node<? n1 n2)
  "Check if the name of N1 comes before N2 when sorting
lexicographically."
  (let loop ((k1 (tree-node-name-index n1)) (k2 (tree-node-name-index n2)))
    (let* ((b1 (bytevector-u8-ref n1 k1))
           (b1* (if (and (zero? b1) (tree-node-directory? n1)) #x2f b1))
           (b2 (bytevector-u8-ref n2 k2))
           (b2* (if (and (zero? b2) (tree-node-directory? n2)) #x2f b2)))
      (cond
       ((< b1* b2*) #t)
       ((> b1* b2*) #f)
       (else (and (> b1 0)
                  (> b2 0)
                  (loop (1+ k1) (1+ k2))))))))

(define* (git-hash-directory directory #:optional
                             (algorithm (hash-algorithm sha1))
                             #:key (select? (const #t)))
  "Compute the Git-style hash of DIRECTORY.  If ALGORITHM is set,
compute hashes using ALGORITHM.  Otherwise, use SHA-1.  Note that by
default, the result will include empty directories, which Git itself
would ignore.  However, you can control exactly which files are
included by specifying a SELECT? predicate that takes two arguments, a
filename and a stat object."
  (let* ((filenames (remove (cut member <> '("." "..")) (scandir directory)))
         (filenames* (map (cut string-append directory "/" <>) filenames))
         (nodes (sort (filter-map (cut read-tree-node <> algorithm select?)
                                  filenames*)
                      tree-node<?))
         (len (fold + 0 (map bytevector-length nodes))))
    ;;(format #t "--==-- ~a --==--~%" directory)
    ;;(for-each display-tree-node nodes)
    (let ((out get-hash (open-hash-port algorithm)))
      (write-git-hash-header out "tree" len)
      (for-each (cut put-bytevector out <>) nodes)
      (force-output out)
      (get-hash))))
