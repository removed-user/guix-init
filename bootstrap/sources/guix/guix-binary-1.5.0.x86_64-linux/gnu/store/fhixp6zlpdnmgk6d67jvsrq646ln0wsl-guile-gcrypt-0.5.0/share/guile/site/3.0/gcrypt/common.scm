;;; guile-gcrypt --- crypto tooling for guile
;;; Copyright © 2013, 2014, 2015, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gcrypt common)
  #:use-module (gcrypt internal)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:re-export (gcrypt-version)
  #:export (gcrypt-error
            strip-error-source
            error-code=?
            error-source
            error-string))

;;; Commentary:
;;;
;;; Common code for the GNU Libgcrypt bindings.
;;;
;;; Code:

(define-syntax GPG_ERR_SOURCE_GCRYPT              ;from <gpg-error.h>
  (identifier-syntax 1))

(define-inlinable (strip-error-source error)
  "Strip the error source bits from ERROR, a libgpg-error error code."
  (logand error #xfffff))

(define-inlinable (gcrypt-error value)
  "Return VALUE as a libgpg-error code originating from Libgcrypt."
  (logior (ash GPG_ERR_SOURCE_GCRYPT 24)
          (strip-error-source value)))

(define-inlinable (error-code=? error1 error2)
  "Return true if ERROR1 and ERROR2 denote the same error code, regardless of
the error source."
  (= (strip-error-source error1) (strip-error-source error2)))

(define-syntax define-error-codes
  (syntax-rules ()
    "Define one variable for each error code given, using
GPG_ERR_SOURCE_GCRYPT as the error source."
    ((_ name value rest ...)
     (begin
       (define-public name value)
       (define-error-codes rest ...)))
    ((_)
     #t)))

;; GPG_ERR_ values of 'gpg_err_code_t' in <gpg-error.h>.
(define-error-codes
  error/no-error 0
  error/general 1
  error/unknown-packet 2
  error/unknown-version 3
  error/public-key-algo 4
  error/digest-algo 5
  error/bad-public-key 6
  error/bad-secret-key 7
  error/bad-signature 8
  error/no-public-key 9
  error/checksum 10
  error/bad-passphrase 11
  error/cipher-algo 12
  error/keyring-open 13
  error/invalid-packet 14
  error/invalid-armor 15
  error/no-user-id 16
  error/no-secret-key 17
  error/wrong-secret-key 18
  error/bad-key 19
  error/compr-algo 20
  error/no-prime 21
  error/no-encoding-method 22
  error/no-encryption-scheme 23
  error/no-signature-scheme 24
  error/invalid-attr 25
  error/no-value 26
  error/not-found 27
  error/value-not-found 28
  error/syntax 29
  error/bad-mpi 30
  error/invalid-passphrase 31
  error/sig-class 32
  error/resource-limit 33
  error/invalid-keyring 34
  error/trustdb 35
  error/bad-cert 36
  error/invalid-user-id 37
  error/unexpected 38
  error/time-conflict 39
  error/keyserver 40
  error/wrong-public-key-algo 41
  error/weak-key 43
  ;; The answer.
  error/invalid-key-length 44
  error/invalid-argument 45
  error/bad-uri 46
  error/invalid-uri 47
  error/network 48
  error/unknown-host 49
  error/selftest-failed 50
  error/not-encrypted 51
  error/not-processed 52
  error/unusable-public-key 53
  error/unusable-secret-key 54
  error/invalid-value 55
  error/bad-cert-chain 56
  error/missing-cert 57
  error/no-data 58
  error/bug 59
  error/not-supported 60
  error/invalid-op 61
  error/timeout 62
  error/internal 63
  error/eof-gcrypt 64
  error/invalid-object 65
  error/too-short 66
  error/too-large 67
  error/no-obj 68
  error/not-implemented 69
  error/conflict 70
  error/invalid-cipher-mode 71
  error/invalid-flag 72
  error/invalid-handle 73
  error/truncated 74
  error/incomplete-line 75
  error/invalid-response 76
  error/no-agent 77
  error/agent 78
  error/invalid-data 79
  error/assuan-server-fault 80
  error/assuan 81
  error/invalid-session-key 82
  error/invalid-sexp 83
  error/unsupported-algorithm 84
  error/no-pin-entry 85
  error/pin-entry 86
  error/bad-pin 87
  error/invalid-name 88
  error/bad-data 89
  error/invalid-parameter 90
  error/wrong-card 91
  error/no-dirmngr 92
  error/dirmngr 93
  error/cert-revoked 94
  error/no-crl-known 95
  error/crl-too-old 96
  error/line-too-long 97
  error/not-trusted 98
  error/canceled 99
  error/bad-ca-cert 100
  error/cert-expired 101
  error/cert-too-young 102
  error/unsupported-cert 103
  error/unknown-sexp 104
  error/unsupported-protection 105
  error/corrupted-protection 106
  error/ambiguous-name 107
  error/card 108
  error/card-reset 109
  error/card-removed 110
  error/invalid-card 111
  error/card-not-present 112
  error/no-pkcs15-app 113
  error/not-confirmed 114
  error/configuration 115
  error/no-policy-match 116
  error/invalid-index 117
  error/invalid-id 118
  error/no-scdaemon 119
  error/scdaemon 120
  error/unsupported-protocol 121
  error/bad-pin-method 122
  error/card-not-initialized 123
  error/unsupported-operation 124
  error/wrong-key-usage 125
  error/nothing-found 126
  error/wrong-blob-type 127
  error/missing-value 128
  error/hardware 129
  error/pin-blocked 130
  error/use-conditions 131
  error/pin-not-synced 132
  error/invalid-crl 133
  error/bad-ber 134
  error/invalid-ber 135
  error/element-not-found 136
  error/identifier-not-found 137
  error/invalid-tag 138
  error/invalid-length 139
  error/invalid-keyinfo 140
  error/unexpected-tag 141
  error/not-der-encoded 142
  error/no-cms-obj 143
  error/invalid-cms-obj 144
  error/unknown-cms-obj 145
  error/unsupported-cms-obj 146
  error/unsupported-encoding 147
  error/unsupported-cms-version 148
  error/unknown-algorithm 149
  error/invalid-engine 150
  error/public-key-not-trusted 151
  error/decrypt-failed 152
  error/key-expired 153
  error/sig-expired 154
  error/encoding-problem 155
  error/invalid-state 156
  error/dup-value 157
  error/missing-action 158
  error/module-not-found 159
  error/invalid-oid-string 160
  error/invalid-time 161
  error/invalid-crl-obj 162
  error/unsupported-crl-version 163
  error/invalid-cert-obj 164
  error/unknown-name 165
  error/locale-problem 166
  error/not-locked 167
  error/protocol-violation 168
  error/invalid-mac 169
  error/invalid-request 170
  error/unknown-extn 171
  error/unknown-crit-extn 172
  error/locked 173
  error/unknown-option 174
  error/unknown-command 175
  error/not-operational 176
  error/no-passphrase 177
  error/no-pin 178
  error/not-enabled 179
  error/no-engine 180
  error/missing-key 181
  error/too-many 182
  error/limit-reached 183
  error/not-initialized 184
  error/missing-issuer-cert 185
  error/no-keyserver 186
  error/invalid-curve 187
  error/unknown-curve 188
  error/dup-key 189
  error/ambiguous 190
  error/no-crypt-ctx 191
  error/wrong-crypt-ctx 192
  error/bad-crypt-ctx 193
  error/crypt-ctx-conflict 194
  error/broken-public-key 195
  error/broken-secret-key 196
  error/mac-algo 197
  error/fully-canceled 198
  error/unfinished 199
  error/buffer-too-short 200
  error/sexp-invalid-len-spec 201
  error/sexp-string-too-long 202
  error/sexp-unmatched-paren 203
  error/sexp-not-canonical 204
  error/sexp-bad-character 205
  error/sexp-bad-quotation 206
  error/sexp-zero-prefix 207
  error/sexp-nested-dh 208
  error/sexp-unmatched-dh 209
  error/sexp-unexpected-punc 210
  error/sexp-bad-hex-char 211
  error/sexp-odd-hex-numbers 212
  error/sexp-bad-oct-char 213
  error/subkeys-exp-or-rev 217
  error/db-corrupted 218
  error/server-failed 219
  error/no-name 220
  error/no-key 221
  error/legacy-key 222
  error/request-too-short 223
  error/request-too-long 224
  error/obj-term-state 225
  error/no-cert-chain 226
  error/cert-too-large 227
  error/invalid-record 228
  error/bad-mac 229
  error/unexpected-msg 230
  error/compr-failed 231
  error/would-wrap 232
  error/fatal-alert 233
  error/no-cipher 234
  error/missing-client-cert 235
  error/close-notify 236
  error/ticket-expired 237
  error/bad-ticket 238
  error/unknown-identity 239
  error/bad-hs-cert 240
  error/bad-hs-cert-req 241
  error/bad-hs-cert-ver 242
  error/bad-hs-change-cipher 243
  error/bad-hs-client-hello 244
  error/bad-hs-server-hello 245
  error/bad-hs-server-hello-done 246
  error/bad-hs-finished 247
  error/bad-hs-server-kex 248
  error/bad-hs-client-kex 249
  error/bogus-string 250
  error/forbidden 251
  error/key-disabled 252
  error/key-on-card 253
  error/invalid-lock-obj 254
  error/true 255
  error/false 256
  error/ass-general 257
  error/ass-accept-failed 258
  error/ass-connect-failed 259
  error/ass-invalid-response 260
  error/ass-invalid-value 261
  error/ass-incomplete-line 262
  error/ass-line-too-long 263
  error/ass-nested-commands 264
  error/ass-no-data-cb 265
  error/ass-no-inquire-cb 266
  error/ass-not-a-server 267
  error/ass-not-a-client 268
  error/ass-server-start 269
  error/ass-read-error 270
  error/ass-write-error 271
  error/ass-too-much-data 273
  error/ass-unexpected-cmd 274
  error/ass-unknown-cmd 275
  error/ass-syntax 276
  error/ass-canceled 277
  error/ass-no-input 278
  error/ass-no-output 279
  error/ass-parameter 280
  error/ass-unknown-inquire 281
  error/engine-too-old 300
  error/window-too-small 301
  error/window-too-large 302
  error/missing-envvar 303
  error/user-id-exists 304
  error/name-exists 305
  error/dup-name 306
  error/too-young 307
  error/too-old 308
  error/unknown-flag 309
  error/invalid-order 310
  error/already-fetched 311
  error/try-later 312
  error/wrong-name 313
  error/no-auth 314
  error/bad-auth 315
  error/system-bug 666)

(define error-source
  (let ((proc (libgcrypt->procedure '* "gcry_strsource" (list int))))
    (lambda (err)
      "Return the error source (a string) for ERR, an error code as thrown
along with 'gcry-error'."
      (pointer->string (proc err)))))

(define error-string
  (let ((proc (libgcrypt->procedure '* "gcry_strerror" (list int))))
    (lambda (err)
      "Return the error description (a string) for ERR, an error code as
thrown along with 'gcry-error'."
      (pointer->string (proc err)))))

(define (gcrypt-error-printer port key args default-printer)
  "Print the gcrypt error specified by ARGS."
  (match args
    ((proc err)
     (format port "In procedure ~a: ~a: ~a"
             proc (error-source err) (error-string err)))))

(set-exception-printer! 'gcry-error gcrypt-error-printer)

;;; gcrypt.scm ends here
