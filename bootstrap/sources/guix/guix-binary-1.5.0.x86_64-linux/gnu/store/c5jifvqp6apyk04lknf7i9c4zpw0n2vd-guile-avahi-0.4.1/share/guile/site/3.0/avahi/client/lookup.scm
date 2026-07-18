;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007, 2023 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guile-Avahi.
;;;
;;; Guile-Avahi is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guile-Avahi is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (avahi client lookup)
  :use-module (avahi client)
  :use-module ((avahi) #:select (%avahi-extension))
  :export (;; browsers
           domain-browser? make-domain-browser domain-browser-client
           free-domain-browser! freed-domain-browser?

           service-browser? make-service-browser service-browser-client
           free-service-browser! freed-service-browser?

           service-type-browser? make-service-type-browser
           service-type-browser-client
           free-service-type-browser! freed-service-type-browser?

           ;; resolvers
           service-resolver? make-service-resolver service-resolver-client
           free-service-resolver! freed-service-resolver?
           address-resolver? make-address-resolver address-resolver-client
           free-address-resolver! freed-address-resolver?
           host-name-resolver? make-host-name-resolver
           host-name-resolver-client
           free-host-name-resolver! freed-host-name-resolver?

           ;; flags
           lookup-flag->string
           lookup-flag/use-wide-area lookup-flag/use-multicast
           lookup-flag/no-txt lookup-flag/no-address

           lookup-result-flag->string
           lookup-result-flag/cached
           lookup-result-flag/wide-area lookup-result-flag/multicast
           lookup-result-flag/local lookup-result-flag/our-own
           lookup-result-flag/static

           domain-browser-type->string
           domain-browser-type/browse domain-browser-type/browse-default
           domain-browser-type/register domain-browser-type/register-default
           domain-browser-type/browse-legacy

           browser-event->string
           browser-event/new browser-event/remove
           browser-event/cache-exhausted browser-event/all-for-now
           browser-event/failure

           resolver-event->string
           resolver-event/found resolver-event/failure))

(unless (getenv "AVAHI_GUILE_CROSS_COMPILING")
  (load-extension %avahi-extension "scm_avahi_lookup_init"))

;;; arch-tag: 9ab68bd4-4705-42e5-89c3-e02551be4d09
