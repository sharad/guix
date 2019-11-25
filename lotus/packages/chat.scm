;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 John J. Foerch <jjfoerch@earthlink.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (lotus packages chat)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gnuzilla))

(define-public skype4pidgin

  ;; https://github.com/EionRobb/skype4pidgin/tree/master/skypeweb#windows
  ;; http://www.webupd8.org/2016/07/chat-with-your-skype-friends-from.html

  ;; Requires devel headers/libs for libpurple and libjson-glib [libglib2.0-dev, libjson-glib-dev and libpurple-dev]

  ;; git clone git://github.com/EionRobb/skype4pidgin.git
  ;; cd skype4pidgin/skypeweb
  ;; make
  ;; sudo make install

  (package
    (name "skype4pidgin")
    (version "v1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://github.com/EionRobb/skype4pidgin.git")
                    (commit version)))
              (sha256
               (base32
                "1mqvqn4jdaj4d2iypfk59rd5629llcpzkfnb4grh4zqb00v8zrp3"))))
    (build-system cmake-build-system)
    (inputs
     `(("pidgin" ,pidgin)
       ;; ("libgcrypt" ,libgcrypt)
       ;; ("libwebp" ,libwebp)
       ;; ("glib" ,glib)
       ;; ("gettext" ,gnu-gettext)
       ;; ("gtk+" ,gtk+-2)
       ;; ("zlib" ,zlib)
       ))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)
       ("patchelf" ,patchelf)))
    (arguments
     `(#:modules ((guix build utils))
                 #:builder (begin
                             )))
    (synopsis "SkypeWeb Plugin for Pidgin")
    (description "Adds a "Skype (HTTP)" protocol to the accounts list. Requires libjson-glib. GPLv3 Licenced.")
    (home-page "https://www.perforce.com/downloads/helix-command-line-client-p4")
    ;; Conkeror is triple licensed.
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))


skype4pidgin

