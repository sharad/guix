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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gnome)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public skype4pidgin

  ;; https://github.com/EionRobb/skype4pidgin/tree/master/skypeweb#windows
  ;; http://www.webupd8.org/2016/07/chat-with-your-skype-friends-from.html

  ;; Requires devel headers/libs for libpurple and libjson-glib [libglib2.0-dev, libjson-glib-dev and libpurple-dev]

  ;; https://github.com/EionRobb/skype4pidgin/archive/1.6.tar.gz
  ;; git clone git://github.com/EionRobb/skype4pidgin.git
  ;; cd skype4pidgin/skypeweb
  ;; make
  ;; sudo make install

  (package
    (name "skype4pidgin")
    (version "1.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/EionRobb/skype4pidgin/archive/" version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0knq59dqhankix10219gpwmrb5qsyg10j9f6n5malibsh32d8175"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which"      ,which)))
    (inputs
     `(("pidgin"     ,pidgin)
       ("glib"       ,glib)
       ("json-glib"  ,json-glib)))
    (arguments
     `(#:tests? #f                            ; Run the test suite (this is the default)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-dir
           (lambda _
             (chdir "skypeweb")
             #t))
         (add-before 'configure 'replace-purple-dir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{PKG_CONFIG_EXECUTABLE\\} --variable=plugindir purple 2>/dev/null")
                (string-append "${PKG_CONFIG_EXECUTABLE} --variable=plugindir purple 2>/dev/null | sed -e "
                               "s@^"(assoc-ref inputs "pidgin")"@"(assoc-ref outputs "out")"@"))
               (("\\$\\{PKG_CONFIG_EXECUTABLE\\} --variable=datadir purple 2>/dev/null")
                (string-append "${PKG_CONFIG_EXECUTABLE} --variable=datadir purple 2>/dev/null | sed -e "
                               "s@^"(assoc-ref inputs "pidgin")"@"(assoc-ref outputs "out")"@")))
             #t)))))
    (synopsis "SkypeWeb Plugin for Pidgin")
    (description "Adds a \"Skype (HTTP)\" protocol to the accounts list. Requires libjson-glib. GPLv3 Licenced.")
    (home-page "https://github.com/EionRobb/skype4pidgin/tree/master/skypeweb#skypeweb-plugin-for-pidgin")
    ;; Conkeror is triple licensed.
    (license (list license:gpl2
                   license:lgpl2.1))))


(define-public bitlbee-purple-plus
  ;; This variant uses libpurple, which provides support for more protocols at
  ;; the expense of a much bigger closure.
  (package/inherit bitlbee-purple
    (name "bitlbee-purple-plus")
    (synopsis "IRC to instant messaging gateway (using Pidgin's libpurple)")
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("which"      ,which)))
    (inputs `(("purple"       ,pidgin)
              ("skype4pidgin" ,skype4pidgin)
              ,@(package-inputs bitlbee)))))
