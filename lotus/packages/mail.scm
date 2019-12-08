;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Brant Gardner <bcg@member.fsf.org>
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

(define-module (lotus packages mail)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl))

;; https://issues.guix.gnu.org/issue/35619

(define-public postfix
  (package
    (name "postfix")
    (version "3.4.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "ftp://ftp.porcupine.org/mirrors/project-history/postfix/official/postfix-"
                version ".tar.gz"))
              (sha256
               (base32
                "17riwr21i9p1h17wpagfiwkpx9bbx7dy4gpdl219a11akm7saawb"))))
    (build-system gnu-build-system)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (add-before 'build 'patch-/bin/sh
                     (lambda _
                       (substitute* (find-files "." "^Makefile.in")
                         (("/bin/sh") (which "sh")))
                       #t))
                   (add-before 'build 'auxlibs
                     (lambda _
                       (setenv "AUXLIBS"
                               "-lnsl -lresolv") ; Required, but postfix OS
                                                 ; detection in leaves these
                                                 ; unset for Guix
                       #t))
                   (add-before 'build 'patch-/usr/include
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* '("makedefs")
                         (("/usr/include") (string-append (assoc-ref
                                                           inputs "bdb")
                                                          "/include" #t)))))
                   (add-before 'build 'configure-postfix ; Move configuration folder
                     (lambda* (#:key outputs #:allow-other-keys)
                       (invoke "make" "makefiles"
                               (string-append "CCARGS=-DDEF_CONFIG_DIR=\\\""
                                (assoc-ref outputs "out") "/etc\\\"" #t))))
                   (delete 'configure) ; no configure script
                   (delete 'check)))) ; no check
    (inputs `(("bdb" ,bdb)
              ("libnsl" ,libnsl)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (synopsis "Full-featured & secure sendmail replacement")
    (description "Postfix attempts to be fast, easy to administer, and
secure.  The outside has a definite Sendmail-ish flavor, but the inside is
completely different.")
    (home-page "https://www.postfix.org/")
    (license license:ibmpl1.0)))




postfix

