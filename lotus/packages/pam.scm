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

(define-module (lotus packages pam)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages tls))


;; from https://cdn-aws.deb.debian.org/debian/pool/main/p/pam-tmpdir/pam-tmpdir_0.09.tar.gz
;; 'https://cdn-aws.deb.debian.org/debian/pool/main/p/pam-tmpdir/pam-tmpdir_0.09.tar.gz'

(define-public pam-tmpdir
  "https://launchpad.net/ubuntu/+archive/primary/+sourcefiles/pam-tmpdir/0.09build1/pam-tmpdir_0.09build1.tar.gz")



(define-public freeradius
  (package
   (name "freeradius")
   (version "3.0.25")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append "ftp://ftp.freeradius.org/pub/freeradius/freeradius-server-" version ".tar.gz"))
     (sha256
      (base32 "042czyi0kq803l13rp6npyxfhmg63ilw35ws6ljn1r6fnf5sd0s8"))))
   ;; (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs        `(("talloc"    ,talloc)
                    ("openssl"   ,openssl)
                    ("perl"      ,perl)
                    ("cppcheck"  ,cppcheck)
                    ("python-2.7" ,python-2.7)))
   (build-system gnu-build-system)
   (arguments
    `(#:parallel-build? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'configure 'patch-absolute-paths
                                (lambda _
                                  (setenv "VERBOSE" "1")
                                  (substitute* "src/include/all.mk"
                                               (("/bin/sh") (which "sh")))
                                  (substitute* "scripts/libtool.mk"
                                               (("\\$\\{LIBTOOL\\} --silent") "${LIBTOOL}"))
                                  #t)))))
   (synopsis "The FreeRADIUS Server Project is a high performance and highly configurable
GPL'd free RADIUS server")
   (description
    "The FreeRADIUS Server Project is a high performance and highly configurable
GPL'd free RADIUS server. The server is similar in some respects to
Livingston's 2.0 server.  While FreeRADIUS started as a variant of the
Cistron RADIUS server, they don't share a lot in common any more. It now has
many more features than Cistron or Livingston, and is much more configurable.
FreeRADIUS is an Internet authentication daemon, which implements the RADIUS
protocol, as defined in RFC 2865 (and others). It allows Network Access
Servers (NAS boxes) to perform authentication for dial-up users. There are
also RADIUS clients available for Web servers, firewalls, Unix logins, and
more.  Using RADIUS allows authentication and authorization for a network to
be centralized, and minimizes the amount of re-configuration which has to be
done when adding or deleting new users.")
   (home-page "https://freeradius.org")
   (license license:gpl2+)))

freeradius
