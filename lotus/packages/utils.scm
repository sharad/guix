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

(define-module (lotus packages utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl))

;; https://issues.guix.gnu.org/issue/35619

(define-public lesspipe
  (package
    (name "lesspipe")
    (version "1.84")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wofr06/lesspipe.git")
                    (commit version)))
              (sha256
               (base32
                "124ffhzrikr88ab14rk6753n8adxijpmg7q3zx7nmqc52wpkfd8q"))))
    (build-system gnu-build-system)
    ;; (inputs `(("bdb" ,bdb)
    ;;           ("libnsl" ,libnsl)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (synopsis "lesspipe.sh, a preprocessor for less")
    (description "To browse files under UNIX the excellent viewer less [1] can be used. By
 setting the environment variable LESSOPEN, less can be enhanced by external
 filters to become even more powerful. Most Linux distributions come already
 with a \"lesspipe.sh\" that covers the most common situations.

 The input filter for less described here is called \"lesspipe.sh\". It is able
 to process a wide variety of file formats. It enables users to deeply inspect
 archives and to display the contents of files in archives without having to
 unpack them before. That means file contents can be properly interpreted even
 if the files are compressed and contained in a hierarchy of archives (often
 found in RPM or DEB archives containing source tarballs). The filter is easily
 extensible for new formats.

 The input filter which is also called \"lesspipe.sh\" is written in a ksh
 compatible language (ksh, bash, zsh) as one of these is nearly always installed
 on UNIX systems and uses comparably few resources. Otherwise an implementation
 in perl for example would have been somewhat simpler to code. The code looks
 less clean than it could as it was tried to make the script compatible with
 a number of old shells and applications especially found on non Linux systems.

 The filter does different things depending on the file format. In most cases
 it is determined on the output of the \"file\" command [2], [6], that recognizes
 lots of formats. Only in a few cases the file suffix is used to determine what
 to display. Up to date file descriptions are included in the \"file\" package.
 Maintaining a list of file formats is therefore only a matter of keeping that
 package up to date.")
    (home-page "https://www-zeuthen.desy.de/~friebel/unix/lesspipe.html")
    (license license:ibmpl1.0)))

;; lesspipe


