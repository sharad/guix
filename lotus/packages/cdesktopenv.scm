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

(define-module (lotus packages cdesktopenv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages gnuzilla))


(define-public libtirpc-gh
  (package (inherit libtirpc)
    (name "libtirpc-gh")
    (version "1.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sharad/libtirpc.git")
                    (commit "master")))
              (sha256
               (base32
                "00msa1a1s1xa1p4ijn4d27f9aj0a1p16bxq4kx6m7d49ljsfn464"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool"  ,libtool)
       ;; ("flex"     ,flex)
       ;; ("bison"    ,bison)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gss"      ,gss)
       ("mit-krb5" ,mit-krb5)))
    (build-system gnu-build-system)))

(define-public motif
  (package
    (name "motif")
    (version "2dc3d5ab4d3f24aa8fff5b7e6ba17a508376148b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.code.sf.net/p/motif/code")
                    (commit version)))
              (sha256
               (base32
                "1mqvqn4jdaj4d2iypfk59rd5629llcpzkfnb4grh4zqb00v8zrp3"))))
    (build-system gnu-build-system)
    (inputs `(;; ("libc" ,glibc)
              ;; ("gcc" ,gcc)
              ("libxft"      ,libxft)
              ("libxt"       ,libxt)
              ("zlib"        ,zlib)
              ("libxext"     ,libxext)
              ("fontconfig"  ,fontconfig)
              ("xbitmaps"    ,xbitmaps)
              ("freetype"    ,freetype)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool"  ,libtool)
       ("flex"     ,flex)
       ("bison"    ,bison)
       ("pkg-config" ,pkg-config)))
       ;; ("patchelf" ,patchelf)
    ;; (arguments)
    (synopsis "Motif user interface component toolkit")
    (description "Motif user interface component toolkit")
    (home-page "https://sourceforge.net/p/motif")
    ;; Conkeror is triple licensed.
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:lgpl2.1))))

(define-public cdesktopenv
  (package
    (name "cdesktopenv")
    (version "build-correction-autotools-conversion")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sharad/cdesktopenv.git")
                    (commit version)))
              (sha256
               (base32
                "0k089q4z02k2f0iz699s41b9lbrbw2gb0dgm41vr6yhh2plxgrzw"))))
    (build-system gnu-build-system)
    (inputs
     `(("libtirpc-gh"   ,libtirpc-gh)
       ("freetype"      ,freetype)
       ("bzip2"         ,bzip2)
       ("libjpeg"       ,libjpeg)
       ("motif"         ,motif)                            ;pkg-config missing
       ("libx11"        ,libx11)
       ("libxmu"        ,libxmu)
       ("libxext"       ,libxext)
       ("libxft"        ,libxft)
       ("libxinerama"   ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxt"         ,libxt)
       ("xbitmaps"      ,xbitmaps)))
    (native-inputs
     `(("coreutils"    ,coreutils)
       ("autoconf"     ,autoconf)
       ("automake"     ,automake)
       ("libtool"      ,libtool)
       ("flex"         ,flex)
       ("bison"        ,bison)
       ("pkg-config"   ,pkg-config)
       ("loksh"        ,loksh)
       ("gawk"         ,gawk)
       ("perl"         ,perl)
       ("tcsh"         ,tcsh)
       ("tcl"          ,tcl)
       ("mkfontdir"    ,mkfontdir)
       ("bdftopcf"     ,bdftopcf)
       ("rpcsvc-proto" ,rpcsvc-proto)))
    (arguments
     `(#:tests? #f                            ; Run the test suite (this is the default)
       #:configure-flags '("--disable-dependency-tracking") ; SHA-1 collision detection
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-entries
           (lambda _
             (substitute* "cde/contrib/desktopentry/cde.desktop"
               (("Exec=/usr/dt/bin/Xsession")
                (string-append "Exec=" (string-append %output "/usr/dt/bin/Xsession"))))))
         (add-before 'configure 'change-dir
           (lambda _
             (begin (chdir "cde")
                    (system "./autogen.sh")
                    #t)))
         (add-after 'install 'dektop-entry
           (lambda _ (let ((src-file    "contrib/desktopentry/cde.desktop")
                           (desktop-dir (string-append %output "/share/xsessions")))
                       (mkdir-p desktop-dir)
                       (copy-file src-file (string-append desktop-dir "/cde.desktop"))))))))
    (synopsis "CDE - Common Desktop Environment")
    (description " The Common Desktop Environment, the classic UNIX desktop
Brought to you by: flibble, jon13



The Common Desktop Environment was created by a collaboration of Sun, HP, IBM, DEC, SCO, Fujitsu and Hitachi. Used on a selection of commercial UNIXs, it is now available as open-source software for the first time.

For support, see: https://sourceforge.net/p/cdesktopenv/wiki/Home/
Features

    X11 Desktop Environment

")
    (home-page "https://sourceforge.net/projects/cdesktopenv/")
    ;; Conkeror is triple licensed.
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))

;; cdesktopenv

