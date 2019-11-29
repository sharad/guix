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
  #:use-module (gnu packages gnuzilla))


(define-public libtirpc-gh
  (package (inherit libtirpc)
    (name "libtirpc-gh")
    (version "1.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattbenjamin/libtirpc-lbx.git")
                    (commit "master")))
              (sha256
               (base32
                "1h0asz1fp6ldq3zll80b4h5pr0y228j778hz6yhkv8hlzf15ivla"))))
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
    (version "8db8a2290683acf94f02e855af668a864d6001c2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.code.sf.net/p/cdesktopenv/code")
                    (commit version)))
              (sha256
               (base32
                "0268v6alylkywqhhpy8nwz2xvmf1bb07c3bzzmgqamkw8p2kakcd"))))
    (build-system gnu-build-system)
    (inputs `(("motif" ,motif)))
    (native-inputs
     `(("autoconf"   ,autoconf)
       ("automake"   ,automake)
       ("libtool"    ,libtool)
       ("flex"       ,flex)
       ("bison"      ,bison)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin #t)))
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
;; motif


libtirpc-gh
