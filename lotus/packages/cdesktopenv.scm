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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gnuzilla))

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
                "07znv5ywi1jwp994bdk5sjxshrc2557f6sfyh7vvqxmvmzixaa5a"))))
    (build-system gnu-build-system)
    (inputs `(("libc" ,glibc)
              ("gcc" ,gcc)))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)
       ("patchelf" ,patchelf)))
    (arguments)
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
    (build-system trivial-build-system)
    (inputs `(("libc" ,glibc)
              ("gcc" ,gcc)
              ("motif" ,motif)))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)
       ("patchelf" ,patchelf)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((tarbin      (string-append (assoc-ref %build-inputs "tar")  "/bin/tar"))
                         (gzipbin     (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip"))
                         (patchelfbin (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf"))
                         (tarball     (assoc-ref %build-inputs "source"))
                         (ld-so (string-append (assoc-ref inputs "libc")
                                               ,(glibc-dynamic-linker)))
                         (bin-dir     (string-append %output "/bin/"))
                         (p4-file     "p4"))
                     (mkdir-p bin-dir)
                     (system (string-append gzipbin " -cd " tarball " | " tarbin " xf -"))
                     (for-each (lambda (file)
                                 (let ((target-file (string-append bin-dir "/" (basename file))))
                                   (chmod file #o777)
                                   (system (string-append patchelfbin " --set-interpreter " ld-so " " file))
                                   (copy-file file target-file)
                                   (chmod target-file #o777)
                                   (system (string-append patchelfbin " --set-interpreter " ld-so " " target-file))
                                   (chmod target-file #o555)))
                               (list p4-file))
                     #t))))
    (synopsis "Perforce p4 cli client")
    (description "Perforce p4 cli client.")
    (home-page "https://www.perforce.com/downloads/helix-command-line-client-p4")
    ;; Conkeror is triple licensed.
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))

cdesktopenv

