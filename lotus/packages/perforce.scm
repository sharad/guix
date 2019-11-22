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

(define-module (lotus packages perforce)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gnuzilla))

(define-public p4
  (package
   (name "p4")
   (version "19.2")
   (source (origin
             (method url-fetch)
             (uri
              (string-append "https://cdist2.perforce.com/perforce/r" version "/bin.linux26x86_64/helix-core-server.tgz"))
             (file-name "p4")
             (sha256
              (base32
               "0pi3dxqy52qgpxj1z6lsc120p26hlhny9nlkqj04m4hkvx980g9d"))))
   (build-system trivial-build-system)
   ;; (inputs `(("glibc" ,myicecat)))
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
                        (bin-dir     (string-append %output "/bin/"))
                        (p4-file     "p4"))
                    (mkdir-p bin-dir)
                    (system (string-append gzipbin " -cd " tarball " | " tarbin " xf -"))
                    (for-each (lambda (file)
                                (let ((target-file (string-append bin-dir "/" (basename file))))
                                  (chmod file #o777)
                                  (system (string-append patchelfbin " --set-interpreter /run/current-system/profile/lib/ld-linux-x86-64.so.2 " file))
                                  (copy-file file target-file)
                                  (chmod target-file #o777)
                                  (system (string-append patchelfbin " --set-interpreter /run/current-system/profile/lib/ld-linux-x86-64.so.2 " target-file))
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
