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

(define-module (lotus packages conkeror)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnuzilla))

(define-public firefox
  (package
   (name "firefox")
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
                  (let ((tarbin  (string-append (assoc-ref %build-inputs "tar")  "/bin/tar"))
                        (gzipbin (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip"))
                        (patchelfbin (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf"))
                        (tarball (assoc-ref %build-inputs "source"))
                        (bin-dir (string-append %output "/bin/"))
                        (p4-file "p4"))
                    (mkdir-p bin-dir)
                    (system (string-append gzipbin " -cd " tarball " | " tarbin " xf -"))
                    (for-each (lambda (file)
                                (let ((target-file (string-append bin-dir "/" (basename file))))
                                  (chmod file #o555)
                                  (system (string-append patchelfbin " --set-interpreter /run/current-system/profile/lib/ld-linux-x86-64.so.2 " file))
                                  (copy-file file target-file)
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

(define-public myconkeror
  (package
    (name "myconkeror")
    (version "0864afd1b830d5485d6b9d99eec6f185bc6bd144")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://repo.or.cz/conkeror.git/snapshot/"
                              version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12m6iyfnii3hmkcnhsclli9mj9nblv0xn2p5dl6mv1mp0p9ck9pv"))))
    (build-system gnu-build-system)
    ;; (inputs `(("icecat" ,icecat)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags `("CC=gcc"
                      ,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after
          'install 'install-app-launcher
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; This overwrites the installed launcher, which execs xulrunner,
            ;; with one that execs 'icecat --app'
            (let* ((out      (assoc-ref outputs "out"))
                   (datadir  (string-append out "/share/conkeror"))
                   (launcher (string-append out "/bin/conkeror")))
              (call-with-output-file launcher
                (lambda (p)
                  (format p "#!~a/bin/bash
exec ~a/bin/icecat --app ~a \"$@\"~%"
                          (assoc-ref inputs "bash") ;implicit input
                          (assoc-ref inputs "icecat")
                          (string-append datadir
                                         "/application.ini"))))
              (chmod launcher #o555)))))))
    (synopsis "Keyboard focused web browser with Emacs look and feel")
    (description "Conkeror is a highly-programmable web browser based on
Mozilla XULRunner which is the base of all Mozilla products including Firefox.
Conkeror has a sophisticated keyboard system for running commands and
interacting with web page content, modelled after Emacs and Lynx.  It is
self-documenting and extensible with JavaScript.

It comes with builtin support for several Web 2.0 sites like several Google
services (Search, Gmail, Maps, Reader, etc.), Del.icio.us, Reddit, Last.fm and
YouTube.  For easier editing of form fields, it can spawn external editors.")
    (home-page "http://conkeror.org")
    ;; Conkeror is triple licensed.
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))
