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
  #:use-module (ice-9 ftw)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build rpath)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (lotus utils))

;; library-file?

(define-public firefox
  (package
   (name "firefox")
   (version "56.0")
   (source (origin
             (method url-fetch)
             (uri
              (string-append "https://ftp.mozilla.org/pub/firefox/releases/" version "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
             (file-name (string-append "firefox-" version ".tar.bz2"))
             (sha256
              (base32
               "06w2pkfxf9yj68h9i7h4765md0pmgn8bdh5qxg7jrf3n22ikhngb"))))
   (build-system trivial-build-system)
   (inputs `(("libc"          ,glibc)
             ("gcc"           ,gcc "lib")
             ("libxcomposite" ,libxcomposite)
             ;; ("libxt"         ,libxt)
             ("gtk+"          ,gtk+)))
   (native-inputs
    `(("tar" ,tar)
      ("gzip" ,gzip)
      ("bzip2" ,bzip2)
      ("patchelf" ,patchelf)))
   (arguments
    `(#:modules ((guix build utils)
                 (guix build rpath)
                 (lotus utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (use-modules (lotus utils))
                  (let* ((tarbin      (string-append (assoc-ref %build-inputs "tar")  "/bin/tar"))
                         (gzipbin     (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip"))
                         (bzip2bin    (string-append (assoc-ref %build-inputs "bzip2") "/bin/bzip2"))
                         (uncompress  bzip2bin)
                         (patchelfbin (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf"))
                         (tarball     (assoc-ref %build-inputs "source"))
                         (ld-so       (string-append (assoc-ref %build-inputs "libc") ,(glibc-dynamic-linker)))
                         (firefox-dir (string-append %output     "/share/firefox"))
                         (firefox-lib (string-append firefox-dir "/lib"))
                         (firefox-bin (string-append firefox-dir "/bin"))
                         (bin-dir     (string-append %output     "/bin")))
                    ;; (unpack tarball)
                    (mkdir-p bin-dir)
                    (mkdir-p bin-dir)
                    (mkdir-p firefox-bin)
                    (mkdir-p firefox-lib)
                    ;; see if can be replaced with unpack
                    (system (string-append uncompress " -cd " tarball " | " tarbin " xf -"))
                    ;; (write (directory-list-files "."))
                    (display (directory-list-files "firefox"))
                    ;; (copy-recursively "." %output)
                    (for-each (lambda (file)
                                (let* ((src-file (string-append "firefox/" file))
                                       (target-file (string-append (if (library-file? src-file)
                                                                       firefox-lib
                                                                       firefox-bin)
                                                                   "/" (basename file))))
                                  (if (directory? src-file)
                                      (copy-recursively src-file target-file)
                                   (begin
                                     (copy-file src-file target-file)
                                     (when (or (elf-binary-file? src-file)
                                               (library-file? src-file))
                                       (chmod target-file #o777)
                                       (when (elf-binary-file? src-file)
                                         (system (string-append patchelfbin " --set-interpreter " ld-so " " target-file)))
                                       (augment-rpath target-file (string-append %output "/lib"))
                                       (augment-rpath target-file (string-append %output "/share/firefox/lib"))
                                       ;; (system (string-append patchelfbin " --set-rpath "
                                       ;;                        (string-append %output "/lib" ":" %output "/share/firefox/lib")
                                       ;;                        " " target-file))
                                      (chmod target-file #o555))))))
                              (directory-list-files "firefox"))
                              ;; (scandir directory regular?)
                              ;; (find-files ".")
                    #t))))
   (synopsis "Firefox")
   (description "Firefox.")
   (home-page "https://www.mozilla.org")
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
    (inputs `(("firefox" ,firefox)))
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
exec ~a/bin/firefox --app ~a \"$@\"~%"
                          (assoc-ref inputs "bash") ;implicit input
                          (assoc-ref inputs "firefox")
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


firefox

