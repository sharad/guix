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
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build-system trivial)
  #:use-module (lotus build-system patchelf)
  #:use-module (lotus build patchelf-build-system)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages video)
  #:use-module (lotus build patchelf-utils))

(define-public firefox-56.0-old
  ;; (hidden-package)
  (package
     (name "firefox-56.0-old")
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
               ("gcc:lib"       ,gcc "lib")
               ("dbus"          ,dbus)
               ("libxcomposite" ,libxcomposite)
               ("libxt"         ,libxt)
               ("gtk+"          ,gtk+)
               ("atk"           ,atk)
               ("cairo"         ,cairo)
               ("dbus-glib"     ,dbus-glib)
               ("fontconfig"    ,fontconfig)
               ("freetype"      ,freetype)
               ("gdk-pixbuf"    ,gdk-pixbuf)
               ("glib"          ,glib)
               ("glibc"         ,glibc)
               ("libx11"        ,libx11)
               ("libxcb"        ,libxcb)
               ("libxdamage"    ,libxdamage)
               ("libxext"       ,libxext)
               ("libxfixes"     ,libxfixes)
               ("libxrender"    ,libxrender)
               ("pango"         ,pango)
               ("pulseaudio"    ,pulseaudio)
               ("libogg"        ,libogg)
               ("libvorbis"     ,libvorbis)
               ("libevent"      ,libevent)
               ("libxinerama"   ,libxinerama)
               ("libxscrnsaver" ,libxscrnsaver)
               ("libffi"        ,libffi)
               ("ffmpeg"        ,ffmpeg)
               ("libvpx"        ,libvpx-1.7)))
     (native-inputs `(("tar"      ,tar)
                      ("gzip"     ,gzip)
                      ("bzip2"    ,bzip2)
                      ("sed"      ,sed)
                      ("patchelf" ,patchelf)))
     (arguments
      `(#:modules ((guix build utils)
                   (guix build rpath)
                   (lotus build patchelf-utils))
        #:builder (begin
                    (use-modules (guix build utils))
                    (use-modules (guix build rpath))
                    (use-modules (lotus build patchelf-utils))
                    (let* ((dep-inputs  '("libc" "gcc:lib" "dbus" "atk" "cairo" "dbus-glib" "fontconfig"
                                          "freetype" "gtk+" "gdk-pixbuf" "glib" "libx11" "libxcb" "libxdamage"
                                          "libxext" "libxfixes" "libxrender" "pango" "libxcomposite" "libxt" "pulseaudio"
                                          "libogg" "libvorbis" "libevent" "libxinerama" "libxscrnsaver" "libffi" "ffmpeg" "libvpx"))
                           (tarbin      (string-append (assoc-ref %build-inputs "tar")   "/bin/tar"))
                           (gzipbin     (string-append (assoc-ref %build-inputs "gzip")  "/bin/gzip"))
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
                      ;; (display (directory-list-files "firefox"))
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
                                         (let* ((lib-paths (append (list firefox-lib)
                                                                   (map (lambda (in)
                                                                          (string-append (assoc-ref %build-inputs in) "/lib"))
                                                                        dep-inputs)))
                                                (rpath     (string-join lib-paths ":")))
                                           ;; (format #t "target-file ~s rpath ~s~%" target-file rpath)
                                           ;; (augment-rpath target-file lib-paths)
                                           (system (string-append patchelfbin " --set-rpath " rpath " " target-file)))
                                         (when (and
                                                (not (library-file? src-file))
                                                (elf-binary-file? src-file))
                                           (system (string-append patchelfbin " --set-interpreter " ld-so " " target-file)))
                                        (chmod target-file #o555))))))
                                (directory-list-files "firefox")) ;; (directory-list-files "firefox/gtk2/libmozgtk.so") - left todo
                      (system (string-append (assoc-ref %build-inputs "sed") "/bin/sed" " -i 's@^lib@../lib/lib@g' " firefox-bin "/dependentlibs.list"))
                      (symlink "../share/firefox/bin/firefox" (string-append bin-dir "/firefox"))
                      #t))))
     (synopsis "Firefox")
     (description "Firefox.")
     (home-page "https://www.mozilla.org")
     ;; Conkeror is triple licensed.
     (license (list
               ;; MPL 1.1 -- this license is not GPL compatible
               license:gpl2
               license:lgpl2.1))))

(define (directory-list-files dir)
  (scandir dir (negate (cut member <> '("." "..")))))

(define-public firefox-56.0
  ;; (hidden-package)
  (package
    (name "firefox-56.0")
    (version "56.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://ftp.mozilla.org/pub/firefox/releases/" version "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
              (file-name (string-append "firefox-" version ".tar.bz2"))
              (sha256
               (base32
                "06w2pkfxf9yj68h9i7h4765md0pmgn8bdh5qxg7jrf3n22ikhngb"))))
    (build-system patchelf-build-system)
    (inputs  `(("libc"          ,glibc)
               ("gcc:lib"       ,gcc "lib")
               ("dbus"          ,dbus)
               ("libxcomposite" ,libxcomposite)
               ("libxt"         ,libxt)
               ("gtk+"          ,gtk+)
               ("atk"           ,atk)
               ("cairo"         ,cairo)
               ("dbus-glib"     ,dbus-glib)
               ("fontconfig"    ,fontconfig)
               ("freetype"      ,freetype)
               ("gdk-pixbuf"    ,gdk-pixbuf)
               ("glib"          ,glib)
               ("glibc"         ,glibc)
               ("libx11"        ,libx11)
               ("libxcb"        ,libxcb)
               ("libxdamage"    ,libxdamage)
               ("libxext"       ,libxext)
               ("libxfixes"     ,libxfixes)
               ("libxrender"    ,libxrender)
               ("pango"         ,pango)
               ("pulseaudio"    ,pulseaudio)
               ("libogg"        ,libogg)
               ("libvorbis"     ,libvorbis)
               ("libevent"      ,libevent)
               ("libxinerama"   ,libxinerama)
               ("libxscrnsaver" ,libxscrnsaver)
               ("libffi"        ,libffi)
               ("ffmpeg"        ,ffmpeg)
               ("libvpx"        ,libvpx-1.7)))
    (arguments `(;; #:modules ((lotus build patchelf-utils))
                 #:output-libs '("/share/firefox/lib")
                 #:phases      (modify-phases %standard-phases
                                 (add-after
                                     'build 'rearrange
                                   (lambda* (#:key inputs outputs #:allow-other-keys)
                                     ;; This overwrites the installed launcher, which execs xulrunner,
                                     ;; with one that execs 'icecat --app'
                                     ;; (use-modules (lotus build patchelf-utils))
                                     ;; (define source (getcwd))
                                     (let* ((source           (getcwd))
                                            (files-to-arrange (find-files source))
                                            (firefox-dir      (string-append source      "/share/firefox"))
                                            (firefox-lib      (string-append firefox-dir "/lib"))
                                            (firefox-bin      (string-append firefox-dir "/bin"))
                                            (firefox-misc     (string-append firefox-dir "/misc"))
                                            (bin-dir          (string-append source      "/bin")))
                                       (format #t "rearrange: outputs ~a~%" outputs)
                                       (for-each (lambda (file)
                                                   (let* ((stripped-file (string-drop file (string-length source)))
                                                          (location      (cond ((library-file? file)
                                                                                firefox-lib)
                                                                               ((and (not (library-file? file))
                                                                                     (elf-binary-file? file))
                                                                                firefox-bin)
                                                                               (#t (if (string=? (dirname stripped-file) "/")
                                                                                       firefox-misc
                                                                                       (string-append firefox-misc (dirname stripped-file))))))
                                                          (target-file   (string-append location "/" (basename file))))
                                                     (format #t "rearrange: src ~a -> target ~a~%" file target-file)
                                                     (mkdir-p (dirname target-file))
                                                     (rename-file file target-file)))
                                                 files-to-arrange)
                                       (copy-file (string-append firefox-misc "/dependentlibs.list")
                                                  (string-append firefox-bin "/dependentlibs.list"))
                                       (invoke "sed" "-i" "s@^lib@../lib/lib@g" (string-append firefox-bin "/dependentlibs.list"))
                                       (mkdir-p bin-dir)
                                       (symlink "../share/firefox/bin/firefox" (string-append bin-dir "/firefox"))
                                       (for-each (lambda (file)
                                                   (format #t "misc: ~a~%" file))
                                                 (directory-list-files firefox-misc))
                                       #t)))
                                 ;; (delete 'strip)
                                 ;; (replace 'strip
                                 ;;   (lambda (#:key target outputs (strip-binaries? #t)
                                 ;;            (strip-command (if target
                                 ;;                               (string-append target "-strip")
                                 ;;                               "strip"))
                                 ;;            (objcopy-command (if target
                                 ;;                                 (string-append target "-objcopy")
                                 ;;                                 "objcopy"))
                                 ;;            (strip-flags '("--strip-debug"
                                 ;;                           "--enable-deterministic-archives"))
                                 ;;            (strip-directories '("share/firefox/lib"
                                 ;;                                 "share/firefox/lib64"
                                 ;;                                 "share/firefox/libexec"
                                 ;;                                 "share/firefox/bin"
                                 ;;                                 "share/firefox/sbin"))
                                 ;;            #:allow-other-keys)
                                 ;;     (define gnu:strip (assoc-ref gnu:%standard-phases 'strip))
                                 ;;     (gnu:strip #:target            target
                                 ;;                #:outputs           outputs
                                 ;;                #:strip-binaries?   strip-binaries?
                                 ;;                #:strip-command     strip-command
                                 ;;                #:objcopy-command   objcopy-command
                                 ;;                #:strip-flags       strip-flags
                                 ;;                #:strip-directories strip-directories)))
                                 (replace 'validate-runpath
                                          (lambda* (#:key (validate-runpath? #t)
                                                          (elf-directories '("share/firefox/lib"
                                                                             "share/firefox/lib64"
                                                                             "share/firefox/libexec"
                                                                             "share/firefox/bin"
                                                                             "share/firefox/sbin"))
                                                          outputs
                                                          #:allow-other-keys)
                                            (define gnu:validate-runpath (assoc-ref %standard-phases 'validate-runpath))
                                            (gnu:validate-runpath #:validate-runpath? validate-runpath?
                                                                  #:elf-directories   elf-directories
                                                                  #:outputs           outputs))))))
                                 
    (synopsis "Firefox")
    (description "Firefox.")
    (home-page "https://www.mozilla.org")
    ;; Conkeror is triple licensed.
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))

(define-public firefox
  (package (inherit firefox-56.0-old)
    (name "firefox")))

(define-public conkeror-firefox
  (package
    (name "conkeror-firefox")
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
                          (string-append datadir "/application.ini"))))
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

