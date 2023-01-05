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

(define-module (lotus packages firefox)
  #:use-module (ice-9 ftw)
  #:use-module (lotus build patchelf-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  ;; #:use-module (guix build rpath)
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
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)


  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages hunspell)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  ;; #:use-module (gnu packages rust)
  ;; #:use-module (gnu packages rust-cbindgen)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)

  #:use-module (lotus packages utils))

;; https://packages.ubuntu.com/search?keywords=ubuntu-restricted-extras
;; https://packages.ubuntu.com/xenial/ubuntu-restricted-extras
;; https://packages.ubuntu.com/xenial/libavcodec-ffmpeg-extra56


;; https://linoxide.com/linux-how-to/install-adobe-flash-player-linux-terminal/
;; https://packages.ubuntu.com/xenial/web/browser-plugin-freshplayer-pepperflash
;; https://pkgs.org/download/adobe-flashplugin
;; https://ubuntu.pkgs.org/19.10/canonical-partner-amd64/adobe-flashplugin_20191210.1-0ubuntu0.19.10.2_amd64.deb.html
;; http://archive.canonical.com/ubuntu/pool/partner/a/adobe-flashplugin/adobe-flashplugin_20191210.1-0ubuntu0.19.10.2_amd64.deb
;; https://fpdownload.adobe.com/get/flashplayer/pdc/32.0.0.303/flash_player_npapi_linux.x86_64.tar.gz

(define patched-firefox-include-adobe-flash #f)

(define patched-firefox-inputs-problem (list glibc
                                             `(,gcc "lib")
                                             dbus
                                             libxcomposite
                                             libxt
                                             gtk+
                                             atk
                                             cairo
                                             dbus-glib
                                             fontconfig
                                             freetype
                                             gdk-pixbuf
                                             glib
                                             glibc
                                             libx11
                                             libxcb
                                             libxdamage
                                             libxext
                                             libxfixes
                                             libxrender
                                             pango
                                             pulseaudio
                                             libogg
                                             libvorbis
                                             libevent
                                             libxinerama
                                             libxscrnsaver
                                             libffi
                                             ffmpeg
                                             libvpx

                                             ;; machine hang
                                             libfdk
                                             libtheora
                                             wavpack
                                             libwebp
                                             speex
                                             opus
                                             x265
                                             xvid
                                             libxv



                                             gst-libav
                                             gst-plugins-base
                                             gst-plugins-good
                                             gst-plugins-bad
                                             gst-plugins-ugly
                                             gst123
                                             gstreamer
                                             openh264
                                             libsmpeg
                                             libmpeg2
                                             ;; "libmad"        ,libmad)

                                             vlc
                                             alsa-lib
                                             bzip2
                                             cups
                                             dbus-glib
                                             gdk-pixbuf
                                             glib
                                             gtk+
                                             gtk+-2
                                             graphite2
                                             pango
                                             freetype
                                             harfbuzz
                                             libcanberra
                                             libgnome
                                             libjpeg-turbo
                                             libogg
                                             ;; "libtheora" ,libtheora) ; wants theora-1.2, not yet released
                                             libvorbis
                                             libxft
                                             libevent
                                             libxinerama
                                             libxscrnsaver
                                             libxcomposite
                                             libxt
                                             libffi
                                             ffmpeg
                                             libvpx
                                             icu4c
                                             pixman
                                             pulseaudio
                                             mesa
                                             mit-krb5
                                             sqlite
                                             startup-notification
                                             unzip
                                             zip
                                             zlib))

;; ,@(if patched-firefox-include-adobe-flash
;;       (list patchelf-adobe-flashplugin)
;;       `())

(define nongnu-mozilla-firefox-inputs `(bzip2
                                        cairo
                                        cups
                                        dbus-glib
                                        freetype
                                        ffmpeg
                                        gdk-pixbuf
                                        glib
                                        gtk+
                                        gtk+-2
                                        hunspell
                                        icu4c-71
                                        jemalloc
                                        libcanberra
                                        libevent
                                        libffi
                                        libgnome
                                        libjpeg-turbo
                                        ;; "libpng-apng" ,libpng-apng
                                        libvpx
                                        libxcomposite
                                        libxft
                                        libxinerama
                                        libxscrnsaver
                                        libxt
                                        mesa
                                        mit-krb5
                                        ;; "nspr" ,nspr
                                        ;; "nss" ,nss
                                        pango
                                        pixman
                                        pulseaudio
                                        startup-notification
                                        sqlite
                                        unzip
                                        zip
                                        zlib))

(define nongnu-mozilla-native-inputs (list autoconf-2.13
                                           `(,rust "cargo")
                                           clang
                                           llvm
                                           nasm
                                           node
                                           perl
                                           pkg-config
                                           python
                                           ("python2" python-2.7)
                                           rust
                                           rust-cbindgen
                                           which
                                           yasm))

(define patched-firefox-inputs  (list glib
                                      `(,gcc "lib")
                                      dbus
                                      libxcomposite
                                      libxt
                                      gtk+
                                      atk
                                      cairo
                                      dbus-glib
                                      fontconfig
                                      freetype
                                      gdk-pixbuf
                                      glib
                                      ;; glibc
                                      libx11
                                      libxcb
                                      libxdamage
                                      libxext
                                      libxfixes
                                      libxrender
                                      pango
                                      pulseaudio
                                      libogg
                                      libvorbis
                                      libevent
                                      libxinerama
                                      libxscrnsaver
                                      libffi
                                      ffmpeg
                                      libvpx
                                      `,@nongnu-mozilla-firefox-inputs))

(define patched-firefox-native-inputs `(,@nongnu-mozilla-native-inputs))

(define patched-firefox-rearrange-method `(lambda* (#:key inputs outputs #:allow-other-keys)
                                            ;; This overwrites the installed launcher, which execs xulrunner,
                                            ;; with one that execs 'icecat --app'
                                            ;; (define source (getcwd))
                                            ;; (use-modules (lotus build patchelf-utils))
                                            (define (required-link? file)
                                              (or (directory? file)
                                                  (string-suffix? ".sh"  file)
                                                  (string-suffix? ".dat" file)
                                                  (string-suffix? ".xml" file)
                                                  (string-suffix? ".ja"  file)))
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
                                                         (string-append firefox-bin  "/dependentlibs.list"))
                                              (invoke "sed" "-i" "s@^lib@../lib/lib@g"
                                                      (string-append firefox-bin "/dependentlibs.list"))
                                              (mkdir-p bin-dir)
                                              (symlink "../share/firefox/bin/firefox"  (string-append bin-dir "/firefox"))
                                              ;; (delete-file (string-append firefox-bin "/updater"))
                                              (for-each (lambda (file)
                                                          (format #t "misc: ~a~%" file)
                                                          (let* ((rel-misc (string-drop firefox-misc (string-length (string-append source
                                                                                                                                   "/share/firefox/"))))
                                                                 (rfile    (string-append "../" rel-misc "/" file))
                                                                 (target   (string-append firefox-bin "/" (basename rfile))))
                                                            (format #t "file: ~a ~a~%" rfile (string-append firefox-misc "/" file))
                                                            (when (required-link? (string-append firefox-misc "/" file))
                                                              (format #t "symlink ~a ~a~%" rfile target)
                                                              (symlink rfile target))))
                                                        (directory-list-files firefox-misc))

                                              (begin
                                                (mkdir-p "lib")
                                                (copy-file (string-append firefox-lib "/libmozsandbox.so") "lib/libmozsandbox.so"))

                                              (when ,patched-firefox-include-adobe-flash
                                                (symlink (string-append (assoc-ref inputs "patchelf-adobe-flashplugin") "/lib/adobe-flashplugin"
                                                                        (string-append firefox-bin "/browser/plugins")))
                                                (begin
                                                  (mkdir-p (string-append firefox-bin "/browser/plugins"))
                                                  (copy-file (string-append (assoc-ref inputs "patchelf-adobe-flashplugin") "/lib/adobe-flashplugin/" "libflashplayer.so")
                                                             (string-append firefox-bin "/browser/plugins/" "libflashplayer.so"))
                                                  (copy-file (string-append (assoc-ref inputs "patchelf-adobe-flashplugin") "/lib/adobe-flashplugin/" "libpepflashplayer.so")
                                                             (string-append firefox-bin "/browser/plugins/" "libpepflashplayer.so"))
                                                  (for-each (lambda (path)
                                                              (let* ((stat (lstat path)))
                                                                (chmod path (logior #o111 (stat:perms stat)))))
                                                            (list (string-append firefox-bin "/browser/plugins/" "libflashplayer.so")
                                                                  (string-append firefox-bin "/browser/plugins/" "libpepflashplayer.so")))))
                                              #t)))

(define patched-firefox-validate-method `(lambda* (#:key (validate-runpath? #t)
                                                         (elf-directories '("share/firefox/lib"
                                                                            "share/firefox/lib64"
                                                                            "share/firefox/libexec"
                                                                            "share/firefox/sbin"
                                                                            "share/firefox/bin"))
                                                         outputs
                                                         #:allow-other-keys)
                                           (define gnu:validate-runpath (assoc-ref %standard-phases 'validate-runpath))
                                           (gnu:validate-runpath #:validate-runpath? validate-runpath?
                                                                 #:elf-directories   elf-directories
                                                                 #:outputs           outputs)))

(define patched-firefox-phases `(modify-phases %standard-phases
                                  (add-after 'build 'rearrange
                                    ,patched-firefox-rearrange-method)
                                  (replace 'validate-runpath
                                    ,patched-firefox-validate-method)))

(define-public patched-firefox-0.0
  ;; (hidden-package
  (package
     (name "patched-firefox-0.0")
     (version "0.0")
     (source (origin (method    url-fetch)
                     (uri       (string-append "https://ftp.mozilla.org/pub/firefox/releases/" version "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
                     (file-name (string-append "firefox-" version ".tar.bz2"))
                     (sha256    (base32 "06w2pkfxf9yj68h9i7h4765md0pmgn8bdh5qxg7jrf3n22ikhngb"))))
     (build-system   patchelf-build-system)
     (inputs         patched-firefox-inputs)
     (native-inputs  patched-firefox-native-inputs)
     (arguments `(#:input-lib-mapping '(("nss" "lib/nss")
                                        ("adobe-flashplugin" "lib/adobe-flashplugin/")
                                        ("out" "share/firefox/lib"))
                  #:phases      ,patched-firefox-phases))
     (synopsis "Patched-Firefox")
     (description "Patched-Firefox.")
     (home-page "https://www.mozilla.org")
     ;; Conkeror is triple licensed.
     (license (list
               ;; MPL 1.1 -- this license is not GPL compatible
               license:gpl2
               license:lgpl2.1))))

(define-public patched-firefox-56.0
  (package (inherit patched-firefox-0.0)
           (name "patched-firefox-56.0")
           (version "56.0")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://ftp.mozilla.org/pub/firefox/releases/" version "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
                           (file-name (string-append "firefox-" version ".tar.bz2"))
                           (sha256    (base32 "06w2pkfxf9yj68h9i7h4765md0pmgn8bdh5qxg7jrf3n22ikhngb"))))))

(define-public patched-firefox-75.0
  (package (inherit patched-firefox-0.0)
           (name "patched-firefox-75.0")
           (version "75.0")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://ftp.mozilla.org/pub/firefox/releases/" version "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
                           (file-name (string-append "firefox-" version ".tar.bz2"))
                           (sha256    (base32 "19jarabhbr141hpaqsfvwdlrhvbjsn9maww28bg1pbf4h41p1nf5"))))
           (arguments `(#:input-lib-mapping '(("nss" "lib/nss")
                                              ("adobe-flashplugin" "lib/adobe-flashplugin/")
                                              ("out" "share/firefox/lib"))
                        #:phases      (modify-phases %standard-phases
                                        (add-after
                                            'build 'rearrange
                                          ,patched-firefox-rearrange-method)
                                        (delete 'validate-runpath))))))

(define-public patched-firefox-80.0
  (package (inherit patched-firefox-0.0)
           (name "patched-firefox-80.0")
           (version "80.0")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://ftp.mozilla.org/pub/firefox/releases/" version "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
                           (file-name (string-append "firefox-" version ".tar.bz2"))
                           (sha256    (base32 "1rxwyzba50hji5vr53n45c0wi37n631yc009iimx2z4jvl31y6c4"))))
           (arguments `(#:input-lib-mapping '(("nss" "lib/nss")
                                              ("adobe-flashplugin" "lib/adobe-flashplugin/")
                                              ("out" "share/firefox/lib"))
                        #:phases      (modify-phases %standard-phases
                                        (add-after
                                            'build 'rearrange
                                          ,patched-firefox-rearrange-method)
                                        (delete 'validate-runpath))))))

