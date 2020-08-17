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
  #:use-module (guix build rpath)
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
  #:use-module (gnu packages libreoffice)  ;for hunspell
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

(define retro-firefox-include-adobe-flash #f)

(define retro-firefox-inputs-problem `(("libc"          ,glibc
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
                                        ("libvpx"        ,libvpx)

                                        ;; machine hang
                                        ("libfdk"        ,libfdk)
                                        ("libtheora"     ,libtheora)
                                        ("wavpack"       ,wavpack)
                                        ("libwebp"       ,libwebp)
                                        ("speex"         ,speex)
                                        ("opus"          ,opus)
                                        ("x265"          ,x265)
                                        ("xvid"          ,xvid)
                                        ("libxv"         ,libxv)



                                        ("gst-libav"        ,gst-libav)
                                        ("gst-plugins-base" ,gst-plugins-base)
                                        ("gst-plugins-good" ,gst-plugins-good)
                                        ("gst-plugins-bad"  ,gst-plugins-bad)
                                        ("gst-plugins-ugly" ,gst-plugins-ugly)
                                        ("gst123"           ,gst123)
                                        ("gstreamer"     ,gstreamer)
                                        ("openh264"      ,openh264)
                                        ("libsmpeg"      ,libsmpeg)
                                        ("libmpeg2"      ,libmpeg2)
                                        ;; ("libmad"        ,libmad)

                                        ("vlc"  ,vlc)
                                        ("alsa-lib" ,alsa-lib)
                                        ("bzip2" ,bzip2)
                                        ("cups" ,cups)
                                        ("dbus-glib" ,dbus-glib)
                                        ("gdk-pixbuf" ,gdk-pixbuf)
                                        ("glib" ,glib)
                                        ("gtk+" ,gtk+)
                                        ("gtk+-2" ,gtk+-2)
                                        ("graphite2" ,graphite2)
                                        ("pango" ,pango)
                                        ("freetype" ,freetype)
                                        ("harfbuzz" ,harfbuzz)
                                        ("libcanberra" ,libcanberra)
                                        ("libgnome" ,libgnome)
                                        ("libjpeg-turbo" ,libjpeg-turbo)
                                        ("libogg" ,libogg)
                                        ;; ("libtheora" ,libtheora) ; wants theora-1.2, not yet released
                                        ("libvorbis" ,libvorbis)
                                        ("libxft" ,libxft)
                                        ("libevent" ,libevent)
                                        ("libxinerama" ,libxinerama)
                                        ("libxscrnsaver" ,libxscrnsaver)
                                        ("libxcomposite" ,libxcomposite)
                                        ("libxt" ,libxt)
                                        ("libffi" ,libffi)
                                        ("ffmpeg" ,ffmpeg)
                                        ("libvpx" ,libvpx)
                                        ("icu4c" ,icu4c)
                                        ("pixman" ,pixman)
                                        ("pulseaudio" ,pulseaudio)
                                        ("mesa" ,mesa)
                                        ("mit-krb5" ,mit-krb5)
                                        ("sqlite" ,sqlite)
                                        ("startup-notification" ,startup-notification)
                                        ("unzip" ,unzip)
                                        ("zip" ,zip)
                                        ("zlib" ,zlib)

                                        ,@(if retro-firefox-include-adobe-flash
                                              (list `("patchelf-adobe-flashplugin" ,patchelf-adobe-flashplugin))
                                              `()))))

(define nongnu-mozilla-firefox-inputs `(("bzip2" ,bzip2)
                                        ("cairo" ,cairo)
                                        ("cups" ,cups)
                                        ("dbus-glib" ,dbus-glib)
                                        ("freetype" ,freetype)
                                        ("ffmpeg" ,ffmpeg)
                                        ("gdk-pixbuf" ,gdk-pixbuf)
                                        ("glib" ,glib)
                                        ("gtk+" ,gtk+)
                                        ("gtk+-2" ,gtk+-2)
                                        ("hunspell" ,hunspell)
                                        ("icu4c" ,icu4c-67)
                                        ("jemalloc" ,jemalloc)
                                        ("libcanberra" ,libcanberra)
                                        ("libevent" ,libevent)
                                        ("libffi" ,libffi)
                                        ("libgnome" ,libgnome)
                                        ("libjpeg-turbo" ,libjpeg-turbo)
                                        ;; ("libpng-apng" ,libpng-apng)
                                        ("libvpx" ,libvpx)
                                        ("libxcomposite" ,libxcomposite)
                                        ("libxft" ,libxft)
                                        ("libxinerama" ,libxinerama)
                                        ("libxscrnsaver" ,libxscrnsaver)
                                        ("libxt" ,libxt)
                                        ("mesa" ,mesa)
                                        ("mit-krb5" ,mit-krb5)
                                        ;; ("nspr" ,nspr)
                                        ;; ("nss" ,nss)
                                        ("pango" ,pango)
                                        ("pixman" ,pixman)
                                        ("pulseaudio" ,pulseaudio)
                                        ("startup-notification" ,startup-notification)
                                        ("sqlite" ,sqlite)
                                        ("unzip" ,unzip)
                                        ("zip" ,zip)
                                        ("zlib" ,zlib)))

(define nongnu-mozilla-native-inputs `(("autoconf" ,autoconf-2.13)
                                       ("cargo" ,rust-1.41 "cargo")
                                       ("clang" ,clang)
                                       ("llvm" ,llvm)
                                       ("nasm" ,nasm)
                                       ("node" ,node)
                                       ("perl" ,perl)
                                       ("pkg-config" ,pkg-config)
                                       ("python" ,python)
                                       ("python2" ,python-2.7)
                                       ("rust" ,rust-1.41)
                                       ("rust-cbindgen" ,rust-cbindgen)
                                       ("which" ,which)
                                       ("yasm" ,yasm)))

(define retro-firefox-inputs `(("libc"          ,glibc)
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
                               ;; ("glibc"         ,glibc)
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
                               ("libvpx"        ,libvpx)
                               ,@nongnu-mozilla-firefox-inputs))

(define retro-firefox-native-inputs `(,@nongnu-mozilla-native-inputs))

(define retro-firefox-phases `(modify-phases %standard-phases
                                       (add-after
                                        'build 'rearrange
                                        (lambda* (#:key inputs outputs #:allow-other-keys)
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
                                            ;; (rename-file (string-append firefox-bin "/updater") (string-append firefox-bin "/stop-updater"))
                                            (delete-file (string-append firefox-bin "/updater"))
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

                                            (when ,retro-firefox-include-adobe-flash
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
                                                                        #:outputs           outputs)))))

(define-public retro-firefox-56.0
  ;; (hidden-package)
  (package
   (name "retro-firefox-56.0")
   (version "56.0")
   (source (origin (method    url-fetch)
                   (uri       (string-append "https://ftp.mozilla.org/pub/firefox/releases/" version "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
                   (file-name (string-append "firefox-" version ".tar.bz2"))
                   (sha256    (base32 "06w2pkfxf9yj68h9i7h4765md0pmgn8bdh5qxg7jrf3n22ikhngb"))))
   (build-system patchelf-build-system)
   (inputs         retro-firefox-inputs)
   (native-inputs  retro-firefox-native-inputs)
   (arguments `(#:input-lib-mapping '(("nss" "lib/nss")
                                      ("adobe-flashplugin" "lib/adobe-flashplugin/")
                                      ("out" "share/firefox/lib"))
                #:phases      ,retro-firefox-phases))
   (synopsis "Retro-Firefox")
   (description "Retro-Firefox.")
   (home-page "https://www.mozilla.org")
   ;; Conkeror is triple licensed.
   (license (list
             ;; MPL 1.1 -- this license is not GPL compatible
             license:gpl2
             license:lgpl2.1))))

(define-public retro-firefox
  (package (inherit retro-firefox-56.0)
           (name "retro-firefox")))


retro-firefox

