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

(define-module (lotus packages flash)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((guix build-system cmake) #:prefix cmake:)
  #:use-module ((lotus build-system deb) #:prefix deb:)
  #:use-module ((lotus build-system patchelf) #:prefix patchelf:)
  #:use-module ((guix  build-system copy) #:prefix copy:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer))

(define-public deb-adobe-flashplugin
  ;; http://archive.canonical.com/ubuntu/pool/partner/a/adobe-flashplugin/adobe-flashplugin_20191210.1-0ubuntu0.19.10.2_amd64.deb
  (package
    (name "deb-adobe-flashplugin")
    (version "20191210.1-0ubuntu0.19.10.2_amd64")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://archive.canonical.com/ubuntu/pool/partner/a/adobe-flashplugin/adobe-flashplugin_" version ".deb"))
              (file-name (string-append "adobe-flashplugin-" version ".deb"))
              (sha256
               (base32
                "0651ky7gdnvxckzp6bir79k2426krgqak1gd2dqwh521s3sk66gn"))))
    (build-system deb:deb-build-system)
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
              ("libvpx"        ,libvpx)
              ("gtk+"          ,gtk+-2)
              ("nspr"          ,nspr)
              ("nss"           ,nss)))
    (arguments `(#:input-lib-mapping '(("out" "lib")
                                       ("nss" "lib/nss"))
                 #:phases            (modify-phases %standard-phases
                                       (add-after
                                           'build 'correct-permission
                                         (lambda* (#:key inputs outputs #:allow-other-keys)
                                           (let* ((file (string-append "lib/adobe-flashplugin/" "libflashplayer.so"))
                                                  (stat (lstat file)))
                                             (chmod file (logior #o111 (stat:perms stat))))
                                           (let* ((file (string-append "lib/adobe-flashplugin/" "libpepflashplayer.so"))
                                                  (stat (lstat file)))
                                             (chmod file (logior #o111 (stat:perms stat)))))))))
    (synopsis "")
    (description "")
    (home-page "https://www-zeuthen.desy.de/~friebel/unix/lesspipe.html")
    (license license:ibmpl1.0)))

(define-public deb-browser-plugin-freshplayer-pepperflash
  ;; http://mirrors.kernel.org/ubuntu/pool/multiverse/f/freshplayerplugin/browser-plugin-freshplayer-pepperflash_0.3.4-3_amd64.deb
  (package
    (name "deb-browser-plugin-freshplayer-pepperflash")
    (version "0.3.4-3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://mirrors.kernel.org/ubuntu/pool/multiverse/f/freshplayerplugin/browser-plugin-freshplayer-pepperflash_" version "_amd64.deb"))
              (file-name (string-append name "-" version ".deb"))
              (sha256
               (base32
                "0hwwx1962kky8hw3bdf8rrjhhjalf635y3v391i83wgmk3zzfcjm"))))
    (build-system deb:deb-build-system)
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
              ("libvpx"        ,libvpx)
              ("gtk+"          ,gtk+-2)
              ("nspr"          ,nspr)
              ("nss"           ,nss)
              ("alsa-lib"      ,alsa-lib)
              ("libevent"      ,libevent)
              ("openssl"       ,openssl)
              ("ffmpeg"        ,ffmpeg)))
    (arguments `(#:input-lib-mapping '(("out" "lib")
                                       ("nss" "lib/nss"))))
    (synopsis "")
    (description "")
    (home-page "https://wiki.debian.org/PepperFlashPlayer")
    (license license:ibmpl1.0)))

(define-public patchelf-adobe-flashplugin
  (package
   (name "patchelf-adobe-flashplugin")
   (version "32.0.0.330")
   (source (origin
            (method url-fetch)
            (uri
             (string-append "https://fpdownload.adobe.com/get/flashplayer/pdc/" version "/flash_player_npapi_linux.x86_64.tar.gz"))
            (file-name (string-append "flash_player_npapi_linux.x86_64.tar.gz"))
            (sha256
             (base32
              "1pf3k1x8c2kbkc9pf9y5n4jilp3g41v8v0q5ng77sbnl92s35zsj"))))
   (build-system patchelf:patchelf-build-system)
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
             ("libvpx"        ,libvpx)
             ("gtk+"          ,gtk+-2)
             ("nspr"          ,nspr)
             ("nss"           ,nss)))
   (arguments `(#:input-lib-mapping '(("out" "lib")
                                      ("nss" "lib/nss"))
                #:phases            (modify-phases %standard-phases
                                      (add-after
                                          'unpack 'changedir
                                        (lambda* (#:key inputs outputs #:allow-other-keys)
                                          (chdir "..")
                                          (let ((cwd (getcwd)))
                                            (begin
                                              (let* ((parent (getcwd))
                                                     (source (string-append (getcwd) "/unpack"))
                                                     (files (directory-list-files parent)))
                                                (for-each (lambda (entry)
                                                            (let ((src (string-append parent "/" entry))
                                                                  (trg (string-append source "/" entry)))
                                                              (mkdir-p (dirname trg))
                                                              (rename-file src trg)))
                                                          files)))
                                            (begin
                                              (delete-file (string-append cwd "/unpack/" "usr/lib/kde4/kcm_adobe_flash_player.so"))
                                              (if #f
                                               (symlink "../../lib64/kde4/kcm_adobe_flash_player.so"
                                                        (string-append cwd "/unpack/" "usr/lib/kde4/kcm_adobe_flash_player.so"))
                                               (delete-file (string-append cwd "/unpack/" "usr/lib64/kde4/kcm_adobe_flash_player.so"))))
                                            (begin
                                              (begin
                                                (delete-file (string-append cwd "/unpack/" "usr/bin/flash-player-properties"))
                                                ;; (delete-file (string-append cwd "/unpack/" "usr/bin"))
                                                (for-each (lambda (path)
                                                            (if (access? (string-append cwd "/unpack/usr/" path) F_OK)
                                                                (copy-recursively (string-append cwd "/unpack/usr/" path) (string-append cwd "/source/" path))
                                                                (format #t "~a not exists.~%" (string-append cwd "/unpack/usr/" path))))
                                                          (list "lib64"
                                                                "share"
                                                                ;; "bin"
                                                                "lib")))
                                              (begin
                                                (mkdir-p (string-append cwd "/source/share/patchelf-adobe-flashplugin"))
                                                (mkdir-p (string-append cwd "/source/lib/adobe-flashplugin"))
                                                (copy-recursively (string-append cwd "/unpack/" "LGPL") (string-append cwd "/source/share/patchelf-adobe-flashplugin/LGPL"))
                                                (copy-file (string-append cwd "/unpack/" "readme.txt")  (string-append cwd "/source/share/patchelf-adobe-flashplugin/readme.txt"))
                                                (copy-file (string-append cwd "/unpack/" "license.pdf") (string-append cwd "/source/share/patchelf-adobe-flashplugin/license.pdf"))
                                                (mkdir-p   (string-append cwd "/source/lib"))
                                                (copy-file (string-append cwd "/unpack/" "libflashplayer.so") (string-append cwd "/source/lib/libflashplayer.so"))
                                                (copy-file (string-append cwd "/unpack/" "libflashplayer.so") (string-append cwd "/source/lib/adobe-flashplugin/libflashplayer.so")))
                                              (begin
                                                (for-each (lambda (path)
                                                            (let* ((stat (lstat path)))
                                                              (chmod path (logior #o111 (stat:perms stat)))))
                                                          (list (string-append cwd "/source/lib/libflashplayer.so")
                                                                (string-append cwd "/source/lib/adobe-flashplugin/libflashplayer.so")))))
                                            (chdir (string-append cwd "/source"))
                                            #t))))))
   (synopsis "")
   (description "")
   (home-page "https://www-zeuthen.desy.de/~friebel/unix/lesspipe.html")
   (license license:ibmpl1.0)))


(define-public gnash
  (package
    (name "gnash")
    (version "0.8.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.gnu.org/gnu/gnash/" version "/gnash-" version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "090j5lly5r6jzbnvlc3mhay6dsrd9sfrkjcgqaibm4nz8lp0f9cn"))))
    (build-system gnu:gnu-build-system)
    (inputs    `(("gconf"     ,gconf)
                 ("libungif"  ,libungif)
                 ("sdl"       ,sdl)
                 ("agg"       ,agg)
                 ("gstreamer" ,gstreamer)))
    (arguments '(#:configure-flags '("--without-gconf")))
    (synopsis "GNU Gnash is the GNU Flash movie player")
    (description "GNU Gnash

GNU Gnash is the GNU Flash movie player — Flash is an animation file format
pioneered by Macromedia which continues to be supported by their successor
company, Adobe. Flash has been extended to include audio and video content, and
programs written in ActionScript, an ECMAScript-compatible language. Gnash is
based on GameSWF, and supports most SWF v7 features and some SWF v8 and v9.

SWF v10 is not supported by GNU Gnash")
    (home-page "https://www.gnu.org/software/gnash/")
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))

(define-public lightspark
  (package
    (name "lightspark")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/lightspark/lightspark/archive/lightspark-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pi896syzbpfdr1lisrb6v2y1sc5bvk98cf63s1ls4xniq61byy7"))))
    (build-system cmake:cmake-build-system)
    ;; (native-inputs
    ;;  `(("pkg-config" ,pkg-config)
    ;;    ("which"      ,which)))
    (inputs
     `(("gnash"    ,gnash)))

    ;; TODO: figure out solution 
    ;; https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/messaging.scm#n1878
    ;; https://github.com/EionRobb/lightspark/blob/master/skypeweb/CMakeLists.txt
    ;; (arguments
    ;;  `(#:tests? #f                            ; Run the test suite (this is the default)
    ;;    ;; #:configure-flags '("-DUSE_SHA1DC=ON") ; SHA-1 collision detection
    ;;    #:phases
    ;;    (modify-phases %standard-phases
    ;;      (add-after 'unpack 'change-dir
    ;;        (lambda _ (chdir "skypeweb"))
    ;;        (substitute* "CMakeLists.txt"
    ;;          (("variable=plugindir purple 2>/dev/null")
    ;;           ("variable=plugindir purple 2>/dev/null")))))))
    ;; (arguments
    ;;  `(#:modules ((guix build utils))
    ;;              #:builder (begin)))
    (synopsis "Lightspark is an open source Flash player implementation for playing files in SWF format")
    (description "Lightspark is an open source Flash player implementation for
playing files in SWF format. Lightspark can run as a web browser plugin or as a
standalone application.

Lightspark supports SWF files written on all versions of the ActionScript language.")
    (home-page "http://lightspark.github.io/")
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))

(define-public rofi-master
  (package (inherit rofi)
           (name "rofi-master")
           (version "master")
           (source (origin
                     (method git-fetch)
                     (uri (git-reference
                           (url "https://github.com/DaveDavenport/rofi.git")
                           (commit version)))
                     (sha256
                      (base32
                       "0yf3iaqq4vgy9pickdd0zkniksczwjx7zripmsa0f54na9pny6lz"))))))


