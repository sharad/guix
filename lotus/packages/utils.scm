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

(define-module (lotus packages utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((lotus build-system deb) #:prefix deb:)
  #:use-module ((lotus build-system cmake) #:prefix cmake:)
  #:use-module ((lotus build-system patchelf) #:prefix patchelf:)
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
  #:use-module (gnu packages nss))

;; https://issues.guix.gnu.org/issue/35619

(define-public lesspipe
  (package
    (name "lesspipe")
    (version "lesspipe")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wofr06/lesspipe.git")
                    (commit version)))
              (sha256
               (base32
                "1pw9mwwfx0k7xqriavhiffh706sh44gfl991c4afh84903wabq7l"))))
    (build-system gnu:gnu-build-system)
    (inputs
     `(("bzip2" ,bzip2)
       ("perl"  ,perl)))
    (arguments
     '(#:modules (((guix build-system gnu) #:prefix gnu:))
       #:tests? #f
       #:make-flags (let ((out  (assoc-ref %outputs "out")))
                      (list
                       (string-append "PREFIX=" out)))
       #:phases
       (modify-phases gnu:%standard-phases
         (delete 'configure))))

    (synopsis "lesspipe.sh, a preprocessor for less")
    (description "To browse files under UNIX the excellent viewer less [1] can be used. By
 setting the environment variable LESSOPEN, less can be enhanced by external
 filters to become even more powerful. Most Linux distributions come already
 with a \"lesspipe.sh\" that covers the most common situations.

 The input filter for less described here is called \"lesspipe.sh\". It is able
 to process a wide variety of file formats. It enables users to deeply inspect
 archives and to display the contents of files in archives without having to
 unpack them before. That means file contents can be properly interpreted even
 if the files are compressed and contained in a hierarchy of archives (often
 found in RPM or DEB archives containing source tarballs). The filter is easily
 extensible for new formats.

 The input filter which is also called \"lesspipe.sh\" is written in a ksh
 compatible language (ksh, bash, zsh) as one of these is nearly always installed
 on UNIX systems and uses comparably few resources. Otherwise an implementation
 in perl for example would have been somewhat simpler to code. The code looks
 less clean than it could as it was tried to make the script compatible with
 a number of old shells and applications especially found on non Linux systems.

 The filter does different things depending on the file format. In most cases
 it is determined on the output of the \"file\" command [2], [6], that recognizes
 lots of formats. Only in a few cases the file suffix is used to determine what
 to display. Up to date file descriptions are included in the \"file\" package.
 Maintaining a list of file formats is therefore only a matter of keeping that
 package up to date.")
    (home-page "https://www-zeuthen.desy.de/~friebel/unix/lesspipe.html")
    (license license:ibmpl1.0)))

(define-public pi-hole
  "https://github.com/pi-hole/pi-hole/blob/master/automated%20install/basic-install.sh")

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
              ("libvpx"        ,libvpx-1.7)
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
              (file-name (string-append "browser-plugin-freshplayer-pepperflash-" version ".deb"))
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
              ("libvpx"        ,libvpx-1.7)
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
  ;; 
  (package
   (name "patchelf-adobe-flashplugin")
   (version "32.0.0.303")
   (source (origin
            (method url-fetch)
            (uri
             (string-append "https://fpdownload.adobe.com/get/flashplayer/pdc/" version "/flash_player_npapi_linux.x86_64.tar.gz"))
            (file-name (string-append "flash_player_npapi_linux.x86_64.tar.gz"))
            (sha256
             (base32
              "0x0mabgswly2v8z13832pkbjsv404aq61pback6sgmp2lyycdg6w"))))
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
             ("libvpx"        ,libvpx-1.7)
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

;; https://www.forticlient.com/repoinfo
;; https://repo.fortinet.com/repo/ubuntu/pool/multiverse/forticlient/forticlient_6.0.8.0140_amd64.deb
;; https://repo.fortinet.com/repo/ubuntu/pool/multiverse/forticlient/forticlient_6.0.8.0140_amd64_u18.deb


;; https://github.com/lightspark/lightspark/tree/lightspark-0.8.1
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
    ;; (inputs
    ;;  `(("pidgin"    ,pidgin)
    ;;    ;; ("libgcrypt" ,libgcrypt)
    ;;    ;; ("libwebp"   ,libwebp)
    ;;    ;; ("gettext"   ,gnu-gettext)
    ;;    ;; ("gtk+"      ,gtk+-2)
    ;;    ;; ("zlib"      ,zlib)
    ;;    ("glib"      ,glib)
    ;;    ("json-glib" ,json-glib)))

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
    (description "Lightspark is an open source Flash player implementation for playing files in SWF format. Lightspark can run as a web browser plugin or as a standalone application.

Lightspark supports SWF files written on all versions of the ActionScript language.")
    (home-page "http://lightspark.github.io/")
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))

(define-public deb-forticlient
  (package
    (name "deb-forticlient")
    (version "6.0.8.0140_amd64")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://repo.fortinet.com/repo/ubuntu/pool/multiverse/forticlient/forticlient_" version "_u18.deb"))
              (file-name (string-append "browser-plugin-freshplayer-pepperflash-" version ".deb"))
              (sha256
               (base32
                "0gs8rm62hrvwf6j4ia24sa5frglnif0qcr3lvm6n3vgr1nkhyymw"))))
    (build-system deb:deb-build-system)
    (arguments `(#:input-lib-mapping '(("out" "lib"))
                 #:phases            (modify-phases %standard-phases
                                       (delete 'validate-runpath))))
    (synopsis "")
    (description "")
    (home-page "https://www.forticlient.com/repoinfo")
    (license license:ibmpl1.0)))



