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
     '(#:modules ((guix build-system gnu) #:prefix gnu:)
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

(define-public adobe-flashplugin
  ;; http://archive.canonical.com/ubuntu/pool/partner/a/adobe-flashplugin/adobe-flashplugin_20191210.1-0ubuntu0.19.10.2_amd64.deb
  (package
    (name "adobe-flashplugin")
    (version "20191210")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://archive.canonical.com/ubuntu/pool/partner/a/adobe-flashplugin/adobe-flashplugin_" version ".1-0ubuntu0.19.10.2_amd64.deb"))
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
                                       ("nss" "lib/nss"))))
    (synopsis "")
    (description "")
    (home-page "https://www-zeuthen.desy.de/~friebel/unix/lesspipe.html")
    (license license:ibmpl1.0)))





(define-public browser-plugin-freshplayer-pepperflash
  ;; http://mirrors.kernel.org/ubuntu/pool/multiverse/f/freshplayerplugin/browser-plugin-freshplayer-pepperflash_0.3.4-3_amd64.deb
  (package
    (name "browser-plugin-freshplayer-pepperflash")
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
    (home-page "https://www-zeuthen.desy.de/~friebel/unix/lesspipe.html")
    (license license:ibmpl1.0)))
