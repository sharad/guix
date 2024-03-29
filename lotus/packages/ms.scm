
(define-module (lotus packages ms)
  #:use-module (ice-9 ftw)
  #:use-module (lotus build patchelf-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((lotus build-system deb) #:prefix deb:)
  #:use-module ((lotus build-system rpm) #:prefix rpm:)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages video)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages chromium))

(define-public rpm-teams
  (package (name "rpm-teams")
           (version "1.3.00.5153-1")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://packages.microsoft.com/yumrepos/ms-teams/teams-" version ".x86_64.rpm"))
                           (file-name (string-append "teams-" version ".x86_64.rpm"))
                           (sha256    (base32 "009rh55r56k1zd7spiz6hy5chgkq9bg4d0zim9rwjyg28754r4zy"))))
           (inputs `(("chromium"                 ,ungoogled-chromium)
                     ("bash"                     ,bash)
                     ("ffmpeg"                   ,ffmpeg)
                     ("alsa-lib"                 ,alsa-lib)
                     ("atk"                      ,atk)
                     ("at-spi2-atk"              ,at-spi2-atk)
                     ("cairo"                    ,cairo)
                     ("cups"                     ,cups)
                     ("cups-minimal"             ,cups-minimal)
                     ("dbus"                     ,dbus)
                     ("expat"                    ,expat)
                     ("gcc:lib"                  ,gcc "lib")
                     ("gcc-toolchain"            ,gcc-toolchain)
                     ("gdk-pixbuf"               ,gdk-pixbuf)
                     ;; ("gdk-pixbuf+svg"           ,gdk-pixbuf+svg)
                     ("gfortran:lib"             ,gfortran "lib")
                     ("glib"                     ,glib)
                     ("glibc"                    ,glibc)
                     ("gtk+"                     ,gtk+)
                     ("libgnome-keyring"         ,libgnome-keyring)
                     ("libsecret"                ,libsecret)
                     ("libx11"                   ,libx11)
                     ("libxcb"                   ,libxcb)
                     ("libxcomposite"            ,libxcomposite)
                     ("libxcursor"               ,libxcursor)
                     ("libxdamage"               ,libxdamage)
                     ("libxext"                  ,libxext)
                     ("libxfixes"                ,libxfixes)
                     ("libxi"                    ,libxi)
                     ("libxkbfile"               ,libxkbfile)
                     ("libxrandr"                ,libxrandr)
                     ("libxrender"               ,libxrender)
                     ("libxscrnsaver"            ,libxscrnsaver)
                     ("libxtst"                  ,libxtst)
                     ("mesa"                     ,mesa)
                     ("nspr"                     ,nspr)
                     ("nss"                      ,nss)
                     ("pango"                    ,pango)
                     ("fontconfig"               ,fontconfig)
                     ("util-linux"               ,util-linux)
                     ("util-linux:lib"           ,util-linux "lib")
                     ("util-linux-with-udev:lib" ,util-linux+udev "lib")
                     ("fontconfig"               ,fontconfig)))
           (propagated-inputs `(("gcc:lib"          ,gcc "lib")
                                ("fontconfig"       ,fontconfig)
                                ("libsecret"        ,libsecret)
                                ("pulseaudio"       ,pulseaudio)))
           (native-inputs `(("bash" ,bash)))
           (build-system rpm:rpm-build-system)
           (arguments `(#:input-lib-mapping '(("out" "share/teams")
                                              ("out" "share/teams/swiftshader")
                                              ("nss" "lib/nss"))))
           (synopsis "rpm-teams")
           (description "rpm-teams.")
           (home-page "https://teams")
           (license license:asl2.0)))


(define-public deb-teams
  (package (name "deb-teams")
           (version "1.4.00.13653")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://packages.microsoft.com/repos/ms-teams/pool/main/t/teams//teams_" version "_amd64.deb"))
                           (file-name (string-append "teams-" version ".amd64.deb"))
                           (sha256    (base32 "1kx4j837fd344zy90nl0j3r8cdvihy6i6gf56wd5n56zngx1fhjv"))))
           (inputs `(("chromium"                 ,ungoogled-chromium)
                     ("bash"                     ,bash)
                     ("ffmpeg"                   ,ffmpeg)
                     ("alsa-lib"                 ,alsa-lib)
                     ("atk"                      ,atk)
                     ("at-spi2-atk"              ,at-spi2-atk)
                     ("cairo"                    ,cairo)
                     ("cups"                     ,cups)
                     ("cups-minimal"             ,cups-minimal)
                     ("dbus"                     ,dbus)
                     ("expat"                    ,expat)
                     ("gcc:lib"                  ,gcc "lib")
                     ("gcc-toolchain"            ,gcc-toolchain)
                     ("gdk-pixbuf"               ,gdk-pixbuf)
                     ;; ("gdk-pixbuf+svg"           ,gdk-pixbuf+svg)
                     ("gfortran:lib"             ,gfortran "lib")
                     ("glib"                     ,glib)
                     ("glibc"                    ,glibc)
                     ("gtk+"                     ,gtk+)
                     ("libgnome-keyring"         ,libgnome-keyring)
                     ("libsecret"                ,libsecret)
                     ("libx11"                   ,libx11)
                     ("libxcb"                   ,libxcb)
                     ("libxcomposite"            ,libxcomposite)
                     ("libxcursor"               ,libxcursor)
                     ("libxdamage"               ,libxdamage)
                     ("libxext"                  ,libxext)
                     ("libxfixes"                ,libxfixes)
                     ("libxi"                    ,libxi)
                     ("libxkbfile"               ,libxkbfile)
                     ("libxrandr"                ,libxrandr)
                     ("libxrender"               ,libxrender)
                     ("libxscrnsaver"            ,libxscrnsaver)
                     ("libxtst"                  ,libxtst)
                     ("mesa"                     ,mesa)
                     ("nspr"                     ,nspr)
                     ("nss"                      ,nss)
                     ("pango"                    ,pango)
                     ("util-linux"               ,util-linux)
                     ("util-linux:lib"           ,util-linux "lib")
                     ("util-linux-with-udev:lib" ,util-linux+udev "lib")
                     ("fontconfig"               ,fontconfig)))
           (propagated-inputs `(("gcc:lib"          ,gcc "lib")
                                ("fontconfig"       ,fontconfig)
                                ("libsecret"        ,libsecret)
                                ("pulseaudio"       ,pulseaudio)))
           (native-inputs `(("bash"              ,bash)))
           (build-system deb:deb-build-system)
           (arguments `(#:input-lib-mapping '(("out" "share/teams")
                                              ("out" "share/teams/swiftshader")
                                              ("nss" "lib/nss"))))
           (synopsis "deb-teams")
           (description "deb-teams.")
           (home-page "https://teams")
           (license license:asl2.0)))


(define-public rpm-slack
  (package (name "rpm-slack")
           (version "4.18.0-0.1")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://downloads.slack-edge.com/releases/linux/4.18.0/prod/x64/slack-" version ".fc21.x86_64.rpm"))
                           (file-name (string-append "slack-" version ".fc21.x86_64.rpm"))
                           (sha256    (base32 "0i0d74aksk4gmi3q4i77gszfachifd78pnyp0y3g05339lzif03a"))))
           (inputs `(("bash"                     ,bash)
                     ("ffmpeg"                   ,ffmpeg)
                     ("alsa-lib"                 ,alsa-lib)
                     ("atk"                      ,atk)
                     ("at-spi2-atk"              ,at-spi2-atk)
                     ("cairo"                    ,cairo)
                     ("cups"                     ,cups)
                     ("cups-minimal"             ,cups-minimal)
                     ("dbus"                     ,dbus)
                     ("expat"                    ,expat)
                     ("gcc:lib"                  ,gcc "lib")
                     ("gcc-toolchain"            ,gcc-toolchain)
                     ("gdk-pixbuf"               ,gdk-pixbuf)
                     ;; ("gdk-pixbuf+svg"           ,gdk-pixbuf+svg)
                     ("gfortran:lib"             ,gfortran "lib")
                     ("glib"                     ,glib)
                     ("glibc"                    ,glibc)
                     ("gtk+"                     ,gtk+)
                     ("libgnome-keyring"         ,libgnome-keyring)
                     ("libsecret"                ,libsecret)
                     ("libx11"                   ,libx11)
                     ("libxcb"                   ,libxcb)
                     ("libxcomposite"            ,libxcomposite)
                     ("libxcursor"               ,libxcursor)
                     ("libxdamage"               ,libxdamage)
                     ("libxext"                  ,libxext)
                     ("libxfixes"                ,libxfixes)
                     ("libxi"                    ,libxi)
                     ("libxkbfile"               ,libxkbfile)
                     ("libxrandr"                ,libxrandr)
                     ("libxrender"               ,libxrender)
                     ("libxscrnsaver"            ,libxscrnsaver)
                     ("libxtst"                  ,libxtst)
                     ("mesa"                     ,mesa)
                     ("nspr"                     ,nspr)
                     ("nss"                      ,nss)
                     ("pango"                    ,pango)
                     ("util-linux"               ,util-linux)
                     ("util-linux:lib"           ,util-linux "lib")
                     ("util-linux-with-udev:lib" ,util-linux+udev "lib")
                     ("fontconfig"               ,fontconfig)))
           (native-inputs `(("bash"              ,bash)))
           (build-system rpm:rpm-build-system)
           (arguments `(#:input-lib-mapping '(("out" "lib")
                                              ("out" "lib/slack")
                                              ("nss" "lib/nss"))))
           (synopsis "rpm-slack")
           (description "rpm-slack.")
           (home-page "https://slack")
           (license license:asl2.0)))




