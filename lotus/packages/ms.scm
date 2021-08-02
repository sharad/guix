(define-module (lotus packages ms)
  #:use-module (ice-9 ftw)
  #:use-module (lotus build patchelf-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build rpath)
  #:use-module (guix build-system trivial)
  #:use-module (lotus build-system rpm)
  #:use-module (lotus build rpm-build-system)
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
  #:use-module (gnu packages gcc))


(define-public rpm-teams
  (package (name "rpm-teams")
           (version "1.3.00.5153-1")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://packages.microsoft.com/yumrepos/ms-teams/teams-" version ".x86_64.rpm"))
                           (file-name (string-append "teams-" version ".x86_64.rpm"))
                           (sha256    (base32 "009rh55r56k1zd7spiz6hy5chgkq9bg4d0zim9rwjyg28754r4zy"))))
           (inputs `(("nss"            ,nss)
                     ("glib"           ,glib)
                     ("libx11"         ,libx11)
                     ("libxcb"         ,libxcb)
                     ("libxcomposite"  ,libxcomposite)
                     ("libxcursor"     ,libxcursor)
                     ("libxdamage"     ,libxdamage)
                     ("libxext"        ,libxext)
                     ("libxfixes"      ,libxfixes)
                     ("libxi"          ,libxi)
                     ("libxrender"     ,libxrender)
                     ("libxtst"        ,libxtst)
                     ("expat"          ,expat)
                     ("util-linux"     ,util-linux)
                     ("libxrandr"      ,libxrandr)
                     ("libxscrnsaver"  ,libxscrnsaver)
                     ("alsa-lib"       ,alsa-lib)
                     ("dbus"           ,dbus)
                     ("at-spi2-atk"    ,at-spi2-atk)
                     ("cups-minimal"   ,cups-minimal)
                     ("gcc:lib"        ,gcc "lib")
                     ("nspr"           ,nspr)
                     ("atk"            ,atk)
                     ("gtk+"           ,gtk+)
                     ("pango"          ,pango)
                     ("cairo"          ,cairo)
                     ("gdk-pixbuf+svg" ,gdk-pixbuf+svg)))
           (build-system rpm-build-system)
           (arguments `(#:input-lib-mapping '(("out" "share/teams")
                                              ("nss" "lib/nss"))))
                        ;; #:phases      (modify-phases %standard-phases
                        ;;                 (add-after
                        ;;                     'build 'rearrange
                        ;;                   ,patched-firefox-rearrange-method)
                        ;;                 (delete 'validate-runpath))
           (synopsis "rpm-teams")
           (description "rpm-teams.")
           (home-page "https://teams")
           (license license:asl2.0)))



rpm-teams

