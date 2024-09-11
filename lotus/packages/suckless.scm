
(define-module (lotus packages suckless)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages suckless)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages))



(define-public surf-kiosk
  (package
    (name "surf-kiosk")
    (version "fb-copy-attempt")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/sharad/surf-kiosk")
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1xr56hskdyy5vs66vfn8g7pyx8fpxmd0adfyff2i1w57k8nhpxmd"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; Use the right file name for dmenu and xprop.
         (add-before 'build 'set-dmenu-and-xprop-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "config.def.h"
               (("dmenu") (search-input-file inputs "/bin/dmenu"))
               (("xprop") (search-input-file inputs "/bin/xprop")))
             #t)))))
    (inputs
     `(("dmenu" ,dmenu)
       ("gcr" ,gcr-3)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk-with-libsoup2)
       ("xprop" ,xprop)))
    (native-inputs
     (list pkg-config))
    (home-page "https://surf.suckless.org/")
    (synopsis "Simple web browser")
    (description
     "Surf is a simple web browser based on WebKit/GTK+.  It is able to
display websites and follow links.  It supports the XEmbed protocol which
makes it possible to embed it in another application.  Furthermore, one can
point surf to another URI by setting its XProperties.")
    (license (list license:expat
                   license:x11))))

surf-kiosk


