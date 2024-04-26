
(define-module (lotus packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages wm))




(define-public stumpwm-gnome
  (package
   (name "stumpwm-gnome")
   (version "master")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/stumpwm/stumpwm-gnome.git")
                  (commit version)))
            (sha256 (base32 "0fjmjq04d4bllmayy18hq3vr70zidlm3m8b8xvf80z7k2qi2gj6j"))))
   ;; (native-inputs
   ;;  `(("autoconf" ,autoconf)
   ;;    ("automake" ,automake)
   ;;    ("libtool"  ,libtool)
   ;;    ("pkg-config" ,pkg-config)))
   (inputs
    `(("stumpwm"       ,stumpwm)
      ("gnome-session" ,gnome-session)))
   (build-system gnu-build-system)
   (arguments
    '(#:tests?     #f
      #:make-flags (let ((out  (assoc-ref %outputs "out")))
                     (list (string-append "PREFIX=" out) "install"))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'replace-path-gnome-session
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (invoke "cat" "session/stumpwm-gnome")
            (substitute* "session/stumpwm-gnome-xsession.desktop"
              (("Exec=gnome-session")
               (string-append "Exec=" (assoc-ref inputs "gnome-session") "/bin/gnome-session"))
              (("TryExec=gnome-session")
               (string-append "TryExec=" (assoc-ref inputs "gnome-session") "/bin/gnome-session")))
            (substitute* "session/stumpwm-gnome"
              (("^stumpwm")
               (string-append (assoc-ref inputs "stumpwm") "/bin/stumpwm")))
            (invoke "cat" "session/stumpwm-gnome")
            #t)))))
   (synopsis "Allows you to use stumpwm with GNOME 3 Session infrastructure on Arch Linux.")
   (description "Allows you to use stumpwm with GNOME 3 Session infrastructure on Arch Linux.")
   (home-page "https://github.com/stumpwm/stumpwm-gnome")
   (license license:ibmpl1.0)))


stumpwm-gnome
