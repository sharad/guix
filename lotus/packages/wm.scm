
(define-module (lotus packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
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
            (sha256 (base32 "0p0lcyz504rw6kv980n1rbfzj4lff11m3j3dp0g3mafr8m061pmm"))))
   ;; (native-inputs
   ;;  `(("autoconf" ,autoconf)
   ;;    ("automake" ,automake)
   ;;    ("libtool"  ,libtool)
   ;;    ("pkg-config" ,pkg-config)))
   (inputs
    `(("stumpwm"      ,stumpwm)))
   (build-system gnu-build-system)
   (arguments
    '(;; #:modules (((guix build-system gnu) #:prefix gnu:))
      #:tests? #f
      #:make-flags (let ((out  (assoc-ref %outputs "out")))
                     (list (string-append "PREFIX=" out) "install"))
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure))))
   (synopsis "Allows you to use stumpwm with GNOME 3 Session infrastructure on Arch Linux.")
   (description "Allows you to use stumpwm with GNOME 3 Session infrastructure on Arch Linux.")
   (home-page "https://github.com/stumpwm/stumpwm-gnome")
   (license license:ibmpl1.0)))


stumpwm-gnome
