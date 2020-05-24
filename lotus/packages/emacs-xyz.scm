
(define-module (lotus packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix cvs-download)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages music)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages wordnet)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))


(define-public emacs-develock
  (package
    (name "emacs-develock")
    (version "0.47")
    (source (origin (method url-fetch)
                    (uri (string-append "http://deb.debian.org/debian/pool/main/d/develock-el/develock-el_" version ".orig.tar.gz"))
                    (sha256 (base32 "1zrkvg33cfwcqx7kd2x94pacmnxyinfx61rjlrmlbb8lpjwxvsn4"))))
    (build-system emacs-build-system)
    (arguments
     '(#:tests? #f)) ; no check target
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'unpack
       ;;     (lambda _
       ;;       (let ((gzipbin (string-append (assoc-ref %build-inputs "gzip")  "/bin/gzip")))
       ;;         (mkdir-p "/tmp/develock")
       ;;         (chdir "/tmp/develock")
       ;;         (system (string-append gzipbin " -dc " (assoc-ref %build-inputs "source") " > " "/tmp/develock/develock.el"))
       ;;         #t))))
    (home-page "http://www.jpl.org/elips/")
    (synopsis "Emacs minor mode for to make font-lock highlight leading and trailing whitespace")
    (description
     "Develock is a minor mode which provides the ability to make font-lock
highlight leading and trailing whitespace, long lines and oddities in the file
buffer for Lisp modes, ChangeLog mode, Texinfo mode, C modes, Java mode,
Jde-mode , CPerl mode, Perl mode, HTML modes, some Mail modes, Tcl mode and Ruby
mode. Here is an example of how to set up your startup file (possibly .emacs) to
use.")
    (license license:gpl3+)))


