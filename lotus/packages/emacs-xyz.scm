
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
  #:use-module (guix gexp)
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
  #:use-module (gnu packages emacs-xyz)
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
    (native-inputs (list gzip tar))
    (build-system emacs-build-system)
    (arguments
     '(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gzipbin (string-append (assoc-ref inputs "gzip")  "/bin/gzip"))
                   (tarbin  (string-append (assoc-ref inputs "tar")   "/bin/tar")))
               (mkdir-p "/tmp/develock")
               (chdir "/tmp/develock")
               (system (string-append tarbin " -xf " (assoc-ref inputs "source") " -C " "/tmp/develock/"))
               #t))))))
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

(define-public emacs-oneliner
  (package
    (name "emacs-oneliner")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/oneliner-elisp/unstable-tarball/"
                           version
                           "/oneliner-"
                           version ".tar.gz"))
       (sha256
        (base32 "0wiy2irwzbnvf7p2wh570dsawf19ds3wjg43zy5wzyxxyrxw4lkc"))))
    (inputs (list emacs-apel-lb))
    (propagated-inputs (list emacs-apel-lb))
    (build-system emacs-build-system)
    (home-page "http://oneliner-elisp.sourceforge.net")
    (synopsis "Emacs Oneliner")
    (description
     "Oneliner integrates UNIX shell with Emacs text editor. Oneliner provides
various syntax for integration. (e.g. You can connect 'shell pipe' and 'Emacs
buffer' directly with simple syntax.)")
    (license license:gpl2+)))

(define-public emacs-doxymacs
  (package
   (name "emacs-doxymacs")
   (version "1.8.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://sourceforge/doxymacs/doxymacs/"
                         version
                         "/doxymacs-"
                         version ".tar.gz"))
     (sha256
      (base32 "1yjs9764jqmzjvwica9pskwqgsa115hrf9f6hx9yw89wphrxhgx2"))))
   (inputs (list libxml2
                 emacs))
   ;; (propagated-inputs (list emacs-apel-lb))
   (build-system gnu-build-system)
   (arguments
     `(#:tests? #f                            ; Run the test suite (this is the default)
       #:configure-flags (list "--with-lispdir"
                               (string-append "--with-lispdir="
                                              %output
                                              "/share/emacs/site-lisp/"
                                              ,name "-" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-purple-dir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "c/doxymacs_parser.c"
               (("inline ")
                ""))
             #t))
         (delete 'make-dynamic-linker-cache))))
   (home-page "http://doxymacs.sourceforge.net")
   (synopsis "Emacs Doxymacs")
   (description
    "doxymacs aims to make creating/using Doxygen-created documentation easier for
the {X}Emacs user.")
   (license license:gpl2+)))

(define-public git-wip
  (package
    (name "git-wip")
    (version "master")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/sharad/git-wip.git")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256 (base32 "1223y8pypgh0r7jzcssdb6syjah14l47qai6bznbbjl9qadp0dgx"))))
    (build-system emacs-build-system)
    (inputs  (list git))
    (arguments
     (list #:tests? #true
           #:test-command #~(list "./test.sh")
           #:phases
           #~(modify-phases %standard-phases
                            (add-after 'unpack 'move-source-files
                              (lambda* (#:key inputs outputs #:allow-other-keys)
                                ;; (substitute* "emacs/git-wip.el"
                                ;;              (("\\(define " all)
                                ;;               (string-append ";;;###autoload\n" all)))
                                (with-output-to-file "test.sh"
                                  (lambda _
                                    (format #t "#!/usr/bin/env bash~%")
                                    (format #t "export GIT_CONFIG_GLOBAL=~a/gitconfig~%" (getcwd))
                                    (format #t "git config --global user.name \"FIRST_NAME LAST_NAME\"~%")
                                    (format #t "git config --global user.email \"FIRST_NAME@example.com\"~%")
                                    (format #t "./test-git-wip.sh~%")))
                                (chmod "test.sh" #o755)
                                (let ((el-files (find-files "./emacs" ".*\\.el$")))
                                  (for-each (lambda (f)
                                              (rename-file f (basename f)))
                                            el-files))))
                            (add-after 'install 'install-bin
                              (lambda _
                                (install-file "git-wip"
                                              (string-append #$output
                                                             "/bin"))
                                (install-file "test-git-wip.sh"
                                              (string-append #$output
                                                             "/bin")))))))
    (synopsis "help track git Work In Progress branches.")
    (description "git-wip is a script that will manage Work In Progress (or WIP) branches. WIP
branches are mostly throw away but identify points of development between
commits. The intent is to tie this script into your editor so that each time you
save your file, the git-wip script captures that state in git. git-wip also
helps you return back to a previous state of development.")
    (home-page "https://github.com/bartman/git-wip.git")
    (license license:gpl3)))


