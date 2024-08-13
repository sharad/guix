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
  #:use-module ((guix build-system cmake) #:prefix cmake:)
  #:use-module ((lotus build-system deb) #:prefix deb:)
  #:use-module ((lotus build-system patchelf) #:prefix patchelf:)
  #:use-module ((guix  build-system copy) #:prefix copy:)
  #:use-module ((guix build-system trivial) #:prefix trivial:)
  #:use-module ((guix build-system cmake))
  #:use-module ((guix build-system meson))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
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
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages c)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages gettext)
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
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages cmake))

;; https://issues.guix.gnu.org/issue/35619

(define-public pi-hole
  "https://github.com/pi-hole/pi-hole/blob/master/automated%20install/basic-install.sh")

(define-public libwm
  (package
    (name "libwm")
    (version "1.0")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://github.com/wmutils/libwm/archive/v" version ".tar.gz"))
             (sha256 (base32 "180aadwhzjgy0p31byfnjq4ps1yhb339gfb7hh68qkrabq698v7m"))))
    (build-system gnu:gnu-build-system)
    (inputs
     `(("libxcb" ,libxcb)))
    (arguments
     '(;; #:modules (((guix build-system gnu) #:prefix gnu:))
       #:tests? #f
       #:make-flags (let ((out  (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))

    (synopsis "A small library for X window manipulation")
    (description "A small library for X window manipulation")
    (home-page "https://github.com/wmutils/libwm")
    (license license:gpl3)))


(define-public wmutils-opt
  (package
    (name "wmutils-opt")
    (version "1.5")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://github.com/wmutils/opt/archive/v" version ".tar.gz"))
             (sha256 (base32 "0wk39aq2lrnc0wjs8pv3cigw3lwy2qzaw0v61bwknd5wabm25bvj"))))
    (build-system gnu:gnu-build-system)
    (inputs
     `(("xcb-util-cursor" ,xcb-util-cursor)
       ("xcb-util"        ,xcb-util)))
    (arguments
     '(;; #:modules (((guix build-system gnu) #:prefix gnu:))
       #:tests? #f
       #:make-flags (let ((out  (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))

    (synopsis "wmutils opt is a set of optional utilities meant to accompany wmutils core.")
    (description "wmutils opt is a set of optional utilities meant to accompany wmutils core.

Just as the core programs, each utility does one job and does it well, like dealing with window events or names.")
    (home-page "https://github.com/wmutils/opt")
    (license license:gpl3)))

(define-public wmutils-core
  (package
    (name "wmutils-core")
    (version "1.5")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://github.com/wmutils/core/archive/v" version ".tar.gz"))
             (sha256 (base32 "0wk39aq2lrnc0wjs8pv3cigw3lwy2qzaw0v61bwknd5wabm25bvj"))))
    (build-system gnu:gnu-build-system)
    (inputs
     `(("xcb-util" ,xcb-util)
       ("xcb-util-cursor" ,xcb-util-cursor)))
    (arguments
     '(;; #:modules (((guix build-system gnu) #:prefix gnu:))
       #:tests? #f
       #:make-flags (let ((out  (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))

    (synopsis "wmutils' core is a set of tools for X windows manipulation.")
    (description "wmutils' core is a set of tools for X windows manipulation.
Each tool only has one purpose, to make it as flexible and reliable as
possible.")
    (home-page "https://github.com/wmutils/core")
    (license license:gpl3)))

(define-public zssh
  (package
    (name "zssh")
    (version "1.5c")
    (source (origin (method url-fetch)
                    (uri (string-append "mirror://sourceforge/zssh/zssh/1.5/zssh-" version ".tgz"))
                    (sha256 (base32 "06z73iq59lz8ibjrgs7d3xl39vh9yld1988yx8khssch4pw41s52"))))
    (build-system gnu:gnu-build-system)
    (inputs
     `(("readline" ,readline)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (arguments
     '(#:tests? #f
       #:make-flags (let ((out  (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _ (zero? (system* "autoreconf" "-vif"))))
         (add-after 'build 'mkdirs
           (lambda _
             (let ((out  (assoc-ref %outputs "out")))
               (zero? (system* "mkdir" "-p" (string-append out "/bin")))
               (zero? (system* "mkdir" "-p" (string-append out "/share/man/man1")))
               (system* "ls" "-rl" out)
               #t))))))
         ;; (delete 'configure)

    (synopsis "zssh (Zmodem SSH) is a program for interactively transferring
files to/from a remote machine while using the secure shell (ssh).")
    (description "zssh (Zmodem SSH) is a program for interactively transferring
files to/from a remote machine while using the secure shell (ssh). It is
intended to be a convenient alternative to scp, avoiding the need to
re-authenticate each time a file is transferred.")
    (home-page "http://zssh.sourceforge.net/")
    (license #f)))

(define-public ecryptfs-simple
  (package
    (name "ecryptfs-simple")
    (version "2016.11.16.1")
    ;; (version "2017")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mhogomchungu/ecryptfs-simple/archive/refs/tags/"
                           version ".tar.gz"))
       ;; (uri (string-append "https://xyne.dev/projects/ecryptfs-simple/src/ecryptfs-simple-"
       ;;                     version ".tar.gz"))
       (sha256
        (base32 "1bz88c6rgzfrzi49hrl5rf4flq674rxn6yc60c4c8sy9dvnhvx4v"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list))
    (inputs
     (list libgcrypt
           keyutils
           ecryptfs-utils
           `(,util-linux "lib")))
    (home-page "https://github.com/mhogomchungu/ecryptfs-simple")
    (synopsis "A very simple utility for working with eCryptfs. old https://xyne.dev/projects/ecryptfs-simple/")
    (description "ecryptfs-simple is a utility for users that want to work with eCryptfs in the
simplest way possible. It allows users to mount and unmount eCryptfs directories
if they have full permissions on the source and target directories.
ecryptfs-simple requires neither superuser privileges nor entries in fstab.
Unlike the utilities in the ecryptfs-utils package, there are no hard-coded
paths (e.g. ~/.Private).

The idea is to make eCryptfs as easy to use as EncFS.

See the ecryptfs-simple help message below for more information.")
    ;; The files src/key_mod/ecryptfs_key_mod_{openssl,pkcs11_helper,tspi}.c
    ;; grant additional permission to link with OpenSSL.
    (license license:gpl2+)))

(define-public pkcs11-provider
  (package
   (name "pkcs11-provider")
   (version "v0.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/latchset/pkcs11-provider/archive/refs/tags/" version ".tar.gz"))
            (sha256 (base32 "12csss35gm4ahcsrjs3z1qcanb4n9rqhgzzgkydh9fs67rjs2f6f"))))
   (build-system meson-build-system)
   (inputs  (list cmake
                  pkg-config
                  openssl))
   (arguments
    (list #:configure-flags
          #~(list (string-append "-Dlibdir=" #$output "/lib"))
          #:phases
          #~(modify-phases %standard-phases
                           (add-before 'configure 'replace-purple-dir
                                       (lambda* (#:key inputs outputs #:allow-other-keys)
                                         (substitute* "meson.build"
                                                      (("libcrypto.get_variable\\(pkgconfig: 'modulesdir'\\)")
                                                       (string-append "'" (assoc-ref outputs "out") "/lib" "'")))
                                         #t)))))

   (synopsis "A pkcs#11 provider for OpenSSL 3.0+")
   (description "pkcs11-provider
This is an OpenSSL 3.x provider to access Hardware and Software Tokens using the
PKCS#11 Cryptographic Token Interface. Access to tokens depends on loading an
appropriate PKCS#11 driver that knows how to talk to the specific token. The
PKCS#11 provider is a connector that allows OpenSSL to make proper use of such
drivers. This code targets PKCS#11 version 3.1 but is backwards compatible to
version 3.0 and 2.40 as well.")
   (home-page "https://github.com/latchset/pkcs11-provider/")
   (license license:gpl3)))

(define-public python-pkcs11-provider
  (package
   (name "python-pkcs11-provider")
   (version "master")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/sharad/python-pkcs11-provider")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0a8wz0ccgw7djs3b77vcvhm8gz6jc2iq6vpg76ipnxm08pwl0qb0"))))
   (build-system gnu:gnu-build-system)
   (inputs  (list python
                  python-cython
                  pkg-config
                  openssl))
   (arguments
    (list #:make-flags
          #~(list (string-append "CC=gcc"
                                 "-Dlibdir=" #$output "/lib"))))

   (synopsis "Write your own PKCS#11 module in Python! ")
   (description "python-pkcs11-provider
Write your own PKCS#11 module in Python! ")
   (home-page "https://github.com/danni/python-pkcs11-provider.git")
   (license license:gpl3)))


;; https://github.com/Pkcs11Interop/pkcs11-mock
;; https://github.com/kinnalru/soft-pkcs11
;; https://github.com/Pkcs11Interop/empty-pkcs11

(define-public git-extras
  (package
    (name "git-extras")
    (version "7.2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/tj/git-extras/archive/refs/tags/" version ".tar.gz"))
             (sha256 (base32 "1wa7qf01df2l7a8578500is9hm0bdrn57l4qrc4yj1rlksdz2w7m"))))
    (build-system gnu:gnu-build-system)
    (inputs  (list util-linux))
    (arguments
     '(;; #:modules (((guix build-system gnu) #:prefix gnu:))
       #:tests? #f
       #:make-flags (let ((out  (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))

    (synopsis "GIT utilities -- repo summary, repl, changelog population, author commit percentages and more")
    (description "git abort, alias, archive-file, authors, browse, browse-ci,
git bulk, brv, changelog, clear, clear-soft, coauthor, commits-since, contrib,
count, cp, create-branch, delete-branch, delete-merged-branches,
delete-squashed-branches, delete-submodule, delete-tag, delta, effort, extras,
feature, force-clone, fork, fresh-branch, get, gh-pages, graft, guilt, ignore,
ignore-io, info, local-commits, lock, locked, magic, merge-into, merge-repo,
missing, mr, obliterate, paste, pr, psykorebase, pull-request, reauthor,
rebase-patch, release, rename-branch, rename-file, rename-tag, rename-remote,
repl, reset-file, root, rscp, scp, sed, setup, show-merged-branches, show-tree,
show-unmerged-branches, stamp, squash, standup, summary, sync, touch, undo,
unlock, utimes")
    (home-page "https://github.com/tj/git-extras")
    (license license:gpl3)))

(define-public wrap-cc
  (lambda* (cc #:optional
               (bin (package-name cc))
               (name (string-append (package-name cc) "-wrapper")))
    ;; https://issues.guix.gnu.org/41428
    (package/inherit cc
      (name name)
      (source #f)
      (build-system trivial:trivial-build-system)
      (outputs '("out"))
      (native-inputs '())
      (inputs '())
      (propagated-inputs `(("cc" ,cc)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((bin-dir     (string-append (assoc-ref %build-inputs "cc") "/bin/"))
                 (wrapper-dir (string-append (assoc-ref %outputs "out")     "/bin/")))
             (mkdir-p wrapper-dir)
             (symlink (string-append bin-dir ,bin)
                      (string-append wrapper-dir "cc"))))))
      (synopsis (string-append "Wrapper for " bin))
      (description
       (string-append "Wraps " (package-name cc) " such that @command{" bin "}
 can be invoked under the name @command{cc}.")))))

(define-public gcc-toolchain-wrapper
  (wrap-cc gcc-toolchain "gcc"))

;; pkcs11-provider

