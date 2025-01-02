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
  #:use-module ((guix build-system copy))
  #:use-module ((guix build-system meson))
  #:use-module ((guix build-system go))
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
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (guix build-system vim)
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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages boost)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages ruby))

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

(define-public pkcs11-proxy
  (package
    (name "pkcs11-proxy")
    (version "master")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/SUNET/pkcs11-proxy.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32 "04y6haadg4nqm2r8w1pd8bkkj4m62iqi35nkwbkkiablsias66k3"))))
    (build-system cmake-build-system)
    (inputs (list openssl
                  libseccomp))
    (arguments
     (list #:tests? #f))
    (synopsis "PKCS11-Proxy is a network proxy for a PKCS11 Library (mirror of original repository)")
    (description "PKCS11-Proxy is a proxy for the PKCS11-library.

This project is based on a stripped down Gnome Keyring without all gnome
dependencies and other features.

The proxy tunnels PKCS11-requests over the network.  One possible use
is to store cryptograhic information on a seperate server.  This way
the crypto it can be isolated from the rest of the system.  Beware:
the connection is not encrypted and can easily be sniffed.  You should
use a secure communication-channel, for example stunnel.

Here is an example of using pkcs11-proxy together with SoftHSM (from the
OpenDNSSEC project).  The benefit of this setup is that no extra hardware
is needed at all.  This could also be considered the greatest weakeness.
For demonstration purposes, however, security is not a consideration.

$ sudo adduser cgielen pkcs11
$ sudo adduser cgielen softhsm

$ softhsm --init-token --slot 0 --label test
The SO PIN must have a length between 4 and 255 characters.
Enter SO PIN:
The user PIN must have a length between 4 and 255 characters.
Enter user PIN:
The token has been initialized.

$ PKCS11_DAEMON_SOCKET=\"tcp://127.0.0.1:2345\" pkcs11-daemon /usr/lib/libsofthsm.so
$ PKCS11_PROXY_SOCKET=\"tcp://127.0.0.1:2345\" pkcs11-tool --module=/usr/lib/libpkcs11-proxy.so -L Available
slots: Slot 0           SoftHSM
  token label:   test token manuf:   SoftHSM token model:   SoftHSM
  token flags:   rng, login required, PIN initialized, token initialized,
  other flags=0x40 serial num  :  1")
   (home-page "https://github.com/SUNET/pkcs11-proxy")
   (license license:gpl3)))

(define-public soft-pkcs11
  (package
    (name "soft-pkcs11")
    (version "master")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sharad/soft-pkcs11.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32 "138adr08fd7c2015fshl8frpp66j4lgvdrdkb2qyv96vz9811s0a"))))
    (build-system cmake-build-system)
    (inputs (list openssl
                  boost
                  ruby))
    (arguments
     (list #:tests? #f))
    (synopsis "software pkcs11 implementation")
    (description "soft-pkcs11

soft-pkcs11 is a software only pkcs11 implementation. The main idea is to be
able to turn your phone into secure keychain. But you can use this module as you
wish.

It is inspired by soft-pkcs11(http://people.su.se/~lha/soft-pkcs11/README)
implementation by Love Hörnquist Åstrand(http://people.su.se/~lha/) and includes
it as an example.")
    (home-page "https://github.com/kinnalru/soft-pkcs11")
    (license license:gpl3)))

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
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sharad/python-pkcs11-provider")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32 "1lxshjj9p65wllh3l4rldrd2cxl489bgrmwdmgii4ypy66lvhi5k"))))
    (build-system gnu:gnu-build-system)
    (inputs (list python-3
                  python-cython
                  pkg-config
                  openssl))
    (propagated-inputs (list python-3))
    (arguments
     (list #:tests? #f
           #:make-flags #~(list "CC=gcc" (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                                     (delete 'configure))))
   (synopsis "Write your own PKCS#11 module in Python!")
   (description "python-pkcs11-provider
Write your own PKCS#11 module in Python!")
   (home-page "https://github.com/danni/python-pkcs11-provider")
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
      (version "14.2.0")
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

(define-public xrandr-invert-colors
  (package
   (name "xrandr-invert-colors")
   (version "master")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/zoltanp/xrandr-invert-colors.git")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256 (base32 "1695wrwr5kxarwcw9sn66y1jl48vjlymvph9m065l7apa71pv80d"))))
   (build-system gnu:gnu-build-system)
   (inputs (list libxcb
                 libxau
                 libxdmcp))
   (arguments
    (list #:tests? #f
          #:make-flags #~(list "CC=gcc" (string-append "PREFIX=" #$output))
          #:phases #~(modify-phases %standard-phases
                                    (delete 'configure))))
   (synopsis "Small utility for inverting the colors on all monitors attached to an XRandR-capable X11 display server.")
   (description "Achknowledgements

Redshift developers, for creating the XRandR gamma setting code. Their code is
reused in this application. http://jonls.dk/redshift/
https://launchpad.net/redshift Alternatives

    xcalib -i -a : Inverts the colors of the current screen.
        Note that as of date of last testing (2014, xcalib version 0.8) xcalib
has not been able to invert the colors on all monitors in a multi-monitor setup,
because all monitors are presented by the X11 server as a single screen. Thus
this limitation is coming from the X11 server. xrandr-invert-colors does not
have this limitation, because it uses XRandR API, which correctly handles all
attached monitors.")
   (home-page "https://github.com/zoltanp/xrandr-invert-colors")
   (license license:gpl3)))

(define-public git-gwatch
  (package
    (name "git-gwatch")
    (version "v0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jw0k/gwatch/archive/refs/tags/" version ".tar.gz"))
              (sha256 (base32 "11bb6y08cr9qdwqap6iwpchcjg149w5z4x959gga7fqwiwwxd3dl"))))
    (build-system cmake-build-system)
    (inputs (list libgit2 libuv))
    (arguments
     (list #:tests? #f
           #:build-type "release"
           #:configure-flags #~(list "-DCMAKE_BUILD_TYPE=Release")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'replace-purple-dir
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (delete-file-recursively "lib")
                   (substitute* "fs_oper_linux.c"
                                (("include \"lib/libuv/include/uv.h\"")
                                 "include \"uv.h\""))
                   (substitute* "CMakeLists.txt"
                                (("gwatch")
                                 "git-gwatch")
                                (("add_subdirectory\\(lib/libgit2\\)")
                                 "")
                                (("add_subdirectory\\(lib/libuv\\)")
                                 "")
                                (("    uv_a")
                                 "    uv"))
                   (invoke "sed" "-i" "93,99d" "CMakeLists.txt")
                   (invoke "sed" "-i"
                           "-e" "$ainstall(TARGETS git-gwatch"
                           "-e" "$a    DESTINATION ${CMAKE_INSTALL_PREFIX}/bin"
                           "-e" "$a    )\\n"
                           "CMakeLists.txt")
                   #t)))))
    (synopsis "Watches a folder for file modifications and commits them to a git repository automatically.")
    (description "A program that watches a folder for file modifications and commits them to a git
repository automatically How gwatch works

After gwatch is started it will watch a given folder and all of its
subfolders (recursively) for changes. If a change occurs, a timer will be
started (30s by default). After the timer expires, gwatch will create a new git
commit with all the modifications. The timer is to prevent creating too many
commits when there are a lot of modifications. In order for gwatch to
successfully create commits, a git repository must be initialized in the watched
folder.

Gwatch works on Linux and on Windows.")
    (home-page "https://github.com/jw0k/gwatch")
    (license license:gpl3)))

(define-public git-watch
  (package
    (name "git-watch")
    (version "v0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gitwatch/gitwatch/archive/refs/tags/" version ".tar.gz"))
              (sha256 (base32 "09cvcjv43da7f9l1c9c8qa6h7hw8v2gns18gnl987qd05wnpdz9q"))))
    (build-system gnu:gnu-build-system)
    (inputs (list tar))
    (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (delete 'build)
                 (replace 'install
                   (lambda _
                     (let ((bin (string-append #$output "/bin")))
                       (copy-file "gitwatch.sh" "git-watch")
                       (install-file "git-watch" (string-append bin))))))))

    (synopsis "Watch a file or folder and automatically commit changes to a git repo easily.")
    (description "A bash script to watch a file or folder and commit changes to a git repo
What to use it for?

That's really up to you, but here are some examples:

   * config files: some programs auto-write their config files, without waiting
for you to click an 'Apply' button; or even if there is such a button, most
programs offer you no way of going back to an earlier version of your settings.
If you commit your config file(s) to a git repo, you can track changes and go
back to older versions. This script makes it convenient, to have all changes
recorded automatically.

   * document files: if you use an editor that does not have built-in git
support (or maybe if you don't like the git support it has), you can use
gitwatch to automatically commit your files when you save them, or combine it
with the editor's auto-save feature to fully automatically and regularly track
your changes

   * more stuff! If you have any other uses, or can think of ones, please let us
know, and we can add them to this list!")
    (home-page "https://github.com/gitwatch/gitwatch")
    (license license:gpl3)))

(define-public pwnat
  (package
    (name "pwnat")
    (version "master")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/samyk/pwnat.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32 "02ci0wmjcs3nck16wddjkm9689kmrf3kbg4mhkkmz3i0785p5vkn"))))
    (build-system gnu:gnu-build-system)
    (inputs (list))
    (propagated-inputs (list))
    (arguments
     (list #:tests? #f
           #:make-flags #~(list (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                         (delete 'configure)
                         (add-after 'unpack 'amend-makefile
                                    (lambda* (#:key inputs outputs #:allow-other-keys)
                                      (invoke "sed" "-i"
                                              "-e" "$ainstall:\\n\\tinstall -D pwnat \\$\\(PREFIX\\)/bin/pwnat\\n\\tinstall -D manpage.txt \\$\\(PREFIX\\)/share/man/man1/pwnat.1"
                                              "Makefile"))))))
    (synopsis "The only tool/technique to punch holes through firewalls/NATs where multiple clients & server can be behind separate NATs without any 3rd party involvement.")
    (description "The only tool/technique to punch holes through firewalls/NATs where multiple
clients & server can be behind separate NATs without any 3rd party involvement.
Pwnat is a newly developed technique, exploiting a property of NAT translation
tables, with no 3rd party, port forwarding, DMZ, DNS, router admin requirements,
STUN/TURN/UPnP/ICE, or spoofing.

by Samy Kamkar, is a tool that allows any client behind a NAT to communicate
with a server behind a separate NAT with no port forwarding and no DMZ setup on
any routers in order to directly communicate with each other.

There is no middle man, no proxy, no third party, no UPnP required, no spoofing,
no DNS tricks. The server does not need to know the client's IP address before
connecting.

More importantly, the client can then connect to any host or port on any remote
host or to a fixed host and port decided by the server.

Simply put, this is a proxy server that works behind a NAT, even when the client
is also behind a NAT.")
   (home-page "http://samy.pl/pwnat/")
   (license license:gpl3)))


;; (define-public fuse-ramfs
;;   (package
;;    (name "fuse-ramfs")
;;    (version "master")
;;    (source (origin
;;              (method git-fetch)
;;              (uri (git-reference
;;                    (url "https://github.com/pfisterer/fuse-ramfs.git")
;;                    (commit version)))
;;              (file-name (git-file-name name version))
;;              (sha256 (base32 "19khq4f9p9pgi38wcps81a3sywqisisp1gkfqbxsaz0blxpain6q"))))
;;    (build-system gnu:gnu-build-system)
;;    (inputs (list fuse))
;;    (propagated-inputs (list))
;;    (arguments
;;     (list #:tests? #f
;;           #:make-flags #~(list (string-append "PREFIX=" #$output))
;;           #:phases #~(modify-phases %standard-phases
;;                         (delete 'configure)
;;                         ;; (add-after 'unpack 'amend-makefile
;;                         ;;            (lambda* (#:key inputs outputs #:allow-other-keys)
;;                         ;;              (substitute* "./fuse-test.cpp"
;;                         ;;                           (("include <fuse.h>")
;;                         ;;                            "include <fuse3/fuse.h>"))
;;                         ;;              (invoke "sed" "-i"
;;                         ;;                      "-e" "$ainstall:\\n\\tinstall -D fuse-test \\$\\(PREFIX\\)/bin/fuse-test\\n"
;;                         ;;                      "Makefile")))
;;                         (add-after 'unpack 'amend-makefile
;;                                    (lambda* (#:key inputs outputs #:allow-other-keys)
;;                                      (invoke "sed" "-i" "-e" "s@fuse.h@fuse3/fuse.h@" "fuse-test.cpp")
;;                                      (invoke "sed" "-i"
;;                                              "-e" "$ainstall:\\n\\tinstall -D fuse-test \\$\\(PREFIX\\)/bin/fuse-test\\n"
;;                                              "Makefile")
;;                                      #t)))))
;;    (synopsis "A simple example for an in-memory, flat FUSE-based file system.")
;;    (description "A simple example for an in-memory, flat FUSE-based file system.")
;;    (home-page "https://github.com/pfisterer/fuse-ramfs.git")
;;    (license license:bsd-4)))


;; fuse-ramfs


(define-public gita
  (let ((commit "2108ddb34c701c15a2610bd295a0a1ab24cb024f")
        (revision "2"))
    (package
      (name "gita")
      (version (git-version "0.16.7" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nosarthur/gita")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "118dzmjgml0c32yllr2178ash2hvgn201i463bv4y0qbywajm9ax"))))
      (build-system python-build-system)
      (native-inputs
       (list git ;for tests
             python-pytest
             python-argcomplete))
      (propagated-inputs
       (list python-pyyaml))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "tests/test_main.py"
                            (("\"gita\\\\n\"") "\"source\\n\"")
                            (("\"gita\"") "\"source\"")
                            (("\"group add gita -n test\"") "\"group add source -n test\""))
               (invoke (search-input-file inputs "/bin/git")
                       "init")
               (add-installed-pythonpath inputs outputs)
               (invoke (search-input-file inputs "/bin/pytest")
                       "-vv" "tests")))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bash-completion (string-append out "/etc/bash_completion.d"))
                      (zsh-completion  (string-append out "/etc/zsh/site-functions"))
                      (profile-dir     (string-append out "/etc/profile.d"))
                      (fish-completion (string-append out "/etc/fish/completions")))
                 (mkdir-p bash-completion)
                 (copy-file "auto-completion/bash/.gita-completion.bash"
                            (string-append bash-completion "/gita"))
                 (mkdir-p profile-dir)
                 (copy-file "auto-completion/zsh/.gita-completion.zsh"
                            (string-append profile-dir "/gita.zsh"))
                 (mkdir-p zsh-completion)
                 (copy-file "auto-completion/zsh/_gita"
                            (string-append zsh-completion "/_gita"))
                 (mkdir-p fish-completion)
                 (copy-file "auto-completion/fish/gita.fish"
                            (string-append fish-completion "/gita"))))))))
      (home-page "https://github.com/nosarthur/gita")
      (synopsis "Command-line tool to manage multiple Git repos")
      (description "This package provides a command-line tool to manage
multiple Git repos.

This tool does two things:
@itemize
@item display the status of multiple Git repos such as branch, modification,
commit message side by side
@item (batch) delegate Git commands/aliases from any working directory
@end itemize

If several repos are related, it helps to see their status together.")
      (license license:expat))))

(define-public go-github-com-bbengfort-memfs
  (package
   (name "go-github-com-bbengfort-memfs")
   (version "master")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/bbengfort/memfs.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0as8bply6r9smmkabf1xb2d65wxjm3hckbw5l5kxy65qybgzbcza"))))
   (build-system go-build-system)
   ;; (arguments
   ;;  (list
   ;;   #:import-path "github.com/bbbengfort/memfs"))
   (inputs
    (list perl
          python))
   ;; (propagated-inputs
   ;;  (list go-github-com-ipfs-go-datastore go-github-com-syndtr-goleveldb))
   (home-page "https://github.com/bbengfort/memfs")
   (synopsis "Implementation of go-github-com-bbengfort-memfs")
   (description
    "This package implements the
@url{https://github.com/bbengfort/memfs} In memory file system that implements
as many FUSE interfaces as possible.")
   (license license:expat)))

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
              (sha256 (base32 "1aiym99ly55kbpbksd0kdwyd89xn6n16yvl6a4szhac1nkm36i6y"))))
    (build-system copy-build-system)
    (inputs  (list git))
    (arguments
     (list #:tests? #true
           #:install-plan #~`(("git-wip" "bin/git-wip"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'move-source-files
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (with-output-to-file "test.sh"
                     (lambda _
                       (format #t "#!/usr/bin/env bash~%")
                       (format #t "export GIT_CONFIG_GLOBAL=~a/gitconfig~%" (getcwd))
                       (format #t "git config --global user.name \"FIRST_NAME LAST_NAME\"~%")
                       (format #t "git config --global user.email \"FIRST_NAME@example.com\"~%")
                       (format #t "./test-git-wip.sh~%")))
                   (chmod "test.sh" #o755)))
               (add-before 'install 'check
                 (lambda _
                   (invoke "./test.sh"))))))
    (synopsis "help track git Work In Progress branches.")
    (description "git-wip is a script that will manage Work In Progress (or WIP) branches. WIP
branches are mostly throw away but identify points of development between
commits. The intent is to tie this script into your editor so that each time you
save your file, the git-wip script captures that state in git. git-wip also
helps you return back to a previous state of development.")
    (home-page "https://github.com/bartman/git-wip.git")
    (license license:gpl3)))

(define-public vim-git-wip
  (package
   (inherit git-wip)
   (name "vim-git-wip")
   (build-system vim-build-system)
   (inputs  (list git git-wip))
   (arguments
    (list #:plugin-name "git-wip"
          #:phases
          #~(modify-phases %standard-phases
                           (add-after 'unpack 'move-source-files
                                      (lambda* (#:key inputs outputs #:allow-other-keys)
                                        (delete-file-recursively "emacs")
                                        (delete-file-recursively "sublime"))))))
   (synopsis "help track git Work In Progress branches. vim plugin.")))
