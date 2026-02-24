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
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((guix build-system cmake) #:prefix cmake:)
  #:use-module ((lotus build-system deb) #:prefix deb:)
  #:use-module ((lotus build-system patchelf) #:prefix patchelf:)
  #:use-module ((guix build-system copy) #:prefix copy:)
  #:use-module ((guix build-system qt))
  #:use-module ((guix build-system trivial) #:prefix trivial:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system go)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages cups)
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
  #:use-module (gnu packages qt)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (guix build-system cargo)
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
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages rust-crates)
  #:use-module (gnu packages shells))

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
                    (uri (string-append "mirror://sourceforge/zssh/zssh/" "1.5" "/zssh-" version ".tgz"))
                    (sha256 (base32 "06z73iq59lz8ibjrgs7d3xl39vh9yld1988yx8khssch4pw41s52"))))
    (build-system gnu:gnu-build-system)
    (inputs
     (list readline))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (arguments
     '(#:tests? #f
       #:make-flags (let ((out  (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _
             (substitute* "openpty.c"
               (("^#include \"zssh.h\"") "#include \"zssh.h\"\n#include <pty.h>\n"))
             (zero? (system* "autoreconf" "-vif"))))
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
              (sha256 (base32 "01k5p8wfj3bkcvbf4lrvc17yq9jhajj32yf1jhbgl5q13hc34d55"))))
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
              (sha256 (base32 "1w3kz8g1317hdcanzmnc76gzslzaqmffvj4n61f5cszhss1f7hdv"))))
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
              (sha256 (base32 "02nbs2yng3kzlxyv05snrixdr9qn3fnmk56jwc8zvm1z43h85s43"))))
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

(define-public proxytunnel
  (package
   (name "proxytunnel")
   (version "1.12.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/proxytunnel/proxytunnel/archive/refs/tags/v" version ".tar.gz"))
            (sha256 (base32 "1yssijk84y4qdfvq27xajpf5min4lyj1474f2p5sq7x9mfkznv0h"))))
   (build-system gnu:gnu-build-system)
   (inputs (list openssl asciidoc xmlto))
   (propagated-inputs (list))
   (arguments
    (list #:tests? #f
          #:make-flags #~(list "CC=gcc" (string-append "prefix=" #$output))
          #:phases #~(modify-phases %standard-phases
                                    (add-after 'unpack 'amend-makefile
                                     (lambda* (#:key inputs outputs #:allow-other-keys)
                                       (substitute* "Makefile"
                                                    (("\\$\\(MAKE\\) -C docs install")
                                                     "echo"))))
                                    (delete 'configure))))
   (synopsis "Stealth tunneling through HTTP(S) proxies ")
   (description "This is proxytunnel, a program that connects stdin and stdout to an origin
server somewhere in the Internet through an industry standard HTTPS proxy. I
originally wrote this program to be used as an extension to SSH, to be able to
SSH to my box at home. In this file, I will describe the use with SSH. If you
want to use it with some other application, feel free, and let me know!")
   (home-page "http://proxytunnel.sf.net/")
   (license license:gpl2)))

(define-public lighttable
  (package
    (name "lighttable")
    (version "0.8.1")
    (source
     (let ((arch (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux" "arm64")
                   ("armhf-linux" "armhf")
                   (_ "x64")))
           (hash (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux"
                    "0m5x9v577h8n16ypzb1y2066alc59v5bw7jiqp2xr7g20s9kb0vy")
                   ("armhf-linux"
                    "047gz12gx8pa5aglykd0785l6i9ivsn4kkgfhi5l0y4jh8hjys8c")
                   (_
                    "06fj725xfhf3fwrf7dya7ijmxq3v76kfmd4lr2067a92zhlwr5pv"))))
       (origin
        (method url-fetch)
        (uri
         (string-append
          "https://github.com/LightTable/LightTable/releases/download/" version
          "/lighttable-" version "-linux.tar.gz"))
        (sha256 (base32 hash)))))
    (supported-systems '("armhf-linux" "aarch64-linux" "x86_64-linux"))
    (native-inputs
     (list tar))
    (inputs
     (list gdk-pixbuf
           gtk+-2
           gconf
           nss
           glibc
           (list gcc "lib")
           libxdamage
           libxcomposite
           libxcursor
           cups-minimal
           libxrandr
           alsa-lib
           dbus
           eudev))
    (build-system patchelf:patchelf-build-system)
    (arguments `(#:input-lib-mapping '(("out" "lib" "")
                                       ("nss" "lib/nss"))
                ;; #:phases            (modify-phases %standard-phases
                ;;                       (add-after
                ;;                           'unpack 'changedir
                ;;                         (lambda* (#:key inputs outputs #:allow-other-keys)
                ;;                           (chdir "..")
                ;;                           (let ((cwd (getcwd)))
                ;;                             (begin
                ;;                               (let* ((parent (getcwd))
                ;;                                      (source (string-append (getcwd) "/unpack"))
                ;;                                      (files (directory-list-files parent)))
                ;;                                 (for-each (lambda (entry)
                ;;                                             (let ((src (string-append parent "/" entry))
                ;;                                                   (trg (string-append source "/" entry)))
                ;;                                               (mkdir-p (dirname trg))
                ;;                                               (rename-file src trg)))
                ;;                                           files)))
                ;;                             (begin
                ;;                               (delete-file (string-append cwd "/unpack/" "usr/lib/kde4/kcm_adobe_flash_player.so"))
                ;;                               (if #f
                ;;                                (symlink "../../lib64/kde4/kcm_adobe_flash_player.so"
                ;;                                         (string-append cwd "/unpack/" "usr/lib/kde4/kcm_adobe_flash_player.so"))
                ;;                                (delete-file (string-append cwd "/unpack/" "usr/lib64/kde4/kcm_adobe_flash_player.so"))))
                ;;                             (begin
                ;;                               (begin
                ;;                                 (delete-file (string-append cwd "/unpack/" "usr/bin/flash-player-properties"))
                ;;                                 ;; (delete-file (string-append cwd "/unpack/" "usr/bin"))
                ;;                                 (for-each (lambda (path)
                ;;                                             (if (access? (string-append cwd "/unpack/usr/" path) F_OK)
                ;;                                                 (copy-recursively (string-append cwd "/unpack/usr/" path) (string-append cwd "/source/" path))
                ;;                                                 (format #t "~a not exists.~%" (string-append cwd "/unpack/usr/" path))))
                ;;                                           (list "lib64"
                ;;                                                 "share"
                ;;                                                 ;; "bin"
                ;;                                                 "lib")))
                ;;                               (begin
                ;;                                 (mkdir-p (string-append cwd "/source/share/patchelf-adobe-flashplugin"))
                ;;                                 (mkdir-p (string-append cwd "/source/lib/adobe-flashplugin"))
                ;;                                 (copy-recursively (string-append cwd "/unpack/" "LGPL") (string-append cwd "/source/share/patchelf-adobe-flashplugin/LGPL"))
                ;;                                 (copy-file (string-append cwd "/unpack/" "readme.txt")  (string-append cwd "/source/share/patchelf-adobe-flashplugin/readme.txt"))
                ;;                                 (copy-file (string-append cwd "/unpack/" "license.pdf") (string-append cwd "/source/share/patchelf-adobe-flashplugin/license.pdf"))
                ;;                                 (mkdir-p   (string-append cwd "/source/lib"))
                ;;                                 (copy-file (string-append cwd "/unpack/" "libflashplayer.so") (string-append cwd "/source/lib/libflashplayer.so"))
                ;;                                 (copy-file (string-append cwd "/unpack/" "libflashplayer.so") (string-append cwd "/source/lib/adobe-flashplugin/libflashplayer.so")))
                ;;                               (begin
                ;;                                 (for-each (lambda (path)
                ;;                                             (let* ((stat (lstat path)))
                ;;                                               (chmod path (logior #o111 (stat:perms stat)))))
                ;;                                           (list (string-append cwd "/source/lib/libflashplayer.so")
                ;;                                                 (string-append cwd "/source/lib/adobe-flashplugin/libflashplayer.so")))))
                ;;                             (chdir (string-append cwd "/source"))
                ;;                             #t)))
                ;;                       )
                ))
    (home-page "https://lighttable.com/")
    (synopsis "Next-generation code editor")
    (description "LightTable is a next-generation code editor that connects you to your code with real-time feedback.")
    (license license:gpl3)))

(define-public reor
  (package
    (name "reor")
    (version "0.2.32")
    (source
     (let ((arch (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux" "arm64")
                   ("armhf-linux" "armhf")
                   (_ "x64")))
           (hash (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux"
                    "0m5x9v577h8n16ypzb1y2066alc59v5bw7jiqp2xr7g20s9kb0vy")
                   ("armhf-linux"
                    "047gz12gx8pa5aglykd0785l6i9ivsn4kkgfhi5l0y4jh8hjys8c")
                   (_
                    "0cngp0fnq64aq2hbp2ya6hdfdcf8sjv4bjcyc5ff5glmb6fvm34w"))))
       (origin
        (method url-fetch)
        (uri
         (string-append
          "https://github.com/reorproject/reor/archive/refs/tags/v-" version ".tar.gz"))
        (sha256
         (base32 hash)))))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:substitutable? #f
           ;; #:wrapper-plan
           ;; #~'(("opt/reor/Reor" (("out" "/opt/reor"))))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (mkdir-p "opt/reor")
                   (invoke "tar" "-xvf" source "--strip-components=1" "-C" "opt/reor")))
               (add-before 'install-wrapper 'install-entrypoint
                 (lambda _
                   (let* ((bin (string-append #$output "/bin")))
                     (delete-file (string-append #$output "/environment-variables"))
                     (mkdir-p bin)
                     (symlink (string-append #$output "/opt/reor/Reor")
                              (string-append bin "/Reor"))))))))
               ;; (add-after 'install-entrypoint 'install-resources
               ;;   (lambda _
               ;;     (let* ((icons (string-append #$output "/share/icons/hicolor/512x512/apps"))
               ;;            (icon.png (string-append #$output
               ;;                                     "/opt/resources/app/"
               ;;                                     "core/img/lticon.png"))
               ;;            (apps (string-append #$output "/share/applications")))
               ;;       (mkdir-p icons)
               ;;       (symlink icon.png
               ;;                (string-append icons "/reor.png"))
               ;;       (mkdir-p apps)
               ;;       (make-desktop-entry-file (string-append apps "/" #$name ".desktop")
               ;;                                #:name "Reor"
               ;;                                #:generic-name "IDE"
               ;;                                #:exec (string-append #$output "/bin/reor --ozone-platform-hint=auto")
               ;;                                #:icon "reor"
               ;;                                #:type "Application"
               ;;                                #:actions '("new-empty-window")
               ;;                                #:keywords '("ide" "editor")
               ;;                                #:categories '("Development" "IDE")
               ;;                                #:startup-notify #t
               ;;                                #:startup-w-m-class "Reor"
               ;;                                #:comment
               ;;                                '(("en" "The next generation code editor.")
               ;;                                  (#f "The next generation code editor."))))))

    (supported-systems '("armhf-linux" "aarch64-linux" "x86_64-linux"))
    (native-inputs
     (list tar))
    (inputs
     (list gdk-pixbuf
           gtk+-2
           gconf
           nss))
    (home-page "https://reor.com/")
    (synopsis "Next-generation code editor")
    (description "Reor is a next-generation code editor that connects you to your code with real-time feedback.")
    (license license:gpl3)))


(define-public go-github-com-guumaster-tablewriter
  (package
    (name "go-github-com-guumaster-tablewriter")
    (version "0.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/guumaster/tablewriter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "019knrimn3cy4733zxwlal38wfaibr4h4v3anzan5j8lizr2f6gl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/guumaster/tablewriter"))
    (propagated-inputs (list go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/guumaster/tablewriter")
    (synopsis "ASCII Table Writer")
    (description "Create & Generate text based table.")
    (license license:expat)))

(define-public go-github-com-guumaster-cligger
  (package
    (name "go-github-com-guumaster-cligger")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/guumaster/cligger")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qazndnj3yrhqspp1m4i66ylvs36nln0x5f0iva5z3h854k34jar"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/guumaster/cligger"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-guumaster-logsymbols))
    (home-page "https://github.com/guumaster/cligger")
    (synopsis "cligger")
    (description "nolint:goprintffuncname.")
    (license license:expat)))

(define-public go-gopkg-in-gookit-color-v1
  (package
   (inherit go-github-com-gookit-color)
   (name "go-gopkg-in-gookit-color-v1")
   (version "1.5.4")
   ;; (version "1.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/gookit/color")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "012naz084chvdqzrrzv9pklqfh259hi2jcp2f3n39fppvjwmzgkf"))))
   (arguments
    (substitute-keyword-arguments
        (package-arguments go-github-com-gookit-color)
        ;; ((#:import-path path) '"gopkg.in/gookit/color.v1")
        ((#:import-path path) "gopkg.in/gookit/color.v1")
        ((#:tests? _ #f) #f)))))


(define-public go-github-com-guumaster-logsymbols
  (package
    (name "go-github-com-guumaster-logsymbols")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/guumaster/logsymbols")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j2w9grmgd785vkhj6bjkcn9liahr4q4zd54ms6y6w79c3sz5giy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/guumaster/logsymbols"))
    (propagated-inputs (list ;; go-github-com-gookit-color
                             go-gopkg-in-gookit-color-v1
                             go-github-com-stretchr-testify
                             go-github-com-mattn-go-isatty))
    (home-page "https://github.com/guumaster/logsymbols")
    (synopsis "log symbols")
    (description
     "@@code{logsymbols} will autodetect if its not in TTY mode and remove all colors.
 You can force color output with this example:.")
    (license license:expat)))

(define-public go-github-com-guumaster-hostctl
  (package
    (name "go-github-com-guumaster-hostctl")
   (version "1.1.4")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/guumaster/hostctl/archive/refs/tags/v" version ".tar.gz"))
     (file-name (git-file-name name version))
     (sha256
      (base32 "00vhysa4xn2n0khmhk7v5pp2a9v5v8mzygsfy3g23xdh5dvn3py3")))) ; update after verifying
   (build-system go-build-system)
   (inputs
    (list go-github-com-spf13-cobra
          go-github-com-spf13-afero
          go-github-com-guumaster-tablewriter
          go-github-com-guumaster-cligger
          go-github-com-docker-docker
          go-github-com-pkg-errors
          go-gopkg-in-yaml-v2))
   (arguments
    `(#:import-path "github.com/guumaster/hostctl"
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
        ;; (delete 'install-license-files)
        (replace 'build
          (lambda* (#:key import-path outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin"))
                   (src (string-append "src/" import-path)))
              (mkdir-p bin)
              (chdir src)
              (invoke "go" "build" "-v" "-o" (string-append bin "/hostctl") "./cmd/hostctl")
              (chdir "../..")
              ;; create fake path for later phase
              (mkdir-p (string-append "src/" import-path))))))))

   (home-page "https://github.com/guumaster/hostctl")
   (synopsis "Command-line tool to manage hosts file profiles")
   (description
    "Hostctl lets you manage multiple profiles of host entries, merge them,
and easily enable/disable them. It’s useful for developers switching between
environments.")
   (license license:expat)))

(define-public go-github-com-ktr0731-go-ansisgr
  (package
   (name "go-github-com-ktr0731-go-ansisgr")
   (version "0.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ktr0731/go-ansisgr")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1hjn39kqglzhh4n595ngmsxcaxqlf3f3pp9y6hmj9s3fzkfljc1i"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/ktr0731/go-ansisgr"))
   (home-page "https://github.com/ktr0731/go-ansisgr")
   (synopsis "go-ansisgr")
   (description
    "go-ansisgr provides an SGR (Select Graphic Rendition, a part of ANSI Escape
Sequence) parser.")
   (license license:expat)))


(define-public go-github-com-ktr0731-go-fuzzyfinder
  (package
    (name "go-github-com-ktr0731-go-fuzzyfinder")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ktr0731/go-fuzzyfinder")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m2f2fv647386r9k1i235cn6cj8rjjb76ny6s3407fy7hmjs9j46"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/ktr0731/go-fuzzyfinder"))
    (propagated-inputs (list go-github-com-pkg-errors
                             go-github-com-nsf-termbox-go
                             go-github-com-mattn-go-runewidth
                             go-github-com-ktr0731-go-ansisgr
                             go-github-com-google-gofuzz
                             go-github-com-google-go-cmp
                             go-github-com-gdamore-tcell-v2))
    (home-page "https://github.com/ktr0731/go-fuzzyfinder")
    (synopsis "go-fuzzyfinder")
    (description
     "Package fuzzyfinder provides terminal user interfaces for fuzzy-finding.")
    (license license:expat)))

(define-public go-github-com-twin-go-color
  (package
   (name "go-github-com-twin-go-color")
   (version "1.4.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/TwiN/go-color")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "05s8r74xm4kb40pxx1f1z72iwfgrbvhqzpgaikj1nqfdmd66pswv"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/TwiN/go-color"))
   (home-page "https://github.com/TwiN/go-color")
   (synopsis "go-color")
   (description
    "An extremely lightweight cross-platform package to colorize text in terminals.")
   (license license:expat)))

(define-public go-github-com-charmbracelet-lipgloss
  (package
    (name "go-github-com-charmbracelet-lipgloss")
    (version "1.1.0")
    ;; (version "2.0.0-beta.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/lipgloss")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        ;; (base32 "193aarw2v37j7fxf3mms80kwbrnr60p3ggh9h3jspc7wx8v4dmwf")
        (base32 "1iww4y37qsyzswsg0p8ws7wb2jl6i2z2n0kwayvfwwh73qjh8zmr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/charmbracelet/lipgloss"))
    (propagated-inputs (list go-github-com-muesli-termenv
                             go-golang-org-x-sys
                             go-github-com-rivo-uniseg
                             go-github-com-muesli-cancelreader
                             go-github-com-lucasb-eyer-go-colorful
                             go-github-com-charmbracelet-x-term
                             go-github-com-charmbracelet-x-exp-golden
                             go-github-com-charmbracelet-x-cellbuf
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-colorprofile
                             go-github-com-aymanbagabas-go-udiff))
    (home-page "https://github.com/charmbracelet/lipgloss")
    (synopsis "Lip Gloss")
    (description
     "Style definitions for nice terminal layouts.  Built with TUIs in mind.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-lipgloss-v2
  (package
   (inherit go-github-com-charmbracelet-lipgloss)
   (name "go-github-com-charmbracelet-lipgloss-v2")
   (version "2.0.0-beta.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/charmbracelet/lipgloss")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "193aarw2v37j7fxf3mms80kwbrnr60p3ggh9h3jspc7wx8v4dmwf"))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments go-github-com-charmbracelet-lipgloss)
     ((#:import-path path) "github.com/charmbracelet/lipgloss/v2")
     ((#:tests? _ #f) #f)))))

(define-public go-github-com-caarlos0-log
  (package
    (name "go-github-com-caarlos0-log")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/caarlos0/log")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "173x6pys68nsdcc79c1pk8fbv0whpvdvlzf5yps93pbcf3shgk57"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/caarlos0/log"))
    (propagated-inputs (list go-github-com-charmbracelet-lipgloss-v2
                             go-github-com-charmbracelet-colorprofile))
    (home-page "https://github.com/caarlos0/log")
    (synopsis "log")
    (description
     "Package log implements a simple and colorful CLI logger that can be used to
pretty-print output without too much effort.")
    (license license:expat)))


(define-public go-github-com-starry-s-zip
  (package
   (name "go-github-com-starry-s-zip")
   (version "0.2.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/STARRY-S/zip")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1n3ynxx6dmwvgfwfv0il3s8m40wdssip400rp20rb23vq6kk4fsr"))))
   (build-system go-build-system)
   (arguments
    (list
     #:tests? #f
     #:import-path "github.com/STARRY-S/zip"))
   (home-page "https://github.com/STARRY-S/zip")
   (synopsis "Go zip library")
   (description
    "Package zip provides support for reading and writing ZIP archives.")
   (license license:bsd-3)))


(define-public go-github-com-bodgit-plumbing
  (package
    (name "go-github-com-bodgit-plumbing")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bodgit/plumbing")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vbyc6pxirnkvcx5acqcxg57ii3h2fv1d9dx3mq6mcbn00jdsqly"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bodgit/plumbing"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/bodgit/plumbing")
    (synopsis "plumbing")
    (description "Package plumbing is a collection of assorted I/O helpers.")
    (license license:bsd-3)))

(define-public go-github-com-bodgit-windows
  (package
    (name "go-github-com-bodgit-windows")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bodgit/windows")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iklb2w7yj1icwfwyrimkbkj2dk43lqs9k00ww09x2r5p8cl0ahr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bodgit/windows"))
    (home-page "https://github.com/bodgit/windows")
    (synopsis "windows")
    (description
     "Package windows is a collection of types native to Windows platforms but are
useful on non-Windows platforms.")
    (license license:bsd-3)))

(define-public go-github-com-bodgit-sevenzip
  (package
    (name "go-github-com-bodgit-sevenzip")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bodgit/sevenzip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hjams64h6i3in3w35ip5d4n0diza5ara58k82y4cb59micv4qqd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/bodgit/sevenzip"))
    (propagated-inputs (list go-golang-org-x-text
                             go-golang-org-x-sync
                             go-go4-org
                             go-github-com-ulikunitz-xz
                             go-github-com-stretchr-testify
                             go-github-com-spf13-afero
                             go-github-com-pierrec-lz4-v4
                             go-github-com-klauspost-compress
                             go-github-com-hashicorp-golang-lru-v2
                             go-github-com-bodgit-windows
                             go-github-com-bodgit-plumbing
                             go-github-com-andybalholm-brotli))
    (home-page "https://github.com/bodgit/sevenzip")
    (synopsis "sevenzip")
    (description "Package sevenzip provides read access to 7-zip archives.")
    (license license:bsd-3)))

(define-public go-github-com-sorairolake-lzip-go
  (package
   (name "go-github-com-sorairolake-lzip-go")
   (version "0.3.8")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/sorairolake/lzip-go")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1d5lxwfg3diij77892bl07n2530263703kps6rirfyjx27z9dzcr"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/sorairolake/lzip-go"))
   (propagated-inputs (list go-github-com-ulikunitz-xz
                            go-github-com-google-go-cmdtest))
   (home-page "https://github.com/sorairolake/lzip-go")
   (synopsis "lzip-go")
   (description
    "Package lzip implements the
@@url{https://www.nongnu.org/lzip/manual/lzip_manual.html#File-format,lzip
compressed format}.")
   (license (list license:asl2.0 license:expat))))

(define-public go-github-com-therootcompany-xz
  (package
   (name "go-github-com-therootcompany-xz")
   (version "1.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/therootcompany/xz")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1kzzgifx0n6rln7mv9z8naqf2dxdb3j2lp992g59adj86hpyvs2a"))))
   (build-system go-build-system)
   (arguments
    (list
     #:import-path "github.com/therootcompany/xz"))
   (home-page "https://github.com/therootcompany/xz")
   (synopsis "Xz")
   (description "Package xz implements XZ decompression natively in Go.")
   (license license:cc0)))

(define-public go-github-com-mholt-archiver-v4
  (package
   (inherit go-github-com-mholt-archiver-v3)
   (name "go-github-com-mholt-archiver-v4")
   (version "4.0.0-alpha.8")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mholt/archiver")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1glp5hbwym63d7kqqmygxlw65g1x9nrkl66sghs6q8qbchhplkky"))))
   (propagated-inputs
    (list go-github-com-andybalholm-brotli
          go-github-com-dsnet-compress
          go-github-com-golang-snappy
          go-github-com-klauspost-compress
          go-github-com-klauspost-pgzip
          go-github-com-nwaples-rardecode
          go-github-com-pierrec-lz4-v4
          go-github-com-ulikunitz-xz
          go-github-com-xi2-xz
          go-github-com-starry-s-zip
          go-github-com-bodgit-sevenzip
          go-github-com-nwaples-rardecode-v2
          go-github-com-sorairolake-lzip-go
          go-github-com-therootcompany-xz))
   (arguments
    (substitute-keyword-arguments
     (package-arguments go-github-com-mholt-archiver-v3)
     ((#:import-path path) "github.com/mholt/archiver/v4")
     ((#:tests? _ #f) #f)))))


(define-public go-github-com-deadc0de6-gocatcli
  (package
   (name "go-github-com-deadc0de6-gocatcli")
   (version "1.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/deadc0de6/gocatcli/archive/refs/tags/v" version ".tar.gz"))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0wj2gbzx7d97binclqcmvaj3igvvka88ifmn965i0kaigys86wa3"))))
   (build-system go-build-system)
   (inputs
    (list go-github-com-spf13-cobra
          go-github-com-spf13-viper
          go-github-com-pterm-pterm
          go-github-com-briandowns-spinner
          go-github-com-ktr0731-go-fuzzyfinder
          go-github-com-caarlos0-log
          go-github-com-twin-go-color
          go-github-com-anacrolix-fuse
          go-github-com-rivo-tview
          go-github-com-h2non-filetype
          go-github-com-mholt-archiver-v4))
   (native-inputs
    (list bash fish zsh))
   (arguments
    `(#:import-path "gocatcli"
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (replace 'build
                              (lambda* (#:key  outputs #:allow-other-keys)
                                (let* ((out (assoc-ref outputs "out"))
                                       (bin (string-append out "/bin")))
                                  (mkdir-p bin)
                                  (chdir "src/gocatcli")
                                  (invoke "go" "build" "-v" "-o" (string-append bin "/gocatcli") "./cmd/gocatcli")
                                  (chdir "../..")
                                  #t)))
                     (add-after 'install 'install-completions
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (bin (string-append out "/bin/gocatcli"))
                                         (bash-dir (string-append out "/etc/bash_completion.d"))
                                         (zsh-dir  (string-append out "/share/zsh/site-functions"))
                                         (fish-dir (string-append out "/share/fish/vendor_completions.d")))
                                    (mkdir-p bash-dir)
                                    (mkdir-p zsh-dir)
                                    (mkdir-p fish-dir)
                                    ;; Capture and write the output of each completion command
                                    (with-output-to-file (string-append bash-dir "/gocatcli")
                                      (lambda () (invoke bin "completion" "bash")))
                                    (with-output-to-file (string-append zsh-dir "/_gocatcli")
                                      (lambda () (invoke bin "completion" "zsh")))
                                    (with-output-to-file (string-append fish-dir "/gocatcli.fish")
                                      (lambda () (invoke bin "completion" "fish")))
                                    #t))))))
    (home-page "https://github.com/deadc0de6/gocatcli")
    (synopsis "Command line catalog tool to index and search offline media.")
    (description "gocatcli gives the ability to navigate, explore and find your files that are stored on external media when those are not connected.")
    (license #f)))


(define* (lotus-cargo-inputs name #:key (module '(lotus packages rust-crates)))
  "Lookup Cargo inputs for NAME defined in MODULE, return an empty list if
unavailable."
  (let ((lookup (module-ref (resolve-interface module) 'lookup-cargo-inputs)))
    (or (lookup name)
        (begin
          (warning (G_ "no Cargo inputs available for '~a'~%") name)
          '()))))

(define-public rust-usrhttpd-v0.1.0
  (package
   (name "rust-usrhttpd")
   (version "v0.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/sharad/rust-usrhttpd")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0rkg6x9sanf7mvqx4q0m1hw860hxbyx29i88wci8j8rmm4yc0hzx"))))
   (build-system cargo-build-system)
   ;; (arguments
   ;;  (list ;; #:skip-build? #t
   ;;        ;; #:cargo-package-crates ''()
   ;;        #:cargo-build-flags '("--release")
   ;;        #:tests? #f))
   (inputs (lotus-cargo-inputs 'xyz))
   (home-page "https://github.com/sharad/rust-usrhttpd")
   (synopsis "Small Rust .htaccess web server")
   (description
    "Modular Rust HTTP server with .htaccess support, reverse proxy, TLS, and directory indexing.")
   (license license:gpl3+)))

rust-usrhttpd-v0.1.0



