;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017–2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2019, 2020 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2021 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 David Thompson <dthompson2@worcester.edu>
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

(define-module (lotus packages haskell-apps)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg))



(define-public git-annex-new1
  (package
    (name "git-annex-new1")
    (version "10.20240701")
    ;; (name "git-annex")
    ;; (version "10.20240227")
    (source
     (origin
       ;; hackage release doesn't include everything needed for extra bits.
       (method git-fetch)
       (uri (git-reference
              (url "https://git.joeyh.name/git/git-annex.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07lxs5d7a100gqyk8m7d7ws0ychqi9d54gwsm79xb1kmwgsjk3f5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "git-annex")))
    (arguments
     `(#:configure-flags
       '("--flags=-Android -Webapp")
       #:haddock? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shell-for-tests
           (lambda _
             ;; Shell.hs defines "/bin/sh" that is used in Git hooks.  We
             ;; shouldn't patch hooks with Guix's current bash because the
             ;; hooks can exist after that bash is garbage collected, but
             ;; let's temporarily patch it so that we can run the tests.
             (copy-file "Utility/Shell.hs" "/tmp/Shell.hs")
             (substitute* "Utility/Shell.hs"
               (("/bin/sh") (which "sh")))))
         (add-before 'configure 'patch-webapp
           (lambda _
             ;; Replace loose references to xdg-open so that 'git annex
             ;; webapp' runs without making the user also install xdg-utils.
             (substitute* '("Assistant/WebApp/DashBoard.hs"
                            "Utility/WebApp.hs")
               (("xdg-open") (which "xdg-open")))
             ;; Also replace loose references to lsof.
             (substitute* "Assistant/Threads/Watcher.hs"
               (("\"lsof\"")
                (string-append "\"" (which "lsof") "\"")))))
         (add-before 'configure 'factor-setup
           (lambda _
             ;; Factor out necessary build logic from the provided
             ;; `Setup.hs' script.  The script as-is does not work because
             ;; it cannot find its dependencies, and there is no obvious way
             ;; to tell it where to look.
             (call-with-output-file "PreConf.hs"
               (lambda (out)
                 (format out "import qualified Build.Configure as Configure~%")
                 (format out "main = Configure.run Configure.tests~%")))
             (call-with-output-file "Setup.hs"
               (lambda (out)
                 (format out "import Distribution.Simple~%")
                 (format out "main = defaultMain~%")))))
         (add-before 'configure 'pre-configure
           (lambda _
             (invoke "runhaskell" "PreConf.hs")))
         (add-after 'build 'build-manpages
           (lambda _
             (invoke "make" "mans")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             ;; We need to set the path so that Git recognizes
             ;; `git annex' as a custom command.
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (getcwd) "/dist/build/git-annex"))
             (when tests?
               (with-directory-excursion "dist/build/git-annex"
                 (symlink "git-annex" "git-annex-shell"))
               (invoke "git-annex" "test"))))
         (add-after 'check 'unpatch-shell-and-rebuild
           (lambda args
             ;; Undo `patch-shell-for-tests'.
             (copy-file "/tmp/Shell.hs" "Utility/Shell.hs")
             (apply (assoc-ref %standard-phases 'build) args)))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completions.d"))
                    (fish (string-append out "/share/fish/vendor_completions.d"))
                    (zsh (string-append out "/share/zsh/site-functions")))
               (setenv "PREFIX" out)
               (invoke "make" "install-mans")
               (mkdir-p bash)
               (copy-file "bash-completion.bash"
                          (string-append bash "/git-annex"))
               (mkdir-p fish)
               (with-output-to-file (string-append fish "/git-annex.fish")
                 (lambda _
                   (invoke (string-append out "/bin/git-annex")
                           "--fish-completion-script" "git-annex")))
               (mkdir-p zsh)
               (with-output-to-file (string-append zsh "/_git-annex")
                 (lambda _
                   (invoke (string-append out "/bin/git-annex")
                           "--zsh-completion-script" "git-annex"))))))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-annex-shell"))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-remote-tor-annex"))))))))
    (inputs
     (list curl
           ghc-aeson
           ghc-ansi-terminal
           ghc-async
           ghc-attoparsec
           ghc-aws
           ghc-bloomfilter
           ghc-byteable
           ghc-case-insensitive
           ghc-clientsession
           ghc-concurrent-output
           ghc-conduit
           ghc-connection
           ghc-crypto-api
           ghc-cryptonite
           ghc-data-default
           ghc-dav
           ghc-dbus
           ghc-disk-free-space
           ghc-dlist
           ghc-edit-distance
           ghc-exceptions
           ghc-fdo-notify
           ghc-feed
           ghc-filepath-bytestring
           ghc-free
           ghc-git-lfs
           ghc-hinotify
           ghc-http-client
           ghc-http-client-tls
           ghc-http-client-restricted
           ghc-http-conduit
           ghc-http-types
           ghc-ifelse
           ghc-magic
           ghc-memory
           ghc-microlens
           ghc-monad-control
           ghc-monad-logger
           ghc-mountpoints
           ghc-network
           ghc-network-bsd
           ghc-network-info
           ghc-network-multicast
           ghc-network-uri
           ghc-old-locale
           ghc-optparse-applicative
           ghc-persistent
           ghc-persistent-sqlite
           ghc-persistent-template
           ghc-quickcheck
           ghc-random
           ghc-regex-tdfa
           ghc-resourcet
           ghc-safesemaphore
           ghc-sandi
           ghc-securemem
           ghc-socks
           ghc-split
           ghc-stm-chans
           ghc-tagsoup
           ghc-torrent
           ghc-transformers
           ghc-unix-compat
           ghc-unliftio-core
           ghc-unordered-containers
           ghc-utf8-string
           ghc-uuid
           ghc-vector
           ghc-wai
           ghc-wai-extra
           ghc-warp
           ghc-warp-tls
           ghc-yesod
           ghc-yesod-core
           ghc-yesod-form
           ghc-yesod-static
           lsof
           rsync
           xdg-utils))
    (propagated-inputs
     (list git))
    (native-inputs
     (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck ghc-tasty-rerun
           perl))
    (home-page "https://git-annex.branchable.com/")
    (synopsis "Manage files with Git, without checking in their contents")
    (description "This package allows managing files with Git, without
checking the file contents into Git.  It can store files in many places,
such as local hard drives and cloud storage services.  It can also be
used to keep a folder in sync between computers.")
    ;; The main author has released all his changes under AGPLv3+ as of March
    ;; 2019 (7.20190219-187-g40ecf58d4).  These are also licensed under the
    ;; original GPLv3+ license, but going forward new changes will be under
    ;; only AGPLv3+.  The other licenses below cover code written by others.
    ;; See git-annex's COPYRIGHT file for details on each file.
    (license (list license:agpl3+
                   license:gpl3+
                   license:bsd-2
                   license:expat
                   license:gpl2))))
git-annex-new1

