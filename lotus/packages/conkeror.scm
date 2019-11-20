;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 John J. Foerch <jjfoerch@earthlink.net>
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

(define-module (lotus packages conkeror)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnuzilla))


;; (define %icecat-version "68.2.0-guix0-preview3")
;; (define %icecat-build-id "20191031000000") ;must be of the form YYYYMMDDhhmmss
(define-public myicecat
  (package
    (name "myicecat")
    (version "60.2.0-gnu1")
    (source
     (origin
      (method url-fetch)
      ;; Temporary URL pending official release:
      (uri "https://alpha.gnu.org/gnu/gnuzilla/60.2.0/icecat-60.2.0-gnu1.tar.bz2")
      #;
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (first (string-split version #\-))
                          "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0lqx7g79x15941rhjr3qsfwsny6vzc7d7abdmvjy6jjbqkqlc1zl"))
      (patches
       (list
        (search-patch  "icecat-avoid-bundled-libraries.patch")
        (search-patch  "icecat-use-system-graphite2+harfbuzz.patch")
        (search-patch  "icecat-use-system-media-libs.patch")
        (mozilla-patch "icecat-CVE-2018-12385.patch"      "80a4a7ef2813" "1vgcbimpnfjqj934v0cryq1g13xac3wfmd4jyhcb5s60x8xyssf5")
        (search-patch  "icecat-CVE-2018-12383.patch")
        (mozilla-patch "icecat-bug-1489744.patch"         "6546ee839d30" "11mhvj77r789b428bfxqq5wdx8yr7lbrdjzr8qjj6fw197pldn51")
        (mozilla-patch "icecat-CVE-2018-12386.patch"      "4808fcb2e6ca" "05sc881l7sh8bag8whd2ggdn198lskqcxq8f41scfpqscw6xs5d5")
        (mozilla-patch "icecat-CVE-2018-12387.patch"      "b8f5c37486e1" "0lvmbh126m695kgdbasy1y5xh9n1j08cwdhn071mgvj6yn8cns5z")
        (mozilla-patch "icecat-bug-1464751.patch"         "d5d00faf0465" "1mj7dbb06brwrk0mvap0z4lfl2hwz1cj6dwjvdrisxm046pdw98i")
        (mozilla-patch "icecat-bug-1472538.patch"         "11462f2b98f2" "1nxgh0plzilylx8r73r7d74pv66qwjqxmd7nqii33p0snl2jjfzs")
        (mozilla-patch "icecat-bug-1478685.patch"         "098585dc86fc" "1b0x4qdh6isvffmibvc8ad8z62m3iky9q6jq0z6gyvn8q252cqal")
        (mozilla-patch "icecat-bug-1486080.patch"         "3f8d57d936ea" "0pz2c18wcgj44v0j8my9xbm90m4bsjcvzmavj569fi8bh6s6zz8p")
        (mozilla-patch "icecat-bug-1423278.patch"         "878ceaee5634" "0i47s5nvrx9vqbnj6s9y9f4ffww20p8nviqa6frg676y1188xlyl")
        (mozilla-patch "icecat-bug-1442010.patch"         "87be1b98ec9a" "15f4l18c7hz9aqn89gg3dwmdidfwgn10dywgpzydm8mps45amx7j")
        (mozilla-patch "icecat-bug-1484559.patch"         "99e58b5307ce" "02fdgbliwzi2r2376wg6k1rky1isfka0smac4ii2cll01jhpfrn6")
        (mozilla-patch "icecat-bug-1487098.patch"         "f25ce451a492" "18nzg39iyxza1686180qk9cc88l5j2hf1h35d62lrqmdgd9vcj33")
        (mozilla-patch "icecat-bug-1484905.patch"         "35c26bc231df" "0qh8d4z6y03h5xh7djci26a01l6zq667lg2k11f6zzg7z2j0h67x")
        (mozilla-patch "icecat-bug-1488061.patch"         "050d0cfa8e3d" "05ql798ynbyz5pvyri4b95j4ixmgnny3zl7sd2ckfrrbm9mxh627")
        (mozilla-patch "icecat-bug-1434963-pt1.patch"     "1e6dad87efed" "1v00a6cmgswjk54041jyv1ib129fxshpzwk6mn6lr0v5hylk3bx9")
        (mozilla-patch "icecat-bug-1434963-pt2.patch"     "6558c46df9ea" "0vdy9dm9w5k1flhcfxwvvff0aa415b5mgmmq5r37i83686768xfb")
        (mozilla-patch "icecat-bug-1434963-pt3.patch"     "686fcfa8abd6" "0ihqr11aq4b0y7mx7bwn8yzn25mv3k2gdphm951mj1g85qg35ann")
        (mozilla-patch "icecat-bug-1491132.patch"         "14120e0c74d6" "188c5fbhqqhmlk88p70l6d97skh7xy4jhqdby1ri3h9ix967515j")))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (use-modules (ice-9 ftw))
          ;; Remove bundled libraries that we don't use, since they may
          ;; contain unpatched security flaws, they waste disk space and
          ;; network bandwidth, and may cause confusion.
          (for-each delete-file-recursively
                    '(;; FIXME: Removing the bundled icu breaks configure.
                      ;;   * The bundled icu headers are used in some places.
                      ;;   * The version number is taken from the bundled copy.
                      ;;"intl/icu"
                      ;;
                      ;; FIXME: A script from the bundled nspr is used.
                      ;;"nsprpub"
                      ;;
                      ;; FIXME: With the update to IceCat 60, using system NSS
                      ;;        broke certificate validation.  See
                      ;;        <https://bugs.gnu.org/32833>.  For now, we use
                      ;;        the bundled NSPR and NSS.  TODO: Investigate,
                      ;;        and try to unbundle these libraries again.
                      ;; UNBUNDLE-ME! "security/nss"
                      ;;
                      ;; TODO: Use more system media libraries.  See:
                      ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                      ;;   * libtheora: esr60 wants v1.2, not yet released.
                      ;;   * soundtouch: avoiding the bundled library would
                      ;;     result in some loss of functionality.  There's
                      ;;     also an issue with exception handling
                      ;;     configuration.  It seems that this is needed in
                      ;;     some moz.build:
                      ;;       DEFINES['ST_NO_EXCEPTION_HANDLING'] = 1
                      ;;   * libopus
                      ;;   * speex
                      ;;
                      "modules/freetype2"
                      "modules/zlib"
                      "modules/libbz2"
                      "ipc/chromium/src/third_party/libevent"
                      "media/libjpeg"
                      "media/libvpx"
                      "media/libogg"
                      "media/libvorbis"
                      ;; "media/libtheora" ; wants theora-1.2, not yet released
                      "media/libtremor"
                      "gfx/harfbuzz"
                      "gfx/graphite2"
                      "js/src/ctypes/libffi"
                      "db/sqlite3"))
          ;; Delete .pyc files, typically present in icecat source tarballs
          (for-each delete-file (find-files "." "\\.pyc$"))
          ;; Delete obj-* directories, sometimes present in icecat tarballs
          (for-each delete-file-recursively
                    (scandir "." (lambda (name)
                                   (string-prefix? "obj-" name))))
          #t))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("graphite2" ,graphite2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("hunspell" ,hunspell)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ;; ("libtheora" ,libtheora) ; wants theora-1.2, not yet released
       ("libvorbis" ,libvorbis)
       ("libxft" ,libxft)
       ("libevent" ,libevent)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxcomposite" ,libxcomposite)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("ffmpeg" ,ffmpeg)
       ("libvpx" ,libvpx)
       ("icu4c" ,icu4c)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ;; See <https://bugs.gnu.org/32833>
       ;;   and related comments in the 'snippet' above.
       ;; UNBUNDLE-ME! ("nspr" ,nspr)
       ;; UNBUNDLE-ME! ("nss" ,nss)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
      ;; Icecat 60 checkes for rust>=1.24
     `(("rust" ,rust-1.24)
       ("cargo" ,rust-1.24 "cargo")
       ("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("yasm" ,yasm)
       ("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf-2.13)
       ("which" ,which)))
    (arguments
     `(#:tests? #f          ; no check target
       #:out-of-source? #t  ; must be built outside of the source directory

       ;; XXX: There are RUNPATH issues such as
       ;; $prefix/lib/icecat-31.6.0/plugin-container NEEDing libmozalloc.so,
       ;; which is not in its RUNPATH, but they appear to be harmless in
       ;; practice somehow.  See <http://hydra.gnu.org/build/378133>.
       #:validate-runpath? #f

       #:imported-modules ,%cargo-build-system-modules ;for `generate-checksums'

       #:configure-flags '("--enable-default-toolkit=cairo-gtk3"

                           "--with-distribution-id=org.gnu"

                           "--enable-startup-notification"
                           "--enable-pulseaudio"

                           "--disable-tests"
                           "--disable-updater"
                           "--disable-crashreporter"
                           "--disable-maintenance-service"
                           "--disable-eme"
                           "--disable-gconf"

                           ;; Stylo requires LLVM/clang.  For now, disable it.
                           "--disable-stylo"

                           ;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           ;; Hack to work around missing
                           ;; "unofficial" branding in icecat.
                           "--enable-official-branding"

                           ;; Avoid bundled libraries.
                           "--with-system-zlib"
                           "--with-system-bz2"
                           "--with-system-jpeg"        ; must be libjpeg-turbo
                           "--with-system-libevent"
                           "--with-system-ogg"
                           "--with-system-vorbis"
                           ;; "--with-system-theora" ; wants theora-1.2, not yet released
                           "--with-system-libvpx"
                           "--with-system-icu"
                           
                           ;; See <https://bugs.gnu.org/32833>
                           ;;   and related comments in the 'snippet' above.
                           ;; UNBUNDLE-ME! "--with-system-nspr"
                           ;; UNBUNDLE-ME! "--with-system-nss"
                           
                           "--with-system-harfbuzz"
                           "--with-system-graphite2"
                           "--enable-system-pixman"
                           "--enable-system-ffi"
                           "--enable-system-hunspell"
                           "--enable-system-sqlite")

                           ;; Fails with "--with-system-png won't work because
                           ;; the system's libpng doesn't have APNG support".
                           ;; According to
                           ;; http://sourceforge.net/projects/libpng-apng/ ,
                           ;; "the Animated Portable Network Graphics (APNG)
                           ;; is an unofficial extension of the Portable
                           ;; Network Graphics (PNG) format";
                           ;; we probably do not wish to support it.
                           ;; "--with-system-png"
                           

       #:modules ((ice-9 ftw)
                  (ice-9 rdelim)
                  (ice-9 match)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'ensure-no-mtimes-pre-1980
          (lambda _
            ;; Without this, the 'source/test/addons/packed.xpi' and
            ;; 'source/test/addons/simple-prefs.xpi' targets fail while trying
            ;; to create zip archives.
            (let ((early-1980 315619200)) ; 1980-01-02 UTC
              (ftw "." (lambda (file stat flag)
                         (unless (<= early-1980 (stat:mtime stat))
                           (utime file early-1980 early-1980))
                         #t))
              #t)))
         (add-after
          'unpack 'link-libxul-with-libraries
          (lambda _
            ;; libxul.so dynamically opens libraries, so here we explicitly
            ;; link them into libxul.so instead.
            ;;
            ;; TODO: It might be preferable to patch in absolute file names in
            ;; calls to dlopen or PR_LoadLibrary, but that didn't seem to
            ;; work.  More investigation is needed.
            (substitute* "toolkit/library/moz.build"
              (("^# This library needs to be last" all)
               (string-append "OS_LIBS += [
    'GL', 'gnome-2', 'canberra', 'Xss', 'cups', 'gssapi_krb5',
    'avcodec', 'avutil', 'pulse' ]\n\n"
                              all)))
            #t))
         (replace 'bootstrap
           (lambda _
             (invoke "sh" "-c" "autoconf old-configure.in > old-configure")))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda _
             (use-modules (guix build cargo-build-system))
             (let ((null-file "/dev/null")
                   (null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
               (substitute* '("Cargo.lock" "servo/Cargo.lock")
                 (("(\"checksum .* = )\".*\"" all name)
                  (string-append name "\"" null-hash "\"")))
               (for-each
                (lambda (filename)
                  (delete-file filename)
                  (let ((dir (dirname filename)))
                    (display (string-append
                              "patch-cargo-checksums: generate-checksums for "
                              dir "\n"))
                    (generate-checksums dir null-file)))
                (find-files "third_party/rust" ".cargo-checksum.json")))
             #t))
         (replace
          'configure
          ;; configure does not work followed by both "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bash (which "bash"))
                   (abs-srcdir (getcwd))
                   (srcdir (string-append "../" (basename abs-srcdir)))
                   (flags `(,(string-append "--prefix=" out)
                            ,(string-append "--with-l10n-base="
                                            abs-srcdir "/l10n")
                            ,@configure-flags)))
              (setenv "SHELL" bash)
              (setenv "CONFIG_SHELL" bash)
              (setenv "AUTOCONF" (which "autoconf")) ; must be autoconf-2.13
              (mkdir "../build")
              (chdir "../build")
              (format #t "build directory: ~s~%" (getcwd))
              (format #t "configure flags: ~s~%" flags)
              (apply invoke bash
                     (string-append srcdir "/configure")
                     flags))))
         (add-before 'configure 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install the '.desktop' file.
             (define (swallow-%%-directives input output)
               ;; Interpret '%%ifdef' directives found in the '.desktop' file.
               (let loop ((state 'top))
                 (match (read-line input 'concat)
                   ((? eof-object?)
                    #t)
                   ((? string? line)
                    (cond ((string-prefix? "%%ifdef" line)
                           (loop 'ifdef))
                          ((string-prefix? "%%else" line)
                           (loop 'else))
                          ((string-prefix? "%%endif" line)
                           (loop 'top))
                          (else
                           (case state
                             ((top else)
                              (display line output)
                              (loop state))
                             (else
                              (loop state)))))))))

             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (call-with-input-file "debian/icecat.desktop.in"
                 (lambda (input)
                   (call-with-output-file "debian/icecat.desktop"
                     (lambda (output)
                       (swallow-%%-directives input output)))))

               (substitute* "debian/icecat.desktop"
                 (("@MOZ_DISPLAY_NAME@")
                  "GNU IceCat")
                 (("^Exec=@MOZ_APP_NAME@")
                  (string-append "Exec=" out "/bin/icecat"))
                 (("@MOZ_APP_NAME@")
                  "icecat"))
               (install-file "debian/icecat.desktop" applications)
               #t)))
         (add-after 'install-desktop-entry 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "browser/branding/official"
                 (for-each
                  (lambda (file)
                    (let* ((size (string-filter char-numeric? file))
                           (icons (string-append out "/share/icons/hicolor/"
                                                 size "x" size "/apps")))
                      (mkdir-p icons)
                      (copy-file file (string-append icons "/icecat.png"))))
                  '("default16.png" "default22.png" "default24.png"
                    "default32.png" "default48.png" "content/icon64.png"
                    "mozicon128.png" "default256.png"))
                 #t))))
         ;; This fixes the file chooser crash that happens with GTK 3.
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share")))
               (wrap-program (car (find-files lib "^icecat$"))
                 `("XDG_DATA_DIRS" ":" prefix (,gtk-share)))
               #t))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

(define-public myconkeror
  (package
    (name "myconkeror")
    (version "0864afd1b830d5485d6b9d99eec6f185bc6bd144")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://repo.or.cz/conkeror.git/snapshot/"
                              version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12m6iyfnii3hmkcnhsclli9mj9nblv0xn2p5dl6mv1mp0p9ck9pv"))))
                ;; "0jz216mjwis7f03j98s4wkcrrq2j3f41fb2y47a5qszc340zhdzv"
    (build-system gnu-build-system)
    (inputs `(("myicecat" ,myicecat)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags `("CC=gcc"
                      ,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after
          'install 'install-app-launcher
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; This overwrites the installed launcher, which execs xulrunner,
            ;; with one that execs 'icecat --app'
            (let* ((out      (assoc-ref outputs "out"))
                   (datadir  (string-append out "/share/conkeror"))
                   (launcher (string-append out "/bin/conkeror")))
              (call-with-output-file launcher
                (lambda (p)
                  (format p "#!~a/bin/bash
exec ~a/bin/icecat --app ~a \"$@\"~%"
                          (assoc-ref inputs "bash") ;implicit input
                          (assoc-ref inputs "icecat")
                          (string-append datadir
                                         "/application.ini"))))
              (chmod launcher #o555)))))))
    (synopsis "Keyboard focused web browser with Emacs look and feel")
    (description "Conkeror is a highly-programmable web browser based on
Mozilla XULRunner which is the base of all Mozilla products including Firefox.
Conkeror has a sophisticated keyboard system for running commands and
interacting with web page content, modelled after Emacs and Lynx.  It is
self-documenting and extensible with JavaScript.

It comes with builtin support for several Web 2.0 sites like several Google
services (Search, Gmail, Maps, Reader, etc.), Del.icio.us, Reddit, Last.fm and
YouTube.  For easier editing of form fields, it can spawn external editors.")
    (home-page "http://conkeror.org")
    ;; Conkeror is triple licensed.
    (license (list
              ;; MPL 1.1 -- this license is not GPL compatible
              license:gpl2
              license:lgpl2.1))))
