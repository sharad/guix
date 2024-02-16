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

(define-module (lotus packages forti)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((guix build-system cmake) #:prefix cmake:)
  #:use-module ((lotus build-system deb) #:prefix deb:)
  #:use-module ((lotus build-system patchelf) #:prefix patchelf:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gnome))

(define-public deb-forticlient-sslvpn
  (package
    (name "deb-forticlient-sslvpn")
    (version "4.4.2333-1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hadler.me/files/forticlient-sslvpn_" version "_amd64.deb"))
              (file-name (string-append name "-" version ".deb"))
              (sha256 (base32 "0xpq8imbsglsisvfyxj75a9lg3jwxb6n4rnd3zp9mbzk5liad4xg"))))
    (build-system deb:deb-build-system)
    (inputs
     `(("libc"    ,glibc)
       ("gcc:lib" ,gcc "lib")
       ("gtk+-2"  ,gtk+-2)
       ("libsm"   ,libsm)))
    (arguments `(#:input-lib-mapping '(("out" "lib"))
                 #:readonly-binaries #f
                 #:phases            (modify-phases %standard-phases
                                       (delete 'validate-runpath)
                                       (add-after 'unpack 'changedir
                                                  (lambda* (#:key inputs outputs #:allow-other-keys)
                                                    (let* ((source (string-append (getcwd)))
                                                           (share  (string-append source "/share")))
                                                      (mkdir-p share)
                                                      (for-each (lambda (file)
                                                                  (let ((src (string-append source "/" file))
                                                                        (trg (string-append source "/share/" file)))
                                                                    (mkdir-p (dirname trg))
                                                                    (rename-file src trg)))
                                                                (find-files "forticlient-sslvpn"))
                                                      (mkdir-p (string-append source "/bin"))
                                                      (symlink "../share/forticlient-sslvpn/64bit/forticlientsslvpn_cli"
                                                               (string-append source "/bin/forticlientsslvpn_cli"))
                                                      #t))))))
    (synopsis "")
    (description "")
    (home-page "https://www.forticlient.com/repoinfo")
    (license license:ibmpl1.0)))

;; https://www.forticlient.com/repoinfo
;; https://repo.fortinet.com/repo/ubuntu/pool/multiverse/forticlient/forticlient_6.0.8.0140_amd64.deb
;; https://repo.fortinet.com/repo/ubuntu/pool/multiverse/forticlient/forticlient_6.0.8.0140_amd64_u18.deb

;; https://docs.fortinet.com/document/forticlient/7.2.3/linux-release-notes/213138/install-forticlient-linux-from-repo-fortinet-com

;; https://repo.fortinet.com/repo/7.0/ubuntu/pool/multiverse/forticlient/forticlient_7.0.11.0369_amd64.deb
;; https://repo.fortinet.com/repo/forticlient/7.2/ubuntu/pool/multiverse/forticlient/forticlient_7.2.3.0790_amd64.deb
;; https://repo.fortinet.com/repo/forticlient/7.2/debian/pool/non-free/f/forticlient/forticlient_7.2.3.0790_amd64.deb

(define-public deb-forticlient-6.0
  ;; (hidden-package)
  (package
    (name "deb-forticlient-6.0")
    (version "6.0.8.0140")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo.fortinet.com/repo/ubuntu/pool/multiverse/forticlient/forticlient_" version "_amd64_u18.deb"))
              (file-name (string-append name "-" version ".deb"))

              (sha256 (base32 "0gs8rm62hrvwf6j4ia24sa5frglnif0qcr3lvm6n3vgr1nkhyymw"))))
    (build-system deb:deb-build-system)
    (arguments `(#:input-lib-mapping '(("out" "lib"))
                 #:readonly-binaries #t
                 #:phases            (modify-phases %standard-phases
                                       (delete 'validate-runpath))))
    (synopsis "")
    (description "")
    (home-page "https://www.forticlient.com/repoinfo")
    (license license:ibmpl1.0)))

(define-public deb-forticlient-7.0
  (package
    (inherit deb-forticlient-6.0)
    (name "deb-forticlient-7.0")
    (version "7.0.11.0369")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo.fortinet.com/repo/7.0/ubuntu/pool/multiverse/forticlient/forticlient_" version "_amd64.deb"))
              (file-name (string-append name "-" version ".deb"))
              (sha256 (base32 "0gs8rm62hrvwf6j4ia24saf5rglnif0qcr3lvm6n3vgr1nkhyymw"))))))

(define-public deb-forticlient-7.2
  (package
    (inherit deb-forticlient-6.0)
    (name "deb-forticlient-7.2")
    (version "7.2.3.0790")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo.fortinet.com/repo/forticlient/7.2/ubuntu/pool/multiverse/forticlient/forticlient_" version "_amd64.deb"))
              (file-name (string-append name "-" version ".deb"))
              (sha256 (base32 "0gs8rm62hrvwf6j4i2a4sa5frglnif0qcr3lvm6n3vgr1nkhyymw"))))))


(define-public deb-forticlient-non-free-7.2
  (package
    (inherit deb-forticlient-7.2)
    (name "deb-forticlient-non-free-7.2")
    (version "7.2.3.0790")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo.fortinet.com/repo/forticlient/7.2/debian/pool/non-free/f/forticlient/forticlient_" version "_amd64.deb"))
              (file-name (string-append name "-" version ".deb"))
              (sha256 (base32 "0gs8rm62hrvwf6j4ia24sa5fgrlnif0qcr3lvm6n3vgr1nkhyymw"))))))
