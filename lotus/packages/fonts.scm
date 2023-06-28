;;; GNU Guix --- Functional package management for GNU
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

(define-module (lotus packages fonts)
  #:use-module (ice-9 regex)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages c)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg))

;; (define-public font-mangal1
;;   (package
;;     (name "font-mangal1")
;;     (version "1.001")                   ; also update description
;;     (source (origin
;;               (method url-fetch/zipbomb)
;;               (uri (string-append "https://devanagarifonts.net/downloadfiles/zippedfontstyles/m/a/mangal-regular.zip?st=kO_I0qIEeRyKhZ8QxN515Q&e=1687945992"))
;;               (file-name "mangal-regular1.zip")
;;               (sha256
;;                (base32
;;                 "1202cmbc8c7dhx7mkcjcrn37fkyijida57xkddi0vkvd0wz648gw"))))
;;     (build-system font-build-system)
;;     (home-page "https://devanagarifonts.net/fonts/mangal-regular")
;;     (synopsis "Mangal typeface")
;;     (description
;;      "Managal font.")
;;     (license license:silofl1.1)))

;; (define-public font-mangal2
;;   (package
;;     (name "font-mangal2")
;;     (version "1.001")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append "https://devanagarifonts.net/downloadfiles/zippedfontstyles/m/a/mangal-regular.zip?st=kO_I0qIEeRyKhZ8QxN515Q&e=1687945992"))
;;               (file-name "mangal-regular2.zip")
;;               (sha256
;;                (base32
;;                 "1202cmbc8c7dhx7mkcjcrn37fkyijida57xkddi0vkvd0wz648gw"))))
;;     (build-system font-build-system)
;;     (home-page "https://devanagarifonts.net/fonts/mangal-regular")
;;     (synopsis "Mangal typeface")
;;     (description
;;      "Managal font.")
;;     (license license:silofl1.1)))

(define-public font-mangal
  (package
    (name "font-mangal")
    (version "1.001")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://devanagarifonts.net/downloadfiles/zippedfontstyles/m/a/mangal-regular.zip?st=kO_I0qIEeRyKhZ8QxN515Q&e=1687945992"))
              (file-name "mangal-regular2.zip")
              (sha256
               (base32
                "1202cmbc8c7dhx7mkcjcrn37fkyijida57xkddi0vkvd0wz648gw"))))
    (build-system font-build-system)
    (home-page "https://devanagarifonts.net/fonts/mangal-regular")
    (synopsis "Mangal typeface")
    (description
     "Managal font.")
    (license license:silofl1.1)))
