;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (lotus packages bicon)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fribidi))



(define-public bicon
  (package
    (replacement bicon/fixed)
    (name "bicon")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/behdad/bicon/releases/"
                       "/download/" version "/bicon-" version
                       ".tar.gz"))
       (sha256
        (base32 "0n9p5pfv9l2yxgzxqlg259yqi8acw7bp91r5cqxr039w0zyqvy22"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("perl"       ,perl)
                     ("kbd"        ,kbd)))
    (inputs        `(("fribidi"    ,fribidi)))
    (synopsis "Implementation of the Unicode bidirectional algorithm")
    (description
     "GNU Bicon is an implementation of the Unicode Bidirectional
Algorithm.  This algorithm is used to properly display text in left-to-right
or right-to-left ordering as necessary.")
    (home-page "https://github.com/behdad/bicon")
    (license lgpl2.1+)))

(define bicon/fixed
  (package
    (inherit bicon)
    (source
     (origin (inherit (package-source bicon))))))
             ;; (patches (search-patches "bicon-CVE-2019-18397.patch"))



