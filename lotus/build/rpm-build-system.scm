;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018, 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (lotus build rpm-build-system)
  #:use-module ((guix build gnu-build-system)      #:prefix gnu:)
  #:use-module ((lotus build patchelf-build-system) #:prefix patchelf:)
  #:use-module (guix build utils)
  ;; #:use-module (gnu packages bootstrap)
  ;; #:use-module (lotus build rpm-utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            rpm-build))


;; Commentary:
;;
;; Builder-side code of the build procedure for ELPA Rpm packages.
;;
;; Code:



;;; All the packages are installed directly under site-lisp, which means that
;;; having that directory in the RPMLOADPATH is enough to have them found by
;;; Rpm.


(define gnu:unpack (assoc-ref gnu:%standard-phases 'unpack))

;; https://blog.packagecloud.io/eng/2015/10/13/inspect-extract-contents-rpm-packages/
;; rpm2cpio ./packagecloud-test-1.1-1.x86_64.rpm | cpio -idmv
(define* (unpack #:key source #:allow-other-keys)
  "Unpack SOURCE into the build directory.  SOURCE may be a compressed
archive, a directory, or an Emacs Lisp file."
  (let ((cwd (getcwd)))
    (mkdir "rpmdata")
    (chdir "rpmdata")
    (format #t "invoking rpm2cpio~%")
    (invoke "sh" "-c" (string-join (list "rpm2cpio" source "|" "cpio" "-idmv") " "))
    (format #t "invoked rpm2cpio ~%")
    (chdir cwd)
    (if (access? "rpmdata/usr" F_OK)
        (copy-recursively "rpmdata/usr" "source")
        (format #t "rpmdata/usr not exists."))
    (if (access? "rpmdata/opt" F_OK)
        (copy-recursively "rpmdata/opt" "source")
        (format #t "rpmdata/opt not exists."))
    (chdir "source")
    #t))

(define %standard-phases
  (modify-phases patchelf:%standard-phases
    (replace 'unpack unpack)
    (delete  'bootstrap)
    (delete  'configure)
    (delete  'check)))

(define* (rpm-build #:key (source #f) (outputs #f) (inputs #f)
                    (phases %standard-phases)
                    #:allow-other-keys
                    #:rest args)
  "Build the given Rpm package, applying all of PHASES in order."
  (apply patchelf:patchelf-build
         #:inputs inputs #:phases phases
         args))

;;; rpm-build-system.scm ends here
