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

(define-module (lotus build deb-build-system)
  #:use-module ((guix build gnu-build-system)       #:prefix gnu:)
  #:use-module ((lotus build patchelf-build-system) #:prefix patchelf:)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            deb-build))


;; Commentary:
;;
;; Builder-side code of the build procedure for Deb packages.
;;
;; Code:

(define gnu:unpack (assoc-ref gnu:%standard-phases 'unpack))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack SOURCE into the build directory.  SOURCE may be a compressed
archive, a directory, or an Emacs Lisp file."
  (let ((cwd (getcwd)))
    (mkdir "data")
    (chdir "data")
    (invoke "ar" "xv" source)
    (chdir cwd)
    (mkdir "debdata")
    (let* ((files (find-files "data")))
      (chdir "debdata")
      (for-each (lambda (file)
                  (format #t "checking ~a~%" file)
                  (when (string-prefix? "data/data" file)
                    (format #t "matched unpacking ~a~%" file)
                    (gnu:unpack #:source (string-append cwd "/" file))))
                files)
      (chdir cwd)
      (if (access? "debdata/usr" F_OK)
          (copy-recursively "debdata/usr" "source")
          (format #t "debdata/usr not exists."))
      (if (access? "debdata/opt" F_OK)
          (copy-recursively "debdata/opt" "source")
          (format #t "debdata/opt not exists."))
      (chdir "source")
      #t)))


(define %standard-phases
  (modify-phases patchelf:%standard-phases
    (replace 'unpack unpack)
    (delete  'bootstrap)
    (delete  'configure)
    (delete  'check)))

(define* (deb-build #:key (source #f) (outputs #f) (inputs #f)
                    (phases %standard-phases)
                    #:allow-other-keys
                    #:rest args)
  "Build the given Deb package, applying all of PHASES in order."
  (apply patchelf:patchelf-build
         #:inputs inputs #:phases phases
         args))

;;; deb-build-system.scm ends here
