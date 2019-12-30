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

(define-module (lotus build patchelf-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (lotus build patchelf-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            patchelf-build))

;; Commentary:
;;
;; Builder-side code of the build procedure for ELPA Patchelf packages.
;;
;; Code:

;;; All the packages are installed directly under site-lisp, which means that
;;; having that directory in the PATCHELFLOADPATH is enough to have them found by
;;; Patchelf.

(define* (install #:key outputs
                  ;; (include %default-include)
                  ;; (exclude %default-exclude)
                  #:allow-other-keys)
  "Install the package contents."

  (define source (getcwd))

  (define* (install-file? file stat #:key verbose?)
    file)

  (let* ((out (assoc-ref outputs "out"))
         (files-to-install (find-files source install-file?)))
    (cond
     ((not (null? files-to-install))
      (for-each
       (lambda (file)
         (let* ((stripped-file (string-drop file (string-length source)))
                (target-file   (string-append out stripped-file)))
           (format #t "`~a' -> `~a'~%" file target-file)
           (install-file file (dirname target-file))))
       files-to-install)
      #t)
     (else
      (format #t "error: No files found to install.\n")
      (find-files source (lambda (file stat)
                           (install-file? file stat #:verbose? #t)))
      #f))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    ;; (replace 'unpack unpack)
    ;; (add-after 'unpack 'add-source-to-load-path add-source-to-load-path)
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'build)
    (delete 'check)
    (replace 'install install)))
    ;; (add-after 'install 'make-autoloads make-autoloads)
    ;; (add-after 'make-autoloads 'patch-el-files patch-el-files)
    ;; ;; The .el files are byte compiled directly in the store.
    ;; (add-after 'patch-el-files 'build build)
    ;; (add-after 'build 'move-doc move-doc)

(define* (patchelf-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Patchelf package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; patchelf-build-system.scm ends here
