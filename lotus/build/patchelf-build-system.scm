;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (ice-9 match)
  #:export (%standard-phases
            patchelf-build))

;; Commentary:
;;
;; Builder-side code of the standard patchelf build procedure.
;;
;; Code:

(define* (configure #:key outputs (configure-flags '()) (out-of-source? #t)
                    build-type target
                    #:allow-other-keys)
  "Configure the given package."
  (let* ((out        (assoc-ref outputs "out"))
         (abs-srcdir (getcwd))
         (srcdir     (if out-of-source?
                         (string-append "../" (basename abs-srcdir))
                         ".")))
    (format #t "source directory: ~s (relative from build: ~s)~%"
            abs-srcdir srcdir)
    (when out-of-source?
      (mkdir "../build")
      (chdir "../build"))
    (format #t "build directory: ~s~%" (getcwd))

    (let ((args `(,srcdir
                  ,@(if build-type
                        (list (string-append "-DPATCHELF_BUILD_TYPE="
                                             build-type))
                        '())
                  ,(string-append "-DPATCHELF_INSTALL_PREFIX=" out)
                  ;; ensure that the libraries are installed into /lib
                  "-DPATCHELF_INSTALL_LIBDIR=lib"
                  ;; add input libraries to rpath
                  "-DPATCHELF_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                  ;; add (other) libraries of the project itself to rpath
                  ,(string-append "-DPATCHELF_INSTALL_RPATH=" out "/lib")
                  ;; enable verbose output from builds
                  "-DPATCHELF_VERBOSE_MAKEFILE=ON"

                  ;;  Cross-build
                  ,@(if target
                        (list (string-append "-DPATCHELF_C_COMPILER="
                                             target "-gcc")
                              (if (string-contains target "mingw")
                                  "-DPATCHELF_SYSTEM_NAME=Windows"
                                  "-DPATCHELF_SYSTEM_NAME=Linux"))
                        '())
                  ,@configure-flags)))
      (format #t "running 'patchelf' with arguments ~s~%" args)
      (apply invoke "patchelf" args))))

(define* (check #:key (tests? #t) (parallel-tests? #t) (test-target "test")
                #:allow-other-keys)
  (let ((gnu-check (assoc-ref gnu:%standard-phases 'check)))
    ;; (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
    (gnu-check #:tests? tests? #:test-target test-target
              #:parallel-tests? parallel-tests?)))

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; and 'check' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'check check)
    (replace 'configure configure)))

(define* (patchelf-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; patchelf-build-system.scm ends here
