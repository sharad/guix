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
  #:use-module (ice-9 match)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)

  #:use-module (ice-9 ftw)
  #:use-module (guix build utils)
  ;; #:use-module (gnu packages bootstrap)
  #:use-module (lotus build utils)
  ;; #:use-module (guix packages)
  ;; #:use-module (guix download)
  ;; #:use-module (guix build rpath)
  #:export (%standard-phases
            patchelf-build))

;; Commentary:
;;
;; Builder-side code of the standard patchelf build procedure.
;;
;; Code:

;; (define* (build #:key outputs inputs #:allow-other-keys)
;;   "Compile .el files."
;;   (let* ((emacs (string-append (assoc-ref inputs "emacs") "/bin/emacs"))
;;          (out (assoc-ref outputs "out"))
;;          (site-lisp (string-append out %install-dir)))
;;     (setenv "SHELL" "sh")
;;     (parameterize ((%emacs emacs))
;;       (emacs-byte-compile-directory site-lisp))))

(define* (build #:key outputs inputs #:allow-other-keys)
  "Compile .el files."
  (begin
    (use-modules (ice-9 ftw))
    (use-modules (guix build utils))
    (use-modules (gnu packages bootstrap))
    (use-modules (lotus build utils))

    ;; (use-modules (guix build rpath))
    (let ((ld-so (string-append (assoc-ref inputs "libc") (glibc-dynamic-linker))))
      (file-system-fold (lambda (dir stat result)    ; enter?
                          result)
                        (lambda (file stat result)   ; leaf
                          (let ((stat stat))
                            (when (or (elf-binary-file? file)
                                      (library-file?    file))
                              (make-file-writable file)
                              (let ((rpath (string-join (map (lambda (in) (string-append in "/lib"))
                                                             (cons* outputs (map cdr inputs)))
                                                        ":")))
                                ;; (format #t "file ~s rpath ~s~%" file rpath)
                                ;; (augment-rpath file lib-paths)
                                (invoke "patchelf" "--set-rpath" rpath file))
                              (when (and (not (library-file? file))
                                         (elf-binary-file? file))
                                (invoke "patchelf" "--set-interpreter" ld-so file))
                              (chmod file (stat:perms stat)))))
                        (const #t)                   ; down
                        (lambda (dir stat result)    ; up
                          result)
                        (const #t)                   ; skip
                        (lambda (file stat errno result)
                          (format (current-error-port)
                                  "warning: failed to process ~a: ~a~%"
                                  file (strerror errno)))
                        #t
                        "source"
                        ;; Don't follow symlinks.
                        lstat)
      #t)))

;; (define* (check #:key (tests? #t) (parallel-tests? #t) (test-target "test")
;;                 #:allow-other-keys)
;;   (let ((gnu-check (assoc-ref gnu:%standard-phases 'check)))
;;     ;; (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
;;     (gnu-check #:tests? tests? #:test-target test-target
;;               #:parallel-tests? parallel-tests?)))


(define* (install #:key outputs (make-flags '()) #:allow-other-keys)
  (copy-recursively "source" outputs))


(define %standard-phaseos
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; and 'check' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'check)
    (delete 'configure)
    (replace 'build build)
    (replace 'install install)))

(define* (patchelf-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; patchelf-build-system.scm ends here
