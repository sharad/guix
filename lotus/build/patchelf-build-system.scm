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
  ;; #:use-module (gnu packages bootstrap)
  ;; #:use-module (gnu packages xbootstrap)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            patchelf-build))


;; Commentary:
;;
;; Builder-side code of the build procedure for ELF binaries packages.
;;
;; Code:

;; Utils


(define (find-rpath-libs inputs
                         outputs
                         input-lib-mapping)

  (define (pkg-config-libs input)
    (define %pkg-config
      (make-parameter "pkg-config"))
    (define %not-space
      (char-set-complement (char-set #\Space)))
    ;; TODO: (car input) "gcc:lib" ??
    ;; (car input) is simple user defined name
    (let* ((p (open-pipe* OPEN_READ (%pkg-config) "--libs-only-L" (car input)))
           (l (read-line p)))
      (if (or (not (zero? (close-pipe p)))
              (eof-object? l))
          '()
          (begin
            (let* ((slist (string-tokenize l %not-space)))
              (map (lambda (lib)
                     (if (string-prefix? "-L" lib)
                         (string-drop lib (string-length "-L"))
                         lib))
                   slist))))))
  (define (find-lib input mapping)
    (let ((pkg      (car input))
          (pkg-path (cdr input)))
      (let ((filtered-libs (apply append (map cdr
                                            (filter (lambda (m) (equal? pkg (car m)))
                                                    mapping)))))
        (map (lambda (lib) (string-append pkg-path "/" lib))
             (if (> (length filtered-libs) 0)
                 filtered-libs
                 '("lib"))))))

  (let ((host-inputs (filter (lambda (input)
                               (not (member (car input) '("source" "patchelf"))))
                             inputs)))
    (apply append
           (map (lambda (input)
                  (let ((plibs (pkg-config-libs input)))
                    (if (> (length plibs) 0)
                        plibs
                        (find-lib input input-lib-mapping))))
                (append host-inputs
                        outputs)))))
;; Utils Ends

(define* (build #:key
                outputs
                inputs
                (input-lib-mapping '())
                (readonly-binaries #f)
                #:allow-other-keys)
  "Patch elf files."

  (define source (getcwd))

  (define (patch-library file rpath)
    (let ((stat (stat file)))
      (when (and (library-file?    file)
                 (not (elf-binary-file? file)))
        (format #t "build: `~a' is an elf binary or library file~%" file)
        (make-file-writable file)
        (invoke "patchelf" "--set-rpath" rpath file)
        (chmod file (stat:perms stat)))))

  (define (patch-elf-binary file rpath loader)
    (let ((stat (stat file)))
      (when (and (not (library-file?    file))
                 (elf-binary-file? file))
         (make-file-writable file)
         (invoke "patchelf" "--set-rpath" rpath file)
         (invoke "patchelf" "--set-interpreter" loader file)
         (chmod file (stat:perms stat)))))

  (define (patch-file file rpath loader)
    ;; (file-info file)
    (let ((stat (stat file)))
      (format #t "~%build: patching `~a'~%" file)
      (if (or (library-file?    file)
              (elf-binary-file? file))
          (begin
            (patch-library file rpath)
            (unless readonly-binaries
              (patch-elf-binary file rpath loader)))
          (begin
            (format #t "build: file ~a is not an executable or library~%" file)
            (format #t "build: invoke: no action for ~a~%" file)))))

  (let* ((loader         (string-append (assoc-ref inputs "libc") (patchelf-dynamic-linker system)))
         (rpath-libs     (find-rpath-libs inputs outputs input-lib-mapping))
         (rpath          (string-join rpath-libs ":"))
         (files-to-build (find-files source)))
    (format #t "output-libs:~%~{    ~a~%~}~%" rpath-libs)
    (cond
       ((not (null? files-to-build))
        (for-each (lambda (file)
                    (patch-file file rpath loader))
                  files-to-build)
        #t)
       (else (format #t "error: No files found to build.~%")
             #f))))

;;; All the packages are installed directly under site-lisp, which means that
;;; having that directory in the PATCHELFLOADPATH is enough to have them found by
;;; Patchelf.

(define* (install #:key outputs
                  #:allow-other-keys)
  "Install the package contents."

  (define source (getcwd))

  (define* (install-file? file stat #:key verbose?)
    file)

  (let* ((out (assoc-ref outputs "out"))
         (files-to-install (find-files source install-file?)))
    (format #t "instaling in ~a~%" out)
    (cond
     ((not (null? files-to-install))
      (for-each (lambda (file)
                  (let* ((type          (stat:type (lstat file)))
                         (stripped-file (string-drop file (string-length source)))
                         (target-file   (string-append out stripped-file)))
                    (if (eq? type 'symlink)
                        (begin
                          (mkdir-p (dirname target-file))
                          (system* "cp" "-a" file target-file))
                        (install-file file (dirname target-file)))))
                files-to-install)
      #t)
     (else
      (format #t "error: No files found to install.~%")
      (find-files source (lambda (file stat)
                           (install-file? file stat #:verbose? #t)))
      #f))))

;; https://git.savannah.gnu.org/cgit/guix.git/tree/guix/build/python-build-system.scm?h=master#n208
(define* (wrap #:key
               inputs
               outputs
               (input-lib-mapping '())
               (readonly-binaries #f)
               #:allow-other-keys)
  (define (list-of-elf-files dir)
    (find-files dir (lambda (file stat)
                      (and (eq? 'regular (stat:type stat))
                           (not (wrapped-program? file))
                           (elf-binary-file? file)
                           (not (library-file? file))))))
  ;; Do not require "bash" to be present in the package inputs
  ;; even when there is nothing to wrap.
  ;; Also, calculate (sh) only once to prevent some I/O.
  (define %sh (delay (search-input-file inputs "bin/bash")))
  (define (sh) (force %sh))
  (define (loader)
    (string-append (assoc-ref inputs "libc") (patchelf-dynamic-linker system)))
  (when readonly-binaries
    (let* ((rpath-libs (find-rpath-libs inputs outputs input-lib-mapping))
           (rpath      (string-join rpath-libs ":"))
           (var `("LD_LIBRARY_PATH" prefix
                  (,rpath))))
      (for-each (lambda (dir)
                  (format #t "WRAP: DIR = ~a~%" dir)
                  (let ((files (list-of-elf-files dir)))
                    (format #t "WRAP: FILES = ~a~%" files)
                    (for-each (cut wrap-ro-program <>
                                   #:sh (sh)
                                   #:loader (loader)
                                   var)
                              files)))
                (map cdr outputs)))))

(define* (wrap-if-ro #:key
                     inputs
                     outputs
                     (input-lib-mapping '())
                     (readonly-binaries #f)
                     #:allow-other-keys)
  (when readonly-binaries
    (wrap #:inputs inputs
          #:outputs outputs
          #:input-lib-mapping input-lib-mapping
          #:readonly-binaries readonly-binaries)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete  'bootstrap)
    (delete  'configure)
    (replace 'build build)
    (delete  'check)
    (replace 'install install)
    (add-after 'install 'wrap-if-ro wrap-if-ro)))

(define* (patchelf-build #:key
                         (source #f)
                         (outputs #f)
                         (inputs #f)
                         system
                         (phases %standard-phases)
                         #:allow-other-keys
                         #:rest args)
  "Build the given Patchelf package, applying all of PHASES in order."
  (format #t "patchelf-build.1 source = ~a~%" source)
  (format #t "patchelf-build.1 inputs = ~a~%" inputs)
  (format #t "patchelf-build.1 outputs = ~a~%" outputs)
  (format #t "patchelf-build.1 system = ~a~%" system)
  ;; (format #t "patchelf-build.1 inputs = ~a~%" inputs)
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; patchelf-build-system.scm ends here
