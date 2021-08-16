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

(define* (build #:key
                outputs
                inputs
                (input-lib-mapping '())
                (readonly-binaries '())
                #:allow-other-keys)
  "Patch elf files."

  (define source (getcwd))

  (define %pkg-config
    (make-parameter "pkg-config"))

  (define %not-space
    (char-set-complement (char-set #\Space)))

  (define (pkg-config-libs input)
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

  (define (find-lib-1 input mapping)
    (map (lambda (lib) (string-append (cdr input) "/" lib))
         (or (assoc-ref mapping (car input))
             '("lib"))))

  (define (find-lib input mapping)
    (let ((pkg      (car input))
          (pkg-path (cdr input)))
      (map (lambda (lib) (string-append pkg-path "/" lib))
           (or (map cadr
                    (filter (lambda (x) (equal? pkg (car x)))
                            mapping))
               '("lib")))))

  (define (find-rpath-libs outputs
                           input-lib-mapping)
    (let ((host-inputs (filter (lambda (input)
                                 (not (member (car input) '("source" "patchelf"))))
                               inputs)))
      (format #t "find-rpath-libs:~%")
      (format #t "find-rpath-libs: host-inputs ~a~%" host-inputs)
      (format #t "find-rpath-libs: outputs ~a~%" outputs)
      (format #t "find-rpath-libs: input-lib-mapping ~a~%" input-lib-mapping)
      (format #t "~%~%")
      (format #t "find-rpath-libs: filtered inputs ~a~%" host-inputs)
      (format #t "~%~%")
      (apply append
           (map (lambda (input)
                  (format #t "find-rpath-libs: working on input: ~a~%" input)
                  (let ((plibs (pkg-config-libs input)))
                    (format #t "find-rpath-libs: pkg-config-libs for ~a input: ~a~%" input plibs)
                    (if (> (length plibs) 0)
                        plibs
                        (let ((found-lib-1 (find-lib-1 input input-lib-mapping))
                              (found-lib (find-lib input input-lib-mapping)))
                          (format #t "find-rpath-libs: found-lib-1 ~a~%" found-lib-1)
                          (format #t "find-rpath-libs: found-lib ~a~%" found-lib)
                          found-lib))))
                (append host-inputs outputs)))))

  (format #t "BUILD:~%")
  (let* ((ld-so             (string-append (assoc-ref inputs "libc") "/lib/ld-linux-x86-64.so.2"))
         (rpath-libs        (find-rpath-libs outputs input-lib-mapping))
         (readonly-binaries readonly-binaries)
         (rpath          (string-join rpath-libs ":"))
         (files-to-build (find-files source)))
    (format #t "output-libs:~%~{    ~a~%~}~%" rpath-libs)
    (cond
       ((not (null? files-to-build))
        (for-each (lambda (file)
                    (file-info file)
                    (let ((stat (stat file)))
                      (format #t "~%build: patching `~a'~%" file)
                      (if (or (library-file?    file)
                              (elf-binary-file? file))
                          (begin
                            (make-file-writable file)
                            (format #t "build: `~a' is an elf binary or library file~%" file)
                            (begin
                              (format #t "~%~%")
                              (format #t "build: invoke patchelf --set-rpath ~a ~a~%" rpath file)
                              (format #t "~%~%")
                              (invoke "patchelf" "--set-rpath" rpath file))
                            (if (library-file? file)
                                (format #t "build: file ~a is not an elf binary, it is a library" file)
                                (begin
                                  (format #t "build: `~a' is not a library file~%" file)
                                  (begin
                                    (format #t "build: `~a' is an elf binary file~%" file)
                                    (format #t "~%~%")
                                    (format #t "build: invoke: patchelf --set-interpreter ~a ~a~%" ld-so file)
                                    (format #t "~%~%")
                                    (invoke "patchelf" "--set-interpreter" ld-so file))))
                            (chmod file (stat:perms stat)))
                          (begin
                            (format #t "build: file ~a is not an executable or library~%" file)
                            (format #t "build: invoke: no action for ~a~%" file)))))
                  files-to-build)
        #t)
       (else
        (format #t "error: No files found to build.\n")
        files-to-build
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
    (format #t "instaling in ~a%" out)
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
      (format #t "error: No files found to install.\n")
      (find-files source (lambda (file stat)
                           (install-file? file stat #:verbose? #t)))
      #f))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete  'bootstrap)
    (delete  'configure)
    (replace 'build build)
    (delete  'check)
    (replace 'install install)))

(define* (patchelf-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Patchelf package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; patchelf-build-system.scm ends here
