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
  ;; #:use-module (gnu packages bootstrap)
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
                #:allow-other-keys)
  "Compile .el files."

  (define source (getcwd))

  (define %pkg-config
    ;; The `pkg-config' command.
    (make-parameter "pkg-config"))

  (define %not-space
    (char-set-complement (char-set #\Space)))

  (define (pkg-config-libs input)
    ;; (format #t "pkg-config-libs: ~a~%" (car input))
    (format #t "pkg-config --libs-only-L ~a~%" (car input))
    (let* ((p (open-pipe* OPEN_READ (%pkg-config) "--libs-only-L" (car input)))
           (l (read-line p)))
      (format #t "pkg-config --libs-only-L ~a  , ~a~%" (car input) l)
      (format #t "pkg-config-libs: ~a~%" l)
      (if (or (not (zero? (close-pipe p)))
              (eof-object? l))
          '()
          (begin
            (let* ((slist (string-tokenize l %not-space))
                   (libs (map (lambda (lib)
                                (if (string-prefix? "-L" lib)
                                    (string-drop lib (string-length "-L"))
                                    lib))
                              slist)))
              (format #t "pkg-config-libs: ~a~%" libs)
              libs)))))

  (define (find-lib input mapping)
    (let* ((mappedlibs (map (lambda (lib) (string-append (cdr input) "/" lib))
                            (or (assoc-ref mapping (car input)) '("lib")))))
      (format #t "input ~a~%" input)
      ;; (format #t "map ~a~%"  map)
      (format #t "mappedlibs ~a~%" mappedlibs)
      mappedlibs))

         ;; ((ld-so (string-append (assoc-ref inputs "libc") (glibc-dynamic-linker))))
  (let* ((ld-so          (string-append (assoc-ref inputs "libc") "/lib/ld-linux-x86-64.so.2"))
         (host-inputs    (filter (lambda (input)
                                   (not (member (car input) '("source" "patchelf"))))
                                 inputs))
         (rpath-libs     (apply append
                                (map (lambda (input)
                                       (let ((plibs (pkg-config-libs input)))
                                         (if (> (length plibs) 0)
                                             plibs
                                             (find-lib input input-lib-mapping))))
                                     (append outputs
                                             host-inputs))))
         (rpath          (string-join rpath-libs ":"))
         (files-to-build (find-files source)))
    (format #t "output-libs:~%~{    ~a~%~}~%" rpath-libs)
    (system* "ls" "-ltr")
    (format #t "build: cwd ~a~%" source)
    (system* "pwd")
    (cond
       ((not (null? files-to-build))
        (for-each (lambda (file)
                    (let ((stat (stat file)))
                      (format #t "build: patching `~a'~%" file)
                      (when (or (elf-binary-file? file)
                                (library-file?    file))
                        (make-file-writable file)
                        (format #t "build: `~a' is a elf binary or library file~%" file)
                        (invoke "patchelf" "--set-rpath" rpath file)
                        (when (and (not (library-file? file))
                                   (elf-binary-file? file))
                          (format #t "build: `~a' is a elf binary file~%" file)
                          (invoke "patchelf" "--set-interpreter" ld-so file))
                        (chmod file (stat:perms stat)))))
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
