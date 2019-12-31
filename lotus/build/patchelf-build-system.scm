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
  ;; #:use-module (lotus build patchelf-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            patchelf-build
            library-file?
            elf-binary-file?))

;; Commentary:
;;
;; Builder-side code of the build procedure for ELPA Patchelf packages.
;;
;; Code:

(define (library-file? file)
  (and (eq? 'regular (stat:type (stat file)))
       (string-suffix? ".so" file)))

(define (elf-binary-file? file)
  (and (eq? 'regular (stat:type (stat file)))
       (not (string-suffix? ".so" file))
       (executable-file? file)
       (elf-file? file)))

(define* (build #:key outputs inputs (output-libs '()) #:allow-other-keys)
  "Compile .el files."
  (define source (getcwd))
  (let* ((output-libs    output-libs)
         (ld-so          (string-append (assoc-ref inputs "libc") "/lib/ld-linux-x86-64.so.2"))
        ;; ((ld-so (string-append (assoc-ref inputs "libc") (glibc-dynamic-linker))))
         (host-inputs    (filter (lambda (in)
                                   (not (member (car in) '("source" "patchelf"))))
                                 inputs))
         (in-rpath       (string-join (map (lambda (in)
                                             (string-append in "/lib"))
                                           (map cdr (append outputs host-inputs)))
                                   ":"))
         (out-rpath      (string-join (map (lambda (lib)
                                             (string-append (assoc-ref outputs "out") lib))
                                           output-libs)
                                      ":"))
         (rpath          (string-join (list in-rpath out-rpath) ":"))
         (files-to-build (find-files source)))
    (format #t "output-libs ~a~%" output-libs)
    (cond
       ((not (null? files-to-build))
        (for-each
         (lambda (file)
           (let ((stat (stat file)))
             ;; (format #t "build:~%outputs ~a~%inputs ~a~%"
             ;;         (length outputs)
             ;;         (length inputs))
             ;; (for-each (lambda (e) (format #t " ~a~%" e)) outputs)
             ;; (for-each (lambda (e) (format #t " ~a~%" e)) inputs)
             ;; (format #t "build:~%outputs~%~{ ~a~%}~%inputs~%~{ ~a~%}~%" outputs inputs)
             (format #t "build `~a'~%" file)
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
        (find-files source)
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
      (for-each
       (lambda (file)
         (let* ((type          (stat:type (lstat file)))
                (stripped-file (string-drop file (string-length source)))
                (target-file   (string-append out stripped-file)))
           ;; (system* "ls" "-l" file)
           ;; (format #t "`~a' -> `~a' ~a ~a ~%" file target-file type (eq? type 'symlink))
           (if (eq? type 'symlink)
               (system* "cp" "-a" file target-file)
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
    ;; (replace 'unpack unpack)
    ;; (add-after 'unpack 'add-source-to-load-path add-source-to-load-path)
    (delete  'bootstrap)
    (delete  'configure)
    (replace 'build build)
    (delete  'check)
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
