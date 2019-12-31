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
  #:use-module ((guix build gnu-build-system)      #:prefix gnu:)
  #:use-module ((lotus build patchelf-build-system) #:prefix patchelf:)
  #:use-module (guix build utils)
  ;; #:use-module (gnu packages bootstrap)
  ;; #:use-module (lotus build deb-utils)
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
;; Builder-side code of the build procedure for ELPA Deb packages.
;;
;; Code:

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
                  (when (string-prefix? "data/control" file)
                    (format #t "matched unpacking ~a~%" file)
                    (gnu:unpack #:source file)))
                files)
      ;; (chdir cwd)
      ;; (copy-recursively "debdata")
      #t)))

;; (define* (build #:key outputs inputs (output-libs '()) #:allow-other-keys)
;;   "Compile .el files."
;;   (define source (getcwd))
;;   (let* ((output-libs    output-libs)
;;          (ld-so          (string-append (assoc-ref inputs "libc") "/lib/ld-linux-x86-64.so.2"))
;;         ;; ((ld-so (string-append (assoc-ref inputs "libc") (glibc-dynamic-linker))))
;;          (host-inputs    (filter (lambda (in)
;;                                    (not (member (car in) '("source" "deb"))))
;;                                  inputs))
;;          (in-rpath       (string-join (map (lambda (in)
;;                                              (string-append in "/lib"))
;;                                            (map cdr (append outputs host-inputs)))
;;                                    ":"))
;;          (out-rpath      (string-join (map (lambda (lib)
;;                                              (string-append (assoc-ref outputs "out") lib))
;;                                            output-libs)
;;                                       ":"))
;;          (rpath          (string-join (list in-rpath out-rpath) ":"))
;;          (files-to-build (find-files source)))
;;     (format #t "output-libs ~a~%" output-libs)
;;     (cond
;;        ((not (null? files-to-build))
;;         (for-each
;;          (lambda (file)
;;            (let ((stat (stat file)))
;;              ;; (format #t "build:~%outputs ~a~%inputs ~a~%"
;;              ;;         (length outputs)
;;              ;;         (length inputs))
;;              ;; (for-each (lambda (e) (format #t " ~a~%" e)) outputs)
;;              ;; (for-each (lambda (e) (format #t " ~a~%" e)) inputs)
;;              ;; (format #t "build:~%outputs~%~{ ~a~%}~%inputs~%~{ ~a~%}~%" outputs inputs)
;;              (format #t "build `~a'~%" file)
;;              (when (or (elf-binary-file? file)
;;                        (library-file?    file))
;;                (make-file-writable file)
;;                (format #t "build: `~a' is a elf binary or library file~%" file)
;;                (invoke "deb" "--set-rpath" rpath file)
;;                (when (and (not (library-file? file))
;;                           (elf-binary-file? file))
;;                  (format #t "build: `~a' is a elf binary file~%" file)
;;                  (invoke "deb" "--set-interpreter" ld-so file))
;;                (chmod file (stat:perms stat)))))
;;          files-to-build)
;;         #t)
;;        (else
;;         (format #t "error: No files found to build.\n")
;;         (find-files source)
;;         #f))))

;;; All the packages are installed directly under site-lisp, which means that
;;; having that directory in the DEBLOADPATH is enough to have them found by
;;; Deb.

;; (define* (install #:key outputs
;;                   #:allow-other-keys)
;;   "Install the package contents."
;;
;;   (define source (getcwd))
;;
;;   (define* (install-file? file stat #:key verbose?)
;;     file)
;;
;;   (let* ((out (assoc-ref outputs "out"))
;;          (files-to-install (find-files source install-file?)))
;;     (cond
;;      ((not (null? files-to-install))
;;       (for-each
;;        (lambda (file)
;;          (let* ((type          (stat:type (lstat file)))
;;                 (stripped-file (string-drop file (string-length source)))
;;                 (target-file   (string-append out stripped-file)))
;;            (if (eq? type 'symlink)
;;                (begin
;;                  (mkdir-p (dirname target-file))
;;                  (system* "cp" "-a" file target-file))
;;                (install-file file (dirname target-file)))))
;;        files-to-install)
;;       #t)
;;      (else
;;       (format #t "error: No files found to install.\n")
;;       (find-files source (lambda (file stat)
;;                            (install-file? file stat #:verbose? #t)))
;;       #f))))

(define %standard-phases
  (modify-phases patchelf:%standard-phases
    ;; (replace 'unpack unpack)
    ;; (add-after 'unpack 'add-source-to-load-path add-source-to-load-path)
    (replace 'unpack unpack)
    (delete  'bootstrap)
    (delete  'configure)
    ;; (replace 'build build)
    (delete  'check)))
    ;; (replace 'install install)

    ;; (add-after 'install 'make-autoloads make-autoloads)
    ;; (add-after 'make-autoloads 'patch-el-files patch-el-files)
    ;; ;; The .el files are byte compiled directly in the store.
    ;; (add-after 'patch-el-files 'build build)
    ;; (add-after 'build 'move-doc move-doc)

(define* (deb-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Deb package, applying all of PHASES in order."
  (apply patchelf:patchelf-build
         #:inputs inputs #:phases phases
         args))

;;; deb-build-system.scm ends here
