;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (lotus build patchelf-utils)
  #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  ;; #:use-module (srfi srfi-34)
  ;; #:use-module (srfi srfi-35)
  ;; #:use-module (srfi srfi-60)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  ;; #:use-module (ice-9 regex)
  ;; #:use-module (ice-9 rdelim)
  ;; #:use-module (ice-9 format)
  ;; #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  ;;   #:use-module (srfi srfi-26)
  ;; #:use-module (guix utils)
  #:use-module (guix build utils)
  ;; #:use-module (srfi srfi-1)
  ;; #:use-module (rnrs bytevectors)
  ;; #:use-module (rnrs io ports)
  ;; #:use-module (gnu packages bootstrap)
  #:export (library-file?
            elf-binary-file?
            elf-pie-file?
            elf-aslr-file?
            elf-file-dynamic?
            regular-file?
            directory?
            directory-list-files
            file-info
            patchelf-dynamic-linker
            wrap-ro-program))


;; (define* (patchelf-dynamic-linker
;;           #:optional (system (or (and=> (%current-target-system)
;;                                         gnu-triplet->nix-system)
;;                                  (%current-system))))
;;   "Return the name of Glibc's dynamic linker for SYSTEM."
;;   ;; See the 'SYSDEP_KNOWN_INTERPRETER_NAMES' cpp macro in libc.
;;   (let ((platform (false-if-platform-not-found
;;                    (lookup-platform-by-system system))))
;;     (cond
;;      ((platform? platform)
;;       (platform-patchelf-dynamic-linker platform))

;;      ;; TODO: Define those as platforms.
;;      ((string=? system "i686-gnu") "/lib/ld.so.1")
;;      ((string=? system "powerpc64-linux") "/lib/ld64.so.1")
;;      ((string=? system "alpha-linux") "/lib/ld-linux.so.2")

;;      ;; TODO: Differentiate between x86_64-linux-gnu and x86_64-linux-gnux32.
;;      ((string=? system "x86_64-linux-gnux32") "/lib/ld-linux-x32.so.2")

;;      ;; XXX: This one is used bare-bones, without a libc, so add a case
;;      ;; here just so we can keep going.
;;      ((string=? system "arm-eabi") "no-ld.so")
;;      ((string=? system "avr") "no-ld.so")
;;      ((string=? system "i686-mingw") "no-ld.so")
;;      ((string=? system "or1k-elf") "no-ld.so")
;;      ((string=? system "x86_64-mingw") "no-ld.so")
;;      ((string-suffix? "-elf" system) "no-ld.so")

;;      (else (error "dynamic linker name not known for this system"
;;                   system)))))



(define* (patchelf-dynamic-linker #:optional system)      ;(use-modules (gnu packages bootstrap))
          ;; #:optional (system (or (and=> (%current-target-system)
          ;;                               gnu-triplet->nix-system)
          ;;                        (%current-system)))

  "Return the name of Glibc's dynamic linker for SYSTEM."
  ;; See the 'SYSDEP_KNOWN_INTERPRETER_NAMES' cpp macro in libc.

  "/lib/ld-linux.so.2")



;; https://stackoverflow.com/questions/38189169/elf-pie-aslr-and-everything-in-between-specifically-within-linux
;; TODO
;; (define (file-header-loc-match loc num)
;;   "Return a procedure that returns true when its argument is a file starting
;; with the bytes in HEADER, a bytevector."
;;   (define len
;;     (bytevector-length header))
;;
;;   (lambda (file)
;;     "Return true if FILE starts with the right magic bytes."
;;     (define (get-header)
;;       (call-with-input-file file
;;         (lambda (port)
;;           (get-bytevector-n port len))
;;         #:binary #t #:guess-encoding #f))
;;
;;     (catch 'system-error
;;       (lambda ()
;;         (equal? (get-header) header))
;;       (lambda args
;;         (if (= EISDIR (system-error-errno args))
;;             #f                                    ;FILE is a directory
;;             (apply throw args))))))

(define (patchelf-get-header file len)
  "Return true if FILE starts with the right magic bytes."
  (call-with-input-file file
    (lambda (port)
      (get-bytevector-n port len))
    #:binary #t #:guess-encoding #f))

(define (patchelf-valid-header? header len)
  (and (not (eof-object? header))
       (= 17 (bytevector-length header))))

(define (elf-pie-file? file)
  (let ((header (patchelf-get-header file 17)))
    (and (patchelf-valid-header? header 17)
         (= 3 (last (bytevector->u8-list header))))))

(define (elf-aslr-file? file)
  (let ((header (patchelf-get-header file 17)))
    (and (patchelf-valid-header? header 17)
        (= 2 (last (bytevector->u8-list header))))))

(define (elf-file-dynamic? file)
  (and (or (elf-file? file)
           (elf-pie-file? file)
           (elf-aslr-file? file))
       (zero? (apply system* "sh" (list "-c" (format #f "readelf -x .interp ~a 2>&1 | grep 'Hex dump of section'" file))))))

(define (regular-file? file)
  (and (not (library-file? file))
       (not (elf-binary-file? file))))


(define (library-file? file)
  (and (eq? 'regular (stat:type (stat file)))
       (string-suffix? ".so" file)))

(define (elf-binary-file? file)
  (and (eq? 'regular (stat:type (stat file)))
       (not (string-suffix? ".so" file))
       (executable-file? file)
       (elf-file-dynamic? file)))

(define (directory? file)
  (let ((stat (stat file)))
    (eq? 'directory (stat:type stat))))

(define (directory-list-files dir)
  (scandir dir (negate (cut member <> '("." "..")))))

(define (file-info file)
  (format #t "~%~%")
  (format #t "file-info: ~%")
  (format #t "file-info: ~a~%" file)
  (format #t "file-info: ~a: (stat:type (stat file)) = ~a~%" file (stat:type (stat file)))
  (format #t "file-info: ~a: (string-suffix? \".so\" file) = ~a~%" file (string-suffix? ".so" file))
  (format #t "file-info: ~a: (executable-file? file)= ~a~%" file (executable-file? file))
  (format #t "file-info: ~a: (elf-file? file) = ~a~%" file (elf-file? file))
  (format #t "file-info: ~a: (elf-file-dynamic? file) = ~a~%" file (elf-file-dynamic? file))
  (format #t "file-info: ~a: (elf-binary-file? file) = ~a~%" file (elf-binary-file? file) file)
  (format #t "file-info: ~%")
  (format #t "~%~%"))


(define* (wrap-ro-program prog
                          #:key
                          (sh     (which "bash"))
                          (loader #f)
                          #:rest vars)
  "Make a wrapper for PROG.  VARS should look like this:

  '(VARIABLE DELIMITER POSITION LIST-OF-DIRECTORIES)

where DELIMITER is optional.  ':' will be used if DELIMITER is not given.

For example, this command:

  (wrap-ro-program \"foo\"
                '(\"PATH\" \":\" = (\"/gnu/.../bar/bin\"))
                '(\"CERT_PATH\" suffix (\"/gnu/.../baz/certs\"
                                        \"/qux/certs\")))

will copy 'foo' to '.foo-real' and create the file 'foo' with the following
contents:

  #!location/of/bin/bash
  export PATH=\"/gnu/.../bar/bin\"
  export CERT_PATH=\"$CERT_PATH${CERT_PATH:+:}/gnu/.../baz/certs:/qux/certs\"
  exec -a $0 location/of/lib/ld-loader.so-2 location/of/.foo-real \"$@\"

This is useful for scripts that expect particular programs to be in $PATH, for
programs that expect particular shared libraries to be in $LD_LIBRARY_PATH, or
modules in $GUILE_LOAD_PATH, etc.

If PROG has previously been wrapped by 'wrap-program', the wrapper is extended
with definitions for VARS. If it is not, SH will be used as interpreter."
  (define vars/filtered
    (match vars
      ((#:sh _ #:loader _ . vars) vars)
      ((#:loader _ #:sh _ . vars) vars)
      ((#:sh _ . vars) vars)
      ((#:loader _ . vars) vars)
      (vars vars)))

  (define wrapped-file
    (string-append (dirname prog) "/." (basename prog) "-real"))

  (define already-wrapped?
    (file-exists? wrapped-file))

  (define (last-line port)
    ;; Return the last line read from PORT and leave PORT's cursor right
    ;; before it.
    (let loop ((previous-line-offset 0)
               (previous-line "")
               (position (seek port 0 SEEK_CUR)))
      (match (read-line port 'concat)
        ((? eof-object?)
         (seek port previous-line-offset SEEK_SET)
         previous-line)
        ((? string? line)
         (loop position line (+ (string-length line) position))))))

  (define (export-variable lst)
    ;; Return a string that exports an environment variable.
    (format #t "EXPORT-VARIABLE: lst: ~a" lst)
    (match lst
      ((var sep '= rest)
       (format #f "export ~a=\"~a\""
               var (string-join rest sep)))
      ((var sep 'prefix rest)
       (format #f "export ~a=\"~a${~a:+~a}$~a\""
               var (string-join rest sep) var sep var))
      ((var sep 'suffix rest)
       (format #f "export ~a=\"$~a${~a+~a}~a\""
               var var var sep (string-join rest sep)))
      ((var '= rest)
       (format #f "export ~a=\"~a\""
               var (string-join rest ":")))
      ((var 'prefix rest)
       (format #f "export ~a=\"~a${~a:+:}$~a\""
               var (string-join rest ":") var var))
      ((var 'suffix rest)
       (format #f "export ~a=\"$~a${~a:+:}~a\""
               var var var (string-join rest ":")))))

  (when (wrapped-program? prog)
    (error (string-append prog " is a wrapper. Refusing to wrap.")))

  (if already-wrapped?

      ;; PROG is already a wrapper: add the new "export VAR=VALUE" lines just
      ;; before the last line.
      (let* ((port (open-file prog "r+"))
             (last (last-line port)))
        (for-each (lambda (var)
                    (display (export-variable var) port)
                    (newline port))
                  vars/filtered)
        (display last port)
        (close-port port))

      ;; PROG is not wrapped yet: create a shell script that sets VARS.
      (let ((prog-tmp (string-append wrapped-file "-tmp")))
        (link prog wrapped-file)

        (call-with-output-file prog-tmp
          (lambda (port)
            (format port
                    "#!~a~%~a~%exec -a \"$0\" \"~a\" \"~a\" \"$@\"~%"
                    sh
                    (string-join (map export-variable vars/filtered) "\n")
                    loader
                    (canonicalize-path wrapped-file))))

        (chmod prog-tmp #o755)
        (rename-file prog-tmp prog))))
