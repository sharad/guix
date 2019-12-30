
(define-module (lotus build patchelf-utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-26)
  ;; #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (gnu packages bootstrap)
  ;; #:use-module (guix packages)
  ;; #:use-module (guix download)
  ;; #:use-module (guix build-system gnu)
  ;; #:use-module (guix build-system trivial)
  ;; #:use-module (gnu packages)
  ;; #:use-module (gnu packages bootstrap)
  ;; #:use-module (gnu packages base)
  ;; #:use-module (gnu packages gcc)
  ;; #:use-module (gnu packages compression)
  ;; #:use-module (gnu packages elf)
  ;; #:use-module (gnu packages xorg)
  ;; #:use-module (gnu packages gtk)
  #:export (library-file?
            elf-binary-file?
            regular-file?
            directory?
            directory-list-files
            patchelf-dynamic-linker))

(define (library-file? file)
  (and (eq? 'regular (stat:type (stat file)))
       (string-suffix? ".so" file)))

(define (elf-binary-file? file)
  (and (eq? 'regular (stat:type (stat file)))
       (not (string-suffix? ".so" file))
       (executable-file? file)
       (elf-file? file)))

(define (regular-file? file)
  (and (not (library-file? file))
       (not (elf-binary-file? file))))

(define (directory? file)
  (let ((stat (stat file)))
    (eq? 'directory (stat:type stat))))

(define (directory-list-files dir)
  (scandir dir (negate (cut member <> '("." "..")))))

(define* (patchelf-dynamic-linker
          #:optional (system (or (and=> (%current-target-system)
                                        gnu-triplet->nix-system)
                                 (%current-system))))
  (use-modules (gnu packages bootstrap))
  (glibc-dynamic-linker system))
