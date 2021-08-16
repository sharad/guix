
(define-module (lotus build patchelf-utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-26)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:export (library-file?
            elf-binary-file?
            elf-pie-file?
            elf-aslr-file?
            elf-file-dynamic?
            regular-file?
            directory?
            directory-list-files
            file-info))
            ;; patchelf-dynamic-linker


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

;; (define* (patchelf-dynamic-linker
;;           #:optional (system (or (and=> (%current-target-system)
;;                                         gnu-triplet->nix-system)
;;                                  (%current-system))))
;;   (use-modules (gnu packages bootstrap))
;;   (glibc-dynamic-linker system))

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
