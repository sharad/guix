

(define-module (lotus packages lisp-xyz)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-check)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lisp-xyz))



;; (define-public sbcl-clx
;;   (let ((commit "52f457f0ba278e51dc0cbb4cab418049e6b32d9c")
;;         (revision "2"))
;;     (package
;;       (name "sbcl-clx")
;;       (version (git-version "0.7.6" revision commit))
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri
;;           (git-reference
;;            (url "https://github.com/sharplispers/clx")
;;            (commit commit)))
;;          (sha256
;;           (base32 "16l0badm7dxwi7x5ynk1scrbrilnxi1nzz79h1v15xi6b41pf65w"))
;;          (file-name (git-file-name "cl-clx" version))))
;;       (build-system asdf-build-system/sbcl)
;;       (native-inputs
;;        (list sbcl-fiasco xorg-server-for-tests))
;;       (arguments
;;        (list #:phases
;;              #~(modify-phases %standard-phases
;;                  (add-before 'check 'prepare-test-environment
;;                    (lambda _
;;                      (system "Xvfb :1 &")
;;                      (setenv "DISPLAY" ":1"))))))
;;       (home-page "https://www.cliki.net/portable-clx")
;;       (synopsis "X11 client library for Common Lisp")
;;       (description "CLX is an X11 client library for Common Lisp.  The code was
;; originally taken from a CMUCL distribution, was modified somewhat in order to
;; make it compile and run under SBCL, then a selection of patches were added
;; from other CLXes around the net.")
;;       (license license:x11))))




