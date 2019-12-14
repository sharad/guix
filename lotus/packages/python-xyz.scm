


(define-module (lotus packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-xyz))


(define-public python-colorama-043
  (package (inherit python-colorama)
           (version "0.4.3")
           (source
            (origin
              (method url-fetch)
              (uri (pypi-uri "colorama" version))
              (sha256
               (base32
                "189n8hpijy14jfan4ha9f5n06mnl33cxz7ay92wjqgkr639s0vg9"))))))

(define-public python-pyyaml-52
  (package (inherit python-pyyaml)
           (name "python-pyyaml")
           (version "5.2")
           (source
            (origin
              (method url-fetch)
              (uri (pypi-uri "PyYAML" version))
              (sha256
               (base32
                "0v1lwxbn0x2s6lbj7iqbn4gpy6rxf17jqvpcqb1jjbaq5k58xvn0"))))))

(define-public python-exifread
  (package
    (name "python-exifread")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ExifRead" version))
       (sha256
        (base32
         "1b90jf6m9vxh9nanhpyvqdq7hmfx5iggw1l8kq10jrs6xgr49qkr"))))
    (build-system python-build-system)
    (home-page "https://github.com/ianare/exif-py")
    (synopsis
     "Read Exif metadata from tiff and jpeg files.")
    (description
     "Read Exif metadata from tiff and jpeg files.")
    (license license:bsd-3)))

(define-public python-organize-tool
  (package
    (name "python-organize-tool")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "organize-tool" version))
       (sha256
        (base32
         "15yyh3ycb1f7q5ig7gq5ppwaf1wpx5dr906w80rfzlkh0z8byvh3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    (inputs
     `(("python-exifread"     ,python-exifread)
       ("python-docopt"       ,python-docopt)
       ("python-appdirs"      ,python-appdirs)
       ("python-send2trash"   ,python-send2trash)
       ("python-pyyaml"       ,python-pyyaml-52)
       ("python-colorama"     ,python-colorama-043)))
    (home-page
     "https://github.com/tfeldmann/organize")
    (synopsis "The file management automation tool")
    (description
     "The file management automation tool")
    (license license:expat)))

;; (define-public python-pdfminer
;;   (package
;;    (name "python-pdfminer")
;;    (version "20191125")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (pypi-uri "pdfminer" version))
;;      (sha256
;;       (base32
;;        "00fwankn96xms8fyjm4f36282qr98pfw2hv3jg4da3ih673hnw4y"))))
;;    (build-system python-build-system)
;;    (propagated-inputs
;;     `(("python-pycryptodome" ,python-pycryptodome)))
;;    (home-page "http://github.com/euske/pdfminer")
;;    (synopsis "PDF parser and analyzer")
;;    (description "PDF parser and analyzer")
;;    (license license:expat)))

;; python-crypto

;; python-pdfminer
