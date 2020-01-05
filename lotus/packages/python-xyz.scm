
(define-module (lotus packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages xml)
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

;; https://files.pythonhosted.org/packages/source/c/camelot-py/camelot-py-0.7.3.tar.gz
;; https://files.pythonhosted.org/packages/source/c/camelot-py/camelot_py-0.7.3-py3-none-any.whl
;; https://files.pythonhosted.org/packages/source/c/camelot-py/camelot_py-0.7.3.whl
;; https://files.pythonhosted.org/packages/source/P/PyYAML/PyYAML-5.2.tar.gz
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

(define-public python-yq
  (package
   (name "python-yq")
   (version "2.9.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "yq" version))
     (sha256
      (base32
       "1p8y69p2s4030xiwpsimq6245vllmb4rla3y3qay7wmbpln9q5ls"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-pyyaml" ,python-pyyaml)
      ("python-setuptools" ,python-setuptools)
      ("python-xmltodict" ,python-xmltodict)))
   (native-inputs
    `(("python-coverage" ,python-coverage)
      ("python-flake8" ,python-flake8)
      ("python-wheel" ,python-wheel)))
   (home-page "https://github.com/kislyuk/yq")
   (synopsis
    "Command-line YAML/XML processor - jq wrapper for YAML/XML documents")
   (description
    "Command-line YAML/XML processor - jq wrapper for YAML/XML documents")
   (license #f)))

(define-public python-xq
  (package
    (name "python-xq")
    (version "0.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xq" version))
        (sha256
          (base32
            "0xr9v3nn4hhkldx6r2hxkyfddx0j6z2v220fmnl14h2dc5f4smr8"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-lxml" ,python-lxml)
        ("python-pygments" ,python-pygments)))
    (home-page "https://github.com/jeffbr13/xq")
    (synopsis "Like jq but for XML and XPath.")
    (description "Like jq but for XML and XPath.")
    (license #f)))

(define-public python-camelot-py
  (package
    (name "python-camelot-py")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://files.pythonhosted.org/packages/70/d6/a47894242a6fba58a2332489358afedc6209da43942ab7f850b932019101/camelot_py-" version "-py3-none-any.whl"))
       (sha256
        (base32
         "11jd3m11k2vppgvrs6x55c6p2k57jrdxkyzwl6c209s8i74jisj9"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/camelot-py/#files")
    (synopsis
     "Read Exif metadata from tiff and jpeg files.")
    (description
     "Read Exif metadata from tiff and jpeg files.")
    (license license:bsd-3)))
