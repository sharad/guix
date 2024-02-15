
(define-module (lotus packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto))

(define-public python-colorama-043
  (package (inherit python-colorama)
           (version "0.4.3")
           (source
            (origin (method url-fetch)
                    (uri (pypi-uri "colorama" version))
                    (sha256 (base32 "189n8hpijy14jfan4ha9f5n06mnl33cxz7ay92wjqgkr639s0vg9"))))))

;; https://files.pythonhosted.org/packages/source/P/PyYAML/PyYAML-5.2.tar.gz
(define-public python-pyyaml-52
  (package (inherit python-pyyaml)
           (name "python-pyyaml")
           (version "5.2")
           (source (origin (method url-fetch)
                           (uri (pypi-uri "PyYAML" version))
                           (sha256 (base32 "0v1lwxbn0x2s6lbj7iqbn4gpy6rxf17jqvpcqb1jjbaq5k58xvn0"))))))

(define-public python-exifread
  (package
    (name "python-exifread")
    (version "2.1.2")
    (source
     (origin (method url-fetch)
             (uri (pypi-uri "ExifRead" version))
             (sha256 (base32 "1b90jf6m9vxh9nanhpyvqdq7hmfx5iggw1l8kq10jrs6xgr49qkr"))))
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
    (source (origin (method url-fetch)
                    (uri (pypi-uri "organize-tool" version))
                    (sha256 (base32 "15yyh3ycb1f7q5ig7gq5ppwaf1wpx5dr906w80rfzlkh0z8byvh3"))))
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

(define-public python-xq
  (package
    (name "python-xq")
    (version "0.0.4")
    (source (origin (method url-fetch)
                    (uri (pypi-uri "xq" version))
                    (sha256 (base32 "0xr9v3nn4hhkldx6r2hxkyfddx0j6z2v220fmnl14h2dc5f4smr8"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-lxml" ,python-lxml)
        ("python-pygments" ,python-pygments)))
    (arguments '(#:phases
                 (modify-phases %standard-phases
                                (delete 'check))))
    (home-page "https://github.com/jeffbr13/xq")
    (synopsis "Like jq but for XML and XPath.")
    (description "Like jq but for XML and XPath.")
    (license #f)))

(define-public python-pdfminer
  (package
    (name "python-pdfminer")
    (version "20191125")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pdfminer" version))
        (sha256
          (base32
            "00fwankn96xms8fyjm4f36282qr98pfw2hv3jg4da3ih673hnw4y"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pycryptodome" ,python-pycryptodome)))
    (home-page "http://github.com/euske/pdfminer")
    (synopsis "PDF parser and analyzer")
    (description "PDF parser and analyzer")
    (license license:expat)))

(define-public python-ordereddict
  (package
    (name "python-ordereddict")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ordereddict" version))
        (sha256
          (base32
            "07qvy11nvgxpzarrni3wrww3vpc9yafgi2bch4j2vvvc42nb8d8w"))))
    (build-system python-build-system)
    (home-page "UNKNOWN")
    (synopsis
      "A drop-in substitute for Py2.7's new collections.OrderedDict that works in Python 2.4-2.6.")
    (description
      "A drop-in substitute for Py2.7's new collections.OrderedDict that works in Python 2.4-2.6.")
    (license #f)))

(define-public python-xmlplain
  (package
    (name "python-xmlplain")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xmlplain" version))
        (sha256
          (base32
            "1qyqfpbsl961p30zri6kp8jpdbmp04jk2n04b2qg2kbfnf5gmk59"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-ordereddict" ,python-ordereddict)
        ("python-pyyaml" ,python-pyyaml)))
    (home-page "https://github.com/guillon/xmlplain")
    (synopsis "XML as plain object module")
    (description "XML as plain object module")
    (license #f)))


(define-public python-i3ipc
  (package
    (name "python-i3ipc")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "i3ipc" version))
        (sha256
          (base32
            "1s6crkdn7q8wmzl5d0pb6rdkhhbvp444yxilrgaylnbr2kbxg078"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six"  ,python-six)
       ("python-xlib" ,python-xlib)))
    (arguments
     '(#:tests? #f))
    (home-page
      "https://github.com/altdesktop/i3ipc-python")
    (synopsis
      "An improved Python library to control i3wm and sway")
    (description
      "An improved Python library to control i3wm and sway")
    (license license:bsd-3)))

(define-public python-rofi
  (package
    (name "python-rofi")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-rofi" version))
        (sha256
          (base32
            "0qbsg7x7qcqrm2b771z8r6f86v3zkafk49yg35xq1lgwl73vimpj"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/bcbnz/python-rofi")
    (synopsis
      "Create simple GUIs using the Rofi application")
    (description
      "Create simple GUIs using the Rofi application")
    (license license:expat)))

(define-public python-rofi-menu
  (package
    (name "python-rofi-menu")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rofi-menu" version))
       (sha256
        (base32
         "102iblj3niqv0l9mq5lb0masph9jgjkygf2dg6skldq4a6b7wwdb"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/miphreal/python-rofi-menu")
    (synopsis "Create rofi menus via python")
    (description "Create rofi menus via python")
    (license license:expat)))

(define-public python-rofi-tmux
  (package
    (name "python-rofi-tmux")
    (version "0.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rofi-tmux" version))
        (sha256
          (base32 "19k8dhnzyvdb6maqyb6bx611kf6h8q2n25zjyr59sgnmi7v8y423"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click"   ,python-click)
       ("python-i3ipc"   ,python-i3ipc)
       ("python-libtmux" ,python-libtmux)
       ("python-rofi"    ,python-rofi)))
    (home-page
      "http://github.com/viniarck/rofi-tmux")
    (synopsis
      "Quickly manages tmux sessions, windows and tmuxinator projects on Rofi")
    (description
      "Quickly manages tmux sessions, windows and tmuxinator projects on Rofi")
    (license license:expat)))

(define-public python-tinydb
  (package
   (name "python-tinydb")
   (version "4.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "tinydb" version))
     (sha256
      (base32
       "00m2cq2ra58ygdwd3f3sky9m6c01c8yg6sdfqs1dbrigp847738v"))))
   (build-system python-build-system)
   (home-page "https://github.com/msiemens/tinydb")
   (synopsis
    "TinyDB is a tiny, document oriented database optimized for your happiness :)")
   (description
    "TinyDB is a tiny, document oriented database optimized for your happiness :)")
   (license license:expat)))

(define-public python-attnmgr
  (package
    (name "python-attnmgr")
    (version "2.0")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/sharad/attnmgr")
                          (commit "master")))
                    (file-name (git-file-name name version))
                    (sha256 (base32 "0g59clri4bjw9ar9syw3bb8zf8ch25p73kgb5vz7z902zzj00z6n"))))
    (arguments
     '(#:tests? #f))
    ;; (build-system python-build-system)
    (build-system pyproject-build-system)
    (inputs  (list python-roffi
                   python-tinydb
                   xprop
                   wmctrl))
    (home-page "https://github.com/sharad/attnmgr")
    (synopsis "attnmgr")
    (description "attnmgr")
    (license license:gpl3)))

;; https://files.pythonhosted.org/packages/source/c/camelot-py/camelot-py-0.7.3.tar.gz
;; https://files.pythonhosted.org/packages/source/c/camelot-py/camelot_py-0.7.3-py3-none-any.whl
;; https://files.pythonhosted.org/packages/source/c/camelot-py/camelot_py-0.7.3.whl
;; (define-public python-camelot-py
;;   (package
;;     (name "python-camelot-py")
;;     (version "0.7.3")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri
;;         (string-append "https://files.pythonhosted.org/packages/70/d6/a47894242a6fba58a2332489358afedc6209da43942ab7f850b932019101/camelot_py-" version "-py3-none-any.whl"))
;;        (sha256
;;         (base32
;;          "11jd3m11k2vppgvrs6x55c6p2k57jrdxkyzwl6c209s8i74jisj9"))))
;;     (build-system python-build-system)
;;     (home-page "https://pypi.org/project/camelot-py/#files")
;;     (synopsis
;;      "Read Exif metadata from tiff and jpeg files.")
;;     (description
;;      "Read Exif metadata from tiff and jpeg files.")
;;     (license license:bsd-3)))


(define-public python-speechrecognition
  (package
    (name "python-speechrecognition")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "SpeechRecognition" version))
       (sha256
        (base32 "10lzmlmv4c6i3ldszdhvjwqf3a8jrv5cd8mr0q5f4dkqdf4331vi"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after  'unpack 'compatibility
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (delete-file "speech_recognition/flac-linux-x86")
             (delete-file "speech_recognition/flac-linux-x86_64")
             (delete-file "speech_recognition/flac-mac")
             (delete-file "speech_recognition/flac-win32.exe")
             (substitute* "speech_recognition/audio.py"
               (("os\\.path\\.join.+$")
                (string-append "\"" (assoc-ref inputs "flac") "/bin/flac" "\"\n")))
             #t)))))
    (inputs  (list python-pyaudio
                   flac))
    (propagated-inputs (list python-pyaudio
                             python-requests
                             python-typing-extensions))
    (home-page "https://github.com/Uberi/speech_recognition#readme")
    (synopsis
     "Library for performing speech recognition, with support for several engines and APIs, online and offline.")
    (description
     "Library for performing speech recognition, with support for several engines and
APIs, online and offline.")
    (license license:bsd-3)))

(define-public python-pyttsx3
  (package
   (name "python-pyttsx3")
   (home-page "https://github.com/nateshmbhat/pyttsx3")
   (version "2.90")
   (source (origin (method git-fetch)
                   (uri (git-reference
                         (url home-page)
                         (commit (string-append "v." version))))
                   (file-name (git-file-name name version))
                   (sha256 (base32 "1g0yhf2ph32if0187aj67qvpdpx4gkw8kmmg47c16plg8ihv4r88"))))
   (inputs (list espeak-ng
                 espeak))
   (arguments
    '(#:tests? #f))
   (build-system pyproject-build-system)
   (synopsis "Offline Text To Speech (TTS) converter for Python ")
   (description "pyttsx3 is a text-to-speech conversion library in Python. Unlike alternative libraries, it works offline.")
   (license license:gpl3)))

(define-public python-playsound
  (package
   (name "python-playsound")
   (home-page "https://github.com/TaylorSMarks/playsound")
   (version "1.3.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "playsound" version))
            (sha256 (base32 "1vbw54iv92gvib9yd7552j26pdzla2zv8ssfcbpv0d1hfwfx2vnc"))))
   (inputs (list gst123
                 python-pygobject
                 python-gst))
   (build-system python-build-system)
   (arguments
    '(#:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after  'unpack 'compatibility
                                 (lambda* (#:key inputs outputs #:allow-other-keys)
                                   (format #t "HELLO1")
                                   (system* "pwd")
                                   (system* "ls")
                                   (format #t "HELLO2")
                                   (substitute* "playsound.py"
                                                (("/usr/bin/python3")
                                                 (string-append "\"" (assoc-ref inputs "python") "/bin/python3" "\"")))
                                   #t)))))
   (synopsis
    "Pure Python, cross platform, single function module with no dependencies for playing sounds.")
   (description
    "Pure Python, cross platform, single function module with no dependencies for
playing sounds.")
   (license license:expat)))



