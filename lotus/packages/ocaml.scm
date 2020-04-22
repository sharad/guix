
(define-module (more packages ocaml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages))


(package
  (name "ocaml-ocp-indent")
  (version "1.8.1")
  (source
    (origin
      (method url-fetch)
      (uri "https://github.com/OCamlPro/ocp-indent/archive/1.8.1.tar.gz")
      (sha256
        (base32
          "0rqcvwzpwwzwqvrzg1l37jjiglrk2ijgjg1c7vy3xvc8lk9zyysw"))))
  (build-system dune-build-system)
  (propagated-inputs
    `(("ocaml-dune" ,ocaml-dune)
      ("ocaml-cmdliner" ,ocaml-cmdliner)
      ("ocaml-base-bytes" ,ocaml-base-bytes)))
  (home-page
    "http://www.typerex.org/ocp-indent.html")
  (synopsis
    "A simple tool to indent OCaml programs")
  (description
    "Ocp-indent is based on an approximate, tolerant OCaml parser and a simple stack
machine ; this is much faster and more reliable than using regexps. Presets and
configuration options available, with the possibility to set them project-wide.
Supports most common syntax extensions, and extensible for others.

Includes:
- An indentor program, callable from the command-line or from within editors
- Bindings for popular editors
- A library that can be directly used by editor writers, or just for\n  fault-tolerant/approximate parsing.
")
  (license #f))
