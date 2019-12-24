;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (lotus build-system patchelf)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (ice-9 match)
  #:export (%patchelf-build-system-modules
            patchelf-build
            patchelf-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using Patchelf. This is implemented as an
;; extension of `gnu-build-system'.
;;
;; Code:

(define %patchelf-build-system-modules
  ;; Build-side modules imported by default.
  `((lotus build patchelf-build-system)
    ,@%gnu-build-system-modules))

(define (default-patchelf)
  "Return the default Patchelf package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages elf))))
    (module-ref module 'patchelf)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (patchelf (default-patchelf))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:source #:patchelf #:inputs #:native-inputs #:outputs
      ,@(if target '() '(#:target))))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@`(("patchelf" ,patchelf))
                    ,@native-inputs
                    ,@(if target
                          ;; Use the standard cross inputs of
                          ;; 'gnu-build-system'.
                          (standard-cross-packages target 'host)
                          '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs inputs)

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    ;; (target-inputs (if target
    ;;                    (standard-cross-packages target 'target)
    ;;                    '()))
    (target-inputs `(,@(if target
                           (standard-cross-packages target 'target)
                           '())
                     ,@`(("libc" ,glibc))))
    (outputs outputs)
    (build (if target patchelf-cross-build patchelf-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (patchelf-build store name inputs
                         #:key (guile #f)
                         (outputs '("out")) (configure-flags ''())
                         (search-paths '())
                         (make-flags ''())
                         (out-of-source? #t)
                         (build-type "RelWithDebInfo")
                         (tests? #t)
                         (test-target "test")
                         (parallel-build? #t) (parallel-tests? #f)
                         (validate-runpath? #t)
                         (patch-shebangs? #t)
                         (strip-binaries? #t)
                         (strip-flags ''("--strip-debug"))
                         (strip-directories ''("lib" "lib64" "libexec"
                                               "bin" "sbin"))
                         (phases '(@ (lotus build patchelf-build-system)
                                     %standard-phases))
                         (system (%current-system))
                         (imported-modules %patchelf-build-system-modules)
                         (modules '((lotus build patchelf-build-system)
                                    (guix build utils))))
  "Build SOURCE using PATCHELF, and with INPUTS. This assumes that SOURCE
provides a 'PatchelfLists.txt' file as its build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (patchelf-build #:source ,(match (assoc-ref inputs "source")
                                   (((? derivation? source))
                                    (derivation->output-path source))
                                   ((source)
                                    source)
                                   (source
                                    source))
                       #:system ,system
                       #:outputs %outputs
                       #:inputs %build-inputs
                       #:search-paths ',(map search-path-specification->sexp
                                             search-paths)
                       #:phases ,phases
                       #:configure-flags ,configure-flags
                       #:make-flags ,make-flags
                       #:out-of-source? ,out-of-source?
                       #:build-type ,build-type
                       #:tests? ,tests?
                       #:test-target ,test-target
                       #:parallel-build? ,parallel-build?
                       #:parallel-tests? ,parallel-tests?
                       #:validate-runpath? ,validate-runpath?
                       #:patch-shebangs? ,patch-shebangs?
                       #:strip-binaries? ,strip-binaries?
                       #:strip-flags ,strip-flags
                       #:strip-directories ,strip-directories)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:system system
                                #:inputs inputs
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))


;;;
;;; Cross-compilation.
;;;

(define* (patchelf-cross-build store name
                            #:key
                            target native-drvs target-drvs
                            (guile #f)
                            (outputs '("out"))
                            (configure-flags ''())
                            (search-paths '())
                            (native-search-paths '())
                            (make-flags ''())
                            (out-of-source? #t)
                            (build-type "RelWithDebInfo")
                            (tests? #f) ; nothing can be done
                            (test-target "test")
                            (parallel-build? #t) (parallel-tests? #f)
                            (validate-runpath? #t)
                            (patch-shebangs? #t)
                            (strip-binaries? #t)
                            (strip-flags ''("--strip-debug"
                                            "--enable-deterministic-archives"))
                            (strip-directories ''("lib" "lib64" "libexec"
                                                  "bin" "sbin"))
                            (phases '(@ (lotus build patchelf-build-system)
                                        %standard-phases))
                            (system (%current-system))
                            (build (nix-system->gnu-triplet system))
                            (imported-modules %patchelf-build-system-modules)
                            (modules '((lotus build patchelf-build-system)
                                       (guix build utils))))
  "Cross-build NAME using PATCHELF for TARGET, where TARGET is a GNU triplet and
with INPUTS.  This assumes that SOURCE provides a 'PatchelfLists.txt' file as its
build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (let ()
         (define %build-host-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name path)
                     `(,name . ,path)))
                  native-drvs))

         (define %build-target-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name (? package? pkg) sub ...)
                     (let ((drv (package-cross-derivation store pkg
                                                          target system)))
                       `(,name . ,(apply derivation->output-path drv sub))))
                    ((name path)
                     `(,name . ,path)))
                  target-drvs))

         (patchelf-build #:source ,(match (assoc-ref native-drvs "source")
                                  (((? derivation? source))
                                   (derivation->output-path source))
                                  ((source)
                                   source)
                                  (source
                                   source))
                      #:system ,system
                      #:build ,build
                      #:target ,target
                      #:outputs %outputs
                      #:inputs %build-target-inputs
                      #:native-inputs %build-host-inputs
                      #:search-paths ',(map search-path-specification->sexp
                                            search-paths)
                      #:native-search-paths ',(map
                                               search-path-specification->sexp
                                               native-search-paths)
                      #:phases ,phases
                      #:configure-flags ,configure-flags
                      #:make-flags ,make-flags
                      #:out-of-source? ,out-of-source?
                      #:build-type ,build-type
                      #:tests? ,tests?
                      #:test-target ,test-target
                      #:parallel-build? ,parallel-build?
                      #:parallel-tests? ,parallel-tests?
                      #:validate-runpath? ,validate-runpath?
                      #:patch-shebangs? ,patch-shebangs?
                      #:strip-binaries? ,strip-binaries?
                      #:strip-flags ,strip-flags
                      #:strip-directories ,strip-directories))))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                               ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:system system
                                #:inputs (append native-drvs target-drvs)
                                #:outputs outputs
                                #:modules imported-modules
                                #:guile-for-build guile-for-build))

(define patchelf-build-system
  (build-system
    (name 'patchelf)
    (description "The standard Patchelf build system")
    (lower lower)))

;;; patchelf.scm ends here
