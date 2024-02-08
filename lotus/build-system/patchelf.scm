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

(define-module (lotus build-system patchelf)
  #:use-module ((lotus build patchelf-build-system))
  #:use-module ((lotus build patchelf-utils))
  ;; #:select (%default-include %default-exclude)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix memoization)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)

  #:use-module (ice-9 format)

  #:use-module (guix build gnu-build-system)
  #:export (%patchelf-build-system-modules
            patchelf-build
            patchelf-build-system))
;; #:re-export (%default-include         ;for convenience
;;              %default-exclude)

;; Commentary:
;;
;; Standard build procedure for Patchelf packages.  This is implemented as an
;; extension of 'gnu-build-system'.
;;
;; Code:

(define %patchelf-build-system-modules
  ;; Build-side modules imported by default.
  `((lotus build patchelf-build-system)
    (lotus build patchelf-utils)
    ,@%gnu-build-system-modules))

(define %default-modules
  ;; Modules in scope in the build-side environment.
  '((lotus build patchelf-build-system)
    (guix build utils)
    (lotus build patchelf-utils)))

;; (define %default-modules
;;   ;; Modules in scope in the build-side environment.
;;   '((guix build gnu-build-system)
;;     (guix build utils)))

(define (default-patchelf)
  "Return the default Patchelf package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((patchelf-mod (resolve-interface '(gnu packages elf))))
    (module-ref patchelf-mod 'patchelf)))

(define (default-pkg-config)
  "Return the default Pkg-Config package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((pkg-config-mod (resolve-interface '(gnu packages pkg-config))))
    (module-ref pkg-config-mod '%pkg-config)))

(define* (lower name
                #:key source inputs native-inputs outputs target
                (implicit-inputs? #t) (implicit-cross-inputs? #t)
                (strip-binaries? #t) system
                (patchelf (default-patchelf))
                (pkg-config (default-pkg-config))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    `(#:inputs #:native-inputs #:outputs
      #:implicit-inputs? #:implicit-cross-inputs?
      ,@(if target '() '(#:target))))
  ;; (and (not target)) ;XXX: no cross-compilation
  (bag
    (name name)
    (system system) (target target)
    (build-inputs `(("patchelf"   ,patchelf)
                    ("pkg-config" ,pkg-config)

                    ,@(if source
                          `(("source" ,source))
                          '())
                    ,@native-inputs

                    ;; When not cross-compiling, ensure implicit inputs come
                    ;; last.  That way, libc headers come last, which allows
                    ;; #include_next to work correctly; see
                    ;; <https://bugs.gnu.org/30756>.
                    ,@(if target '() inputs)
                    ,@(if (and target implicit-cross-inputs?)
                          (standard-cross-packages target 'host)
                          '())
                    ,@(if implicit-inputs?
                          (standard-packages)
                          '())))
    (host-inputs (if target inputs '()))

    ;; older used
    ;; (host-inputs `(,@(if source
    ;;                      `(("source" ,source))
    ;;                      '())
    ;;                ,@inputs
    ;;                ;; Keep the standard inputs of 'gnu-build-system'.
    ;;                ,@(standard-packages)))

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    (target-inputs (if (and target implicit-cross-inputs?)
                       (standard-cross-packages target 'target)
                       '()))
    ;; (outputs (if strip-binaries?
    ;;              outputs
    ;;              (delete "debug" outputs)))
    (outputs outputs)
    ;; (build (if target gnu-cross-build gnu-build))
    (build patchelf-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define %license-file-regexp
  ;; Regexp matching license files.
  "^(COPYING.*|LICEN[CS]E.*|[Ll]icen[cs]e.*|Copy[Rr]ight(\\.(txt|md))?)$")

(define %bootstrap-scripts
  ;; Typical names of Autotools "bootstrap" scripts.
  #~%bootstrap-scripts)

(define %strip-flags
  #~'("--strip-unneeded" "--enable-deterministic-archives"))

(define %strip-directories
  #~'("lib" "lib64" "libexec" "bin" "sbin"))

(define* (patchelf-build name inputs
                         #:key
                         ;; (guile #f)
                         guile source
                         (outputs '("out"))
                         (search-paths '())
                         ;; (bootstrap-scripts %bootstrap-scripts)
                         (configure-flags ''())
                         (make-flags ''())
                         (out-of-source? #f)
                         (tests? #f)
                         (test-target "check")
                         (parallel-build? #t)
                         (parallel-tests? #t)
                         (patch-shebangs? #t)
                         (strip-binaries? #t)
                         (strip-flags %strip-flags)
                         (strip-directories %strip-directories)
                         (validate-runpath? #t)
                         (make-dynamic-linker-cache? #t)
                         (license-file-regexp %license-file-regexp)
                         ;; (phases '%standard-phases)
                         (phases '(@ (lotus build patchelf-build-system)
                                     %standard-phases))
                         (input-lib-mapping ''())
                         (readonly-binaries ''())
                         (locale "en_US.utf8")
                         (system (%current-system))
                         (build (nix-system->gnu-triplet system))
                         ;; (imported-modules %gnu-build-system-modules)
                         (imported-modules %patchelf-build-system-modules)
                         (modules %default-modules)
                         ;; (modules %patchelf-build-system-modules)
                         ;; (modules '((lotus build patchelf-build-system)
                         ;;            (guix build utils)
                         ;;            (lotus build patchelf-utils)))
                         ;; (modules '())
                         (substitutable? #t)
                         allowed-references
                         disallowed-references)
  "Build SOURCE using PATCHELF, and with INPUTS."


  ;; https://git.savannah.gnu.org/cgit/guix.git/tree/guix/build-system/gnu.scm?h=master#n343

  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(patchelf-build #:source #+source
                                ;; last used
                                ;; #:source ,(match (assoc-ref inputs "source")
                                ;;             (((? derivation? source))
                                ;;              (derivation->output-path source))
                                ;;             ((source)
                                ;;              source)
                                ;;             (source
                                ;;              source))
                                #:system #$system
                                #:build #$build
                                #:outputs %outputs
                                #:inputs %build-inputs
                                #:search-paths '#$(sexp->gexp
                                                   (map search-path-specification->sexp
                                                        search-paths))
                                #:phases #$(if (pair? phases)
                                               (sexp->gexp phases)
                                               phases)
                                #:input-lib-mapping #$input-lib-mapping
                                #:readonly-binaries #$readonly-binaries
                                #:locale #$locale
                                ;; #:bootstrap-scripts #$bootstrap-scripts
                                #:configure-flags #$(if (pair? configure-flags)
                                                        (sexp->gexp configure-flags)
                                                        configure-flags)
                                #:make-flags #$(if (pair? make-flags)
                                                   (sexp->gexp make-flags)
                                                   make-flags)
                                #:out-of-source? #$out-of-source?
                                #:tests? #$tests?
                                #:test-target #$test-target
                                #:parallel-build? #$parallel-build?
                                #:parallel-tests? #$parallel-tests?
                                #:patch-shebangs? #$patch-shebangs?
                                #:license-file-regexp #$license-file-regexp
                                #:strip-binaries? #$strip-binaries?
                                #:validate-runpath? #$validate-runpath?
                                #:make-dynamic-linker-cache? #$make-dynamic-linker-cache?
                                #:license-file-regexp #$license-file-regexp
                                #:strip-flags #$strip-flags
                                #:strip-directories #$strip-directories)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    ;; Note: Always pass #:graft? #f.  Without it, ALLOWED-REFERENCES &
    ;; co. would be interpreted as referring to grafted packages.
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))

(define patchelf-build-system
  (build-system
    (name 'patchelf)
    (description "The build system for Patchelf packages")
    (lower lower)))

;;; patchelf.scm ends here
