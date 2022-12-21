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

(define-module (lotus build-system rpm)
  #:use-module ((lotus build rpm-build-system))
  ;; #:use-module ((lotus build rpm-utils))
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
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((lotus build-system patchelf) #:prefix patchelf:)
  #:use-module (ice-9 match)

  #:use-module (gnu packages package-management)
  #:use-module (gnu packages cpio)
  #:export (%rpm-build-system-modules
            rpm-build
            rpm-build-system))
;; #:re-export (%default-include         ;for convenience
;;              %default-exclude)

;; Commentary:
;;
;; Standard build procedure for Rpm packages.  This is implemented as an
;; extension of 'gnu-build-system'.
;;
;; Code:

(define %rpm-build-system-modules
  ;; Build-side modules imported by default.
  `((lotus build rpm-build-system)
    ,@patchelf:%patchelf-build-system-modules))


(define (default-patchelf)
  "Return the default Patchelf package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((patchelf-mod (resolve-interface '(gnu packages elf))))
    (module-ref patchelf-mod 'patchelf)))

(define* (lower name
                #:key source inputs native-inputs outputs target
                (implicit-inputs? #t) (implicit-cross-inputs? #t)
                (strip-binaries? #t) system
                (patchelf (default-patchelf))
                ;; (pkg-config (default-pkg-config))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:inputs #:native-inputs #:outputs
      #:implicit-inputs? #:implicit-cross-inputs?
      ,@(if target '() '(#:target)))) ;#:host-inputs


  ;; '(#:target #:rpm #:inputs #:native-inputs)

  ;; (and (not target)                               ;XXX: no cross-compilation
  ;;      (bag
  ;;        (name name)
  ;;        (system system)
  ;;        (host-inputs `(,@(if source
  ;;                             `(("source" ,source))
  ;;                             '())
  ;;                       ,@inputs
  ;;                       ;; Keep the standard inputs of 'gnu-build-system'.
  ;;                       ,@(gnu:standard-packages)))
  ;;        (build-inputs `(("binutils" ,binutils)
  ;;                        ("patchelf" ,patchelf)
  ;;                        ,@native-inputs))
  ;;        ;; (build-inputs `(,@(if source
  ;;        ;;                       `(("source" ,source))
  ;;        ;;                       '())
  ;;        ;;                 ,@native-inputs
  ;;        ;;                 ,@(if (and target implicit-cross-inputs?)
  ;;        ;;                       (standard-cross-packages target 'host)
  ;;        ;;                       '())
  ;;        ;;                 ,@(if implicit-inputs?
  ;;        ;;                       (standard-packages)
  ;;        ;;                       '())))
  ;;        (outputs outputs)
  ;;        (build rpm-build)
  ;;        (arguments (strip-keyword-arguments private-keywords arguments))))




  ;; (and (not target)) ;XXX: no cross-compilation
  (bag
    (name name)
    (system system) (target target)
    (build-inputs `(("binutils" ,binutils)
                    ("patchelf" ,patchelf)

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


    ;; (host-inputs `(,@(if source
    ;;                      `(("source" ,source))
    ;;                      '())
    ;;                ,@inputs
    ;;                ;; Keep the standard inputs of 'gnu-build-system'.
    ;;                ,@(gnu:standard-packages)))

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
    ;;              (delete "rpmug" outputs)))
    (outputs outputs)
    ;; (build (if target gnu-cross-build gnu-build))
    (build rpm-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))



(define* (rpm-build name inputs
                    #:key
                    ;; guile
                    (guile #f)
                    source
                    (outputs '("out"))
                    (search-paths '())
                    (bootstrap-scripts %bootstrap-scripts)
                    (configure-flags ''())
                    (make-flags ''())
                    (out-of-source? #f)
                    (tests? #f)
                    (test-target "check")
                    (test-command ''("make" "check"))
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
                    ;; (phases '(@ (lotus build patchelf-build-system)
                    ;;             %standard-phases))
                    (phases '(@ (lotus build rpm-build-system)
                                %standard-phases))
                    (input-lib-mapping ''())
                    ;; (readonly-binaries ''())
                    (locale "en_US.utf8")
                    (system (%current-system))
                    (build (nix-system->gnu-triplet system))
                    ;; (imported-modules %gnu-build-system-modules)
                    (imported-modules %rpm-build-system-modules)
                    ;; (modules %default-modules)
                    (modules '((lotus build rpm-build-system)
                               (guix build utils)))
                    (substitutable? #t)
                    allowed-references
                    disallowed-references)

  "Build SOURCE using RPM, and with INPUTS."

  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(rpm-build #:source #+source
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
                           #:input-lib-mapping ,input-lib-mapping
                           #:readonly-binaries ,readonly-binaries
                           #:locale #$locale
                           #:bootstrap-scripts #$bootstrap-scripts
                           #:configure-flags #$(if (pair? configure-flags)
                                                   (sexp->gexp configure-flags)
                                                   configure-flags)
                           #:make-flags #$(if (pair? make-flags)
                                              (sexp->gexp make-flags)
                                              make-flags)
                           #:out-of-source? #$out-of-source?
                           #:tests? #$tests?
                           ;; #:test-command ,test-command
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

(define rpm-build-system
  (build-system
    (name 'rpm)
    (description "The build system for Rpm packages")
    (lower lower)))

;;; rpm.scm ends here
