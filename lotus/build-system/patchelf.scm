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
  #:use-module ((lotus build patchelf-build-system)
                #:select (%default-include %default-exclude))
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%patchelf-build-system-modules
            patchelf-build
            patchelf-build-system)
  #:re-export (%default-include         ;for convenience
               %default-exclude))


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

(define (default-patchelf)
  "Return the default Patchelf package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((patchelf-mod (resolve-interface '(gnu packages elf))))
    (module-ref patchelf-mod 'patchelf-minimal)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (patchelf (default-patchelf))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:patchelf #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("patchelf" ,patchelf)
                         ,@native-inputs))
         (outputs outputs)
         (build patchelf-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (patchelf-build store name inputs
                         #:key source
                         (tests? #f)
                         (parallel-tests? #t)
                         (test-command ''("make" "check"))
                         (phases '(@ (lotus build patchelf-build-system)
                                     %standard-phases))
                         (outputs '("out"))
                         (include (quote %default-include))
                         (exclude (quote %default-exclude))
                         (search-paths '())
                         (system (%current-system))
                         (guile #f)
                         (imported-modules %patchelf-build-system-modules)
                         (modules '((lotus build patchelf-build-system)
                                    (guix build utils)
                                    (guix build patchelf-utils))))
  "Build SOURCE using PATCHELF, and with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (patchelf-build #:name ,name
                       #:source ,(match (assoc-ref inputs "source")
                                   (((? derivation? source))
                                    (derivation->output-path source))
                                   ((source)
                                    source)
                                   (source
                                    source))
                       #:system ,system
                       #:test-command ,test-command
                       #:tests? ,tests?
                       #:phases ,phases
                       #:outputs %outputs
                       #:include ,include
                       #:exclude ,exclude
                       #:search-paths ',(map search-path-specification->sexp
                                             search-paths)
                       #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define patchelf-build-system
  (build-system
    (name 'patchelf)
    (description "The build system for Patchelf packages")
    (lower lower)))

;;; patchelf.scm ends here
