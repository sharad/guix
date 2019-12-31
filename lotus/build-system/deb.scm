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

(define-module (lotus build-system deb)
  #:use-module ((lotus build deb-build-system))
  ;; #:use-module ((lotus build deb-utils))
  ;; #:select (%default-include %default-exclude)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((lotus build-system patchelf) #:prefix patchelf:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages base)
  #:export (%deb-build-system-modules
            deb-build
            deb-build-system))
;; #:re-export (%default-include         ;for convenience
;;              %default-exclude)

;; Commentary:
;;
;; Standard build procedure for Deb packages.  This is implemented as an
;; extension of 'gnu-build-system'.
;;
;; Code:

(define %deb-build-system-modules
  ;; Build-side modules imported by default.
  `((lotus build deb-build-system)
    ,@patchelf:%patchelf-build-system-modules))

;; (define (default-deb)
;;   "Return the default Deb package."
;;   ;; Lazily resolve the binding to avoid a circular dependency.
;;   (let ((deb-mod (resolve-interface '(gnu packages elf))))
;;     (module-ref deb-mod 'deb)))

(define* (lower name
                #:key source inputs native-inputs outputs system target ;; host-inputs
                ;; (deb (default-deb))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:deb #:inputs #:native-inputs)) ;#:host-inputs

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs
                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(gnu:standard-packages)))
         (build-inputs `(("binutils" ,binutils)
                         ,@native-inputs))
         ;; (build-inputs `(,@(if source
         ;;                       `(("source" ,source))
         ;;                       '())
         ;;                 ,@native-inputs
         ;;                 ,@(if (and target implicit-cross-inputs?)
         ;;                       (standard-cross-packages target 'host)
         ;;                       '())
         ;;                 ,@(if implicit-inputs?
         ;;                       (standard-packages)
         ;;                       '())))
         (outputs outputs)
         (build deb-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (deb-build store name inputs
                         #:key source ;; host-inputs
                         (tests? #f)
                         (parallel-tests? #t)
                         (test-command ''("make" "check"))
                         (phases '(@ (lotus build deb-build-system)
                                     %standard-phases))
                         (outputs '("out"))
                         (output-libs ''("/lib"))
                         (search-paths '())
                         (system (%current-system))
                         (guile #f)
                         (imported-modules %deb-build-system-modules)
                         (modules '((lotus build deb-build-system)
                                    (guix build utils))))
  "Build SOURCE using DEB, and with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (deb-build #:name ,name
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
                       #:output-libs ,output-libs
                       ;; #:exclude ,exclude
                       #:search-paths ',(map search-path-specification->sexp
                                             search-paths)
                       ;; #:host-inputs ',inputs
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

(define deb-build-system
  (build-system
    (name 'deb)
    (description "The build system for Deb packages")
    (lower lower)))

;;; deb.scm ends here
