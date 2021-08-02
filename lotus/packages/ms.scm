(define-module (lotus packages ms)
  #:use-module (ice-9 ftw)
  #:use-module (lotus build patchelf-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build rpath)
  #:use-module (guix build-system trivial)
  #:use-module (lotus build-system rpm)
  #:use-module (lotus build rpm-build-system)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base))


(define-public rpm-teams
  (package (name "rpm-teams")
           (version "1.3.00.5153-1")
           (source (origin (method    url-fetch)
                           (uri       (string-append "https://packages.microsoft.com/yumrepos/ms-teams/teams-" version ".x86_64.rpm"))
                           (file-name (string-append "teams-" version ".x86_64.rpm"))
                           (sha256    (base32 "009rh55r56k1zd7spiz6hy5chgkq9bg4d0zim9rwjyg28754r4zy"))))
           (build-system rpm-build-system)
           (arguments `(#:input-lib-mapping '(("nss" "lib/nss")
                                              ("adobe-flashplugin" "lib/adobe-flashplugin/")
                                              ("out" "share/firefox/lib"))))
                        ;; #:phases      (modify-phases %standard-phases
                        ;;                 (add-after
                        ;;                     'build 'rearrange
                        ;;                   ,patched-firefox-rearrange-method)
                        ;;                 (delete 'validate-runpath))
           (synopsis "rpm-teams")
           (description "rpm-teams.")
           (home-page "https://teams")
                                        ;; Conkeror is triple licensed.
           (license (list
                     ;; MPL 1.1 -- this license is not GPL compatible
                     license:gpl2
                     license:lgpl2.1))))



rpm-teams

