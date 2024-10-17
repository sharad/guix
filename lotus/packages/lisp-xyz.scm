

(define-module (lotus packages lisp-xyz)
  #:use-module (gnu packages lisp-xyz))


(define-public sbcl-clx
  (let ((commit "52f457f0ba278e51dc0cbb4cab418049e6b32d9c")
        (revision "2"))
    (package
      (name "sbcl-clx")
      (version (git-version "0.7.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/sharplispers/clx")
           (commit commit)))
         (sha256
          (base32 "0hcyjj7z1xmjfh4f8zljyinnf2d4ap6gazfxkmiz8vvb8ks6i5y3"))
         (file-name (git-file-name "cl-clx" version)))))))

