(define-module (vup go-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-github-com-hashicorp-vault
  (package
    (name "go-github-com-hashicorp-vault")
    (version "1.7.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hashicorp/vault")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "06h31flxrv10r2n012i5g2mkqcqi53z7vikd1vbz001jyszi5n5c"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/hashicorp/vault"))
    (home-page
      "https://www.vaultproject.io")
    (synopsis "A tool for managing secrets")
    (description
      "A tool for managing secrets")
    (license license:mpl2.0)))
