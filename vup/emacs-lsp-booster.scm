(define-module (vup emacs-lsp-booster)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages emacs)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix import utils)
               #:select (beautify-description)))

(define-public rust-winsafe-0.0.19
  (package
    (name "rust-winsafe")
    (version "0.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winsafe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rodrigocfd/winsafe")
    (synopsis "Windows API and GUI in safe, idiomatic Rust")
    (description
     "This package provides Windows API and GUI in safe, idiomatic Rust.")
    (license license:expat)))

(define-public rust-which-6.0.1
  (package
    (name "rust-which")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "which" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mz0vijj9qvsmfqkjqw3wf8zqn19p2x0gg7gzfnhaa1bibsy84c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-winsafe" ,rust-winsafe-0.0.19))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/harryfei/which-rs.git")
    (synopsis
     "Rust equivalent of Unix command \"which\". Locate installed executable in cross platforms.")
    (description
     "This package provides a Rust equivalent of Unix command \"which\".  Locate
installed executable in cross platforms.")
    (license license:expat)))

(define-public emacs-lsp-booster
  (package
    (name "rust-emacs-lsp-booster")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/blahgeek/emacs-lsp-booster")
                    (commit "4200ed6ae0cd83b8e3fd1dbefb09121480951a22")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "156qbp957kfh3w69sz9w3r7sr4dy6lndchn82zyl3v7qq75wsxr9"))))
    (build-system cargo-build-system)
    (inputs (list emacs))
    (arguments
    `(#:cargo-inputs
      (("rust-anyhow" ,rust-anyhow-1)
       ("rust-clap" ,rust-clap-4)
       ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2)
       ("rust-emacs" ,rust-emacs-0.18)        
       ("rust-env_logger" ,rust-env-logger-0.10)
       ("rust-lazy_static" ,rust-lazy-static-1)
       ("rust-log" ,rust-log-0.4)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde_json" ,rust-serde-json-1)
       ("rust-smallvec" ,rust-smallvec-1)
       ("rust-tempfile" ,rust-tempfile-3)
       ("rust-which" ,rust-which-6.0.1))))
    (home-page "FILLMEIN")
    (synopsis "FILLMEIN")
    (description
      (beautify-description "FILLMEIN"))
    (license #f)))
