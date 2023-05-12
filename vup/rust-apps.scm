(define-module (vup rust-apps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix import utils) #:select (beautify-description))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages tls))

(define-public ra-multiplex
  (package
    (name "ra-multiplex")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ra-multiplex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ph42x923j9i23y2x59g93ggv885k1ldixs5bg0xkqhmf43nsm3y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-directories" ,rust-directories-4)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/pr2502/ra-multiplex")
    (synopsis
     "share one rust-analyzer server instance between multiple LSP clients to save resources")
    (description
     "share one rust-analyzer server instance between multiple LSP clients to save
resources")
    (license license:expat)))
