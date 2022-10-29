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
  #:use-module (gnu packages tls)
  #:use-module (vup rust-nightly))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.87")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_json" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ibxrq43axvspv350wvx7w05l4s7b1gvaa0dysf6pmshn6vpgrvc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.147")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ln8rqbybpxmk4fvh6lgm75acs1d8x90fi44fhx3x77wm0n3c7ag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.147")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rc9jj8bbhf3lkf07ln8kyljigyzc4kk90nzg4dc2gwqmsdxd4yi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-ra-multiplex-0.2
  (package
    (name "rust-ra-multiplex")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ra-multiplex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "084gm3vfag4pbpi8rpf22agh0ag9sxwljl57vrgg67wbg3q95pdj"))))
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

(define-public rust-wiremock-0.4
  (package
    (name "rust-wiremock")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wiremock" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j196v0nkf7irhw4v0fs5z8pwh84m6pdlsxljr8brdi93fcin346"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-deadpool" ,rust-deadpool-0.7)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-timer" ,rust-futures-timer-3)
                       ("rust-http-types" ,rust-http-types-2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/LukeMathWalker/wiremock-rs")
    (synopsis "HTTP mocking to test Rust applications.")
    (description "HTTP mocking to test Rust applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-urlparse-0.7
  (package
    (name "rust-urlparse")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "urlparse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05w58v7siyiymxsv7q0kxp6lvqh4hs6piiq3j21nfv07x7a540qi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/yykamei/rust-urlparse")
    (synopsis "This is a URL parsing library like urllib.parse in Python3.x.")
    (description
     "This is a URL parsing library like urllib.parse in Python3.x.")
    (license license:expat)))

(define-public rust-displaydoc-0.1
  (package
    (name "rust-displaydoc")
    (version "0.1.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "displaydoc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06zl66x08jjd1lhk9hcva7v6fk4zwzjbb9p95687y48nb96sphmd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/yaahc/displaydoc")
    (synopsis
     "A derive macro for implementing the display Trait via a doc comment and string interpolation
")
    (description
     "This package provides a derive macro for implementing the display Trait via a
doc comment and string interpolation")
    (license (list license:expat license:asl2.0))))

(define-public rust-thirtyfour-0.27
  (package
    (name "rust-thirtyfour")
    (version "0.27.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thirtyfour" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v8zqs1r0iiirm9wkxp8ivvn05g7qn65vfwlkbcgifclzw559hka"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-displaydoc" ,rust-displaydoc-0.1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-http-client" ,rust-http-client-6)
                       ("rust-isahc" ,rust-isahc-0.9)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-stringmatch" ,rust-stringmatch-0.3)
                       ("rust-surf" ,rust-surf-2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-urlparse" ,rust-urlparse-0.7))))
    (home-page "https://github.com/stevepryde/thirtyfour")
    (synopsis
     "Thirtyfour is a Selenium / WebDriver library for Rust, for automated website UI testing.

It supports the full W3C WebDriver spec.
")
    (description
     "Thirtyfour is a Selenium / WebDriver library for Rust, for automated website UI
testing.  It supports the full W3C WebDriver spec.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stringmatch-0.3
  (package
    (name "rust-stringmatch")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "stringmatch" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h1vb2yiw1p12h86yrs8vbs7ww58qggw4pw9iy1w65h3fymzmh58"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/stevepryde/stringmatch")
    (synopsis
     "Allow the use of regular expressions or strings wherever you need string comparison")
    (description
     "Allow the use of regular expressions or strings wherever you need string
comparison")
    (license license:expat)))

(define-public rust-thirtyfour-sync-0.27
  (package
    (name "rust-thirtyfour-sync")
    (version "0.27.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thirtyfour_sync" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0f1x5km903irvl16wb8z28fzpkwyald1gxn6a7c0kmpyqbk4f7mb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-stringmatch" ,rust-stringmatch-0.3)
                       ("rust-thirtyfour" ,rust-thirtyfour-0.27))))
    (home-page "https://github.com/stevepryde/thirtyfour_sync")
    (synopsis
     "Thirtyfour is a Selenium / WebDriver library for Rust, for automated website UI testing.

This crate is the synchronous version only. For async, see the `thirtyfour` crate instead.
")
    (description
     "Thirtyfour is a Selenium / WebDriver library for Rust, for automated website UI
testing.  This crate is the synchronous version only.  For async, see the
`thirtyfour` crate instead.")
    (license (list license:expat license:asl2.0))))

(define-public rust-predicates-2
  (package
    (name "rust-predicates")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "predicates" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g0cjv6nn2s18kzsa3nkfhv7myxv9lbb710r0xrv8cj7dszbbam5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-concolor" ,rust-concolor-0.0.8)
                       ("rust-difflib" ,rust-difflib-0.4)
                       ("rust-float-cmp" ,rust-float-cmp-0.9)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-normalize-line-endings" ,rust-normalize-line-endings-0.3)
                       ("rust-predicates-core" ,rust-predicates-core-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://github.com/assert-rs/predicates-rs")
    (synopsis "An implementation of boolean-valued predicate functions.")
    (description "An implementation of boolean-valued predicate functions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-enum-ordinalize-3
  (package
    (name "rust-enum-ordinalize")
    (version "3.1.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "enum-ordinalize" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v639grfkm9gvfmxrhd10w7jlk8q5bmdc1fxifd0g0z3zq7gqw11"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://magiclen.org/enum-ordinalize")
    (synopsis
     "This crates provides a procedural macro to let enums not only get its variants' ordinal but also be constructed from an ordinal.")
    (description
     "This crates provides a procedural macro to let enums not only get its variants
ordinal but also be constructed from an ordinal.")
    (license license:expat)))

(define-public rust-educe-0.4
  (package
    (name "rust-educe")
    (version "0.4.19")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "educe" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b7wk1h4jhymwv6a012wfacn36wvjhh3pjjgnw6x224crp4pqyy0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-enum-ordinalize" ,rust-enum-ordinalize-3)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://magiclen.org/educe")
    (synopsis
     "This crate provides procedural macros to help you implement Rust-built-in traits quickly.")
    (description
     "This crate provides procedural macros to help you implement Rust-built-in traits
quickly.")
    (license license:expat)))

(define-public rust-tokio-serde-0.8
  (package
    (name "rust-tokio-serde")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rjl40amhpmfxlynv3gcvizgvm9hm983zqlfk6pqkdw6fdin26li"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bincode" ,rust-bincode-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-educe" ,rust-educe-0.4)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-rmp-serde" ,rust-rmp-serde-0.15)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-cbor" ,rust-serde-cbor-0.11)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/carllerche/tokio-serde")
    (synopsis
     "Send and receive Serde encodable types over the network using Tokio.

This library is used as a building block for serialization format specific
libraries.
")
    (description
     "Send and receive Serde encodable types over the network using Tokio.  This
library is used as a building block for serialization format specific libraries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-syslog-5
  (package
    (name "rust-syslog")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "syslog" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0paii62qnwjnfliygdal1x3hqxjkci1nlczfydv7kh3rnvqqwpcs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-error-chain" ,rust-error-chain-0.12)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-time" ,rust-time-0.1))))
    (home-page "https://github.com/Geal/rust-syslog")
    (synopsis "Send log messages to syslog")
    (description "Send log messages to syslog")
    (license license:expat)))

(define-public rust-time-macros-0.2
  (package
    (name "rust-time-macros")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nszl1kchvqg3zapwnjpnk8vpi3cz7mk3cc80wpiabcqh5gcmivj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "    Procedural macros for the time crate.
    This crate is an implementation detail and should not be relied upon directly.
")
    (description
     "Procedural macros for the time crate.  This crate is an implementation detail
and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c2j1n31j6yfzbskb81dyky6j98s1xkysf92ik75sfpvm74i02iy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itoa" ,rust-itoa-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time-macros" ,rust-time-macros-0.2))))
    (home-page "https://time-rs.github.io")
    (synopsis
     "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
     "Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))

(define-public rust-rouille-3
  (package
    (name "rust-rouille")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rouille" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wdnxd0mbz9nvv9yfhn17clhmdnr30l3yfk8qbkxrh9s0lfhh3v6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-brotli2" ,rust-brotli2-0.3)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-deflate" ,rust-deflate-0.9)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-multipart" ,rust-multipart-0.18)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha1" ,rust-sha1-0.6)
                       ("rust-threadpool" ,rust-threadpool-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tiny-http" ,rust-tiny-http-0.8)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/tomaka/rouille")
    (synopsis "High-level idiomatic web framework.")
    (description "High-level idiomatic web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-retry-1
  (package
    (name "rust-retry")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "retry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xm3p41ygijbjpyj81psqhb2r3rdcqwlk5pl48lgsqwsjh5cd5dc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/jimmycuadra/retry")
    (synopsis "Utilities for retrying operations that can fail.")
    (description "Utilities for retrying operations that can fail.")
    (license license:expat)))

(define-public rust-socket2-0.4
  (package
    (name "rust-socket2")
    (version "0.4.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "socket2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gaf57dc16s1lfyv388w9vdl9qay15xds78jcwakml9kj3dx5qh2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rust-lang/socket2")
    (synopsis
     "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.
")
    (description
     "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lock-api-0.4
  (package
    (name "rust-lock-api")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lock_api" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1py41vk243hwk345nhkn5nw0bd4m03gzjmprdjqq6rg5dwv12l23"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-owning-ref" ,rust-owning-ref-0.4)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
     "Wrappers to create fully-featured Mutex and RwLock types.  Compatible with
no_std.")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-0.12
  (package
    (name "rust-parking-lot")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "parking_lot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives.")
    (description
     "More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-msvc-0.42
  (package
    (name "rust-windows-x86-64-msvc")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xdnvhg8yj4fgjy0vkrahq5cbgfpcd7ak2bdv8s5lwjrazc0j07l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnullvm-0.42
  (package
    (name "rust-windows-x86-64-gnullvm")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0a10rns9b07m9snlr97iqxq42zi9ai547gb5fqlv7vihpb92bm89"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnu-0.42
  (package
    (name "rust-windows-x86-64-gnu")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vdh8k5a4m6pfkc5gladqznyqxgapkjm0qb8iwqvqb1nnlhinyxz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.42
  (package
    (name "rust-windows-i686-msvc")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ii2hrsdif2ms79dfiyfzm1n579jzj42ji3fpsxd57d3v9jjzhc4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.42
  (package
    (name "rust-windows-i686-gnu")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rsxdjp50nk38zfd1dxj12i2qmhpvxsm6scdq8v1d10ncygy3spv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-msvc-0.42
  (package
    (name "rust-windows-aarch64-msvc")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1d6d9ny0yl5l9vvagydigvkfcphzk2aygchiccywijimb8pja3yx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-gnullvm-0.42
  (package
    (name "rust-windows-aarch64-gnullvm")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17m1p753qk02r25afg31dxym4rpy7kpr0z8nwl5f1jzhyrqsmlj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Code gen support for the windows crate")
    (description "Code gen support for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-sys-0.42
  (package
    (name "rust-windows-sys")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19waf8aryvyq9pzk0gamgfwjycgzk4gnrazpfvv171cby0h1hgjs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.42)
                       ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.42)
                       ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.42)
                       ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.42)
                       ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.42)
                       ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.42)
                       ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.42))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "Rust for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasi-0.11
  (package
    (name "rust-wasi")
    (version "0.11.0+wasi-snapshot-preview1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/bytecodealliance/wasi")
    (synopsis "Experimental WASI API bindings for Rust")
    (description "Experimental WASI API bindings for Rust")
    (license (list license:asl2.0 unknown-license! license:asl2.0
                   license:expat))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.137")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12dz2lk4a7lm03k079n2rkm1l6cpdhvy6nrngbfprzrv19icqzzw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mio-0.8
  (package
    (name "rust-mio")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pjqn6jvmqkgyykf2z5danqka1rfs3il7w4d0qin8yi062y35mz5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-windows-sys" ,rust-windows-sys-0.42))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description "Lightweight non-blocking IO")
    (license license:expat)))

(define-public rust-autocfg-1
  (package
    (name "rust-autocfg")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "autocfg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Automatic cfg for Rust compiler features")
    (license (list license:asl2.0 license:expat))))

(define-public rust-tokio-1
  (package
    (name "rust-tokio")
    (version "1.21.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16chkl1wabwinnqya4zrjz7a1wn6mb20s699lwmp0mf9gm4krq59"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio-macros" ,rust-tokio-macros-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://tokio.rs")
    (synopsis
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

(define-public rust-slab-0.4
  (package
    (name "rust-slab")
    (version "0.4.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "slab" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vyw3rkdfdfkzfa1mh83s237sll8v5kazfwxma60bq4b59msf526"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tokio-rs/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description "Pre-allocated storage for a uniform data type")
    (license license:expat)))

(define-public rust-tokio-util-0.7
  (package
    (name "rust-tokio-util")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h67jb56bsxy4pi1a41pda8d52569ci5clvqv3c6cg9vy1sy1chb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.
")
    (description "Additional utilities for working with Tokio.")
    (license license:expat)))

(define-public rust-futures-task-0.3
  (package
    (name "rust-futures-task")
    (version "0.3.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-task" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sp6k18py8nv3dmy3j00w83bfmk6fzi7mwzxsflym9nrqlx3kyrg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.
")
    (description "Tools for working with tasks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-macro-0.3
  (package
    (name "rust-futures-macro")
    (version "0.3.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pc3c5mydmwy50f0whcljcd41f0z1ci0r65dka8r2syqagh8ryxx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.
")
    (description "The futures-rs procedural macro implementations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-io-0.3
  (package
    (name "rust-futures-io")
    (version "0.3.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-io" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1szl4w206x2inliipf5hvjbrd8w8i0gnglz8akmsvp3bl19gpx80"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.
")
    (description
     "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the
futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-sink-0.3
  (package
    (name "rust-futures-sink")
    (version "0.3.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-sink" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ygwh57nzinpj2sk6akc6sgavl3njsrjyimvy50dyydalkqmrh9r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.
")
    (description "The asynchronous `Sink` trait for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-core-0.3
  (package
    (name "rust-futures-core")
    (version "0.3.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b6k9fd6bkga9556jyx78di278bdp2p81cls99nawcs6grx9m404"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.
")
    (description "The core traits and types in for the `futures` library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-channel-0.3
  (package
    (name "rust-futures-channel")
    (version "0.3.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-channel" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vcyyxwdgh92nl277053zvqd3qpzf6jhb5kibgs0aq95j9d2dfjj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.
")
    (description "Channels for asynchronous communication using futures-rs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-util-0.3
  (package
    (name "rust-futures-util")
    (version "0.3.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mk5vh8q5bkkvxji8r1nimh87hgi190nz4l4zynrqbxxgac7cxhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-macro" ,rust-futures-macro-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "Common utilities and extension traits for the futures-rs library.
")
    (description
     "Common utilities and extension traits for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crc16-0.4
  (package
    (name "rust-crc16")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crc16" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zzwb5iv51wnh96532cxkk4aa8ys47rhzrjy98wqcys25ks8k01k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/blackbeam/rust-crc16")
    (synopsis "A CRC16 implementation")
    (description "This package provides a CRC16 implementation")
    (license license:expat)))

(define-public rust-async-native-tls-0.4
  (package
    (name "rust-async-native-tls")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "async-native-tls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zhkka5azpr03wg2bswabmwcwcqbdia17h2d17hk4wk47kn4qzfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures
")
    (description "Native TLS using futures")
    (license (list license:expat license:asl2.0))))

(define-public rust-redis-0.21
  (package
    (name "rust-redis")
    (version "0.21.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "redis" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00qisc3agcn9li76pc171wz2z4s8xvli9pcywk9jm6nhd0n2a72p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-async-native-tls" ,rust-async-native-tls-0.4)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-combine" ,rust-combine-4)
                       ("rust-crc16" ,rust-crc16-0.4)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-r2d2" ,rust-r2d2-0.8)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-sha1" ,rust-sha1-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/redis-rs/redis-rs")
    (synopsis "Redis driver for Rust.")
    (description "Redis driver for Rust.")
    (license license:bsd-3)))

(define-public rust-parity-tokio-ipc-0.9
  (package
    (name "rust-parity-tokio-ipc")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "parity-tokio-ipc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dj2ilhpc2dmjg6f6qmalkh30f1r5ws71yzm930lq02ynwpy70cr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/paritytech/parity-tokio-ipc")
    (synopsis "Interprocess communication library for tokio.
")
    (description "Interprocess communication library for tokio.")
    (license (list license:expat license:asl2.0))))

(define-public rust-conhash-0.4
  (package
    (name "rust-conhash")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "conhash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jhzkf744si69mrvg4il1p8pqdysh9cgl530igcx0y47096kdmlr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-md5" ,rust-md5-0.3))))
    (home-page "https://github.com/zonyitoo/conhash-rs")
    (synopsis "Consistent Hashing library in Rust")
    (description "Consistent Hashing library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-memcached-rs-0.4
  (package
    (name "rust-memcached-rs")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "memcached-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "078bxv85gm7b5g2z8mrdjm1bcdnz3wc5mv5ksqwwsmmmm11ld073"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bufstream" ,rust-bufstream-0.1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-conhash" ,rust-conhash-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-semver" ,rust-semver-0.9)
                       ("rust-unix-socket" ,rust-unix-socket-0.5))))
    (home-page "https://github.com/zonyitoo/memcached-rs")
    (synopsis "A MemCached Library in Rust")
    (description "This package provides a MemCached Library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-md5-asm-0.5
  (package
    (name "rust-md5-asm")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md5-asm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ixmkg8j7sqy9zln6pz9xi2dl2d9zpm8pz6p49za47n1bvradfbk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of MD5 compression function")
    (description "Assembly implementation of MD5 compression function")
    (license license:expat)))

(define-public rust-typenum-1
  (package
    (name "rust-typenum")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "typenum" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11yrvz1vd43gqv738yw1v75rzngjbs7iwcgzjy3cq5ywkv2imy6w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-scale-info" ,rust-scale-info-1))))
    (home-page "https://github.com/paholg/typenum")
    (synopsis
     "Typenum is a Rust library for type-level numbers evaluated at
    compile time. It currently supports bits, unsigned integers, and signed
    integers. It also provides a type-level array of type-level numbers, but its
    implementation is incomplete.")
    (description
     "Typenum is a Rust library for type-level numbers evaluated at compile time.  It
currently supports bits, unsigned integers, and signed integers.  It also
provides a type-level array of type-level numbers, but its implementation is
incomplete.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-common-0.1
  (package
    (name "rust-crypto-common")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crypto-common" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Common cryptographic traits")
    (description "Common cryptographic traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-const-oid-0.9
  (package
    (name "rust-const-oid")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "const-oid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q8n1zsa73130hxa2w88qw36g8nprz21j52abpva3khm59a26bkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustCrypto/formats/tree/master/const-oid")
    (synopsis
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard
as defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e. embedded) support
")
    (description
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard as
defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e.  embedded) support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-digest-0.10
  (package
    (name "rust-digest")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "digest" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v7qvhh0apbgagnj2dc1x8pnwxmvd5z4vdpjxg9cnym3cmrwbyxd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-block-buffer" ,rust-block-buffer-0.10)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for cryptographic hash functions")
    (description "Traits for cryptographic hash functions")
    (license (list license:expat license:asl2.0))))

(define-public rust-md-5-0.10
  (package
    (name "rust-md-5")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md-5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jmrykh705dfclkgxwjysj5y8l1nyrn1gddw5xpgyjyla1l50rb3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-md5-asm" ,rust-md5-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "MD5 hash function")
    (description "MD5 hash function")
    (license (list license:expat license:asl2.0))))

(define-public rust-pulldown-cmark-0.0.3
  (package
    (name "rust-pulldown-cmark")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pulldown-cmark" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01vc9rw3dwrwq0qwfrfsh1qnhaqpxj3y8l29n11jdq6jfqayhqc3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-getopts" ,rust-getopts-0.2))))
    (home-page "https://github.com/raphlinus/pulldown-cmark")
    (synopsis "A pull parser for CommonMark")
    (description "This package provides a pull parser for CommonMark")
    (license license:asl2.0)))

(define-public rust-skeptic-0.4
  (package
    (name "rust-skeptic")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "skeptic" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0si5hbzfb68ipbx30lcpmq3nkvy4mbvpmg2vmrhsx2szdyhgisr4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pulldown-cmark" ,rust-pulldown-cmark-0.0.3)
                       ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/budziq/rust-skeptic")
    (synopsis "Test your Rust markdown documentation via Cargo")
    (description "Test your Rust markdown documentation via Cargo")
    (license (list license:expat license:asl2.0))))

(define-public rust-local-encoding-0.2
  (package
    (name "rust-local-encoding")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "local-encoding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rhsb8x10i0959ry38da3j1avnmihqwmyygr7wpy8ypz747v5kp1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
                       ("rust-skeptic" ,rust-skeptic-0.4)
                       ("rust-winapi" ,rust-winapi-0.2))))
    (home-page "https://github.com/bozaro/local-encoding-rs")
    (synopsis
     "Rust library for encoding/decoding string with local charset. It usefull for work with ANSI strings on Windows.")
    (description
     "Rust library for encoding/decoding string with local charset.  It usefull for
work with ANSI strings on Windows.")
    (license license:expat)))

(define-public rust-libmount-0.1
  (package
    (name "rust-libmount")
    (version "0.1.15")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libmount" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0py3kl473jgfwnfajzr0xi9xs2lk8npks3320md2zgaw5nnw5i13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.14)
                       ("rust-quick-error" ,rust-quick-error-1))))
    (home-page "http://github.com/tailhook/libmount")
    (synopsis "    The type-safe wrapper around mount system call
")
    (description "The type-safe wrapper around mount system call")
    (license (list license:expat license:asl2.0))))

(define-public rust-simple-asn1-0.4
  (package
    (name "rust-simple-asn1")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "simple_asn1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jxy9as8nj65c2n27j843g4fpb95x4fjz31w6qx63q3wwlys2b39"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/acw/simple_asn1")
    (synopsis "A simple DER/ASN.1 encoding/decoding library.")
    (description
     "This package provides a simple DER/ASN.1 encoding/decoding library.")
    (license license:isc)))

(define-public rust-pem-0.8
  (package
    (name "rust-pem")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pem" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sqkzp87j6s79sjxk4n913gcmalzb2fdc75l832d0j7a3z9cnmpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/jcreekmore/pem-rs.git")
    (synopsis "Parse and encode PEM-encoded data.")
    (description "Parse and encode PEM-encoded data.")
    (license license:expat)))

(define-public rust-jsonwebtoken-7
  (package
    (name "rust-jsonwebtoken")
    (version "7.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jsonwebtoken" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ciz205wcjcn7n6i871zz5xlbzk863b0ybgiqi7li9ipwhawraxg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.12)
                       ("rust-pem" ,rust-pem-0.8)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simple-asn1" ,rust-simple-asn1-0.4))))
    (home-page "https://github.com/Keats/jsonwebtoken")
    (synopsis "Create and decode JWTs in a strongly typed way.")
    (description "Create and decode JWTs in a strongly typed way.")
    (license license:expat)))

(define-public rust-percent-encoding-2
  (package
    (name "rust-percent-encoding")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "percent-encoding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bp3zrsk3kr47fbpipyczidbbx4g54lzxdm77ni1i3qws10mdzfl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/servo/rust-url/")
    (synopsis "Percent encoding and decoding")
    (description "Percent encoding and decoding")
    (license (list license:expat license:asl2.0))))

(define-public rust-http-0.2
  (package
    (name "rust-http")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1693pkg43czk26fima0l0l5h2h9rvm8n84pff5zc35b9w90kvx3m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-itoa" ,rust-itoa-1))))
    (home-page "https://github.com/hyperium/http")
    (synopsis "A set of types for representing HTTP requests and responses.
")
    (description
     "This package provides a set of types for representing HTTP requests and
responses.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyperx-1
  (package
    (name "rust-hyperx")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyperx" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k08dp5bw415wp3j3fg5paidn6l4mmy59kn65cz1ql7jq8pyj5sn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-language-tags" ,rust-language-tags-0.3)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/dekellum/hyperx")
    (synopsis "Hyper's typed header module, eXtracted and improved")
    (description "Hyper's typed header module, eXtracted and improved")
    (license license:expat)))

(define-public rust-futures-locks-0.7
  (package
    (name "rust-futures-locks")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-locks" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12jmgrycw0ppbidz0jasj9ca6f0a4ksrx7s2g1bvw9r2nx7jvd1y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/asomers/futures-locks")
    (synopsis "Futures-aware lock primitives
")
    (description "Futures-aware lock primitives")
    (license (list license:expat license:asl2.0))))

(define-public rust-counted-array-0.1
  (package
    (name "rust-counted-array")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "counted-array" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zj8yr39pb0q2v4gskbcr9rs3lh90xrpn4p0nqh0k2aw2x9qqkrq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-0.2))))
    (home-page "https://github.com/durka/counted-array")
    (synopsis
     "Macro for declaring fixed-size arrays without counting elements by hand. Supports lazy_static.")
    (description
     "Macro for declaring fixed-size arrays without counting elements by hand.
Supports lazy_static.")
    (license license:expat)))

(define-public rust-ar-0.9
  (package
    (name "rust-ar")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ar" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sdvvrf4inrkmrh6lzwg4z8x38b3gncbb8yqrgayqcd9d1yzfynn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/mdsteele/rust-ar")
    (synopsis "A library for encoding/decoding Unix archive files.")
    (description
     "This package provides a library for encoding/decoding Unix archive files.")
    (license license:expat)))

(define-public rust-sccache-0.3
  (package
    (name "rust-sccache")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sccache" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wk0csqwq8q5h6vpz6wlqddj2fkdfpqxb5akr25raz4qka2gghha"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-ar" ,rust-ar-0.9)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bincode" ,rust-bincode-1)
                       ("rust-blake3" ,rust-blake3-0.3)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-2)
                       ("rust-counted-array" ,rust-counted-array-0.1)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-daemonize" ,rust-daemonize-0.4)
                       ("rust-directories" ,rust-directories-4)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-locks" ,rust-futures-locks-0.7)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyperx" ,rust-hyperx-1)
                       ("rust-jobserver" ,rust-jobserver-0.1)
                       ("rust-jsonwebtoken" ,rust-jsonwebtoken-7)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libmount" ,rust-libmount-0.1)
                       ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                       ("rust-local-encoding" ,rust-local-encoding-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-memcached-rs" ,rust-memcached-rs-0.4)
                       ("rust-nix" ,rust-nix-0.23)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-number-prefix" ,rust-number-prefix-0.4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-parity-tokio-ipc" ,rust-parity-tokio-ipc-0.9)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-redis" ,rust-redis-0.21)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-retry" ,rust-retry-1)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rouille" ,rust-rouille-3)
                       ("rust-semver" ,rust-semver-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.1)
                       ("rust-syslog" ,rust-syslog-5)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-serde" ,rust-tokio-serde-0.8)
                       ("rust-tokio-util" ,rust-tokio-util-0.6)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-version-compare" ,rust-version-compare-0.1)
                       ("rust-void" ,rust-void-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-which" ,rust-which-4)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-zip" ,rust-zip-0.5)
                       ("rust-zstd" ,rust-zstd-0.6))
       #:cargo-development-inputs (("rust-assert-cmd" ,rust-assert-cmd-2)
                                   ("rust-cc" ,rust-cc-1)
                                   ("rust-chrono" ,rust-chrono-0.4)
                                   ("rust-itertools" ,rust-itertools-0.10)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-predicates" ,rust-predicates-2)
                                   ("rust-serial-test" ,rust-serial-test-0.5)
                                   ("rust-thirtyfour-sync" ,rust-thirtyfour-sync-0.27)
                                   ("rust-wiremock" ,rust-wiremock-0.4))))
    (home-page "https://github.com/mozilla/sccache/")
    (synopsis
     "Sccache is a ccache-like tool. It is used as a compiler wrapper and avoids compilation when possible, storing a cache in a remote storage using the S3 API.")
    (description
     "Sccache is a ccache-like tool.  It is used as a compiler wrapper and avoids
compilation when possible, storing a cache in a remote storage using the S3 API.")
    (license license:asl2.0)))

