(define-module (vup atuin)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io)
  #:use-module (vup rust-apps)
  #:use-module (vup rust-nightly)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-sha2-0.10
  (package
    (name "rust-sha2")
    (version "0.10.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h5xrrv2y06kr1gsz4pwrm3lsp206nm2gjxgbf21wfrfzsavgrl2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-sha2-asm" ,rust-sha2-asm-0.6))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "Pure Rust implementation of the SHA-2 hash function family
including SHA-224, SHA-256, SHA-384, and SHA-512.
")
    (description
     "Pure Rust implementation of the SHA-2 hash function family including SHA-224,
SHA-256, SHA-384, and SHA-512.")
    (license (list license:expat license:asl2.0))))

(define-public rust-packed-simd-2-0.3
  (package
    (name "rust-packed-simd-2")
    (version "0.3.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "packed_simd_2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10p2bm0p57shg3arlpfwm6z0bbnlkyr4g0dlkmpwvz6qaba4r4d1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-core-arch" ,rust-core-arch-0.1)
                       ("rust-libm" ,rust-libm-0.1)
                       ("rust-sleef-sys" ,rust-sleef-sys-0.1))))
    (home-page "https://github.com/rust-lang/packed_simd")
    (synopsis "Portable Packed SIMD vectors")
    (description "Portable Packed SIMD vectors")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-0.8
  (package
    (name "rust-rand")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rand" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
                       ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Random number generators and other randomness functionality.
")
    (description
     "Random number generators and other randomness functionality.")
    (license (list license:expat license:asl2.0))))

(define-public rust-password-hash-0.4
  (package
    (name "rust-password-hash")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "password-hash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "003p2hssyrcaxyq9fs8x2wx5di8ny9byaakskrf352pfm963fxkn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/password-hash")
    (synopsis
     "Traits which describe the functionality of password hashing algorithms,
as well as a `no_std`-friendly implementation of the PHC string format
(a well-defined subset of the Modular Crypt Format a.k.a. MCF)
")
    (description
     "Traits which describe the functionality of password hashing algorithms, as well
as a `no_std`-friendly implementation of the PHC string format (a well-defined
subset of the Modular Crypt Format a.k.a.  MCF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-pbkdf2-0.11
  (package
    (name "rust-pbkdf2")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pbkdf2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05q9wqjvfrs4dvw03yn3bvcs4zghz0a7ycfa53pz2k2fqhp6k843"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-password-hash" ,rust-password-hash-0.4)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/pbkdf2")
    (synopsis "Generic implementation of PBKDF2")
    (description "Generic implementation of PBKDF2")
    (license (list license:expat license:asl2.0))))

(define-public rust-tiny-bip39-1
  (package
    (name "rust-tiny-bip39")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tiny-bip39" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q98iv3wgbd41wyxxd5is8sddi53k9ary45rbi5fi8dmb39r9k32"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.11)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/maciejhirsz/tiny-bip39/")
    (synopsis
     "A fork of the bip39 crate with fixes to v0.6. Rust implementation of BIP-0039")
    (description
     "This package provides a fork of the bip39 crate with fixes to v0.6.  Rust
implementation of BIP-0039")
    (license (list license:expat license:asl2.0))))

(define-public rust-runtime-format-0.1
  (package
    (name "rust-runtime-format")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "runtime-format" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1594ayzc5r2xr0sy4wxvjfw0hgs4cs1r5z64mj1ldbxi26230ddh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tinyvec" ,rust-tinyvec-1))))
    (home-page "https://github.com/conradludgate/strfmt")
    (synopsis "rust library for formatting dynamic strings")
    (description "rust library for formatting dynamic strings")
    (license license:expat)))

(define-public rust-rtoolbox-0.0.1
  (package
    (name "rust-rtoolbox")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rtoolbox" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jjjngwn1fa39kschc8zb4hynp4b0jdind7z225cph7m2k2j4kh3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "")
    (synopsis
     "Utility functions for other crates, no backwards compatibility guarantees.")
    (description
     "Utility functions for other crates, no backwards compatibility guarantees.")
    (license license:asl2.0)))

(define-public rust-rpassword-7
  (package
    (name "rust-rpassword")
    (version "7.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rpassword" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08l3jbjwpsj6awm4lacm2bcj3cn9jhy4j6q21n68k49lmdiwyy36"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-rtoolbox" ,rust-rtoolbox-0.0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/conradkleinespel/rpassword")
    (synopsis "Read passwords in console applications.")
    (description "Read passwords in console applications.")
    (license license:asl2.0)))

(define-public rust-vte-0.11
  (package
    (name "rust-vte")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vte" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0iah6qvi8r6a1lslcwjg2g0jnczz72f3cvr3ihb2vv6j5b0j3bhs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-utf8parse" ,rust-utf8parse-0.2)
                       ("rust-vte-generate-state-changes" ,rust-vte-generate-state-changes-0.1))))
    (home-page "https://github.com/alacritty/vte")
    (synopsis "Parser for implementing terminal emulators")
    (description "Parser for implementing terminal emulators")
    (license (list license:asl2.0 license:expat))))

(define-public rust-vt100-0.15
  (package
    (name "rust-vt100")
    (version "0.15.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vt100" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pklc8y984axmxr0cd363srr2d27wd5rj15xlcmkjznvy0xqdkc4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vte" ,rust-vte-0.11))))
    (home-page "https://github.com/doy/vt100-rust")
    (synopsis "Library for parsing terminal data")
    (description "Library for parsing terminal data")
    (license license:expat)))

(define-public rust-portable-atomic-0.3
  (package
    (name "rust-portable-atomic")
    (version "0.3.19")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portable-atomic" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02x09r2zl5ybkazi6k5pfrz8pzs0yzpyxp5d84r5lhrfgjwagxi6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/taiki-e/portable-atomic")
    (synopsis
     "Portable atomic types including support for 128-bit atomics, atomic float, etc.
")
    (description
     "Portable atomic types including support for 128-bit atomics, atomic float, etc.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-indicatif-0.17
  (package
    (name "rust-indicatif")
    (version "0.17.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "indicatif" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ad70n05p6k6p3lsyws3hkq3rbq4ap9k83bgfpb68f67kfm0kxff"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-number-prefix" ,rust-number-prefix-0.4)
                       ("rust-portable-atomic" ,rust-portable-atomic-0.3)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vt100" ,rust-vt100-0.15))))
    (home-page "https://github.com/console-rs/indicatif")
    (synopsis "A progress bar and cli reporting library for Rust")
    (description
     "This package provides a progress bar and cli reporting library for Rust")
    (license license:expat)))

(define-public rust-env-logger-0.10
  (package
    (name "rust-env-logger")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "env_logger" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w797qgkrmqdacsbc0j6yvpnmvfc9lx6k8fm79rndkxci5mapkc5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-humantime" ,rust-humantime-2)
                       ("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/rust-cli/env_logger/")
    (synopsis
     "A logging implementation for `log` which is configured via an environment
variable.
")
    (description
     "This package provides a logging implementation for `log` which is configured via
an environment variable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-signal-hook-mio-0.2
  (package
    (name "rust-signal-hook-mio")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "signal-hook-mio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bwrrbd0lhwzlf63708vyzlh20693s5bg5s0ak6adjbyycajxb99"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.7)
                       ("rust-mio" ,rust-mio-0.6)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-mio-uds" ,rust-mio-uds-0.6)
                       ("rust-signal-hook" ,rust-signal-hook-0.3))))
    (home-page "https://github.com/vorner/signal-hook")
    (synopsis "MIO support for signal-hook")
    (description "MIO support for signal-hook")
    (license (list license:asl2.0 license:expat))))

(define-public rust-signal-hook-0.3
  (package
    (name "rust-signal-hook")
    (version "0.3.15")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "signal-hook" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fc2wvl8sp6l6kd5hh0csn9bl2yl4cqsj8vc0ygd08bd2zqnh9vk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1))))
    (home-page "https://github.com/vorner/signal-hook")
    (synopsis "Unix signal handling")
    (description "Unix signal handling")
    (license (list license:asl2.0 license:expat))))

(define-public rust-filedescriptor-0.8
  (package
    (name "rust-filedescriptor")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "filedescriptor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vplyh0cw35kzq7smmp2ablq0zsknk5rkvvrywqsqfrchmjxk6bi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis "More ergonomic wrappers around RawFd and RawHandle")
    (description "More ergonomic wrappers around RawFd and RawHandle")
    (license license:expat)))

(define-public rust-crossterm-winapi-0.9
  (package
    (name "rust-crossterm-winapi")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crossterm_winapi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "075z15gxm4rn5yywq46khbg29bf504ix0f06zq3hx8aa91db7q9a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/crossterm-rs/crossterm-winapi")
    (synopsis
     "WinAPI wrapper that provides some basic simple abstractions around common WinAPI calls")
    (description
     "WinAPI wrapper that provides some basic simple abstractions around common WinAPI
calls")
    (license license:expat)))

(define-public rust-crossterm-0.26
  (package
    (name "rust-crossterm")
    (version "0.26.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crossterm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04rxvmbf3scywy0m7rhg586lf833vpb33czijxi80fakadkxlk58"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-crossterm-winapi" ,rust-crossterm-winapi-0.9)
                       ("rust-filedescriptor" ,rust-filedescriptor-0.8)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-mio" ,rust-signal-hook-mio-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/crossterm-rs/crossterm")
    (synopsis "A crossplatform terminal library for manipulating terminals.")
    (description
     "This package provides a crossplatform terminal library for manipulating
terminals.")
    (license license:expat)))

(define-public rust-clap-complete-4
  (package
    (name "rust-clap-complete")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_complete" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gclf83c51lrr65ib89z3gi8zfzpkqyi0lwrn9j3g20hnk7jvhh1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-clap-lex" ,rust-clap-lex-0.4)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_complete")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description "Generate shell completion scripts for your clap::Command")
    (license (list license:expat license:asl2.0))))

(define-public rust-iri-string-0.4
  (package
    (name "rust-iri-string")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "iri-string" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y2z4f5y87hnff2d5lcl811hp7iv2f5qri7x3fgm48z2q4w7c3wg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-nom" ,rust-nom-7)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/lo48576/iri-string")
    (synopsis "IRI as string types")
    (description "IRI as string types")
    (license (list license:expat license:asl2.0))))

(define-public rust-tower-http-0.3
  (package
    (name "rust-tower-http")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower-http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0n6qr26ivwqv19fih5pcjk2nvmys77m964lwkqiyil9dy15h8wzq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-http-range-header" ,rust-http-range-header-0.3)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-iri-string" ,rust-iri-string-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description "Tower middleware and utilities for HTTP clients and servers")
    (license license:expat)))

(define-public rust-chronoutil-0.2
  (package
    (name "rust-chronoutil")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chronoutil" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zc33vbn7p93kk43nvafqrsh5dj6ihqgbb533lhalwmp9f98r9a3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4))))
    (home-page "https://github.com/olliemath/chronoutil")
    (synopsis "Powerful extensions to rust's Chrono crate")
    (description "Powerful extensions to rust's Chrono crate")
    (license license:expat)))

(define-public rust-tower-layer-0.3
  (package
    (name "rust-tower-layer")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower-layer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l7i17k9vlssrdg4s3b0ia5jjkmmxsvv8s9y9ih0jfi8ssz8s362"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Decorates a `Service` to allow easy composition between `Service`s.
")
    (description
     "Decorates a `Service` to allow easy composition between `Service`s.")
    (license license:expat)))

(define-public rust-iri-string-0.7
  (package
    (name "rust-iri-string")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "iri-string" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h07hkfkkjjvgzlaqpr5fia7hrgv7qxqdw4xrpdc3936gmk9p191"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-memchr" ,rust-memchr-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/lo48576/iri-string")
    (synopsis "IRI as string types")
    (description "IRI as string types")
    (license (list license:expat license:asl2.0))))

(define-public rust-http-range-header-0.3
  (package
    (name "rust-http-range-header")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http-range-header" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0aas8c5dagfhcqpmqq9xw6a8nkl3lfg4g4mpddvyz1cj1bnqxzhb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/MarcusGrass/parse-range-headers")
    (synopsis "No-dep range header parser")
    (description "No-dep range header parser")
    (license license:expat)))

(define-public rust-tower-http-0.4
  (package
    (name "rust-tower-http")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower-http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0n5n98fcszjq92jhc2ppk6hjwn8lxim3g3cfhax4dv7knfll47ax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.3)
                       ("rust-base64" ,rust-base64-0.20)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-http-range-header" ,rust-http-range-header-0.3)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-iri-string" ,rust-iri-string-0.7)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description "Tower middleware and utilities for HTTP clients and servers")
    (license license:expat)))

(define-public rust-tower-service-0.3
  (package
    (name "rust-tower-service")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower-service" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lmfzmmvid2yp2l36mbavhmqgsvzqf7r2wiwz73ml4xmwaf1rg5n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Trait representing an asynchronous, request / response based, client or server.
")
    (description
     "Trait representing an asynchronous, request / response based, client or server.")
    (license license:expat)))

(define-public rust-hdrhistogram-7
  (package
    (name "rust-hdrhistogram")
    (version "7.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hdrhistogram" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a1al1rfxcqmx0n9h100ggvg036f4rv69fq12kimazvw9zsvj6bz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/HdrHistogram/HdrHistogram_rust")
    (synopsis "A port of HdrHistogram to Rust")
    (description "This package provides a port of HdrHistogram to Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-tower-0.4
  (package
    (name "rust-tower")
    (version "0.4.13")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tower" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hdrhistogram" ,rust-hdrhistogram-7)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
     "Tower is a library of modular and reusable components for building robust
clients and servers.
")
    (description
     "Tower is a library of modular and reusable components for building robust
clients and servers.")
    (license license:expat)))

(define-public rust-tungstenite-0.18
  (package
    (name "rust-tungstenite")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tungstenite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1207jv8ciklgnqwjhxc1c1xhplrfab231191apyz0k6d56vnmvih"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description "Lightweight stream-based WebSocket implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-tungstenite-0.18
  (package
    (name "rust-tokio-tungstenite")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-tungstenite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z8bxhq6d1ndh4x914wwk72l93ha1sl0jmnb6knvqiqi869rqcal"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tungstenite" ,rust-tungstenite-0.18)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket
implementation")
    (license license:expat)))

(define-public rust-tokio-macros-2
  (package
    (name "rust-tokio-macros")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kni17i1sbyz8rvn83g48z4058z8ckcb7lgfvpbfk1brr2yp79b1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://tokio.rs")
    (synopsis "Tokio's proc macros.
")
    (description "Tokio's proc macros.")
    (license license:expat)))

(define-public rust-socket2-0.4
  (package
    (name "rust-socket2")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "socket2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qnn1r41jqj20m0a2nzzjgzndlmpg5maiyjchccaypfqxq8sk934"))))
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

(define-public rust-tokio-1
  (package
    (name "rust-tokio")
    (version "1.27.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00f07c752xjcq983c06xs692c39gg6fv5abqi54izqfbxsj4gpnh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio-macros" ,rust-tokio-macros-2)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://tokio.rs")
    (synopsis
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.
")
    (description
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

(define-public rust-sync-wrapper-0.1
  (package
    (name "rust-sync-wrapper")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sync_wrapper" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3))))
    (home-page "https://docs.rs/sync_wrapper")
    (synopsis
     "A tool for enlisting the compilerâs help in proving the absence of concurrency")
    (description
     "This package provides a tool for enlisting the compilerâs help in proving the
absence of concurrency")
    (license license:asl2.0)))

(define-public rust-serde-path-to-error-0.1
  (package
    (name "rust-serde-path-to-error")
    (version "0.1.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_path_to_error" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q5vb8pfjan001qp529h3npy7jn35zsy3k7srppns1knahfmrw7p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dtolnay/path-to-error")
    (synopsis "Path to the element that failed to deserialize")
    (description "Path to the element that failed to deserialize")
    (license (list license:expat license:asl2.0))))

(define-public rust-matchit-0.7
  (package
    (name "rust-matchit")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "matchit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h7a1a57wamz0305dipj20shv2b5dw47jjp6dsgfaxmpmznlhwmq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ibraheemdev/matchit")
    (synopsis "A blazing fast URL router.")
    (description "This package provides a blazing fast URL router.")
    (license license:expat)))

(define-public rust-itoa-1
  (package
    (name "rust-itoa")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "itoa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19jc2sa3wvdc29zhgbwf3bayikq4rq18n20dbyg9ahd4hbsxjfj5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-no-panic" ,rust-no-panic-0.1))))
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis "Fast integer primitive to string conversion")
    (description "Fast integer primitive to string conversion")
    (license (list license:expat license:asl2.0))))

(define-public rust-httparse-1
  (package
    (name "rust-httparse")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "httparse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "010rrfahm1jss3p022fqf3j3jmm72vhn4iqhykahb9ynpaag75yq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/seanmonstar/httparse")
    (synopsis "A tiny, safe, speedy, zero-copy HTTP/1.x parser.")
    (description
     "This package provides a tiny, safe, speedy, zero-copy HTTP/1.x parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-h2-0.3
  (package
    (name "rust-h2")
    (version "0.3.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "h2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kcjammvjdmxnqh8fljidirxpcdsna75vgv6yljgd0dmi52vbrsv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis "An HTTP/2 client and server")
    (description "An HTTP/2 client and server")
    (license license:expat)))

(define-public rust-hyper-0.14
  (package
    (name "rust-hyper")
    (version "0.14.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyper" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16cqihimhv50gq1nz5dw71icxmbwsws8fzfmzw4j70hryr7mapnc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-want" ,rust-want-0.3))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description "This package provides a fast and correct HTTP library.")
    (license license:expat)))

(define-public rust-headers-0.3
  (package
    (name "rust-headers")
    (version "0.3.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "headers" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11258p6q2md68sfhmqrgrx23vjiapqcbxffh1hz223awivdp5qzk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-headers-core" ,rust-headers-core-0.2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-sha1" ,rust-sha1-0.10))))
    (home-page "https://hyper.rs")
    (synopsis "typed HTTP headers")
    (description "typed HTTP headers")
    (license license:expat)))

(define-public rust-axum-macros-0.3
  (package
    (name "rust-axum-macros")
    (version "0.3.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "axum-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xrjwwq3adyzfk1lw27gypvqry8pn3vpjwhb52g96ig67dhj9d9b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Macros for axum")
    (description "Macros for axum")
    (license license:expat)))

(define-public rust-http-body-0.4
  (package
    (name "rust-http-body")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http-body" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l967qwwlvhp198xdrnc0p5d7jwfcp6q2lm510j6zqw4s4b8zwym"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/hyperium/http-body")
    (synopsis
     "Trait representing an asynchronous, streaming, HTTP request or response body.
")
    (description
     "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (license license:expat)))

(define-public rust-http-0.2
  (package
    (name "rust-http")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10j4jjpngaymxjvi92hllr2y6acr09pq61cvzxd44qzvkb4zyvmx"))))
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

(define-public rust-axum-core-0.3
  (package
    (name "rust-axum-core")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "axum-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kpwwn0kmbrvnvr2ga2vr81wmnz83f0l79kpm2wk92r41k45iydj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for axum")
    (description "Core types and traits for axum")
    (license license:expat)))

(define-public rust-axum-0.6
  (package
    (name "rust-axum")
    (version "0.6.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "axum" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14a49dwmkf1p1fj2w7znv0gaicqrhcgkxcylyg8yf7i2v77qr7rl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-axum-core" ,rust-axum-core-0.3)
                       ("rust-axum-macros" ,rust-axum-macros-0.3)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-http-body" ,rust-http-body-0.4)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-matchit" ,rust-matchit-0.7)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.18)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description "Web framework that focuses on ergonomics and modularity")
    (license license:expat)))

(define-public rust-atuin-server-13
  (package
    (name "rust-atuin-server")
    (version "13.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atuin-server" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17gsfb0mqjydcgr0nxkjr3yg7z8wisz4zn5cwab7w1fz6ynjcqkc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-common" ,rust-atuin-common-13)
                       ("rust-axum" ,rust-axum-0.6)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chronoutil" ,rust-chronoutil-0.2)
                       ("rust-config" ,rust-config-0.13)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sodiumoxide" ,rust-sodiumoxide-0.2)
                       ("rust-sqlx" ,rust-sqlx-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://atuin.sh")
    (synopsis "server library for atuin")
    (description "server library for atuin")
    (license license:expat)))

(define-public rust-urlencoding-2
  (package
    (name "rust-urlencoding")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "urlencoding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1agfwfzw66krnpqjiv4mhjqq1fcqgwdzikd7x9v835inz4kp9nz8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://lib.rs/urlencoding")
    (synopsis "A Rust library for doing URL percentage encoding.")
    (description
     "This package provides a Rust library for doing URL percentage encoding.")
    (license license:expat)))

(define-public rust-syn-1
  (package
    (name "rust-syn")
    (version "1.0.109")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "syn" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-macros-0.6
  (package
    (name "rust-sqlx-macros")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sqlx-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ja94162jvvi1p5kb60pvyxkrz6pg75nawlxn5sybrw9x55fcrlr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-either" ,rust-either-1)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.6)
                       ("rust-sqlx-rt" ,rust-sqlx-rt-0.6)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
     "Macros for SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-whoami-1
  (package
    (name "rust-whoami")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "whoami" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s4zdcmikx7blgavwkilw7skgz5h7i98jkl69v09qh6a29226w1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/ardaku/whoami/blob/stable/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment.")
    (description "Retrieve the current user and environment.")
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-futures-rustls-0.22
  (package
    (name "rust-futures-rustls")
    (version "0.22.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1g9725rg0rzf95wlqkgcmg1vcmpr2ncizwpa6j08rpwc0bniwhfj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-webpki" ,rust-webpki-0.22))))
    (home-page "https://github.com/quininer/futures-rustls")
    (synopsis "Asynchronous TLS/SSL streams for futures using Rustls.")
    (description "Asynchronous TLS/SSL streams for futures using Rustls.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-sqlx-rt-0.6
  (package
    (name "rust-sqlx-rt")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sqlx-rt" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0940ds08czarr1fgvaxmadkxjxd67jr88g16wsqn2kl9bwj3ykc0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-native-tls" ,rust-async-native-tls-0.4)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-futures-rustls" ,rust-futures-rustls-0.22)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Runtime abstraction used by SQLx, the Rust SQL toolkit. Not intended to be used directly.")
    (description
     "Runtime abstraction used by SQLx, the Rust SQL toolkit.  Not intended to be used
directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlformat-0.2
  (package
    (name "rust-sqlformat")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sqlformat" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gpf3a5yr53vhk8n54h1rz6igx87gis52w4bcws85nyik68vq4hc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itertools" ,rust-itertools-0.10)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-unicode-categories" ,rust-unicode-categories-0.1))))
    (home-page "https://github.com/shssoichiro/sqlformat-rs")
    (synopsis "Formats whitespace in a SQL string to make it easier to read")
    (description
     "Formats whitespace in a SQL string to make it easier to read")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha1-0.10
  (package
    (name "rust-sha1")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18zb80sxn31kxdpl1ly6w17hkrvyf08zbxnpy8ckb6f3h3f96hph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-sha1-asm" ,rust-sha1-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-1 hash function")
    (description "SHA-1 hash function")
    (license (list license:expat license:asl2.0))))

(define-public rust-ubyte-0.10
  (package
    (name "rust-ubyte")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ubyte" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rlg6sr14i3rd4kfhrwd7b7w7krlg6kpjxkd6vcx0si8gnp0s7y8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/SergioBenitez/ubyte")
    (synopsis
     "A simple, complete, const-everything, saturating, human-friendly, no_std library for byte units.
")
    (description
     "This package provides a simple, complete, const-everything, saturating,
human-friendly, no_std library for byte units.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-stream-0.1
  (package
    (name "rust-tokio-stream")
    (version "0.1.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-stream" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04rkkibd980z21wfpnc6l1mbj4hq6gf9yidsixd4kgsxy1s2pdcg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.
")
    (description "Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))

(define-public rust-oid-registry-0.4
  (package
    (name "rust-oid-registry")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oid-registry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0akbah3j8231ayrp2l1y5d9zmvbvqcsj0sa6s6dz6h85z8bhgqiq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3))))
    (home-page "https://github.com/rusticata/oid-registry")
    (synopsis "Object Identifier (OID) database")
    (description "Object Identifier (OID) database")
    (license (list license:expat license:asl2.0))))

(define-public rust-der-parser-7
  (package
    (name "rust-der-parser")
    (version "7.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10kfa2gzl3x20mwgrd43cyi79xgkqxyzcyrh0xylv4apa33qlfgy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4))))
    (home-page "https://github.com/rusticata/der-parser")
    (synopsis "Parser/encoder for ASN.1 BER/DER data")
    (description "Parser/encoder for ASN.1 BER/DER data")
    (license (list license:expat license:asl2.0))))

(define-public rust-displaydoc-0.2
  (package
    (name "rust-displaydoc")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "displaydoc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11i8p5snlc1hs4g5q3wiyr75dn276l6kr0si5m7xmfa6y31mvy9v"))))
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

(define-public rust-asn1-rs-impl-0.1
  (package
    (name "rust-asn1-rs-impl")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1va27bn7qxqp4wanzjlkagnynv6jnrhnwmcky2ahzb1r405p6xr7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Implementation details for the `asn1-rs` crate")
    (description "Implementation details for the `asn1-rs` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-asn1-rs-derive-0.1
  (package
    (name "rust-asn1-rs-derive")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gzf9vab06lk0zjvbr07axx64fndkng2s28bnj27fnwd548pb2yv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Derive macros for the `asn1-rs` crate")
    (description "Derive macros for the `asn1-rs` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-asn1-rs-0.3
  (package
    (name "rust-asn1-rs")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0czsk1nd4dx2k83f7jzkn8klx05wbmblkx1jh51i4c170akhbzrh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs-derive" ,rust-asn1-rs-derive-0.1)
                       ("rust-asn1-rs-impl" ,rust-asn1-rs-impl-0.1)
                       ("rust-bitvec" ,rust-bitvec-1)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Parser/encoder for ASN.1 BER/DER data")
    (description "Parser/encoder for ASN.1 BER/DER data")
    (license (list license:expat license:asl2.0))))

(define-public rust-x509-parser-0.13
  (package
    (name "rust-x509-parser")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x509-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "077bi0xyaa8cmrqf3rrw1z6kkzscwd1nxdxgs7mgz2ambg7bmfcz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-der-parser" ,rust-der-parser-7)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-oid-registry" ,rust-oid-registry-0.4)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/rusticata/x509-parser")
    (synopsis "Parser for the X.509 v3 format (RFC 5280 certificates)")
    (description "Parser for the X.509 v3 format (RFC 5280 certificates)")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-rustls-0.23
  (package
    (name "rust-tokio-rustls")
    (version "0.23.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nfsmmi8l1lgpbfy6079d5i13984djzcxrdr9jc06ghi0cwyhgn4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls" ,rust-rustls-0.20)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-webpki" ,rust-webpki-0.22))))
    (home-page "https://github.com/tokio-rs/tls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-state-0.5
  (package
    (name "rust-state")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "state" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fzji31ijbkimbzdy4dln9mp5xp7lm1a0dnqxv4n10hywphnds6v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-loom" ,rust-loom-0.5))))
    (home-page "https://github.com/SergioBenitez/state")
    (synopsis
     "A library for safe and effortless global and thread-local state management.
")
    (description
     "This package provides a library for safe and effortless global and thread-local
state management.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stable-pattern-0.1
  (package
    (name "rust-stable-pattern")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "stable-pattern" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i8hq82vm82mqj02qqcsd7caibrih7x5w3a1xpm8hpv30261cr25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-memchr" ,rust-memchr-2))))
    (home-page "https://github.com/SergioBenitez/stable-pattern")
    (synopsis "Stable port of std::str::Pattern and friends.")
    (description "Stable port of std::str::Pattern and friends.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-pemfile-1
  (package
    (name "rust-rustls-pemfile")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-pemfile" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16x5jhja8z0j5hcrlaqqz5qnyg9qgv8qqffwbdil6fl0b1nvb56i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description "Basic .pem file parser for keys and certificates")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-base64-0.21
  (package
    (name "rust-base64")
    (version "0.21.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "base64" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sidjip5b33sr6w7kasfj9qxpbda41nw0x4gjjk55g55a6mdv954"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/marshallpierce/rust-base64")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description "encodes and decodes base64 as bytes or utf8")
    (license (list license:expat license:asl2.0))))

(define-public rust-universal-hash-0.5
  (package
    (name "rust-universal-hash")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "universal-hash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dfqh2jnf4pz2cr9v4adpyxinz658vadlbwsjgigf6cs7jvn0cbx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits which describe the functionality of universal hash functions (UHFs)")
    (description
     "Traits which describe the functionality of universal hash functions (UHFs)")
    (license (list license:expat license:asl2.0))))

(define-public rust-polyval-0.6
  (package
    (name "rust-polyval")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "polyval" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iihmpn1h1ag5zl368yfq0jz1drfdw7xg7zpaqpcppqiikh39wky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-universal-hash" ,rust-universal-hash-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis
     "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing
a Message Authentication Code (MAC)
")
    (description
     "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing a
Message Authentication Code (MAC)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ghash-0.5
  (package
    (name "rust-ghash")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ghash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h1y3v3kj8xxkf2snv1yly0lr20fdh3jrm60p382szbiwl6pac6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-polyval" ,rust-polyval-0.6)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis
     "Universal hash over GF(2^128) useful for constructing a Message Authentication Code (MAC),
as in the AES-GCM authenticated encryption cipher.
")
    (description
     "Universal hash over GF(2^128) useful for constructing a Message Authentication
Code (MAC), as in the AES-GCM authenticated encryption cipher.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ctr-0.9
  (package
    (name "rust-ctr")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ctr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d88b73waamgpfjdml78icxz45d95q7vi2aqa604b0visqdfws83"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cipher" ,rust-cipher-0.4))))
    (home-page "https://github.com/RustCrypto/block-modes")
    (synopsis "CTR block modes of operation")
    (description "CTR block modes of operation")
    (license (list license:expat license:asl2.0))))

(define-public rust-block-padding-0.3
  (package
    (name "rust-block-padding")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "block-padding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y5v92alqzn9ikmyqfl3a4j6va87j967ii2n3jh2h330z4nyr40a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis "Padding and unpadding of messages divided into blocks.")
    (description "Padding and unpadding of messages divided into blocks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-inout-0.1
  (package
    (name "rust-inout")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "inout" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xf9gf09nc7y1a261xlfqsf66yn6mb81ahlzzyyd1934sr9hbhd0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block-padding" ,rust-block-padding-0.3)
                       ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
     "Custom reference types for code generic over in-place and buffer-to-buffer modes of operation.")
    (description
     "Custom reference types for code generic over in-place and buffer-to-buffer modes
of operation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cipher-0.4
  (package
    (name "rust-cipher")
    (version "0.4.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cipher" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-inout" ,rust-inout-0.1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for describing block ciphers and stream ciphers")
    (description "Traits for describing block ciphers and stream ciphers")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-0.8
  (package
    (name "rust-aes")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0haj74iqjjhxz4s6yh3v21s68snn74y93ji5d9bnr66921kzsg23"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/block-ciphers")
    (synopsis
     "Pure Rust implementation of the Advanced Encryption Standard (a.k.a. Rijndael)")
    (description
     "Pure Rust implementation of the Advanced Encryption Standard (a.k.a.  Rijndael)")
    (license (list license:expat license:asl2.0))))

(define-public rust-typenum-1
  (package
    (name "rust-typenum")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "typenum" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fhb9iaqyjn4dzn2vl86kxjhp4xpw5gynczlnqzf4x6rjgpn2ya9"))))
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

(define-public rust-aead-0.5
  (package
    (name "rust-aead")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aead" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j6pmc8pk4ha64bj9l6xzbhd85s2y1dblna2zsq83h0zy6w2w6aw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-heapless" ,rust-heapless-0.7))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms,
such as AES-GCM as ChaCha20Poly1305, which provide a high-level API
")
    (description
     "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms, such
as AES-GCM as ChaCha20Poly1305, which provide a high-level API")
    (license (list license:expat license:asl2.0))))

(define-public rust-aes-gcm-0.10
  (package
    (name "rust-aes-gcm")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aes-gcm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z2429v2d2wyf809h2wc4vwwibwypz3y4p7sn4kzkjb91ip3dqc2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-aes" ,rust-aes-0.8)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-ghash" ,rust-ghash-0.5)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis "Pure Rust implementation of the AES-GCM (Galois/Counter Mode)
Authenticated Encryption with Associated Data (AEAD) Cipher
with optional architecture-specific hardware acceleration
")
    (description
     "Pure Rust implementation of the AES-GCM (Galois/Counter Mode) Authenticated
Encryption with Associated Data (AEAD) Cipher with optional
architecture-specific hardware acceleration")
    (license (list license:asl2.0 license:expat))))

(define-public rust-cookie-0.17
  (package
    (name "rust-cookie")
    (version "0.17.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cookie" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "096c52jg9iq4lfcps2psncswv33fc30mmnaa2sbzzcfcw71kgyvy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes-gcm" ,rust-aes-gcm-0.10)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/SergioBenitez/cookie-rs")
    (synopsis
     "HTTP cookie parsing and cookie jar management. Supports signed and private
(encrypted, authenticated) jars.
")
    (description
     "HTTP cookie parsing and cookie jar management.  Supports signed and private
(encrypted, authenticated) jars.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-http-0.5
  (package
    (name "rust-rocket-http")
    (version "0.5.0-rc.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rocket_http" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1x4h411ldb59c6bq05r7dzi65xiqz7akd63zydkkm832j74i4q4k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cookie" ,rust-cookie-0.17)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pear" ,rust-pear-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-stable-pattern" ,rust-stable-pattern-0.1)
                       ("rust-state" ,rust-state-0.5)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-x509-parser" ,rust-x509-parser-0.13))))
    (home-page "https://rocket.rs")
    (synopsis
     "Types, traits, and parsers for HTTP requests, responses, and headers.
")
    (description
     "Types, traits, and parsers for HTTP requests, responses, and headers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bitflags-2
  (package
    (name "rust-bitflags")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bitflags" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hf0vwasgr3crjxx2794jnfkh0y9y7g4drkl0n5ynzg4rc7iwzs8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.
")
    (description
     "This package provides a macro to generate structures which behave like bitflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-devise-core-0.4
  (package
    (name "rust-devise-core")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "devise_core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sp5idq0idng9i5kwjd8slvc724s97r28arrhyqq1jpx1ax0vd9m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.10)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
     "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-devise-codegen-0.4
  (package
    (name "rust-devise-codegen")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "devise_codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mpy5mmsigkj5f72gby82yk4advcqj97am2wzn0dwkj8vnwg934w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-devise-core" ,rust-devise-core-0.4)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
     "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-devise-0.4
  (package
    (name "rust-devise")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "devise" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y45iag4hyvspkdsf6d856hf0ihf9vjnaga3c7y6c72l7zywxsnn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-devise-codegen" ,rust-devise-codegen-0.4)
                       ("rust-devise-core" ,rust-devise-core-0.4))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
     "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-codegen-0.5
  (package
    (name "rust-rocket-codegen")
    (version "0.5.0-rc.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rocket_codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12shzkr9zmc0v3r190nhcfavly28nngja2g4h94p93122hzkb4vh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-devise" ,rust-devise-0.4)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rocket-http" ,rust-rocket-http-0.5)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework.")
    (description "Procedural macros for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-slab-0.4
  (package
    (name "rust-slab")
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "slab" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bgwxig8gkqp6gz8rvrpdj6qwa10karnsxwx7wsj5ay8kcf3aa35"))))
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
    (version "0.7.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cp6yx4789j6gvbp4xnbk7lpd7q0j2a2qd4g1pg2b4q0afadh9sl"))))
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

(define-public rust-multer-2
  (package
    (name "rust-multer")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "multer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08nzrccp8n65qsddfshxbyn82zdmzg32i3gpgajx5jx4wy61km3f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-spin" ,rust-spin-0.9)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/rousan/multer-rs")
    (synopsis
     "An async parser for `multipart/form-data` content-type in Rust.")
    (description
     "An async parser for `multipart/form-data` content-type in Rust.")
    (license license:expat)))

(define-public rust-unsafe-libyaml-0.2
  (package
    (name "rust-unsafe-libyaml")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unsafe-libyaml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v7g7cgs05nviyavyka22xjs0avkyr5f15cnafzp8f7x592j885d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/unsafe-libyaml")
    (synopsis "libyaml transpiled to rust by c2rust")
    (description "libyaml transpiled to rust by c2rust")
    (license license:expat)))

(define-public rust-crossbeam-deque-0.8
  (package
    (name "rust-crossbeam-deque")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crossbeam-deque" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vqczbcild7nczh5z116w8w46z991kpjyw7qxkf24c14apwdcvyf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-rayon-core-0.5
  (package
    (name "rust-rustc-rayon-core")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zqbr87x58j2g9rgm2lc0254b6yqabb41jvddw99qd8fy2m8srk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Core APIs for Rayon - fork for rustc")
    (description "Core APIs for Rayon - fork for rustc")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-rayon-0.5
  (package
    (name "rust-rustc-rayon")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "040p2am25g3isnpsixrcrjrv70yz2lzkbq8gpv76xjipi3fam0gb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-rustc-rayon-core" ,rust-rustc-rayon-core-0.5))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Simple work-stealing parallelism for Rust - fork for rustc")
    (description "Simple work-stealing parallelism for Rust - fork for rustc")
    (license (list license:expat license:asl2.0))))

(define-public rust-indexmap-1
  (package
    (name "rust-indexmap")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "indexmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-autocfg" ,rust-autocfg-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.5)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-serde-yaml-0.9
  (package
    (name "rust-serde-yaml")
    (version "0.9.19")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_yaml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "046xygjf0dgp6apqvqkr59vkivz6r9xhcbb351rhm9bs0j66qbpq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unsafe-libyaml" ,rust-unsafe-libyaml-0.2))))
    (home-page "https://github.com/dtolnay/serde-yaml")
    (synopsis "YAML data format for Serde")
    (description "YAML data format for Serde")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-diagnostics-0.10
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-macro2-diagnostics" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fs9plv7xw3c986mfp5fkssladmb3gammxca60m95qhpb2ilnv30"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://github.com/SergioBenitez/proc-macro2-diagnostics")
    (synopsis "Diagnostics for proc-macro2.")
    (description "Diagnostics for proc-macro2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pear-codegen-0.2
  (package
    (name "rust-pear-codegen")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pear_codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g4xhy6gsxh40rva42fqaqlzzxl8gi6qw0c2m0p9zw4k7yjs6qcn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.10)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "")
    (synopsis "A (codegen) pear is a fruit.")
    (description "This package provides a (codegen) pear is a fruit.")
    (license (list license:expat license:asl2.0))))

(define-public rust-inlinable-string-0.1
  (package
    (name "rust-inlinable-string")
    (version "0.1.15")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "inlinable_string" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ysjci8yfvxgf51z0ny2nnwhxrclhmb3vbngin8v4bznhr3ybyn8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/fitzgen/inlinable_string")
    (synopsis
     "The `inlinable_string` crate provides the `InlinableString` type -- an owned, grow-able UTF-8 string that stores small strings inline and avoids heap-allocation -- and the `StringExt` trait which abstracts string operations over both `std::string::String` and `InlinableString` (or even your own custom string type).")
    (description
     "The `inlinable_string` crate provides the `InlinableString` type -- an owned,
grow-able UTF-8 string that stores small strings inline and avoids
heap-allocation -- and the `StringExt` trait which abstracts string operations
over both `std::string::String` and `InlinableString` (or even your own custom
string type).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pear-0.2
  (package
    (name "rust-pear")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pear" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s17f54y0qf6xvffardhq2lvg55n2hz0drc4a9bh6x88ly05dj8f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-inlinable-string" ,rust-inlinable-string-0.1)
                       ("rust-pear-codegen" ,rust-pear-codegen-0.2)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "")
    (synopsis "A pear is a fruit.")
    (description "This package provides a pear is a fruit.")
    (license (list license:expat license:asl2.0))))

(define-public rust-figment-0.10
  (package
    (name "rust-figment")
    (version "0.10.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "figment" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jbcq7y8695bjs61pkcpxrhqg6ssximacrpc1m0028lv8qmn0mjf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic" ,rust-atomic-0.5)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pear" ,rust-pear-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/SergioBenitez/Figment")
    (synopsis "A configuration library so con-free, it's unreal.")
    (description
     "This package provides a configuration library so con-free, it's unreal.")
    (license (list license:expat license:asl2.0))))

(define-public rust-binascii-0.1
  (package
    (name "rust-binascii")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "binascii" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wnaglgl72pn5ilv61q6y34w76gbg7crb8ifqk6lsxnq2gajjg9q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/naim94a/binascii-rs")
    (synopsis
     "Useful no-std binascii operations including base64, base32 and base16 (hex)")
    (description
     "Useful no-std binascii operations including base64, base32 and base16 (hex)")
    (license license:expat)))

(define-public rust-rocket-0.5
  (package
    (name "rust-rocket")
    (version "0.5.0-rc.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rocket" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jcwrkqvmbh1gwvg55kv6mdp8c9331hqzd45jq9gsp5f05s4ywsq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-binascii" ,rust-binascii-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-figment" ,rust-figment-0.10)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-multer" ,rust-multer-2)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-rocket-codegen" ,rust-rocket-codegen-0.5)
                       ("rust-rocket-http" ,rust-rocket-http-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-state" ,rust-state-0.5)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-ubyte" ,rust-ubyte-0.10)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://rocket.rs")
    (synopsis
     "Web framework with a focus on usability, security, extensibility, and speed.
")
    (description
     "Web framework with a focus on usability, security, extensibility, and speed.")
    (license (list license:expat license:asl2.0))))

(define-public rust-matrixmultiply-0.3
  (package
    (name "rust-matrixmultiply")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "matrixmultiply" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "112dbbjfiw4yji8vf94hk9hlhd9sa67qr3v0vkzfcx2hsd6mvn5d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rawpointer" ,rust-rawpointer-0.2)
                       ("rust-thread-tree" ,rust-thread-tree-0.3))))
    (home-page "https://github.com/bluss/matrixmultiply/")
    (synopsis
     "General matrix multiplication for f32 and f64 matrices. Operates on matrices with general layout (they can use arbitrary row and column stride). Detects and uses AVX or SSE2 on x86 platforms transparently for higher performance. Uses a microkernel strategy, so that the implementation is easy to parallelize and optimize.

Supports multithreading.")
    (description
     "General matrix multiplication for f32 and f64 matrices.  Operates on matrices
with general layout (they can use arbitrary row and column stride).  Detects and
uses AVX or SSE2 on x86 platforms transparently for higher performance.  Uses a
microkernel strategy, so that the implementation is easy to parallelize and
optimize.  Supports multithreading.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndarray-0.15
  (package
    (name "rust-ndarray")
    (version "0.15.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ndarray" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cpsm28hyk8qfjs4g9649dprv3hm53z12qqwyyjqbi3yjr72vcdd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-approx" ,rust-approx-0.4)
                       ("rust-approx" ,rust-approx-0.5)
                       ("rust-cblas-sys" ,rust-cblas-sys-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-matrixmultiply" ,rust-matrixmultiply-0.3)
                       ("rust-num-complex" ,rust-num-complex-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rawpointer" ,rust-rawpointer-0.2)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
     "An n-dimensional array for general elements and for numerics. Lightweight array views and slicing; views support chunking and splitting.")
    (description
     "An n-dimensional array for general elements and for numerics.  Lightweight array
views and slicing; views support chunking and splitting.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-macros-0.2
  (package
    (name "rust-time-macros")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dkgswnm5bj88wi80p94c9808dbdzis363fnwnw19a0xwxbsd07x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-time-core" ,rust-time-core-0.1))))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "    Procedural macros for the time crate.
    This crate is an implementation detail and should not be relied upon directly.
")
    (description
     "Procedural macros for the time crate.  This crate is an implementation detail
and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-core-0.1
  (package
    (name "rust-time-core")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z803zwzyh16nk3c4nmkw8v69nyj0r4v8s3yag68mvya38gkw59f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "This crate is an implementation detail and should not be relied upon directly.")
    (description
     "This crate is an implementation detail and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-threads-0.1
  (package
    (name "rust-num-threads")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "num_threads" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i5vmffsv6g79z869flp1sja69g1gapddjagdw1k3q9f3l2cw698"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/jhpratt/num_threads")
    (synopsis
     "A minimal library that determines the number of running threads for the current process.")
    (description
     "This package provides a minimal library that determines the number of running
threads for the current process.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.84")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-shared" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pcvk1c97r1pprzfaxxn359r0wqg5bm33ylbwgjh8f4cwbvzwih0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (description
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.84")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-backend" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ffc0wb293ha56i66f830x7f8aa2xql69a21lrasy1ncbgyr1klm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool
")
    (description "Backend code generation of the wasm-bindgen tool")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-support-0.2
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.84")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-macro-support" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xm56lpi0rihh8ny7x085dgs3jdm47spgqflb98wghyadwq83zra"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend-0.2)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (description
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in
the shared backend crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-0.2
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.84")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1idlq28awqhq8rclb22rn5xix82w9a4rgy11vkapzhzd1dygf8ac"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (description
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-0.2
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.84")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fx5gh0b4n6znfa3blz92wn1k4bbiysyq9m95s7rn3gk46ydry1i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro-0.2))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.
")
    (description "Easy support for interacting between JS and Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-js-sys-0.3
  (package
    (name "rust-js-sys")
    (version "0.3.61")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "js-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c075apyc5fxp2sbgr87qcvq53pcjxmp05l47lzlhpn5a0hxwpa4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (description
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.20")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "142qkacz9gby0693p70744zs37svxc2dj6mkbfkmx6yinknby36d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itoa" ,rust-itoa-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num-threads" ,rust-num-threads-0.1)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time-core" ,rust-time-core-0.1)
                       ("rust-time-macros" ,rust-time-macros-0.2))))
    (home-page "https://time-rs.github.io")
    (synopsis
     "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
     "Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))

(define-public rust-mysqlclient-sys-0.2
  (package
    (name "rust-mysqlclient-sys")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mysqlclient-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16wndr59cbpc2wgli45zfgi0hi837pbrsh1aqh2k0ads50akh6zn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/sgrif/mysqlclient-sys")
    (synopsis "Auto-generated rust bindings for libmysqlclient")
    (description "Auto-generated rust bindings for libmysqlclient")
    (license (list license:expat license:asl2.0))))

(define-public rust-ipnet-2
  (package
    (name "rust-ipnet")
    (version "2.7.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ipnet" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zxmnidy5qha1i384fzjfxcsi0qvkbcp730h26q4z3dg54hyxdhj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/krisprice/ipnet")
    (synopsis
     "Provides types and useful methods for working with IPv4 and IPv6 network addresses, commonly called IP prefixes. The new `IpNet`, `Ipv4Net`, and `Ipv6Net` types build on the existing `IpAddr`, `Ipv4Addr`, and `Ipv6Addr` types already provided in Rust's standard library and align to their design to stay consistent. The module also provides useful traits that extend `Ipv4Addr` and `Ipv6Addr` with methods for `Add`, `Sub`, `BitAnd`, and `BitOr` operations. The module only uses stable feature so it is guaranteed to compile using the stable toolchain.")
    (description
     "This package provides types and useful methods for working with IPv4 and IPv6
network addresses, commonly called IP prefixes.  The new `IpNet`, `Ipv4Net`, and
`Ipv6Net` types build on the existing `IpAddr`, `Ipv4Addr`, and `Ipv6Addr` types
already provided in Rust's standard library and align to their design to stay
consistent.  The module also provides useful traits that extend `Ipv4Addr` and
`Ipv6Addr` with methods for `Add`, `Sub`, `BitAnd`, and `BitOr` operations.  The
module only uses stable feature so it is guaranteed to compile using the stable
toolchain.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-derives-2
  (package
    (name "rust-diesel-derives")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "diesel_derives" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i0hjhk8fxlyb442yfqw8gnn1zkqfs9nfbqlvm7kvgl6y3f4zmqa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://diesel.rs")
    (synopsis
     "You should not use this crate directly, it is internal to Diesel.")
    (description
     "You should not use this crate directly, it is internal to Diesel.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-2
  (package
    (name "rust-diesel")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "diesel" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "010w0b8y9dm07rpgw8vj32xy78yxkgr40qadxh5ya5n934ms54a3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-diesel-derives" ,rust-diesel-derives-2)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-ipnetwork" ,rust-ipnetwork-0.17)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.23)
                       ("rust-mysqlclient-sys" ,rust-mysqlclient-sys-0.2)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pq-sys" ,rust-pq-sys-0.4)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-r2d2" ,rust-r2d2)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://diesel.rs")
    (synopsis
     "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL")
    (description
     "This package provides a safe, extensible ORM and Query Builder for PostgreSQL,
SQLite, and MySQL")
    (license (list license:expat license:asl2.0))))

(define-public rust-borsh-schema-derive-internal-0.10
  (package
    (name "rust-borsh-schema-derive-internal")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "borsh-schema-derive-internal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kaw1xdprb8chqj50c8gxjb5dadx1rac91zg8s81njpp8g60ahk3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "http://borsh.io")
    (synopsis "Schema Generator for Borsh
")
    (description "Schema Generator for Borsh")
    (license license:asl2.0)))

(define-public rust-borsh-derive-internal-0.10
  (package
    (name "rust-borsh-derive-internal")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "borsh-derive-internal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yx27ic6aal83bdi1h6v80wfs9ixvw51qzmdgcn8sn8rd4akid5g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license license:asl2.0)))

(define-public rust-borsh-derive-0.10
  (package
    (name "rust-borsh-derive")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "borsh-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xslbx3qj531aq8ny1bkr45ibjmpsx0szsfc57rm33akj4v62m07"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-borsh-derive-internal" ,rust-borsh-derive-internal-0.10)
                       ("rust-borsh-schema-derive-internal" ,rust-borsh-schema-derive-internal-0.10)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license license:asl2.0)))

(define-public rust-borsh-0.10
  (package
    (name "rust-borsh")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "borsh" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sq4l9jfik5dmpy1islcj40bing1jkji2q1qbrkvq1d02n92f521"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-borsh-derive" ,rust-borsh-derive-0.10)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.12))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-decimal-1
  (package
    (name "rust-rust-decimal")
    (version "1.29.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rust_decimal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z3d14mzqz28s635wyl93ibzl2gxnscg25r8xjsizvk10nv3dg96"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-borsh" ,rust-borsh-0.10)
                       ("rust-bytecheck" ,rust-bytecheck-0.6)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-diesel" ,rust-diesel-2)
                       ("rust-diesel" ,rust-diesel-1)
                       ("rust-ndarray" ,rust-ndarray-0.15)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-postgres" ,rust-postgres-0.19)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rocket" ,rust-rocket-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio-postgres" ,rust-tokio-postgres-0.7))))
    (home-page "https://github.com/paupino/rust-decimal")
    (synopsis
     "Decimal number implementation written in pure Rust suitable for financial and fixed-precision calculations.")
    (description
     "Decimal number implementation written in pure Rust suitable for financial and
fixed-precision calculations.")
    (license license:expat)))

(define-public rust-sha2-0.9
  (package
    (name "rust-sha2")
    (version "0.9.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "006q2f0ar26xcjxqz8zsncfgz86zqa5dkwlwv03rhx1rpzhs2n2d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block-buffer" ,rust-block-buffer-0.9)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-digest" ,rust-digest-0.9)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-sha2-asm" ,rust-sha2-asm-0.6))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "Pure Rust implementation of the SHA-2 hash function family
including SHA-224, SHA-256, SHA-384, and SHA-512.
")
    (description
     "Pure Rust implementation of the SHA-2 hash function family including SHA-224,
SHA-256, SHA-384, and SHA-512.")
    (license (list license:expat license:asl2.0))))

(define-public rust-spki-0.5
  (package
    (name "rust-spki")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "spki" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09qaddm4kw01xm9638910bm4yqnshzh2p38lvc3kxkvc5b01ml24"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-der" ,rust-der-0.5)
                       ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/spki")
    (synopsis
     "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their
associated AlgorithmIdentifiers (i.e. OIDs)
")
    (description
     "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their
associated AlgorithmIdentifiers (i.e.  OIDs)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs5-0.4
  (package
    (name "rust-pkcs5")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xhyi3k5p6lxb28ivcd1f3skdbmhzk0gamfry7q56pifx9xi8g6n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.7)
                       ("rust-block-modes" ,rust-block-modes-0.8)
                       ("rust-der" ,rust-der-0.5)
                       ("rust-des" ,rust-des-0.7)
                       ("rust-hmac" ,rust-hmac-0.11)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.9)
                       ("rust-scrypt" ,rust-scrypt-0.8)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-spki" ,rust-spki-0.5))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs5")
    (synopsis
     "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #5:
Password-Based Cryptography Specification Version 2.1 (RFC 8018)
")
    (description
     "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #5:
Password-Based Cryptography Specification Version 2.1 (RFC 8018)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs8-0.8
  (package
    (name "rust-pkcs8")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs8" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l29h4mrgi2kpsl98jzky3ni5by3xa1sc6db9yd8l1i1p0zxmavw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-der" ,rust-der-0.5)
                       ("rust-pkcs5" ,rust-pkcs5-0.4)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-spki" ,rust-spki-0.5)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs8")
    (synopsis
     "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #8:
Private-Key Information Syntax Specification (RFC 5208), with additional
support for PKCS#8v2 asymmetric key packages (RFC 5958)
")
    (description
     "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #8:
Private-Key Information Syntax Specification (RFC 5208), with additional support
for PKCS#8v2 asymmetric key packages (RFC 5958)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pem-rfc7468-0.3
  (package
    (name "rust-pem-rfc7468")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pem-rfc7468" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c7vrrksg8fqzxb7q4clzl14f0qnqky7jqspjqi4pailiybmvph1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pem-rfc7468")
    (synopsis
     "PEM Encoding (RFC 7468) for PKIX, PKCS, and CMS Structures, implementing a
strict subset of the original Privacy-Enhanced Mail encoding intended
specifically for use with cryptographic keys, certificates, and other messages.
Provides a no_std-friendly, constant-time implementation suitable for use with
cryptographic private keys.
")
    (description
     "PEM Encoding (RFC 7468) for PKIX, PKCS, and CMS Structures, implementing a
strict subset of the original Privacy-Enhanced Mail encoding intended
specifically for use with cryptographic keys, certificates, and other messages.
Provides a no_std-friendly, constant-time implementation suitable for use with
cryptographic private keys.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-der-derive-0.5
  (package
    (name "rust-der-derive")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zw4p6yqklv4i76ms2a0gcmna648337r379d5ljgpbir5cyqylrs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der/derive")
    (synopsis
     "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (description
     "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (license (list license:asl2.0 license:expat))))

(define-public rust-crypto-bigint-0.3
  (package
    (name "rust-crypto-bigint")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crypto-bigint" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08gx92sj93hk2smqy4nvk8lmpjjjqm7a9ps22q3pxqqxzbas3ih3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rlp" ,rust-rlp-0.5)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/crypto-bigint")
    (synopsis
     "Pure Rust implementation of a big integer library which has been designed from
the ground-up for use in cryptographic applications. Provides constant-time,
no_std-friendly implementations of modern formulas using const generics.
")
    (description
     "Pure Rust implementation of a big integer library which has been designed from
the ground-up for use in cryptographic applications.  Provides constant-time,
no_std-friendly implementations of modern formulas using const generics.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-const-oid-0.7
  (package
    (name "rust-const-oid")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "const-oid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wwl3cncd8p2fa54vzmghflh4nh9ml02xfbv38nf5ziifh28riz4"))))
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

(define-public rust-der-0.5
  (package
    (name "rust-der")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0p3h7nszn7jhjacpmkjrcyx5g8p3ma1qhxfy3397m7l3fdfq26b9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-const-oid" ,rust-const-oid-0.7)
                       ("rust-crypto-bigint" ,rust-crypto-bigint-0.3)
                       ("rust-der-derive" ,rust-der-derive-0.5)
                       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.3)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der")
    (synopsis
     "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets
")
    (description
     "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs1-0.3
  (package
    (name "rust-pkcs1")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pkcs1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0813szfx13n4xl6l19m3lwj7pqgljqwc6ipxhr2dv0yc9k06d3x7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-der" ,rust-der-0.5)
                       ("rust-pkcs8" ,rust-pkcs8-0.8)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs1")
    (synopsis
     "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #1:
RSA Cryptography Specifications Version 2.2 (RFC 8017)
")
    (description
     "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #1: RSA
Cryptography Specifications Version 2.2 (RFC 8017)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-smallvec-1
  (package
    (name "rust-smallvec")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "smallvec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q2k15fzxgwjpcdv3f323w24rbbfyv711ayz85ila12lg7zbw1x5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/servo/rust-smallvec")
    (synopsis
     "'Small vector' optimization: store up to a small number of items on the stack")
    (description
     "Small vector optimization: store up to a small number of items on the stack")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-bigint-dig-0.8
  (package
    (name "rust-num-bigint-dig")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "num-bigint-dig" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01b9lnqkjgwr1fv8jlw8w8y8pf70h2h9panq969r0pxw793ck693"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libm" ,rust-libm-0.2)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-iter" ,rust-num-iter-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/dignifiedquire/num-bigint")
    (synopsis "Big integer implementation for Rust")
    (description "Big integer implementation for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-rsa-0.6
  (package
    (name "rust-rsa")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02viiiylxpk2hx5h5qrpm4lcd8ildvafbw0rn6rx44wnqia2gwjc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-iter" ,rust-num-iter-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pkcs1" ,rust-pkcs1-0.3)
                       ("rust-pkcs8" ,rust-pkcs8-0.8)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/RSA")
    (synopsis "Pure Rust RSA implementation")
    (description "Pure Rust RSA implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-paste-1
  (package
    (name "rust-paste")
    (version "1.0.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "paste" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ybxr9wjw3fi0ha008cqfx08vk1iakqq5pbl77i3zym8cm06qx4z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis "Macros for all your token pasting needs")
    (description "Macros for all your token pasting needs")
    (license (list license:expat license:asl2.0))))

(define-public rust-mac-address-1
  (package
    (name "rust-mac-address")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mac_address" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qz91b6b4gdfpfq0z6fzkzdrndyx10dys2347ijvg0l3bhiy6f5j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-nix" ,rust-nix-0.23)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rep-nop/mac_address")
    (synopsis "Cross-platform retrieval of a network interface MAC address.")
    (description
     "Cross-platform retrieval of a network interface MAC address.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libsqlite3-sys-0.24
  (package
    (name "rust-libsqlite3-sys")
    (version "0.24.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libsqlite3-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "054xxfz7w4xy2iyqpv4g9vajwrkcxc0sgi7vq4y4bl67f3jlb1w9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Native bindings to the libsqlite3 library")
    (description "Native bindings to the libsqlite3 library")
    (license license:expat)))

(define-public rust-ipnetwork-0.19
  (package
    (name "rust-ipnetwork")
    (version "0.19.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ipnetwork" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09yil123s063qqn9n9m6fg2v9rbgzlp9lkjs40zpbwq64rhz310z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/achanda/ipnetwork")
    (synopsis "A library to work with IP CIDRs in Rust")
    (description
     "This package provides a library to work with IP CIDRs in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-hmac-0.12
  (package
    (name "rust-hmac")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hmac" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis
     "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
     "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (license (list license:expat license:asl2.0))))

(define-public rust-hkdf-0.12
  (package
    (name "rust-hkdf")
    (version "0.12.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hkdf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dyl16cf15hka32hv3l7dwgr3xj3brpfr27iyrbpdhlzdfgh46kr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hmac" ,rust-hmac-0.12))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
     "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-hashlink-0.8
  (package
    (name "rust-hashlink")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hashlink" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yhgpv6k8pr7d3gp89gqcgxk2diai6gk4j05mmhdhy22ig7izzk9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/kyren/hashlink")
    (synopsis
     "HashMap-like containers that hold their key-value pairs in a user controllable order")
    (description
     "HashMap-like containers that hold their key-value pairs in a user controllable
order")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-intrusive-0.4
  (package
    (name "rust-futures-intrusive")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-intrusive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mdf2qb6ayfi19l7qbqi7zp62wyyibfgmc93flrh70dziykgf156"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.11))))
    (home-page "https://github.com/Matthias247/futures-intrusive")
    (synopsis
     "Futures based on intrusive data structures - for std and no-std environments.
")
    (description
     "Futures based on intrusive data structures - for std and no-std environments.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-macro-0.3
  (package
    (name "rust-futures-macro")
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dk3m7jpbj9qkxcwmqxnan1pli23hhcjqzwpp2wdh6v36zclxc9y"))))
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
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-io" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14cbfwn1d9qzdzys7vjpx9c85javpdx0iavllpf40fxy7kx25m49"))))
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

(define-public rust-futures-util-0.3
  (package
    (name "rust-futures-util")
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-util" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1aw45s2adi1asd6827ya6n4i43wps4wqzv8z462yq0qm91zb3xiy"))))
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

(define-public rust-futures-task-0.3
  (package
    (name "rust-futures-task")
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-task" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y98b6mkb3svh0zvs1axmka11bgfd8pc2f0459a1cyxk6c6m8rgx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.
")
    (description "Tools for working with tasks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-executor-0.3
  (package
    (name "rust-futures-executor")
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-executor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10qgp9hfvwvywx7ylhij12n4yyfm3r678lk2qwswknjcyyfxv5qr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-task" ,rust-futures-task-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
     "Executors for asynchronous tasks based on the futures-rs library.
")
    (description
     "Executors for asynchronous tasks based on the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-sink-0.3
  (package
    (name "rust-futures-sink")
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-sink" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wnjp3h90mm6qfq59fy26ayrvbncy3hrsp481alb5bzc98x0i4zc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.
")
    (description "The asynchronous `Sink` trait for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-portable-atomic-1
  (package
    (name "rust-portable-atomic")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portable-atomic" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ibcxhk65yf9xamyrwiiv9v3cz4068p09m5lx3wf8j4sw38s00pn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-critical-section" ,rust-critical-section-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/taiki-e/portable-atomic")
    (synopsis
     "Portable atomic types including support for 128-bit atomics, atomic float, etc.
")
    (description
     "Portable atomic types including support for 128-bit atomics, atomic float, etc.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-futures-core-0.3
  (package
    (name "rust-futures-core")
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pb9vsvh6xnv0xpzj73g6wl1357nzacyxq0x96n3sdknmb0s1mw6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-portable-atomic" ,rust-portable-atomic-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.
")
    (description "The core traits and types in for the `futures` library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-channel-0.3
  (package
    (name "rust-futures-channel")
    (version "0.3.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "futures-channel" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b7p7jb1bbwddqs3k4vg70jq1hdb7i1knzgdn6sfghywl2ji6iqn"))))
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

(define-public rust-zeroize-1
  (package
    (name "rust-zeroize")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zeroize" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ndar43r58zbmasjhrhgas168vxb4i0rwbkcnszhjybwpbqmc29a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (description
     "Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
optimized away by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))

(define-public rust-nanorand-0.7
  (package
    (name "rust-nanorand")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nanorand" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hr60b8zlfy7mxjcwx2wfmhpkx7vfr3v9x12shmv1c10b0y32lba"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/Absolucy/nanorand-rs")
    (synopsis "A tiny, fast, zero-dep library for random number generation.")
    (description
     "This package provides a tiny, fast, zero-dep library for random number
generation.")
    (license license:zlib)))

(define-public rust-flume-0.10
  (package
    (name "rust-flume")
    (version "0.10.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "flume" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xvm1wpzkjvf99jxy9jp3dxw5nipa9blg7j0ngvxj0rl3i2b8mqn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-nanorand" ,rust-nanorand-0.7)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-spin" ,rust-spin-0.9))))
    (home-page "https://github.com/zesterer/flume")
    (synopsis "A blazingly fast multi-producer channel")
    (description
     "This package provides a blazingly fast multi-producer channel")
    (license (list license:asl2.0 license:expat))))

(define-public rust-event-listener-2
  (package
    (name "rust-event-listener")
    (version "2.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "event-listener" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q4w3pndc518crld6zsqvvpy9lkzwahp2zgza9kbzmmqh9gif1h2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/smol-rs/event-listener")
    (synopsis "Notify async tasks or threads")
    (description "Notify async tasks or threads")
    (license (list license:asl2.0 license:expat))))

(define-public rust-encoding-rs-0.8
  (package
    (name "rust-encoding-rs")
    (version "0.8.32")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "encoding_rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "155kf4mxq7h0mmh0jwx21mpxw7h405pp9jlanmq36h45xvs326h7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://docs.rs/encoding_rs/")
    (synopsis "A Gecko-oriented implementation of the Encoding Standard")
    (description
     "This package provides a Gecko-oriented implementation of the Encoding Standard")
    (license (list license:bsd-3))))

(define-public rust-dotenvy-0.15
  (package
    (name "rust-dotenvy")
    (version "0.15.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dotenvy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16s3n973n5aqym02692i1npb079n5mb0fwql42ikmwn8wnrrbbqs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-3))))
    (home-page "https://github.com/allan2/dotenvy")
    (synopsis "A well-maintained fork of the dotenv crate")
    (description
     "This package provides a well-maintained fork of the dotenv crate")
    (license license:expat)))

(define-public rust-crc-catalog-2
  (package
    (name "rust-crc-catalog")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crc-catalog" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "115l7pzskv5xzp9i7146rp1qrbfdi7gikig1p80p6zpham7fib4w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/akhilles/crc-catalog.git")
    (synopsis
     "Catalog of CRC algorithms (generated from http://reveng.sourceforge.net/crc-catalogue) expressed as simple Rust structs.")
    (description
     "Catalog of CRC algorithms (generated from
http://reveng.sourceforge.net/crc-catalogue) expressed as simple Rust structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crc-3
  (package
    (name "rust-crc")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zkx87a5x06xfd6xm5956w4vmdfs0wcxpsn7iwj5jbp2rcapmv46"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crc-catalog" ,rust-crc-catalog-2))))
    (home-page "https://github.com/mrhooray/crc-rs.git")
    (synopsis "Rust implementation of CRC with support of various standards")
    (description
     "Rust implementation of CRC with support of various standards")
    (license (list license:expat license:asl2.0))))

(define-public rust-atoi-1
  (package
    (name "rust-atoi")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atoi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13mycnr954w17lcvvbpzr4rmhl1h13cg8hq63j0rrx9g6497vifp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/pacman82/atoi-rs")
    (synopsis "Parse integers directly from `[u8]` slices in safe code")
    (description "Parse integers directly from `[u8]` slices in safe code")
    (license license:expat)))

(define-public rust-ahash-0.7
  (package
    (name "rust-ahash")
    (version "0.7.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ahash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0isw672fiwx8cjl040jrck6pi85xcszkz6q0xsqkiy6qjl31mdgw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-const-random" ,rust-const-random-0.1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
     "A non-cryptographic hash function using AES-NI for high performance")
    (description
     "This package provides a non-cryptographic hash function using AES-NI for high
performance")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-core-0.6
  (package
    (name "rust-sqlx-core")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sqlx-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ab04inkrf0jc5n2nxdjj2czz67kvvcygxzzllxg78w3794430ps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.7)
                       ("rust-atoi" ,rust-atoi-1)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bit-vec" ,rust-bit-vec-0.6)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bstr" ,rust-bstr-0.2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-dirs" ,rust-dirs-4)
                       ("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-either" ,rust-either-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-event-listener" ,rust-event-listener-2)
                       ("rust-flume" ,rust-flume-0.10)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-intrusive" ,rust-futures-intrusive-0.4)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-git2" ,rust-git2-0.14)
                       ("rust-hashlink" ,rust-hashlink-0.8)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-ipnetwork" ,rust-ipnetwork-0.19)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.24)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mac-address" ,rust-mac-address-1)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rsa" ,rust-rsa-0.6)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sqlformat" ,rust-sqlformat-0.2)
                       ("rust-sqlx-rt" ,rust-sqlx-rt-0.6)
                       ("rust-stringprep" ,rust-stringprep-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Core of SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
     "Core of SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-0.6
  (package
    (name "rust-sqlx")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sqlx" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12016l2h90qp386jsr99pp0s6mdzchg64kwm9baqx1r5m41kpppq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-sqlx-core" ,rust-sqlx-core-0.6)
                       ("rust-sqlx-macros" ,rust-sqlx-macros-0.6))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "ð§° The Rust SQL Toolkit. An async, pure Rust SQL crate featuring compile-time checked queries without a DSL. Supports PostgreSQL, MySQL, and SQLite.")
    (description
     "ð§° The Rust SQL Toolkit.  An async, pure Rust SQL crate featuring compile-time
checked queries without a DSL. Supports PostgreSQL, MySQL, and SQLite.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sql-builder-3
  (package
    (name "rust-sql-builder")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sql-builder" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h5xp47zz9chv545lpmal51fq3z162z2f99mb4lhcbgcsaaqs05i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/perdumonocle/sql-builder.git")
    (synopsis "Simple SQL code generator.")
    (description "Simple SQL code generator.")
    (license license:expat)))

(define-public rust-libsodium-sys-0.2
  (package
    (name "rust-libsodium-sys")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libsodium-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zcjka23grayr8kjrgbada6vwagp0kkni9m45v0gpbanrn3r6xvb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/sodiumoxide/sodiumoxide.git")
    (synopsis "FFI binding to libsodium")
    (description "FFI binding to libsodium")
    (license (list license:expat license:asl2.0))))

(define-public rust-sodiumoxide-0.2
  (package
    (name "rust-sodiumoxide")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sodiumoxide" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0a00rcp2vphrs8qh0477rzs6lhsng1m5i0l4qamagnf2nsnf6sz2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ed25519" ,rust-ed25519-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libsodium-sys" ,rust-libsodium-sys-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/sodiumoxide/sodiumoxide")
    (synopsis "Fast cryptographic library for Rust (bindings to libsodium)")
    (description "Fast cryptographic library for Rust (bindings to libsodium)")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-regex-1
  (package
    (name "rust-serde-regex")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_regex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tailhook/serde-regex")
    (synopsis "    A serde wrapper that (de)serializes regex as strings
")
    (description
     "This package provides a serde wrapper that (de)serializes regex as strings")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.95")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_json" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i273701q76j01rkcl962ky51n84h1y8f0dpfxvsl0n8galyq8fp"))))
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

(define-public rust-semver-1
  (package
    (name "rust-semver")
    (version "1.0.17")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "semver" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vf03d99sikkradjj33q1l9h4fqgd1h7darjypic6pnh4qrkdgdy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dtolnay/semver")
    (synopsis "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (description
     "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (license (list license:expat license:asl2.0))))

(define-public rust-rmp-0.8
  (package
    (name "rust-rmp")
    (version "0.8.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rmp" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17rw803xv84csxgd654g7q64kqf9zgkvhsn8as3dbmlg6mr92la4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Pure Rust MessagePack serialization implementation")
    (description "Pure Rust MessagePack serialization implementation")
    (license license:expat)))

(define-public rust-rmp-serde-1
  (package
    (name "rust-rmp-serde")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rmp-serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0glisa0pcj56dhsaqp5vkqkcqqnb2dcal8kjzf50n8p0jbhkpcf5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-rmp" ,rust-rmp-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Serde bindings for RMP")
    (description "Serde bindings for RMP")
    (license license:expat)))

(define-public rust-parse-duration-2
  (package
    (name "rust-parse-duration")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "parse_duration" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pd97dmlv1i6pvr2byi65q1fzv667gvhnf3ld2lsawh17vlyadvh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num" ,rust-num-0.2)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/zeta12ti/parse_duration/")
    (synopsis "Parses a duration from a string.")
    (description "Parses a duration from a string.")
    (license license:expat)))

(define-public rust-minspan-0.1
  (package
    (name "rust-minspan")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minspan" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s7lh0ryq0kk6sm6z5f2ikqq437xca0gzc61ds80pbh8qdxa2s8j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "")
    (synopsis
     "a package for determining the minimum span of one vector within another")
    (description
     "a package for determining the minimum span of one vector within another")
    (license license:expat)))

(define-public rust-memchr-2
  (package
    (name "rust-memchr")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "memchr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vanfk5mzs1g1syqnj03q8n0syggnhn55dq535h2wxr7rwpfbzrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense license:expat))))

(define-public rust-itertools-0.10
  (package
    (name "rust-itertools")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "itertools" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-derive-0.12
  (package
    (name "rust-logos-derive")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "logos-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v295x78vcskab88hshl530w9d1vn61cmlaic4d6dydsila4kn51"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-beef" ,rust-beef-0.5)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.6)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/maciejhirsz/logos")
    (synopsis "Create ridiculously fast Lexers")
    (description "Create ridiculously fast Lexers")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-0.12
  (package
    (name "rust-logos")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "logos" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w82qm3hck5cr6ax3j3yzrpf4zzbffahz126ahyqwyn6h8b072xz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-logos-derive" ,rust-logos-derive-0.12))))
    (home-page "https://github.com/maciejhirsz/logos")
    (synopsis "Create ridiculously fast Lexers")
    (description "Create ridiculously fast Lexers")
    (license (list license:expat license:asl2.0))))

(define-public rust-tinyvec-1
  (package
    (name "rust-tinyvec")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tinyvec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l6bl2h62a5m44jdnpn7lmj14rd44via8180i7121fvm73mmrk47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tinyvec-macros" ,rust-tinyvec-macros-0.1))))
    (home-page "https://github.com/Lokathor/tinyvec")
    (synopsis "`tinyvec` provides 100% safe vec-like data structures.")
    (description "`tinyvec` provides 100% safe vec-like data structures.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-rkyv-derive-0.7
  (package
    (name "rust-rkyv-derive")
    (version "0.7.40")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rkyv_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xp49id7ql4afl80ws0a4wx58cvj7h7ad1hb92ljmz2dginfs9pz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Derive macro for rkyv")
    (description "Derive macro for rkyv")
    (license license:expat)))

(define-public rust-rend-0.4
  (package
    (name "rust-rend")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rend" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1av8mfxrc6dvm0hmn2ymi56jrv6a7dqssxwdn1zx6h4j1790h42q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytecheck" ,rust-bytecheck-0.6))))
    (home-page "https://github.com/djkoloski/rend")
    (synopsis "Endian-aware primitives for Rust")
    (description "Endian-aware primitives for Rust")
    (license license:expat)))

(define-public rust-bytecheck-derive-0.6
  (package
    (name "rust-bytecheck-derive")
    (version "0.6.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytecheck_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r9dlkx5w1p8d5gif2yvn6bz1856yij2fxi4wakq2vxl7ia2a4p3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/djkoloski/bytecheck")
    (synopsis "Derive macro for bytecheck")
    (description "Derive macro for bytecheck")
    (license license:expat)))

(define-public rust-bytecheck-0.6
  (package
    (name "rust-bytecheck")
    (version "0.6.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytecheck" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07vxs7y441f7d6mjzmli80ykmfajwk9jqci549b29sr319j13zhk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytecheck-derive" ,rust-bytecheck-derive-0.6)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/djkoloski/bytecheck")
    (synopsis "Derive macro for bytecheck")
    (description "Derive macro for bytecheck")
    (license license:expat)))

(define-public rust-wyz-0.5
  (package
    (name "rust-wyz")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wyz" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-tap" ,rust-tap-1)
                       ("rust-typemap" ,rust-typemap-0.3))))
    (home-page "https://myrrlyn.net/crates/wyz")
    (synopsis "myrrlynâs utility collection")
    (description "myrrlynâs utility collection")
    (license license:expat)))

(define-public rust-radium-0.7
  (package
    (name "rust-radium")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "radium" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://ferrilab.github.io/ferrilab")
    (synopsis "Portable interfaces for maybe-atomic types")
    (description "Portable interfaces for maybe-atomic types")
    (license license:expat)))

(define-public rust-funty-2
  (package
    (name "rust-funty")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "funty" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://ferrilab.github.io/ferrilab")
    (synopsis "Trait generalization over the primitive types")
    (description "Trait generalization over the primitive types")
    (license license:expat)))

(define-public rust-bitvec-1
  (package
    (name "rust-bitvec")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bitvec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-funty" ,rust-funty-2)
                       ("rust-radium" ,rust-radium-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tap" ,rust-tap-1)
                       ("rust-wyz" ,rust-wyz-0.5))))
    (home-page "https://bitvecto-rs.github.io/bitvec")
    (synopsis "Addresses memory by bits, for packed collections and bitfields")
    (description
     "Addresses memory by bits, for packed collections and bitfields")
    (license license:expat)))

(define-public rust-rkyv-0.7
  (package
    (name "rust-rkyv")
    (version "0.7.40")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rkyv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hsd81bkm1rvh3kl66fhnadji4h88w2pgf0yrp4cnqdav52is3y3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bitvec" ,rust-bitvec-1)
                       ("rust-bytecheck" ,rust-bytecheck-0.6)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.1)
                       ("rust-rend" ,rust-rend-0.4)
                       ("rust-rkyv-derive" ,rust-rkyv-derive-0.7)
                       ("rust-seahash" ,rust-seahash-4)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Zero-copy deserialization framework for Rust")
    (description "Zero-copy deserialization framework for Rust")
    (license license:expat)))

(define-public rust-windows-interface-0.46
  (package
    (name "rust-windows-interface")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-interface" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0a87zxh3wq5ng1vvgqf7jhydsshrpc5w39pyvr0l1vyv3q5k67xc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the windows crate")
    (description "The interface macro for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-implement-0.46
  (package
    (name "rust-windows-implement")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-implement" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09kw706qcms5yy34lf714zspj34v8jirfxv7yycavpcsa9czpd69"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description "The implement macro for the windows crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.46
  (package
    (name "rust-windows")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "098crdz9gxgy3614ygznqqhn28q097r498b3rg35584nd8gb9b6d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-implement" ,rust-windows-implement-0.46)
                       ("rust-windows-interface" ,rust-windows-interface-0.46)
                       ("rust-windows-targets" ,rust-windows-targets-0.42))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "Rust for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-iana-time-zone-haiku-0.1
  (package
    (name "rust-iana-time-zone-haiku")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "iana-time-zone-haiku" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jix9qrqxclj9r4wkg7d3fr987d77vdg3qy2c5hl4ry19wlaw0q7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cxx" ,rust-cxx-1)
                       ("rust-cxx-build" ,rust-cxx-build-1))))
    (home-page "https://github.com/strawlab/iana-time-zone")
    (synopsis "iana-time-zone support crate for Haiku OS")
    (description "iana-time-zone support crate for Haiku OS")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-sys-0.8
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p5r2wckarkpkyc4z83q08dwpvcafrb1h6fxfa3qnikh8szww9sq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-system-properties-0.1
  (package
    (name "rust-android-system-properties")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "android_system_properties" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/nical/android_system_properties")
    (synopsis "Minimal Android system properties wrapper")
    (description "Minimal Android system properties wrapper")
    (license (list license:expat license:asl2.0))))

(define-public rust-iana-time-zone-0.1
  (package
    (name "rust-iana-time-zone")
    (version "0.1.54")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "iana-time-zone" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v9yhwp0g5mbzf4k3k2d8x11q2ghwxci3hamy39gi6bfg1vcq5qc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-system-properties" ,rust-android-system-properties-0.1)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-iana-time-zone-haiku" ,rust-iana-time-zone-haiku-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-windows" ,rust-windows-0.46))))
    (home-page "https://github.com/strawlab/iana-time-zone")
    (synopsis "get the IANA time zone for the current system")
    (description "get the IANA time zone for the current system")
    (license (list license:expat license:asl2.0))))

(define-public rust-criterion-plot-0.5
  (package
    (name "rust-criterion-plot")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "criterion-plot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cast" ,rust-cast-0.3)
                       ("rust-itertools" ,rust-itertools-0.10))))
    (home-page "https://github.com/bheisler/criterion.rs")
    (synopsis "Criterion's plotting library")
    (description "Criterion's plotting library")
    (license (list license:expat license:asl2.0))))

(define-public rust-ciborium-ll-0.2
  (package
    (name "rust-ciborium-ll")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ciborium-ll" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06ygqh33k3hp9r9mma43gf189b6cyq62clk65f4w1q54nni30c11"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ciborium-io" ,rust-ciborium-io-0.2)
                       ("rust-half" ,rust-half-1))))
    (home-page "https://github.com/enarx/ciborium")
    (synopsis "Low-level CBOR codec primitives")
    (description "Low-level CBOR codec primitives")
    (license license:asl2.0)))

(define-public rust-ciborium-io-0.2
  (package
    (name "rust-ciborium-io")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ciborium-io" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sdkk7l7pqi2nsbm9c6g8im1gb1qdd83l25ja9xwhg07mx9yfv9l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/enarx/ciborium")
    (synopsis "Simplified Read/Write traits for no_std usage")
    (description "Simplified Read/Write traits for no_std usage")
    (license license:asl2.0)))

(define-public rust-ciborium-0.2
  (package
    (name "rust-ciborium")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ciborium" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13vqkm88kaq8nvxhaj6qsl0gsc16rqsin014fx5902y6iib3ghdh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ciborium-io" ,rust-ciborium-io-0.2)
                       ("rust-ciborium-ll" ,rust-ciborium-ll-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/enarx/ciborium")
    (synopsis "serde implementation of CBOR using ciborium-basic")
    (description "serde implementation of CBOR using ciborium-basic")
    (license license:asl2.0)))

(define-public rust-anes-0.1
  (package
    (name "rust-anes")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/zrzka/anes-rs")
    (synopsis "ANSI Escape Sequences provider & parser")
    (description "ANSI Escape Sequences provider & parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-criterion-0.4
  (package
    (name "rust-criterion")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "criterion" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jsl4r0yc3fpkyjbi8aa1jrm69apqq9rxwnjnd9brqmaq44nxiz7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anes" ,rust-anes-0.1)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-cast" ,rust-cast-0.3)
                       ("rust-ciborium" ,rust-ciborium-0.2)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-criterion-plot" ,rust-criterion-plot-0.5)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oorandom" ,rust-oorandom-11.1)
                       ("rust-plotters" ,rust-plotters-0.3)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smol" ,rust-smol-1)
                       ("rust-tinytemplate" ,rust-tinytemplate-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://bheisler.github.io/criterion.rs/book/index.html")
    (synopsis "Statistics-driven micro-benchmarking library")
    (description "Statistics-driven micro-benchmarking library")
    (license (list license:asl2.0 license:expat))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chrono" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fv7idr8c7vdb0xi32w45a7pafnyzk7m0bknfggj5pva0qcmjg2f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-criterion" ,rust-criterion-0.4)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.5)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time" ,rust-time-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-interim-0.1
  (package
    (name "rust-interim")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "interim" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12yk65b4l5819ffp2059vb54klkx8bjfv9zzlkd795bv7742mzcz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-logos" ,rust-logos-0.12)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/conradludgate/interim")
    (synopsis
     "parses simple English dates, inspired by Linux date command, and forked from chrono-english")
    (description
     "parses simple English dates, inspired by Linux date command, and forked from
chrono-english")
    (license license:expat)))

(define-public rust-dlv-list-0.3
  (package
    (name "rust-dlv-list")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dlv-list" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mqj5rdkcjksw3kvjj0nga6rzcpppx0kimjwi527yhifz6kw5206"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/sgodwincs/dlv-list-rs")
    (synopsis "Semi-doubly linked list implemented using a vector")
    (description "Semi-doubly linked list implemented using a vector")
    (license license:expat)))

(define-public rust-ordered-multimap-0.4
  (package
    (name "rust-ordered-multimap")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ordered-multimap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jljv1257pfyf855jlwwas5mqkzk40b9lqfx40f73qbpf7ildmyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dlv-list" ,rust-dlv-list-0.3)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/sgodwincs/ordered-multimap-rs")
    (synopsis "Insertion ordered multimap")
    (description "Insertion ordered multimap")
    (license license:expat)))

(define-public rust-rust-ini-0.18
  (package
    (name "rust-rust-ini")
    (version "0.18.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rust-ini" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1px22l3m84v7f46pa3p4bsjykivw8ryq6af8kpkzdd16c11z5mgn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-ordered-multimap" ,rust-ordered-multimap-0.4)
                       ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/zonyitoo/rust-ini")
    (synopsis "An Ini configuration file parsing library in Rust")
    (description "An Ini configuration file parsing library in Rust")
    (license license:expat)))

(define-public rust-ron-0.7
  (package
    (name "rust-ron")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ron" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06iz51r6pyi197jjpfddq8h8884y85myaswfan07cnqylqwkj1w8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/ron-rs/ron")
    (synopsis "Rusty Object Notation")
    (description "Rusty Object Notation")
    (license (list license:expat license:asl2.0))))

(define-public rust-json5-0.4
  (package
    (name "rust-json5")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "json5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h9hni897zmn3vcixfbwwkj2gkz27h7z9dah8bk1qv37mwhxpc4n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pest" ,rust-pest-2)
                       ("rust-pest-derive" ,rust-pest-derive-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/callum-oakley/json5-rs")
    (synopsis "A Rust JSON5 serializer and deserializer which speaks Serde.")
    (description
     "This package provides a Rust JSON5 serializer and deserializer which speaks
Serde.")
    (license license:isc)))

(define-public rust-config-0.13
  (package
    (name "rust-config")
    (version "0.13.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "config" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19sjgx88jkx55yqln9z85hfp80a2aj1sipn7qqa1ghmzd1zsyyfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-json5" ,rust-json5-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-ron" ,rust-ron-0.7)
                       ("rust-rust-ini" ,rust-rust-ini-0.18)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (home-page "https://github.com/mehcode/config-rs")
    (synopsis "Layered configuration system for Rust applications.")
    (description "Layered configuration system for Rust applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-4
  (package
    (name "rust-clap-derive")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i65yn9n1hydvwrimqp9civ67h1iwd9v1y4yi6z7vf6nav6l95iz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
     "Parse command line argument by defining a struct, derive crate.")
    (description
     "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-lex-0.4
  (package
    (name "rust-clap-lex")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_lex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18dyxyc0g5xrazj8k6mdjd8v3fvka8z3b9k8yl13avlczskdabca"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
    (synopsis "Minimal, flexible command line parser")
    (description "Minimal, flexible command line parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-miniz-oxide-0.6
  (package
    (name "rust-miniz-oxide")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "miniz_oxide" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08j82769wkgbzfpg0k2qa744w3sg79vx1vsmjw88p1yy5rc15qpj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-adler" ,rust-adler-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-simd-adler32" ,rust-simd-adler32-0.3))))
    (home-page "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis
     "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (description
     "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (license (list license:expat license:zlib license:asl2.0))))

(define-public rust-version-check-0.9
  (package
    (name "rust-version-check")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "version_check" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/SergioBenitez/version_check")
    (synopsis
     "Tiny crate to check the version of the installed/running rustc.")
    (description
     "Tiny crate to check the version of the installed/running rustc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-getrandom-0.2
  (package
    (name "rust-getrandom")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "getrandom" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cbb766pcyi7sws0fnp1pxkz0nhiya0ckallq502bxmq49mfnnn0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/rust-random/getrandom")
    (synopsis
     "A small cross-platform library for retrieving random data from system source")
    (description
     "This package provides a small cross-platform library for retrieving random data
from system source")
    (license (list license:expat license:asl2.0))))

(define-public rust-ahash-0.8
  (package
    (name "rust-ahash")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ahash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bzcsxdl2wd6j2p4214qh9sqkqn69gi7f9lk1xi8yj063r6zd69c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic-polyfill" ,rust-atomic-polyfill-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-const-random" ,rust-const-random-0.1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
     "A non-cryptographic hash function using AES-NI for high performance")
    (description
     "This package provides a non-cryptographic hash function using AES-NI for high
performance")
    (license (list license:expat license:asl2.0))))

(define-public rust-hashbrown-0.13
  (package
    (name "rust-hashbrown")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hashbrown" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03ji3n19j4b6mf2wlla81vsixcmlivglp6hgk79d1pcxfcrw38s3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "A Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's SwissTable hash map")
    (license (list license:expat license:asl2.0))))

(define-public rust-object-0.30
  (package
    (name "rust-object")
    (version "0.30.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "object" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fdl7qjsz1j9kl3j7f4656fswzrqpyj2kgaizhknmjrx7mfjd1pa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.57))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
     "A unified interface for reading and writing object file formats.")
    (description
     "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-gimli-0.27
  (package
    (name "rust-gimli")
    (version "0.27.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gimli" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1d5v6jjchf4872jynjsg5ni4vankm1341bas8qindygb6g9962md"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-stable-deref-trait" ,rust-stable-deref-trait-1))))
    (home-page "https://github.com/gimli-rs/gimli")
    (synopsis "A library for reading and writing the DWARF debugging format.")
    (description
     "This package provides a library for reading and writing the DWARF debugging
format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-redox-syscall-0.3
  (package
    (name "rust-redox-syscall")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "redox_syscall" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0acgiy2lc1m2vr8cr33l5s7k9wzby8dybyab1a9p753hcbr68xjn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://gitlab.redox-os.org/redox-os/syscall")
    (synopsis "A Rust library to access raw Redox system calls")
    (description
     "This package provides a Rust library to access raw Redox system calls")
    (license license:expat)))

(define-public rust-tempfile-3
  (package
    (name "rust-tempfile")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tempfile" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "163rp254r3x7i5hisagrpxid2166pq94jvk511dpkmc1yf2fryxr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.3)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "A library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fs-extra-1
  (package
    (name "rust-fs-extra")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fs_extra" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/webdesus/fs_extra")
    (synopsis
     "Expanding std::fs and std::io. Recursively copy folders with information about process and much more.")
    (description
     "Expanding std::fs and std::io.  Recursively copy folders with information about
process and much more.")
    (license license:expat)))

(define-public rust-afl-0.12
  (package
    (name "rust-afl")
    (version "0.12.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "afl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jmiinzrg0402mdzp24cbnzxc56y2mb2cpzl47dnzvdpcjxjw9my"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-xdg" ,rust-xdg-2)
                       ("rust-xdg" ,rust-xdg-2))))
    (home-page "https://github.com/rust-fuzz/afl.rs")
    (synopsis "Fuzzing Rust code with american-fuzzy-lop")
    (description "Fuzzing Rust code with american-fuzzy-lop")
    (license license:asl2.0)))

(define-public rust-cpp-demangle-0.4
  (package
    (name "rust-cpp-demangle")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cpp_demangle" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hi4f93mr60997yxv6a96frzs2mrbqqj93x7lkbdszn1pi0gsiml"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-afl" ,rust-afl-0.12)
                       ("rust-cfg-if" ,rust-cfg-if-1))))
    (home-page "https://github.com/gimli-rs/cpp_demangle")
    (synopsis "A crate for demangling C++ symbols")
    (description "This package provides a crate for demangling C++ symbols")
    (license (list license:expat license:asl2.0))))

(define-public rust-addr2line-0.19
  (package
    (name "rust-addr2line")
    (version "0.19.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "addr2line" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15ywmr5wx22q69ffnn79qp65ir5p1x0k2q06plcpv6v74c5xcvx7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.4)
                       ("rust-fallible-iterator" ,rust-fallible-iterator-0.2)
                       ("rust-gimli" ,rust-gimli-0.27)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/gimli-rs/addr2line")
    (synopsis
     "A cross-platform symbolication library written in Rust, using `gimli`")
    (description
     "This package provides a cross-platform symbolication library written in Rust,
using `gimli`")
    (license (list license:asl2.0 license:expat))))

(define-public rust-backtrace-0.3
  (package
    (name "rust-backtrace")
    (version "0.3.67")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "backtrace" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jk48laqafvbyc0mn1v0didk307qc0zji3z5jcq2lpqqdmnkfg93"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-addr2line" ,rust-addr2line-0.19)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpp-demangle" ,rust-cpp-demangle-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-miniz-oxide" ,rust-miniz-oxide-0.6)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-rustc-demangle" ,rust-rustc-demangle-0.1)
                       ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis
     "A library to acquire a stack trace (backtrace) at runtime in a Rust program.
")
    (description
     "This package provides a library to acquire a stack trace (backtrace) at runtime
in a Rust program.")
    (license (list license:expat license:asl2.0))))

(define-public rust-linux-raw-sys-0.3
  (package
    (name "rust-linux-raw-sys")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "linux-raw-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03gninf16bnhb4z47ya239ijg8wpnwr9w4f21a7mfvcfd1rhwmfd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/sunfishcode/linux-raw-sys")
    (synopsis "Generated bindings for Linux's userspace API")
    (description "Generated bindings for Linux's userspace API")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.140")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "037qjmhfv8iyzfv6zqapxxvf6p1ydg6dzgzhkjbimbhzj8s768lr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-errno-0.3
  (package
    (name "rust-errno")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "errno" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "185gqhl5389apaqki46wv5k57rc4lbaqin4sx0zlg7crdjbs1mjh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-errno-dragonfly" ,rust-errno-dragonfly-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://github.com/lambda-fairy/rust-errno")
    (synopsis "Cross-platform interface to the `errno` variable.")
    (description "Cross-platform interface to the `errno` variable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustix-0.37
  (package
    (name "rust-rustix")
    (version "0.37.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustix" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y9p9an9ha33iyqqr7rbrizrvn5sv3z24ada100y9kjfcbfbaj63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-errno" ,rust-errno-0.3)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-linux-raw-sys" ,rust-linux-raw-sys-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://github.com/bytecodealliance/rustix")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock2-like syscalls")
    (description
     "Safe Rust bindings to POSIX/Unix/Linux/Winsock2-like syscalls")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-io-lifetimes-1
  (package
    (name "rust-io-lifetimes")
    (version "1.0.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "io-lifetimes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sqa81fm172q7gnd3q2v7l3a6fb2ymz6q96j9rhvq48izba0y9q9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-hermit-abi" ,rust-hermit-abi-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://github.com/sunfishcode/io-lifetimes")
    (synopsis "A low-level I/O ownership and borrowing library")
    (description
     "This package provides a low-level I/O ownership and borrowing library")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-hermit-abi-0.3
  (package
    (name "rust-hermit-abi")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hermit-abi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11j2v3q58kmi5mhjvh6hfrb7il2yzg7gmdf5lpwnwwv6qj04im7y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/hermitcore/rusty-hermit")
    (synopsis "Hermit system calls definitions.")
    (description "Hermit system calls definitions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-is-terminal-0.4
  (package
    (name "rust-is-terminal")
    (version "0.4.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "is-terminal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n14ql1fpx4d5i6b2r9rs1bbpwg114q0d4fb7blifcdb97vifq15"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hermit-abi" ,rust-hermit-abi-0.3)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-1)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://github.com/sunfishcode/is-terminal")
    (synopsis "Test whether a given stream is a terminal")
    (description "Test whether a given stream is a terminal")
    (license license:expat)))

(define-public rust-concolor-query-0.3
  (package
    (name "rust-concolor-query")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "concolor-query" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ks4g514kx31nnv3bxa7cj5xgg6vwkljn8a001njxjnpqd91vlc8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://github.com/rust-cli/concolor")
    (synopsis "Look up colored console capabilities")
    (description "Look up colored console capabilities")
    (license (list license:expat license:asl2.0))))

(define-public rust-concolor-override-1
  (package
    (name "rust-concolor-override")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "concolor-override" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zq3qvpb5ly4r1vqpw4p6lksmhf0qa2d613aafq2zicdjyhx8md8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-cli/concolor")
    (synopsis "Global override of color control")
    (description "Global override of color control")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-msvc-0.42
  (package
    (name "rust-windows-x86-64-msvc")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnullvm-0.42
  (package
    (name "rust-windows-x86-64-gnullvm")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-x86-64-gnu-0.42
  (package
    (name "rust-windows-x86-64-gnu")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_x86_64_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-msvc-0.42
  (package
    (name "rust-windows-i686-msvc")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-i686-gnu-0.42
  (package
    (name "rust-windows-i686-gnu")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_i686_gnu" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-msvc-0.42
  (package
    (name "rust-windows-aarch64-msvc")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_msvc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-aarch64-gnullvm-0.42
  (package
    (name "rust-windows-aarch64-gnullvm")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows_aarch64_gnullvm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Import lib for Windows")
    (description "Import lib for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-targets-0.42
  (package
    (name "rust-windows-targets")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-targets" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))))
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
    (synopsis "Import libs for Windows")
    (description "Import libs for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-sys-0.45
  (package
    (name "rust-windows-sys")
    (version "0.45.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "windows-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.42))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "Rust for Windows")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-wincon-0.2
  (package
    (name "rust-anstyle-wincon")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anstyle-wincon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yil703c9gp5hn1d8fn5m3dr4mlwml80v6mvhwr9y52v2kv7l4n3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstyle" ,rust-anstyle-0.3)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Styling legacy Windows terminals")
    (description "Styling legacy Windows terminals")
    (license (list license:expat license:asl2.0))))

(define-public rust-utf8parse-0.2
  (package
    (name "rust-utf8parse")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "utf8parse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02ip1a0az0qmc2786vxk2nqwsgcwf17d3a38fkf0q7hrmwh9c6vi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/alacritty/vte")
    (synopsis "Table-driven UTF-8 parser")
    (description "Table-driven UTF-8 parser")
    (license (list license:asl2.0 license:expat))))

(define-public rust-anstyle-parse-0.1
  (package
    (name "rust-anstyle-parse")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anstyle-parse" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05h1zy6y2ha2k8cnah9s0a296w2g3bfyfi2z7r7x3zly9r9vpld7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-utf8parse" ,rust-utf8parse-0.2))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Parse ANSI Style Escapes")
    (description "Parse ANSI Style Escapes")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-0.3
  (package
    (name "rust-anstyle")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anstyle" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ll42s5ch4ldp0fw8laidqji540p7higd06h2v111qq2pn0rxsi3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description "ANSI text styling")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstream-0.2
  (package
    (name "rust-anstream")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anstream" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vzzkn9h8ai1mh1k6f4lzh6dyfx08dsvvcasnz10aq802kfmh8il"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstyle" ,rust-anstyle-0.3)
                       ("rust-anstyle-parse" ,rust-anstyle-parse-0.1)
                       ("rust-anstyle-wincon" ,rust-anstyle-wincon-0.2)
                       ("rust-concolor-override" ,rust-concolor-override-1)
                       ("rust-concolor-query" ,rust-concolor-query-0.3)
                       ("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-utf8parse" ,rust-utf8parse-0.2))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis
     "A simple cross platform library for writing colored text to a terminal.")
    (description
     "This package provides a simple cross platform library for writing colored text
to a terminal.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-builder-4
  (package
    (name "rust-clap-builder")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_builder" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jcbgi7bg73w4427x4mbwv08d2bwj2ff9zd79albk95xvsjwl7v7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.2)
                       ("rust-anstyle" ,rust-anstyle-0.3)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-clap-lex" ,rust-clap-lex-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-terminal-size" ,rust-terminal-size-0.2)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-4
  (package
    (name "rust-clap")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "095nqa9yd9x2ix0kvhqz1kgr206215vc0a6sqm85pvxm8455zyvf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap-builder" ,rust-clap-builder-4)
                       ("rust-clap-derive" ,rust-clap-derive-4)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-base64-0.20
  (package
    (name "rust-base64")
    (version "0.20.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "base64" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r855djiv8rirg37w5arazk42ya5gm5gd2bww75v14w0sy02i8hf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/marshallpierce/rust-base64")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description "encodes and decodes base64 as bytes or utf8")
    (license (list license:expat license:asl2.0))))

(define-public rust-uuid-macro-internal-1
  (package
    (name "rust-uuid-macro-internal")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "uuid-macro-internal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jbccm5pk216zrrf0ibd1j7m86lgmsyibs8d59ykhak5g2l01cy1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "")
    (synopsis "Private implementation details of the uuid! macro.")
    (description "Private implementation details of the uuid! macro.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-derive-arbitrary-1
  (package
    (name "rust-derive-arbitrary")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "derive_arbitrary" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dy8pmv7d8diqhbh1b88v8q5g0inwkgb465877jqimbjqjgfpkgk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rust-fuzz/arbitrary")
    (synopsis "Derives arbitrary traits")
    (description "Derives arbitrary traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-arbitrary-1
  (package
    (name "rust-arbitrary")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "arbitrary" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0km5cj0sxfzv863blfjpz49mlikaxbaslyzk463i9jn1fgzril72"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-arbitrary" ,rust-derive-arbitrary-1))))
    (home-page "https://github.com/rust-fuzz/arbitrary/")
    (synopsis
     "The trait for generating structured data from unstructured data")
    (description
     "The trait for generating structured data from unstructured data")
    (license (list license:expat license:asl2.0))))

(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "uuid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ygxkj6bgfjkbhpdj1s0607zi9l8886pcw04lryd647f4r9q8x0n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-uuid-macro-internal" ,rust-uuid-macro-internal-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
     "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.159")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11c59pxvq01mv12ixflzymgg0jz4wwwi7da7n7s3s2avh0blsqac"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.159")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0r9hd539dzp9b7h13w9akwh33pv8f3bvk449ym0yrbix7hsfh11w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-common-13
  (package
    (name "rust-atuin-common")
    (version "13.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atuin-common" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07b6ya4fncrpghd7s3dypql36ddzmkhh2mb6gsi4hr2dy8c8lppr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://atuin.sh")
    (synopsis "common library for atuin")
    (description "common library for atuin")
    (license license:expat)))

(define-public rust-atuin-client-13
  (package
    (name "rust-atuin-client")
    (version "13.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atuin-client" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17nisdc9yifmqj3avd342bhqgfs7wffkghpcnilgi60ayxni4w4p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-common" ,rust-atuin-common-13)
                       ("rust-base64" ,rust-base64-0.20)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-config" ,rust-config-0.13)
                       ("rust-directories" ,rust-directories-4)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-interim" ,rust-interim-0.1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-minspan" ,rust-minspan-0.1)
                       ("rust-parse-duration" ,rust-parse-duration-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-regex" ,rust-serde-regex-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-shellexpand" ,rust-shellexpand-2)
                       ("rust-sodiumoxide" ,rust-sodiumoxide-0.2)
                       ("rust-sql-builder" ,rust-sql-builder-3)
                       ("rust-sqlx" ,rust-sqlx-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://atuin.sh")
    (synopsis "client library for atuin")
    (description "client library for atuin")
    (license license:expat)))

(define-public rust-quote-1
  (package
    (name "rust-quote")
    (version "1.0.26")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "quote" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z521piwggwzs0rj4wjx4ma6af1g6f1h5dkp382y5akqyx5sy924"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.54")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-macro2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d3mv0fy9hp3a3n3ks1pvr33aj5a0h0ic9kjllabax4wg42a2wp4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "syn" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h6cmirz2blyvfx6gvj223vc2dmazba7xn7721qn4r8lnxxpiqr1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-trait-0.1
  (package
    (name "rust-async-trait")
    (version "0.1.68")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "async-trait" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hp8ysdjr8c43avm7bkj73cd22ra3dpzag82bjyyj6qn5a7xvk5r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/async-trait")
    (synopsis "Type erasure for async trait methods")
    (description "Type erasure for async trait methods")
    (license (list license:expat license:asl2.0))))

; (define rust-1.65 (module-ref (resolve-module '(gnu packages rust)) 'rust-1.65))

(define-public atuin
  (package
    (name "rust-atuin")
    (version "13.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atuin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h416xwqk2j4zfq6pmm0lg3x0wsmb716rz86q6hwb5mz5hdc217v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-nightly-1.67
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-client" ,rust-atuin-client-13)
                       ("rust-atuin-common" ,rust-atuin-common-13)
                       ("rust-atuin-server" ,rust-atuin-server-13)
                       ("rust-base64" ,rust-base64-0.20)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cassowary" ,rust-cassowary-0.3)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete" ,rust-clap-complete-4)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossterm" ,rust-crossterm-0.26)
                       ("rust-directories" ,rust-directories-4)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-interim" ,rust-interim-0.1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rpassword" ,rust-rpassword-7)
                       ("rust-runtime-format" ,rust-runtime-format-0.1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tiny-bip39" ,rust-tiny-bip39-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-whoami" ,rust-whoami-1))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-server-feature
                    (lambda* _
                      (delete-file "Cargo.toml.orig")
                      (substitute* "Cargo.toml"
                        (("    \"sync\",") "")
                        (("    \"server\",") ""))
                      #t)))))
    (home-page "https://atuin.sh")
    (synopsis "atuin - magical shell history")
    (description "atuin - magical shell history")
    (license license:expat)))
