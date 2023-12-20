(define-module (vup atuin)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages perl)
  #:use-module (vup rust-apps)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-ndarray-rand-0.14
  (package
    (name "rust-ndarray-rand")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ndarray-rand" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1225iiqhc9h0sd4sdf4a4vf6fpdwy3s41ksd2rdmywncga9qyq35"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ndarray" ,rust-ndarray-0.15)
                       ("rust-quickcheck" ,rust-quickcheck-0.9)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-distr" ,rust-rand-distr-0.4))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand-isaac" ,rust-rand-isaac-0.3))))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
     "Constructors for randomized arrays. `rand` integration for `ndarray`.")
    (description
     "Constructors for randomized arrays. `rand` integration for `ndarray`.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-macros-0.2
  (package
   (name "rust-time-macros")
   (version "0.2.12")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "time-macros" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0lbldy232ag8ganwn492v70rp3livjqix947v44lhfkbxmlm9ikm"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-time-core" ,rust-time-core-0.1))))
   (home-page "https://github.com/time-rs/time")
   (synopsis "Procedural macros for the time crate")
   (description "This package provides procedural macros for the time
crate.")
   (license (list license:expat license:asl2.0))))

(define-public rust-hashbrown-0.13
  (package
    (name "rust-hashbrown")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hashbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f602rk7pgdhw1s57g81822g7b2m5i2wibrpaqp11afk5kk8mzrk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-fnv" ,rust-fnv-1)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "A Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.26")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01qyfpqbwljyywnq2qwwx1x7rqm0h4pcrxx20qwsph88dfn0k7d7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included
       #:cargo-inputs (("rust-deranged" ,rust-deranged-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num-threads" ,rust-num-threads-0.1)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time-core" ,rust-time-core-0.1)
                       ("rust-time-macros" ,rust-time-macros-0.2))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-test" ,rust-serde-test-1)
        ("rust-time-macros" ,rust-time-macros-0.2)
        ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://time-rs.github.io")
    (synopsis "Date and time library")
    (description "This package provides a date and time library.  It is fully
interoperable with the standard library, and is mostly compatible with
@code{#![no_std]}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.108")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ssj59s7lpzqh1m50kfzlnrip0p0jg9lmhn4098i33a0mhz7w71x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-indexmap" ,rust-indexmap-2)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-ryu" ,rust-ryu-1)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-automod" ,rust-automod-1)
        ("rust-indoc" ,rust-indoc-2)
        ("rust-ref-cast" ,rust-ref-cast-1)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-stacker" ,rust-serde-stacker-0.1)
        ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "JSON serialization file format")
    (description
     "This package provides a JSON serialization file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tiny-bip39-1
  (package
    (name "rust-tiny-bip39")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiny-bip39" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q98iv3wgbd41wyxxd5is8sddi53k9ary45rbi5fi8dmb39r9k32"))))
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
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "runtime-format" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "154c7jq7kbpc5acn2ysa2ilab2x0i5y7d34jwznni9xw71dqv589"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tinyvec" ,rust-tinyvec-1))))
    (home-page "https://github.com/conradludgate/strfmt")
    (synopsis "rust library for formatting dynamic strings")
    (description "rust library for formatting dynamic strings")
    (license license:expat)))

(define-public rust-deltae-0.3
  (package
    (name "rust-deltae")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deltae" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d3hw9hpvicl9x0x34jr2ybjk5g5ym1lhbyz6zj31110gq8zaaap"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://gitlab.com/ryanobeirne/deltae")
    (synopsis "Calculate Delta E between two colors in CIE Lab space.")
    (description "Calculate Delta E between two colors in CIE Lab space.")
    (license license:expat)))

(define-public rust-cint-0.3
  (package
    (name "rust-cint")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cint" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16l9glvaxshbp3awcga3s8cdfv00gb1n2s7ixzxxjwc5yz6qf3ks"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytemuck" ,rust-bytemuck-1))))
    (home-page "https://github.com/termhn/cint")
    (synopsis
     "A lean, minimal, and stable set of types for color interoperation between crates in Rust.")
    (description
     "This package provides a lean, minimal, and stable set of types for color
interoperation between crates in Rust.")
    (license (list license:expat license:asl2.0 license:zlib))))

(define-public rust-csscolorparser-0.6
  (package
    (name "rust-csscolorparser")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "csscolorparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gxh11hajx96mf5sd0az6mfsxdryfqvcfcphny3yfbfscqq7sapb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cint" ,rust-cint-0.3)
                       ("rust-lab" ,rust-lab-0.11)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-rgb" ,rust-rgb-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/mazznoer/csscolorparser-rs")
    (synopsis "CSS color parser library")
    (description "CSS color parser library")
    (license (list license:expat license:asl2.0))))

(define-public rust-wezterm-color-types-0.2
  (package
    (name "rust-wezterm-color-types")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wezterm-color-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xvphmrqgg69v9l879xj5lq010z13f5ixi854ykmny6j7m47lvjc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-csscolorparser" ,rust-csscolorparser-0.6)
                       ("rust-deltae" ,rust-deltae-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wezterm-dynamic" ,rust-wezterm-dynamic-0.1))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis "Types for working with colors")
    (description "Types for working with colors")
    (license license:expat)))

(define-public rust-wezterm-dynamic-derive-0.1
  (package
    (name "rust-wezterm-dynamic-derive")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wezterm-dynamic-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w07qf8njyq19nxi9vpshwprk00blhzg9ybis2rhfba433rmx7qc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis
     "config serialization for wezterm via dynamic json-like data values")
    (description
     "config serialization for wezterm via dynamic json-like data values")
    (license license:expat)))

(define-public rust-wezterm-dynamic-0.1
  (package
    (name "rust-wezterm-dynamic")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wezterm-dynamic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1al8fmfr852m62mlcr0v2lg3a18icl2sv79zv7jnv9v0rk07hpm7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-ordered-float" ,rust-ordered-float-3)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wezterm-dynamic-derive" ,rust-wezterm-dynamic-derive-0.1))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis
     "config serialization for wezterm via dynamic json-like data values")
    (description
     "config serialization for wezterm via dynamic json-like data values")
    (license license:expat)))

(define-public rust-wezterm-bidi-0.2
  (package
    (name "rust-wezterm-bidi")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wezterm-bidi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dkcwscvlwnv6lnagxfb08rcd21gfyrxbr7afcjaj3wvycn3hq0m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-wezterm-dynamic" ,rust-wezterm-dynamic-0.1))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis "The Unicode Bidi Algorithm (UBA)")
    (description "The Unicode Bidi Algorithm (UBA)")
    (license license:expat)))

(define-public rust-vtparse-0.6
  (package
    (name "rust-vtparse")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vtparse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l5yz9650zhkaffxn28cvfys7plcw2wd6drajyf41pshn37jm6vd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-utf8parse" ,rust-utf8parse-0.2))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis "Low level escape sequence parser")
    (description "Low level escape sequence parser")
    (license license:expat)))

(define-public rust-memmem-0.1
  (package
    (name "rust-memmem")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memmem" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05ccifqgxdfxk6yls41ljabcccsz3jz6549l1h3cwi17kr494jm6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "http://github.com/jneem/memmem")
    (synopsis "Substring searching")
    (description "Substring searching")
    (license (list license:expat license:asl2.0))))

(define-public rust-finl-unicode-1
  (package
    (name "rust-finl-unicode")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "finl_unicode" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ipdx778849czik798sjbgk5yhwxqybydac18d2g9jb20dxdrkwg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://finl.xyz")
    (synopsis
     "Library for handling Unicode functionality for finl (categories and grapheme segmentation)")
    (description
     "Library for handling Unicode functionality for finl (categories and grapheme
segmentation)")
    (license (list license:expat license:asl2.0))))

(define-public rust-termwiz-0.20
  (package
    (name "rust-termwiz")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "termwiz" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yj80sli95wcw0im2iic9h7mx20hms3f9shxk7jarjqgl5waj2cm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cassowary" ,rust-cassowary-0.3)
                       ("rust-filedescriptor" ,rust-filedescriptor-0.8)
                       ("rust-finl-unicode" ,rust-finl-unicode-1)
                       ("rust-fixedbitset" ,rust-fixedbitset-0.4)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-image" ,rust-image-0.24)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memmem" ,rust-memmem-0.1)
                       ("rust-nix" ,rust-nix-0.24)
                       ("rust-num-derive" ,rust-num-derive-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-ordered-float" ,rust-ordered-float-3)
                       ("rust-pest" ,rust-pest-2)
                       ("rust-pest-derive" ,rust-pest-derive-2)
                       ("rust-phf" ,rust-phf-0.10)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-semver" ,rust-semver-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-signal-hook" ,rust-signal-hook-0.1)
                       ("rust-siphasher" ,rust-siphasher-0.3)
                       ("rust-terminfo" ,rust-terminfo-0.7)
                       ("rust-termios" ,rust-termios-0.3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-ucd-trie" ,rust-ucd-trie-0.1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-vtparse" ,rust-vtparse-0.6)
                       ("rust-wezterm-bidi" ,rust-wezterm-bidi-0.2)
                       ("rust-wezterm-color-types" ,rust-wezterm-color-types-0.2)
                       ("rust-wezterm-dynamic" ,rust-wezterm-dynamic-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis "Terminal Wizardry for Unix and Windows")
    (description "Terminal Wizardry for Unix and Windows")
    (license license:expat)))

(define-public rust-heck-0.4
  (package
    (name "rust-heck")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "heck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description "heck is a case conversion library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-strum-macros-0.25
  (package
    (name "rust-strum-macros")
    (version "0.25.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strum_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "184y62g474zqb2f7n16x3ghvlyjbh50viw32p9w9l5lwmjlizp13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/Peternator7/strum")
    (synopsis "Helpful macros for working with enums and strings")
    (description "Helpful macros for working with enums and strings")
    (license license:expat)))

(define-public rust-strum-0.25
  (package
    (name "rust-strum")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09g1q55ms8vax1z0mxlbva3vm8n2r1179kfvbccnkjcidzm58399"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-phf" ,rust-phf-0.10)
                       ("rust-strum-macros" ,rust-strum-macros-0.25))))
    (home-page "https://github.com/Peternator7/strum")
    (synopsis "Helpful macros for working with enums and strings")
    (description "Helpful macros for working with enums and strings")
    (license license:expat)))

(define-public rust-lru-0.12
  (package
    (name "rust-lru")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lru" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1myillpwqfcins062g28jvj48cxw8818zcx08ydzsl6misxfx519"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hashbrown" ,rust-hashbrown-0.14))))
    (home-page "https://github.com/jeromefroe/lru-rs")
    (synopsis "A LRU cache implementation")
    (description "This package provides a LRU cache implementation")
    (license license:expat)))

(define-public rust-ratatui-0.24
  (package
    (name "rust-ratatui")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ratatui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "099433has5bhl6zbckpv2qskb66h7xz4rfgc6xn5cyjjzdy93g0f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cassowary" ,rust-cassowary-0.3)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-indoc" ,rust-indoc-2)
                       ("rust-itertools" ,rust-itertools-0.11)
                       ("rust-lru" ,rust-lru-0.12)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-strum" ,rust-strum-0.25)
                       ("rust-termion" ,rust-termion-2)
                       ("rust-termwiz" ,rust-termwiz-0.20)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/ratatui-org/ratatui")
    (synopsis "A library that's all about cooking up terminal user interfaces")
    (description
     "This package provides a library that's all about cooking up terminal user
interfaces")
    (license license:expat)))

(define-public rust-vte-0.11
  (package
    (name "rust-vte")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vte" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15r1ff4j8ndqj9vsyil3wqwxhhl7jsz5g58f31n0h1wlpxgjn0pm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vt100" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pklc8y984axmxr0cd363srr2d27wd5rj15xlcmkjznvy0xqdkc4"))))
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

(define-public rust-indicatif-0.17
  (package
    (name "rust-indicatif")
    (version "0.17.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indicatif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "098ggvg7ps4097p5n9hmb3pqqy10bi8vjfzb7pci79xrklf78a7v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-instant" ,rust-instant-0.1)
                       ("rust-number-prefix" ,rust-number-prefix-0.4)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
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

(define-public rust-signal-hook-0.3
  (package
    (name "rust-signal-hook")
    (version "0.3.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "signal-hook" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "filedescriptor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vplyh0cw35kzq7smmp2ablq0zsknk5rkvvrywqsqfrchmjxk6bi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/wez/wezterm")
    (synopsis "More ergonomic wrappers around RawFd and RawHandle")
    (description
     "More ergonomic wrappers around @code{RawFd} and @code{RawHandle}")
    (license license:expat)))

(define-public rust-crossterm-winapi-0.9
  (package
    (name "rust-crossterm-winapi")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossterm_winapi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/crossterm-rs/crossterm-winapi")
    (synopsis
     "WinAPI wrapper that provides some basic simple abstractions around common WinAPI calls")
    (description
     "@code{WinAPI} wrapper that provides some basic simple abstractions around common
@code{WinAPI} calls")
    (license license:expat)))

(define-public rust-crossterm-0.27
  (package
    (name "rust-crossterm")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossterm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pr413ki440xgddlmkrc4j1bfx1h8rpmll87zn8ykja1bm2gwxpl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
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

(define-public rust-colored-2
  (package
    (name "rust-colored")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "colored" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xhnlnyv3am5xx0gw5bgrfh33d3p06x44v0yycn02f5w5x4fqx16"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/mackwic/colored")
    (synopsis "The most simple way to add colors in your terminal")
    (description "The most simple way to add colors in your terminal")
    (license license:mpl2.0)))

(define-public rust-tree-magic-db-3
  (package
    (name "rust-tree-magic-db")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tree_magic_db" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00kzsn98cv0r7yzwi2dcm0fzpbxmc7pxijhb5dgb3cr7ai5c4gz7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "")
    (synopsis
     "Packages the FreeDesktop.org shared MIME database for optional use with tree_magic_mini")
    (description
     "Packages the @code{FreeDesktop.org} shared MIME database for optional use with
tree_magic_mini")
    (license license:gpl2+)))

(define-public rust-tree-magic-mini-3
  (package
    (name "rust-tree-magic-mini")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tree_magic_mini" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vdazv3y1iggriwx5ksin72c2ds0xjdhx1yvmd5nxkya0w3gvbci"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytecount" ,rust-bytecount-0.6)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-petgraph" ,rust-petgraph-0.6)
                       ("rust-tree-magic-db" ,rust-tree-magic-db-3))))
    (home-page "https://github.com/mbrubeck/tree_magic/")
    (synopsis
     "Determines the MIME type of a file by traversing a filetype tree.")
    (description
     "Determines the MIME type of a file by traversing a filetype tree.")
    (license license:expat)))

(define-public rust-derive-new-0.5
  (package
    (name "rust-derive-new")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive-new" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d9m5kcj1rdmdjqfgj7rxxhdzx0as7p4rp1mjx5j6w5dl2f3461l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/nrc/derive-new")
    (synopsis
     "`#[derive(new)]` implements simple constructor functions for structs and enums.")
    (description
     "`#[derive(new)]` implements simple constructor functions for structs and enums.")
    (license license:expat)))

(define-public rust-wl-clipboard-rs-0.7
  (package
    (name "rust-wl-clipboard-rs")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wl-clipboard-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n8sg981h3d08hnnlrsgs81w2qz3n8a5ml0jcsgnapdpzcyk06lq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-new" ,rust-derive-new-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.24)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tree-magic-mini" ,rust-tree-magic-mini-3)
                       ("rust-wayland-client" ,rust-wayland-client-0.29)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.29))))
    (home-page "https://github.com/YaLTeR/wl-clipboard-rs")
    (synopsis
     "Access to the Wayland clipboard for terminal and other window-less applications.")
    (description
     "Access to the Wayland clipboard for terminal and other window-less applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clipboard-win-4
  (package
    (name "rust-clipboard-win")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clipboard-win" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qh3rypkf1lazniq4nr04hxsck0d55rigb5sjvpvgnap4dyc54bi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-error-code" ,rust-error-code-2)
                       ("rust-str-buf" ,rust-str-buf-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/DoumanAsh/clipboard-win")
    (synopsis "Provides simple way to interact with Windows clipboard.")
    (description
     "This package provides simple way to interact with Windows clipboard.")
    (license license:boost1.0)))

(define-public rust-cli-clipboard-0.4
  (package
    (name "rust-cli-clipboard")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cli-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g9y1w3ln5wn202mwxwhsilhifwww2p34fan99w5k8ia98fpq204"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clipboard-win" ,rust-clipboard-win-4)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-objc-foundation" ,rust-objc-foundation-0.1)
                       ("rust-objc-id" ,rust-objc-id-0.1)
                       ("rust-wl-clipboard-rs" ,rust-wl-clipboard-rs-0.7)
                       ("rust-x11-clipboard" ,rust-x11-clipboard-0.7))))
    (home-page "https://github.com/actuallyallie/cli-clipboard")
    (synopsis
     "cli-clipboard is a cross-platform library for getting and setting the contents of the OS-level clipboard.")
    (description
     "cli-clipboard is a cross-platform library for getting and setting the contents
of the OS-level clipboard.")
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-server-postgres-17
  (package
    (name "rust-atuin-server-postgres")
    (version "17.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-server-postgres" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ilc66mvdiinv27l8s86ayba1fmvkxzmlldpkgwr1gh73h6wknjr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-common" ,rust-atuin-common-17)
                       ("rust-atuin-server-database" ,rust-atuin-server-database-17)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sqlx" ,rust-sqlx-0.7)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://atuin.sh")
    (synopsis "server postgres database library for atuin")
    (description "server postgres database library for atuin")
    (license license:expat)))

(define-public rust-webpki-roots-0.25
  (package
    (name "rust-webpki-roots")
    (version "0.25.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "045g7az4mj1002m55iydln4jhyah4br2n0zms3wbz41vicpa8y0p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rustls/webpki-roots")
    (synopsis "Mozilla's CA root certificates for use with webpki")
    (description "Mozilla's CA root certificates for use with webpki")
    (license license:mpl2.0)))

(define-public rust-webpki-roots-0.24
  (package
    (name "rust-webpki-roots")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "120q85pvzpckvvrg085a5jhh91fby94pgiv9y1san7lxbmnm94dj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls-webpki" ,rust-rustls-webpki-0.101))))
    (home-page "https://github.com/rustls/webpki-roots")
    (synopsis "Mozilla's CA root certificates for use with webpki")
    (description "Mozilla's CA root certificates for use with webpki")
    (license license:mpl2.0)))

(define-public rust-tungstenite-0.20
  (package
    (name "rust-tungstenite")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fbgcv3h4h1bhhf5sqbwqsp7jnc44bi4m41sgmhzdsk2zl8aqgcy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.24))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description "Lightweight stream-based @code{WebSocket} implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-native-tls-0.3
  (package
    (name "rust-tokio-native-tls")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wkfg6zn85zckmv4im7mv20ca6b1vmlib5xwz9p7g19wjfmpdbmv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis
     "An implementation of TLS/SSL streams for Tokio using native-tls giving an implementation of TLS
for nonblocking I/O streams.
")
    (description
     "An implementation of TLS/SSL streams for Tokio using native-tls giving an
implementation of TLS for nonblocking I/O streams.")
    (license license:expat)))

(define-public rust-rustls-native-certs-0.6
  (package
    (name "rust-rustls-native-certs")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-schannel" ,rust-schannel-0.1)
                       ("rust-security-framework" ,rust-security-framework-2))))
    (home-page "https://github.com/rustls/rustls-native-certs")
    (synopsis
     "rustls-native-certs allows rustls to use the platform native certificate store")
    (description
     "rustls-native-certs allows rustls to use the platform native certificate store")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-tokio-tungstenite-0.20
  (package
    (name "rust-tokio-tungstenite")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v1v24l27hxi5hlchs7hfd5rgzi167x0ygbw220nvq0w5b5msb91"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
                       ("rust-tungstenite" ,rust-tungstenite-0.20)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "Tokio binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation")
    (license license:expat)))

(define-public rust-sync-wrapper-0.1
  (package
    (name "rust-sync-wrapper")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sync_wrapper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))))
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

(define-public rust-matchit-0.7
  (package
    (name "rust-matchit")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "matchit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "156bgdmmlv4crib31qhgg49nsjk88dxkdqp80ha2pk2rk6n6ax0f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ibraheemdev/matchit")
    (synopsis "A high performance, zero-copy URL router.")
    (description
     "This package provides a high performance, zero-copy URL router.")
    (license (list license:expat license:bsd-3))))

(define-public rust-headers-0.3
  (package
    (name "rust-headers")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "headers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w62gnwh2p1lml0zqdkrx9dp438881nhz32zrzdy61qa0a9kns06"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.21)
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
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qkb5cg06bnp8994ay0smk57shd5hpphcmp90kd7p65dxh86mjnd"))))
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

(define-public rust-mime-0.3
  (package
    (name "rust-mime")
    (version "0.3.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/hyperium/mime")
    (synopsis "Strongly Typed Mimes")
    (description "Strongly Typed Mimes")
    (license (list license:expat license:asl2.0))))

(define-public rust-iri-string-0.7
  (package
    (name "rust-iri-string")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "iri-string" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h07hkfkkjjvgzlaqpr5fia7hrgv7qxqdw4xrpdc3936gmk9p191"))))
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
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-range-header" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13vm511vq3bhschkw2xi9nhxzkw53m55gn9vxg7qigfxc29spl5d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/MarcusGrass/parse-range-headers")
    (synopsis "No-dep range header parser")
    (description "No-dep range header parser")
    (license license:expat)))

(define-public rust-zstd-safe-7
  (package
    (name "rust-zstd-safe")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gpav2lcibrpmyslmjkcn3w0w64qif3jjljd2h8lr4p249s7qx23"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zstd-sys" ,rust-zstd-sys-2))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Safe low-level bindings for the zstd compression library.")
    (description "Safe low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zstd-0.13
  (package
    (name "rust-zstd")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0401q54s9r35x2i7m1kwppgkj79g0pb6xz3xpby7qlkdb44k7yxz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zstd-safe" ,rust-zstd-safe-7))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Binding for the zstd compression library.")
    (description "Binding for the zstd compression library.")
    (license license:expat)))

(define-public rust-deflate64-0.1
  (package
    (name "rust-deflate64")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deflate64" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aagh5mmyr8p08if33hizqwiq2as90v9smla89nydq6pivsfy766"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/anatawa12/deflate64-rs#readme")
    (synopsis "Deflate64 implementation based on .NET's implementation")
    (description "Deflate64 implementation based on .NET's implementation")
    (license license:expat)))

(define-public rust-async-compression-0.4
  (package
    (name "rust-async-compression")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-compression" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19f2mdiz7jrmpbhjxmpfmixfv5640iknhxhfb57x723k5bxhqbdw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-brotli" ,rust-brotli-3)
                       ("rust-bzip2" ,rust-bzip2-0.4)
                       ("rust-deflate64" ,rust-deflate64-0.1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-xz2" ,rust-xz2-0.1)
                       ("rust-zstd" ,rust-zstd-0.13)
                       ("rust-zstd-safe" ,rust-zstd-safe-7))))
    (home-page "https://github.com/Nullus157/async-compression")
    (synopsis
     "Adaptors between compression crates and Rust's modern asynchronous IO types.
")
    (description
     "Adaptors between compression crates and Rust's modern asynchronous IO types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tower-http-0.4
  (package
    (name "rust-tower-http")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tower-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h0i2flrw25zwxv72sifq4v5mwcb030spksy7r2a4xl2d4fvpib1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-2)
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

(define-public rust-http-body-0.4
  (package
    (name "rust-http-body")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l967qwwlvhp198xdrnc0p5d7jwfcp6q2lm510j6zqw4s4b8zwym"))))
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

(define-public rust-axum-core-0.3
  (package
    (name "rust-axum-core")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b1d9nkqb8znaba4qqzxzc968qwj4ybn4vgpyz9lz4a7l9vsb7vm"))))
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
                       ("rust-tower-http" ,rust-tower-http-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for axum")
    (description "Core types and traits for axum")
    (license license:expat)))

(define-public rust-axum-0.6
  (package
    (name "rust-axum")
    (version "0.6.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gynqkg3dcy1zd7il69h8a3zax86v6qq5zpawqyn87mr6979x0iv"))))
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
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.20)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description "Web framework that focuses on ergonomics and modularity")
    (license license:expat)))

(define-public rust-atuin-server-database-17
  (package
    (name "rust-atuin-server-database")
    (version "17.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-server-database" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09552zrib6vxyqg5wv70l61bg12njnyn75cs14xfrchx4vz04n6j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-common" ,rust-atuin-common-17)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://atuin.sh")
    (synopsis "server database library for atuin")
    (description "server database library for atuin")
    (license license:expat)))

(define-public rust-atuin-server-17
  (package
    (name "rust-atuin-server")
    (version "17.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08q0gxr8wjbrkysz91cvj451b2ikafvhgg19xs7jwqkpv4av1clh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-argon2" ,rust-argon2-0.5)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-common" ,rust-atuin-common-17)
                       ("rust-atuin-server-database" ,rust-atuin-server-database-17)
                       ("rust-axum" ,rust-axum-0.6)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-config" ,rust-config-0.13)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.4)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://atuin.sh")
    (synopsis "server library for atuin")
    (description "server library for atuin")
    (license license:expat)))

(define-public rust-sql-builder-3
  (package
    (name "rust-sql-builder")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sql-builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h5xp47zz9chv545lpmal51fq3z162z2f99mb4lhcbgcsaaqs05i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/perdumonocle/sql-builder.git")
    (synopsis "Simple SQL code generator.")
    (description "Simple SQL code generator.")
    (license license:expat)))

(define-public rust-shellexpand-3
  (package
    (name "rust-shellexpand")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "shellexpand" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-os-str-bytes" ,rust-os-str-bytes-6))))
    (home-page "https://gitlab.com/ijackson/rust-shellexpand")
    (synopsis "Shell-like expansions in strings")
    (description "Shell-like expansions in strings")
    (license (list license:expat license:asl2.0))))

(define-public rust-semver-1
  (package
    (name "rust-semver")
    (version "1.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "semver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "140hmbfa743hbmah1zjf07s8apavhvn04204qjigjiz5w6iscvw3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dtolnay/semver")
    (synopsis "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (description
     "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (license (list license:expat license:asl2.0))))

(define-public rust-signature-derive-1
  (package
    (name "rust-signature-derive")
    (version "1.0.0-pre.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "signature_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03wj342zvljknqwg3qbc9acrcsrzhdp1d2d6pfrh4p1b087k3rln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/signature_derive")
    (synopsis "Custom derive support for the 'signature' crate")
    (description "Custom derive support for the signature crate")
    (license (list license:asl2.0 license:expat))))

(define-public rust-signature-1
  (package
    (name "rust-signature")
    (version "1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "signature" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z3xg405pg827g6hfdprnszsdqkkbrsfx7f1dl04nv9g7cxks8vl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-signature-derive" ,rust-signature-derive-1))))
    (home-page "https://github.com/RustCrypto/traits/tree/master/signature")
    (synopsis
     "Traits for cryptographic signature algorithms (e.g. ECDSA, Ed25519)")
    (description
     "Traits for cryptographic signature algorithms (e.g. ECDSA, Ed25519)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rfc6979-0.3
  (package
    (name "rust-rfc6979")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rfc6979" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fzsp705b5lhwd2r9il9grc3lj6rm3b2r89vh0xv181gy5xg2hvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crypto-bigint" ,rust-crypto-bigint-0.4)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/rfc6979")
    (synopsis
     "Pure Rust implementation of RFC6979: Deterministic Usage of the
Digital Signature Algorithm (DSA) and Elliptic Curve Digital Signature Algorithm (ECDSA)
")
    (description
     "Pure Rust implementation of RFC6979: Deterministic Usage of the Digital
Signature Algorithm (DSA) and Elliptic Curve Digital Signature Algorithm (ECDSA)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-subtle-2
  (package
    (name "rust-subtle")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "subtle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g2yjs7gffgmdvkkq0wrrh0pxds3q0dv6dhkw9cdpbib656xdkc1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://dalek.rs/")
    (synopsis
     "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (description
     "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-sec1-0.3
  (package
    (name "rust-sec1")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sec1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a09lk5w3nyggpyz54m10nnlg9v8qbh6kw3v1bgla31988c4rqiv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.1)
                       ("rust-der" ,rust-der-0.6)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-pkcs8" ,rust-pkcs8-0.9)
                       ("rust-serdect" ,rust-serdect-0.1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/sec1")
    (synopsis
     "Pure Rust implementation of SEC1: Elliptic Curve Cryptography encoding formats
including ASN.1 DER-serialized private keys as well as the
Elliptic-Curve-Point-to-Octet-String encoding
")
    (description
     "Pure Rust implementation of SEC1: Elliptic Curve Cryptography encoding formats
including ASN.1 DER-serialized private keys as well as the
Elliptic-Curve-Point-to-Octet-String encoding")
    (license (list license:asl2.0 license:expat))))

(define-public rust-group-0.12
  (package
    (name "rust-group")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "group" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ixspxqdpq0hxg0hd9s6rngrp6rll21v4jjnr7ar1lzvdhxgpysx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ff" ,rust-ff-0.12)
                       ("rust-memuse" ,rust-memuse-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/zkcrypto/group")
    (synopsis "Elliptic curve group traits and utilities")
    (description "Elliptic curve group traits and utilities")
    (license (list license:expat license:asl2.0))))

(define-public rust-ff-derive-0.12
  (package
    (name "rust-ff-derive")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ff_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jvqilpzzbw3j3sh1sf0914n2l4v020iifmgvglnc7wgfjh6znqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-addchain" ,rust-addchain-0.2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-num-bigint" ,rust-num-bigint-0.3)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/zkcrypto/ff")
    (synopsis
     "Procedural macro library used to build custom prime field implementations")
    (description
     "Procedural macro library used to build custom prime field implementations")
    (license (list license:expat license:asl2.0))))

(define-public rust-ff-0.12
  (package
    (name "rust-ff")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q3imz4m3dj2cy182i20wa8kbclgj13ddfngqb2miicc6cjzq4yh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitvec" ,rust-bitvec-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-ff-derive" ,rust-ff-derive-0.12)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/zkcrypto/ff")
    (synopsis "Library for building and interfacing with finite fields")
    (description "Library for building and interfacing with finite fields")
    (license (list license:expat license:asl2.0))))

(define-public rust-serdect-0.1
  (package
    (name "rust-serdect")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serdect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b6krqs77vzwzdjcrcywlmlwd3msfpgmkkbxx8q9njypyhdwx3q3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/serdect")
    (synopsis
     "Constant-time serde serializer/deserializer helpers for data that potentially
contains secrets (e.g. cryptographic keys)
")
    (description
     "Constant-time serde serializer/deserializer helpers for data that potentially
contains secrets (e.g. cryptographic keys)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-crypto-bigint-0.4
  (package
    (name "rust-crypto-bigint")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto-bigint" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vqprgj0aj1340w186zyspi58397ih78jsc0iydvhs6zrlilnazg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-der" ,rust-der-0.6)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rlp" ,rust-rlp-0.5)
                       ("rust-serdect" ,rust-serdect-0.1)
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

(define-public rust-base16ct-0.1
  (package
    (name "rust-base16ct")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base16ct" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1klccxr7igf73wpi0x3asjd8n0xjg0v6a7vxgvfk5ybvgh1hd6il"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustCrypto/formats/tree/master/base16ct")
    (synopsis
     "Pure Rust implementation of Base16 a.k.a hexadecimal (RFC 4648) which avoids
any usages of data-dependent branches/LUTs and thereby provides portable
\"best effort\" constant-time operation and embedded-friendly no_std support
")
    (description
     "Pure Rust implementation of Base16 a.k.a hexadecimal (RFC 4648) which avoids any
usages of data-dependent branches/LUTs and thereby provides portable \"best
effort\" constant-time operation and embedded-friendly no_std support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-elliptic-curve-0.12
  (package
    (name "rust-elliptic-curve")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "elliptic-curve" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lwi108mh6drw5nzqzlz7ighdba5qxdg5vmwwnw1j2ihnn58ifz7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.1)
                       ("rust-base64ct" ,rust-base64ct-1)
                       ("rust-crypto-bigint" ,rust-crypto-bigint-0.4)
                       ("rust-der" ,rust-der-0.6)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ff" ,rust-ff-0.12)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-group" ,rust-group-0.12)
                       ("rust-hex-literal" ,rust-hex-literal-0.3)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.6)
                       ("rust-pkcs8" ,rust-pkcs8-0.9)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-sec1" ,rust-sec1-0.3)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serdect" ,rust-serdect-0.1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/elliptic-curve")
    (synopsis
     "General purpose Elliptic Curve Cryptography (ECC) support, including types
and traits for representing various elliptic curve forms, scalars, points,
and public/secret keys composed thereof.
")
    (description
     "General purpose Elliptic Curve Cryptography (ECC) support, including types and
traits for representing various elliptic curve forms, scalars, points, and
public/secret keys composed thereof.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ecdsa-0.14
  (package
    (name "rust-ecdsa")
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ecdsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p1wxap2s6jm06y2w3cal8dkz6p9223ir9wws70rgx8h929h2cs1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-der" ,rust-der-0.6)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.12)
                       ("rust-rfc6979" ,rust-rfc6979-0.3)
                       ("rust-serdect" ,rust-serdect-0.1)
                       ("rust-signature" ,rust-signature-1))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/ecdsa")
    (synopsis
     "Pure Rust implementation of the Elliptic Curve Digital Signature Algorithm
(ECDSA) as specified in FIPS 186-4 (Digital Signature Standard), providing
RFC6979 deterministic signatures as well as support for added entropy
")
    (description
     "Pure Rust implementation of the Elliptic Curve Digital Signature Algorithm
(ECDSA) as specified in FIPS 186-4 (Digital Signature Standard), providing
RFC6979 deterministic signatures as well as support for added entropy")
    (license (list license:asl2.0 license:expat))))

(define-public rust-p384-0.11
  (package
    (name "rust-p384")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "p384" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1al021z9j5y7nznqvngf5snglxjsrbcfrh47ksxm5pidcjzwbj6z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ecdsa" ,rust-ecdsa-0.14)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.12)
                       ("rust-hex-literal" ,rust-hex-literal-0.3)
                       ("rust-serdect" ,rust-serdect-0.1)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page
     "https://github.com/RustCrypto/elliptic-curves/tree/master/p384")
    (synopsis
     "Pure Rust implementation of the NIST P-384 (a.k.a. secp384r1) elliptic curve
as defined in SP 800-186 with support for ECDH, ECDSA signing/verification,
and general purpose curve arithmetic support.
")
    (description
     "Pure Rust implementation of the NIST P-384 (a.k.a.  secp384r1) elliptic curve as
defined in SP 800-186 with support for ECDH, ECDSA signing/verification, and
general purpose curve arithmetic support.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-iso8601-0.4
  (package
    (name "rust-iso8601")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "iso8601" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15nfg6d4qlniw4gk7039s5y07lzgr1dp9snsw63lsxarnyz4zfg5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-nom" ,rust-nom-7))))
    (home-page "https://github.com/badboy/iso8601")
    (synopsis "Parsing ISO8601 dates using nom")
    (description "Parsing ISO8601 dates using nom")
    (license license:expat)))

(define-public rust-rusty-paseto-0.6
  (package
    (name "rust-rusty-paseto")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusty_paseto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05myhzsw0pf2fs8a513a1pngpmxvfabwd7v2q0vcr8cvz7n6xpvc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.7)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-blake2" ,rust-blake2-0.9)
                       ("rust-chacha20" ,rust-chacha20-0.8)
                       ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.9)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-1)
                       ("rust-erased-serde" ,rust-erased-serde-0.3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-iso8601" ,rust-iso8601-0.4)
                       ("rust-p384" ,rust-p384-0.11)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/rrrodzilla/rusty_paseto")
    (synopsis
     "A type-driven, ergonomic alternative to JWT for secure stateless PASETO tokens.")
    (description
     "This package provides a type-driven, ergonomic alternative to JWT for secure
stateless PASETO tokens.")
    (license (list license:expat license:asl2.0))))

(define-public rust-merlin-3
  (package
    (name "rust-merlin")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "merlin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z9rh9jlpcs0i0cijbs6pcq26gl4qwz05y7zbnv7h2gwk4kqxhsq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-hex" ,rust-hex-0.3)
                       ("rust-keccak" ,rust-keccak-0.1)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://docs.rs/merlin")
    (synopsis
     "Composable proof transcripts for public-coin arguments of knowledge")
    (description
     "Composable proof transcripts for public-coin arguments of knowledge")
    (license license:expat)))

(define-public rust-ed25519-2
  (package
    (name "rust-ed25519")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ed25519" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkcs8" ,rust-pkcs8-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/ed25519")
    (synopsis
     "Edwards Digital Signature Algorithm (EdDSA) over Curve25519 (as specified in RFC 8032)
support library providing signature type definitions and PKCS#8 private key
decoding/encoding support
")
    (description
     "Edwards Digital Signature Algorithm (@code{EdDSA}) over Curve25519 (as specified
in RFC 8032) support library providing signature type definitions and PKCS#8
private key decoding/encoding support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ed25519-dalek-2
  (package
    (name "rust-ed25519-dalek")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ed25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h13qm789m9gdjl6jazss80hqi8ll37m0afwcnw23zcbqjp8wqhz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-curve25519-dalek" ,rust-curve25519-dalek-4)
                       ("rust-ed25519" ,rust-ed25519-2)
                       ("rust-merlin" ,rust-merlin-3)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis
     "Fast and efficient ed25519 EdDSA key generations, signing, and verification in pure Rust.")
    (description
     "Fast and efficient ed25519 @code{EdDSA} key generations, signing, and
verification in pure Rust.")
    (license license:bsd-3)))

(define-public rust-digest-0.10
  (package
    (name "rust-digest")
    (version "0.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "digest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-block-buffer" ,rust-block-buffer-0.10)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits for cryptographic hash functions and message authentication codes")
    (description
     "Traits for cryptographic hash functions and message authentication codes")
    (license (list license:expat license:asl2.0))))

(define-public rust-platforms-3
  (package
    (name "rust-platforms")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "platforms" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c6bzwn877aqdbbmyqsl753ycbciwvbdh4lpzijb8vrfb4zsprhl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://rustsec.org")
    (synopsis
     "Rust platform registry with information about valid Rust platforms (target
triple, target_arch, target_os) sourced from the Rust compiler.
")
    (description
     "Rust platform registry with information about valid Rust platforms (target
triple, target_arch, target_os) sourced from the Rust compiler.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-fiat-crypto-0.2
  (package
    (name "rust-fiat-crypto")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fiat-crypto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dxn0g50pv0ppal779vi7k40fr55pbhkyv4in7i13pgl4sn3wmr7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/mit-plv/fiat-crypto")
    (synopsis "Fiat-crypto generated Rust")
    (description "Fiat-crypto generated Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-curve25519-dalek-derive-0.1
  (package
    (name "rust-curve25519-dalek-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curve25519-dalek-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis "curve25519-dalek Derives")
    (description "curve25519-dalek Derives")
    (license (list license:expat license:asl2.0))))

(define-public rust-curve25519-dalek-4
  (package
    (name "rust-curve25519-dalek")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curve25519-dalek" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p7ns5917k6369gajrsbfj24llc5zfm635yh3abla7sb5rm8r6z8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-curve25519-dalek-derive" ,rust-curve25519-dalek-derive-0.1)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ff" ,rust-ff-0.13)
                       ("rust-fiat-crypto" ,rust-fiat-crypto-0.2)
                       ("rust-group" ,rust-group-0.13)
                       ("rust-platforms" ,rust-platforms-3)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis
     "A pure-Rust implementation of group operations on ristretto255 and Curve25519")
    (description
     "This package provides a pure-Rust implementation of group operations on
ristretto255 and Curve25519")
    (license license:bsd-3)))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.150")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g10n8c830alndgjb8xk1i9kz5z727np90z1z81119pr8d3jmnc9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cpufeatures-0.2
  (package
    (name "rust-cpufeatures")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cpufeatures" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l0gzsyy576n017g9bf0vkv5hhg9cpz1h1libxyfdlzcgbh0yhnf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
     "Lightweight runtime CPU feature detection for aarch64, loongarch64, and x86/x86_64 targets,
with no_std support and support for mobile targets including Android and iOS
")
    (description
     "Lightweight runtime CPU feature detection for aarch64, loongarch64, and
x86/x86_64 targets, with no_std support and support for mobile targets including
Android and @code{iOS}")
    (license (list license:expat license:asl2.0))))

(define-public rust-argon2-0.5
  (package
    (name "rust-argon2")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "argon2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y820hkza66lfliaxg49zskz7agj8wf7aak528livg261an4rfhp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-blake2" ,rust-blake2-0.10)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-password-hash" ,rust-password-hash-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/argon2")
    (synopsis
     "Pure Rust implementation of the Argon2 password hashing function with support
for the Argon2d, Argon2i, and Argon2id algorithmic variants
")
    (description
     "Pure Rust implementation of the Argon2 password hashing function with support
for the Argon2d, Argon2i, and Argon2id algorithmic variants")
    (license (list license:expat license:asl2.0))))

(define-public rust-rusty-paserk-0.3
  (package
    (name "rust-rusty-paserk")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusty_paserk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v70nbn7f5dwwngnhsqm407y61rdgrjz82nrlih2wycygzh73m2n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-argon2" ,rust-argon2-0.5)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-base64ct" ,rust-base64ct-1)
                       ("rust-blake2" ,rust-blake2-0.10)
                       ("rust-chacha20" ,rust-chacha20-0.9)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-curve25519-dalek" ,rust-curve25519-dalek-4)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rusty-paseto" ,rust-rusty-paseto-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/conradludgate/rusty-paserk")
    (synopsis
     "Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key management")
    (description
     "Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key
management")
    (license license:expat)))

(define-public rust-parse-duration-2
  (package
    (name "rust-parse-duration")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parse_duration" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pd97dmlv1i6pvr2byi65q1fzv667gvhnf3ld2lsawh17vlyadvh"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "minspan" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s7lh0ryq0kk6sm6z5f2ikqq437xca0gzc61ds80pbh8qdxa2s8j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "")
    (synopsis
     "a package for determining the minimum span of one vector within another")
    (description
     "a package for determining the minimum span of one vector within another")
    (license license:expat)))

(define-public rust-logos-codegen-0.13
  (package
    (name "rust-logos-codegen")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s7drl8vfp9viw9mfyz8dll1gfvp1dc6np82abj0402y548p6j6w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-beef" ,rust-beef-0.5)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.6)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/maciejhirsz/logos")
    (synopsis
     "Implementation details for logos-codegen and logos-derive. Not for public consumption.")
    (description
     "Implementation details for logos-codegen and logos-derive.  Not for public
consumption.")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-derive-0.13
  (package
    (name "rust-logos-derive")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zi6s400yfw1ma7wnawyjjgbq1nqmx0xjdh18j8dfhhzkwi0vz6v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-logos-codegen" ,rust-logos-codegen-0.13))))
    (home-page "https://github.com/maciejhirsz/logos")
    (synopsis "Create ridiculously fast Lexers")
    (description "Create ridiculously fast Lexers")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-0.13
  (package
    (name "rust-logos")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hfjqmmcq6fbfwpca6874b1k3lsqi75n584kkg4qmwcgj16wl060"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-logos-derive" ,rust-logos-derive-0.13))))
    (home-page "https://github.com/maciejhirsz/logos")
    (synopsis "Create ridiculously fast Lexers")
    (description "Create ridiculously fast Lexers")
    (license (list license:expat license:asl2.0))))

(define-public rust-interim-0.1
  (package
    (name "rust-interim")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "interim" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0d32gcccw1iyjg79jbzzhsib52ikxkddzk5fxax274ji2x43jyqi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-logos" ,rust-logos-0.13)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/conradludgate/interim")
    (synopsis
     "parses simple English dates, inspired by Linux date command, and forked from chrono-english")
    (description
     "parses simple English dates, inspired by Linux date command, and forked from
chrono-english")
    (license license:expat)))

(define-public rust-universal-hash-0.5
  (package
    (name "rust-universal-hash")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "universal-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sh79x677zkncasa95wz05b36134822w6qxmi1ck05fwi33f47gw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis
     "Traits which describe the functionality of universal hash functions (UHFs)")
    (description
     "Traits which describe the functionality of universal hash functions (UHFs)")
    (license (list license:expat license:asl2.0))))

(define-public rust-poly1305-0.8
  (package
    (name "rust-poly1305")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1grs77skh7d8vi61ji44i8gpzs3r9x7vay50i6cg8baxfa8bsnc1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-opaque-debug" ,rust-opaque-debug-0.3)
                       ("rust-universal-hash" ,rust-universal-hash-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/universal-hashes")
    (synopsis
     "The Poly1305 universal hash function and message authentication code")
    (description
     "The Poly1305 universal hash function and message authentication code")
    (license (list license:asl2.0 license:expat))))

(define-public rust-chacha20-0.9
  (package
    (name "rust-chacha20")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chacha20" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0678wipx6kghp71hpzhl2qvx80q7caz3vm8vsvd07b1fpms3yqf3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2))))
    (home-page "https://github.com/RustCrypto/stream-ciphers")
    (synopsis
     "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits
from the RustCrypto `cipher` crate, with optional architecture-specific
hardware acceleration (AVX2, SSE2). Additionally provides the ChaCha8, ChaCha12,
XChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional
rand_core-compatible RNGs based on those ciphers.
")
    (description
     "The @code{ChaCha20} stream cipher (RFC 8439) implemented in pure Rust using
traits from the @code{RustCrypto} `cipher` crate, with optional
architecture-specific hardware acceleration (AVX2, SSE2).  Additionally provides
the @code{ChaCha8}, @code{ChaCha12}, X@code{ChaCha20}, X@code{ChaCha12} and
X@code{ChaCha8} stream ciphers, and also optional rand_core-compatible RNGs
based on those ciphers.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-crypto-secretbox-0.1
  (package
    (name "rust-crypto-secretbox")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crypto_secretbox" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qa1w5s8dbyb88269zrmvbnillqahz394pl07bsds6gpmn3wzmmr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.5)
                       ("rust-chacha20" ,rust-chacha20-0.9)
                       ("rust-cipher" ,rust-cipher-0.4)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-poly1305" ,rust-poly1305-0.8)
                       ("rust-salsa20" ,rust-salsa20-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page
     "https://github.com/RustCrypto/nacl-compat/tree/master/crypto_secretbox")
    (synopsis
     "Pure Rust implementation of the XSalsa20Poly1305 (a.k.a. NaCl crypto_secretbox)
authenticated encryption cipher as well as the libsodium variant of
XChaCha20Poly1305
")
    (description
     "Pure Rust implementation of the XSalsa20Poly1305 (a.k.a. @code{NaCl}
crypto_secretbox) authenticated encryption cipher as well as the libsodium
variant of X@code{ChaCha20Poly1305}")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dlv-list-0.3
  (package
    (name "rust-dlv-list")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dlv-list" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mqj5rdkcjksw3kvjj0nga6rzcpppx0kimjwi527yhifz6kw5206"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ordered-multimap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jljv1257pfyf855jlwwas5mqkzk40b9lqfx40f73qbpf7ildmyc"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rust-ini" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1px22l3m84v7f46pa3p4bsjykivw8ryq6af8kpkzdd16c11z5mgn"))))
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

(define-public rust-json5-0.4
  (package
    (name "rust-json5")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "json5" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h9hni897zmn3vcixfbwwkj2gkz27h7z9dah8bk1qv37mwhxpc4n"))))
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
    (version "0.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jjag1x3rl77zjykbrykzhd5fsiv8vy40y4lxkj46xicjw8qwwr3"))))
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

(define-public rust-typed-builder-macro-0.15
  (package
    (name "rust-typed-builder-macro")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-builder-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qncxrwrhsznqyk5b8bkcvq0wmil8k1av60z04ykxcfh84f1b8r9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/idanarye/rust-typed-builder")
    (synopsis "Compile-time type-checked builder derive")
    (description "Compile-time type-checked builder derive")
    (license (list license:expat license:asl2.0))))

(define-public rust-typed-builder-0.15
  (package
    (name "rust-typed-builder")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lk6jnkx5lwmg3dlfxnls26d68svy28air4wrg2fhxaqm22krs3z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-typed-builder-macro" ,rust-typed-builder-macro-0.15))))
    (home-page "https://github.com/idanarye/rust-typed-builder")
    (synopsis "Compile-time type-checked builder derive")
    (description "Compile-time type-checked builder derive")
    (license (list license:expat license:asl2.0))))

(define-public rust-urlencoding-2
  (package
    (name "rust-urlencoding")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urlencoding" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://lib.rs/urlencoding")
    (synopsis "A Rust library for doing URL percentage encoding.")
    (description
     "This package provides a Rust library for doing URL percentage encoding.")
    (license license:expat)))

(define-public rust-prettyplease-0.2
  (package
    (name "rust-prettyplease")
    (version "0.2.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prettyplease" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17az47j29q76gnyqvd5giryjz2fp7zw7vzcka1rb8ndbfgbmn05f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/prettyplease")
    (synopsis "A minimal `syn` syntax tree pretty-printer")
    (description
     "This package provides a minimal `syn` syntax tree pretty-printer")
    (license (list license:expat license:asl2.0))))

(define-public rust-yansi-term-0.1
  (package
    (name "rust-yansi-term")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w8vjlvxba6yvidqdvxddx3crl6z66h39qxj8xi6aqayw2nk0p7y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/botika/yansi-term")
    (synopsis "Library for ANSI terminal colours and styles (bold, underline)")
    (description
     "Library for ANSI terminal colours and styles (bold, underline)")
    (license license:expat)))

(define-public rust-annotate-snippets-0.9
  (package
    (name "rust-annotate-snippets")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "annotate-snippets" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-yansi-term" ,rust-yansi-term-0.1))))
    (home-page "https://github.com/rust-lang/annotate-snippets-rs")
    (synopsis "Library for building code annotations")
    (description "Library for building code annotations")
    (license (list license:asl2.0 license:expat))))

(define-public rust-bindgen-0.69
  (package
    (name "rust-bindgen")
    (version "0.69.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hkrccfri0223b2r5cvacy83ld6s76n2m68518bsfilrhk1ypz4z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-annotate-snippets" ,rust-annotate-snippets-0.9)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cexpr" ,rust-cexpr-0.6)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lazycell" ,rust-lazycell-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-libsqlite3-sys-0.27
  (package
    (name "rust-libsqlite3-sys")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libsqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05pp60ncrmyjlxxjj187808jkvpxm06w5lvvdwwvxd2qrmnj4kng"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Native bindings to the libsqlite3 library")
    (description "Native bindings to the libsqlite3 library")
    (license license:expat)))

(define-public rust-flume-0.11
  (package
    (name "rust-flume")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flume" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10girdbqn77wi802pdh55lwbmymy437k7kklnvj12aaiwaflbb2m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-nanorand" ,rust-nanorand-0.7)
                       ("rust-spin" ,rust-spin-0.9))))
    (home-page "https://github.com/zesterer/flume")
    (synopsis "A blazingly fast multi-producer channel")
    (description
     "This package provides a blazingly fast multi-producer channel")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlx-sqlite-0.7
  (package
    (name "rust-sqlx-sqlite")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "143laha7wf8dmi0xwycwqmvxdcnb25dq7jnqrsgvmis8v6vpc291"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atoi" ,rust-atoi-2)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-flume" ,rust-flume-0.11)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-intrusive" ,rust-futures-intrusive-0.5)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.27)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "SQLite driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
     "SQLite driver implementation for SQLx.  Not for direct use; see the `sqlx` crate
for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-home-0.5
  (package
    (name "rust-home")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "home" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nqx1krijvpd03d96avsdyknd12h8hs3xhxwgqghf8v9xxzc4i2l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis "Shared definitions of home directories.")
    (description "Shared definitions of home directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-etcetera-0.8
  (package
    (name "rust-etcetera")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "etcetera" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hxrsn75dirbjhwgkdkh0pnpqrnq17ypyhjpjaypgax1hd91nv8k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/lunacookies/etcetera")
    (synopsis
     "An unopinionated library for obtaining configuration, data, cache, & other directories")
    (description
     "An unopinionated library for obtaining configuration, data, cache, & other
directories")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-postgres-0.7
  (package
    (name "rust-sqlx-postgres")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-postgres" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "090wm9s6mm53ggn1xwr183cnn8yxly8rgcksdk4hrlfcnz1hmb6n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atoi" ,rust-atoi-2)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bit-vec" ,rust-bit-vec-0.6)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-etcetera" ,rust-etcetera-0.8)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-ipnetwork" ,rust-ipnetwork-0.20)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mac-address" ,rust-mac-address-1)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-stringprep" ,rust-stringprep-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "PostgreSQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
     "@code{PostgreSQL} driver implementation for SQLx.  Not for direct use; see the
`sqlx` crate for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-mysql-0.7
  (package
    (name "rust-sqlx-mysql")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-mysql" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "190ygz5a3pqcd9vvqjv2i4r1xh8vi53j4272yrld07zpblwrawg3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atoi" ,rust-atoi-2)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-stringprep" ,rust-stringprep-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "MySQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
     "@code{MySQL} driver implementation for SQLx.  Not for direct use; see the `sqlx`
crate for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nix-0.27
  (package
    (name "rust-nix")
    (version "0.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ly0kkmij5f0sqz35lx9czlbk6zpihb7yh1bsy4irzwfd2f4xc1f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-pin-utils" ,rust-pin-utils-0.1))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "Rust friendly bindings to *nix APIs")
    (license license:expat)))

(define-public rust-atomic-write-file-0.1
  (package
    (name "rust-atomic-write-file")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atomic-write-file" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dl4x0srdwjxm3zz3fj1c7m44i3b7mjiad550fqklj1n4bfbxkgd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-nix" ,rust-nix-0.27)
                       ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/andreacorbellini/rust-atomic-write-file")
    (synopsis "Write files atomically to a file system")
    (description "Write files atomically to a file system")
    (license license:bsd-3)))

(define-public rust-sqlx-macros-core-0.7
  (package
    (name "rust-sqlx-macros-core")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-macros-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h88wahkxa6nam536lhwr1y0yxlr6la8b1x0hs0n88v790clbgfh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-atomic-write-file" ,rust-atomic-write-file-0.1)
                       ("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-either" ,rust-either-1)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-sqlx-mysql" ,rust-sqlx-mysql-0.7)
                       ("rust-sqlx-postgres" ,rust-sqlx-postgres-0.7)
                       ("rust-sqlx-sqlite" ,rust-sqlx-sqlite-0.7)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Macro support core for SQLx, the Rust SQL toolkit. Not intended to be used directly.")
    (description
     "Macro support core for SQLx, the Rust SQL toolkit.  Not intended to be used
directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-macros-0.7
  (package
    (name "rust-sqlx-macros")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19gjwisiym07q7ibkp9nkvvbywjh0r5rc572msvzyzadvh01r5l9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-sqlx-macros-core" ,rust-sqlx-macros-core-0.7)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
     "Macros for SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlformat-0.2
  (package
    (name "rust-sqlformat")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlformat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11cymbg6xr7xyy5hvry8pfkgpwzfl2br9wy0546lvgp7i23jfyvb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itertools" ,rust-itertools-0.11)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-unicode-categories" ,rust-unicode-categories-0.1))))
    (home-page "https://github.com/shssoichiro/sqlformat-rs")
    (synopsis "Formats whitespace in a SQL string to make it easier to read")
    (description
     "Formats whitespace in a SQL string to make it easier to read")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-webpki-0.101
  (package
    (name "rust-rustls-webpki")
    (version "0.101.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ring" ,rust-ring-0.17)
                       ("rust-untrusted" ,rust-untrusted-0.9))))
    (home-page "https://github.com/rustls/webpki")
    (synopsis "Web PKI X.509 Certificate Verification.")
    (description "Web PKI X.509 Certificate Verification.")
    (license license:isc)))

(define-public rust-untrusted-0.9
  (package
    (name "rust-untrusted")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "untrusted" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/briansmith/untrusted")
    (synopsis
     "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted inputs in Rust.")
    (description
     "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted
inputs in Rust.")
    (license license:isc)))

(define-public rust-cc-1
  (package
    (name "rust-cc")
    (version "1.0.83")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l643zidlb5iy1dskc5ggqs4wqa29a02f44piczqc8zcnsq4y5zi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-jobserver" ,rust-jobserver-0.1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis
     "A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.
")
    (description
     "This package provides a build-time dependency for Cargo build scripts to assist
in invoking the native C compiler to compile native C code into a static archive
to be linked into Rust code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ring-0.17
  (package
    (name "rust-ring")
    (version "0.17.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ring" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rh830kj0638fdif6470ggq396nvnxmj74jayvmwqsgn31p5wkb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-spin" ,rust-spin-0.9)
                       ("rust-untrusted" ,rust-untrusted-0.9)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/briansmith/ring")
    (synopsis "Safe, fast, small crypto using Rust.")
    (description "Safe, fast, small crypto using Rust.")
    (license #f)))

(define-public rust-rustls-0.21
  (package
    (name "rust-rustls")
    (version "0.21.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fcqlk5ff9mdxrgd89i1655s10r9n33v9cjhmxcdaxapxnn4i5k2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-sct" ,rust-sct-0.7))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Rustls is a modern TLS library written in Rust.")
    (description "Rustls is a modern TLS library written in Rust.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-ubyte-0.10
  (package
    (name "rust-ubyte")
    (version "0.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ubyte" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1spj3k9sx6xvfn7am9vm1b463hsr79nyvj8asi2grqhyrvvdw87p"))))
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

(define-public rust-oid-registry-0.4
  (package
    (name "rust-oid-registry")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oid-registry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0akbah3j8231ayrp2l1y5d9zmvbvqcsj0sa6s6dz6h85z8bhgqiq"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10kfa2gzl3x20mwgrd43cyi79xgkqxyzcyrh0xylv4apa33qlfgy"))))
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

(define-public rust-asn1-rs-derive-0.1
  (package
    (name "rust-asn1-rs-derive")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asn1-rs-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gzf9vab06lk0zjvbr07axx64fndkng2s28bnj27fnwd548pb2yv"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asn1-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0czsk1nd4dx2k83f7jzkn8klx05wbmblkx1jh51i4c170akhbzrh"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x509-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "077bi0xyaa8cmrqf3rrw1z6kkzscwd1nxdxgs7mgz2ambg7bmfcz"))))
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

(define-public rust-tokio-rustls-0.24
  (package
    (name "rust-tokio-rustls")
    (version "0.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls" ,rust-rustls-0.21)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/rustls/tokio-rustls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-state-0.6
  (package
    (name "rust-state")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n3n2h324h1y5zhaajh6kplvzfvg1l6hsr8siggmf4yq8m24m31b"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stable-pattern" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i8hq82vm82mqj02qqcsd7caibrih7x5w3a1xpm8hpv30261cr25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-memchr" ,rust-memchr-2))))
    (home-page "https://github.com/SergioBenitez/stable-pattern")
    (synopsis "Stable port of std::str::Pattern and friends.")
    (description "Stable port of std::str::Pattern and friends.")
    (license (list license:expat license:asl2.0))))

(define-public rust-base64-0.21
  (package
    (name "rust-base64")
    (version "0.21.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y8x2xs9nszj5ix7gg4ycn5a6wy7ca74zxwqri3bdqzdjha6lqrm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/marshallpierce/rust-base64")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description "encodes and decodes base64 as bytes or utf8")
    (license (list license:expat license:asl2.0))))

(define-public rust-cookie-0.18
  (package
    (name "rust-cookie")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cookie" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y2ywf9isq0dwpj7m7jq7r1g9cs3xr2i6qipw5v030hj2kv1rn9w"))))
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
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17iq208zf9rfxdnx8hfjxnn51074cc9li99yjigzwnfhjhv6d89p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cookie" ,rust-cookie-0.18)
                       ("rust-either" ,rust-either-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pear" ,rust-pear-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-ref-cast" ,rust-ref-cast-1)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-stable-pattern" ,rust-stable-pattern-0.1)
                       ("rust-state" ,rust-state-0.6)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
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

(define-public rust-devise-core-0.4
  (package
    (name "rust-devise-core")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "devise_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sp5idq0idng9i5kwjd8slvc724s97r28arrhyqq1jpx1ax0vd9m"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "devise_codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mpy5mmsigkj5f72gby82yk4advcqj97am2wzn0dwkj8vnwg934w"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "devise" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y45iag4hyvspkdsf6d856hf0ihf9vjnaga3c7y6c72l7zywxsnn"))))
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
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket_codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k6hdf9s9y73kzj89qs688gnfjj1sl4imp6pdjz22pzpmdk808x2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-devise" ,rust-devise-0.4)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rocket-http" ,rust-rocket-http-0.5)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework.")
    (description "Procedural macros for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-multer-2
  (package
    (name "rust-multer")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "multer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hjiphaypj3phqaj5igrzcia9xfmf4rr4ddigbh8zzb96k1bvb01"))))
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

(define-public rust-toml-edit-0.21
  (package
    (name "rust-toml-edit")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_edit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00xa3qfk34qazvnkfxyyyqqc6nyl2ksks1c5bd53n5has0y3hkfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-winnow" ,rust-winnow-0.5))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Yet another format-preserving TOML parser.")
    (description "Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-0.8
  (package
    (name "rust-toml")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ddbahcrrxf9374mkn3c1h2a2g6a883qx23kywl6k8lxikn9b8d1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-toml-edit" ,rust-toml-edit-0.21))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis
     "A native Rust encoder and decoder of TOML-formatted files and streams. Provides
implementations of the standard Serialize/Deserialize traits for TOML data to
facilitate deserializing and serializing Rust structures.
")
    (description
     "This package provides a native Rust encoder and decoder of TOML-formatted files
and streams.  Provides implementations of the standard Serialize/Deserialize
traits for TOML data to facilitate deserializing and serializing Rust
structures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-yansi-1
  (package
    (name "rust-yansi")
    (version "1.0.0-rc.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yansi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xr3n41j5v00scfkac2d6vhkxiq9nz3l5j6vw8f3g3bqixdjjrqk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-is-terminal" ,rust-is-terminal-0.4))))
    (home-page "https://github.com/SergioBenitez/yansi")
    (synopsis "A dead simple ANSI terminal color painting library.")
    (description
     "This package provides a dead simple ANSI terminal color painting library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-diagnostics-0.10
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2-diagnostics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j48ipc80pykvhx6yhndfa774s58ax1h6sm6mlhf09ls76f6l1mg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-1))))
    (home-page "https://github.com/SergioBenitez/proc-macro2-diagnostics")
    (synopsis "Diagnostics for proc-macro2.")
    (description "Diagnostics for proc-macro2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pear-codegen-0.2
  (package
    (name "rust-pear-codegen")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pear_codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m0dras73cm92sqn1715ypn46h9z1r8sc043kq9rq1n8v89hz7ys"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "inlinable_string" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ysjci8yfvxgf51z0ny2nnwhxrclhmb3vbngin8v4bznhr3ybyn8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/fitzgen/inlinable_string")
    (synopsis
     "The `inlinable_string` crate provides the `InlinableString` type -- an owned, grow-able UTF-8 string that stores small strings inline and avoids heap-allocation -- and the `StringExt` trait which abstracts string operations over both `std::string::String` and `InlinableString` (or even your own custom string type).")
    (description
     "The `inlinable_string` crate provides the `@code{InlinableString`} type -- an
owned, grow-able UTF-8 string that stores small strings inline and avoids
heap-allocation -- and the `@code{StringExt`} trait which abstracts string
operations over both `std::string::String` and `@code{InlinableString`} (or even
your own custom string type).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pear-0.2
  (package
    (name "rust-pear")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pear" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "077pd1lbr5g99gsmcbglcrq6izl32qvd2l2bc2cx6aajf76qd8v1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-inlinable-string" ,rust-inlinable-string-0.1)
                       ("rust-pear-codegen" ,rust-pear-codegen-0.2)
                       ("rust-yansi" ,rust-yansi-1))))
    (home-page "")
    (synopsis "A pear is a fruit.")
    (description "This package provides a pear is a fruit.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bytemuck-derive-1
  (package
    (name "rust-bytemuck-derive")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytemuck_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cgj75df2v32l4fmvnp25xxkkz4lp6hz76f7hfhd55wgbzmvfnln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/Lokathor/bytemuck")
    (synopsis "derive proc-macros for `bytemuck`")
    (description "derive proc-macros for `bytemuck`")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-bytemuck-1
  (package
    (name "rust-bytemuck")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytemuck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ik1ma5n3bg700skkzhx50zjk7kj7mbsphi773if17l04pn2hk9p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytemuck-derive" ,rust-bytemuck-derive-1))))
    (home-page "https://github.com/Lokathor/bytemuck")
    (synopsis "A crate for mucking around with piles of bytes.")
    (description
     "This package provides a crate for mucking around with piles of bytes.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-atomic-0.6
  (package
    (name "rust-atomic")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atomic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15193mfhmrq3p6vi1a10hw3n6kvzf5h32zikhby3mdj0ww1q10cd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytemuck" ,rust-bytemuck-1))))
    (home-page "https://github.com/Amanieu/atomic-rs")
    (synopsis "Generic Atomic<T> wrapper type")
    (description "Generic Atomic<T> wrapper type")
    (license (list license:asl2.0 license:expat))))

(define-public rust-figment-0.10
  (package
    (name "rust-figment")
    (version "0.10.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "figment" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vps8n6nnn0ca2cww60bibm5ka4d9lq2d5jik9z0b535h9fkx7v4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic" ,rust-atomic-0.6)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pear" ,rust-pear-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-toml" ,rust-toml-0.8)
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "binascii" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wnaglgl72pn5ilv61q6y34w76gbg7crb8ifqk6lsxnq2gajjg9q"))))
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
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rocket" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l4i93dai7pyzlkvdjkqg2g7ni1r6749cwx4nrrhsrr6rdybaywy"))))
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
                       ("rust-indexmap" ,rust-indexmap-2)
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
                       ("rust-state" ,rust-state-0.6)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-ubyte" ,rust-ubyte-0.10)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-yansi" ,rust-yansi-1))))
    (home-page "https://rocket.rs")
    (synopsis
     "Web framework with a focus on usability, security, extensibility, and speed.
")
    (description
     "Web framework with a focus on usability, security, extensibility, and speed.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-table-macro-syntax-0.1
  (package
    (name "rust-diesel-table-macro-syntax")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_table_macro_syntax" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i9115qgsnargr6a707lqcjc45wqzq351a2gbvnnyw2kqkpmfmgw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-syn" ,rust-syn-2))))
    (home-page "https://diesel.rs")
    (synopsis "Internal diesel crate")
    (description "Internal diesel crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-derives-2
  (package
    (name "rust-diesel-derives")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel_derives" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i1bzp6rxnrrlgz1y946ap3203vjvack9a05h135mxblfmrkg0zg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-diesel-table-macro-syntax" ,rust-diesel-table-macro-syntax-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://diesel.rs")
    (synopsis
     "You should not use this crate directly, it is internal to Diesel.")
    (description
     "You should not use this crate directly, it is internal to Diesel.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel-2
  (package
    (name "rust-diesel")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diesel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n1ihxd5zfqg94zy01zf85ml716fqmsivj7pxiw8qzzi8bwgrik2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-diesel-derives" ,rust-diesel-derives-2)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-ipnetwork" ,rust-ipnetwork-0.17)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.26)
                       ("rust-mysqlclient-sys" ,rust-mysqlclient-sys-0.2)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pq-sys" ,rust-pq-sys-0.4)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-r2d2" ,rust-r2d2-0.8)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://diesel.rs")
    (synopsis
     "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL")
    (description
     "This package provides a safe, extensible ORM and Query Builder for
@code{PostgreSQL}, SQLite, and @code{MySQL}")
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-derive-0.1
  (package
    (name "rust-syn-derive")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yxydi22apcisjg0hff6dfm5x8hd6cqicav56sblx67z0af1ha8k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/Kyuuhachi/syn_derive")
    (synopsis "Derive macros for `syn::Parse` and `quote::ToTokens`")
    (description "Derive macros for `syn::Parse` and `quote::@code{ToTokens`}")
    (license (list license:expat license:asl2.0))))

(define-public rust-terminal-size-0.2
  (package
    (name "rust-terminal-size")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "terminal_size" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0drj7gb77kay5r1cv53ysq3g9g4f8n0jkhld0kadi3lzkvqzcswf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustix" ,rust-rustix-0.37)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/eminence/terminal-size")
    (synopsis "Gets the size of your Linux or Windows terminal")
    (description "Gets the size of your Linux or Windows terminal")
    (license (list license:expat license:asl2.0))))

(define-public rust-is-terminal-0.4
  (package
    (name "rust-is-terminal")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "is-terminal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12xgvc7nsrp3pn8hcxajfhbli2l5wnh3679y2fmky88nhj4qj26b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hermit-abi" ,rust-hermit-abi-0.3)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/sunfishcode/is-terminal")
    (synopsis "Test whether a given stream is a terminal")
    (description "Test whether a given stream is a terminal")
    (license license:expat)))

(define-public rust-anstyle-1
  (package
    (name "rust-anstyle")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11yxw02b6parn29s757z96rgiqbn8qy0fk9a3p3bhczm85dhfybh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description "ANSI text styling")
    (license (list license:expat license:asl2.0))))

(define-public rust-winnow-0.5
  (package
    (name "rust-winnow")
    (version "0.5.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winnow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ywwrl647bl7jibk44bblyi8mrd8f4dq8425xr66shnvwgrld642"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.3)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-is-terminal" ,rust-is-terminal-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-terminal-size" ,rust-terminal-size-0.2))))
    (home-page "https://github.com/winnow-rs/winnow")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
     "This package provides a byte-oriented, zero-copy, parser combinators library")
    (license license:expat)))

(define-public rust-toml-datetime-0.6
  (package
    (name "rust-toml-datetime")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_datetime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wds4pm2cn6agd38f0ivm65xnc7c7bmk9m0fllcaq82nd3lz8l1m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "A TOML-compatible datetime type")
    (description "This package provides a TOML-compatible datetime type")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-spanned-0.6
  (package
    (name "rust-serde-spanned")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_spanned" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "102ym47sr1y48ml42wjv6aq8y77bij1qckx1j0gb3rbka21jn0hj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Serde-compatible spanned Value")
    (description "Serde-compatible spanned Value")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-edit-0.20
  (package
    (name "rust-toml-edit")
    (version "0.20.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml_edit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10bdyrl1yj5jxkiqfa2fyx9inlzlm7s8nf1jnysp4k6qwky2gx3h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-kstring" ,rust-kstring-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-spanned" ,rust-serde-spanned-0.6)
                       ("rust-toml-datetime" ,rust-toml-datetime-0.6)
                       ("rust-winnow" ,rust-winnow-0.5))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Yet another format-preserving TOML parser.")
    (description "Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro-crate-2
  (package
    (name "rust-proc-macro-crate")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro-crate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s23imns07vmacn2xjd5hv2h6rr94iqq3fd2frwa6i4h2nk6d0vy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-toml-edit" ,rust-toml-edit-0.20))))
    (home-page "https://github.com/bkchr/proc-macro-crate")
    (synopsis "Replacement for crate (macro_rules keyword) in proc-macros
")
    (description "Replacement for crate (macro_rules keyword) in proc-macros")
    (license (list license:expat license:asl2.0))))

(define-public rust-borsh-derive-1
  (package
    (name "rust-borsh-derive")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "borsh-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qsbs44s1dvlyq4h1i0advr2i2n89dagz3c06sg29dd7grx6a17l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro-crate" ,rust-proc-macro-crate-2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-syn-derive" ,rust-syn-derive-0.1))))
    (home-page "https://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license license:asl2.0)))

(define-public rust-ascii-1
  (package
    (name "rust-ascii")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ascii" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05nyyp39x4wzc1959kv7ckwqpkdzjd9dw4slzyjh73qbhjcfqayr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/tomprogrammer/rust-ascii")
    (synopsis "ASCII-only equivalents to `char`, `str` and `String`.")
    (description "ASCII-only equivalents to `char`, `str` and `String`.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-borsh-1
  (package
    (name "rust-borsh")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "borsh" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vh5sjz4igvsg6013m2n02w2iwkhv1i51zjbfwpwkgfdynmpyqdz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ascii" ,rust-ascii-1)
                       ("rust-borsh-derive" ,rust-borsh-derive-1)
                       ("rust-bson" ,rust-bson-2)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-aliases" ,rust-cfg-aliases-0.1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14))))
    (home-page "https://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-decimal-1
  (package
    (name "rust-rust-decimal")
    (version "1.33.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rust_decimal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i0xgwgdnhbcbj8x6lw7pcgg4jb5lkww1j1wfbdc33ybbkn6lrq6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-borsh" ,rust-borsh-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-diesel" ,rust-diesel-1)
                       ("rust-diesel" ,rust-diesel-2)
                       ("rust-ndarray" ,rust-ndarray-0.15)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-postgres" ,rust-postgres-0.19)
                       ("rust-proptest" ,rust-proptest-1)
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

(define-public rust-mac-address-1
  (package
    (name "rust-mac-address")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mac_address" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0miqmn1768vxr5f3gbsnr1dlnzc5ilrrj0n07fziblwyy6afwqs8"))))
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

(define-public rust-serde-derive-internals-0.26
  (package
    (name "rust-serde-derive-internals")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive_internals" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g2zdr6s8i0r29yy7pdl6ahimq8w6ck70hvrciiry2ljwwlq5gw5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis "AST representation used by Serde derive macros. Unstable.")
    (description "AST representation used by Serde derive macros.  Unstable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-schemars-derive-0.8
  (package
    (name "rust-schemars-derive")
    (version "0.8.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rq9sdcf5hyvsyj9v9nfy2jgjbjzaldjq4i6y2fcz72xlrpzsry7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde-derive-internals" ,rust-serde-derive-internals-0.26)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Macros for #[derive(JsonSchema)], for use with schemars")
    (description
     "Macros for #[derive(@code{JsonSchema})], for use with schemars")
    (license license:expat)))

(define-public rust-bigdecimal-0.4
  (package
    (name "rust-bigdecimal")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bigdecimal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jcbzgna6292vgq0slw5iah929wl0xbps22zr63bp99y8az1jrn0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-libm" ,rust-libm-0.2)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/akubera/bigdecimal-rs")
    (synopsis "Arbitrary precision decimal numbers")
    (description "Arbitrary precision decimal numbers")
    (license (list license:expat license:asl2.0))))

(define-public rust-schemars-0.8
  (package
    (name "rust-schemars")
    (version "0.8.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schemars" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aadpjkaq7yl11b02pg4mwanylck328zg0q7w56dv6j89568z8j5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.4)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-either" ,rust-either-1)
                       ("rust-enumset" ,rust-enumset-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-schemars-derive" ,rust-schemars-derive-0.8)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-smol-str" ,rust-smol-str-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://graham.cool/schemars/")
    (synopsis "Generate JSON Schemas from Rust code")
    (description "Generate JSON Schemas from Rust code")
    (license license:expat)))

(define-public rust-ipnetwork-0.20
  (package
    (name "rust-ipnetwork")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ipnetwork" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03hhmxyimz0800z44wl3z1ak8iw91xcnk7sgx5p5jinmx50naimz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-schemars" ,rust-schemars-0.8)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/achanda/ipnetwork")
    (synopsis "A library to work with IP CIDRs in Rust")
    (description
     "This package provides a library to work with IP CIDRs in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-intrusive-0.5
  (package
    (name "rust-futures-intrusive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-intrusive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vwm08d1pli6bdaj0i7xhk3476qlx4pll6i0w03gzdnh7lh0r4qx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))
    (home-page "https://github.com/Matthias247/futures-intrusive")
    (synopsis
     "Futures based on intrusive data structures - for std and no-std environments.
")
    (description
     "Futures based on intrusive data structures - for std and no-std environments.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crc-catalog-2
  (package
    (name "rust-crc-catalog")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crc-catalog" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr"))))
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
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zkx87a5x06xfd6xm5956w4vmdfs0wcxpsn7iwj5jbp2rcapmv46"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crc-catalog" ,rust-crc-catalog-2))))
    (home-page "https://github.com/mrhooray/crc-rs.git")
    (synopsis "Rust implementation of CRC with support of various standards")
    (description
     "Rust implementation of CRC with support of various standards")
    (license (list license:expat license:asl2.0))))

(define-public rust-atoi-2
  (package
    (name "rust-atoi")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atoi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a05h42fggmy7h0ajjv6m7z72l924i7igbx13hk9d8pyign9k3gj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/pacman82/atoi-rs")
    (synopsis "Parse integers directly from `[u8]` slices in safe code")
    (description "Parse integers directly from `[u8]` slices in safe code")
    (license license:expat)))

(define-public rust-zerocopy-derive-0.7
  (package
    (name "rust-zerocopy-derive")
    (version "0.7.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "146a01cx6cqm3gm6b133fj5hkf6gcrlfk73rganrzdxcwf4js0g1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description "Custom derive for traits from the zerocopy crate")
    (license (list license:bsd-2 license:asl2.0 license:expat))))

(define-public rust-zerocopy-0.7
  (package
    (name "rust-zerocopy")
    (version "0.7.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "067yx9iq018kqp6imyg1hrbbxz6b88csnbaa66li8flaax1f6ggl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-zerocopy-derive" ,rust-zerocopy-derive-0.7))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis "Utilities for zero-copy parsing and serialization")
    (description "Utilities for zero-copy parsing and serialization")
    (license (list license:bsd-2 license:asl2.0 license:expat))))

(define-public rust-ahash-0.8
  (package
    (name "rust-ahash")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ahash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yn9i8nc6mmv28ig9w3dga571q09vg9f1f650mi5z8phx42r6hli"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic-polyfill" ,rust-atomic-polyfill-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-const-random" ,rust-const-random-0.1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-zerocopy" ,rust-zerocopy-0.7))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
     "A non-cryptographic hash function using AES-NI for high performance")
    (description
     "This package provides a non-cryptographic hash function using AES-NI for high
performance")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-core-0.7
  (package
    (name "rust-sqlx-core")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gdz44yb9qwxv4xl4hv6w4vbqx0zzdlzsf9j9gcj1qir6wy0ljyq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-async-io" ,rust-async-io-1)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-atoi" ,rust-atoi-2)
                       ("rust-bigdecimal" ,rust-bigdecimal-0.3)
                       ("rust-bit-vec" ,rust-bit-vec-0.6)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-dotenvy" ,rust-dotenvy-0.15)
                       ("rust-either" ,rust-either-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-event-listener" ,rust-event-listener-2)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-intrusive" ,rust-futures-intrusive-0.5)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hashlink" ,rust-hashlink-0.8)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-ipnetwork" ,rust-ipnetwork-0.20)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mac-address" ,rust-mac-address-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rust-decimal" ,rust-rust-decimal-1)
                       ("rust-rustls" ,rust-rustls-0.21)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-sqlformat" ,rust-sqlformat-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.25))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Core of SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
     "Core of SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-0.7
  (package
    (name "rust-sqlx")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kv3hyx7izmmsjqh3l47zrfhjlcblpg20cvnk7pr8dm7klkkr86v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-sqlx-core" ,rust-sqlx-core-0.7)
                       ("rust-sqlx-macros" ,rust-sqlx-macros-0.7)
                       ("rust-sqlx-mysql" ,rust-sqlx-mysql-0.7)
                       ("rust-sqlx-postgres" ,rust-sqlx-postgres-0.7)
                       ("rust-sqlx-sqlite" ,rust-sqlx-sqlite-0.7))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "ð§° The Rust SQL Toolkit. An async, pure Rust SQL crate featuring compile-time checked queries without a DSL. Supports PostgreSQL, MySQL, and SQLite.")
    (description
     "ð§° The Rust SQL Toolkit.  An async, pure Rust SQL crate featuring compile-time
checked queries without a DSL. Supports @code{PostgreSQL}, @code{MySQL}, and
SQLite.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.171")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10j6s97fk7fgjiqhhrx6a44rqxr7v3w985i3avx4d36i7dh9961q"))))
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
    (version "1.0.171")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a9lvibgi42mhmgafp747mvshsq6ybx6rzcjqh398rfp9wg7vqih"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-common-17
  (package
    (name "rust-atuin-common")
    (version "17.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-common" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1331wphyk94hlq02r7smgdy88i6xdci57w8nk6c897adlf6n3mal"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-eyre" ,rust-eyre-0.6)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sqlx" ,rust-sqlx-0.7)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-typed-builder" ,rust-typed-builder-0.15)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://atuin.sh")
    (synopsis "common library for atuin")
    (description "common library for atuin")
    (license license:expat)))

(define-public rust-atuin-client-17
  (package
    (name "rust-atuin-client")
    (version "17.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qq3ylzj7zmms0w7i8b12dna45wnkmaqnh6n0a3xjlympaysac05"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-common" ,rust-atuin-common-17)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-config" ,rust-config-0.13)
                       ("rust-crypto-secretbox" ,rust-crypto-secretbox-0.1)
                       ("rust-directories" ,rust-directories-4)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-interim" ,rust-interim-0.1)
                       ("rust-itertools" ,rust-itertools-0.11)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-minspan" ,rust-minspan-0.1)
                       ("rust-parse-duration" ,rust-parse-duration-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-rmp" ,rust-rmp-0.8)
                       ("rust-rusty-paserk" ,rust-rusty-paserk-0.3)
                       ("rust-rusty-paseto" ,rust-rusty-paseto-0.6)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-regex" ,rust-serde-regex-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-shellexpand" ,rust-shellexpand-3)
                       ("rust-sql-builder" ,rust-sql-builder-3)
                       ("rust-sqlx" ,rust-sqlx-0.7)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-typed-builder" ,rust-typed-builder-0.15)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://atuin.sh")
    (synopsis "client library for atuin")
    (description "client library for atuin")
    (license license:expat)))

(define-public rust-metrics-macros-0.7
  (package
    (name "rust-metrics-macros")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pxn61w1j4jhwp8q5ml5q9qz4v9p1iiv0kaaqy2ibhrlzmmf5v6x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Macros for the metrics crate.")
    (description "Macros for the metrics crate.")
    (license license:expat)))

(define-public rust-metrics-0.21
  (package
    (name "rust-metrics")
    (version "0.21.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "metrics" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ibndxzk0sja8cgwrr73b9vzbgfvwzwxwkxqiivnmmwy00dazqzx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests pass on earlier version of rust
       #:cargo-inputs
       (("rust-metrics-macros" ,rust-metrics-macros-0.7)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Lightweight metrics facade")
    (description "This package provides a lightweight metrics facade.")
    (license license:expat)))

(define-public rust-prost-derive-0.11
  (package
    (name "rust-prost-derive")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d3mw2s2jba1f7wcjmjd6ha2a255p2rmynxhm1nysv9w1z8xilp5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "A Protocol Buffers implementation for the Rust Language.")
    (description
     "This package provides a Protocol Buffers implementation for the Rust Language.")
    (license license:asl2.0)))


(define-public rust-prost-0.11
  (package
    (name "rust-prost")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kc1hva2h894hc0zf6r4r8fsxfpazf7xn5rj3jya9sbrsyhym0hb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-prost-derive" ,rust-prost-derive-0.11))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.8)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "A Protocol Buffers implementation for the Rust Language.")
    (description
     "This package provides a Protocol Buffers implementation for the Rust Language.")
    (license license:asl2.0)))

(define-public rust-noisy-float-0.2
  (package
    (name "rust-noisy-float")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "noisy_float" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bqkl82pfp98i98s3van73hkvqcx5p55dm1wagg57gy0xgkfd3wp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-approx" ,rust-approx-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/SergiusIW/noisy_float-rs")
    (synopsis
     "Contains floating point types that panic if they are set to an illegal value, such as NaN")
    (description
     "This package contains floating point types that panic if they are set to an
illegal value, such as @code{NaN}")
    (license license:asl2.0)))

(define-public rust-ndarray-stats-0.5
  (package
    (name "rust-ndarray-stats")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ndarray-stats" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "007lr2q3dq17n69dlkm2md1jkh9nhz1f0rzxs5dpp1wnmivq8nmg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-ndarray" ,rust-ndarray-0.15)
                       ("rust-noisy-float" ,rust-noisy-float-0.2)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rand" ,rust-rand-0.8))
       #:cargo-development-inputs (("rust-approx" ,rust-approx-0.4)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-ndarray" ,rust-ndarray-0.15)
                                   ("rust-ndarray-rand" ,rust-ndarray-rand-0.14)
                                   ("rust-num-bigint" ,rust-num-bigint-0.4)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-quickcheck-macros" ,rust-quickcheck-macros-1))))
    (home-page "https://github.com/rust-ndarray/ndarray-stats")
    (synopsis
     "Statistical routines for ArrayBase, the n-dimensional array data structure provided by ndarray.")
    (description
     "Statistical routines for @code{ArrayBase}, the n-dimensional array data
structure provided by ndarray.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sketches-ddsketch-0.2
  (package
    (name "rust-sketches-ddsketch")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sketches-ddsketch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q873ja2yvvls9327a7yw1mcprw0ia2cjj72snfg5mrfi30hd938"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-approx" ,rust-approx-0.5)
                                   ("rust-rand" ,rust-rand-0.7)
                                   ("rust-rand-distr" ,rust-rand-distr-0.2))))
    (home-page "https://github.com/mheffner/rust-sketches-ddsketch")
    (synopsis "A direct port of the Golang DDSketch implementation.
")
    (description
     "This package provides a direct port of the Golang DDSketch implementation.")
    (license license:asl2.0)))

(define-public rust-prost-types-0.11
  (package
    (name "rust-prost-types")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ryk38sqkp2nf4dgdqdfbgn6zwwvjraw6hqq6d9a6088shj4di1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-prost" ,rust-prost-0.11))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "A Protocol Buffers implementation for the Rust Language.")
    (description
     "This package provides a Protocol Buffers implementation for the Rust Language.")
    (license license:asl2.0)))

(define-public rust-mach2-0.4
  (package
    (name "rust-mach2")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mach2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/JohnTitor/mach2")
    (synopsis
     "A Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX.")
    (description
     "This package provides a Rust interface to the user-space API of the Mach 3.0
kernel that underlies OSX.")
    (license (list license:bsd-2 license:expat license:asl2.0))))

(define-public rust-quanta-0.11
  (package
    (name "rust-quanta")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quanta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1axrw0nqc90bq671w05jd9460pmwg86c4r132mjsi4c2g8m6czm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-prost-types" ,rust-prost-types-0.11)
                       ("rust-raw-cpuid" ,rust-raw-cpuid-10)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-average" ,rust-average-0.13)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description "high-speed timing library")
    (license license:expat)))

(define-public rust-metrics-util-0.15
  (package
    (name "rust-metrics-util")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0glpkmrj7zkg9b290x6qxf93kmd9b4b4sbkk1fs19l8y95pfvqjd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-metrics" ,rust-metrics-0.21)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-ordered-float" ,rust-ordered-float-3)
                       ("rust-quanta" ,rust-quanta-0.11)
                       ("rust-radix-trie" ,rust-radix-trie-0.2)
                       ("rust-sketches-ddsketch" ,rust-sketches-ddsketch-0.2))
       #:cargo-development-inputs (("rust-approx" ,rust-approx-0.5)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
                                   ("rust-getopts" ,rust-getopts-0.2)
                                   ("rust-hdrhistogram" ,rust-hdrhistogram-7)
                                   ("rust-mockall" ,rust-mockall-0.11)
                                   ("rust-ndarray" ,rust-ndarray-0.15)
                                   ("rust-ndarray-stats" ,rust-ndarray-stats-0.5)
                                   ("rust-noisy-float" ,rust-noisy-float-0.2)
                                   ("rust-ordered-float" ,rust-ordered-float-3)
                                   ("rust-predicates-core" ,rust-predicates-core-1)
                                   ("rust-predicates-tree" ,rust-predicates-tree-1)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-distr" ,rust-rand-distr-0.4)
                                   ("rust-sketches-ddsketch" ,rust-sketches-ddsketch-0.2)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Helper types/functions used by the metrics ecosystem.")
    (description "Helper types/functions used by the metrics ecosystem.")
    (license license:expat)))

(define-public rust-metrics-exporter-prometheus-0.12
  (package
    (name "rust-metrics-exporter-prometheus")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics-exporter-prometheus" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l19s21jfmwm72cxfjq35xb79a5wi4fv7c1p993dnqj8gk7afkqx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-metrics" ,rust-metrics-0.21)
                       ("rust-metrics-util" ,rust-metrics-util-0.15)
                       ("rust-quanta" ,rust-quanta-0.11)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-tracing" ,rust-tracing-0.1)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/metrics-rs/metrics")
    (license #f)
    (synopsis
     "A metrics-compatible exporter for sending metrics to Prometheus.")
    (description
     "This package provides a metrics-compatible exporter for sending metrics to
Prometheus.")))

(define-public atuin
  (package
    (name "atuin")
    (version "17.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13r7p9gq8ka7npd905gii5pzpdpvr7viggvr2i48brdvqhm096j3"))))
    (build-system cargo-build-system)
    (native-inputs (list perl))
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-atuin-client" ,rust-atuin-client-17)
                       ("rust-atuin-common" ,rust-atuin-common-17)
                       ("rust-atuin-server" ,rust-atuin-server-17)
                       ("rust-atuin-server-postgres" ,rust-atuin-server-postgres-17)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete" ,rust-clap-complete-4)
                       ("rust-cli-clipboard" ,rust-cli-clipboard-0.4)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-directories" ,rust-directories-4)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-interim" ,rust-interim-0.1)
                       ("rust-itertools" ,rust-itertools-0.11)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ratatui" ,rust-ratatui-0.24)
                       ("rust-rpassword" ,rust-rpassword-7)
                       ("rust-runtime-format" ,rust-runtime-format-0.1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tiny-bip39" ,rust-tiny-bip39-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-whoami" ,rust-whoami-1)
                       ("rust-metrics" ,rust-metrics-0.21)
                       ("rust-metrics-exporter-prometheus" ,rust-metrics-exporter-prometheus-0.12))
       #:cargo-development-inputs (("rust-tracing-tree" ,rust-tracing-tree-0.2))))
    (home-page "https://atuin.sh")
    (synopsis "atuin - magical shell history")
    (description "atuin - magical shell history")
    (license license:expat)))

