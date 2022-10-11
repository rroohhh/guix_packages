(define-module (vup vaultwarden)
 #:use-module (vup rust-nightly)
 #:use-module (gnu packages perl)
 #:use-module (guix build-system cargo)
 #:use-module (guix build-system copy)
 #:use-module (guix gexp)
 #:use-module (guix packages)
 #:use-module (guix download)
 #:use-module ((guix import utils) #:select (beautify-description))
 #:use-module (guix git-download)
 #:use-module ((guix licenses) #:prefix license:))

(define rust-addr2line_0_17_0
  (package
    (name "rust-addr2line")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "addr2line" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sw16zqy6w0ar633z69m7lw6gb0k1y7xj3387a8wly43ij5div5r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-gimli" ,rust-gimli_0_26_1))))
    (home-page "None")
    (synopsis "A cross-platform symbolication library written in Rust, using `gimli`")
    (description
      (beautify-description "A cross-platform symbolication library written in Rust, using `gimli`"))
    (license license:asl2.0)))

(define rust-adler_1_0_2
  (package
    (name "rust-adler")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "adler" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A simple clean-room implementation of the Adler-32 checksum")
    (description
      (beautify-description "A simple clean-room implementation of the Adler-32 checksum"))
    (license license:asl2.0)))

(define rust-aead_0_4_3
  (package
    (name "rust-aead")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aead" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xw8kp9j1whfdxhgmr2qf9xgslkg52zh6gzmhsh13y9w3s73nq8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5))))
    (home-page "None")
    (synopsis "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms,\nsuch as AES-GCM as ChaCha20Poly1305, which provide a high-level API")
    (description
      (beautify-description "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms,\nsuch as AES-GCM as ChaCha20Poly1305, which provide a high-level API"))
    (license license:expat)))

(define rust-aes_0_7_5
  (package
    (name "rust-aes")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f0sdx2fsa8w3l7xzsyi9ry3shvnnsgc0znh50if9fm95vslg2wy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-cpufeatures" ,rust-cpufeatures_0_2_2)        
        ("rust-opaque-debug" ,rust-opaque-debug_0_3_0))
       #:cargo-development-inputs
       (("rust-cipher" ,rust-cipher_0_3_0))))
    (home-page "None")
    (synopsis "Pure Rust implementation of the Advanced Encryption Standard (a.k.a. Rijndael)")
    (description
      (beautify-description "Pure Rust implementation of the Advanced Encryption Standard (a.k.a. Rijndael)"))
    (license license:expat)))

(define rust-aes-gcm_0_9_4
  (package
    (name "rust-aes-gcm")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aes-gcm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xndncn1phjb7pjam63vl0yp7h8jh95m0yxanr1092vx7al8apyz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aead" ,rust-aead_0_4_3)        
        ("rust-aes" ,rust-aes_0_7_5)        
        ("rust-cipher" ,rust-cipher_0_3_0)        
        ("rust-ctr" ,rust-ctr_0_8_0)        
        ("rust-ghash" ,rust-ghash_0_4_4)        
        ("rust-subtle" ,rust-subtle_2_4_1))))
    (home-page "None")
    (synopsis "Pure Rust implementation of the AES-GCM (Galois/Counter Mode)\nAuthenticated Encryption with Associated Data (AEAD) Cipher\nwith optional architecture-specific hardware acceleration")
    (description
      (beautify-description "Pure Rust implementation of the AES-GCM (Galois/Counter Mode)\nAuthenticated Encryption with Associated Data (AEAD) Cipher\nwith optional architecture-specific hardware acceleration"))
    (license license:asl2.0)))

(define rust-aho-corasick_0_7_18
  (package
    (name "rust-aho-corasick")
    (version "0.7.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vv50b3nvkhyy7x7ip19qnsq11bqlnffkmj2yx2xlyk5wzawydqy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr_2_5_0))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      (beautify-description "Fast multiple substring searching."))
    (license (list license:unlicense
               license:expat))))

(define rust-alloc-no-stdlib_2_0_3
  (package
    (name "rust-alloc-no-stdlib")
    (version "2.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alloc-no-stdlib" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cqxcfhsd85cywzqb1f8wlix08gmjlm5ncn4wpmc9l8a94q4gvrm"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dropbox/rust-alloc-no-stdlib")
    (synopsis "A dynamic allocator that may be used with or without the stdlib. This allows a package with nostd to allocate memory dynamically and be used either with a custom allocator, items on the stack, or by a package that wishes to simply use Box\u003c\u003e. It also provides options to use calloc or a mutable global variable for pre-zeroed memory")
    (description
      (beautify-description "A dynamic allocator that may be used with or without the stdlib. This allows a package with nostd to allocate memory dynamically and be used either with a custom allocator, items on the stack, or by a package that wishes to simply use Box\u003c\u003e. It also provides options to use calloc or a mutable global variable for pre-zeroed memory"))
    (license license:bsd-3)))

(define rust-alloc-stdlib_0_2_1
  (package
    (name "rust-alloc-stdlib")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alloc-stdlib" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hj3r1x88aajnvigdck0diygj2isc90wa271kkj1swgiq3nxfzk9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib_2_0_3))))
    (home-page "https://github.com/dropbox/rust-alloc-no-stdlib")
    (synopsis "A dynamic allocator example that may be used with the stdlib")
    (description
      (beautify-description "A dynamic allocator example that may be used with the stdlib"))
    (license license:bsd-3)))

(define rust-ansi_term_0_12_1
  (package
    (name "rust-ansi_term")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles (bold, underline)")
    (description
      (beautify-description "Library for ANSI terminal colours and styles (bold, underline)"))
    (license license:expat)))

(define rust-async-compression_0_3_13
  (package
    (name "rust-async-compression")
    (version "0.3.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-compression" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16fgb5jyr3m830slh0n9w32klx53lb1ichayzkd81b02zy2cg2c5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-brotli" ,rust-brotli_3_3_4)        
        ("rust-flate2" ,rust-flate2_1_0_23)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "None")
    (synopsis "Adaptors between compression crates and Rust\u0027s modern asynchronous IO types.")
    (description
      (beautify-description "Adaptors between compression crates and Rust\u0027s modern asynchronous IO types."))
    (license license:expat)))

(define rust-async-stream_0_3_3
  (package
    (name "rust-async-stream")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-stream" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zjhv5l6yjyy46r1myazy00njmhw19jgxppswydrdsgag4qcimfs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-stream-impl" ,rust-async-stream-impl_0_3_3)        
        ("rust-futures-core" ,rust-futures-core_0_3_21))))
    (home-page "None")
    (synopsis "Asynchronous streams using async \u0026 await notation")
    (description
      (beautify-description "Asynchronous streams using async \u0026 await notation"))
    (license license:expat)))

(define rust-async-stream-impl_0_3_3
  (package
    (name "rust-async-stream-impl")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-stream-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09xy4ryvh8qdj3da4vascb9g69psj0wpc8nxnqpzl7d7fgdh7whh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "proc macros for async-stream crate")
    (description
      (beautify-description "proc macros for async-stream crate"))
    (license license:expat)))

(define rust-async-trait_0_1_53
  (package
    (name "rust-async-trait")
    (version "0.1.53")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-trait" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "003nzwgwb2apz6nww5wmh66k8f47npifll8c33zgkz1d999a6spd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Type erasure for async trait methods")
    (description
      (beautify-description "Type erasure for async trait methods"))
    (license license:expat)))

(define rust-async_once_0_2_6
  (package
    (name "rust-async_once")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async_once" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10nyq5s78lfds0scxncbfx52ncy5s68rzbivhwbndkdblc7g3r1c"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/hjiayz/async_once")
    (synopsis "async once tool for lazy_static")
    (description
      (beautify-description "async once tool for lazy_static"))
    (license license:expat)))

(define rust-atomic_0_5_1
  (package
    (name "rust-atomic")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atomic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k135q1qfmxxyzrlhr47r0j38r5fnd4163rgl552qxyagrk853dq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_1_0))))
    (home-page "None")
    (synopsis "Generic Atomic\u003cT\u003e wrapper type")
    (description
      (beautify-description "Generic Atomic\u003cT\u003e wrapper type"))
    (license (list license:asl2.0
               license:expat))))

(define rust-atty_0_2_14
  (package
    (name "rust-atty")
    (version "0.2.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hermit-abi" ,rust-hermit-abi_0_1_19)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
      (beautify-description "A simple interface for querying atty"))
    (license license:expat)))

(define rust-autocfg_1_1_0
  (package
    (name "rust-autocfg")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Automatic cfg for Rust compiler features")
    (description
      (beautify-description "Automatic cfg for Rust compiler features"))
    (license license:asl2.0)))

(define rust-backtrace_0_3_65
  (package
    (name "rust-backtrace")
    (version "0.3.65")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qggp0d8pbw5vfnpm0r7lrn6wmh5yjiz4yc4bzynb8l26i2pv88i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-addr2line" ,rust-addr2line_0_17_0)        
        ("rust-cc" ,rust-cc_1_0_73)        
        ("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-miniz_oxide" ,rust-miniz_oxide_0_5_1)        
        ("rust-object" ,rust-object_0_28_4)        
        ("rust-rustc-demangle" ,rust-rustc-demangle_0_1_21))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis "A library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (description
      (beautify-description "A library to acquire a stack trace (backtrace) at runtime in a Rust program."))
    (license (list license:expat
               license:asl2.0))))

(define rust-base-x_0_2_10
  (package
    (name "rust-base-x")
    (version "0.2.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base-x" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x5ggdzbh41bh0kj23rng9i10386887174wp6zikzgaggf9s86fw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/OrKoN/base-x-rs")
    (synopsis "Encode/decode any base")
    (description
      (beautify-description "Encode/decode any base"))
    (license license:expat)))

(define rust-base64_0_13_0
  (package
    (name "rust-base64")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z82g23mbzjgijkpcrilc7nljpxpvpf7zxf6iyiapkgka2ngwkch"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description
      (beautify-description "encodes and decodes base64 as bytes or utf8"))
    (license (list license:expat
               license:asl2.0))))

(define rust-binascii_0_1_4
  (package
    (name "rust-binascii")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "binascii" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wnaglgl72pn5ilv61q6y34w76gbg7crb8ifqk6lsxnq2gajjg9q"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Useful no-std binascii operations including base64, base32 and base16 (hex)")
    (description
      (beautify-description "Useful no-std binascii operations including base64, base32 and base16 (hex)"))
    (license license:expat)))

(define rust-bitflags_1_3_2
  (package
    (name "rust-bitflags")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
      (beautify-description "A macro to generate structures which behave like bitflags."))
    (license (list license:expat
               license:asl2.0))))

(define rust-block-buffer_0_7_3
  (package
    (name "rust-block-buffer")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-buffer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12v8wizynqin0hqf140kmp9s38q223mp1b0hkqk8j5pk8720v560"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-padding" ,rust-block-padding_0_1_5)        
        ("rust-byte-tools" ,rust-byte-tools_0_3_1)        
        ("rust-byteorder" ,rust-byteorder_1_4_3)        
        ("rust-generic-array" ,rust-generic-array_0_12_4))))
    (home-page "None")
    (synopsis "Buffer type for block processing of data")
    (description
      (beautify-description "Buffer type for block processing of data"))
    (license license:expat)))

(define rust-block-buffer_0_9_0
  (package
    (name "rust-block-buffer")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-buffer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r4pf90s7d7lj1wdjhlnqa26vvbm6pnc33z138lxpnp9srpi2lj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5))))
    (home-page "None")
    (synopsis "Buffer type for block processing of data")
    (description
      (beautify-description "Buffer type for block processing of data"))
    (license license:expat)))

(define rust-block-buffer_0_10_2
  (package
    (name "rust-block-buffer")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-buffer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "097k9xkd8gqrl03qg4fwhjvanp3ac0pq4drg8pynk9cyhi8zxxqb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5))))
    (home-page "None")
    (synopsis "Buffer type for block processing of data")
    (description
      (beautify-description "Buffer type for block processing of data"))
    (license license:expat)))

(define rust-block-padding_0_1_5
  (package
    (name "rust-block-padding")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-padding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xbkmysiz23vimd17rnsjpw9bgjxipwfslwyygqlkx4in3dxwygs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byte-tools" ,rust-byte-tools_0_3_1))))
    (home-page "None")
    (synopsis "Padding and unpadding of messages divided into blocks.")
    (description
      (beautify-description "Padding and unpadding of messages divided into blocks."))
    (license license:expat)))

(define rust-brotli_3_3_4
  (package
    (name "rust-brotli")
    (version "3.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "brotli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s7z0nrv04wxniwijh5iig1w31sphc6lz38zc8lr7qlarkdv3851"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib_2_0_3)        
        ("rust-alloc-stdlib" ,rust-alloc-stdlib_0_2_1)        
        ("rust-brotli-decompressor" ,rust-brotli-decompressor_2_3_2))))
    (home-page "https://github.com/dropbox/rust-brotli")
    (synopsis "A brotli compressor and decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. All included code is safe.")
    (description
      (beautify-description "A brotli compressor and decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. All included code is safe."))
    (license (list license:bsd-3
               license:expat))))

(define rust-brotli-decompressor_2_3_2
  (package
    (name "rust-brotli-decompressor")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "brotli-decompressor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "102v89h9z0p45j5fsjna97761nxx9nxz9ccpwxma6p5zad32vbar"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib_2_0_3)        
        ("rust-alloc-stdlib" ,rust-alloc-stdlib_0_2_1))))
    (home-page "https://github.com/dropbox/rust-brotli-decompressor")
    (synopsis "A brotli decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. Alternatively, --features=unsafe turns off array bounds checks and memory initialization but provides a safe interface for the caller.  Without adding the --features=unsafe argument, all included code is safe. For compression in addition to this library, download https://github.com/dropbox/rust-brotli")
    (description
      (beautify-description "A brotli decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. Alternatively, --features=unsafe turns off array bounds checks and memory initialization but provides a safe interface for the caller.  Without adding the --features=unsafe argument, all included code is safe. For compression in addition to this library, download https://github.com/dropbox/rust-brotli"))
    (license (list license:bsd-3
               license:expat))))

(define rust-bumpalo_3_9_1
  (package
    (name "rust-bumpalo")
    (version "3.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bumpalo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1688dv6s0cbj72p9lmll8a02a85dzxvdw2is7pji490zmd35m954"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A fast bump allocation arena for Rust.")
    (description
      (beautify-description "A fast bump allocation arena for Rust."))
    (license (list license:expat
               license:asl2.0))))

(define rust-byte-tools_0_3_1
  (package
    (name "rust-byte-tools")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byte-tools" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mqi29wsm8njpl51pfwr31wmpzs5ahlcb40wsjyd92l90ixcmdg3"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Bytes related utility functions")
    (description
      (beautify-description "Bytes related utility functions"))
    (license license:expat)))

(define rust-byteorder_1_4_3
  (package
    (name "rust-byteorder")
    (version "1.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byteorder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0456lv9xi1a5bcm32arknf33ikv76p3fr9yzki4lb2897p2qkh8l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/byteorder")
    (synopsis "Library for reading/writing numbers in big-endian and little-endian.")
    (description
      (beautify-description "Library for reading/writing numbers in big-endian and little-endian."))
    (license license:expat)))

(define rust-bytes_0_4_12
  (package
    (name "rust-bytes")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0768a55q2fsqdjsvcv98ndg9dq7w2g44dvq1avhwpxrdzbydyvr0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder_1_4_3)        
        ("rust-iovec" ,rust-iovec_0_1_4))))
    (home-page "None")
    (synopsis "Types and traits for working with bytes")
    (description
      (beautify-description "Types and traits for working with bytes"))
    (license license:expat)))

(define rust-bytes_1_1_0
  (package
    (name "rust-bytes")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y70b249m02lfp0j6565b29kviapj4xsl9whamcqwddnp9kjv1y4"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Types and traits for working with bytes")
    (description
      (beautify-description "Types and traits for working with bytes"))
    (license license:expat)))

(define rust-cached_0_34_0
  (package
    (name "rust-cached")
    (version "0.34.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16kc2xmywza7bw6wr2idwjp4xh4vbc8ypwdqprgb7fklxbfpdpxa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait_0_1_53)        
        ("rust-async_once" ,rust-async_once_0_2_6)        
        ("rust-cached_proc_macro" ,rust-cached_proc_macro_0_12_0)        
        ("rust-cached_proc_macro_types" ,rust-cached_proc_macro_types_0_1_0)        
        ("rust-futures" ,rust-futures_0_3_21)        
        ("rust-hashbrown" ,rust-hashbrown_0_12_1)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-once_cell" ,rust-once_cell_1_10_0)        
        ("rust-thiserror" ,rust-thiserror_1_0_31)        
        ("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license license:expat)))

(define rust-cached_proc_macro_0_12_0
  (package
    (name "rust-cached_proc_macro")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached_proc_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bydrvl47d8s7lycpjjjn0m3sc6amn4hq1izvwybkikpkdzz7q5w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cached_proc_macro_types" ,rust-cached_proc_macro_types_0_1_0)        
        ("rust-darling" ,rust-darling_0_13_4)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license license:expat)))

(define rust-cached_proc_macro_types_0_1_0
  (package
    (name "rust-cached_proc_macro_types")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached_proc_macro_types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qrni8q6vj1l1d62fgyln1s1scchk2q9jv8whl0p6dmlj58r4krs"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license license:expat)))

(define rust-cc_1_0_73
  (package
    (name "rust-cc")
    (version "1.0.73")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04ccylrjq94jssh8f7d7hxv64gs9f1m1jrsxb7wqgfxk4xljmzrg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode.")
    (description
      (beautify-description "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode."))
    (license (list license:expat
               license:asl2.0))))

(define rust-cfg-if_0_1_10
  (package
    (name "rust-cfg-if")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08h80ihs74jcyp24cd75wwabygbbdgl05k6p5dmq8akbr78vv1a7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted.")
    (description
      (beautify-description "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted."))
    (license (list license:expat
               license:asl2.0))))

(define rust-cfg-if_1_0_0
  (package
    (name "rust-cfg-if")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted.")
    (description
      (beautify-description "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted."))
    (license (list license:expat
               license:asl2.0))))

(define rust-chashmap_2_2_2
  (package
    (name "rust-chashmap")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chashmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0igsvpc2ajd6w68w4dwn0fln6yww8gq4pq9x02wj36g3q71a6hgz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-owning_ref" ,rust-owning_ref_0_3_3)        
        ("rust-parking_lot" ,rust-parking_lot_0_4_8))))
    (home-page "None")
    (synopsis "Fast, concurrent hash maps with extensive API.")
    (description
      (beautify-description "Fast, concurrent hash maps with extensive API."))
    (license license:expat)))

(define rust-chrono_0_4_19
  (package
    (name "rust-chrono")
    (version "0.4.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wyfl6c00vhfl562spnfcna3zkw8jqvcp652m9iskhl8j26dc2k7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125)        
        ("rust-num-integer" ,rust-num-integer_0_1_45)        
        ("rust-num-traits" ,rust-num-traits_0_2_15)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-time" ,rust-time_0_1_43)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description
      (beautify-description "Date and time library for Rust"))
    (license (list license:expat
               license:asl2.0))))

(define rust-chrono-tz_0_6_1
  (package
    (name "rust-chrono-tz")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono-tz" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lim4lvdras2wqc7ix6l8g47nk4mpkah4880cg780c6s88c9ym2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chrono-tz-build" ,rust-chrono-tz-build_0_0_2)        
        ("rust-phf" ,rust-phf_0_10_1))
       #:cargo-development-inputs
       (("rust-chrono" ,rust-chrono_0_4_19))))
    (home-page "None")
    (synopsis "TimeZone implementations for rust-chrono from the IANA database")
    (description
      (beautify-description "TimeZone implementations for rust-chrono from the IANA database"))
    (license (list license:expat
               license:asl2.0))))

(define rust-chrono-tz-build_0_0_2
  (package
    (name "rust-chrono-tz-build")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono-tz-build" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0schy3z03psvmc6734hgkx52cdb3zvixgzhvhr0mzxmj7x4qs1fv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-parse-zoneinfo" ,rust-parse-zoneinfo_0_3_0)        
        ("rust-phf" ,rust-phf_0_10_1)        
        ("rust-phf_codegen" ,rust-phf_codegen_0_10_0))))
    (home-page "None")
    (synopsis "internal build script for chrono-tz")
    (description
      (beautify-description "internal build script for chrono-tz"))
    (license (list license:expat
               license:asl2.0))))

(define rust-cipher_0_3_0
  (package
    (name "rust-cipher")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cipher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dyzsv0c84rgz98d5glnhsz4320wl24x3bq511vnyf0mxir21rby"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5))))
    (home-page "None")
    (synopsis "Traits for describing block ciphers and stream ciphers")
    (description
      (beautify-description "Traits for describing block ciphers and stream ciphers"))
    (license license:expat)))

(define rust-const_fn_0_4_9
  (package
    (name "rust-const_fn")
    (version "0.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const_fn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0df9fv9jhnh9b4ni3s2fbfcvq77iia4lbb89fklwawbgv2vdrp7v"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An attribute for easy generation of const functions with conditional compilations.")
    (description
      (beautify-description "An attribute for easy generation of const functions with conditional compilations."))
    (license license:asl2.0)))

(define rust-cookie_0_15_1
  (package
    (name "rust-cookie")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03gql9c2l0wg3hpfp67wg2ns21wysk0xsjxwdbjrf0s6grrcgwfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-percent-encoding" ,rust-percent-encoding_2_1_0)        
        ("rust-time" ,rust-time_0_2_27)        
        ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars.")
    (description
      (beautify-description "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars."))
    (license license:expat)))

(define rust-cookie_0_16_0
  (package
    (name "rust-cookie")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01fa6z8sqqg19ya0l9ifh8vn05l5hpxdzkbh489mpymhw5np1m4l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aes-gcm" ,rust-aes-gcm_0_9_4)        
        ("rust-base64" ,rust-base64_0_13_0)        
        ("rust-hkdf" ,rust-hkdf_0_12_3)        
        ("rust-hmac" ,rust-hmac_0_12_1)        
        ("rust-percent-encoding" ,rust-percent-encoding_2_1_0)        
        ("rust-rand" ,rust-rand_0_8_5)        
        ("rust-sha2" ,rust-sha2_0_10_2)        
        ("rust-subtle" ,rust-subtle_2_4_1)        
        ("rust-time" ,rust-time_0_3_9)        
        ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars.")
    (description
      (beautify-description "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars."))
    (license license:expat)))

(define rust-cookie_store_0_15_1
  (package
    (name "rust-cookie_store")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z0navy9k0ivrdvz492q8c4nhd3iv5l77hwfppskdp1j15607xxk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cookie" ,rust-cookie_0_15_1)        
        ("rust-idna" ,rust-idna_0_2_3)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-publicsuffix" ,rust-publicsuffix_2_1_1)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-time" ,rust-time_0_2_27)        
        ("rust-url" ,rust-url_2_2_2))))
    (home-page "None")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description
      (beautify-description "Implementation of Cookie storage and retrieval"))
    (license (list license:expat
               license:asl2.0))))

(define rust-cookie_store_0_16_0
  (package
    (name "rust-cookie_store")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0idrphkllykjmvx1vnjyihi3w76lphwbj6k0vqzpiib4lqvgsfzq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cookie" ,rust-cookie_0_16_0)        
        ("rust-idna" ,rust-idna_0_2_3)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-publicsuffix" ,rust-publicsuffix_2_1_1)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-time" ,rust-time_0_3_9)        
        ("rust-url" ,rust-url_2_2_2))))
    (home-page "None")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description
      (beautify-description "Implementation of Cookie storage and retrieval"))
    (license (list license:expat
               license:asl2.0))))

(define rust-core-foundation_0_9_3
  (package
    (name "rust-core-foundation")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-core-foundation-sys" ,rust-core-foundation-sys_0_8_3)        
        ("rust-libc" ,rust-libc_0_2_125))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
      (beautify-description "Bindings to Core Foundation for macOS"))
    (license (list license:expat
               license:asl2.0))))

(define rust-core-foundation-sys_0_8_3
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p5r2wckarkpkyc4z83q08dwpvcafrb1h6fxfa3qnikh8szww9sq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
      (beautify-description "Bindings to Core Foundation for macOS"))
    (license (list license:expat
               license:asl2.0))))

(define rust-cpufeatures_0_2_2
  (package
    (name "rust-cpufeatures")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpufeatures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jsyrw41g5sqdpc9jgk57969hc0xw4c52j9amvmll4mbcwb019jr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "Lightweight runtime CPU feature detection for x86/x86_64 and aarch64 with\nno_std support and support for mobile targets including Android and iOS")
    (description
      (beautify-description "Lightweight runtime CPU feature detection for x86/x86_64 and aarch64 with\nno_std support and support for mobile targets including Android and iOS"))
    (license license:expat)))

(define rust-crc32fast_1_3_2
  (package
    (name "rust-crc32fast")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc32fast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03c8f29yx293yf43xar946xbls1g60c207m9drf8ilqhr25vsh5m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
    (description
      (beautify-description "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation"))
    (license license:expat)))

(define rust-cron_0_11_0
  (package
    (name "rust-cron")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cron" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kbkbii015i92dv29g7yvn57yabrhgq0aq376xd0s41y4klijqnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chrono" ,rust-chrono_0_4_19)        
        ("rust-nom" ,rust-nom_7_1_1)        
        ("rust-once_cell" ,rust-once_cell_1_10_0))))
    (home-page "None")
    (synopsis "A cron expression parser and schedule explorer.")
    (description
      (beautify-description "A cron expression parser and schedule explorer."))
    (license license:expat)))

(define rust-crossbeam-utils_0_8_8
  (package
    (name "rust-crossbeam-utils")
    (version "0.8.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f6b3xrbyc3yx0qa1digmy48mxmh58359kv34qy6ws5p433j9w8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
      (beautify-description "Utilities for concurrent programming"))
    (license license:expat)))

(define rust-crypto-common_0_1_3
  (package
    (name "rust-crypto-common")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-common" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s1wpm88qlrp079mzh3dlxm9vbqs4ch016yp9pzhcdjygfi2r5ap"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5)        
        ("rust-typenum" ,rust-typenum_1_15_0))))
    (home-page "None")
    (synopsis "Common cryptographic traits")
    (description
      (beautify-description "Common cryptographic traits"))
    (license license:expat)))

(define rust-crypto-mac_0_11_1
  (package
    (name "rust-crypto-mac")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-mac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05672ncc54h66vph42s0a42ljl69bwnqjh0x4xgj2v1395psildi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5)        
        ("rust-subtle" ,rust-subtle_2_4_1))))
    (home-page "None")
    (synopsis "Trait for Message Authentication Code (MAC) algorithms")
    (description
      (beautify-description "Trait for Message Authentication Code (MAC) algorithms"))
    (license license:expat)))

(define rust-ctr_0_8_0
  (package
    (name "rust-ctr")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ctr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sk1aykwhkny92cnvl6s75dx3fyvfzw5xkd6xz3y7w5anhgvk6q4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cipher" ,rust-cipher_0_3_0))))
    (home-page "None")
    (synopsis "CTR block modes of operation")
    (description
      (beautify-description "CTR block modes of operation"))
    (license license:expat)))

(define rust-ctrlc_3_2_2
  (package
    (name "rust-ctrlc")
    (version "3.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ctrlc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r88w8l4hxc64w43xlwjk5f60vg57vdahnjy3w5f0qb89slflzxk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix_0_24_1)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/Detegr/rust-ctrlc")
    (synopsis "Easy Ctrl-C handler for Rust projects")
    (description
      (beautify-description "Easy Ctrl-C handler for Rust projects"))
    (license (list license:expat
               license:asl2.0))))

(define rust-darling_0_13_4
  (package
    (name "rust-darling")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g25pad4mhq7315mw9n4wpg8j3mwyhwvr541kgdl0aar1j2ra7d0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling_core" ,rust-darling_core_0_13_4)        
        ("rust-darling_macro" ,rust-darling_macro_0_13_4))))
    (home-page "None")
    (synopsis "A proc-macro library for reading attributes into structs when\nimplementing custom derives.")
    (description
      (beautify-description "A proc-macro library for reading attributes into structs when\nimplementing custom derives."))
    (license license:expat)))

(define rust-darling_core_0_13_4
  (package
    (name "rust-darling_core")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "046n83f9jpszlngpjxkqi39ayzxf5a35q673c69jr1dn0ylnb7c5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fnv" ,rust-fnv_1_0_7)        
        ("rust-ident_case" ,rust-ident_case_1_0_1)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-strsim" ,rust-strsim_0_10_0)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Helper crate for proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
      (beautify-description "Helper crate for proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code."))
    (license license:expat)))

(define rust-darling_macro_0_13_4
  (package
    (name "rust-darling_macro")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0d8q8ibmsb1yzby6vwgh2wx892jqqfv9clwhpm19rprvz1wjd5ww"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling_core" ,rust-darling_core_0_13_4)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Internal support for a proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
      (beautify-description "Internal support for a proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code."))
    (license license:expat)))

(define rust-dashmap_5_3_3
  (package
    (name "rust-dashmap")
    (version "5.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dashmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wv5qa3ic6h4lvjah0whryqng5cl807bfks9m6vqbr82sgxmc6rr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-hashbrown" ,rust-hashbrown_0_12_1)        
        ("rust-lock_api" ,rust-lock_api_0_4_7))))
    (home-page "https://github.com/xacrimon/dashmap")
    (synopsis "Blazing fast concurrent HashMap for Rust.")
    (description
      (beautify-description "Blazing fast concurrent HashMap for Rust."))
    (license license:expat)))

(define rust-data-encoding_2_3_2
  (package
    (name "rust-data-encoding")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "data-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mvd8bjq5mq50fcf931cff57vwmbsvs1kpxynkzrshli98y3kqiy"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Efficient and customizable data-encoding functions like base64, base32, and hex")
    (description
      (beautify-description "Efficient and customizable data-encoding functions like base64, base32, and hex"))
    (license license:expat)))

(define rust-data-url_0_1_1
  (package
    (name "rust-data-url")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "data-url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14z15yiyklp5dv0k0q6pd83irrn0y8hj9y3fj17akkrbf37byc1s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-matches" ,rust-matches_0_1_9))))
    (home-page "None")
    (synopsis "Processing of data: URL according to WHATWG\u2019s Fetch Standard")
    (description
      (beautify-description "Processing of data: URL according to WHATWG\u2019s Fetch Standard"))
    (license license:expat)))

(define rust-devise_0_3_1
  (package
    (name "rust-devise")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15dmibnykic2a1ndi66shyvxmpfysnhf05lg2iv8871g0w5miish"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-devise_codegen" ,rust-devise_codegen_0_3_1)        
        ("rust-devise_core" ,rust-devise_core_0_3_1))))
    (home-page "None")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      (beautify-description "A library for devising derives and other procedural macros."))
    (license (list license:expat
               license:asl2.0))))

(define rust-devise_codegen_0_3_1
  (package
    (name "rust-devise_codegen")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cp7nnfwvjp6wfq11n0ffjjrwfa1wbsb58g1bz3ha6z5lvkp6g0j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-devise_core" ,rust-devise_core_0_3_1)        
        ("rust-quote" ,rust-quote_1_0_18))))
    (home-page "None")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      (beautify-description "A library for devising derives and other procedural macros."))
    (license (list license:expat
               license:asl2.0))))

(define rust-devise_core_0_3_1
  (package
    (name "rust-devise_core")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l00qiih4z14ai0c3s16nlvw0kv4p07ygi6a0ms0knc78xpz87l4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics_0_9_1)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      (beautify-description "A library for devising derives and other procedural macros."))
    (license (list license:expat
               license:asl2.0))))

(define rust-diesel_1_4_8
  (package
    (name "rust-diesel")
    (version "1.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kcfkfhsv5yv3ksj440ajgic930359i2bqi77ss4dm5pyvn3b0dj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-byteorder" ,rust-byteorder_1_4_3)        
        ("rust-chrono" ,rust-chrono_0_4_19)        
        ("rust-diesel_derives" ,rust-diesel_derives_1_4_1)        
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys_0_22_2)        
        ("rust-mysqlclient-sys" ,rust-mysqlclient-sys_0_2_5)        
        ("rust-pq-sys" ,rust-pq-sys_0_4_6)        
        ("rust-r2d2" ,rust-r2d2_0_8_9)        
        ("rust-url" ,rust-url_1_7_2))))
    (home-page "https://diesel.rs")
    (synopsis "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL")
    (description
      (beautify-description "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL"))
    (license license:expat)))

(define rust-diesel_derives_1_4_1
  (package
    (name "rust-diesel_derives")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_derives" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lsq133fwk0zj8xvxhdxqgg0xs31zf3abnwdyshaf0ldca7hkxa5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://diesel.rs")
    (synopsis "You should not use this crate directly, it is internal to Diesel.")
    (description
      (beautify-description "You should not use this crate directly, it is internal to Diesel."))
    (license license:expat)))

(define rust-diesel_migrations_1_4_0
  (package
    (name "rust-diesel_migrations")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_migrations" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k4g03ciqwya2xc1xvy5s9cs6q55k45wxa1gszswfg9m2f2dwg5z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-migrations_internals" ,rust-migrations_internals_1_4_1)        
        ("rust-migrations_macros" ,rust-migrations_macros_1_4_2))))
    (home-page "https://diesel.rs")
    (synopsis "Migration management for diesel")
    (description
      (beautify-description "Migration management for diesel"))
    (license license:expat)))

(define rust-digest_0_8_1
  (package
    (name "rust-digest")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "digest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1madjl27f3kj5ql7kwgvb9c8b7yb7bv7yfgx7rqzj4i3fp4cil7k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_12_4))))
    (home-page "None")
    (synopsis "Traits for cryptographic hash functions")
    (description
      (beautify-description "Traits for cryptographic hash functions"))
    (license license:expat)))

(define rust-digest_0_9_0
  (package
    (name "rust-digest")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "digest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rmhvk33rgvd6ll71z8sng91a52rw14p0drjn1da0mqa138n1pfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5))))
    (home-page "None")
    (synopsis "Traits for cryptographic hash functions")
    (description
      (beautify-description "Traits for cryptographic hash functions"))
    (license license:expat)))

(define rust-digest_0_10_3
  (package
    (name "rust-digest")
    (version "0.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "digest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01nmj9cci5qdm4q4wlmz104rzr68d5m823kdzd95bypslq68dyzj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer_0_10_2)        
        ("rust-crypto-common" ,rust-crypto-common_0_1_3)        
        ("rust-subtle" ,rust-subtle_2_4_1))))
    (home-page "None")
    (synopsis "Traits for cryptographic hash functions")
    (description
      (beautify-description "Traits for cryptographic hash functions"))
    (license license:expat)))

(define rust-dirs_4_0_0
  (package
    (name "rust-dirs")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n8020zl4f0frfnzvgb9agvk4a14i1kjz4daqnxkgslndwmaffna"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dirs-sys" ,rust-dirs-sys_0_3_7))))
    (home-page "None")
    (synopsis "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
      (beautify-description "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS."))
    (license license:expat)))

(define rust-dirs-sys_0_3_7
  (package
    (name "rust-dirs-sys")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19md1cnkazham8a6kh22v12d8hh3raqahfk6yb043vrjr68is78v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125)        
        ("rust-redox_users" ,rust-redox_users_0_4_3)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "System-level helper functions for the dirs and directories crates.")
    (description
      (beautify-description "System-level helper functions for the dirs and directories crates."))
    (license license:expat)))

(define rust-discard_1_0_4
  (package
    (name "rust-discard")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "discard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h67ni5bxvg95s91wgicily4ix7lcw7cq0a5gy9njrybaibhyb91"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Pauan/rust-discard")
    (synopsis "Discard trait which allows for intentionally leaking memory")
    (description
      (beautify-description "Discard trait which allows for intentionally leaking memory"))
    (license license:expat)))

(define rust-dotenvy_0_15_1
  (package
    (name "rust-dotenvy")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dotenvy" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0psjavasdqgqn5c5vk0ybiqa2x2yx643b4avsw0zsrh3qf1im1by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dirs" ,rust-dirs_4_0_0))))
    (home-page "https://github.com/allan2/dotenvy")
    (synopsis "A well-maintained fork of the `dotenv` crate")
    (description
      (beautify-description "A well-maintained fork of the `dotenv` crate"))
    (license license:expat)))

(define rust-either_1_6_1
  (package
    (name "rust-either")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mwl9vngqf5jvrhmhn9x60kr5hivxyjxbmby2pybncxfqhf4z3g7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      (beautify-description "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases."))
    (license (list license:expat
               license:asl2.0))))

(define rust-email-encoding_0_1_0
  (package
    (name "rust-email-encoding")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "email-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "164zca4pis56ql0byyrkx1kiaya1y91bl25cj1vlckl2cq8jk436"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_13_0))))
    (home-page "None")
    (synopsis "Low level email encoding RFCs implementations")
    (description
      (beautify-description "Low level email encoding RFCs implementations"))
    (license license:expat)))

(define rust-encoding_rs_0_8_31
  (package
    (name "rust-encoding_rs")
    (version "0.8.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0azc6rblf75vd862ymjahdfch27j1sshb7zynshrx7ywi5an6llq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "https://docs.rs/encoding_rs/")
    (synopsis "A Gecko-oriented implementation of the Encoding Standard")
    (description
      (beautify-description "A Gecko-oriented implementation of the Encoding Standard"))
    (license license:asl2.0)))

(define rust-enum-as-inner_0_3_4
  (package
    (name "rust-enum-as-inner")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "enum-as-inner" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m2l8zh0yd7i95qkyha86ca8m0bnhfimv38dr3n4p41yh6di03ap"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-heck" ,rust-heck_0_4_0)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "A proc-macro for deriving inner field accessor functions on enums.")
    (description
      (beautify-description "A proc-macro for deriving inner field accessor functions on enums."))
    (license (list license:expat
               license:asl2.0))))

(define rust-error-chain_0_12_4
  (package
    (name "rust-error-chain")
    (version "0.12.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-chain" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z6y5isg0il93jp287sv7pn10i4wrkik2cpyk376wl61rawhcbrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "Yet another error boilerplate library.")
    (description
      (beautify-description "Yet another error boilerplate library."))
    (license (list license:expat
               license:asl2.0))))

(define rust-fake-simd_0_1_2
  (package
    (name "rust-fake-simd")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fake-simd" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vfylvk4va2ivqx85603lyqqp0zk52cgbs4n5nfbbbqx577qm2p8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Crate for mimicking simd crate on stable Rust")
    (description
      (beautify-description "Crate for mimicking simd crate on stable Rust"))
    (license (list license:expat
               license:asl2.0))))

(define rust-fastrand_1_7_0
  (package
    (name "rust-fastrand")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fastrand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pvci54f2cm69ybc308z213xdybgqpvf2pcvq1kch69mwp7g1z63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-instant" ,rust-instant_0_1_12))))
    (home-page "None")
    (synopsis "A simple and fast random number generator")
    (description
      (beautify-description "A simple and fast random number generator"))
    (license license:asl2.0)))

(define rust-fern_0_6_1
  (package
    (name "rust-fern")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fern" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ahys5fmc10vcgf6yyai0jiypl8pqwidydhqkbp7jph79447pp9v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log_0_4_17)        
        ("rust-syslog" ,rust-syslog_6_0_1))))
    (home-page "None")
    (synopsis "Simple, efficient logging")
    (description
      (beautify-description "Simple, efficient logging"))
    (license license:expat)))

(define rust-figment_0_10_6
  (package
    (name "rust-figment")
    (version "0.10.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "figment" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pr2w6pldkkjavj1sacn9xiibzhlf13ply0gnnxan616qy9442vr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atomic" ,rust-atomic_0_5_1)        
        ("rust-pear" ,rust-pear_0_2_3)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-toml" ,rust-toml_0_5_9)        
        ("rust-uncased" ,rust-uncased_0_9_6)        
        ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "A configuration library so con-free, it\u0027s unreal.")
    (description
      (beautify-description "A configuration library so con-free, it\u0027s unreal."))
    (license license:expat)))

(define rust-flate2_1_0_23
  (package
    (name "rust-flate2")
    (version "1.0.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flate2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bq9vavadgqqr72z5bzbp952c4q67a71kfc4r55qzlw6cvlj55dk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-crc32fast" ,rust-crc32fast_1_3_2)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-miniz_oxide" ,rust-miniz_oxide_0_5_1))))
    (home-page "https://github.com/rust-lang/flate2-rs")
    (synopsis "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.\nSupports miniz_oxide, miniz.c, and multiple zlib implementations. Supports\nzlib, gzip, and raw deflate streams.")
    (description
      (beautify-description "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.\nSupports miniz_oxide, miniz.c, and multiple zlib implementations. Supports\nzlib, gzip, and raw deflate streams."))
    (license (list license:expat
               license:asl2.0))))

(define rust-fnv_1_0_7
  (package
    (name "rust-fnv")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fnv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fowler\u2013Noll\u2013Vo hash function")
    (description
      (beautify-description "Fowler\u2013Noll\u2013Vo hash function"))
    (license (list license:asl2.0
               license:expat))))

(define rust-foreign-types_0_3_2
  (package
    (name "rust-foreign-types")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-foreign-types-shared" ,rust-foreign-types-shared_0_1_1))))
    (home-page "None")
    (synopsis "A framework for Rust wrappers over C APIs")
    (description
      (beautify-description "A framework for Rust wrappers over C APIs"))
    (license (list license:expat
               license:asl2.0))))

(define rust-foreign-types-shared_0_1_1
  (package
    (name "rust-foreign-types-shared")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An internal crate used by foreign-types")
    (description
      (beautify-description "An internal crate used by foreign-types"))
    (license (list license:expat
               license:asl2.0))))

(define rust-form_urlencoded_1_0_1
  (package
    (name "rust-form_urlencoded")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "form_urlencoded" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1491fmakavcmsjbm3q6iy0bhmn9l422jasdhzx5hkljgza3mmhjz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-matches" ,rust-matches_0_1_9)        
        ("rust-percent-encoding" ,rust-percent-encoding_2_1_0))))
    (home-page "None")
    (synopsis "Parser and serializer for the application/x-www-form-urlencoded syntax, as used by HTML forms.")
    (description
      (beautify-description "Parser and serializer for the application/x-www-form-urlencoded syntax, as used by HTML forms."))
    (license (list license:expat
               license:asl2.0))))

(define rust-fuchsia-cprng_0_1_1
  (package
    (name "rust-fuchsia-cprng")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-cprng" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fnkqrbz7ixxzsb04bsz9p0zzazanma8znfdqjvh39n14vapfvx0"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Rust crate for the Fuchsia cryptographically secure pseudorandom number generator")
    (description
      (beautify-description "Rust crate for the Fuchsia cryptographically secure pseudorandom number generator"))
    (license #t)))

(define rust-fuchsia-zircon_0_3_3
  (package
    (name "rust-fuchsia-zircon")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10jxc5ks1x06gpd0xg51kcjrxr35nj6qhx2zlc5n7bmskv3675rf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys_0_3_3))))
    (home-page "None")
    (synopsis "Rust bindings for the Zircon kernel")
    (description
      (beautify-description "Rust bindings for the Zircon kernel"))
    (license license:bsd-3)))

(define rust-fuchsia-zircon-sys_0_3_3
  (package
    (name "rust-fuchsia-zircon-sys")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19zp2085qsyq2bh1gvcxq1lb8w6v6jj9kbdkhpdjrl95fypakjix"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Low-level Rust bindings for the Zircon kernel")
    (description
      (beautify-description "Low-level Rust bindings for the Zircon kernel"))
    (license license:bsd-3)))

(define rust-futures_0_3_21
  (package
    (name "rust-futures")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17id2zvn2acny759indn6yj2acfa6lhkwzaidxr2pqfiaigycgzp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-channel" ,rust-futures-channel_0_3_21)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-executor" ,rust-futures-executor_0_3_21)        
        ("rust-futures-io" ,rust-futures-io_0_3_21)        
        ("rust-futures-sink" ,rust-futures-sink_0_3_21)        
        ("rust-futures-task" ,rust-futures-task_0_3_21)        
        ("rust-futures-util" ,rust-futures-util_0_3_21))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "An implementation of futures and streams featuring zero allocations,\ncomposability, and iterator-like interfaces.")
    (description
      (beautify-description "An implementation of futures and streams featuring zero allocations,\ncomposability, and iterator-like interfaces."))
    (license license:expat)))

(define rust-futures-channel_0_3_21
  (package
    (name "rust-futures-channel")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-channel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0420lz2fmxa356ax1rp2sqi7b27ykfhvq4w9f1sla4hlp7j3q263"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-sink" ,rust-futures-sink_0_3_21))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.")
    (description
      (beautify-description "Channels for asynchronous communication using futures-rs."))
    (license license:expat)))

(define rust-futures-core_0_3_21
  (package
    (name "rust-futures-core")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lqhc6mqklh5bmkpr77p42lqwjj8gaskk5ba2p3kl1z4nw2gs28c"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.")
    (description
      (beautify-description "The core traits and types in for the `futures` library."))
    (license license:expat)))

(define rust-futures-executor_0_3_21
  (package
    (name "rust-futures-executor")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-executor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19mq96kwgf06axgdc2fbrjhqzdnxww9vw6cz8b82gqr9z86bj84l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-task" ,rust-futures-task_0_3_21)        
        ("rust-futures-util" ,rust-futures-util_0_3_21))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Executors for asynchronous tasks based on the futures-rs library.")
    (description
      (beautify-description "Executors for asynchronous tasks based on the futures-rs library."))
    (license license:expat)))

(define rust-futures-io_0_3_21
  (package
    (name "rust-futures-io")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-io" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0swn29fysas36ikk5aw55104fi98117amvgxw9g96pjs5ab4ah7w"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.")
    (description
      (beautify-description "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library."))
    (license license:expat)))

(define rust-futures-macro_0_3_21
  (package
    (name "rust-futures-macro")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04pmj5xfk5rdhlj69wc7w3zvdg3xardg8srig96lszrk00wf3h9k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.")
    (description
      (beautify-description "The futures-rs procedural macro implementations."))
    (license license:expat)))

(define rust-futures-sink_0_3_21
  (package
    (name "rust-futures-sink")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-sink" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s58gx5yw1a21xviw2qgc0wzk225vgn4kbzddrp141m3kw9kw5i1"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.")
    (description
      (beautify-description "The asynchronous `Sink` trait for the futures-rs library."))
    (license license:expat)))

(define rust-futures-task_0_3_21
  (package
    (name "rust-futures-task")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-task" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0skpiz2ljisywajv79p70yapfwhkqhb39wxy3f09v47mdfbnmijp"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.")
    (description
      (beautify-description "Tools for working with tasks."))
    (license license:expat)))

(define rust-futures-timer_3_0_2
  (package
    (name "rust-futures-timer")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-timer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b5v7lk9838ix6jdcrainsyrh7xrf24pwm61dp13907qkn806jz6"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/async-rs/futures-timer")
    (synopsis "Timeouts for futures.")
    (description
      (beautify-description "Timeouts for futures."))
    (license (list license:expat
               license:asl2.0))))

(define rust-futures-util_0_3_21
  (package
    (name "rust-futures-util")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sh3wqi8p36csjffy0irq8nlx9shqxp7z4dsih6bknarsvaspdyq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-channel" ,rust-futures-channel_0_3_21)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-io" ,rust-futures-io_0_3_21)        
        ("rust-futures-macro" ,rust-futures-macro_0_3_21)        
        ("rust-futures-sink" ,rust-futures-sink_0_3_21)        
        ("rust-futures-task" ,rust-futures-task_0_3_21)        
        ("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-pin-utils" ,rust-pin-utils_0_1_0)        
        ("rust-slab" ,rust-slab_0_4_6))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Common utilities and extension traits for the futures-rs library.")
    (description
      (beautify-description "Common utilities and extension traits for the futures-rs library."))
    (license license:expat)))

(define rust-generator_0_7_0
  (package
    (name "rust-generator")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vhj3f0rf4mlh5vz7pz5rxmgry1cc62x21mf9ld1r292m2f2gnf1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_73)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-rustversion" ,rust-rustversion_1_0_6)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/Xudong-Huang/generator-rs.git")
    (synopsis "Stackfull Generator Library in Rust")
    (description
      (beautify-description "Stackfull Generator Library in Rust"))
    (license (list license:expat
               license:asl2.0))))

(define rust-generic-array_0_12_4
  (package
    (name "rust-generic-array")
    (version "0.12.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generic-array" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gfpay78vijl9vrwl1k9v7fbvbhkhcmnrk4kfg9l6x24y4s9zpzz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-typenum" ,rust-typenum_1_15_0))))
    (home-page "None")
    (synopsis "Generic types implementing functionality of arrays")
    (description
      (beautify-description "Generic types implementing functionality of arrays"))
    (license license:expat)))

(define rust-generic-array_0_14_5
  (package
    (name "rust-generic-array")
    (version "0.14.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generic-array" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00qqhls43bzvyb7s26iw6knvsz3mckbxl3rhaahvypzhqwzd6j7x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-typenum" ,rust-typenum_1_15_0)        
        ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "Generic types implementing functionality of arrays")
    (description
      (beautify-description "Generic types implementing functionality of arrays"))
    (license license:expat)))

(define rust-getrandom_0_1_16
  (package
    (name "rust-getrandom")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getrandom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kjzmz60qx9mn615ks1akjbf36n3lkv27zfwbcam0fzmj56wphwg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-wasi" ,rust-wasi_0_9_0+wasi-snapshot-preview1))))
    (home-page "None")
    (synopsis "A small cross-platform library for retrieving random data from system source")
    (description
      (beautify-description "A small cross-platform library for retrieving random data from system source"))
    (license license:expat)))

(define rust-getrandom_0_2_6
  (package
    (name "rust-getrandom")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getrandom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b8588g2z36s1082licl623lclbdz9jp03gnz39bi0qwjnc0rrwv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-wasi" ,rust-wasi_0_10_2+wasi-snapshot-preview1))))
    (home-page "None")
    (synopsis "A small cross-platform library for retrieving random data from system source")
    (description
      (beautify-description "A small cross-platform library for retrieving random data from system source"))
    (license license:expat)))

(define rust-ghash_0_4_4
  (package
    (name "rust-ghash")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ghash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "169wvrc2k9lw776x3pmqp76kc0w5717wz01bfg9rz0ypaqbcr0qm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-opaque-debug" ,rust-opaque-debug_0_3_0)        
        ("rust-polyval" ,rust-polyval_0_5_3))))
    (home-page "None")
    (synopsis "Universal hash over GF(2^128) useful for constructing a Message Authentication Code (MAC),\nas in the AES-GCM authenticated encryption cipher.")
    (description
      (beautify-description "Universal hash over GF(2^128) useful for constructing a Message Authentication Code (MAC),\nas in the AES-GCM authenticated encryption cipher."))
    (license license:asl2.0)))

(define rust-gimli_0_26_1
  (package
    (name "rust-gimli")
    (version "0.26.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gimli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m0vi36ypv4gx9gzcw6y456yqnlypizhwlcqrmg6vkwd0lnkgk3q"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library for reading and writing the DWARF debugging format.")
    (description
      (beautify-description "A library for reading and writing the DWARF debugging format."))
    (license (list license:asl2.0
               license:expat))))

(define rust-glob_0_3_0
  (package
    (name "rust-glob")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x25wfr7vg3mzxc9x05dcphvd3nwlcmbnxrvwcvrrdwplcrrk4cv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/glob")
    (synopsis "Support for matching file paths against Unix shell style patterns.")
    (description
      (beautify-description "Support for matching file paths against Unix shell style patterns."))
    (license (list license:expat
               license:asl2.0))))

(define rust-governor_0_4_2
  (package
    (name "rust-governor")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "governor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11rcam9zfnkflbg75vyr6pd86gzks8xbqmck4dir2810xsamjxqr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dashmap" ,rust-dashmap_5_3_3)        
        ("rust-futures" ,rust-futures_0_3_21)        
        ("rust-futures-timer" ,rust-futures-timer_3_0_2)        
        ("rust-no-std-compat" ,rust-no-std-compat_0_4_1)        
        ("rust-nonzero_ext" ,rust-nonzero_ext_0_3_0)        
        ("rust-parking_lot" ,rust-parking_lot_0_12_0)        
        ("rust-quanta" ,rust-quanta_0_9_3)        
        ("rust-rand" ,rust-rand_0_8_5)        
        ("rust-smallvec" ,rust-smallvec_1_8_0))))
    (home-page "https://github.com/antifuchs/governor")
    (synopsis "A rate-limiting implementation in Rust")
    (description
      (beautify-description "A rate-limiting implementation in Rust"))
    (license license:expat)))

(define rust-h2_0_3_13
  (package
    (name "rust-h2")
    (version "0.3.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "h2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0msasdyv0n7avs5i1csjrs0rvdsp4k5z3fwl8rd53jbzcdnjra1p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-fnv" ,rust-fnv_1_0_7)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-sink" ,rust-futures-sink_0_3_21)        
        ("rust-futures-util" ,rust-futures-util_0_3_21)        
        ("rust-http" ,rust-http_0_2_7)        
        ("rust-indexmap" ,rust-indexmap_1_8_1)        
        ("rust-slab" ,rust-slab_0_4_6)        
        ("rust-tokio-util" ,rust-tokio-util_0_7_1)        
        ("rust-tracing" ,rust-tracing_0_1_34))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "None")
    (synopsis "An HTTP/2 client and server")
    (description
      (beautify-description "An HTTP/2 client and server"))
    (license license:expat)))

(define rust-half_1_8_2
  (package
    (name "rust-half")
    (version "1.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "half" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mqbmx2m9qd4lslkb42fzgldsklhv9c4bxsc8j82r80d8m24mfza"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Half-precision floating point f16 and bf16 types for Rust implementing the IEEE 754-2008 standard binary16 and bfloat16 types.")
    (description
      (beautify-description "Half-precision floating point f16 and bf16 types for Rust implementing the IEEE 754-2008 standard binary16 and bfloat16 types."))
    (license license:expat)))

(define rust-handlebars_4_2_2
  (package
    (name "rust-handlebars")
    (version "4.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "handlebars" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sy5c95idi2db7bvmsx1sql33lisfsy9a0qm8l17357h401s7mlr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log_0_4_17)        
        ("rust-pest" ,rust-pest_2_1_3)        
        ("rust-pest_derive" ,rust-pest_derive_2_1_0)        
        ("rust-quick-error" ,rust-quick-error_2_0_1)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-walkdir" ,rust-walkdir_2_3_2))))
    (home-page "https://github.com/sunng87/handlebars-rust")
    (synopsis "Handlebars templating implemented in Rust.")
    (description
      (beautify-description "Handlebars templating implemented in Rust."))
    (license license:expat)))

(define rust-hashbrown_0_11_2
  (package
    (name "rust-hashbrown")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vkjsf5nzs7qcia5ya79j9sq2p1caz4crrncr1675wwyj3ag0pmb"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A Rust port of Google\u0027s SwissTable hash map")
    (description
      (beautify-description "A Rust port of Google\u0027s SwissTable hash map"))
    (license (list license:asl2.0
               license:expat))))

(define rust-hashbrown_0_12_1
  (package
    (name "rust-hashbrown")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hycsz4nbnxcma1lngl66q7hlrwn1scdqdj4jqghiw5bk3w4q3fv"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A Rust port of Google\u0027s SwissTable hash map")
    (description
      (beautify-description "A Rust port of Google\u0027s SwissTable hash map"))
    (license license:expat)))

(define rust-heck_0_4_0
  (package
    (name "rust-heck")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ygphsnfwl2xpa211vbqkz1db6ri1kvkg8p8sqybi37wclg7fh15"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      (beautify-description "heck is a case conversion library."))
    (license license:expat)))

(define rust-hermit-abi_0_1_19
  (package
    (name "rust-hermit-abi")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hermit-abi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "hermit-abi is small interface to call functions from the unikernel RustyHermit.\nIt is used to build the target `x86_64-unknown-hermit`.")
    (description
      (beautify-description "hermit-abi is small interface to call functions from the unikernel RustyHermit.\nIt is used to build the target `x86_64-unknown-hermit`."))
    (license (list license:expat
               license:asl2.0))))

(define rust-hkdf_0_12_3
  (package
    (name "rust-hkdf")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hkdf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dyl16cf15hka32hv3l7dwgr3xj3brpfr27iyrbpdhlzdfgh46kr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hmac" ,rust-hmac_0_12_1))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
      (beautify-description "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)"))
    (license license:expat)))

(define rust-hmac_0_11_0
  (package
    (name "rust-hmac")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16z61aibdg4di40sqi4ks2s4rz6r29w4sx4gvblfph3yxch26aia"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac_0_11_1)        
        ("rust-digest" ,rust-digest_0_9_0))))
    (home-page "None")
    (synopsis "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
      (beautify-description "Generic implementation of Hash-based Message Authentication Code (HMAC)"))
    (license license:expat)))

(define rust-hmac_0_12_1
  (package
    (name "rust-hmac")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-digest" ,rust-digest_0_10_3))))
    (home-page "None")
    (synopsis "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
      (beautify-description "Generic implementation of Hash-based Message Authentication Code (HMAC)"))
    (license license:expat)))

(define rust-hostname_0_3_1
  (package
    (name "rust-hostname")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hostname" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rz8yf70cvzl3nry71m4bz9w6x4j9kdz3qng6pnwhk2h20z1qwrw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125)        
        ("rust-match_cfg" ,rust-match_cfg_0_1_0)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "Cross-platform system\u0027s host name functions")
    (description
      (beautify-description "Cross-platform system\u0027s host name functions"))
    (license license:expat)))

(define rust-html5gum_0_4_0
  (package
    (name "rust-html5gum")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "html5gum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c7bmyqcinvr0qrhi6i62ls2zhx0ppby36l1sanj4lxmdnv4ib9d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-jetscii" ,rust-jetscii_0_5_2))))
    (home-page "None")
    (synopsis "A WHATWG-compliant HTML5 tokenizer and tag soup parser.")
    (description
      (beautify-description "A WHATWG-compliant HTML5 tokenizer and tag soup parser."))
    (license license:expat)))

(define rust-http_0_2_7
  (package
    (name "rust-http")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fxzyvspr6g8znc6i0kif0bhpih8ibhy7xc6k984j8pm19bp11pz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-fnv" ,rust-fnv_1_0_7)        
        ("rust-itoa" ,rust-itoa_1_0_1))))
    (home-page "None")
    (synopsis "A set of types for representing HTTP requests and responses.")
    (description
      (beautify-description "A set of types for representing HTTP requests and responses."))
    (license license:expat)))

(define rust-http-body_0_4_4
  (package
    (name "rust-http-body")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http-body" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1imjszmk34603m7chfnhd3rq263bxbdlaxhlbzd06wv7354zix0z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-http" ,rust-http_0_2_7)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9))))
    (home-page "None")
    (synopsis "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (description
      (beautify-description "Trait representing an asynchronous, streaming, HTTP request or response body."))
    (license license:expat)))

(define rust-httparse_1_7_1
  (package
    (name "rust-httparse")
    (version "1.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "httparse" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k60q1hx96cvmjn6k3yjkff87fz0ga2a4z0g9ss8a9x5nndy4v29"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A tiny, safe, speedy, zero-copy HTTP/1.x parser.")
    (description
      (beautify-description "A tiny, safe, speedy, zero-copy HTTP/1.x parser."))
    (license (list license:expat
               license:asl2.0))))

(define rust-httpdate_1_0_2
  (package
    (name "rust-httpdate")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "httpdate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08bln7b1ibdw26gl8h4dr6rlybvlkyhlha309xbh9ghxh9nf78f4"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "HTTP date parsing and formatting")
    (description
      (beautify-description "HTTP date parsing and formatting"))
    (license (list license:expat
               license:asl2.0))))

(define rust-hyper_0_14_18
  (package
    (name "rust-hyper")
    (version "0.14.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1whf3qqvxcpbp1dfpzy6lhl4yjl1wfcby2nrc4417gpy1alf0smj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-futures-channel" ,rust-futures-channel_0_3_21)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-h2" ,rust-h2_0_3_13)        
        ("rust-http" ,rust-http_0_2_7)        
        ("rust-http-body" ,rust-http-body_0_4_4)        
        ("rust-httparse" ,rust-httparse_1_7_1)        
        ("rust-httpdate" ,rust-httpdate_1_0_2)        
        ("rust-itoa" ,rust-itoa_1_0_1)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-socket2" ,rust-socket2_0_4_4)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-tower-service" ,rust-tower-service_0_3_1)        
        ("rust-tracing" ,rust-tracing_0_1_34)        
        ("rust-want" ,rust-want_0_3_0))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util_0_3_21))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description
      (beautify-description "A fast and correct HTTP library."))
    (license license:expat)))

(define rust-hyper-tls_0_5_0
  (package
    (name "rust-hyper-tls")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01crgy13102iagakf6q4mb75dprzr7ps1gj0l5hxm1cvm7gks66n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-native-tls" ,rust-native-tls_0_2_10)        
        ("rust-tokio-native-tls" ,rust-tokio-native-tls_0_3_0))
       #:cargo-development-inputs
       (("rust-hyper" ,rust-hyper_0_14_18)        
        ("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "https://hyper.rs")
    (synopsis "Default TLS implementation for use with hyper")
    (description
      (beautify-description "Default TLS implementation for use with hyper"))
    (license (list license:expat
               license:asl2.0))))

(define rust-ident_case_1_0_1
  (package
    (name "rust-ident_case")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ident_case" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Utility for applying case rules to Rust identifiers.")
    (description
      (beautify-description "Utility for applying case rules to Rust identifiers."))
    (license (list license:expat
               license:asl2.0))))

(define rust-idna_0_1_5
  (package
    (name "rust-idna")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "idna" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kl4gs5kaydn4v07c6ka33spm9qdh2np0x7iw7g5zd8z1c7rxw1q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-matches" ,rust-matches_0_1_9)        
        ("rust-unicode-bidi" ,rust-unicode-bidi_0_3_8)        
        ("rust-unicode-normalization" ,rust-unicode-normalization_0_1_19))))
    (home-page "None")
    (synopsis "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (description
      (beautify-description "IDNA (Internationalizing Domain Names in Applications) and Punycode."))
    (license (list license:expat
               license:asl2.0))))

(define rust-idna_0_2_3
  (package
    (name "rust-idna")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "idna" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y7ca2w5qp9msgl57n03zqp78gq1bk2crqzg6kv7a542mdphm2j1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-matches" ,rust-matches_0_1_9)        
        ("rust-unicode-bidi" ,rust-unicode-bidi_0_3_8)        
        ("rust-unicode-normalization" ,rust-unicode-normalization_0_1_19))))
    (home-page "None")
    (synopsis "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (description
      (beautify-description "IDNA (Internationalizing Domain Names in Applications) and Punycode."))
    (license (list license:expat
               license:asl2.0))))

(define rust-indexmap_1_8_1
  (package
    (name "rust-indexmap")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vpnb7yq9ckdil8vnzh0p2w62j7gpfvyvlwvq9nqn7xavwr70r0g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_1_0)        
        ("rust-hashbrown" ,rust-hashbrown_0_11_2)        
        ("rust-serde" ,rust-serde_1_0_137))))
    (home-page "None")
    (synopsis "A hash table with consistent order and fast iteration.\n\nThe indexmap is a hash table where the iteration order of the key-value\npairs is independent of the hash values of the keys. It has the usual\nhash table functionality, it preserves insertion order except after\nremovals, and it allows lookup of its elements by either hash table key\nor numerical index. A corresponding hash set type is also provided.\n\nThis crate was initially published under the name ordermap, but it was renamed to\nindexmap.")
    (description
      (beautify-description "A hash table with consistent order and fast iteration.\n\nThe indexmap is a hash table where the iteration order of the key-value\npairs is independent of the hash values of the keys. It has the usual\nhash table functionality, it preserves insertion order except after\nremovals, and it allows lookup of its elements by either hash table key\nor numerical index. A corresponding hash set type is also provided.\n\nThis crate was initially published under the name ordermap, but it was renamed to\nindexmap."))
    (license (list license:asl2.0
               license:expat))))

(define rust-inlinable_string_0_1_15
  (package
    (name "rust-inlinable_string")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inlinable_string" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ysjci8yfvxgf51z0ny2nnwhxrclhmb3vbngin8v4bznhr3ybyn8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The `inlinable_string` crate provides the `InlinableString` type -- an owned, grow-able UTF-8 string that stores small strings inline and avoids heap-allocation -- and the `StringExt` trait which abstracts string operations over both `std::string::String` and `InlinableString` (or even your own custom string type).")
    (description
      (beautify-description "The `inlinable_string` crate provides the `InlinableString` type -- an owned, grow-able UTF-8 string that stores small strings inline and avoids heap-allocation -- and the `StringExt` trait which abstracts string operations over both `std::string::String` and `InlinableString` (or even your own custom string type)."))
    (license (list license:asl2.0
               license:expat))))

(define rust-instant_0_1_12
  (package
    (name "rust-instant")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "instant" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b2bx5qdlwayriidhrag8vhy10kdfimfhmb3jnjmsz2h9j1bwnvs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "A partial replacement for std::time::Instant that works on WASM too.")
    (description
      (beautify-description "A partial replacement for std::time::Instant that works on WASM too."))
    (license license:bsd-3)))

(define rust-iovec_0_1_4
  (package
    (name "rust-iovec")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iovec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ph73qygwx8i0mblrf110cj59l00gkmsgrpzz1rm85syz5pymcxj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125))))
    (home-page "https://github.com/carllerche/iovec")
    (synopsis "Portable buffer type for scatter/gather I/O operations")
    (description
      (beautify-description "Portable buffer type for scatter/gather I/O operations"))
    (license (list license:expat
               license:asl2.0))))

(define rust-ipconfig_0_2_2
  (package
    (name "rust-ipconfig")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ipconfig" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mzsagc6bk3i3fpggqlq8am5rxn4hgs297rsaya90w79xj5g3qpp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-socket2" ,rust-socket2_0_3_19)        
        ("rust-widestring" ,rust-widestring_0_4_3)        
        ("rust-winapi" ,rust-winapi_0_3_9)        
        ("rust-winreg" ,rust-winreg_0_6_2))))
    (home-page "https://github.com/liranringel/ipconfig")
    (synopsis "Get network adapters information and network configuration for windows.")
    (description
      (beautify-description "Get network adapters information and network configuration for windows."))
    (license (list license:expat
               license:asl2.0))))

(define rust-ipnet_2_5_0
  (package
    (name "rust-ipnet")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ipnet" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0asr5bwhbfxgxwappmvs0rvb0ncc5adnhfi9yiz4axlc9j1m97c7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Provides types and useful methods for working with IPv4 and IPv6 network addresses, commonly called IP prefixes. The new `IpNet`, `Ipv4Net`, and `Ipv6Net` types build on the existing `IpAddr`, `Ipv4Addr`, and `Ipv6Addr` types already provided in Rust\u0027s standard library and align to their design to stay consistent. The module also provides useful traits that extend `Ipv4Addr` and `Ipv6Addr` with methods for `Add`, `Sub`, `BitAnd`, and `BitOr` operations. The module only uses stable feature so it is guaranteed to compile using the stable toolchain.")
    (description
      (beautify-description "Provides types and useful methods for working with IPv4 and IPv6 network addresses, commonly called IP prefixes. The new `IpNet`, `Ipv4Net`, and `Ipv6Net` types build on the existing `IpAddr`, `Ipv4Addr`, and `Ipv6Addr` types already provided in Rust\u0027s standard library and align to their design to stay consistent. The module also provides useful traits that extend `Ipv4Addr` and `Ipv6Addr` with methods for `Add`, `Sub`, `BitAnd`, and `BitOr` operations. The module only uses stable feature so it is guaranteed to compile using the stable toolchain."))
    (license license:expat)))

(define rust-itoa_1_0_1
  (package
    (name "rust-itoa")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0d8wr2qf5b25a04xf10rz9r0pdbjdgb0zaw3xvf8k2sqcz1qzaqs"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast functions for printing integer primitives to an io::Write")
    (description
      (beautify-description "Fast functions for printing integer primitives to an io::Write"))
    (license license:expat)))

(define rust-jetscii_0_5_2
  (package
    (name "rust-jetscii")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jetscii" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wz3358p1vimnhk1s43xlry0fci0whw7m5zj36j7h6lcdsvraa29"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A tiny library to efficiently search strings and byte slices for sets of ASCII characters or bytes.")
    (description
      (beautify-description "A tiny library to efficiently search strings and byte slices for sets of ASCII characters or bytes."))
    (license license:expat)))

(define rust-job_scheduler_1_2_1-tarball
  (package
    (name "rust-job_scheduler")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/BlackDex/job_scheduler/tarball/9100fc596a083fd9c0b560f8f11f108e0a19d07e")
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m2hcnc472kqxahnp73xpvbmwdw0978gvymdg2izfw44ial02z6a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chrono" ,rust-chrono_0_4_19)        
        ("rust-cron" ,rust-cron_0_11_0)        
        ("rust-uuid" ,rust-uuid_1_0_0))))
    (home-page "None")
    (synopsis "A simple cron-like job scheduling library for Rust.")
    (description
      (beautify-description "A simple cron-like job scheduling library for Rust."))
    (license (list license:expat
               license:asl2.0))))

(define rust-job_scheduler_1_2_1
  (package
    (name "rust-job_scheduler")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/BlackDex/job_scheduler")
               (commit "9100fc596a083fd9c0b560f8f11f108e0a19d07e")))
        (file-name (git-file-name name version))
        (sha256
          (base32 
            "0jvi0h511i4lxy6a1q6hzx8aidd539lyhpasyck62fn0m9xkfx5z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chrono" ,rust-chrono_0_4_19)        
        ("rust-cron" ,rust-cron_0_11_0)        
        ("rust-uuid" ,rust-uuid_1_0_0))))
    (home-page "None")
    (synopsis "A simple cron-like job scheduling library for Rust.")
    (description
      (beautify-description "A simple cron-like job scheduling library for Rust."))
    (license (list license:expat
               license:asl2.0))))

(define rust-js-sys_0_3_57
  (package
    (name "rust-js-sys")
    (version "0.3.57")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "js-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15rksw2lq0lccb1cfc4zijwibzds0gfz2hq7fnic45yv43w2c6k7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_80))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bindings for all JS global objects and functions in all JS environments like\nNode.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (description
      (beautify-description "Bindings for all JS global objects and functions in all JS environments like\nNode.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate."))
    (license (list license:expat
               license:asl2.0))))

(define rust-jsonwebtoken_8_1_0
  (package
    (name "rust-jsonwebtoken")
    (version "8.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jsonwebtoken" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vyb7iqd8zs79d2k4vdrpxqxxqbqlarl385g82afgfl1gz0m346c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_13_0)        
        ("rust-pem" ,rust-pem_1_0_2)        
        ("rust-ring" ,rust-ring_0_16_20)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-simple_asn1" ,rust-simple_asn1_0_6_1))))
    (home-page "https://github.com/Keats/jsonwebtoken")
    (synopsis "Create and decode JWTs in a strongly typed way.")
    (description
      (beautify-description "Create and decode JWTs in a strongly typed way."))
    (license license:expat)))

(define rust-kernel32-sys_0_2_2
  (package
    (name "rust-kernel32-sys")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "kernel32-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1389av0601a9yz8dvx5zha9vmkd6ik7ax0idpb032d28555n41vm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_2_8)        
        ("rust-winapi-build" ,rust-winapi-build_0_1_1))))
    (home-page "None")
    (synopsis "Contains function definitions for the Windows API library kernel32. See winapi for types and constants.")
    (description
      (beautify-description "Contains function definitions for the Windows API library kernel32. See winapi for types and constants."))
    (license license:expat)))

(define rust-lazy_static_1_4_0
  (package
    (name "rust-lazy_static")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy_static" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A macro for declaring lazily evaluated statics in Rust.")
    (description
      (beautify-description "A macro for declaring lazily evaluated statics in Rust."))
    (license (list license:expat
               license:asl2.0))))

(define rust-lazycell_1_3_0
  (package
    (name "rust-lazycell")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazycell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library providing a lazily filled Cell struct")
    (description
      (beautify-description "A library providing a lazily filled Cell struct"))
    (license (list license:expat
               license:asl2.0))))

(define rust-lettre_0_10_0-rc_6
  (package
    (name "rust-lettre")
    (version "0.10.0-rc.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lettre" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12f2bglj3ni3di4rl4v0wix9m3x3qw3nd847cqxwkrky3w070v1g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_13_0)        
        ("rust-email-encoding" ,rust-email-encoding_0_1_0)        
        ("rust-fastrand" ,rust-fastrand_1_7_0)        
        ("rust-hostname" ,rust-hostname_0_3_1)        
        ("rust-httpdate" ,rust-httpdate_1_0_2)        
        ("rust-idna" ,rust-idna_0_2_3)        
        ("rust-mime" ,rust-mime_0_3_16)        
        ("rust-native-tls" ,rust-native-tls_0_2_10)        
        ("rust-nom" ,rust-nom_7_1_1)        
        ("rust-once_cell" ,rust-once_cell_1_10_0)        
        ("rust-quoted_printable" ,rust-quoted_printable_0_4_5)        
        ("rust-regex" ,rust-regex_1_5_5)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-tracing" ,rust-tracing_0_1_34))))
    (home-page "https://lettre.rs")
    (synopsis "Email client")
    (description
      (beautify-description "Email client"))
    (license license:expat)))

(define rust-libc_0_2_125
  (package
    (name "rust-libc")
    (version "0.2.125")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0axwhkyv58vx7i1sri4cqlnj77f0cn6pmbcip2zyjvcgd6pd45jr"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
      (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license license:expat)))

(define rust-libmimalloc-sys_0_1_25
  (package
    (name "rust-libmimalloc-sys")
    (version "0.1.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libmimalloc-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b1fi84giac7lsg0v2p39xbnn735prnqr2gpbpslh12ma9h17jhi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_73))))
    (home-page "None")
    (synopsis "Sys crate wrapping the mimalloc allocator")
    (description
      (beautify-description "Sys crate wrapping the mimalloc allocator"))
    (license license:expat)))

(define rust-libsqlite3-sys_0_22_2
  (package
    (name "rust-libsqlite3-sys")
    (version "0.22.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libsqlite3-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17gqc2mwih81j3ds479gl5zmsxqzzrcrj3yyv62vh34bgy8n82r9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_73)        
        ("rust-pkg-config" ,rust-pkg-config_0_3_25)        
        ("rust-vcpkg" ,rust-vcpkg_0_2_15))))
    (home-page "None")
    (synopsis "Native bindings to the libsqlite3 library")
    (description
      (beautify-description "Native bindings to the libsqlite3 library"))
    (license license:expat)))

(define rust-linked-hash-map_0_5_4
  (package
    (name "rust-linked-hash-map")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "linked-hash-map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ww8zsraqnvrsknd315481185igwkx5n14xnhq5i8216z65b7fbz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/contain-rs/linked-hash-map")
    (synopsis "A HashMap wrapper that holds key-value pairs in insertion order")
    (description
      (beautify-description "A HashMap wrapper that holds key-value pairs in insertion order"))
    (license (list license:expat
               license:asl2.0))))

(define rust-lock_api_0_4_7
  (package
    (name "rust-lock_api")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lwckl9l51y69bwf854kmdmmr1543spbxaa9xjclc3lllsvaazrj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_1_0)        
        ("rust-scopeguard" ,rust-scopeguard_1_1_0))))
    (home-page "None")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      (beautify-description "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std."))
    (license license:expat)))

(define rust-log_0_4_17
  (package
    (name "rust-log")
    (version "0.4.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0biqlaaw1lsr8bpnmbcc0fvgjj34yy79ghqzyi0ali7vgil2xcdb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "A lightweight logging facade for Rust")
    (description
      (beautify-description "A lightweight logging facade for Rust"))
    (license license:expat)))

(define rust-loom_0_5_4
  (package
    (name "rust-loom")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "loom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02a30cv9l2afjq5bg42hgcjspx8fgwyij0cf9saw8b73539wgigd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-generator" ,rust-generator_0_7_0)        
        ("rust-scoped-tls" ,rust-scoped-tls_1_0_0)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-tracing" ,rust-tracing_0_1_34)        
        ("rust-tracing-subscriber" ,rust-tracing-subscriber_0_3_11))))
    (home-page "https://github.com/tokio-rs/loom")
    (synopsis "Permutation testing for concurrent code")
    (description
      (beautify-description "Permutation testing for concurrent code"))
    (license license:expat)))

(define rust-lru-cache_0_1_2
  (package
    (name "rust-lru-cache")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lru-cache" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "071viv6g2p3akwqmfb3c8vsycs5n7kr17b70l7la071jv0d4zqii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-linked-hash-map" ,rust-linked-hash-map_0_5_4))))
    (home-page "https://github.com/contain-rs/lru-cache")
    (synopsis "A cache that holds a limited number of key-value pairs")
    (description
      (beautify-description "A cache that holds a limited number of key-value pairs"))
    (license (list license:expat
               license:asl2.0))))

(define rust-mach_0_3_2
  (package
    (name "rust-mach")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mach" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yksa8lwzqh150gr4417rls1wk20asy9vhp8kq5g9n7z58xyh8xq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "A Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX.")
    (description
      (beautify-description "A Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX."))
    (license license:bsd-2)))

(define rust-maplit_1_0_2
  (package
    (name "rust-maplit")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "maplit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07b5kjnhrrmfhgqm9wprjw8adx6i225lqp49gasgqg74lahnabiy"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Collection \u201cliteral\u201d macros for HashMap, HashSet, BTreeMap, and BTreeSet.")
    (description
      (beautify-description "Collection \u201cliteral\u201d macros for HashMap, HashSet, BTreeMap, and BTreeSet."))
    (license (list license:expat
               license:asl2.0))))

(define rust-match_cfg_0_1_0
  (package
    (name "rust-match_cfg")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "match_cfg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r5j3zqc3qr8ybcx95bk8q57mkizmgmffj5lmicd4i8d9riyigpz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/gnzlbg/match_cfg")
    (synopsis "A convenience macro to ergonomically define an item depending on a large number\nof `#[cfg]` parameters. Structured like match statement, the first matching\nbranch is the item that gets emitted.")
    (description
      (beautify-description "A convenience macro to ergonomically define an item depending on a large number\nof `#[cfg]` parameters. Structured like match statement, the first matching\nbranch is the item that gets emitted."))
    (license (list license:expat
               license:asl2.0))))

(define rust-matchers_0_1_0
  (package
    (name "rust-matchers")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matchers" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-regex-automata" ,rust-regex-automata_0_1_10))))
    (home-page "https://github.com/hawkw/matchers")
    (synopsis "Regex matching on character and byte streams.")
    (description
      (beautify-description "Regex matching on character and byte streams."))
    (license license:expat)))

(define rust-matches_0_1_9
  (package
    (name "rust-matches")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matches" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gw5ib38jfgyyah8nyyxr036grqv1arkf1srgfa4h386dav7iqx3"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A macro to evaluate, as a boolean, whether an expression matches a pattern.")
    (description
      (beautify-description "A macro to evaluate, as a boolean, whether an expression matches a pattern."))
    (license license:expat)))

(define rust-maybe-uninit_2_0_0
  (package
    (name "rust-maybe-uninit")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "maybe-uninit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "004y0nzmpfdrhz251278341z6ql34iv1k6dp1h6af7d6nd6jwc30"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "MaybeUninit for friends of backwards compatibility")
    (description
      (beautify-description "MaybeUninit for friends of backwards compatibility"))
    (license license:asl2.0)))

(define rust-memchr_2_5_0
  (package
    (name "rust-memchr")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vanfk5mzs1g1syqnj03q8n0syggnhn55dq535h2wxr7rwpfbzrd"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis "Safe interface to memchr.")
    (description
      (beautify-description "Safe interface to memchr."))
    (license (list license:unlicense
               license:expat))))

(define rust-migrations_internals_1_4_1
  (package
    (name "rust-migrations_internals")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "migrations_internals" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0q2qk4jpa16mcfcmhjz6hdg2s73az1k7j0cy08vvh87h997chkrb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-diesel" ,rust-diesel_1_4_8))))
    (home-page "https://diesel.rs")
    (synopsis "Internal implementation of diesels migration mechanism")
    (description
      (beautify-description "Internal implementation of diesels migration mechanism"))
    (license license:expat)))

(define rust-migrations_macros_1_4_2
  (package
    (name "rust-migrations_macros")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "migrations_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "173qgwans6n2jf6b7qajq273rvg1r9c34p5fflzr53gx14lz2lwp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-migrations_internals" ,rust-migrations_internals_1_4_1)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://diesel.rs")
    (synopsis "Codegeneration macros for diesels embedded migrations")
    (description
      (beautify-description "Codegeneration macros for diesels embedded migrations"))
    (license license:expat)))

(define rust-mimalloc_0_1_29
  (package
    (name "rust-mimalloc")
    (version "0.1.29")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mimalloc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "163n88d7mmvxyxv0gy2axhy3kvfhn3g68xch5rrjxbv9r61ssr1g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libmimalloc-sys" ,rust-libmimalloc-sys_0_1_25))))
    (home-page "None")
    (synopsis "Performance and security oriented drop-in allocator")
    (description
      (beautify-description "Performance and security oriented drop-in allocator"))
    (license license:expat)))

(define rust-mime_0_3_16
  (package
    (name "rust-mime")
    (version "0.3.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13dcm9lh01hdwfjcg74ppljyjfj1c6w3a3cwkhxf0w8wa37cfq1a"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Strongly Typed Mimes")
    (description
      (beautify-description "Strongly Typed Mimes"))
    (license (list license:expat
               license:asl2.0))))

(define rust-minimal-lexical_0_2_1
  (package
    (name "rust-minimal-lexical")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minimal-lexical" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast float parsing conversion routines.")
    (description
      (beautify-description "Fast float parsing conversion routines."))
    (license (list license:expat
               license:asl2.0))))

(define rust-miniz_oxide_0_5_1
  (package
    (name "rust-miniz_oxide")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10phz3ppw4p8pz4rwniy3qkw95wiq64kbvpb0l8kjcrzpka9pcnj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-adler" ,rust-adler_1_0_2))))
    (home-page "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (description
      (beautify-description "DEFLATE compression and decompression library rewritten in Rust based on miniz"))
    (license license:asl2.0)))

(define rust-mio_0_6_23
  (package
    (name "rust-mio")
    (version "0.6.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i2c1vl8lr45apkh8xbh9k56ihfsmqff5l7s2fya7whvp7sndzaa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_0_1_10)        
        ("rust-fuchsia-zircon" ,rust-fuchsia-zircon_0_3_3)        
        ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys_0_3_3)        
        ("rust-iovec" ,rust-iovec_0_1_4)        
        ("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-miow" ,rust-miow_0_2_2)        
        ("rust-net2" ,rust-net2_0_2_37)        
        ("rust-slab" ,rust-slab_0_4_6)        
        ("rust-winapi" ,rust-winapi_0_2_8))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description
      (beautify-description "Lightweight non-blocking IO"))
    (license license:expat)))

(define rust-mio_0_8_3
  (package
    (name "rust-mio")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "168pqd9v7llhhal1jy5l1k0k8qp0g8hsddv6w1s93n24kc6magbi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-wasi" ,rust-wasi_0_11_0+wasi-snapshot-preview1)        
        ("rust-windows-sys" ,rust-windows-sys_0_36_1))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description
      (beautify-description "Lightweight non-blocking IO"))
    (license license:expat)))

(define rust-mio-extras_2_0_6
  (package
    (name "rust-mio-extras")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio-extras" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "069gfhlv0wlwfx1k2sriwfws490kjp490rv2qivyfb01j3i3yh2j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazycell" ,rust-lazycell_1_3_0)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-mio" ,rust-mio_0_6_23)        
        ("rust-slab" ,rust-slab_0_4_6))))
    (home-page "None")
    (synopsis "Extra components for use with Mio")
    (description
      (beautify-description "Extra components for use with Mio"))
    (license license:expat)))

(define rust-miow_0_2_2
  (package
    (name "rust-miow")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kcl8rnv0bhiarcdakik670w8fnxzlxhi1ys7152sck68510in7b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
        ("rust-net2" ,rust-net2_0_2_37)        
        ("rust-winapi" ,rust-winapi_0_2_8)        
        ("rust-ws2_32-sys" ,rust-ws2_32-sys_0_2_1))))
    (home-page "https://github.com/yoshuawuyts/miow")
    (synopsis "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O\nabstractions.")
    (description
      (beautify-description "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O\nabstractions."))
    (license (list license:expat
               license:asl2.0))))

(define rust-multer_2_0_2
  (package
    (name "rust-multer")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "multer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0diqknyfg0m131bm19rll4abg34ad7k122arcwb5q7anhzk3b3sz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-encoding_rs" ,rust-encoding_rs_0_8_31)        
        ("rust-futures-util" ,rust-futures-util_0_3_21)        
        ("rust-http" ,rust-http_0_2_7)        
        ("rust-httparse" ,rust-httparse_1_7_1)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-mime" ,rust-mime_0_3_16)        
        ("rust-spin" ,rust-spin_0_9_3)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-tokio-util" ,rust-tokio-util_0_6_9)        
        ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "https://github.com/rousan/multer-rs")
    (synopsis "An async parser for `multipart/form-data` content-type in Rust.")
    (description
      (beautify-description "An async parser for `multipart/form-data` content-type in Rust."))
    (license license:expat)))

(define rust-mysqlclient-sys_0_2_5
  (package
    (name "rust-mysqlclient-sys")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mysqlclient-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16wndr59cbpc2wgli45zfgi0hi837pbrsh1aqh2k0ads50akh6zn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pkg-config" ,rust-pkg-config_0_3_25)        
        ("rust-vcpkg" ,rust-vcpkg_0_2_15))))
    (home-page "None")
    (synopsis "Auto-generated rust bindings for libmysqlclient")
    (description
      (beautify-description "Auto-generated rust bindings for libmysqlclient"))
    (license license:expat)))

(define rust-native-tls_0_2_10
  (package
    (name "rust-native-tls")
    (version "0.2.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "native-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ad4dhkbc3r9rbqdym1cl5zwkqzfa9i8bs0p1c79hzsm30v2yzpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-openssl" ,rust-openssl_0_10_40)        
        ("rust-openssl-probe" ,rust-openssl-probe_0_1_5)        
        ("rust-openssl-sys" ,rust-openssl-sys_0_9_73)        
        ("rust-schannel" ,rust-schannel_0_1_19)        
        ("rust-security-framework" ,rust-security-framework_2_6_1)        
        ("rust-security-framework-sys" ,rust-security-framework-sys_2_6_1)        
        ("rust-tempfile" ,rust-tempfile_3_3_0))))
    (home-page "None")
    (synopsis "A wrapper over a platform\u0027s native TLS implementation")
    (description
      (beautify-description "A wrapper over a platform\u0027s native TLS implementation"))
    (license (list license:expat
               license:asl2.0))))

(define rust-net2_0_2_37
  (package
    (name "rust-net2")
    (version "0.2.37")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "net2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bk8jp0i12gvhrlaqbfq19ancja70r1rg3sywbhjl0385g8k05ir"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_0_1_10)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/deprecrated/net2-rs")
    (synopsis "Extensions to the standard library\u0027s networking types as proposed in RFC 1158.")
    (description
      (beautify-description "Extensions to the standard library\u0027s networking types as proposed in RFC 1158."))
    (license (list license:expat
               license:asl2.0))))

(define rust-nix_0_24_1
  (package
    (name "rust-nix")
    (version "0.24.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sa16i3916lgmrqxghmv50bxz87j42xrfbp3m1dd1b04g4qdy5wg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description
      (beautify-description "Rust friendly bindings to *nix APIs"))
    (license license:expat)))

(define rust-no-std-compat_0_4_1
  (package
    (name "rust-no-std-compat")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "no-std-compat" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "132vrf710zsdp40yp1z3kgc2ss8pi0z4gmihsz3y7hl4dpd56f5r"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A `#![no_std]` compatibility layer that will make porting your crate to no_std *easy*.")
    (description
      (beautify-description "A `#![no_std]` compatibility layer that will make porting your crate to no_std *easy*."))
    (license license:expat)))

(define rust-nom_7_1_1
  (package
    (name "rust-nom")
    (version "7.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0djc3lq5xihnwhrvkc4bj0fd58sjf632yh6hfiw545x355d3x458"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-minimal-lexical" ,rust-minimal-lexical_0_2_1))))
    (home-page "None")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
      (beautify-description "A byte-oriented, zero-copy, parser combinators library"))
    (license license:expat)))

(define rust-nonzero_ext_0_3_0
  (package
    (name "rust-nonzero_ext")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nonzero_ext" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08fghyinb07xwhbj7vwvlhg45g5cvhvld2min25njidir12rdgrq"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Extensions and additional traits for non-zero integer types")
    (description
      (beautify-description "Extensions and additional traits for non-zero integer types"))
    (license license:asl2.0)))

(define rust-num-bigint_0_4_3
  (package
    (name "rust-num-bigint")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-bigint" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0py73wsa5j4izhd39nkqzqv260r0ma08vy30ky54ld3vkhlbcfpr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_1_0)        
        ("rust-num-integer" ,rust-num-integer_0_1_45)        
        ("rust-num-traits" ,rust-num-traits_0_2_15))))
    (home-page "https://github.com/rust-num/num-bigint")
    (synopsis "Big integer implementation for Rust")
    (description
      (beautify-description "Big integer implementation for Rust"))
    (license license:expat)))

(define rust-num-derive_0_3_3
  (package
    (name "rust-num-derive")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gbl94ckzqjdzy4j8b1p55mz01g6n1l9bckllqvaj0wfz7zm6sl7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://github.com/rust-num/num-derive")
    (synopsis "Numeric syntax extensions")
    (description
      (beautify-description "Numeric syntax extensions"))
    (license license:expat)))

(define rust-num-integer_0_1_45
  (package
    (name "rust-num-integer")
    (version "0.1.45")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-integer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ncwavvwdmsqzxnn65phv6c6nn72pnv9xhpmjd6a429mzf4k6p92"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_1_0)        
        ("rust-num-traits" ,rust-num-traits_0_2_15))))
    (home-page "https://github.com/rust-num/num-integer")
    (synopsis "Integer traits and functions")
    (description
      (beautify-description "Integer traits and functions"))
    (license license:expat)))

(define rust-num-traits_0_2_15
  (package
    (name "rust-num-traits")
    (version "0.2.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-traits" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kfdqqw2ndz0wx2j75v9nbjx7d3mh3150zs4p5595y02rwsdx3jp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_1_0))))
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description
      (beautify-description "Numeric traits for generic mathematics"))
    (license license:expat)))

(define rust-num_cpus_1_13_1
  (package
    (name "rust-num_cpus")
    (version "1.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_cpus" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18apx62z4j4lajj2fi6r1i8slr9rs2d0xrbj2ls85qfyxck4brhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hermit-abi" ,rust-hermit-abi_0_1_19)        
        ("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "Get the number of CPUs on a machine.")
    (description
      (beautify-description "Get the number of CPUs on a machine."))
    (license license:expat)))

(define rust-num_threads_0_1_6
  (package
    (name "rust-num_threads")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_threads" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i5vmffsv6g79z869flp1sja69g1gapddjagdw1k3q9f3l2cw698"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "A minimal library that determines the number of running threads for the current process.")
    (description
      (beautify-description "A minimal library that determines the number of running threads for the current process."))
    (license license:expat)))

(define rust-object_0_28_4
  (package
    (name "rust-object")
    (version "0.28.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "object" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0964501nlfh806mik3f9v6n05mx74qa0w7byvn0sqpwm5lprhb74"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr_2_5_0))))
    (home-page "None")
    (synopsis "A unified interface for reading and writing object file formats.")
    (description
      (beautify-description "A unified interface for reading and writing object file formats."))
    (license license:asl2.0)))

(define rust-once_cell_1_10_0
  (package
    (name "rust-once_cell")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "once_cell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fgclb93az22gq5lmqsm84kilx1p1xpij559bmvx2mn1x8vy1ww7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Single assignment cells and lazy values.")
    (description
      (beautify-description "Single assignment cells and lazy values."))
    (license license:expat)))

(define rust-opaque-debug_0_2_3
  (package
    (name "rust-opaque-debug")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "opaque-debug" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "172j6bs8ndclqxa2m64qc0y1772rr73g4l9fg2svscgicnbfff98"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Macro for opaque Debug trait implementation")
    (description
      (beautify-description "Macro for opaque Debug trait implementation"))
    (license license:expat)))

(define rust-opaque-debug_0_3_0
  (package
    (name "rust-opaque-debug")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "opaque-debug" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m8kzi4nd6shdqimn0mgb24f0hxslhnqd1whakyq06wcqd086jk2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Macro for opaque Debug trait implementation")
    (description
      (beautify-description "Macro for opaque Debug trait implementation"))
    (license license:expat)))

(define rust-openssl_0_10_40
  (package
    (name "rust-openssl")
    (version "0.10.40")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03pbv3ig4ai9whg0znnz9y5fmd7i58fqzb75byraq4f9191sd0gv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-foreign-types" ,rust-foreign-types_0_3_2)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-once_cell" ,rust-once_cell_1_10_0)        
        ("rust-openssl-macros" ,rust-openssl-macros_0_1_0)        
        ("rust-openssl-sys" ,rust-openssl-sys_0_9_73))))
    (home-page "None")
    (synopsis "OpenSSL bindings")
    (description
      (beautify-description "OpenSSL bindings"))
    (license license:asl2.0)))

(define rust-openssl-macros_0_1_0
  (package
    (name "rust-openssl-macros")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v3kgnzbadrf9c06q4cqmbjas53av73n5w7wwz3n0nb6257y80dm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Internal macros used by the openssl crate.")
    (description
      (beautify-description "Internal macros used by the openssl crate."))
    (license (list license:expat
               license:asl2.0))))

(define rust-openssl-probe_0_1_5
  (package
    (name "rust-openssl-probe")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-probe" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kq18qm48rvkwgcggfkqq6pm948190czqc94d6bm2sir5hq1l0gz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/openssl-probe")
    (synopsis "Tool for helping to find SSL certificate locations on the system for OpenSSL")
    (description
      (beautify-description "Tool for helping to find SSL certificate locations on the system for OpenSSL"))
    (license (list license:expat
               license:asl2.0))))

(define rust-openssl-src_111_18_0+1_1_1n
  (package
    (name "rust-openssl-src")
    (version "111.18.0+1.1.1n")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-src" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v7zlbnc6whd929davb6rbjr4hna1q9j1h3x28ch5l78w4kak5vq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_73))))
    (home-page "None")
    (synopsis "Source of OpenSSL and logic to build it.")
    (description
      (beautify-description "Source of OpenSSL and logic to build it."))
    (license (list license:expat
               license:asl2.0))))

(define rust-openssl-sys_0_9_73
  (package
    (name "rust-openssl-sys")
    (version "0.9.73")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h0bv7cwrbbwdnpj96mb2b0p0gkajwc5g4rl3qf1ka70nfgx2pwx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_1_0)        
        ("rust-cc" ,rust-cc_1_0_73)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-openssl-src" ,rust-openssl-src_111_18_0+1_1_1n)        
        ("rust-pkg-config" ,rust-pkg-config_0_3_25)        
        ("rust-vcpkg" ,rust-vcpkg_0_2_15))))
    (home-page "None")
    (synopsis "FFI bindings to OpenSSL")
    (description
      (beautify-description "FFI bindings to OpenSSL"))
    (license license:expat)))

(define rust-owning_ref_0_3_3
  (package
    (name "rust-owning_ref")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "owning_ref" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dqgf5hwbmvkf2ffbik5xmhvaqvqi6iklhwk9x47n0wycd0lzy6d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-stable_deref_trait" ,rust-stable_deref_trait_1_2_0))))
    (home-page "None")
    (synopsis "A library for creating references that carry their owner with them.")
    (description
      (beautify-description "A library for creating references that carry their owner with them."))
    (license license:expat)))

(define rust-parity-ws_0_11_1
  (package
    (name "rust-parity-ws")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parity-ws" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iphdnjzihq86c54r9sdskn6c639kpql6rwsxg1i43ymka9d70sr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder_1_4_3)        
        ("rust-bytes" ,rust-bytes_0_4_12)        
        ("rust-httparse" ,rust-httparse_1_7_1)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-mio" ,rust-mio_0_6_23)        
        ("rust-mio-extras" ,rust-mio-extras_2_0_6)        
        ("rust-rand" ,rust-rand_0_7_3)        
        ("rust-sha-1" ,rust-sha-1_0_8_2)        
        ("rust-slab" ,rust-slab_0_4_6)        
        ("rust-url" ,rust-url_2_2_2))))
    (home-page "None")
    (synopsis "Lightweight, event-driven WebSockets for Rust.")
    (description
      (beautify-description "Lightweight, event-driven WebSockets for Rust."))
    (license license:expat)))

(define rust-parking_lot_0_4_8
  (package
    (name "rust-parking_lot")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ph0kv3dfcxpjbi83wkzammqb7lm95j8in7w7hz17hgkjxdqz78l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-owning_ref" ,rust-owning_ref_0_3_3)        
        ("rust-parking_lot_core" ,rust-parking_lot_core_0_2_14))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license (list license:asl2.0
               license:expat))))

(define rust-parking_lot_0_11_2
  (package
    (name "rust-parking_lot")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16gzf41bxmm10x82bla8d6wfppy9ym3fxsmdjyvn61m66s0bf5vx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-instant" ,rust-instant_0_1_12)        
        ("rust-lock_api" ,rust-lock_api_0_4_7)        
        ("rust-parking_lot_core" ,rust-parking_lot_core_0_8_5))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license (list license:asl2.0
               license:expat))))

(define rust-parking_lot_0_12_0
  (package
    (name "rust-parking_lot")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n7gp0cnfghglc370cxhawwfijvhj3wrjh8gdi8c06m6jcjfrxc7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lock_api" ,rust-lock_api_0_4_7)        
        ("rust-parking_lot_core" ,rust-parking_lot_core_0_9_3))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license (list license:asl2.0
               license:expat))))

(define rust-parking_lot_core_0_2_14
  (package
    (name "rust-parking_lot_core")
    (version "0.2.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yip8m6npxb87ilnn0q774psp1zd0vgv66fcjkkvr9rlyz6aicad"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125)        
        ("rust-rand" ,rust-rand_0_4_6)        
        ("rust-smallvec" ,rust-smallvec_0_6_14)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license (list license:asl2.0
               license:expat))))

(define rust-parking_lot_core_0_8_5
  (package
    (name "rust-parking_lot_core")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05ij4zxsylx99srbq8qd1k2wiwaq8krkf9y4cqkhvb5wjca8wvnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-instant" ,rust-instant_0_1_12)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-redox_syscall" ,rust-redox_syscall_0_2_13)        
        ("rust-smallvec" ,rust-smallvec_1_8_0)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license (list license:asl2.0
               license:expat))))

(define rust-parking_lot_core_0_9_3
  (package
    (name "rust-parking_lot_core")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ab95rljb99rm51wcic16jgbajcr6lgbqkrr21w7bc2wyb5pk8h9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-redox_syscall" ,rust-redox_syscall_0_2_13)        
        ("rust-smallvec" ,rust-smallvec_1_8_0)        
        ("rust-windows-sys" ,rust-windows-sys_0_36_1))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license license:expat)))

(define rust-parse-zoneinfo_0_3_0
  (package
    (name "rust-parse-zoneinfo")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parse-zoneinfo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0h8g6jy4kckn2gk8sd5adaws180n1ip65xhzw5jxlq4w8ibg41f7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-regex" ,rust-regex_1_5_5))))
    (home-page "None")
    (synopsis "Parse zoneinfo files from the IANA database")
    (description
      (beautify-description "Parse zoneinfo files from the IANA database"))
    (license license:expat)))

(define rust-paste_1_0_7
  (package
    (name "rust-paste")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z15h1rnq1wcacpcvgm77djl3413gs1nlhmn90qpcvjx2c2hwlhc"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Macros for all your token pasting needs")
    (description
      (beautify-description "Macros for all your token pasting needs"))
    (license license:expat)))

(define rust-pear_0_2_3
  (package
    (name "rust-pear")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pear" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00l7llav8cidhclx0m2gxm267pfa90c7r2x7xbinij74qm0l5r0m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-inlinable_string" ,rust-inlinable_string_0_1_15)        
        ("rust-pear_codegen" ,rust-pear_codegen_0_2_3)        
        ("rust-yansi" ,rust-yansi_0_5_1))))
    (home-page "None")
    (synopsis "A pear is a fruit.")
    (description
      (beautify-description "A pear is a fruit."))
    (license (list license:expat
               license:asl2.0))))

(define rust-pear_codegen_0_2_3
  (package
    (name "rust-pear_codegen")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pear_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l4209fi1n0wj110l12l4xpy32d1xffm61nm82vyq0r37ijcm9c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics_0_9_1)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "A (codegen) pear is a fruit.")
    (description
      (beautify-description "A (codegen) pear is a fruit."))
    (license (list license:expat
               license:asl2.0))))

(define rust-pem_1_0_2
  (package
    (name "rust-pem")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pem" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iqrvfnm71x9pvff39d5ajwn3gc9glxlv4d4h22max7342db18z9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_13_0))))
    (home-page "https://github.com/jcreekmore/pem-rs.git")
    (synopsis "Parse and encode PEM-encoded data.")
    (description
      (beautify-description "Parse and encode PEM-encoded data."))
    (license license:expat)))

(define rust-percent-encoding_1_0_1
  (package
    (name "rust-percent-encoding")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cgq08v1fvr6bs5fvy390cz830lq4fak8havdasdacxcw790s09i"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Percent encoding and decoding")
    (description
      (beautify-description "Percent encoding and decoding"))
    (license (list license:expat
               license:asl2.0))))

(define rust-percent-encoding_2_1_0
  (package
    (name "rust-percent-encoding")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bp3zrsk3kr47fbpipyczidbbx4g54lzxdm77ni1i3qws10mdzfl"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Percent encoding and decoding")
    (description
      (beautify-description "Percent encoding and decoding"))
    (license (list license:expat
               license:asl2.0))))

(define rust-pest_2_1_3
  (package
    (name "rust-pest")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lry80bm90x47nq71wxq83kjrm9ashpz4kbm92p90ysdx4m8gx0h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ucd-trie" ,rust-ucd-trie_0_1_3))))
    (home-page "https://pest-parser.github.io/")
    (synopsis "The Elegant Parser")
    (description
      (beautify-description "The Elegant Parser"))
    (license (list license:expat
               license:asl2.0))))

(define rust-pest_derive_2_1_0
  (package
    (name "rust-pest_derive")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l5jfa6ril71cw5nsiw0r45br54dd8cj2r1nc2d1wq6wb3jilgc3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pest" ,rust-pest_2_1_3)        
        ("rust-pest_generator" ,rust-pest_generator_2_1_3))))
    (home-page "https://pest-parser.github.io/")
    (synopsis "pest\u0027s derive macro")
    (description
      (beautify-description "pest\u0027s derive macro"))
    (license (list license:expat
               license:asl2.0))))

(define rust-pest_generator_2_1_3
  (package
    (name "rust-pest_generator")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mfgl0p6v91ywdqr9i8w053v70cnfqjk8y5rhwbvir9idridpf4r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pest" ,rust-pest_2_1_3)        
        ("rust-pest_meta" ,rust-pest_meta_2_1_3)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://pest-parser.github.io/")
    (synopsis "pest code generator")
    (description
      (beautify-description "pest code generator"))
    (license (list license:expat
               license:asl2.0))))

(define rust-pest_meta_2_1_3
  (package
    (name "rust-pest_meta")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_meta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07d1jbbbpxpchk0j37ljas46sdyyg599z3zw2ac0f5sk9x06xgjl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-maplit" ,rust-maplit_1_0_2)        
        ("rust-pest" ,rust-pest_2_1_3)        
        ("rust-sha-1" ,rust-sha-1_0_8_2))))
    (home-page "https://pest-parser.github.io/")
    (synopsis "pest meta language parser and validator")
    (description
      (beautify-description "pest meta language parser and validator"))
    (license (list license:expat
               license:asl2.0))))

(define rust-phf_0_10_1
  (package
    (name "rust-phf")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0naj8n5nasv5hj5ldlva3cl6y3sv7zp3kfgqylhbrg55v3mg3fzs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-phf_shared" ,rust-phf_shared_0_10_0))))
    (home-page "None")
    (synopsis "Runtime support for perfect hash function data structures")
    (description
      (beautify-description "Runtime support for perfect hash function data structures"))
    (license license:expat)))

(define rust-phf_codegen_0_10_0
  (package
    (name "rust-phf_codegen")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k8kdad9wk2d5972k6jmjki2xpdy2ky4zd19rv7ybm2dpjlc7cag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-phf_generator" ,rust-phf_generator_0_10_0)        
        ("rust-phf_shared" ,rust-phf_shared_0_10_0))))
    (home-page "None")
    (synopsis "Codegen library for PHF types")
    (description
      (beautify-description "Codegen library for PHF types"))
    (license license:expat)))

(define rust-phf_generator_0_10_0
  (package
    (name "rust-phf_generator")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mlq6hlajsvlsx6rhw49g9ricsm017lrxmgmmbk85sxm7f4qaljx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-phf_shared" ,rust-phf_shared_0_10_0)        
        ("rust-rand" ,rust-rand_0_8_5))))
    (home-page "None")
    (synopsis "PHF generation logic")
    (description
      (beautify-description "PHF generation logic"))
    (license license:expat)))

(define rust-phf_shared_0_10_0
  (package
    (name "rust-phf_shared")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15n02nc8yqpd8hbxngblar2g53p3nllc93d8s8ih3p5cf7bnlydn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-siphasher" ,rust-siphasher_0_3_10)        
        ("rust-uncased" ,rust-uncased_0_9_6))))
    (home-page "None")
    (synopsis "Support code shared by PHF libraries")
    (description
      (beautify-description "Support code shared by PHF libraries"))
    (license license:expat)))

(define rust-pico-args_0_4_2
  (package
    (name "rust-pico-args")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pico-args" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s646i0pbcck300rqldb21m151zxp66m3mdskha063blrfbcv2yv"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An ultra simple CLI arguments parser.")
    (description
      (beautify-description "An ultra simple CLI arguments parser."))
    (license license:expat)))

(define rust-pin-project-lite_0_2_9
  (package
    (name "rust-pin-project-lite")
    (version "0.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pin-project-lite" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05n1z851l356hpgqadw4ar64mjanaxq1qlwqsf2k05ziq8xax9z0"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A lightweight version of pin-project written with declarative macros.")
    (description
      (beautify-description "A lightweight version of pin-project written with declarative macros."))
    (license license:asl2.0)))

(define rust-pin-utils_0_1_0
  (package
    (name "rust-pin-utils")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pin-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Utilities for pinning")
    (description
      (beautify-description "Utilities for pinning"))
    (license license:expat)))

(define rust-pkg-config_0_3_25
  (package
    (name "rust-pkg-config")
    (version "0.3.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkg-config" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bh3vij79cshj884py4can1f8rvk52niaii1vwxya9q69gnc9y0x"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts.")
    (description
      (beautify-description "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts."))
    (license license:expat)))

(define rust-polyval_0_5_3
  (package
    (name "rust-polyval")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "polyval" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1890wqvc0csc9y9k9k4gsbz91rgdnhn6xnfmy9pqkh674fvd46c4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-cpufeatures" ,rust-cpufeatures_0_2_2)        
        ("rust-opaque-debug" ,rust-opaque-debug_0_3_0)        
        ("rust-universal-hash" ,rust-universal-hash_0_4_1))))
    (home-page "None")
    (synopsis "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing\na Message Authentication Code (MAC)")
    (description
      (beautify-description "POLYVAL is a GHASH-like universal hash over GF(2^128) useful for constructing\na Message Authentication Code (MAC)"))
    (license license:asl2.0)))

(define rust-ppv-lite86_0_2_16
  (package
    (name "rust-ppv-lite86")
    (version "0.2.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ppv-lite86" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wkqwnvnfcgqlrahphl45vdlgi2f1bs7nqcsalsllp1y4dp9x7zb"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description
      (beautify-description "Implementation of the crypto-simd API for x86"))
    (license (list license:expat
               license:asl2.0))))

(define rust-pq-sys_0_4_6
  (package
    (name "rust-pq-sys")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pq-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1npz9756283pjq3lcpwss8xh1rw4sx8f6dz8cxdg90h5bbp5xhka"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-vcpkg" ,rust-vcpkg_0_2_15))))
    (home-page "None")
    (synopsis "Auto-generated rust bindings for libpq")
    (description
      (beautify-description "Auto-generated rust bindings for libpq"))
    (license license:expat)))

(define rust-proc-macro-hack_0_5_19
  (package
    (name "rust-proc-macro-hack")
    (version "0.5.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-hack" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rg0kzsj7lj00qj602d3h77spwfz48vixn1wbjp7a4yrq65w9w6v"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Procedural functionlike!() macros using only Macros 1.1")
    (description
      (beautify-description "Procedural functionlike!() macros using only Macros 1.1"))
    (license license:expat)))

(define rust-proc-macro2_1_0_38
  (package
    (name "rust-proc-macro2")
    (version "1.0.38")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1aj8mil84prgjga536bk45q17hcigxaz7b8q4bx7b4ackn7b89wh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-xid" ,rust-unicode-xid_0_2_3))))
    (home-page "None")
    (synopsis "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple\ntoken-based libraries from the procedural macro use case.")
    (description
      (beautify-description "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple\ntoken-based libraries from the procedural macro use case."))
    (license license:expat)))

(define rust-proc-macro2-diagnostics_0_9_1
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2-diagnostics" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nmazlb1dkznjds7qwms7yxhi33ajc3isji2lsgx8r3lsqk9gwjb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93)        
        ("rust-version_check" ,rust-version_check_0_9_4)        
        ("rust-yansi" ,rust-yansi_0_5_1))))
    (home-page "None")
    (synopsis "Diagnostics for proc-macro2.")
    (description
      (beautify-description "Diagnostics for proc-macro2."))
    (license (list license:expat
               license:asl2.0))))

(define rust-psl-types_2_0_10
  (package
    (name "rust-psl-types")
    (version "2.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "psl-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w74li516dsalxmsk5mfcqbgdbg0dl04qdv2iggszjly5p3agvg8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Common types for the public suffix implementation crates")
    (description
      (beautify-description "Common types for the public suffix implementation crates"))
    (license (list license:expat
               license:asl2.0))))

(define rust-publicsuffix_2_1_1
  (package
    (name "rust-publicsuffix")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "publicsuffix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q9kbcqh9pa06p3kq7d3ksbnqjhs88v5wk5qg89wrgkbmpnp4a99"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder_1_4_3)        
        ("rust-hashbrown" ,rust-hashbrown_0_11_2)        
        ("rust-idna" ,rust-idna_0_2_3)        
        ("rust-psl-types" ,rust-psl-types_2_0_10))))
    (home-page "None")
    (synopsis "Extract root domain and suffix from a domain name")
    (description
      (beautify-description "Extract root domain and suffix from a domain name"))
    (license (list license:expat
               license:asl2.0))))

(define rust-quanta_0_9_3
  (package
    (name "rust-quanta")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quanta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a1q4ah3jvhrnvl7067li88qza664fi0lx0jiffqfpid54afgbr0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils_0_8_8)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-mach" ,rust-mach_0_3_2)        
        ("rust-once_cell" ,rust-once_cell_1_10_0)        
        ("rust-raw-cpuid" ,rust-raw-cpuid_10_3_0)        
        ("rust-wasi" ,rust-wasi_0_10_2+wasi-snapshot-preview1)        
        ("rust-web-sys" ,rust-web-sys_0_3_57)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description
      (beautify-description "high-speed timing library"))
    (license license:expat)))

(define rust-quick-error_1_2_3
  (package
    (name "rust-quick-error")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))))
    (build-system cargo-build-system)
    (home-page "http://github.com/tailhook/quick-error")
    (synopsis "A macro which makes error types pleasant to write.")
    (description
      (beautify-description "A macro which makes error types pleasant to write."))
    (license (list license:expat
               license:asl2.0))))

(define rust-quick-error_2_0_1
  (package
    (name "rust-quick-error")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))))
    (build-system cargo-build-system)
    (home-page "http://github.com/tailhook/quick-error")
    (synopsis "A macro which makes error types pleasant to write.")
    (description
      (beautify-description "A macro which makes error types pleasant to write."))
    (license (list license:expat
               license:asl2.0))))

(define rust-quickcheck_1_0_3
  (package
    (name "rust-quickcheck")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mjhkfqwrb8mdyxdqr4zzbj1rm5dfx25n9zcc25lb6fxwiw673sq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand" ,rust-rand_0_8_5))))
    (home-page "https://github.com/BurntSushi/quickcheck")
    (synopsis "Automatic property based testing with shrinking.")
    (description
      (beautify-description "Automatic property based testing with shrinking."))
    (license (list license:unlicense
               license:expat))))

(define rust-quote_1_0_18
  (package
    (name "rust-quote")
    (version "1.0.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lca4xnwdc2sp76bf4n50kifmi5phhxr9520w623mfcksr7bbzm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38))))
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
      (beautify-description "Quasi-quoting macro quote!(...)"))
    (license license:expat)))

(define rust-quoted_printable_0_4_5
  (package
    (name "rust-quoted_printable")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quoted_printable" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03rc6d5ym6glgvqahlrdammf0566aijpcb1qwcc3997pb772vviz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/staktrace/quoted-printable/blob/master/README.md")
    (synopsis "A simple encoder/decoder for quoted-printable data")
    (description
      (beautify-description "A simple encoder/decoder for quoted-printable data"))
    (license license:bsd-0)))

(define rust-r2d2_0_8_9
  (package
    (name "rust-r2d2")
    (version "0.8.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "r2d2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vxjgh83bss63mkx308p16iwl33s80c781p422f3r5w0p315np2l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log_0_4_17)        
        ("rust-parking_lot" ,rust-parking_lot_0_11_2)        
        ("rust-scheduled-thread-pool" ,rust-scheduled-thread-pool_0_2_5))))
    (home-page "None")
    (synopsis "A generic connection pool")
    (description
      (beautify-description "A generic connection pool"))
    (license (list license:expat
               license:asl2.0))))

(define rust-rand_0_4_6
  (package
    (name "rust-rand")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14qjfv3gggzhnma20k0sc1jf8y6pplsaq7n1j9ls5c8kf2wl0a2m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fuchsia-cprng" ,rust-fuchsia-cprng_0_1_1)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-rand_core" ,rust-rand_core_0_3_1)        
        ("rust-rdrand" ,rust-rdrand_0_4_0)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Random number generators and other randomness functionality.")
    (description
      (beautify-description "Random number generators and other randomness functionality."))
    (license (list license:expat
               license:asl2.0))))

(define rust-rand_0_7_3
  (package
    (name "rust-rand")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00sdaimkbz491qgi6qxkv582yivl32m2jd401kzbn94vsiwicsva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom_0_1_16)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-rand_chacha" ,rust-rand_chacha_0_2_2)        
        ("rust-rand_core" ,rust-rand_core_0_5_1))
       #:cargo-development-inputs
       (("rust-rand_hc" ,rust-rand_hc_0_2_0))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Random number generators and other randomness functionality.")
    (description
      (beautify-description "Random number generators and other randomness functionality."))
    (license license:expat)))

(define rust-rand_0_8_5
  (package
    (name "rust-rand")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125)        
        ("rust-rand_chacha" ,rust-rand_chacha_0_3_1)        
        ("rust-rand_core" ,rust-rand_core_0_6_3))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Random number generators and other randomness functionality.")
    (description
      (beautify-description "Random number generators and other randomness functionality."))
    (license license:expat)))

(define rust-rand_chacha_0_2_2
  (package
    (name "rust-rand_chacha")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00il36fkdbsmpr99p9ksmmp6dn1md7rmnwmz0rr77jbrca2yvj7l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ppv-lite86" ,rust-ppv-lite86_0_2_16)        
        ("rust-rand_core" ,rust-rand_core_0_5_1))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "ChaCha random number generator")
    (description
      (beautify-description "ChaCha random number generator"))
    (license license:expat)))

(define rust-rand_chacha_0_3_1
  (package
    (name "rust-rand_chacha")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ppv-lite86" ,rust-ppv-lite86_0_2_16)        
        ("rust-rand_core" ,rust-rand_core_0_6_3))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "ChaCha random number generator")
    (description
      (beautify-description "ChaCha random number generator"))
    (license license:expat)))

(define rust-rand_core_0_3_1
  (package
    (name "rust-rand_core")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jzdgszfa4bliigiy4hi66k7fs3gfwi2qxn8vik84ph77fwdwvvs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand_core" ,rust-rand_core_0_4_2))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license (list license:expat
               license:asl2.0))))

(define rust-rand_core_0_4_2
  (package
    (name "rust-rand_core")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p09ynysrq1vcdlmcqnapq4qakl2yd1ng3kxh3qscpx09k2a6cww"))))
    (build-system cargo-build-system)
    (home-page "https://rust-random.github.io/book")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license (list license:expat
               license:asl2.0))))

(define rust-rand_core_0_5_1
  (package
    (name "rust-rand_core")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06bdvx08v3rkz451cm7z59xwwqn1rkfh6v9ay77b14f8dwlybgch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom_0_1_16))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license license:expat)))

(define rust-rand_core_0_6_3
  (package
    (name "rust-rand_core")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rxlxc3bpzgwphcg9c9yasvv9idipcg2z2y4j0vlb52jyl418kyk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom_0_2_6))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license license:expat)))

(define rust-rand_hc_0_2_0
  (package
    (name "rust-rand_hc")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_hc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g31sqwpmsirdlwr0svnacr4dbqyz339im4ssl9738cjgfpjjcfa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand_core" ,rust-rand_core_0_5_1))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "HC128 random number generator")
    (description
      (beautify-description "HC128 random number generator"))
    (license (list license:expat
               license:asl2.0))))

(define rust-raw-cpuid_10_3_0
  (package
    (name "rust-raw-cpuid")
    (version "10.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "raw-cpuid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04nsrq2azx7brclhikafjjsfg9df05lm0jjcx73wrvp335qw92vk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2))))
    (home-page "https://github.com/gz/rust-cpuid")
    (synopsis "A library to parse the x86 CPUID instruction, written in rust with no external dependencies. The implementation closely resembles the Intel CPUID manual description. The library does only depend on libcore.")
    (description
      (beautify-description "A library to parse the x86 CPUID instruction, written in rust with no external dependencies. The implementation closely resembles the Intel CPUID manual description. The library does only depend on libcore."))
    (license license:expat)))

(define rust-rdrand_0_4_0
  (package
    (name "rust-rdrand")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rdrand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cjq0kwx1bk7jx3kzyciiish5gqsj7620dm43dc52sr8fzmm9037"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand_core" ,rust-rand_core_0_3_1))))
    (home-page "None")
    (synopsis "An implementation of random number generator based on rdrand and rdseed instructions")
    (description
      (beautify-description "An implementation of random number generator based on rdrand and rdseed instructions"))
    (license license:isc)))

(define rust-redox_syscall_0_2_13
  (package
    (name "rust-redox_syscall")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hpgwvgjlg1j9z7bjf5y18fkd8ag7y4znhqxg85hnpp5qz25pwk2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2))))
    (home-page "None")
    (synopsis "A Rust library to access raw Redox system calls")
    (description
      (beautify-description "A Rust library to access raw Redox system calls"))
    (license license:expat)))

(define rust-redox_users_0_4_3
  (package
    (name "rust-redox_users")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_users" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0asw3s4iy69knafkhvlbchy230qawc297vddjdwjs5nglwvxhcxh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom_0_2_6)        
        ("rust-redox_syscall" ,rust-redox_syscall_0_2_13)        
        ("rust-thiserror" ,rust-thiserror_1_0_31))))
    (home-page "None")
    (synopsis "A Rust library to access Redox users and groups functionality")
    (description
      (beautify-description "A Rust library to access Redox users and groups functionality"))
    (license license:expat)))

(define rust-ref-cast_1_0_7
  (package
    (name "rust-ref-cast")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ref-cast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1arjfjs0b52wa4z43p6v2rmppnssyr5pr8l8rkj86avcbdi5hpb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ref-cast-impl" ,rust-ref-cast-impl_1_0_7))))
    (home-page "None")
    (synopsis "Safely cast \u0026T to \u0026U where the struct U contains a single field of type T.")
    (description
      (beautify-description "Safely cast \u0026T to \u0026U where the struct U contains a single field of type T."))
    (license license:expat)))

(define rust-ref-cast-impl_1_0_7
  (package
    (name "rust-ref-cast-impl")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ref-cast-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07zs83c6nj07h1b1jssmqi6p5xa3xp03l66598vnjhf955784hx0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Derive implementation for ref_cast::RefCast.")
    (description
      (beautify-description "Derive implementation for ref_cast::RefCast."))
    (license license:expat)))

(define rust-regex_1_5_5
  (package
    (name "rust-regex")
    (version "1.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11kjfh41h7i33sskb8i36kl03260rrjw74nb2njhbzr5ddxn848s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick_0_7_18)        
        ("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-regex-syntax" ,rust-regex-syntax_0_6_25))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license license:expat)))

(define rust-regex-automata_0_1_10
  (package
    (name "rust-regex-automata")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-automata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-regex-syntax" ,rust-regex-syntax_0_6_25))))
    (home-page "https://github.com/BurntSushi/regex-automata")
    (synopsis "Automata construction and matching using regular expressions.")
    (description
      (beautify-description "Automata construction and matching using regular expressions."))
    (license (list license:unlicense
               license:expat))))

(define rust-regex-syntax_0_6_25
  (package
    (name "rust-regex-syntax")
    (version "0.6.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16y87hz1bxmmz6kk360cxwfm3jnbsxb3x4zw9x1gzz7khic2i5zl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      (beautify-description "A regular expression parser."))
    (license (list license:expat
               license:asl2.0))))

(define rust-remove_dir_all_0_5_3
  (package
    (name "rust-remove_dir_all")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "remove_dir_all" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rzqbsgkmr053bxxl04vmvsd1njyz0nxvly97aip6aa2cmb15k9s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "A safe, reliable implementation of remove_dir_all for Windows")
    (description
      (beautify-description "A safe, reliable implementation of remove_dir_all for Windows"))
    (license (list license:expat
               license:asl2.0))))

(define rust-reqwest_0_11_10
  (package
    (name "rust-reqwest")
    (version "0.11.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "reqwest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ss1ijakw48dgpxaj5a38pk0r3vmzhdgaj842ssfir9m9ymgg8a6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-compression" ,rust-async-compression_0_3_13)        
        ("rust-base64" ,rust-base64_0_13_0)        
        ("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-cookie" ,rust-cookie_0_15_1)        
        ("rust-cookie_store" ,rust-cookie_store_0_15_1)        
        ("rust-encoding_rs" ,rust-encoding_rs_0_8_31)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-util" ,rust-futures-util_0_3_21)        
        ("rust-h2" ,rust-h2_0_3_13)        
        ("rust-http" ,rust-http_0_2_7)        
        ("rust-http-body" ,rust-http-body_0_4_4)        
        ("rust-hyper-tls" ,rust-hyper-tls_0_5_0)        
        ("rust-ipnet" ,rust-ipnet_2_5_0)        
        ("rust-js-sys" ,rust-js-sys_0_3_57)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-mime" ,rust-mime_0_3_16)        
        ("rust-native-tls" ,rust-native-tls_0_2_10)        
        ("rust-percent-encoding" ,rust-percent-encoding_2_1_0)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-serde_urlencoded" ,rust-serde_urlencoded_0_7_1)        
        ("rust-tokio-native-tls" ,rust-tokio-native-tls_0_3_0)        
        ("rust-tokio-socks" ,rust-tokio-socks_0_5_1)        
        ("rust-tokio-util" ,rust-tokio-util_0_6_9)        
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver_0_20_4)        
        ("rust-url" ,rust-url_2_2_2)        
        ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_80)        
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures_0_4_30)        
        ("rust-web-sys" ,rust-web-sys_0_3_57)        
        ("rust-winreg" ,rust-winreg_0_10_1))
       #:cargo-development-inputs
       (("rust-hyper" ,rust-hyper_0_14_18)        
        ("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "None")
    (synopsis "higher level HTTP client library")
    (description
      (beautify-description "higher level HTTP client library"))
    (license (list license:expat
               license:asl2.0))))

(define rust-resolv-conf_0_7_0
  (package
    (name "rust-resolv-conf")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "resolv-conf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "005sk8r1php2g41yn7fdf1sn8cafyaqm6jxia42h2v88saa47r2j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hostname" ,rust-hostname_0_3_1)        
        ("rust-quick-error" ,rust-quick-error_1_2_3))))
    (home-page "http://github.com/tailhook/resolv-conf")
    (synopsis "The resolv.conf file parser")
    (description
      (beautify-description "The resolv.conf file parser"))
    (license (list license:expat
               license:asl2.0))))

(define rust-ring_0_16_20
  (package
    (name "rust-ring")
    (version "0.16.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ring" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z682xp7v38ayq9g9nkbhhfpj6ygralmlx7wdmsfv8rnw99cylrh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_73)        
        ("rust-once_cell" ,rust-once_cell_1_10_0)        
        ("rust-spin" ,rust-spin_0_5_2)        
        ("rust-untrusted" ,rust-untrusted_0_7_1)        
        ("rust-web-sys" ,rust-web-sys_0_3_57)        
        ("rust-winapi" ,rust-winapi_0_3_9))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "Safe, fast, small crypto using Rust.")
    (description
      (beautify-description "Safe, fast, small crypto using Rust."))
    (license #t)))

(define rust-rmp_0_8_11
  (package
    (name "rust-rmp")
    (version "0.8.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rmp" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17rw803xv84csxgd654g7q64kqf9zgkvhsn8as3dbmlg6mr92la4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder_1_4_3)        
        ("rust-num-traits" ,rust-num-traits_0_2_15)        
        ("rust-paste" ,rust-paste_1_0_7))))
    (home-page "None")
    (synopsis "Pure Rust MessagePack serialization implementation")
    (description
      (beautify-description "Pure Rust MessagePack serialization implementation"))
    (license license:expat)))

(define rust-rmpv_1_0_0
  (package
    (name "rust-rmpv")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rmpv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m4p668sh2h9rsl9n1gzdf6xhxa1g2wby9arzqw52p7rlari726y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-traits" ,rust-num-traits_0_2_15)        
        ("rust-rmp" ,rust-rmp_0_8_11))))
    (home-page "None")
    (synopsis "Value variant for RMP")
    (description
      (beautify-description "Value variant for RMP"))
    (license license:expat)))

(define rust-rocket_0_5_0-rc_2
  (package
    (name "rust-rocket")
    (version "0.5.0-rc.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05wkp7a91ak4jgjhqkpifxh1qiv4vymhkks9ngz0b974zj1x1slq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-stream" ,rust-async-stream_0_3_3)        
        ("rust-async-trait" ,rust-async-trait_0_1_53)        
        ("rust-atomic" ,rust-atomic_0_5_1)        
        ("rust-atty" ,rust-atty_0_2_14)        
        ("rust-binascii" ,rust-binascii_0_1_4)        
        ("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-either" ,rust-either_1_6_1)        
        ("rust-figment" ,rust-figment_0_10_6)        
        ("rust-futures" ,rust-futures_0_3_21)        
        ("rust-indexmap" ,rust-indexmap_1_8_1)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-multer" ,rust-multer_2_0_2)        
        ("rust-num_cpus" ,rust-num_cpus_1_13_1)        
        ("rust-parking_lot" ,rust-parking_lot_0_12_0)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-rand" ,rust-rand_0_8_5)        
        ("rust-ref-cast" ,rust-ref-cast_1_0_7)        
        ("rust-rocket_codegen" ,rust-rocket_codegen_0_5_0-rc_2)        
        ("rust-rocket_http" ,rust-rocket_http_0_5_0-rc_2)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-state" ,rust-state_0_5_3)        
        ("rust-tempfile" ,rust-tempfile_3_3_0)        
        ("rust-time" ,rust-time_0_3_9)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-tokio-stream" ,rust-tokio-stream_0_1_8)        
        ("rust-tokio-util" ,rust-tokio-util_0_7_1)        
        ("rust-ubyte" ,rust-ubyte_0_10_1)        
        ("rust-version_check" ,rust-version_check_0_9_4)        
        ("rust-yansi" ,rust-yansi_0_5_1))))
    (home-page "https://rocket.rs")
    (synopsis "Web framework for nightly with a focus on ease-of-use, expressibility, and speed.")
    (description
      (beautify-description "Web framework for nightly with a focus on ease-of-use, expressibility, and speed."))
    (license license:expat)))

(define rust-rocket_codegen_0_5_0-rc_2
  (package
    (name "rust-rocket_codegen")
    (version "0.5.0-rc.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iwvk69rsbww6j5r1r8mqr66mxrnpxks43np00ncvsb1kjxvdbnn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-devise" ,rust-devise_0_3_1)        
        ("rust-glob" ,rust-glob_0_3_0)        
        ("rust-indexmap" ,rust-indexmap_1_8_1)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-rocket_http" ,rust-rocket_http_0_5_0-rc_2)        
        ("rust-syn" ,rust-syn_1_0_93)        
        ("rust-unicode-xid" ,rust-unicode-xid_0_2_3))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework.")
    (description
      (beautify-description "Procedural macros for the Rocket web framework."))
    (license license:expat)))

(define rust-rocket_http_0_5_0-rc_2
  (package
    (name "rust-rocket_http")
    (version "0.5.0-rc.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18hpzjmgvl4ibgk62i4qcpq949qsp3s0nqvi4k0y6kcm4z8nbv9d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cookie" ,rust-cookie_0_16_0)        
        ("rust-either" ,rust-either_1_6_1)        
        ("rust-futures" ,rust-futures_0_3_21)        
        ("rust-http" ,rust-http_0_2_7)        
        ("rust-hyper" ,rust-hyper_0_14_18)        
        ("rust-indexmap" ,rust-indexmap_1_8_1)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-pear" ,rust-pear_0_2_3)        
        ("rust-percent-encoding" ,rust-percent-encoding_2_1_0)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-ref-cast" ,rust-ref-cast_1_0_7)        
        ("rust-rustls" ,rust-rustls_0_20_4)        
        ("rust-rustls-pemfile" ,rust-rustls-pemfile_1_0_0)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-smallvec" ,rust-smallvec_1_8_0)        
        ("rust-stable-pattern" ,rust-stable-pattern_0_1_0)        
        ("rust-state" ,rust-state_0_5_3)        
        ("rust-time" ,rust-time_0_3_9)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-tokio-rustls" ,rust-tokio-rustls_0_23_4)        
        ("rust-uncased" ,rust-uncased_0_9_6))))
    (home-page "https://rocket.rs")
    (synopsis "Types, traits, and parsers for HTTP requests, responses, and headers.")
    (description
      (beautify-description "Types, traits, and parsers for HTTP requests, responses, and headers."))
    (license license:expat)))

(define rust-rustc-demangle_0_1_21
  (package
    (name "rust-rustc-demangle")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hn3xyd2n3bg3jnc5a5jbzll32n4r5a65bqzs287l30m5c53xw3y"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/rustc-demangle")
    (synopsis "Rust compiler symbol demangling.")
    (description
      (beautify-description "Rust compiler symbol demangling."))
    (license (list license:expat
               license:asl2.0))))

(define rust-rustc_version_0_2_3
  (package
    (name "rust-rustc_version")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-semver" ,rust-semver_0_9_0))))
    (home-page "None")
    (synopsis "A library for querying the version of a installed rustc compiler")
    (description
      (beautify-description "A library for querying the version of a installed rustc compiler"))
    (license (list license:expat
               license:asl2.0))))

(define rust-rustls_0_20_4
  (package
    (name "rust-rustls")
    (version "0.20.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08b941jj4kk1bfg82zrr5b2ifa4ip155g9cpqmmp116v1n6ypgsg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log_0_4_17)        
        ("rust-ring" ,rust-ring_0_16_20)        
        ("rust-sct" ,rust-sct_0_7_0)        
        ("rust-webpki" ,rust-webpki_0_22_0))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Rustls is a modern TLS library written in Rust.")
    (description
      (beautify-description "Rustls is a modern TLS library written in Rust."))
    (license (list license:asl2.0
               license:isc
               license:expat))))

(define rust-rustls-pemfile_1_0_0
  (package
    (name "rust-rustls-pemfile")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-pemfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1afrjj5l8gw8qm7njwf55nrgb8whqyfq56pyb0a0dzw7wyfjqlp7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_13_0))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description
      (beautify-description "Basic .pem file parser for keys and certificates"))
    (license (list license:asl2.0
               license:isc
               license:expat))))

(define rust-rustversion_1_0_6
  (package
    (name "rust-rustversion")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustversion" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gxj6skypbk0wlbks3pdqb0lclpwbzmyv9xbqkijsvk6zbl3ik7j"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Conditional compilation according to rustc compiler version")
    (description
      (beautify-description "Conditional compilation according to rustc compiler version"))
    (license license:expat)))

(define rust-ryu_1_0_9
  (package
    (name "rust-ryu")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17qlxkqm4h8h9xqj6rh2vnmwxyzikbsj5w223chmr5l2qx8bgd3k"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast floating point to string conversion")
    (description
      (beautify-description "Fast floating point to string conversion"))
    (license license:asl2.0)))

(define rust-same-file_1_0_6
  (package
    (name "rust-same-file")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "same-file" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi-util" ,rust-winapi-util_0_1_5))))
    (home-page "https://github.com/BurntSushi/same-file")
    (synopsis "A simple crate for determining whether two file paths point to the same file.")
    (description
      (beautify-description "A simple crate for determining whether two file paths point to the same file."))
    (license (list license:unlicense
               license:expat))))

(define rust-schannel_0_1_19
  (package
    (name "rust-schannel")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "schannel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xdwr3clrylywpv2r5hw7mrxmsf7ljagwiymw2z60ki3kihbl1cg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl")
    (description
      (beautify-description "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl"))
    (license license:expat)))

(define rust-scheduled-thread-pool_0_2_5
  (package
    (name "rust-scheduled-thread-pool")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scheduled-thread-pool" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mz7s21q1d7xn9j15dlhhv1y86q2r2z6hpax5nh3y1q42byp8vyw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-parking_lot" ,rust-parking_lot_0_11_2))))
    (home-page "None")
    (synopsis "A scheduled thread pool")
    (description
      (beautify-description "A scheduled thread pool"))
    (license (list license:expat
               license:asl2.0))))

(define rust-scoped-tls_1_0_0
  (package
    (name "rust-scoped-tls")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hj8lifzvivdb1z02lfnzkshpvk85nkgzxsy2hc0zky9wf894spa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/scoped-tls")
    (synopsis "Library implementation of the standard library\u0027s old `scoped_thread_local!`\nmacro for providing scoped access to thread local storage (TLS) so any type can\nbe stored into TLS.")
    (description
      (beautify-description "Library implementation of the standard library\u0027s old `scoped_thread_local!`\nmacro for providing scoped access to thread local storage (TLS) so any type can\nbe stored into TLS."))
    (license (list license:expat
               license:asl2.0))))

(define rust-scopeguard_1_1_0
  (package
    (name "rust-scopeguard")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kbqm85v43rq92vx7hfiay6pmcga03vrjbbfwqpyj3pwsg3b16nj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies.")
    (description
      (beautify-description "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies."))
    (license (list license:expat
               license:asl2.0))))

(define rust-sct_0_7_0
  (package
    (name "rust-sct")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sct" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "193w3dg2pcn7138ab4c586pl76nkryn4h6wqlwvqj5gqr6vwsgfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ring" ,rust-ring_0_16_20)        
        ("rust-untrusted" ,rust-untrusted_0_7_1))))
    (home-page "https://github.com/ctz/sct.rs")
    (synopsis "Certificate transparency SCT verification library")
    (description
      (beautify-description "Certificate transparency SCT verification library"))
    (license (list license:asl2.0
               license:isc
               license:expat))))

(define rust-security-framework_2_6_1
  (package
    (name "rust-security-framework")
    (version "2.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p0hgqba3h2glm7mgp5d45l2gpmh28kn5vddlfa032mg5wblzh9d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_1_3_2)        
        ("rust-core-foundation" ,rust-core-foundation_0_9_3)        
        ("rust-core-foundation-sys" ,rust-core-foundation-sys_0_8_3)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-security-framework-sys" ,rust-security-framework-sys_2_6_1))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description
      (beautify-description "Security.framework bindings for macOS and iOS"))
    (license license:expat)))

(define rust-security-framework-sys_2_6_1
  (package
    (name "rust-security-framework-sys")
    (version "2.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mn5lm0jip9nm6ydqm6qd9alyiwq15c027777jsbyibs2wxa2q01"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-core-foundation-sys" ,rust-core-foundation-sys_0_8_3)        
        ("rust-libc" ,rust-libc_0_2_125))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description
      (beautify-description "Apple `Security.framework` low-level FFI bindings"))
    (license license:expat)))

(define rust-semver_0_9_0
  (package
    (name "rust-semver")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-semver-parser" ,rust-semver-parser_0_7_0))))
    (home-page "None")
    (synopsis "Parser and evaluator for Cargo\u0027s flavor of Semantic Versioning")
    (description
      (beautify-description "Parser and evaluator for Cargo\u0027s flavor of Semantic Versioning"))
    (license (list license:expat
               license:asl2.0))))

(define rust-semver-parser_0_7_0
  (package
    (name "rust-semver-parser")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/steveklabnik/semver-parser")
    (synopsis "Parsing of the semver spec.")
    (description
      (beautify-description "Parsing of the semver spec."))
    (license (list license:expat
               license:asl2.0))))

(define rust-serde_1_0_137
  (package
    (name "rust-serde")
    (version "1.0.137")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l8pynxnmld179a33l044yvkigq3fhiwgx0518a1b0vzqxa8vsk1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde_derive" ,rust-serde_derive_1_0_137))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
      (beautify-description "A generic serialization/deserialization framework"))
    (license license:expat)))

(define rust-serde_cbor_0_11_2
  (package
    (name "rust-serde_cbor")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_cbor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xf1bq7ixha30914pd5jl3yw9v1x6car7xgrpimvfvs5vszjxvrb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-half" ,rust-half_1_8_2)        
        ("rust-serde" ,rust-serde_1_0_137))))
    (home-page "None")
    (synopsis "CBOR support for serde.")
    (description
      (beautify-description "CBOR support for serde."))
    (license (list license:expat
               license:asl2.0))))

(define rust-serde_derive_1_0_137
  (package
    (name "rust-serde_derive")
    (version "1.0.137")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gkqhpw86zvppd0lwa8ljzpglwczxq3d7cnkfwirfn9r1jxgl9hz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license license:expat)))

(define rust-serde_json_1_0_81
  (package
    (name "rust-serde_json")
    (version "1.0.81")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p7100hlvw4azgcalzf1vgray5cg6b6saqfwb32h7v8s5ary4z4v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-itoa" ,rust-itoa_1_0_1)        
        ("rust-ryu" ,rust-ryu_1_0_9)        
        ("rust-serde" ,rust-serde_1_0_137))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
      (beautify-description "A JSON serialization file format"))
    (license license:expat)))

(define rust-serde_urlencoded_0_7_1
  (package
    (name "rust-serde_urlencoded")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_urlencoded" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-form_urlencoded" ,rust-form_urlencoded_1_0_1)        
        ("rust-itoa" ,rust-itoa_1_0_1)        
        ("rust-ryu" ,rust-ryu_1_0_9)        
        ("rust-serde" ,rust-serde_1_0_137))))
    (home-page "None")
    (synopsis "`x-www-form-urlencoded` meets Serde")
    (description
      (beautify-description "`x-www-form-urlencoded` meets Serde"))
    (license (list license:expat
               license:asl2.0))))

(define rust-sha-1_0_8_2
  (package
    (name "rust-sha-1")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha-1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pv387q0r7llk2cqzyq0nivzvkgqgzsiygqzlv7b68z9xl5lvngp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer_0_7_3)        
        ("rust-digest" ,rust-digest_0_8_1)        
        ("rust-fake-simd" ,rust-fake-simd_0_1_2)        
        ("rust-opaque-debug" ,rust-opaque-debug_0_2_3))))
    (home-page "None")
    (synopsis "SHA-1 hash function")
    (description
      (beautify-description "SHA-1 hash function"))
    (license license:expat)))

(define rust-sha-1_0_9_8
  (package
    (name "rust-sha-1")
    (version "0.9.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha-1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19jibp8l9k5v4dnhj5kfhaczdfd997h22qz0hin6pw9wvc9ngkcr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer_0_9_0)        
        ("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-cpufeatures" ,rust-cpufeatures_0_2_2)        
        ("rust-opaque-debug" ,rust-opaque-debug_0_3_0))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest_0_9_0))))
    (home-page "None")
    (synopsis "SHA-1 hash function")
    (description
      (beautify-description "SHA-1 hash function"))
    (license license:expat)))

(define rust-sha1_0_6_1
  (package
    (name "rust-sha1")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w1p0s9060cv1vlgfa5c93kjksmvzjjc8j780lns3jj5fk4hbnn1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-sha1_smol" ,rust-sha1_smol_1_0_0))))
    (home-page "None")
    (synopsis "SHA-1 hash function")
    (description
      (beautify-description "SHA-1 hash function"))
    (license license:bsd-3)))

(define rust-sha1_0_10_1
  (package
    (name "rust-sha1")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bw56hxajrgb3pjg0cr5xrvmx0jna39564iw2p14ama5cmzlwzy7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-cpufeatures" ,rust-cpufeatures_0_2_2)        
        ("rust-digest" ,rust-digest_0_10_3))))
    (home-page "None")
    (synopsis "SHA-1 hash function")
    (description
      (beautify-description "SHA-1 hash function"))
    (license license:expat)))

(define rust-sha1_smol_1_0_0
  (package
    (name "rust-sha1_smol")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha1_smol" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04nhbhvsk5ms1zbshs80iq5r1vjszp2xnm9f0ivj38q3dhc4f6mf"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Minimal dependency free implementation of SHA1 for Rust.")
    (description
      (beautify-description "Minimal dependency free implementation of SHA1 for Rust."))
    (license license:bsd-3)))

(define rust-sha2_0_9_9
  (package
    (name "rust-sha2")
    (version "0.9.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "006q2f0ar26xcjxqz8zsncfgz86zqa5dkwlwv03rhx1rpzhs2n2d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer_0_9_0)        
        ("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-cpufeatures" ,rust-cpufeatures_0_2_2)        
        ("rust-opaque-debug" ,rust-opaque-debug_0_3_0))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest_0_9_0))))
    (home-page "None")
    (synopsis "Pure Rust implementation of the SHA-2 hash function family\nincluding SHA-224, SHA-256, SHA-384, and SHA-512.")
    (description
      (beautify-description "Pure Rust implementation of the SHA-2 hash function family\nincluding SHA-224, SHA-256, SHA-384, and SHA-512."))
    (license license:expat)))

(define rust-sha2_0_10_2
  (package
    (name "rust-sha2")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xi6xnqzwaml6d87rpr75a7yin6njbd0pi8drqygxvl11z3axpjm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-cpufeatures" ,rust-cpufeatures_0_2_2)        
        ("rust-digest" ,rust-digest_0_10_3))))
    (home-page "None")
    (synopsis "Pure Rust implementation of the SHA-2 hash function family\nincluding SHA-224, SHA-256, SHA-384, and SHA-512.")
    (description
      (beautify-description "Pure Rust implementation of the SHA-2 hash function family\nincluding SHA-224, SHA-256, SHA-384, and SHA-512."))
    (license license:expat)))

(define rust-sharded-slab_0_1_4
  (package
    (name "rust-sharded-slab")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sharded-slab" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cbb8kgwsyr3zzhsv8jrs3y1j3vsw4jxil42lfq31ikhdy0bl3wh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy_static" ,rust-lazy_static_1_4_0))))
    (home-page "https://github.com/hawkw/sharded-slab")
    (synopsis "A lock-free concurrent slab.")
    (description
      (beautify-description "A lock-free concurrent slab."))
    (license license:expat)))

(define rust-signal-hook-registry_1_4_0
  (package
    (name "rust-signal-hook-registry")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "signal-hook-registry" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c2mhijg54y6c1zi4630yki1vpq3z96ljfnsrdy0rb64ilr767p5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125))))
    (home-page "None")
    (synopsis "Backend crate for signal-hook")
    (description
      (beautify-description "Backend crate for signal-hook"))
    (license (list license:asl2.0
               license:expat))))

(define rust-simple_asn1_0_6_1
  (package
    (name "rust-simple_asn1")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "simple_asn1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iapyiaf0ipwkjivrrmarslycb1pwfmzihmrjk391fdr70f2nxja"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-bigint" ,rust-num-bigint_0_4_3)        
        ("rust-num-traits" ,rust-num-traits_0_2_15)        
        ("rust-thiserror" ,rust-thiserror_1_0_31)        
        ("rust-time" ,rust-time_0_3_9))))
    (home-page "None")
    (synopsis "A simple DER/ASN.1 encoding/decoding library.")
    (description
      (beautify-description "A simple DER/ASN.1 encoding/decoding library."))
    (license license:isc)))

(define rust-siphasher_0_3_10
  (package
    (name "rust-siphasher")
    (version "0.3.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "siphasher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pi5sb2j2wi92zfqj6qxnk11vk1qq2plya5g2a5kzbwrd0hf7lvv"))))
    (build-system cargo-build-system)
    (home-page "https://docs.rs/siphasher")
    (synopsis "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (description
      (beautify-description "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust"))
    (license (list license:expat
               license:asl2.0))))

(define rust-slab_0_4_6
  (package
    (name "rust-slab")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slab" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cmvcy9ppsh3dz8mi6jljx7bxyknvgpas4aid2ayxk1vjpz3qw7b"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description
      (beautify-description "Pre-allocated storage for a uniform data type"))
    (license license:expat)))

(define rust-smallvec_0_6_14
  (package
    (name "rust-smallvec")
    (version "0.6.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q4hz0ssnv24s6fq5kfp2wzrrprrrjiwc42a0h7s7nwym3mwlzxr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-maybe-uninit" ,rust-maybe-uninit_2_0_0))))
    (home-page "None")
    (synopsis "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack"))
    (license (list license:expat
               license:asl2.0))))

(define rust-smallvec_1_8_0
  (package
    (name "rust-smallvec")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10zf4fn63p2d6sx8qap3jvyarcfw563308x3431hd4c34r35gpgj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack"))
    (license (list license:expat
               license:asl2.0))))

(define rust-socket2_0_3_19
  (package
    (name "rust-socket2")
    (version "0.3.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vldz14mxqxnjqb6an2pj7mgclv7nrk45cpscwq7g3fj2c0mfbhj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/rust-lang/socket2")
    (synopsis "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended.")
    (description
      (beautify-description "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended."))
    (license (list license:expat
               license:asl2.0))))

(define rust-socket2_0_4_4
  (package
    (name "rust-socket2")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q71bsw7sqr3nq71gszywgymxxfv311a3w1aia4k5binjisjpmv6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/rust-lang/socket2")
    (synopsis "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended.")
    (description
      (beautify-description "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended."))
    (license license:expat)))

(define rust-spin_0_5_2
  (package
    (name "rust-spin")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b84m6dbzrwf2kxylnw82d3dr8w06av7rfkr8s85fb5f43rwyqvf"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Spin-based synchronization primitives")
    (description
      (beautify-description "Spin-based synchronization primitives"))
    (license license:expat)))

(define rust-spin_0_9_3
  (package
    (name "rust-spin")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "038sza9l5bfq6z80r2cbjihp89iyk40j1zir9cq6k2xzs2qc4c65"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Spin-based synchronization primitives")
    (description
      (beautify-description "Spin-based synchronization primitives"))
    (license license:expat)))

(define rust-stable-pattern_0_1_0
  (package
    (name "rust-stable-pattern")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stable-pattern" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i8hq82vm82mqj02qqcsd7caibrih7x5w3a1xpm8hpv30261cr25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr_2_5_0))))
    (home-page "None")
    (synopsis "Stable port of std::str::Pattern and friends.")
    (description
      (beautify-description "Stable port of std::str::Pattern and friends."))
    (license license:expat)))

(define rust-stable_deref_trait_1_2_0
  (package
    (name "rust-stable_deref_trait")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stable_deref_trait" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An unsafe marker trait for types like Box and Rc that dereference to a stable address even when moved, and hence can be used with libraries such as owning_ref and rental.")
    (description
      (beautify-description "An unsafe marker trait for types like Box and Rc that dereference to a stable address even when moved, and hence can be used with libraries such as owning_ref and rental."))
    (license (list license:expat
               license:asl2.0))))

(define rust-standback_0_2_17
  (package
    (name "rust-standback")
    (version "0.2.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "standback" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zr8zy3kzryaggz3k0j4135m3zbd31pyqmja8cyj8yp07mpzn4z1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "New standard library, old compiler.")
    (description
      (beautify-description "New standard library, old compiler."))
    (license license:expat)))

(define rust-state_0_5_3
  (package
    (name "rust-state")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "state" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fzji31ijbkimbzdy4dln9mp5xp7lm1a0dnqxv4n10hywphnds6v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-loom" ,rust-loom_0_5_4))))
    (home-page "None")
    (synopsis "A library for safe and effortless global and thread-local state management.")
    (description
      (beautify-description "A library for safe and effortless global and thread-local state management."))
    (license (list license:expat
               license:asl2.0))))

(define rust-stdweb_0_4_20
  (package
    (name "rust-stdweb")
    (version "0.4.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1md14n9rzxzdskz3hpgln8vxfwqsw2cswc0f5nslh4r82rmlj8nh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-discard" ,rust-discard_1_0_4)        
        ("rust-rustc_version" ,rust-rustc_version_0_2_3)        
        ("rust-stdweb-derive" ,rust-stdweb-derive_0_5_3)        
        ("rust-stdweb-internal-macros" ,rust-stdweb-internal-macros_0_2_9)        
        ("rust-stdweb-internal-runtime" ,rust-stdweb-internal-runtime_0_1_5)        
        ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_80))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "A standard library for the client-side Web")
    (description
      (beautify-description "A standard library for the client-side Web"))
    (license (list license:expat
               license:asl2.0))))

(define rust-stdweb-derive_0_5_3
  (package
    (name "rust-stdweb-derive")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vsh7g0gaxn4kxqq3knhymdn02p2pfxmnd2j0vplpj6c1yj60yn8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_derive" ,rust-serde_derive_1_0_137)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Derive macros for the `stdweb` crate")
    (description
      (beautify-description "Derive macros for the `stdweb` crate"))
    (license (list license:expat
               license:asl2.0))))

(define rust-stdweb-internal-macros_0_2_9
  (package
    (name "rust-stdweb-internal-macros")
    (version "0.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "049fq8fl5ny9l5if2qv7kxwng7g6ns95h4fbm3zx360dmpv5zyjq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base-x" ,rust-base-x_0_2_10)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_derive" ,rust-serde_derive_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-sha1" ,rust-sha1_0_6_1)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal procedural macros for the `stdweb` crate")
    (description
      (beautify-description "Internal procedural macros for the `stdweb` crate"))
    (license (list license:expat
               license:asl2.0))))

(define rust-stdweb-internal-runtime_0_1_5
  (package
    (name "rust-stdweb-internal-runtime")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-runtime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h0nkppb4r8dbrbms2hw9n5xdcs392m0r5hj3b6lsx3h6fx02dr1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal runtime for the `stdweb` crate")
    (description
      (beautify-description "Internal runtime for the `stdweb` crate"))
    (license (list license:expat
               license:asl2.0))))

(define rust-strsim_0_10_0
  (package
    (name "rust-strsim")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice.")
    (description
      (beautify-description "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice."))
    (license license:expat)))

(define rust-subtle_2_4_1
  (package
    (name "rust-subtle")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "subtle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00b6jzh9gzb0h9n25g06nqr90z3xzqppfhhb260s1hjhh4pg7pkb"))))
    (build-system cargo-build-system)
    (home-page "https://dalek.rs/")
    (synopsis "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (description
      (beautify-description "Pure-Rust traits and utilities for constant-time cryptographic implementations."))
    (license license:bsd-3)))

(define rust-syn_1_0_93
  (package
    (name "rust-syn")
    (version "1.0.93")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wpc7mb6j0h27j4lhcbpd2qm9s9n4jjs2r9dyijyqwlbas4na1h4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-unicode-xid" ,rust-unicode-xid_0_2_3))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
      (beautify-description "Parser for Rust source code"))
    (license license:expat)))

(define rust-syslog_6_0_1
  (package
    (name "rust-syslog")
    (version "6.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syslog" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k0snk06c3gzq8g6kkqvpbbh5zg64nkzdjc303jda2hmd364904p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-error-chain" ,rust-error-chain_0_12_4)        
        ("rust-hostname" ,rust-hostname_0_3_1)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-time" ,rust-time_0_3_9))))
    (home-page "None")
    (synopsis "Send log messages to syslog")
    (description
      (beautify-description "Send log messages to syslog"))
    (license license:expat)))

(define rust-tempfile_3_3_0
  (package
    (name "rust-tempfile")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r3rdp66f7w075mz6blh244syr3h0lbm07ippn7xrbgfxbs1xnsw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-fastrand" ,rust-fastrand_1_7_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-redox_syscall" ,rust-redox_syscall_0_2_13)        
        ("rust-remove_dir_all" ,rust-remove_dir_all_0_5_3)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "http://stebalien.com/projects/tempfile-rs")
    (synopsis "A library for managing temporary files and directories.")
    (description
      (beautify-description "A library for managing temporary files and directories."))
    (license license:expat)))

(define rust-thiserror_1_0_31
  (package
    (name "rust-thiserror")
    (version "1.0.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16h6d602kmjilbfw28zma22wnh03klqba82n4rv7zlkk4girz0mx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-thiserror-impl" ,rust-thiserror-impl_1_0_31))))
    (home-page "None")
    (synopsis "derive(Error)")
    (description
      (beautify-description "derive(Error)"))
    (license license:expat)))

(define rust-thiserror-impl_1_0_31
  (package
    (name "rust-thiserror-impl")
    (version "1.0.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16npm1s1cv9kxkk7is7blnayfnf41hny46gqprc4c916ws4vr5h3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
      (beautify-description "Implementation detail of the `thiserror` crate"))
    (license license:expat)))

(define rust-thread_local_1_1_4
  (package
    (name "rust-thread_local")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread_local" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1001bvz6a688wf3izcrh3jqrkiqaarf44wf08azm071ig1xw45jm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-once_cell" ,rust-once_cell_1_10_0))))
    (home-page "None")
    (synopsis "Per-object thread-local storage")
    (description
      (beautify-description "Per-object thread-local storage"))
    (license (list license:asl2.0
               license:expat))))

(define rust-threadpool_1_8_1
  (package
    (name "rust-threadpool")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "threadpool" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1amgfyzvynbm8pacniivzq9r0fh3chhs7kijic81j76l6c5ycl6h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num_cpus" ,rust-num_cpus_1_13_1))))
    (home-page "https://github.com/rust-threadpool/rust-threadpool")
    (synopsis "A thread pool for running a number of jobs on a fixed set of worker threads.")
    (description
      (beautify-description "A thread pool for running a number of jobs on a fixed set of worker threads."))
    (license (list license:expat
               license:asl2.0))))

(define rust-time_0_1_43
  (package
    (name "rust-time")
    (version "0.1.43")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f14wrgxj7ya2v4msg5mni7046bsm2angm7cn3pd3yv04gpm12na"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_125))
       #:cargo-development-inputs
       (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://time-rs.github.io")
    (synopsis "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      (beautify-description "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]."))
    (license (list license:expat
               license:asl2.0))))

(define rust-time_0_2_27
  (package
    (name "rust-time")
    (version "0.2.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hm209d078jfgxzjhi5xqim64q31rlj1h70zz57qbmpbirzsjlj7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-const_fn" ,rust-const_fn_0_4_9)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-standback" ,rust-standback_0_2_17)        
        ("rust-stdweb" ,rust-stdweb_0_4_20)        
        ("rust-time-macros" ,rust-time-macros_0_1_1)        
        ("rust-version_check" ,rust-version_check_0_9_4)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://time-rs.github.io")
    (synopsis "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      (beautify-description "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]."))
    (license license:expat)))

(define rust-time_0_3_9
  (package
    (name "rust-time")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p8dsb0zwa2r9bz2f31kxfsij6qhmkf1as3ch82z0q58lw42ww62"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-itoa" ,rust-itoa_1_0_1)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-num_threads" ,rust-num_threads_0_1_6)        
        ("rust-quickcheck" ,rust-quickcheck_1_0_3)        
        ("rust-time-macros" ,rust-time-macros_0_2_4))))
    (home-page "https://time-rs.github.io")
    (synopsis "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      (beautify-description "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]."))
    (license license:expat)))

(define rust-time-macros_0_1_1
  (package
    (name "rust-time-macros")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wg24yxpxcfmim6dgblrf8p321m7cyxpdivzvp8bcb7i4rp9qzlm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19)        
        ("rust-time-macros-impl" ,rust-time-macros-impl_0_1_2))))
    (home-page "None")
    (synopsis "Procedural macros for the time crate.")
    (description
      (beautify-description "Procedural macros for the time crate."))
    (license license:expat)))

(define rust-time-macros_0_2_4
  (package
    (name "rust-time-macros")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14h712p63k121cwi80x8ydn99k703wkcw2ksivd7r0addwd7nra2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Procedural macros for the time crate.")
    (description
      (beautify-description "Procedural macros for the time crate."))
    (license license:expat)))

(define rust-time-macros-impl_0_1_2
  (package
    (name "rust-time-macros-impl")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-macros-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bs8xc3qbndk4nw6vwnmh5bwail6vwji4hd1aqzly6a33cd18g7x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-standback" ,rust-standback_0_2_17)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "None")
    (synopsis "Procedural macros for the time crate.")
    (description
      (beautify-description "Procedural macros for the time crate."))
    (license license:expat)))

(define rust-tinyvec_1_6_0
  (package
    (name "rust-tinyvec")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tinyvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l6bl2h62a5m44jdnpn7lmj14rd44via8180i7121fvm73mmrk47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-tinyvec_macros" ,rust-tinyvec_macros_0_1_0))))
    (home-page "None")
    (synopsis "`tinyvec` provides 100% safe vec-like data structures.")
    (description
      (beautify-description "`tinyvec` provides 100% safe vec-like data structures."))
    (license license:asl2.0)))

(define rust-tinyvec_macros_0_1_0
  (package
    (name "rust-tinyvec_macros")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tinyvec_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p5zvgbas5nh403fbxica819mf3g83n8g2hzpfazfr56w6klv9yd"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Some macros for tiny containers")
    (description
      (beautify-description "Some macros for tiny containers"))
    (license license:asl2.0)))

(define rust-tokio_1_18_2
  (package
    (name "rust-tokio")
    (version "1.18.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "158klcakw40y37kgbafg9z1y12vgflh35ad6bbfxss6g4w2by0s9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-libc" ,rust-libc_0_2_125)        
        ("rust-memchr" ,rust-memchr_2_5_0)        
        ("rust-mio" ,rust-mio_0_8_3)        
        ("rust-num_cpus" ,rust-num_cpus_1_13_1)        
        ("rust-once_cell" ,rust-once_cell_1_10_0)        
        ("rust-parking_lot" ,rust-parking_lot_0_12_0)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-signal-hook-registry" ,rust-signal-hook-registry_1_4_0)        
        ("rust-socket2" ,rust-socket2_0_4_4)        
        ("rust-tokio-macros" ,rust-tokio-macros_1_7_0)        
        ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://tokio.rs")
    (synopsis "An event-driven, non-blocking I/O platform for writing asynchronous I/O\nbacked applications.")
    (description
      (beautify-description "An event-driven, non-blocking I/O platform for writing asynchronous I/O\nbacked applications."))
    (license license:expat)))

(define rust-tokio-macros_1_7_0
  (package
    (name "rust-tokio-macros")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ds34qsfvgf63cjgdx3gr4pl7i76fifyar15ksbillcc8hpzfmxm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://tokio.rs")
    (synopsis "Tokio\u0027s proc macros.")
    (description
      (beautify-description "Tokio\u0027s proc macros."))
    (license license:expat)))

(define rust-tokio-native-tls_0_3_0
  (package
    (name "rust-tokio-native-tls")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-native-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yvikgmph2qjq0ni2h2wfaxkzhbnc09c2544av0zidyj1dk9bngp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-native-tls" ,rust-native-tls_0_2_10)        
        ("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "https://tokio.rs")
    (synopsis "An implementation of TLS/SSL streams for Tokio using native-tls giving an implementation of TLS\nfor nonblocking I/O streams.")
    (description
      (beautify-description "An implementation of TLS/SSL streams for Tokio using native-tls giving an implementation of TLS\nfor nonblocking I/O streams."))
    (license license:expat)))

(define rust-tokio-rustls_0_23_4
  (package
    (name "rust-tokio-rustls")
    (version "0.23.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nfsmmi8l1lgpbfy6079d5i13984djzcxrdr9jc06ghi0cwyhgn4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rustls" ,rust-rustls_0_20_4)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-webpki" ,rust-webpki_0_22_0))))
    (home-page "https://github.com/tokio-rs/tls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description
      (beautify-description "Asynchronous TLS/SSL streams for Tokio using Rustls."))
    (license (list license:expat
               license:asl2.0))))

(define rust-tokio-socks_0_5_1
  (package
    (name "rust-tokio-socks")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-socks" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h6qixh17yjf98bjyw4q8i36pf2lyfbcr9hkjjb6aalx0bx5s5ji"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-either" ,rust-either_1_6_1)        
        ("rust-futures-util" ,rust-futures-util_0_3_21)        
        ("rust-thiserror" ,rust-thiserror_1_0_31))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "https://github.com/sticnarf/tokio-socks")
    (synopsis "Asynchronous SOCKS proxy support for Rust.")
    (description
      (beautify-description "Asynchronous SOCKS proxy support for Rust."))
    (license license:expat)))

(define rust-tokio-stream_0_1_8
  (package
    (name "rust-tokio-stream")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-stream" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qwq0y21xprsql4v9y1cm1ymhgw66rznjmnjrjsii27zxy25852h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.")
    (description
      (beautify-description "Utilities to work with `Stream` and `tokio`."))
    (license license:expat)))

(define rust-tokio-util_0_6_9
  (package
    (name "rust-tokio-util")
    (version "0.6.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h2cc3ickn6wj5c0bhw8v5drzrwr5r6n0rjbxgc6qdsx7scf36cy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-sink" ,rust-futures-sink_0_3_21)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.")
    (description
      (beautify-description "Additional utilities for working with Tokio."))
    (license license:expat)))

(define rust-tokio-util_0_7_1
  (package
    (name "rust-tokio-util")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r0p83nisf732qydg23qvmdd6gbrvyr1qvfs8hhbl7a1cyqdxpqf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-futures-core" ,rust-futures-core_0_3_21)        
        ("rust-futures-sink" ,rust-futures-sink_0_3_21)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-tracing" ,rust-tracing_0_1_34))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio_1_18_2))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.")
    (description
      (beautify-description "Additional utilities for working with Tokio."))
    (license license:expat)))

(define rust-toml_0_5_9
  (package
    (name "rust-toml")
    (version "0.5.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mr40c0x3ma0dbzh4v43bfn4sj3k9ihpgq6fz1js88l6fnky30ld"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde_1_0_137))))
    (home-page "https://github.com/alexcrichton/toml-rs")
    (synopsis "A native Rust encoder and decoder of TOML-formatted files and streams. Provides\nimplementations of the standard Serialize/Deserialize traits for TOML data to\nfacilitate deserializing and serializing Rust structures.")
    (description
      (beautify-description "A native Rust encoder and decoder of TOML-formatted files and streams. Provides\nimplementations of the standard Serialize/Deserialize traits for TOML data to\nfacilitate deserializing and serializing Rust structures."))
    (license (list license:expat
               license:asl2.0))))

(define rust-totp-lite_1_0_3
  (package
    (name "rust-totp-lite")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "totp-lite" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12ql4pi9q7sf5651588wia2l5h4mil3kv9jrrkib5gvlpvl0k05i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-digest" ,rust-digest_0_9_0)        
        ("rust-hmac" ,rust-hmac_0_11_0)        
        ("rust-sha-1" ,rust-sha-1_0_9_8)        
        ("rust-sha2" ,rust-sha2_0_9_9))))
    (home-page "https://github.com/fosskers/totp-lite")
    (synopsis "A simple, correct TOTP library.")
    (description
      (beautify-description "A simple, correct TOTP library."))
    (license license:expat)))

(define rust-tower-service_0_3_1
  (package
    (name "rust-tower-service")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-service" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iih764s3f6vlkspfmr72fkrs2lw1v3wiqmc6bd5zq1hdlfzs39n"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Trait representing an asynchronous, request / response based, client or server.")
    (description
      (beautify-description "Trait representing an asynchronous, request / response based, client or server."))
    (license license:expat)))

(define rust-tracing_0_1_34
  (package
    (name "rust-tracing")
    (version "0.1.34")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02bx698j7p50dcg01s3x26swpjs2lcrly32ghklhz7x78k5ws3jx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-pin-project-lite" ,rust-pin-project-lite_0_2_9)        
        ("rust-tracing-attributes" ,rust-tracing-attributes_0_1_21)        
        ("rust-tracing-core" ,rust-tracing-core_0_1_26))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust.")
    (description
      (beautify-description "Application-level tracing for Rust."))
    (license license:expat)))

(define rust-tracing-attributes_0_1_21
  (package
    (name "rust-tracing-attributes")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-attributes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z2bjwkh0azvxw0fqcn36iy7r33wgaq559xp3n5gk6blav9qlsyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93))))
    (home-page "https://tokio.rs")
    (synopsis "Procedural macro attributes for automatically instrumenting functions.")
    (description
      (beautify-description "Procedural macro attributes for automatically instrumenting functions."))
    (license license:expat)))

(define rust-tracing-core_0_1_26
  (package
    (name "rust-tracing-core")
    (version "0.1.26")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bq7c1y28hi7mli25pj9iljam4vcnlqk7zf2k3a8c67822kqqk7m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-valuable" ,rust-valuable_0_1_0))))
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing.")
    (description
      (beautify-description "Core primitives for application-level tracing."))
    (license license:expat)))

(define rust-tracing-log_0_1_3
  (package
    (name "rust-tracing-log")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08prnkxq8yas6jvvjnvyx5v3hwblas5527wxxgbiw2yis8rsvpbq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-tracing-core" ,rust-tracing-core_0_1_26))))
    (home-page "https://tokio.rs")
    (synopsis "Provides compatibility between `tracing` and the `log` crate.")
    (description
      (beautify-description "Provides compatibility between `tracing` and the `log` crate."))
    (license license:expat)))

(define rust-tracing-subscriber_0_3_11
  (package
    (name "rust-tracing-subscriber")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-subscriber" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15i5pyxb0y2y9jhfhd4wlhkmyq51rws3vx76cizh6f7zpa9qzhjb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi_term" ,rust-ansi_term_0_12_1)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-matchers" ,rust-matchers_0_1_0)        
        ("rust-regex" ,rust-regex_1_5_5)        
        ("rust-sharded-slab" ,rust-sharded-slab_0_1_4)        
        ("rust-smallvec" ,rust-smallvec_1_8_0)        
        ("rust-thread_local" ,rust-thread_local_1_1_4)        
        ("rust-tracing" ,rust-tracing_0_1_34)        
        ("rust-tracing-core" ,rust-tracing-core_0_1_26)        
        ("rust-tracing-log" ,rust-tracing-log_0_1_3))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities for implementing and composing `tracing` subscribers.")
    (description
      (beautify-description "Utilities for implementing and composing `tracing` subscribers."))
    (license license:expat)))

(define rust-trust-dns-proto_0_20_4
  (package
    (name "rust-trust-dns-proto")
    (version "0.20.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-proto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cafw8m2488xlr251b0khf6h2d7g4ix0s164j33838dnzvlx956a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait_0_1_53)        
        ("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-data-encoding" ,rust-data-encoding_2_3_2)        
        ("rust-enum-as-inner" ,rust-enum-as-inner_0_3_4)        
        ("rust-futures-channel" ,rust-futures-channel_0_3_21)        
        ("rust-futures-io" ,rust-futures-io_0_3_21)        
        ("rust-futures-util" ,rust-futures-util_0_3_21)        
        ("rust-idna" ,rust-idna_0_2_3)        
        ("rust-ipnet" ,rust-ipnet_2_5_0)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-rand" ,rust-rand_0_8_5)        
        ("rust-smallvec" ,rust-smallvec_1_8_0)        
        ("rust-thiserror" ,rust-thiserror_1_0_31)        
        ("rust-tinyvec" ,rust-tinyvec_1_6_0)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-url" ,rust-url_2_2_2))))
    (home-page "http://www.trust-dns.org/index.html")
    (synopsis "Trust-DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Trust-DNS projects.")
    (description
      (beautify-description "Trust-DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Trust-DNS projects."))
    (license (list license:expat
               license:asl2.0))))

(define rust-trust-dns-resolver_0_20_4
  (package
    (name "rust-trust-dns-resolver")
    (version "0.20.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-resolver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ymprysz8f5qjaj74x488pjhbwy329yybs2clgx5x6frm8xkibpc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-futures-util" ,rust-futures-util_0_3_21)        
        ("rust-ipconfig" ,rust-ipconfig_0_2_2)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-lru-cache" ,rust-lru-cache_0_1_2)        
        ("rust-parking_lot" ,rust-parking_lot_0_11_2)        
        ("rust-resolv-conf" ,rust-resolv-conf_0_7_0)        
        ("rust-smallvec" ,rust-smallvec_1_8_0)        
        ("rust-thiserror" ,rust-thiserror_1_0_31)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-trust-dns-proto" ,rust-trust-dns-proto_0_20_4))))
    (home-page "http://www.trust-dns.org/index.html")
    (synopsis "Trust-DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries.")
    (description
      (beautify-description "Trust-DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries."))
    (license (list license:expat
               license:asl2.0))))

(define rust-try-lock_0_2_3
  (package
    (name "rust-try-lock")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "try-lock" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hkn1ksmg5hdqgqdw1ahy5qk69f4crh2psf0v61qphyrf777nm2r"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/seanmonstar/try-lock")
    (synopsis "A lightweight atomic lock.")
    (description
      (beautify-description "A lightweight atomic lock."))
    (license license:expat)))

(define rust-typenum_1_15_0
  (package
    (name "rust-typenum")
    (version "1.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typenum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11yrvz1vd43gqv738yw1v75rzngjbs7iwcgzjy3cq5ywkv2imy6w"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Typenum is a Rust library for type-level numbers evaluated at\n    compile time. It currently supports bits, unsigned integers, and signed\n    integers. It also provides a type-level array of type-level numbers, but its\n    implementation is incomplete.")
    (description
      (beautify-description "Typenum is a Rust library for type-level numbers evaluated at\n    compile time. It currently supports bits, unsigned integers, and signed\n    integers. It also provides a type-level array of type-level numbers, but its\n    implementation is incomplete."))
    (license license:expat)))

(define rust-ubyte_0_10_1
  (package
    (name "rust-ubyte")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ubyte" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zp8x55w57dkcy20vnc50izsjcgz7mj9b0d9z3i5v188wywnnxa2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde_1_0_137))))
    (home-page "None")
    (synopsis "A simple, complete, const-everything, saturating, human-friendly, no_std library for byte units.")
    (description
      (beautify-description "A simple, complete, const-everything, saturating, human-friendly, no_std library for byte units."))
    (license license:expat)))

(define rust-ucd-trie_0_1_3
  (package
    (name "rust-ucd-trie")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-trie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "072cblf8v3wzyaz3lhbpzgil4s03dpzg1ppy3gqx2l4v622y3pjn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "A trie for storing Unicode codepoint sets and maps.")
    (description
      (beautify-description "A trie for storing Unicode codepoint sets and maps."))
    (license (list license:expat
               license:asl2.0))))

(define rust-uncased_0_9_6
  (package
    (name "rust-uncased")
    (version "0.9.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uncased" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l3flz044hfdnsddahj08dflqprfydszkm4vkf458l724xryvbjv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde_1_0_137)        
        ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "Case-preserving, ASCII case-insensitive, no_std string types.")
    (description
      (beautify-description "Case-preserving, ASCII case-insensitive, no_std string types."))
    (license license:expat)))

(define rust-unicode-bidi_0_3_8
  (package
    (name "rust-unicode-bidi")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-bidi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14p95n9kw9p7psp0vsp0j9yfkfg6sn1rlnymvmwmya0x60l736q9"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Implementation of the Unicode Bidirectional Algorithm")
    (description
      (beautify-description "Implementation of the Unicode Bidirectional Algorithm"))
    (license license:expat)))

(define rust-unicode-normalization_0_1_19
  (package
    (name "rust-unicode-normalization")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-normalization" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yabhmg8zlcksda3ajly9hpbzqgbhknxwch8dwkfkaa1569r0ifm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-tinyvec" ,rust-tinyvec_1_6_0))))
    (home-page "https://github.com/unicode-rs/unicode-normalization")
    (synopsis "This crate provides functions for normalization of\nUnicode strings, including Canonical and Compatible\nDecomposition and Recomposition, as described in\nUnicode Standard Annex #15.")
    (description
      (beautify-description "This crate provides functions for normalization of\nUnicode strings, including Canonical and Compatible\nDecomposition and Recomposition, as described in\nUnicode Standard Annex #15."))
    (license (list license:expat
               license:asl2.0))))

(define rust-unicode-xid_0_2_3
  (package
    (name "rust-unicode-xid")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "015zxvwk0is9ls3v016krn5gpr5rk5smyzg6c9j58439ckrm2zlm"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31.")
    (description
      (beautify-description "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31."))
    (license license:expat)))

(define rust-universal-hash_0_4_1
  (package
    (name "rust-universal-hash")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "universal-hash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01av09i0rqcl8f0xgvn2g07kzyafgbiwdhkfwq0m14kyd67lw8cz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-generic-array" ,rust-generic-array_0_14_5)        
        ("rust-subtle" ,rust-subtle_2_4_1))))
    (home-page "None")
    (synopsis "Trait for universal hash functions")
    (description
      (beautify-description "Trait for universal hash functions"))
    (license license:expat)))

(define rust-untrusted_0_7_1
  (package
    (name "rust-untrusted")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "untrusted" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jkbqaj9d3v5a91pp3wp9mffvng1nhycx6sh4qkdd9qyr62ccmm1"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted inputs in Rust.")
    (description
      (beautify-description "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted inputs in Rust."))
    (license license:isc)))

(define rust-url_1_7_2
  (package
    (name "rust-url")
    (version "1.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nim1c90mxpi9wgdw2xh8dqd72vlklwlzam436akcrhjac6pqknx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-idna" ,rust-idna_0_1_5)        
        ("rust-matches" ,rust-matches_0_1_9)        
        ("rust-percent-encoding" ,rust-percent-encoding_1_0_1))))
    (home-page "None")
    (synopsis "URL library for Rust, based on the WHATWG URL Standard")
    (description
      (beautify-description "URL library for Rust, based on the WHATWG URL Standard"))
    (license (list license:expat
               license:asl2.0))))

(define rust-url_2_2_2
  (package
    (name "rust-url")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "132pzpvfvpw33gjlzqd55n5iag9qddzffq8qbp1myfykna1w61x5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-form_urlencoded" ,rust-form_urlencoded_1_0_1)        
        ("rust-idna" ,rust-idna_0_2_3)        
        ("rust-matches" ,rust-matches_0_1_9)        
        ("rust-percent-encoding" ,rust-percent-encoding_2_1_0)        
        ("rust-serde" ,rust-serde_1_0_137))))
    (home-page "None")
    (synopsis "URL library for Rust, based on the WHATWG URL Standard")
    (description
      (beautify-description "URL library for Rust, based on the WHATWG URL Standard"))
    (license (list license:expat
               license:asl2.0))))

(define rust-uuid_1_0_0
  (package
    (name "rust-uuid")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uuid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w668dw9jq0dz24smh1v3w3ilzi6fcs45vc702hnwkbc8lcx7z4c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom_0_2_6))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
      (beautify-description "A library to generate and parse UUIDs."))
    (license license:asl2.0)))

(define rust-valuable_0_1_0
  (package
    (name "rust-valuable")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "valuable" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v9gp3nkjbl30z0fd56d8mx7w1csk86wwjhfjhr400wh9mfpw2w3"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Object-safe value inspection, used to pass un-typed structured data across trait-object boundaries.")
    (description
      (beautify-description "Object-safe value inspection, used to pass un-typed structured data across trait-object boundaries."))
    (license license:expat)))

(define rust-vaultwarden_1_25_0
  (package
    (name "vaultwarden")
    (version "1.25.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dani-garcia/vaultwarden")
               (commit version)))
        (file-name (git-file-name name version))
        (modules '((guix build utils)))
        (snippet 
          #~(begin 
              (substitute* "Cargo.toml"
                (("job_scheduler = \\{.*")
                 (string-append "job_scheduler = { path = \"" #$(package-source rust-job_scheduler_1_2_1) "\" }")))))
        (sha256
          (base32 
            "18y7x0csvydfwjv2h6g7d8afdcyb8lqvfiqhlxjss8r99h42xfmk"))))
    (build-system cargo-build-system)
    (native-inputs (list perl))
    (arguments
     `(#:phases (modify-phases %standard-phases
                               (delete 'check))
       #:features '("sqlite" "vendored_openssl")
       #:rust ,rust-nightly-1.60
       #:cargo-inputs
       (("rust-backtrace" ,rust-backtrace_0_3_65)        
        ("rust-bytes" ,rust-bytes_1_1_0)        
        ("rust-cached" ,rust-cached_0_34_0)        
        ("rust-chashmap" ,rust-chashmap_2_2_2)        
        ("rust-chrono" ,rust-chrono_0_4_19)        
        ("rust-chrono-tz" ,rust-chrono-tz_0_6_1)        
        ("rust-cookie" ,rust-cookie_0_16_0)        
        ("rust-cookie_store" ,rust-cookie_store_0_16_0)        
        ("rust-ctrlc" ,rust-ctrlc_3_2_2)        
        ("rust-data-encoding" ,rust-data-encoding_2_3_2)        
        ("rust-data-url" ,rust-data-url_0_1_1)        
        ("rust-diesel" ,rust-diesel_1_4_8)        
        ("rust-diesel_migrations" ,rust-diesel_migrations_1_4_0)        
        ("rust-dotenvy" ,rust-dotenvy_0_15_1)        
        ("rust-fern" ,rust-fern_0_6_1)        
        ("rust-futures" ,rust-futures_0_3_21)        
        ("rust-governor" ,rust-governor_0_4_2)        
        ("rust-handlebars" ,rust-handlebars_4_2_2)        
        ("rust-html5gum" ,rust-html5gum_0_4_0)        
        ("rust-idna" ,rust-idna_0_2_3)        
        ("rust-rand_hc" ,rust-rand_hc_0_2_0)
        ("rust-job_scheduler-other" ,rust-job_scheduler_1_2_1-tarball)        
; we need to pull in deps from job_scheduler
        ("rust-cron" ,rust-cron_0_11_0)        
        ("rust-jsonwebtoken" ,rust-jsonwebtoken_8_1_0)        
        ("rust-lettre" ,rust-lettre_0_10_0-rc_6)        
        ("rust-libsqlite3-sys" ,rust-libsqlite3-sys_0_22_2)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-mimalloc" ,rust-mimalloc_0_1_29)        
        ("rust-num-derive" ,rust-num-derive_0_3_3)        
        ("rust-num-traits" ,rust-num-traits_0_2_15)        
        ("rust-once_cell" ,rust-once_cell_1_10_0)        
        ("rust-openssl" ,rust-openssl_0_10_40)        
        ("rust-parity-ws" ,rust-parity-ws_0_11_1)        
        ("rust-paste" ,rust-paste_1_0_7)        
        ("rust-percent-encoding" ,rust-percent-encoding_2_1_0)        
        ("rust-pico-args" ,rust-pico-args_0_4_2)        
        ("rust-rand" ,rust-rand_0_8_5)        
        ("rust-regex" ,rust-regex_1_5_5)        
        ("rust-reqwest" ,rust-reqwest_0_11_10)        
        ("rust-ring" ,rust-ring_0_16_20)        
        ("rust-rmpv" ,rust-rmpv_1_0_0)        
        ("rust-rocket" ,rust-rocket_0_5_0-rc_2)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-syslog" ,rust-syslog_6_0_1)        
        ("rust-time" ,rust-time_0_3_9)        
        ("rust-tokio" ,rust-tokio_1_18_2)        
        ("rust-totp-lite" ,rust-totp-lite_1_0_3)        
        ("rust-tracing" ,rust-tracing_0_1_34)        
        ("rust-url" ,rust-url_2_2_2)        
        ("rust-uuid" ,rust-uuid_1_0_0)        
        ("rust-webauthn-rs" ,rust-webauthn-rs_0_3_2)        
        ("rust-yubico" ,rust-yubico_0_11_0))))
    (home-page "https://github.com/dani-garcia/vaultwarden")
    (synopsis "")
    (description
      (beautify-description ""))
    (license license:gpl3)))

(define rust-vcpkg_0_2_15
  (package
    (name "rust-vcpkg")
    (version "0.2.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vcpkg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library to find native dependencies in a vcpkg tree at build\ntime in order to be used in Cargo build scripts.")
    (description
      (beautify-description "A library to find native dependencies in a vcpkg tree at build\ntime in order to be used in Cargo build scripts."))
    (license (list license:expat
               license:asl2.0))))

(define rust-version_check_0_9_4
  (package
    (name "rust-version_check")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Tiny crate to check the version of the installed/running rustc.")
    (description
      (beautify-description "Tiny crate to check the version of the installed/running rustc."))
    (license (list license:expat
               license:asl2.0))))

(define rust-walkdir_2_3_2
  (package
    (name "rust-walkdir")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "walkdir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mnszy33685v8y9js8mw6x2p3iddqs8vfj7n2dhqddnlbirz5340"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-same-file" ,rust-same-file_1_0_6)        
        ("rust-winapi" ,rust-winapi_0_3_9)        
        ("rust-winapi-util" ,rust-winapi-util_0_1_5))))
    (home-page "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory.")
    (description
      (beautify-description "Recursively walk a directory."))
    (license (list license:unlicense
               license:expat))))

(define rust-want_0_3_0
  (package
    (name "rust-want")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "want" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "181b2zmwfq389x9n2g1n37cvcvvdand832zz6v8i1l8wrdlaks0w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log_0_4_17)        
        ("rust-try-lock" ,rust-try-lock_0_2_3))))
    (home-page "None")
    (synopsis "Detect when another Future wants a result.")
    (description
      (beautify-description "Detect when another Future wants a result."))
    (license license:expat)))

(define rust-wasi_0_9_0+wasi-snapshot-preview1
  (package
    (name "rust-wasi")
    (version "0.9.0+wasi-snapshot-preview1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06g5v3vrdapfzvfq662cij7v8a1flwr2my45nnncdv2galrdzkfc"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Experimental WASI API bindings for Rust")
    (description
      (beautify-description "Experimental WASI API bindings for Rust"))
    (license license:asl2.0)))

(define rust-wasi_0_10_2+wasi-snapshot-preview1
  (package
    (name "rust-wasi")
    (version "0.10.2+wasi-snapshot-preview1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ii7nff4y1mpcrxzzvbpgxm7a1nn3szjf1n21jnx37c2g6dbsvzx"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Experimental WASI API bindings for Rust")
    (description
      (beautify-description "Experimental WASI API bindings for Rust"))
    (license license:asl2.0)))

(define rust-wasi_0_11_0+wasi-snapshot-preview1
  (package
    (name "rust-wasi")
    (version "0.11.0+wasi-snapshot-preview1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Experimental WASI API bindings for Rust")
    (description
      (beautify-description "Experimental WASI API bindings for Rust"))
    (license license:asl2.0)))

(define rust-wasm-bindgen_0_2_80
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.80")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b88vhwy2fdskh04wck82d2yczwk9vs2dglz38zmxi87r6bh2dr7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro_0_2_80))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.")
    (description
      (beautify-description "Easy support for interacting between JS and Rust."))
    (license (list license:expat
               license:asl2.0))))

(define rust-wasm-bindgen-backend_0_2_80
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.80")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x6kfhvvy74smy2qjd1zaw4q8f9kbq1gacm57lkpk9x3py2l3q2k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bumpalo" ,rust-bumpalo_3_9_1)        
        ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
        ("rust-log" ,rust-log_0_4_17)        
        ("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93)        
        ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared_0_2_80))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool")
    (description
      (beautify-description "Backend code generation of the wasm-bindgen tool"))
    (license (list license:expat
               license:asl2.0))))

(define rust-wasm-bindgen-futures_0_4_30
  (package
    (name "rust-wasm-bindgen-futures")
    (version "0.4.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-futures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cpkkcjh2pkvsgf00kaxz8aa2fmp3rgzysl8vwslrqbm9gj1sx3g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
        ("rust-js-sys" ,rust-js-sys_0_3_57)        
        ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_80)        
        ("rust-web-sys" ,rust-web-sys_0_3_57))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bridging the gap between Rust Futures and JavaScript Promises")
    (description
      (beautify-description "Bridging the gap between Rust Futures and JavaScript Promises"))
    (license (list license:expat
               license:asl2.0))))

(define rust-wasm-bindgen-macro_0_2_80
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.80")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xaihb09jxnwld8bipj93c8k9w7ccvkwy4bnzsi86zjdg3zygjhp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quote" ,rust-quote_1_0_18)        
        ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support_0_2_80))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (description
      (beautify-description "Definition of the `#[wasm_bindgen]` attribute, an internal dependency"))
    (license (list license:expat
               license:asl2.0))))

(define rust-wasm-bindgen-macro-support_0_2_80
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.80")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro-support" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02z2xjrwkq1ajmzv42irj79krlkqyprbkcda671gyvvmlk3hvv4r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_38)        
        ("rust-quote" ,rust-quote_1_0_18)        
        ("rust-syn" ,rust-syn_1_0_93)        
        ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend_0_2_80)        
        ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared_0_2_80))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate")
    (description
      (beautify-description "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate"))
    (license (list license:expat
               license:asl2.0))))

(define rust-wasm-bindgen-shared_0_2_80
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.80")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i37ljck1c6l4xypx042ybjcm2lb3xfdjs4lk96rdrfy63svfm6m"))))
    (build-system cargo-build-system)
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Shared support between wasm-bindgen and wasm-bindgen cli, an internal\ndependency.")
    (description
      (beautify-description "Shared support between wasm-bindgen and wasm-bindgen cli, an internal\ndependency."))
    (license (list license:expat
               license:asl2.0))))

(define rust-web-sys_0_3_57
  (package
    (name "rust-web-sys")
    (version "0.3.57")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "web-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10y237lkvxg6w5p39y1bqck2qjiivqc5np5c4jywhw1ccr0yf5vv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-js-sys" ,rust-js-sys_0_3_57)        
        ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_80))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/web-sys/index.html")
    (synopsis "Bindings for all Web APIs, a procedurally generated crate from WebIDL")
    (description
      (beautify-description "Bindings for all Web APIs, a procedurally generated crate from WebIDL"))
    (license (list license:expat
               license:asl2.0))))

(define rust-webauthn-rs_0_3_2
  (package
    (name "rust-webauthn-rs")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webauthn-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hbfz6krpv88ph3wlwnzn6h6sldh8fj3xizmfrc5jcjbrgn6dclh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_13_0)        
        ("rust-nom" ,rust-nom_7_1_1)        
        ("rust-openssl" ,rust-openssl_0_10_40)        
        ("rust-rand" ,rust-rand_0_8_5)        
        ("rust-serde" ,rust-serde_1_0_137)        
        ("rust-serde_cbor" ,rust-serde_cbor_0_11_2)        
        ("rust-serde_derive" ,rust-serde_derive_1_0_137)        
        ("rust-serde_json" ,rust-serde_json_1_0_81)        
        ("rust-thiserror" ,rust-thiserror_1_0_31)        
        ("rust-tracing" ,rust-tracing_0_1_34)        
        ("rust-url" ,rust-url_2_2_2))))
    (home-page "None")
    (synopsis "Webauthn Framework for Rust Web Servers")
    (description
      (beautify-description "Webauthn Framework for Rust Web Servers"))
    (license license:mpl2.0)))

(define rust-webpki_0_22_0
  (package
    (name "rust-webpki")
    (version "0.22.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gd1gxip5kgdwmrvhj5gjxij2mgg2mavq1ych4q1h272ja0xg5gh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ring" ,rust-ring_0_16_20)        
        ("rust-untrusted" ,rust-untrusted_0_7_1))))
    (home-page "None")
    (synopsis "Web PKI X.509 Certificate Verification.")
    (description
      (beautify-description "Web PKI X.509 Certificate Verification."))
    (license #f)))

(define rust-widestring_0_4_3
  (package
    (name "rust-widestring")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "widestring" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z3ba8qrxb62vpfgk7n2xs2grm9kdaj1cz4q8s0gs8fx8h0r8s61"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A wide string Rust library for converting to and from wide strings, such as those often used in Windows API or other FFI libaries. Both `u16` and `u32` string types are provided, including support for UTF-16 and UTF-32, malformed encoding, C-style strings, etc.")
    (description
      (beautify-description "A wide string Rust library for converting to and from wide strings, such as those often used in Windows API or other FFI libaries. Both `u16` and `u32` string types are provided, including support for UTF-16 and UTF-32, malformed encoding, C-style strings, etc."))
    (license (list license:expat
               license:asl2.0))))

(define rust-winapi_0_2_8
  (package
    (name "rust-winapi")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yh816lh6lf56dpsgxy189c2ai1z3j8mw9si6izqb6wsjkbcjz8n"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Raw FFI bindings for all of Windows API.")
    (description
      (beautify-description "Raw FFI bindings for all of Windows API."))
    (license license:expat)))

(define rust-winapi_0_3_9
  (package
    (name "rust-winapi")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi-i686-pc-windows-gnu" ,rust-winapi-i686-pc-windows-gnu_0_4_0)        
        ("rust-winapi-x86_64-pc-windows-gnu" ,rust-winapi-x86_64-pc-windows-gnu_0_4_0))))
    (home-page "None")
    (synopsis "Raw FFI bindings for all of Windows API.")
    (description
      (beautify-description "Raw FFI bindings for all of Windows API."))
    (license (list license:expat
               license:asl2.0))))

(define rust-winapi-build_0_1_1
  (package
    (name "rust-winapi-build")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-build" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Common code for build.rs in WinAPI -sys crates.")
    (description
      (beautify-description "Common code for build.rs in WinAPI -sys crates."))
    (license license:expat)))

(define rust-winapi-i686-pc-windows-gnu_0_4_0
  (package
    (name "rust-winapi-i686-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-i686-pc-windows-gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import libraries for the i686-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the i686-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead."))
    (license (list license:expat
               license:asl2.0))))

(define rust-winapi-util_0_1_5
  (package
    (name "rust-winapi-util")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y71bp7f6d536czj40dhqk0d55wfbbwqfp2ymqf1an5ibgl6rv3h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "A dumping ground for high level safe wrappers over winapi.")
    (description
      (beautify-description "A dumping ground for high level safe wrappers over winapi."))
    (license (list license:unlicense
               license:expat))))

(define rust-winapi-x86_64-pc-windows-gnu_0_4_0
  (package
    (name "rust-winapi-x86_64-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-x86_64-pc-windows-gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the x86_64-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead."))
    (license (list license:expat
               license:asl2.0))))

(define rust-windows-sys_0_36_1
  (package
    (name "rust-windows-sys")
    (version "0.36.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lmqangv0zg1l46xiq7rfnqwsx8f8m52mqbgg2mrx7x52rd1a17a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-windows_aarch64_msvc" ,rust-windows_aarch64_msvc_0_36_1)        
        ("rust-windows_i686_gnu" ,rust-windows_i686_gnu_0_36_1)        
        ("rust-windows_i686_msvc" ,rust-windows_i686_msvc_0_36_1)        
        ("rust-windows_x86_64_gnu" ,rust-windows_x86_64_gnu_0_36_1)        
        ("rust-windows_x86_64_msvc" ,rust-windows_x86_64_msvc_0_36_1))))
    (home-page "None")
    (synopsis "Rust for Windows")
    (description
      (beautify-description "Rust for Windows"))
    (license license:expat)))

(define rust-windows_aarch64_msvc_0_36_1
  (package
    (name "rust-windows_aarch64_msvc")
    (version "0.36.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_aarch64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ixaxs2c37ll2smprzh0xq5p238zn8ylzb3lk1zddqmd77yw7f4v"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Code gen support for the windows crate")
    (description
      (beautify-description "Code gen support for the windows crate"))
    (license license:expat)))

(define rust-windows_i686_gnu_0_36_1
  (package
    (name "rust-windows_i686_gnu")
    (version "0.36.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dm3svxfzamrv6kklyda9c3qylgwn5nwdps6p0kc9x6s077nq3hq"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Code gen support for the windows crate")
    (description
      (beautify-description "Code gen support for the windows crate"))
    (license license:expat)))

(define rust-windows_i686_msvc_0_36_1
  (package
    (name "rust-windows_i686_msvc")
    (version "0.36.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "097h2a7wig04wbmpi3rz1akdy4s8gslj5szsx8g2v0dj91qr3rz2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Code gen support for the windows crate")
    (description
      (beautify-description "Code gen support for the windows crate"))
    (license license:expat)))

(define rust-windows_x86_64_gnu_0_36_1
  (package
    (name "rust-windows_x86_64_gnu")
    (version "0.36.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qfrck3jnihymfrd01s8260d4snql8ks2p8yaabipi3nhwdigkad"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Code gen support for the windows crate")
    (description
      (beautify-description "Code gen support for the windows crate"))
    (license license:expat)))

(define rust-windows_x86_64_msvc_0_36_1
  (package
    (name "rust-windows_x86_64_msvc")
    (version "0.36.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "103n3xijm5vr7qxr1dps202ckfnv7njjnnfqmchg8gl5ii5cl4f8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Code gen support for the windows crate")
    (description
      (beautify-description "Code gen support for the windows crate"))
    (license license:expat)))

(define rust-winreg_0_6_2
  (package
    (name "rust-winreg")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winreg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jdcqr6zmvwyrp87h48miasfdvv16gjsb60rc8dy2kqwb3mnv65j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "Rust bindings to MS Windows Registry API")
    (description
      (beautify-description "Rust bindings to MS Windows Registry API"))
    (license license:expat)))

(define rust-winreg_0_10_1
  (package
    (name "rust-winreg")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winreg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17c6h02z88ijjba02bnxi5k94q5cz490nf3njh9yypf8fbig9l40"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "Rust bindings to MS Windows Registry API")
    (description
      (beautify-description "Rust bindings to MS Windows Registry API"))
    (license license:expat)))

(define rust-ws2_32-sys_0_2_1
  (package
    (name "rust-ws2_32-sys")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ws2_32-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi_0_2_8)        
        ("rust-winapi-build" ,rust-winapi-build_0_1_1))))
    (home-page "None")
    (synopsis "Contains function definitions for the Windows API library ws2_32. See winapi for types and constants.")
    (description
      (beautify-description "Contains function definitions for the Windows API library ws2_32. See winapi for types and constants."))
    (license license:expat)))

(define rust-yansi_0_5_1
  (package
    (name "rust-yansi")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "yansi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v4qljgzh73knr7291cgwrf56zrvhmpn837n5n5pypzq1kciq109"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A dead simple ANSI terminal color painting library.")
    (description
      (beautify-description "A dead simple ANSI terminal color painting library."))
    (license (list license:expat
               license:asl2.0))))

(define rust-yubico_0_11_0
  (package
    (name "rust-yubico")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "yubico" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yymc14rl9axqyqrfk0s5fgcac4rlqaa3qsasyi2j101qk97agqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64_0_13_0)        
        ("rust-form_urlencoded" ,rust-form_urlencoded_1_0_1)        
        ("rust-futures" ,rust-futures_0_3_21)        
        ("rust-hmac" ,rust-hmac_0_12_1)        
        ("rust-rand" ,rust-rand_0_8_5)        
        ("rust-reqwest" ,rust-reqwest_0_11_10)        
        ("rust-sha1" ,rust-sha1_0_10_1)        
        ("rust-threadpool" ,rust-threadpool_1_8_1))))
    (home-page "None")
    (synopsis "Yubikey client API library")
    (description
      (beautify-description "Yubikey client API library"))
    (license license:expat)))

(define-public vaultwarden rust-vaultwarden_1_25_0)

(define-public web-vault
  (let* ((version "v2022.6.0"))
    (package
      (name "web-vault")
      (version version)
      (source
        (origin
          (method url-fetch)
          (uri (string-append "https://github.com/dani-garcia/bw_web_builds/releases/download/" version "/bw_web_" version ".tar.gz"))
          (sha256
            (base32
              "1fq7f2224dqiidiq0kw1lzhhwiqdk8dd3gjhaqwvqb72rr3z34px"))))
      (build-system copy-build-system)
      (home-page "https://github.com/dani-garcia/bw_web_builds")
      (synopsis "Web vault builds for vaultwarden")
      (description "Web vault builds for vaultwarden")
      (license license:gpl3))))

vaultwarden
