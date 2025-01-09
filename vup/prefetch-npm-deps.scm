(define-module (vup prefetch-npm-deps)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix import utils) #:select (beautify-description)))

(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define nixpkgs-version "24.11-pre")

(define nixpkgs
  (let ((version nixpkgs-version))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/nixos/nixpkgs")
            (commit version)))
      (file-name (string-append "nixpkgs-" version "-checkout"))
      (sha256
       (base32 "011gzqngzrwggg1dilj6wr6194adjy4xd0adj3hq5zfgavh0z5s7")))))

(define-public rust-symlink-0.1
  (package
    (name "rust-symlink")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symlink" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02h1i0b81mxb4vns4xrvrfibpcvs7jqqav8p3yilwik8cv73r5x7"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.com/chris-morgan/symlink")
    (synopsis "Create symlinks in a cross-platform manner")
    (description
     "This package provides Create symlinks in a cross-platform manner.")
    (license (list license:expat license:asl2.0))))

(define-public prefetch-npm-deps
  (package
    (name "prefetch-npm-deps")
    (version nixpkgs-version)
    (source
     (origin
       (method computed-origin-method)
       (file-name (string-append "prefetch-npm-deps-" version))
       (sha256 #f)
       (uri
        (delay
          (with-imported-modules '((guix build utils))
            #~(begin
                (use-modules (guix build utils))
                (mkdir #$output)
                (copy-recursively
                 (string-append #$nixpkgs "/pkgs/build-support/node/fetch-npm-deps")
                 #$output)
                ))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow_1_0_82)
        ("rust-backoff" ,rust-backoff_0_4_0)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-env_logger" ,rust-env_logger_0_11_3)
        ("rust-isahc" ,rust-isahc_1_7_2)
        ("rust-log" ,rust-log_0_4_21)
        ("rust-nix-nar" ,rust-nix-nar_0_3_0)
        ("rust-rayon" ,rust-rayon_1_10_0)
        ("rust-serde" ,rust-serde_1_0_198)
        ("rust-serde_json" ,rust-serde_json_1_0_116)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-tempfile" ,rust-tempfile_3_10_1)
        ("rust-url" ,rust-url-2)
        ("rust-walkdir" ,rust-walkdir_2_5_0))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl nghttp2 openssl zlib))
    (home-page "FILLMEIN")
    (synopsis "FILLMEIN")
    (description
     (beautify-description "FILLMEIN"))
    (license #f)))

(define-public prefetch-npm-deps-bin (file-append prefetch-npm-deps "/bin/prefetch-npm-deps"))

(define rust-nix-nar_0_3_0
  (package
    (name "rust-nix-nar")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nix-nar" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1d950f6nrx33jpfilxynrg4n1nspqnyg86bas3yc8ydim1c92m6m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-camino" ,rust-camino-1)
        ("rust-is_executable" ,rust-is_executable_1_0_1)
        ("rust-symlink" ,rust-symlink-0.1)
        ("rust-thiserror" ,rust-thiserror_1_0_58))))
    (home-page "https://gitlab.com/abstract-binary/nix-nar-rs")
    (synopsis "Library to manipulate Nix Archive (nar) files")
    (description
     (beautify-description "Library to manipulate Nix Archive (nar) files"))
    (license (list license:asl2.0 license:lgpl2.1+))))

(define rust-isahc_1_7_2
  (package
    (name "rust-isahc")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "isahc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1scfgyv3dpjbkqa9im25cd12cs6rbd8ygcaw67f3dx41sys08kik"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-channel" ,rust-async-channel-1)
        ("rust-castaway" ,rust-castaway_0_1_2)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-curl" ,rust-curl_0_4_46)
        ("rust-curl-sys" ,rust-curl-sys_0_4_72+curl-8_6_0)
        ("rust-event-listener" ,rust-event-listener-2)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-http" ,rust-http_0_2_12)
        ("rust-log" ,rust-log_0_4_21)
        ("rust-once_cell" ,rust-once_cell_1_19_0)
        ("rust-polling" ,rust-polling-2)
        ("rust-slab" ,rust-slab_0_4_9)
        ("rust-sluice" ,rust-sluice_0_5_5)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-futures" ,rust-tracing-futures_0_2_5)
        ("rust-url" ,rust-url-2)
        ("rust-waker-fn" ,rust-waker-fn-1))))
    (home-page "None")
    (synopsis "The practical HTTP client that is fun to use.")
    (description
     (beautify-description "The practical HTTP client that is fun to use."))
    (license (list license:expat))))

(define rust-log_0_4_21
  (package
    (name "rust-log")
    (version "0.4.21")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "log" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "074hldq1q8rlzq2s2qa8f25hj4s3gpw71w64vdwzjd01a4g8rvch"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A lightweight logging facade for Rust")
    (description
     (beautify-description "A lightweight logging facade for Rust"))
    (license (list license:expat license:asl2.0))))

(define rust-env_logger_0_11_3
  (package
    (name "rust-env_logger")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_logger" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fa34dr082zfih5pw821d13kr6lcg18x6z08pa09d0aip8wmicrq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anstream" ,rust-anstream_0_6_13)
        ("rust-anstyle" ,rust-anstyle-1)
        ("rust-env_filter" ,rust-env_filter_0_1_0)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-log" ,rust-log_0_4_21))))
    (home-page "None")
    (synopsis "A logging implementation for `log` which is configured via an environment\nvariable.")
    (description
     (beautify-description "A logging implementation for `log` which is configured via an environment\nvariable."))
    (license (list license:expat license:asl2.0))))

(define rust-serde_1_0_198
  (package
    (name "rust-serde")
    (version "1.0.198")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1k0z8mwkkl46bwfk16z7v8xidi5pwnj4a9fsf42k8cchjw6a8ilq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde_derive" ,rust-serde_derive_1_0_198))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     (beautify-description "A generic serialization/deserialization framework"))
    (license (list license:expat license:asl2.0))))

(define rust-serde_json_1_0_116
  (package
    (name "rust-serde_json")
    (version "1.0.116")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04r81f5myl41zrsyghnbmbl39c4n3azldb9zxfafnzyi4rqxn5ry"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-itoa" ,rust-itoa_1_0_11)
        ("rust-ryu" ,rust-ryu_1_0_17)
        ("rust-serde" ,rust-serde_1_0_198))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
     (beautify-description "A JSON serialization file format"))
    (license (list license:expat license:asl2.0))))

(define rust-tempfile_3_10_1
  (package
    (name "rust-tempfile")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wdzz35ri168jn9al4s1g2rnsrr5ci91khgarc2rvpb3nappzdw5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-fastrand" ,rust-fastrand_2_0_2)
        ("rust-rustix" ,rust-rustix_0_38_32)
        ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "A library for managing temporary files and directories.")
    (description
     (beautify-description "A library for managing temporary files and directories."))
    (license (list license:expat license:asl2.0))))

(define rust-anyhow_1_0_82
  (package
    (name "rust-anyhow")
    (version "1.0.82")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anyhow" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "06952ih07mhfnim7r1mmwkj1k0ag66d7z9psw2dnlvvfydx86f7m"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description
     (beautify-description "Flexible concrete Error type built on std::error::Error"))
    (license (list license:expat license:asl2.0))))

(define rust-backoff_0_4_0
  (package
    (name "rust-backoff")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "backoff" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1h80d9xn5wngxdgza2m8w4x1kyhk0x6k9ydvsj50j2pcn6fdnbdn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom_0_2_14)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/ihrwein/backoff")
    (synopsis "Retry operations with exponential backoff policy.")
    (description
     (beautify-description "Retry operations with exponential backoff policy."))
    (license (list license:expat license:asl2.0))))

(define rust-walkdir_2_5_0
  (package
    (name "rust-walkdir")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "walkdir" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-same-file" ,rust-same-file-1)
        ("rust-winapi-util" ,rust-winapi-util-0.1))))
    (home-page "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory.")
    (description
     (beautify-description "Recursively walk a directory."))
    (license (list license:unlicense license:expat))))

(define rust-rayon_1_10_0
  (package
    (name "rust-rayon")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-either" ,rust-either_1_11_0)
        ("rust-rayon-core" ,rust-rayon-core_1_12_1))))
    (home-page "None")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description
     (beautify-description "Simple work-stealing parallelism for Rust"))
    (license (list license:expat license:asl2.0))))

(define rust-thiserror_1_0_58
  (package
    (name "rust-thiserror")
    (version "1.0.58")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15rjgd1abi2mzjgzfhrvmsxf9h65n95h6sp8f4s52q4i00wqhih3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-thiserror-impl" ,rust-thiserror-impl_1_0_58))))
    (home-page "None")
    (synopsis "derive(Error)")
    (description
     (beautify-description "derive(Error)"))
    (license (list license:expat license:asl2.0))))

(define rust-is_executable_1_0_1
  (package
    (name "rust-is_executable")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "is_executable" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1j59iqaxcgax0qll30rarpcr7y3dpkl38iv4mlkfcxbvsv3cv6ps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "None")
    (synopsis "Is there an executable file at the given path?")
    (description
     (beautify-description "Is there an executable file at the given path?"))
    (license (list license:asl2.0 license:expat))))

(define rust-thiserror-impl_1_0_58
  (package
    (name "rust-thiserror-impl")
    (version "1.0.58")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xylyqcb8rv5yh2yf97hg4n4kg27qccc0ijafr1zqklrhahkn7y6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_81)
        ("rust-quote" ,rust-quote_1_0_36)
        ("rust-syn" ,rust-syn_2_0_59))))
    (home-page "None")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
     (beautify-description "Implementation detail of the `thiserror` crate"))
    (license (list license:expat license:asl2.0))))

(define rust-quote_1_0_36
  (package
    (name "rust-quote")
    (version "1.0.36")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quote" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_81))))
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
     (beautify-description "Quasi-quoting macro quote!(...)"))
    (license (list license:expat license:asl2.0))))

(define rust-proc-macro2_1_0_81
  (package
    (name "rust-proc-macro2")
    (version "1.0.81")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fiyxjg5x5nn4vnazz93dnirf0s3grdnbf63m44qyq94q2q9f59x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "None")
    (synopsis "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     (beautify-description "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple token-based libraries from the procedural macro use case."))
    (license (list license:expat license:asl2.0))))

(define rust-syn_2_0_59
  (package
    (name "rust-syn")
    (version "2.0.59")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nk1f98z027qdjwsn756hnshp0y4cka4pq729ig6awdhqzzk2raa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_81)
        ("rust-quote" ,rust-quote_1_0_36)
        ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
     (beautify-description "Parser for Rust source code"))
    (license (list license:expat license:asl2.0))))

(define rust-curl-sys_0_4_72+curl-8_6_0
  (package
    (name "rust-curl-sys")
    (version "0.4.72+curl-8.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curl-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1sn97cah732ldcwkw5knm6kh57hx0gfxqmniiwgd2iy42j1xrjr9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_94)
        ("rust-libc" ,rust-libc_0_2_153)
        ("rust-libz-sys" ,rust-libz-sys_1_1_16)
        ("rust-openssl-sys" ,rust-openssl-sys_0_9_102)
        ("rust-pkg-config" ,rust-pkg-config_0_3_30)
        ("rust-vcpkg" ,rust-vcpkg-0.2)
        ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "None")
    (synopsis "Native bindings to the libcurl library")
    (description
     (beautify-description "Native bindings to the libcurl library"))
    (license (list license:expat))))

(define rust-slab_0_4_9
  (package
    (name "rust-slab")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "slab" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg_1_2_0))))
    (home-page "None")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description
     (beautify-description "Pre-allocated storage for a uniform data type"))
    (license (list license:expat))))

(define rust-castaway_0_1_2
  (package
    (name "rust-castaway")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "castaway" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xhspwy477qy5yg9c3jp713asxckjpx0vfrmz5l7r5zg7naqysd2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Safe, zero-cost downcasting for limited compile-time specialization.")
    (description
     (beautify-description "Safe, zero-cost downcasting for limited compile-time specialization."))
    (license (list license:expat))))

(define rust-sluice_0_5_5
  (package
    (name "rust-sluice")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sluice" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1d9ywr5039ibgaby8sc72f8fs5lpp8j5y6p3npya4jplxz000x3d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-channel" ,rust-async-channel-1)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3))))
    (home-page "None")
    (synopsis "Efficient ring buffer for byte buffers, FIFO queues, and SPSC channels")
    (description
     (beautify-description "Efficient ring buffer for byte buffers, FIFO queues, and SPSC channels"))
    (license (list license:expat))))

(define rust-tracing-futures_0_2_5
  (package
    (name "rust-tracing-futures")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-futures" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wimg0iwa2ldq7xv98lvivvf3q9ykfminig8r1bs0ig22np9bl4p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pin-project" ,rust-pin-project_1_1_5)
        ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities for instrumenting `futures` with `tracing`.")
    (description
     (beautify-description "Utilities for instrumenting `futures` with `tracing`."))
    (license (list license:expat))))

(define rust-http_0_2_12
  (package
    (name "rust-http")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes_1_6_0)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-itoa" ,rust-itoa_1_0_11))))
    (home-page "None")
    (synopsis "A set of types for representing HTTP requests and responses.")
    (description
     (beautify-description "A set of types for representing HTTP requests and responses."))
    (license (list license:expat license:asl2.0))))

(define rust-curl_0_4_46
  (package
    (name "rust-curl")
    (version "0.4.46")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "curl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dk7xi1fv57ak5wsgzig702czv3ssrgyk120b7qhy2dsdvfn288y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-curl-sys" ,rust-curl-sys_0_4_72+curl-8_6_0)
        ("rust-libc" ,rust-libc_0_2_153)
        ("rust-openssl-probe" ,rust-openssl-probe_0_1_5)
        ("rust-openssl-sys" ,rust-openssl-sys_0_9_102)
        ("rust-schannel" ,rust-schannel_0_1_23)
        ("rust-socket2" ,rust-socket2_0_5_6)
        ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/alexcrichton/curl-rust")
    (synopsis "Rust bindings to libcurl for making HTTP requests")
    (description
     (beautify-description "Rust bindings to libcurl for making HTTP requests"))
    (license (list license:expat))))

(define rust-once_cell_1_19_0
  (package
    (name "rust-once_cell")
    (version "1.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "once_cell" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Single assignment cells and lazy values.")
    (description
     (beautify-description "Single assignment cells and lazy values."))
    (license (list license:expat license:asl2.0))))

(define rust-pkg-config_0_3_30
  (package
    (name "rust-pkg-config")
    (version "0.3.30")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pkg-config" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1v07557dj1sa0aly9c90wsygc0i8xv5vnmyv0g94lpkvj8qb4cfj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts.")
    (description
     (beautify-description "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts."))
    (license (list license:expat license:asl2.0))))

(define rust-libz-sys_1_1_16
  (package
    (name "rust-libz-sys")
    (version "1.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libz-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yd7mnw0h469rbsxfbb0r6czc4q8fabn9jkbiz99a9kbcrg3n52y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_94)
        ("rust-libc" ,rust-libc_0_2_153)
        ("rust-pkg-config" ,rust-pkg-config_0_3_30)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "None")
    (synopsis "Low-level bindings to the system libz library (also known as zlib).")
    (description
     (beautify-description "Low-level bindings to the system libz library (also known as zlib)."))
    (license (list license:expat license:asl2.0))))

(define rust-openssl-sys_0_9_102
  (package
    (name "rust-openssl-sys")
    (version "0.9.102")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18jzni7xzdcqwf9r8kp6j46abrxqn82dvc2ylf9kij7varyn75y5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc_1_0_94)
        ("rust-libc" ,rust-libc_0_2_153)
        ("rust-pkg-config" ,rust-pkg-config_0_3_30)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "None")
    (synopsis "FFI bindings to OpenSSL")
    (description
     (beautify-description "FFI bindings to OpenSSL"))
    (license (list license:expat))))

(define rust-cc_1_0_94
  (package
    (name "rust-cc")
    (version "1.0.94")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rrm9yw419rwhnkdx9s31nlidqp2s5arf26ckwai3h4x48jf7xhp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode.")
    (description
     (beautify-description "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode."))
    (license (list license:expat license:asl2.0))))

(define rust-libc_0_2_153
  (package
    (name "rust-libc")
    (version "0.2.153")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
     (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license (list license:expat license:asl2.0))))

(define rust-autocfg_1_2_0
  (package
    (name "rust-autocfg")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "autocfg" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "102c77is3pii4rsqfsc5vrbk6qabjy0yqc0gwqzmjjb9fp3spzgi"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Automatic cfg for Rust compiler features")
    (description
     (beautify-description "Automatic cfg for Rust compiler features"))
    (license (list license:asl2.0 license:expat))))

(define rust-pin-project_1_1_5
  (package
    (name "rust-pin-project")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pin-project" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cxl146x0q7lawp0m1826wsgj8mmmfs6ja8q7m6f7ff5j6vl7gxn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pin-project-internal" ,rust-pin-project-internal_1_1_5))))
    (home-page "None")
    (synopsis "A crate for safe and ergonomic pin-projection.")
    (description
     (beautify-description "A crate for safe and ergonomic pin-projection."))
    (license (list license:asl2.0 license:expat))))

(define rust-pin-project-internal_1_1_5
  (package
    (name "rust-pin-project-internal")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pin-project-internal" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0r9r4ivwiyqf45sv6b30l1dx282lxaax2f6gl84jwa3q590s8f1g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_81)
        ("rust-quote" ,rust-quote_1_0_36)
        ("rust-syn" ,rust-syn_2_0_59))))
    (home-page "None")
    (synopsis "Implementation detail of the `pin-project` crate.")
    (description
     (beautify-description "Implementation detail of the `pin-project` crate."))
    (license (list license:asl2.0 license:expat))))

(define rust-itoa_1_0_11
  (package
    (name "rust-itoa")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itoa" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nv9cqjwzr3q58qz84dcz63ggc54yhf1yqar1m858m1kfd4g3wa9"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast integer primitive to string conversion")
    (description
     (beautify-description "Fast integer primitive to string conversion"))
    (license (list license:expat license:asl2.0))))

(define rust-bytes_1_6_0
  (package
    (name "rust-bytes")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytes" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jf2awc1fywpk15m6pxay3wqcg65ararg9xi4b08vnszwiyy2kai"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Types and traits for working with bytes")
    (description
     (beautify-description "Types and traits for working with bytes"))
    (license (list license:expat))))

(define rust-socket2_0_5_6
  (package
    (name "rust-socket2")
    (version "0.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "socket2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0w98g7dh9m74vpxln401hl4knpjzrx7jhng7cbh46x9vm70dkzq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc_0_2_153)
        ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/rust-lang/socket2")
    (synopsis "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended.")
    (description
     (beautify-description "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended."))
    (license (list license:expat license:asl2.0))))

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
    (license (list license:expat license:asl2.0))))

(define rust-schannel_0_1_23
  (package
    (name "rust-schannel")
    (version "0.1.23")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "schannel" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0d1m156bsjrws6xzzr1wyfyih9i22mb2csb5pc5kmkrvci2ibjgv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "None")
    (synopsis "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl")
    (description
     (beautify-description "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl"))
    (license (list license:expat))))

(define rust-env_filter_0_1_0
  (package
    (name "rust-env_filter")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_filter" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1spp4jx0fissi0bg00d8nn4vnjwf6y3hr7d0vmcq65gb214al2d0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log_0_4_21)
        ("rust-regex" ,rust-regex_1_10_4))))
    (home-page "None")
    (synopsis "Filter log events using environment variables")
    (description
     (beautify-description "Filter log events using environment variables"))
    (license (list license:expat license:asl2.0))))

(define rust-anstream_0_6_13
  (package
    (name "rust-anstream")
    (version "0.6.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstream" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yv2idkyf9mp9xwc684v0ywqiy86lwc9gvllwdishl7y6czx0syr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anstyle" ,rust-anstyle-1)
        ("rust-anstyle-parse" ,rust-anstyle-parse_0_2_3)
        ("rust-anstyle-query" ,rust-anstyle-query_1_0_2)
        ("rust-anstyle-wincon" ,rust-anstyle-wincon-3)
        ("rust-colorchoice" ,rust-colorchoice-1)
        ("rust-utf8parse" ,rust-utf8parse-0.2))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "A simple cross platform library for writing colored text to a terminal.")
    (description
     (beautify-description "A simple cross platform library for writing colored text to a terminal."))
    (license (list license:expat license:asl2.0))))

(define rust-regex_1_10_4
  (package
    (name "rust-regex")
    (version "1.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0k5sb0h2mkwf51ab0gvv3x38jp1q7wgxf63abfbhi0wwvvgxn5y1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick_1_1_3)
        ("rust-memchr" ,rust-memchr_2_7_2)
        ("rust-regex-automata" ,rust-regex-automata_0_4_6)
        ("rust-regex-syntax" ,rust-regex-syntax_0_8_3))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
     (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license (list license:expat license:asl2.0))))

(define rust-regex-automata_0_4_6
  (package
    (name "rust-regex-automata")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-automata" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1spaq7y4im7s56d1gxa2hi4hzf6dwswb1bv8xyavzya7k25kpf46"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick_1_1_3)
        ("rust-memchr" ,rust-memchr_2_7_2)
        ("rust-regex-syntax" ,rust-regex-syntax_0_8_3))))
    (home-page "None")
    (synopsis "Automata construction and matching using regular expressions.")
    (description
     (beautify-description "Automata construction and matching using regular expressions."))
    (license (list license:expat license:asl2.0))))

(define rust-regex-syntax_0_8_3
  (package
    (name "rust-regex-syntax")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-syntax" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mhzkm1pkqg6y53xv056qciazlg47pq0czqs94cn302ckvi49bdd"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A regular expression parser.")
    (description
     (beautify-description "A regular expression parser."))
    (license (list license:expat license:asl2.0))))

(define rust-aho-corasick_1_1_3
  (package
    (name "rust-aho-corasick")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aho-corasick" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr_2_7_2))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
     (beautify-description "Fast multiple substring searching."))
    (license (list license:unlicense license:expat))))

(define rust-memchr_2_7_2
  (package
    (name "rust-memchr")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memchr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07bcqxb0vx4ji0648ny5xsicjnpma95x1n07v7mi7jrhsz2l11kc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis "Provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32) routines for\n1, 2 or 3 byte search and single substring search.")
    (description
     (beautify-description "Provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32) routines for\n1, 2 or 3 byte search and single substring search."))
    (license (list license:unlicense license:expat))))

(define rust-anstyle-query_1_0_2
  (package
    (name "rust-anstyle-query")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-query" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0j3na4b1nma39g4x7cwvj009awxckjf3z2vkwhldgka44hqj72g2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "None")
    (synopsis "Look up colored console capabilities")
    (description
     (beautify-description "Look up colored console capabilities"))
    (license (list license:expat license:asl2.0))))

(define rust-anstyle-parse_0_2_3
  (package
    (name "rust-anstyle-parse")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle-parse" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "134jhzrz89labrdwxxnjxqjdg06qvaflj1wkfnmyapwyldfwcnn7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-utf8parse" ,rust-utf8parse-0.2))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Parse ANSI Style Escapes")
    (description
     (beautify-description "Parse ANSI Style Escapes"))
    (license (list license:expat license:asl2.0))))

(define rust-serde_derive_1_0_198
  (package
    (name "rust-serde_derive")
    (version "1.0.198")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nb6hn1xpvhih00x6jkfm42na6pwz59h2zayj2x865xhd6wdm3p8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2_1_0_81)
        ("rust-quote" ,rust-quote_1_0_36)
        ("rust-syn" ,rust-syn_2_0_59))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license (list license:expat license:asl2.0))))

(define rust-ryu_1_0_17
  (package
    (name "rust-ryu")
    (version "1.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ryu" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "188vrsh3zlnl5xl7lw0rp2sc0knpx8yaqpwvr648b6h12v4rfrp8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast floating point to string conversion")
    (description
     (beautify-description "Fast floating point to string conversion"))
    (license (list license:asl2.0 license:boost1.0))))

(define rust-rustix_0_38_32
  (package
    (name "rust-rustix")
    (version "0.38.32")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustix" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "12fvzwnsb13svnqzsf01maz44dib8kmgp2w8cxp7f8azwrhliq35"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags_2_5_0)
        ("rust-errno" ,rust-errno-0.3)
        ("rust-libc" ,rust-libc_0_2_153)
        ("rust-linux-raw-sys" ,rust-linux-raw-sys_0_4_13)
        ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "None")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (description
     (beautify-description "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls"))
    (license (list license:asl2.0 license:asl2.0 license:expat))))

(define rust-fastrand_2_0_2
  (package
    (name "rust-fastrand")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fastrand" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "111rlv1988jkfiymr41zf1mhyr5mwy68mwcnrjim5j7l3idxd2v5"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A simple and fast random number generator")
    (description
     (beautify-description "A simple and fast random number generator"))
    (license (list license:asl2.0 license:expat))))

(define rust-bitflags_2_5_0
  (package
    (name "rust-bitflags")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1h91vdx1il069vdiiissj8ymzj130rbiic0dbs77yxjgjim9sjyg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
     (beautify-description "A macro to generate structures which behave like bitflags."))
    (license (list license:expat license:asl2.0))))

(define rust-linux-raw-sys_0_4_13
  (package
    (name "rust-linux-raw-sys")
    (version "0.4.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "linux-raw-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "172k2c6422gsc914ig8rh99mb9yc7siw6ikc3d9xw1k7vx0s3k81"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Generated bindings for Linux\u0027s userspace API")
    (description
     (beautify-description "Generated bindings for Linux\u0027s userspace API"))
    (license (list license:asl2.0 license:asl2.0 license:expat))))

(define rust-getrandom_0_2_14
  (package
    (name "rust-getrandom")
    (version "0.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "getrandom" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0v18s6lpkvil6dkdfb86l84mwpqbpw6928qp0n0hj4dhxh32xcll"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-libc" ,rust-libc_0_2_153)
        ("rust-wasi" ,rust-wasi-0.11))))
    (home-page "None")
    (synopsis "A small cross-platform library for retrieving random data from system source")
    (description
     (beautify-description "A small cross-platform library for retrieving random data from system source"))
    (license (list license:expat license:asl2.0))))

(define rust-either_1_11_0
  (package
    (name "rust-either")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "either" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18l0cwyw18syl8b52syv6balql8mnwfyhihjqqllx5pms93iqz54"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
     (beautify-description "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases."))
    (license (list license:expat license:asl2.0))))

(define rust-rayon-core_1_12_1
  (package
    (name "rust-rayon-core")
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon-core" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-deque" ,rust-crossbeam-deque_0_8_5)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page "None")
    (synopsis "Core APIs for Rayon")
    (description
     (beautify-description "Core APIs for Rayon"))
    (license (list license:expat license:asl2.0))))

(define rust-crossbeam-deque_0_8_5
  (package
    (name "rust-crossbeam-deque")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-deque" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03bp38ljx4wj6vvy4fbhx41q8f585zyqix6pncz1mkz93z08qgv1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-epoch" ,rust-crossbeam-epoch_0_9_18)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description
     (beautify-description "Concurrent work-stealing deque"))
    (license (list license:expat license:asl2.0))))

(define rust-crossbeam-epoch_0_9_18
  (package
    (name "rust-crossbeam-epoch")
    (version "0.9.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-epoch" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description
     (beautify-description "Epoch-based garbage collection"))
    (license (list license:expat license:asl2.0))))
