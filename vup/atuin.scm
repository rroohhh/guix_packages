(define-module (vup atuin)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-shell)
  #:use-module (gnu packages crates-database)
  #:use-module (guix download)
  #:use-module ((guix import utils) #:select (beautify-description)))


(define-public atuin
  (package
    (name "rust-atuin")
    (version "18.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "183hkgnb0q0195y7871g5c7xxdsfi505f7m1y7zvpzivb7575imc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:tests? #f
      #:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-atuin-client" ,rust-atuin-client_18_1_0)
       ("rust-atuin-common" ,rust-atuin-common_18_1_0)
       ("rust-atuin-dotfiles" ,rust-atuin-dotfiles_0_1_0)
       ("rust-atuin-server" ,rust-atuin-server_18_1_0)
       ("rust-atuin-server-postgres" ,rust-atuin-server-postgres_18_1_0)
       ("rust-base64" ,rust-base64_0_21_7)
       ("rust-clap" ,rust-clap_4_5_1)
       ("rust-clap_complete" ,rust-clap_complete_4_5_1)
       ("rust-clap_complete_nushell" ,rust-clap_complete_nushell_4_5_1)
       ("rust-cli-clipboard" ,rust-cli-clipboard-0.4)
       ("rust-colored" ,rust-colored-2)
       ("rust-crossterm" ,rust-crossterm-0.27)
       ("rust-directories" ,rust-directories-5)
       ("rust-env_logger" ,rust-env_logger_0_11_3)
       ("rust-eyre" ,rust-eyre_0_6_12)
       ("rust-fs-err" ,rust-fs-err-2)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
       ("rust-indicatif" ,rust-indicatif_0_17_8)
       ("rust-interim" ,rust-interim_0_1_1)
       ("rust-itertools" ,rust-itertools_0_12_1)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-ratatui" ,rust-ratatui_0_25_0)
       ("rust-rpassword" ,rust-rpassword_7_3_1)
       ("rust-runtime-format" ,rust-runtime-format_0_1_3)
       ("rust-rustix" ,rust-rustix_0_38_31)
       ("rust-semver" ,rust-semver_1_0_22)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-serde_yaml" ,rust-serde_yaml_0_9_32)
       ("rust-sysinfo" ,rust-sysinfo_0_30_6)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tiny-bip39" ,rust-tiny-bip39_1_0_0)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
       ("rust-unicode-segmentation" ,rust-unicode-segmentation_1_11_0)
       ("rust-unicode-width" ,rust-unicode-width-0.1)
       ("rust-uuid" ,rust-uuid_1_7_0)
       ("rust-whoami" ,rust-whoami_1_5_1))
      #:cargo-development-inputs
      (("rust-tracing-tree" ,rust-tracing-tree_0_3_0))))
    (home-page "{'workspace': True}")
    (synopsis "atuin - magical shell history")
    (description
      (beautify-description "atuin - magical shell history"))
    (license (list license:expat))))

(define-public rust-atuin-server_18_1_0
  (package
    (name "rust-atuin-server")
    (version "18.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-server" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hhds58jjnaixsylqnnz5vsryjvn24f6nr2nsra62pb4bygwhql5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-argon2" ,rust-argon2_0_5_3)
       ("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-atuin-common" ,rust-atuin-common_18_1_0)
       ("rust-atuin-server-database" ,rust-atuin-server-database_18_1_0)
       ("rust-axum" ,rust-axum_0_7_4)
       ("rust-axum-server" ,rust-axum-server_0_6_0)
       ("rust-base64" ,rust-base64_0_21_7)
       ("rust-config" ,rust-config_0_13_4)
       ("rust-eyre" ,rust-eyre_0_6_12)
       ("rust-fs-err" ,rust-fs-err-2)
       ("rust-metrics" ,rust-metrics_0_21_1)
       ("rust-metrics-exporter-prometheus" ,rust-metrics-exporter-prometheus_0_12_2)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-reqwest" ,rust-reqwest_0_11_24)
       ("rust-rustls" ,rust-rustls-0.21)
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_2_1_1)
       ("rust-semver" ,rust-semver_1_0_22)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tower" ,rust-tower-0.4)
       ("rust-tower-http" ,rust-tower-http_0_5_2)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "{'workspace': True}")
    (synopsis "server library for atuin")
    (description
      (beautify-description "server library for atuin"))
    (license (list license:expat))))

(define-public rust-whoami_1_5_1
  (package
    (name "rust-whoami")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "whoami" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1aafr70h2zlqr73i58bj84hdf9rgplxbpygqbgsqhkk3mngv8jm4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-redox_syscall" ,rust-redox_syscall_0_4_1)
       ("rust-wasite" ,rust-wasite_0_1_0)
       ("rust-web-sys" ,rust-web-sys_0_3_69))))
    (home-page "https://github.com/ardaku/whoami/blob/v1/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment.")
    (description
      (beautify-description "Retrieve the current user and environment."))
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-env_logger_0_11_3
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

(define-public rust-eyre_0_6_12
  (package
    (name "rust-eyre")
    (version "0.6.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "eyre" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-indenter" ,rust-indenter-0.3)
       ("rust-once_cell" ,rust-once_cell_1_19_0))))
    (home-page "None")
    (synopsis "Flexible concrete Error Reporting type built on std::error::Error with customizable Reports")
    (description
      (beautify-description "Flexible concrete Error Reporting type built on std::error::Error with customizable Reports"))
    (license (list license:expat license:asl2.0))))

(define-public rust-serde_yaml_0_9_32
  (package
    (name "rust-serde_yaml")
    (version "0.9.32")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_yaml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bqzv9n82wv0aj95qhj8xaq34a6wdfwizdcm9xvllk8mjkcpbl4g"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-indexmap" ,rust-indexmap_2_2_5)
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-ryu" ,rust-ryu_1_0_17)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-unsafe-libyaml" ,rust-unsafe-libyaml-0.2))))
    (home-page "None")
    (synopsis "YAML data format for Serde")
    (description
      (beautify-description "YAML data format for Serde"))
    (license (list license:expat license:asl2.0))))

(define-public rust-tiny-bip39_1_0_0
  (package
    (name "rust-tiny-bip39")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tiny-bip39" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0q98iv3wgbd41wyxxd5is8sddi53k9ary45rbi5fi8dmb39r9k32"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-anyhow" ,rust-anyhow_1_0_80)
       ("rust-hmac" ,rust-hmac-0.12)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-pbkdf2" ,rust-pbkdf2-0.11)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-rustc-hash" ,rust-rustc-hash-1)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-thiserror" ,rust-thiserror_1_0_57)
       ("rust-unicode-normalization" ,rust-unicode-normalization_0_1_23)
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_92)
       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/maciejhirsz/tiny-bip39/")
    (synopsis "A fork of the bip39 crate with fixes to v0.6. Rust implementation of BIP-0039")
    (description
      (beautify-description "A fork of the bip39 crate with fixes to v0.6. Rust implementation of BIP-0039"))
    (license (list license:expat license:asl2.0))))

(define-public rust-clap_complete_nushell_4_5_1
  (package
    (name "rust-clap_complete_nushell")
    (version "4.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_complete_nushell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dm7wsxvdr2w98lp03220x4p2yafbv91g49380hdzrvc0a7f9l40"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-clap" ,rust-clap_4_5_1)
       ("rust-clap_complete" ,rust-clap_complete_4_5_1))))
    (home-page "None")
    (synopsis "A generator library used with clap for Nushell completion scripts")
    (description
      (beautify-description "A generator library used with clap for Nushell completion scripts"))
    (license (list license:expat license:asl2.0))))

(define-public rust-ratatui_0_25_0
  (package
    (name "rust-ratatui")
    (version "0.25.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ratatui" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sw0s4s9y09n2nsxs996frrlmy4fay7ibwfrvar0fvmswi99wrd5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_2_4_2)
       ("rust-cassowary" ,rust-cassowary-0.3)
       ("rust-crossterm" ,rust-crossterm-0.27)
       ("rust-indoc" ,rust-indoc-2)
       ("rust-itertools" ,rust-itertools_0_12_1)
       ("rust-lru" ,rust-lru_0_12_3)
       ("rust-paste" ,rust-paste-1)
       ("rust-stability" ,rust-stability_0_1_1)
       ("rust-strum" ,rust-strum-0.25)
       ("rust-unicode-segmentation" ,rust-unicode-segmentation_1_11_0)
       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "None")
    (synopsis "A library that\u0027s all about cooking up terminal user interfaces")
    (description
      (beautify-description "A library that\u0027s all about cooking up terminal user interfaces"))
    (license (list license:expat))))

(define-public rust-semver_1_0_22
  (package
    (name "rust-semver")
    (version "1.0.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jir6q2ps4s5v52bqxpvwj35p0m0ahl5pf62ppwksbv5kvk3zm4j"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Parser and evaluator for Cargo\u0027s flavor of Semantic Versioning")
    (description
      (beautify-description "Parser and evaluator for Cargo\u0027s flavor of Semantic Versioning"))
    (license (list license:expat license:asl2.0))))

(define-public rust-interim_0_1_1
  (package
    (name "rust-interim")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "interim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0d32gcccw1iyjg79jbzzhsib52ikxkddzk5fxax274ji2x43jyqi"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-logos" ,rust-logos-0.13)
       ("rust-time" ,rust-time_0_3_34))))
    (home-page "None")
    (synopsis "parses simple English dates, inspired by Linux date command, and forked from chrono-english")
    (description
      (beautify-description "parses simple English dates, inspired by Linux date command, and forked from chrono-english"))
    (license (list license:expat))))

(define-public rust-rpassword_7_3_1
  (package
    (name "rust-rpassword")
    (version "7.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rpassword" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gvy3lcpph9vv1rl0cjfn72ylvmgbw2vklmj6w0iv4cpr3ijniw0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_153)
       ("rust-rtoolbox" ,rust-rtoolbox_0_0_2)
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/conradkleinespel/rpassword")
    (synopsis "Read passwords in console applications.")
    (description
      (beautify-description "Read passwords in console applications."))
    (license (list license:asl2.0))))

(define-public rust-itertools_0_12_1
  (package
    (name "rust-itertools")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-either" ,rust-either-1))))
    (home-page "None")
    (synopsis "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      (beautify-description "Extra iterator adaptors, iterator methods, free functions, and macros."))
    (license (list license:expat license:asl2.0))))

(define-public rust-indicatif_0_17_8
  (package
    (name "rust-indicatif")
    (version "0.17.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indicatif" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18xyqxw9i5x4sbpzckhfz3nm984iq9r7nbi2lk76nz888n7mlfkn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-console" ,rust-console_0_15_8)
       ("rust-instant" ,rust-instant-0.1)
       ("rust-number_prefix" ,rust-number_prefix_0_4_0)
       ("rust-portable-atomic" ,rust-portable-atomic-1)
       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "None")
    (synopsis "A progress bar and cli reporting library for Rust")
    (description
      (beautify-description "A progress bar and cli reporting library for Rust"))
    (license (list license:expat))))

(define-public rust-base64_0_21_7
  (package
    (name "rust-base64")
    (version "0.21.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description
      (beautify-description "encodes and decodes base64 as bytes or utf8"))
    (license (list license:expat license:asl2.0))))

(define-public rust-rustix_0_38_31
  (package
    (name "rust-rustix")
    (version "0.38.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jg9yj3i6qnzk1y82hng7rb1bwhslfbh57507dxcs9mgcakf38vf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_2_4_2)
       ("rust-errno" ,rust-errno-0.3)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-linux-raw-sys" ,rust-linux-raw-sys_0_4_13)
       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "None")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (description
      (beautify-description "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls"))
    (license (list license:asl2.0 license:asl2.0 license:expat))))

(define-public rust-serde_1_0_197
  (package
    (name "rust-serde")
    (version "1.0.197")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qjcxqd3p4yh5cmmax9q4ics1zy34j5ij32cvjj5dc5rw5rwic9z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde_derive" ,rust-serde_derive_1_0_197))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
      (beautify-description "A generic serialization/deserialization framework"))
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-tree_0_3_0
  (package
    (name "rust-tracing-tree")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-tree" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xb6csq7hpjjr9x7qx1h6r3ra7p2mxvirh4vp71q8r1z5k6rw4v5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.49)
       ("rust-tracing-core" ,rust-tracing-core-0.1)
       ("rust-tracing-log" ,rust-tracing-log-0.2)
       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "None")
    (synopsis "A Tracing Layer which prints a tree of spans and events.")
    (description
      (beautify-description "A Tracing Layer which prints a tree of spans and events."))
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-common_18_1_0
  (package
    (name "rust-atuin-common")
    (version "18.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-common" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0avjlsn6r6hshssvbz2pjgfqf0ljw40w54301qy3zgsr98m0msjh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-eyre" ,rust-eyre_0_6_12)
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-semver" ,rust-semver_1_0_22)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-sqlx" ,rust-sqlx_0_7_3)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-typed-builder" ,rust-typed-builder_0_18_1)
       ("rust-uuid" ,rust-uuid_1_7_0))
      #:cargo-development-inputs
      (("rust-pretty_assertions" ,rust-pretty_assertions_1_4_0))))
    (home-page "{'workspace': True}")
    (synopsis "common library for atuin")
    (description
      (beautify-description "common library for atuin"))
    (license (list license:expat))))

(define-public rust-clap_4_5_1
  (package
    (name "rust-clap")
    (version "4.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ni08mammjr61fg7cx900zgvcdfb4z7fjrlm1xx5f4r9xx0xa669"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-clap_builder" ,rust-clap_builder_4_5_1)
       ("rust-clap_derive" ,rust-clap_derive_4_5_0))))
    (home-page "None")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (list license:expat license:asl2.0))))

(define-public rust-log_0_4_21
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

(define-public rust-serde_json_1_0_114
  (package
    (name "rust-serde_json")
    (version "1.0.114")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q4saigxwkf8bw4y5kp6k33dnavlvvwa2q4zmag59vrjsqdrpw65"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-ryu" ,rust-ryu_1_0_17)
       ("rust-serde" ,rust-serde_1_0_197))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
      (beautify-description "A JSON serialization file format"))
    (license (list license:expat license:asl2.0))))

(define-public rust-uuid_1_7_0
  (package
    (name "rust-uuid")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uuid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0aivp5ys7sg2izlj2sn6rr8p43vdcwg64naj8n0kqbd15iqcj37h"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atomic" ,rust-atomic_0_5_3)
       ("rust-getrandom" ,rust-getrandom_0_2_12)
       ("rust-serde" ,rust-serde_1_0_197))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
      (beautify-description "A library to generate and parse UUIDs."))
    (license (list license:asl2.0 license:expat))))

(define-public rust-unicode-segmentation_1_11_0
  (package
    (name "rust-unicode-segmentation")
    (version "1.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-segmentation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00kjpwp1g8fqm45drmwivlacn3y9jx73bvs09n6s3x73nqi7vj6l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis "This crate provides Grapheme Cluster, Word and Sentence boundaries\naccording to Unicode Standard Annex #29 rules.")
    (description
      (beautify-description "This crate provides Grapheme Cluster, Word and Sentence boundaries\naccording to Unicode Standard Annex #29 rules."))
    (license (list license:expat license:asl2.0))))

(define-public rust-sysinfo_0_30_6
  (package
    (name "rust-sysinfo")
    (version "0.30.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sysinfo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15nishmi0q1fcplc4njwjwhry430q1j5clsryyzqaalzmyf92ik7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-ntapi" ,rust-ntapi-0.4)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-rayon" ,rust-rayon_1_9_0)
       ("rust-windows" ,rust-windows_0_52_0))))
    (home-page "None")
    (synopsis "Library to get system information such as processes, CPUs, disks, components and networks")
    (description
      (beautify-description "Library to get system information such as processes, CPUs, disks, components and networks"))
    (license (list license:expat))))

(define-public rust-runtime-format_0_1_3
  (package
    (name "rust-runtime-format")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "runtime-format" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "154c7jq7kbpc5acn2ysa2ilab2x0i5y7d34jwznni9xw71dqv589"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-tinyvec" ,rust-tinyvec-1))))
    (home-page "None")
    (synopsis "rust library for formatting dynamic strings")
    (description
      (beautify-description "rust library for formatting dynamic strings"))
    (license (list license:expat))))

(define-public rust-tokio_1_36_0
  (package
    (name "rust-tokio")
    (version "1.36.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c89p36zbd4abr1z3l5mipp43x7z4c9b4vp4s6r8y0gs2mjmya31"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-backtrace" ,rust-backtrace_0_3_69)
       ("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-mio" ,rust-mio_0_8_11)
       ("rust-num_cpus" ,rust-num_cpus_1_16_0)
       ("rust-parking_lot" ,rust-parking_lot_0_12_1)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-signal-hook-registry" ,rust-signal-hook-registry_1_4_1)
       ("rust-socket2" ,rust-socket2_0_5_6)
       ("rust-tokio-macros" ,rust-tokio-macros-2)
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://tokio.rs")
    (synopsis "An event-driven, non-blocking I/O platform for writing asynchronous I/O\nbacked applications.")
    (description
      (beautify-description "An event-driven, non-blocking I/O platform for writing asynchronous I/O\nbacked applications."))
    (license (list license:expat))))

(define-public rust-atuin-server-postgres_18_1_0
  (package
    (name "rust-atuin-server-postgres")
    (version "18.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-server-postgres" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17iap9018s0wvh3mpcmyycqfiqfr5jfk2f68sidwrm5861cwxm44"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-atuin-common" ,rust-atuin-common_18_1_0)
       ("rust-atuin-server-database" ,rust-atuin-server-database_18_1_0)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-sqlx" ,rust-sqlx_0_7_3)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "{'workspace': True}")
    (synopsis "server postgres database library for atuin")
    (description
      (beautify-description "server postgres database library for atuin"))
    (license (list license:expat))))

(define-public rust-atuin-dotfiles_0_1_0
  (package
    (name "rust-atuin-dotfiles")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-dotfiles" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iqvz8nn7jzndc1zl1j0pxsgkqimx5k20p9ngljgkv0ddyrsjqlq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atuin-client" ,rust-atuin-client_18_1_0)
       ("rust-atuin-common" ,rust-atuin-common_18_1_0)
       ("rust-crypto_secretbox" ,rust-crypto_secretbox_0_1_1)
       ("rust-eyre" ,rust-eyre_0_6_12)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-rmp" ,rust-rmp-0.8)
       ("rust-tokio" ,rust-tokio_1_36_0))))
    (home-page "{'workspace': True}")
    (synopsis "The dotfiles crate for Atuin")
    (description
      (beautify-description "The dotfiles crate for Atuin"))
    (license #f)))

(define-public rust-clap_complete_4_5_1
  (package
    (name "rust-clap_complete")
    (version "4.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_complete" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "136sfwmmbryz4lyyrcywaba9ib7yjbi3753gmsczn2zlb9ylspl8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-development-inputs
      (("rust-clap" ,rust-clap_4_5_1))))
    (home-page "None")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description
      (beautify-description "Generate shell completion scripts for your clap::Command"))
    (license (list license:expat license:asl2.0))))

(define-public rust-time_0_3_34
  (package
    (name "rust-time")
    (version "0.3.34")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jc7wgprzqjhzd0nqkbmdlnjwyddnswmjw86ni2vq55v45jqn968"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-deranged" ,rust-deranged_0_3_11)
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-num_threads" ,rust-num_threads_0_1_7)
       ("rust-powerfmt" ,rust-powerfmt-0.2)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-time-core" ,rust-time-core-0.1)
       ("rust-time-macros" ,rust-time-macros_0_2_17))
      #:cargo-development-inputs
      (("rust-num-conv" ,rust-num-conv_0_1_0))))
    (home-page "https://time-rs.github.io")
    (synopsis "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      (beautify-description "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]."))
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-client_18_1_0
  (package
    (name "rust-atuin-client")
    (version "18.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-client" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10abq4fklnk5gygn82qvwy6p67964604976mi99q8crlvyca9sbp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-atuin-common" ,rust-atuin-common_18_1_0)
       ("rust-base64" ,rust-base64_0_21_7)
       ("rust-clap" ,rust-clap_4_5_1)
       ("rust-config" ,rust-config_0_13_4)
       ("rust-crypto_secretbox" ,rust-crypto_secretbox_0_1_1)
       ("rust-directories" ,rust-directories-5)
       ("rust-eyre" ,rust-eyre_0_6_12)
       ("rust-fs-err" ,rust-fs-err-2)
       ("rust-futures" ,rust-futures-0.3)
       ("rust-generic-array" ,rust-generic-array-0.14)
       ("rust-hex" ,rust-hex-0.4)
       ("rust-indicatif" ,rust-indicatif_0_17_8)
       ("rust-interim" ,rust-interim_0_1_1)
       ("rust-itertools" ,rust-itertools_0_12_1)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-minspan" ,rust-minspan_0_1_1)
       ("rust-parse_duration" ,rust-parse_duration_2_1_1)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-regex" ,rust-regex_1_10_3)
       ("rust-reqwest" ,rust-reqwest_0_11_24)
       ("rust-rmp" ,rust-rmp-0.8)
       ("rust-rusty_paserk" ,rust-rusty_paserk_0_3_0)
       ("rust-rusty_paseto" ,rust-rusty_paseto_0_6_1)
       ("rust-semver" ,rust-semver_1_0_22)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-serde_regex" ,rust-serde_regex_1_1_0)
       ("rust-serde_with" ,rust-serde_with_3_6_1)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-shellexpand" ,rust-shellexpand-3)
       ("rust-sql-builder" ,rust-sql-builder_3_1_1)
       ("rust-sqlx" ,rust-sqlx_0_7_3)
       ("rust-thiserror" ,rust-thiserror_1_0_57)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-typed-builder" ,rust-typed-builder_0_18_1)
       ("rust-urlencoding" ,rust-urlencoding-2)
       ("rust-uuid" ,rust-uuid_1_7_0)
       ("rust-whoami" ,rust-whoami_1_5_1))
      #:cargo-development-inputs
      (("rust-pretty_assertions" ,rust-pretty_assertions_1_4_0)
       ("rust-tokio" ,rust-tokio_1_36_0))))
    (home-page "{'workspace': True}")
    (synopsis "client library for atuin")
    (description
      (beautify-description "client library for atuin"))
    (license (list license:expat))))

(define-public rust-metrics_0_21_1
  (package
    (name "rust-metrics")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "metrics" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ibndxzk0sja8cgwrr73b9vzbgfvwzwxwkxqiivnmmwy00dazqzx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ahash" ,rust-ahash_0_8_11)
       ("rust-metrics-macros" ,rust-metrics-macros_0_7_1)
       ("rust-portable-atomic" ,rust-portable-atomic-1))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "A lightweight metrics facade.")
    (description
      (beautify-description "A lightweight metrics facade."))
    (license (list license:expat))))

(define-public rust-argon2_0_5_3
  (package
    (name "rust-argon2")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "argon2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wn0kk97k49wxidfigmz1pdqmygqzi4h6w72ib7cpq765s4i0diw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64ct" ,rust-base64ct-1)
       ("rust-blake2" ,rust-blake2-0.10)
       ("rust-cpufeatures" ,rust-cpufeatures_0_2_12)
       ("rust-password-hash" ,rust-password-hash-0.5))))
    (home-page "None")
    (synopsis "Pure Rust implementation of the Argon2 password hashing function with support\nfor the Argon2d, Argon2i, and Argon2id algorithmic variants")
    (description
      (beautify-description "Pure Rust implementation of the Argon2 password hashing function with support\nfor the Argon2d, Argon2i, and Argon2id algorithmic variants"))
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-pemfile_2_1_1
  (package
    (name "rust-rustls-pemfile")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-pemfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1awxak91qgraqrsk7bwxyn2aijhzyrs7flmaddajmxbgbrl750gl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)
       ("rust-rustls-pki-types" ,rust-rustls-pki-types_1_3_1))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description
      (beautify-description "Basic .pem file parser for keys and certificates"))
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-axum_0_7_4
  (package
    (name "rust-axum")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "axum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17kv7v8m981cqmfbv5m538fzxhw51l9bajv06kfddi7njarb8dhj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-axum-core" ,rust-axum-core_0_4_3)
       ("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-http-body" ,rust-http-body_1_0_0)
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)
       ("rust-hyper" ,rust-hyper_1_2_0)
       ("rust-hyper-util" ,rust-hyper-util_0_1_3)
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-matchit" ,rust-matchit-0.7)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-mime" ,rust-mime-0.3)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-serde_path_to_error" ,rust-serde_path_to_error_0_1_15)
       ("rust-serde_urlencoded" ,rust-serde_urlencoded_0_7_1)
       ("rust-sync_wrapper" ,rust-sync_wrapper_0_1_2)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tower-layer" ,rust-tower-layer-0.3)
       ("rust-tower-service" ,rust-tower-service-0.3)
       ("rust-tracing" ,rust-tracing-0.1))
      #:cargo-development-inputs
      (("rust-rustversion" ,rust-rustversion-1)
       ("rust-tower" ,rust-tower-0.4))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description
      (beautify-description "Web framework that focuses on ergonomics and modularity"))
    (license (list license:expat))))

(define-public rust-metrics-exporter-prometheus_0_12_2
  (package
    (name "rust-metrics-exporter-prometheus")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "metrics-exporter-prometheus" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l19s21jfmwm72cxfjq35xb79a5wi4fv7c1p993dnqj8gk7afkqx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)
       ("rust-hyper" ,rust-hyper_0_14_28)
       ("rust-indexmap" ,rust-indexmap-1)
       ("rust-ipnet" ,rust-ipnet_2_9_0)
       ("rust-metrics" ,rust-metrics_0_21_1)
       ("rust-metrics-util" ,rust-metrics-util_0_15_1)
       ("rust-quanta" ,rust-quanta_0_11_1)
       ("rust-thiserror" ,rust-thiserror_1_0_57)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "A metrics-compatible exporter for sending metrics to Prometheus.")
    (description
      (beautify-description "A metrics-compatible exporter for sending metrics to Prometheus."))
    (license (list license:expat))))

(define-public rust-axum-server_0_6_0
  (package
    (name "rust-axum-server")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "axum-server" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dn0cx9ww1ph1dvljayhr62f898wl8xifpl3nsjg84jfxk1ldbf1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-arc-swap" ,rust-arc-swap_1_7_0)
       ("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-http-body" ,rust-http-body_1_0_0)
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)
       ("rust-hyper-util" ,rust-hyper-util_0_1_3)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-rustls" ,rust-rustls-0.21)
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_2_1_1)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
       ("rust-tower" ,rust-tower-0.4)
       ("rust-tower-service" ,rust-tower-service-0.3))
      #:cargo-development-inputs
      (("rust-hyper" ,rust-hyper_1_2_0))))
    (home-page "https://github.com/programatik29/axum-server")
    (synopsis "High level server designed to be used with axum framework.")
    (description
      (beautify-description "High level server designed to be used with axum framework."))
    (license (list license:expat))))

(define-public rust-reqwest_0_11_24
  (package
    (name "rust-reqwest")
    (version "0.11.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "reqwest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l920imsa9ahqir9w0ph020g5gcbvvrf72qk99gdxbw5xfa014n6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)
       ("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-encoding_rs" ,rust-encoding_rs_0_8_33)
       ("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-h2" ,rust-h2_0_3_24)
       ("rust-http" ,rust-http_0_2_12)
       ("rust-http-body" ,rust-http-body-0.4)
       ("rust-hyper" ,rust-hyper_0_14_28)
       ("rust-hyper-rustls" ,rust-hyper-rustls-0.24)
       ("rust-ipnet" ,rust-ipnet_2_9_0)
       ("rust-js-sys" ,rust-js-sys_0_3_69)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-mime" ,rust-mime-0.3)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-rustls" ,rust-rustls-0.21)
       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-serde_urlencoded" ,rust-serde_urlencoded_0_7_1)
       ("rust-sync_wrapper" ,rust-sync_wrapper_0_1_2)
       ("rust-system-configuration" ,rust-system-configuration-0.5)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tokio-rustls" ,rust-tokio-rustls-0.24)
       ("rust-tower-service" ,rust-tower-service-0.3)
       ("rust-url" ,rust-url-2)
       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures_0_4_42)
       ("rust-web-sys" ,rust-web-sys_0_3_69)
       ("rust-winreg" ,rust-winreg-0.50))
      #:cargo-development-inputs
      (("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_92))))
    (home-page "None")
    (synopsis "higher level HTTP client library")
    (description
      (beautify-description "higher level HTTP client library"))
    (license (list license:expat license:asl2.0))))

(define-public rust-tower-http_0_5_2
  (package
    (name "rust-tower-http")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xakj3x0anp55gjqibiwvzma5iz0w9pcjsr7qk97sx4qm4sd970y"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_2_4_2)
       ("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-http-body" ,rust-http-body_1_0_0)
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-tower-layer" ,rust-tower-layer-0.3)
       ("rust-tower-service" ,rust-tower-service-0.3)
       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description
      (beautify-description "Tower middleware and utilities for HTTP clients and servers"))
    (license (list license:expat))))

(define-public rust-config_0_13_4
  (package
    (name "rust-config")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "config" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jjag1x3rl77zjykbrykzhd5fsiv8vy40y4lxkj46xicjw8qwwr3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)
       ("rust-nom" ,rust-nom-7)
       ("rust-pathdiff" ,rust-pathdiff-0.2)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/mehcode/config-rs")
    (synopsis "Layered configuration system for Rust applications.")
    (description
      (beautify-description "Layered configuration system for Rust applications."))
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-server-database_18_1_0
  (package
    (name "rust-atuin-server-database")
    (version "18.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-server-database" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0knhmdars44gc8x6lphclfk62w6skzawi7acm94106n06i8dgfw2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-atuin-common" ,rust-atuin-common_18_1_0)
       ("rust-eyre" ,rust-eyre_0_6_12)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "{'workspace': True}")
    (synopsis "server database library for atuin")
    (description
      (beautify-description "server database library for atuin"))
    (license (list license:expat))))

(define-public rust-ahash_0_8_11
  (package
    (name "rust-ahash")
    (version "0.8.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ahash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-getrandom" ,rust-getrandom_0_2_12)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-version_check" ,rust-version_check_0_9_4)
       ("rust-zerocopy" ,rust-zerocopy-0.7))))
    (home-page "None")
    (synopsis "A non-cryptographic hash function using AES-NI for high performance")
    (description
      (beautify-description "A non-cryptographic hash function using AES-NI for high performance"))
    (license (list license:expat license:asl2.0))))

(define-public rust-metrics-macros_0_7_1
  (package
    (name "rust-metrics-macros")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "metrics-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0krmj7zyr4g14jdpk1jasi1w2nw64hqdxb2lfx4zxphp0vqgmd1q"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Macros for the metrics crate.")
    (description
      (beautify-description "Macros for the metrics crate."))
    (license (list license:expat))))

(define-public rust-version_check_0_9_4
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
    (license (list license:expat license:asl2.0))))

(define-public rust-once_cell_1_19_0
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

(define-public rust-getrandom_0_2_12
  (package
    (name "rust-getrandom")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getrandom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d8jb9bv38nkwlqqdjcav6gxckgwc9g30pm3qq506rvncpm9400r"))))
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

(define-public rust-libc_0_2_153
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

(define-public rust-proc-macro2_1_0_78
  (package
    (name "rust-proc-macro2")
    (version "1.0.78")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bjak27pqdn4f4ih1c9nr3manzyavsgqmf76ygw9k76q8pb2lhp2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "None")
    (synopsis "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
      (beautify-description "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple token-based libraries from the procedural macro use case."))
    (license (list license:expat license:asl2.0))))

(define-public rust-syn_2_0_52
  (package
    (name "rust-syn")
    (version "2.0.52")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01saay6pi9x19f6lin3mw3xawdyyagpzzy39ghz2rw6i6rdx36dn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
      (beautify-description "Parser for Rust source code"))
    (license (list license:expat license:asl2.0))))

(define-public rust-cpufeatures_0_2_12
  (package
    (name "rust-cpufeatures")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpufeatures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "012m7rrak4girqlii3jnqwrr73gv1i980q4wra5yyyhvzwk5xzjk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_153))))
    (home-page "None")
    (synopsis "Lightweight runtime CPU feature detection for aarch64, loongarch64, and x86/x86_64 targets, \nwith no_std support and support for mobile targets including Android and iOS")
    (description
      (beautify-description "Lightweight runtime CPU feature detection for aarch64, loongarch64, and x86/x86_64 targets, \nwith no_std support and support for mobile targets including Android and iOS"))
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-pki-types_1_3_1
  (package
    (name "rust-rustls-pki-types")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-pki-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a0g7453h07701vyxjj05gv903a0shi43mf7hl3cdd08hsr6gpjy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rustls/pki-types")
    (synopsis "Shared types for the rustls PKI ecosystem")
    (description
      (beautify-description "Shared types for the rustls PKI ecosystem"))
    (license (list license:expat license:asl2.0))))

(define-public rust-itoa_1_0_10
  (package
    (name "rust-itoa")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k7xjfki7mnv6yzjrbnbnjllg86acmbnk4izz2jmm1hx2wd6v95i"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast integer primitive to string conversion")
    (description
      (beautify-description "Fast integer primitive to string conversion"))
    (license (list license:expat license:asl2.0))))

(define-public rust-sync_wrapper_0_1_2
  (package
    (name "rust-sync_wrapper")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sync_wrapper" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))))
    (build-system cargo-build-system)
    (home-page "https://docs.rs/sync_wrapper")
    (synopsis "A tool for enlisting the compiler\u0027s help in proving the absence of concurrency")
    (description
      (beautify-description "A tool for enlisting the compiler\u0027s help in proving the absence of concurrency"))
    (license (list license:asl2.0))))

(define-public rust-http-body-util_0_1_0
  (package
    (name "rust-http-body-util")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http-body-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0h78a6jj2vky0wmgmq5f1h541cmhmlij09gw63fxl59h77mpkjs1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-http-body" ,rust-http-body_1_0_0)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "None")
    (synopsis "Combinators and adapters for HTTP request or response bodies.")
    (description
      (beautify-description "Combinators and adapters for HTTP request or response bodies."))
    (license (list license:expat))))

(define-public rust-hyper-util_0_1_3
  (package
    (name "rust-hyper-util")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1akngan7j0n2n0wd25c6952mvqbkj9gp1lcwzyxjc0d37l8yyf6a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-http-body" ,rust-http-body_1_0_0)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-socket2" ,rust-socket2_0_5_6)
       ("rust-tokio" ,rust-tokio_1_36_0))
      #:cargo-development-inputs
      (("rust-hyper" ,rust-hyper_1_2_0))))
    (home-page "https://hyper.rs")
    (synopsis "hyper utilities")
    (description
      (beautify-description "hyper utilities"))
    (license (list license:expat))))

(define-public rust-axum-core_0_4_3
  (package
    (name "rust-axum-core")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "axum-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qx28wg4j6qdcdrisqwyaavlzc0zvbsrcwa99zf9456lfbyn6p51"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-http-body" ,rust-http-body_1_0_0)
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)
       ("rust-mime" ,rust-mime-0.3)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-rustversion" ,rust-rustversion-1)
       ("rust-sync_wrapper" ,rust-sync_wrapper_0_1_2)
       ("rust-tower-layer" ,rust-tower-layer-0.3)
       ("rust-tower-service" ,rust-tower-service-0.3)
       ("rust-tracing" ,rust-tracing-0.1))
      #:cargo-development-inputs
      (("rust-futures-util" ,rust-futures-util-0.3))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for axum")
    (description
      (beautify-description "Core types and traits for axum"))
    (license (list license:expat))))

(define-public rust-serde_path_to_error_0_1_15
  (package
    (name "rust-serde_path_to_error")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_path_to_error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z1k6kkwi4v4psbwd91rrw9pqk90bikx4xaprzmzsffy82i59lgb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-serde" ,rust-serde_1_0_197))))
    (home-page "None")
    (synopsis "Path to the element that failed to deserialize")
    (description
      (beautify-description "Path to the element that failed to deserialize"))
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper_1_2_0
  (package
    (name "rust-hyper")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fi6k7hz5fmdph0a5r8hw50d7h2n9zxkizmafcmb65f67bblhr8q"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-futures-channel" ,rust-futures-channel-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-h2" ,rust-h2_0_4_2)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-http-body" ,rust-http-body_1_0_0)
       ("rust-httparse" ,rust-httparse-1)
       ("rust-httpdate" ,rust-httpdate-1)
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-smallvec" ,rust-smallvec_1_13_1))
      #:cargo-development-inputs
      (("rust-tokio" ,rust-tokio_1_36_0))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description
      (beautify-description "A fast and correct HTTP library."))
    (license (list license:expat))))

(define-public rust-http_1_1_0
  (package
    (name "rust-http")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n426lmcxas6h75c2cp25m933pswlrfjz10v91vc62vib2sdvf91"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-fnv" ,rust-fnv-1)
       ("rust-itoa" ,rust-itoa_1_0_10))))
    (home-page "None")
    (synopsis "A set of types for representing HTTP requests and responses.")
    (description
      (beautify-description "A set of types for representing HTTP requests and responses."))
    (license (list license:expat license:asl2.0))))

(define-public rust-http-body_1_0_0
  (package
    (name "rust-http-body")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http-body" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hyn8n3iadrbwq8y0p1rl1275s4nm49bllw5wji29g4aa3dqbb0w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-http" ,rust-http_1_1_0))))
    (home-page "None")
    (synopsis "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (description
      (beautify-description "Trait representing an asynchronous, streaming, HTTP request or response body."))
    (license (list license:expat))))

(define-public rust-bytes_1_5_0
  (package
    (name "rust-bytes")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08w2i8ac912l8vlvkv3q51cd4gr09pwlg3sjsjffcizlrb0i5gd2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Types and traits for working with bytes")
    (description
      (beautify-description "Types and traits for working with bytes"))
    (license (list license:expat))))

(define-public rust-serde_urlencoded_0_7_1
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
      (("rust-form_urlencoded" ,rust-form_urlencoded_1_2_1)
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-ryu" ,rust-ryu_1_0_17)
       ("rust-serde" ,rust-serde_1_0_197))))
    (home-page "None")
    (synopsis "`x-www-form-urlencoded` meets Serde")
    (description
      (beautify-description "`x-www-form-urlencoded` meets Serde"))
    (license (list license:expat license:asl2.0))))

(define-public rust-socket2_0_5_6
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

(define-public rust-h2_0_4_2
  (package
    (name "rust-h2")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "h2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hqr2l7kl9zqjcjdv69v9jx6v65mlbsavsyff8mr6lgqkbjk1l1i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-fnv" ,rust-fnv-1)
       ("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-futures-sink" ,rust-futures-sink-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-http" ,rust-http_1_1_0)
       ("rust-indexmap" ,rust-indexmap_2_2_5)
       ("rust-slab" ,rust-slab_0_4_9)
       ("rust-tokio-util" ,rust-tokio-util-0.7)
       ("rust-tracing" ,rust-tracing-0.1))
      #:cargo-development-inputs
      (("rust-tokio" ,rust-tokio_1_36_0))))
    (home-page "None")
    (synopsis "An HTTP/2 client and server")
    (description
      (beautify-description "An HTTP/2 client and server"))
    (license (list license:expat))))

(define-public rust-smallvec_1_13_1
  (package
    (name "rust-smallvec")
    (version "1.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mzk9j117pn3k1gabys0b7nz8cdjsx5xc6q7fwnm8r0an62d7v76"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack"))
    (license (list license:expat license:asl2.0))))

(define-public rust-slab_0_4_9
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
      (("rust-autocfg" ,rust-autocfg-1))))
    (home-page "None")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description
      (beautify-description "Pre-allocated storage for a uniform data type"))
    (license (list license:expat))))

(define-public rust-indexmap_2_2_5
  (package
    (name "rust-indexmap")
    (version "2.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x4x9zdqvlkfks3y84dsynh1p8na3nn48nn454s26rqla6fr42vv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-equivalent" ,rust-equivalent-1)
       ("rust-hashbrown" ,rust-hashbrown-0.14)
       ("rust-serde" ,rust-serde_1_0_197))))
    (home-page "None")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
      (beautify-description "A hash table with consistent order and fast iteration."))
    (license (list license:asl2.0 license:expat))))

(define-public rust-form_urlencoded_1_2_1
  (package
    (name "rust-form_urlencoded")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "form_urlencoded" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-percent-encoding" ,rust-percent-encoding-2))))
    (home-page "None")
    (synopsis "Parser and serializer for the application/x-www-form-urlencoded syntax, as used by HTML forms.")
    (description
      (beautify-description "Parser and serializer for the application/x-www-form-urlencoded syntax, as used by HTML forms."))
    (license (list license:expat license:asl2.0))))

(define-public rust-ryu_1_0_17
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

(define-public rust-hyper_0_14_28
  (package
    (name "rust-hyper")
    (version "0.14.28")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "107gkvqx4h9bl17d602zkm2dgpfq86l2dr36yzfsi8l3xcsy35mz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-futures-channel" ,rust-futures-channel-0.3)
       ("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-h2" ,rust-h2_0_3_24)
       ("rust-http" ,rust-http_0_2_12)
       ("rust-http-body" ,rust-http-body-0.4)
       ("rust-httparse" ,rust-httparse-1)
       ("rust-httpdate" ,rust-httpdate-1)
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
       ("rust-socket2" ,rust-socket2_0_5_6)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tower-service" ,rust-tower-service-0.3)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-want" ,rust-want-0.3))
      #:cargo-development-inputs
      (("rust-futures-util" ,rust-futures-util-0.3))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description
      (beautify-description "A fast and correct HTTP library."))
    (license (list license:expat))))

(define-public rust-ipnet_2_9_0
  (package
    (name "rust-ipnet")
    (version "2.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ipnet" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hzrcysgwf0knf83ahb3535hrkw63mil88iqc6kjaryfblrqylcg"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Provides types and useful methods for working with IPv4 and IPv6 network addresses, commonly called IP prefixes. The new `IpNet`, `Ipv4Net`, and `Ipv6Net` types build on the existing `IpAddr`, `Ipv4Addr`, and `Ipv6Addr` types already provided in Rust\u0027s standard library and align to their design to stay consistent. The module also provides useful traits that extend `Ipv4Addr` and `Ipv6Addr` with methods for `Add`, `Sub`, `BitAnd`, and `BitOr` operations. The module only uses stable feature so it is guaranteed to compile using the stable toolchain.")
    (description
      (beautify-description "Provides types and useful methods for working with IPv4 and IPv6 network addresses, commonly called IP prefixes. The new `IpNet`, `Ipv4Net`, and `Ipv6Net` types build on the existing `IpAddr`, `Ipv4Addr`, and `Ipv6Addr` types already provided in Rust\u0027s standard library and align to their design to stay consistent. The module also provides useful traits that extend `Ipv4Addr` and `Ipv6Addr` with methods for `Add`, `Sub`, `BitAnd`, and `BitOr` operations. The module only uses stable feature so it is guaranteed to compile using the stable toolchain."))
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror_1_0_57
  (package
    (name "rust-thiserror")
    (version "1.0.57")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jxj8y5kxgq6lcizzssnyz6a9xvssqnayp0953r7b5yjiszbqi8y"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-thiserror-impl" ,rust-thiserror-impl_1_0_57))))
    (home-page "None")
    (synopsis "derive(Error)")
    (description
      (beautify-description "derive(Error)"))
    (license (list license:expat license:asl2.0))))

(define-public rust-metrics-util_0_15_1
  (package
    (name "rust-metrics-util")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "metrics-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0glpkmrj7zkg9b290x6qxf93kmd9b4b4sbkk1fs19l8y95pfvqjd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-epoch" ,rust-crossbeam-epoch_0_9_18)
       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
       ("rust-hashbrown" ,rust-hashbrown_0_13_1)
       ("rust-metrics" ,rust-metrics_0_21_1)
       ("rust-num_cpus" ,rust-num_cpus_1_16_0)
       ("rust-quanta" ,rust-quanta_0_11_1)
       ("rust-sketches-ddsketch" ,rust-sketches-ddsketch_0_2_2))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Helper types/functions used by the metrics ecosystem.")
    (description
      (beautify-description "Helper types/functions used by the metrics ecosystem."))
    (license (list license:expat))))

(define-public rust-quanta_0_11_1
  (package
    (name "rust-quanta")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quanta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1axrw0nqc90bq671w05jd9460pmwg86c4r132mjsi4c2g8m6czm1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-mach2" ,rust-mach2-0.4)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-raw-cpuid" ,rust-raw-cpuid-10)
       ("rust-wasi" ,rust-wasi-0.11)
       ("rust-web-sys" ,rust-web-sys_0_3_69)
       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description
      (beautify-description "high-speed timing library"))
    (license (list license:expat))))

(define-public rust-http_0_2_12
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
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-fnv" ,rust-fnv-1)
       ("rust-itoa" ,rust-itoa_1_0_10))))
    (home-page "None")
    (synopsis "A set of types for representing HTTP requests and responses.")
    (description
      (beautify-description "A set of types for representing HTTP requests and responses."))
    (license (list license:expat license:asl2.0))))

(define-public rust-h2_0_3_24
  (package
    (name "rust-h2")
    (version "0.3.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "h2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jf9488b66nayxzp3iw3b2rb64y49hdbbywnv9wfwrsv14i48b5v"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-fnv" ,rust-fnv-1)
       ("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-futures-sink" ,rust-futures-sink-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-http" ,rust-http_0_2_12)
       ("rust-indexmap" ,rust-indexmap_2_2_5)
       ("rust-slab" ,rust-slab_0_4_9)
       ("rust-tokio-util" ,rust-tokio-util-0.7)
       ("rust-tracing" ,rust-tracing-0.1))
      #:cargo-development-inputs
      (("rust-tokio" ,rust-tokio_1_36_0))))
    (home-page "None")
    (synopsis "An HTTP/2 client and server")
    (description
      (beautify-description "An HTTP/2 client and server"))
    (license (list license:expat))))

(define-public rust-thiserror-impl_1_0_57
  (package
    (name "rust-thiserror-impl")
    (version "1.0.57")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "108wacybfa95akidi07ahabfwkl0sfj3srp67np5sdzgbckcnlx9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "None")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
      (beautify-description "Implementation detail of the `thiserror` crate"))
    (license (list license:expat license:asl2.0))))

(define-public rust-num_cpus_1_16_0
  (package
    (name "rust-num_cpus")
    (version "1.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_cpus" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hermit-abi" ,rust-hermit-abi_0_3_9)
       ("rust-libc" ,rust-libc_0_2_153))))
    (home-page "None")
    (synopsis "Get the number of CPUs on a machine.")
    (description
      (beautify-description "Get the number of CPUs on a machine."))
    (license (list license:expat license:asl2.0))))

(define-public rust-sketches-ddsketch_0_2_2
  (package
    (name "rust-sketches-ddsketch")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sketches-ddsketch" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p6n1v0p0773d0b5qnsnw526g7hhlb08bx95wm0zb09xnwa6qqw5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mheffner/rust-sketches-ddsketch")
    (synopsis "A direct port of the Golang DDSketch implementation.")
    (description
      (beautify-description "A direct port of the Golang DDSketch implementation."))
    (license (list license:asl2.0))))

(define-public rust-hashbrown_0_13_1
  (package
    (name "rust-hashbrown")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f602rk7pgdhw1s57g81822g7b2m5i2wibrpaqp11afk5kk8mzrk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ahash" ,rust-ahash_0_8_11))))
    (home-page "None")
    (synopsis "A Rust port of Google\u0027s SwissTable hash map")
    (description
      (beautify-description "A Rust port of Google\u0027s SwissTable hash map"))
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-epoch_0_9_18
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

(define-public rust-hermit-abi_0_3_9
  (package
    (name "rust-hermit-abi")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hermit-abi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Hermit system calls definitions.")
    (description
      (beautify-description "Hermit system calls definitions."))
    (license (list license:expat license:asl2.0))))

(define-public rust-web-sys_0_3_69
  (package
    (name "rust-web-sys")
    (version "0.3.69")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "web-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vqkxk935xa8zcnsi4bd88sb267ly2i24xl1yiq26d1n32hskbvp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-js-sys" ,rust-js-sys_0_3_69)
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_92))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/web-sys/index.html")
    (synopsis "Bindings for all Web APIs, a procedurally generated crate from WebIDL")
    (description
      (beautify-description "Bindings for all Web APIs, a procedurally generated crate from WebIDL"))
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen_0_2_92
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.92")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a4mcw13nsk3fr8fxjzf9kk1wj88xkfsmnm0pjraw01ryqfm7qjb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro_0_2_92))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.")
    (description
      (beautify-description "Easy support for interacting between JS and Rust."))
    (license (list license:expat license:asl2.0))))

(define-public rust-js-sys_0_3_69
  (package
    (name "rust-js-sys")
    (version "0.3.69")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "js-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v99rz97asnzapb0jsc3jjhvxpfxr7h7qd97yqyrf9i7viimbh99"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_92))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bindings for all JS global objects and functions in all JS environments like\nNode.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (description
      (beautify-description "Bindings for all JS global objects and functions in all JS environments like\nNode.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate."))
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro_0_2_92
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.92")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09npa1srjjabd6nfph5yc03jb26sycjlxhy0c2a1pdrpx4yq5y51"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-quote" ,rust-quote-1)
       ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support_0_2_92))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (description
      (beautify-description "Definition of the `#[wasm_bindgen]` attribute, an internal dependency"))
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-support_0_2_92
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.92")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro-support" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dqv2xs8zcyw4kjgzj84bknp2h76phmsb3n7j6hn396h4ssifkz9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52)
       ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend_0_2_92)
       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared_0_2_92))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate")
    (description
      (beautify-description "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate"))
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared_0_2_92
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.92")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15kyavsrna2cvy30kg03va257fraf9x00ny554vxngvpyaa0q6dg"))))
    (build-system cargo-build-system)
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Shared support between wasm-bindgen and wasm-bindgen cli, an internal\ndependency.")
    (description
      (beautify-description "Shared support between wasm-bindgen and wasm-bindgen cli, an internal\ndependency."))
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-backend_0_2_92
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.92")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nj7wxbi49f0rw9d44rjzms26xlw6r76b2mrggx8jfbdjrxphkb1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bumpalo" ,rust-bumpalo_3_15_3)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52)
       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared_0_2_92))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool")
    (description
      (beautify-description "Backend code generation of the wasm-bindgen tool"))
    (license (list license:expat license:asl2.0))))

(define-public rust-bumpalo_3_15_3
  (package
    (name "rust-bumpalo")
    (version "3.15.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bumpalo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nsm985rzdcnx8lmy9px1179f8yc8jarg5n8aw8jldmvf6m898cf"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A fast bump allocation arena for Rust.")
    (description
      (beautify-description "A fast bump allocation arena for Rust."))
    (license (list license:expat license:asl2.0))))

(define-public rust-arc-swap_1_7_0
  (package
    (name "rust-arc-swap")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arc-swap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ps2jh8dmhw53649yjyn3nlilm61qq0cr0b9j8dd3s11mxh00gbv"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Atomically swappable Arc")
    (description
      (beautify-description "Atomically swappable Arc"))
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-futures_0_4_42
  (package
    (name "rust-wasm-bindgen-futures")
    (version "0.4.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-futures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h322zjvpjllcpj7dahfxjsv6inkr6y0baw7nkdwivr1c4v19g3n"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-js-sys" ,rust-js-sys_0_3_69)
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_92)
       ("rust-web-sys" ,rust-web-sys_0_3_69))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bridging the gap between Rust Futures and JavaScript Promises")
    (description
      (beautify-description "Bridging the gap between Rust Futures and JavaScript Promises"))
    (license (list license:expat license:asl2.0))))

(define-public rust-encoding_rs_0_8_33
  (package
    (name "rust-encoding_rs")
    (version "0.8.33")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qa5k4a0ipdrxq4xg9amms9r9pnnfn7nfh2i9m3mw0ka563b6s3j"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1))))
    (home-page "https://docs.rs/encoding_rs/")
    (synopsis "A Gecko-oriented implementation of the Encoding Standard")
    (description
      (beautify-description "A Gecko-oriented implementation of the Encoding Standard"))
    (license (list license:bsd-3))))

(define-public rust-bitflags_2_4_2
  (package
    (name "rust-bitflags")
    (version "2.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pqd142hyqlzr7p9djxq2ff0jx07a2sb2xp9lhw69cbf80s0jmzd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_197))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
      (beautify-description "A macro to generate structures which behave like bitflags."))
    (license (list license:expat license:asl2.0))))

(define-public rust-lazy_static_1_4_0
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
    (arguments
    `(#:cargo-inputs
      (("rust-spin" ,rust-spin-0.5))))
    (home-page "None")
    (synopsis "A macro for declaring lazily evaluated statics in Rust.")
    (description
      (beautify-description "A macro for declaring lazily evaluated statics in Rust."))
    (license (list license:expat license:asl2.0))))

(define-public rust-wasite_0_1_0
  (package
    (name "rust-wasite")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasite" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nw5h9nmcl4fyf4j5d4mfdjfgvwi1cakpi349wc4zrr59wxxinmq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ardaku/wasite/blob/stable/CHANGELOG.md")
    (synopsis "WASI Terminal Environment API")
    (description
      (beautify-description "WASI Terminal Environment API"))
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-redox_syscall_0_4_1
  (package
    (name "rust-redox_syscall")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags-1))))
    (home-page "None")
    (synopsis "A Rust library to access raw Redox system calls")
    (description
      (beautify-description "A Rust library to access raw Redox system calls"))
    (license (list license:expat))))

(define-public rust-anstream_0_6_13
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

(define-public rust-env_filter_0_1_0
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
       ("rust-regex" ,rust-regex_1_10_3))))
    (home-page "None")
    (synopsis "Filter log events using environment variables")
    (description
      (beautify-description "Filter log events using environment variables"))
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-query_1_0_2
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

(define-public rust-anstyle-parse_0_2_3
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

(define-public rust-regex_1_10_3
  (package
    (name "rust-regex")
    (version "1.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05cvihqy0wgnh9i8a9y2n803n5azg2h0b7nlqy6rsvxhy00vwbdn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick-1)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-regex-automata" ,rust-regex-automata_0_4_6)
       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-automata_0_4_6
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
      (("rust-aho-corasick" ,rust-aho-corasick-1)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "None")
    (synopsis "Automata construction and matching using regular expressions.")
    (description
      (beautify-description "Automata construction and matching using regular expressions."))
    (license (list license:expat license:asl2.0))))

(define-public rust-anyhow_1_0_80
  (package
    (name "rust-anyhow")
    (version "1.0.80")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anyhow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qdlk0mbf6mycr9rxyfc0ic9n8nn5v6pgh4qf07p6qa15vjjrlss"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description
      (beautify-description "Flexible concrete Error type built on std::error::Error"))
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-normalization_0_1_23
  (package
    (name "rust-unicode-normalization")
    (version "0.1.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-normalization" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x81a50h2zxigj74b9bqjsirxxbyhmis54kg600xj213vf31cvd5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-tinyvec" ,rust-tinyvec-1))))
    (home-page "https://github.com/unicode-rs/unicode-normalization")
    (synopsis "This crate provides functions for normalization of\nUnicode strings, including Canonical and Compatible\nDecomposition and Recomposition, as described in\nUnicode Standard Annex #15.")
    (description
      (beautify-description "This crate provides functions for normalization of\nUnicode strings, including Canonical and Compatible\nDecomposition and Recomposition, as described in\nUnicode Standard Annex #15."))
    (license (list license:expat license:asl2.0))))

(define-public rust-lru_0_12_3
  (package
    (name "rust-lru")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lru" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p5hryc967wdh56q9wzb2x9gdqy3yd0sqmnb2fcf7z28wrsjw9nk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hashbrown" ,rust-hashbrown-0.14))))
    (home-page "https://github.com/jeromefroe/lru-rs")
    (synopsis "A LRU cache implementation")
    (description
      (beautify-description "A LRU cache implementation"))
    (license (list license:expat))))

(define-public rust-stability_0_1_1
  (package
    (name "rust-stability")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stability" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kn3vcicmpg8bnyalp15i2j0dbv6c0wc62022bcs58jdi5vv3lgb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "Rust API stability attributes for the rest of us.")
    (description
      (beautify-description "Rust API stability attributes for the rest of us."))
    (license (list license:expat))))

(define-public rust-rtoolbox_0_0_2
  (package
    (name "rust-rtoolbox")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rtoolbox" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03n9z8x353kylxhr9im8zawcisnmid3jiqrs8rbdn313cd7d4iy2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_153)
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "None")
    (synopsis "Utility functions for other crates, no backwards compatibility guarantees.")
    (description
      (beautify-description "Utility functions for other crates, no backwards compatibility guarantees."))
    (license (list license:asl2.0))))

(define-public rust-number_prefix_0_4_0
  (package
    (name "rust-number_prefix")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "number_prefix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Library for numeric prefixes (kilo, giga, kibi).")
    (description
      (beautify-description "Library for numeric prefixes (kilo, giga, kibi)."))
    (license (list license:expat))))

(define-public rust-console_0_15_8
  (package
    (name "rust-console")
    (version "0.15.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "console" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sz4nl9nz8pkmapqni6py7jxzi7nzqjxzb3ya4kxvmkb0zy867qf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-encode_unicode" ,rust-encode_unicode_0_3_6)
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-unicode-width" ,rust-unicode-width-0.1)
       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/console-rs/console")
    (synopsis "A terminal and console abstraction for Rust")
    (description
      (beautify-description "A terminal and console abstraction for Rust"))
    (license (list license:expat))))

(define-public rust-encode_unicode_0_3_6
  (package
    (name "rust-encode_unicode")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encode_unicode" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07w3vzrhxh9lpjgsg2y5bwzfar2aq35mdznvcp3zjl0ssj7d4mx3"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.")
    (description
      (beautify-description "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16."))
    (license (list license:expat license:asl2.0))))

(define-public rust-linux-raw-sys_0_4_13
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

(define-public rust-serde_derive_1_0_197
  (package
    (name "rust-serde_derive")
    (version "1.0.197")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02v1x0sdv8qy06lpr6by4ar1n3jz3hmab15cgimpzhgd895v7c3y"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx_0_7_3
  (package
    (name "rust-sqlx")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kv3hyx7izmmsjqh3l47zrfhjlcblpg20cvnk7pr8dm7klkkr86v"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-sqlx-core" ,rust-sqlx-core_0_7_3)
       ("rust-sqlx-macros" ,rust-sqlx-macros_0_7_3)
       ("rust-sqlx-mysql" ,rust-sqlx-mysql_0_7_3)
       ("rust-sqlx-postgres" ,rust-sqlx-postgres_0_7_3)
       ("rust-sqlx-sqlite" ,rust-sqlx-sqlite_0_7_3))))
    (home-page "None")
    (synopsis "The Rust SQL Toolkit. An async, pure Rust SQL crate featuring compile-time checked queries without a DSL. Supports PostgreSQL, MySQL, and SQLite.")
    (description
      (beautify-description "The Rust SQL Toolkit. An async, pure Rust SQL crate featuring compile-time checked queries without a DSL. Supports PostgreSQL, MySQL, and SQLite."))
    (license (list license:expat license:asl2.0))))

(define-public rust-pretty_assertions_1_4_0
  (package
    (name "rust-pretty_assertions")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty_assertions" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rmsnqlpmpfjp5gyi31xgc48kdhc1kqn246bnc494nwadhdfwz5g"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-diff" ,rust-diff-0.1)
       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "None")
    (synopsis "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
    (description
      (beautify-description "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs."))
    (license (list license:expat license:asl2.0))))

(define-public rust-typed-builder_0_18_1
  (package
    (name "rust-typed-builder")
    (version "0.18.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typed-builder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0glkkhmdgyyxyvyq5zriz5a8h3ybb0j0ks3hhwbcp4qv0548fka4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-typed-builder-macro" ,rust-typed-builder-macro_0_18_1))))
    (home-page "None")
    (synopsis "Compile-time type-checked builder derive")
    (description
      (beautify-description "Compile-time type-checked builder derive"))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-mysql_0_7_3
  (package
    (name "rust-sqlx-mysql")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-mysql" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "190ygz5a3pqcd9vvqjv2i4r1xh8vi53j4272yrld07zpblwrawg3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atoi" ,rust-atoi-2)
       ("rust-base64" ,rust-base64_0_21_7)
       ("rust-bitflags" ,rust-bitflags_2_4_2)
       ("rust-byteorder" ,rust-byteorder-1)
       ("rust-bytes" ,rust-bytes_1_5_0)
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
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-md-5" ,rust-md-5_0_10_6)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-rsa" ,rust-rsa-0.9)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-sha1" ,rust-sha1-0.10)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-smallvec" ,rust-smallvec_1_13_1)
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)
       ("rust-stringprep" ,rust-stringprep_0_1_4)
       ("rust-thiserror" ,rust-thiserror_1_0_57)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-uuid" ,rust-uuid_1_7_0)
       ("rust-whoami" ,rust-whoami_1_5_1))))
    (home-page "None")
    (synopsis "MySQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
      (beautify-description "MySQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details."))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-sqlite_0_7_3
  (package
    (name "rust-sqlx-sqlite")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-sqlite" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "143laha7wf8dmi0xwycwqmvxdcnb25dq7jnqrsgvmis8v6vpc291"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atoi" ,rust-atoi-2)
       ("rust-flume" ,rust-flume_0_11_0)
       ("rust-futures-channel" ,rust-futures-channel-0.3)
       ("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-futures-executor" ,rust-futures-executor-0.3)
       ("rust-futures-intrusive" ,rust-futures-intrusive_0_5_0)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.27)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-regex" ,rust-regex_1_10_3)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-url" ,rust-url-2)
       ("rust-urlencoding" ,rust-urlencoding-2)
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "None")
    (synopsis "SQLite driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
      (beautify-description "SQLite driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details."))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-postgres_0_7_3
  (package
    (name "rust-sqlx-postgres")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-postgres" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "090wm9s6mm53ggn1xwr183cnn8yxly8rgcksdk4hrlfcnz1hmb6n"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atoi" ,rust-atoi-2)
       ("rust-base64" ,rust-base64_0_21_7)
       ("rust-bitflags" ,rust-bitflags_2_4_2)
       ("rust-byteorder" ,rust-byteorder-1)
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
       ("rust-home" ,rust-home_0_5_9)
       ("rust-itoa" ,rust-itoa_1_0_10)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-md-5" ,rust-md-5_0_10_6)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-sha1" ,rust-sha1-0.10)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-smallvec" ,rust-smallvec_1_13_1)
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)
       ("rust-stringprep" ,rust-stringprep_0_1_4)
       ("rust-thiserror" ,rust-thiserror_1_0_57)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-uuid" ,rust-uuid_1_7_0)
       ("rust-whoami" ,rust-whoami_1_5_1))))
    (home-page "None")
    (synopsis "PostgreSQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
      (beautify-description "PostgreSQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details."))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-macros_0_7_3
  (package
    (name "rust-sqlx-macros")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19gjwisiym07q7ibkp9nkvvbywjh0r5rc572msvzyzadvh01r5l9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)
       ("rust-sqlx-macros-core" ,rust-sqlx-macros-core_0_7_3)
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
      (beautify-description "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly."))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-core_0_7_3
  (package
    (name "rust-sqlx-core")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gdz44yb9qwxv4xl4hv6w4vbqx0zzdlzsf9j9gcj1qir6wy0ljyq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ahash" ,rust-ahash_0_8_11)
       ("rust-atoi" ,rust-atoi-2)
       ("rust-byteorder" ,rust-byteorder-1)
       ("rust-bytes" ,rust-bytes_1_5_0)
       ("rust-crc" ,rust-crc-3)
       ("rust-crossbeam-queue" ,rust-crossbeam-queue_0_3_11)
       ("rust-dotenvy" ,rust-dotenvy-0.15)
       ("rust-either" ,rust-either-1)
       ("rust-event-listener" ,rust-event-listener-2)
       ("rust-futures-channel" ,rust-futures-channel-0.3)
       ("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-futures-intrusive" ,rust-futures-intrusive_0_5_0)
       ("rust-futures-io" ,rust-futures-io-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-hashlink" ,rust-hashlink_0_8_4)
       ("rust-hex" ,rust-hex-0.4)
       ("rust-indexmap" ,rust-indexmap_2_2_5)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-paste" ,rust-paste-1)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-rustls" ,rust-rustls-0.21)
       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-smallvec" ,rust-smallvec_1_13_1)
       ("rust-sqlformat" ,rust-sqlformat_0_2_3)
       ("rust-thiserror" ,rust-thiserror_1_0_57)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-url" ,rust-url-2)
       ("rust-uuid" ,rust-uuid_1_7_0)
       ("rust-webpki-roots" ,rust-webpki-roots-0.25))))
    (home-page "None")
    (synopsis "Core of SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
      (beautify-description "Core of SQLx, the rust SQL toolkit. Not intended to be used directly."))
    (license (list license:expat license:asl2.0))))

(define-public rust-md-5_0_10_6
  (package
    (name "rust-md-5")
    (version "0.10.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md-5" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-digest" ,rust-digest-0.10))))
    (home-page "None")
    (synopsis "MD5 hash function")
    (description
      (beautify-description "MD5 hash function"))
    (license (list license:expat license:asl2.0))))

(define-public rust-stringprep_0_1_4
  (package
    (name "rust-stringprep")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stringprep" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rkfsf7riynsmqj3hbldfrvmna0i9chx2sz39qdpl40s4d7dfhdv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-finl_unicode" ,rust-finl_unicode_1_2_0)
       ("rust-unicode-bidi" ,rust-unicode-bidi_0_3_15)
       ("rust-unicode-normalization" ,rust-unicode-normalization_0_1_23))))
    (home-page "None")
    (synopsis "An implementation of the stringprep algorithm")
    (description
      (beautify-description "An implementation of the stringprep algorithm"))
    (license (list license:expat license:asl2.0))))

(define-public rust-finl_unicode_1_2_0
  (package
    (name "rust-finl_unicode")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "finl_unicode" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ipdx778849czik798sjbgk5yhwxqybydac18d2g9jb20dxdrkwg"))))
    (build-system cargo-build-system)
    (home-page "https://finl.xyz")
    (synopsis "Library for handling Unicode functionality for finl (categories and grapheme segmentation)")
    (description
      (beautify-description "Library for handling Unicode functionality for finl (categories and grapheme segmentation)"))
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-bidi_0_3_15
  (package
    (name "rust-unicode-bidi")
    (version "0.3.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-bidi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xcdxm7h0ydyprwpcbh436rbs6s6lph7f3gr527lzgv6lw053y88"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Implementation of the Unicode Bidirectional Algorithm")
    (description
      (beautify-description "Implementation of the Unicode Bidirectional Algorithm"))
    (license (list license:expat license:asl2.0))))

(define-public rust-flume_0_11_0
  (package
    (name "rust-flume")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flume" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10girdbqn77wi802pdh55lwbmymy437k7kklnvj12aaiwaflbb2m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-futures-sink" ,rust-futures-sink-0.3)
       ("rust-spin" ,rust-spin-0.9))))
    (home-page "None")
    (synopsis "A blazingly fast multi-producer channel")
    (description
      (beautify-description "A blazingly fast multi-producer channel"))
    (license (list license:asl2.0 license:expat))))

(define-public rust-futures-intrusive_0_5_0
  (package
    (name "rust-futures-intrusive")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-intrusive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vwm08d1pli6bdaj0i7xhk3476qlx4pll6i0w03gzdnh7lh0r4qx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-lock_api" ,rust-lock_api_0_4_11)
       ("rust-parking_lot" ,rust-parking_lot_0_12_1))))
    (home-page "https://github.com/Matthias247/futures-intrusive")
    (synopsis "Futures based on intrusive data structures - for std and no-std environments.")
    (description
      (beautify-description "Futures based on intrusive data structures - for std and no-std environments."))
    (license (list license:expat license:asl2.0))))

(define-public rust-parking_lot_0_12_1
  (package
    (name "rust-parking_lot")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lock_api" ,rust-lock_api_0_4_11)
       ("rust-parking_lot_core" ,rust-parking_lot_core_0_9_9))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license (list license:expat license:asl2.0))))

(define-public rust-lock_api_0_4_11
  (package
    (name "rust-lock_api")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iggx0h4jx63xm35861106af3jkxq06fpqhpkhgw0axi2n38y5iw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg-1)
       ("rust-scopeguard" ,rust-scopeguard_1_2_0))))
    (home-page "None")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      (beautify-description "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std."))
    (license (list license:expat license:asl2.0))))

(define-public rust-parking_lot_core_0_9_9
  (package
    (name "rust-parking_lot_core")
    (version "0.9.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13h0imw1aq86wj28gxkblhkzx6z1gk8q18n0v76qmmj6cliajhjc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-redox_syscall" ,rust-redox_syscall_0_4_1)
       ("rust-smallvec" ,rust-smallvec_1_13_1)
       ("rust-windows-targets" ,rust-windows-targets_0_48_5))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-targets_0_48_5
  (package
    (name "rust-windows-targets")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows-targets" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows_aarch64_gnullvm" ,rust-windows_aarch64_gnullvm_0_48_5)
       ("rust-windows_aarch64_msvc" ,rust-windows_aarch64_msvc_0_48_5)
       ("rust-windows_i686_gnu" ,rust-windows_i686_gnu_0_48_5)
       ("rust-windows_i686_msvc" ,rust-windows_i686_msvc_0_48_5)
       ("rust-windows_x86_64_gnu" ,rust-windows_x86_64_gnu_0_48_5)
       ("rust-windows_x86_64_gnullvm" ,rust-windows_x86_64_gnullvm_0_48_5)
       ("rust-windows_x86_64_msvc" ,rust-windows_x86_64_msvc_0_48_5))))
    (home-page "None")
    (synopsis "Import libs for Windows")
    (description
      (beautify-description "Import libs for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_aarch64_msvc_0_48_5
  (package
    (name "rust-windows_aarch64_msvc")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_aarch64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_x86_64_gnu_0_48_5
  (package
    (name "rust-windows_x86_64_gnu")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_i686_gnu_0_48_5
  (package
    (name "rust-windows_i686_gnu")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_aarch64_gnullvm_0_48_5
  (package
    (name "rust-windows_aarch64_gnullvm")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_aarch64_gnullvm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_i686_msvc_0_48_5
  (package
    (name "rust-windows_i686_msvc")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_x86_64_gnullvm_0_48_5
  (package
    (name "rust-windows_x86_64_gnullvm")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_gnullvm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_x86_64_msvc_0_48_5
  (package
    (name "rust-windows_x86_64_msvc")
    (version "0.48.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-scopeguard_1_2_0
  (package
    (name "rust-scopeguard")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies.")
    (description
      (beautify-description "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies."))
    (license (list license:expat license:asl2.0))))

(define-public rust-home_0_5_9
  (package
    (name "rust-home")
    (version "0.5.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "home" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19grxyg35rqfd802pcc9ys1q3lafzlcjcv2pl2s5q8xpyr5kblg3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "None")
    (synopsis "Shared definitions of home directories.")
    (description
      (beautify-description "Shared definitions of home directories."))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-macros-core_0_7_3
  (package
    (name "rust-sqlx-macros-core")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-macros-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0h88wahkxa6nam536lhwr1y0yxlr6la8b1x0hs0n88v790clbgfh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atomic-write-file" ,rust-atomic-write-file_0_1_3)
       ("rust-dotenvy" ,rust-dotenvy-0.15)
       ("rust-either" ,rust-either-1)
       ("rust-heck" ,rust-heck-0.4)
       ("rust-hex" ,rust-hex-0.4)
       ("rust-once_cell" ,rust-once_cell_1_19_0)
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)
       ("rust-sqlx-mysql" ,rust-sqlx-mysql_0_7_3)
       ("rust-sqlx-postgres" ,rust-sqlx-postgres_0_7_3)
       ("rust-sqlx-sqlite" ,rust-sqlx-sqlite_0_7_3)
       ("rust-syn" ,rust-syn-1)
       ("rust-tempfile" ,rust-tempfile_3_10_1)
       ("rust-tokio" ,rust-tokio_1_36_0)
       ("rust-url" ,rust-url-2))))
    (home-page "None")
    (synopsis "Macro support core for SQLx, the Rust SQL toolkit. Not intended to be used directly.")
    (description
      (beautify-description "Macro support core for SQLx, the Rust SQL toolkit. Not intended to be used directly."))
    (license (list license:expat license:asl2.0))))

(define-public rust-atomic-write-file_0_1_3
  (package
    (name "rust-atomic-write-file")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atomic-write-file" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02zv9i5hvcf3ji0nlkgd6a0rrcvq1y2dhnw4zrj8sr5zg6r4s858"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nix" ,rust-nix_0_28_0)
       ("rust-rand" ,rust-rand-0.8))))
    (home-page "None")
    (synopsis "Write files atomically to a file system")
    (description
      (beautify-description "Write files atomically to a file system"))
    (license (list license:bsd-3))))

(define-public rust-tempfile_3_10_1
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
       ("rust-fastrand" ,rust-fastrand-2)
       ("rust-rustix" ,rust-rustix_0_38_31)
       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "A library for managing temporary files and directories.")
    (description
      (beautify-description "A library for managing temporary files and directories."))
    (license (list license:expat license:asl2.0))))

(define-public rust-nix_0_28_0
  (package
    (name "rust-nix")
    (version "0.28.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r0rylax4ycx3iqakwjvaa178jrrwiiwghcw95ndzy72zk25c8db"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_2_4_2)
       ("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-cfg_aliases" ,rust-cfg_aliases_0_1_1)
       ("rust-libc" ,rust-libc_0_2_153))))
    (home-page "None")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description
      (beautify-description "Rust friendly bindings to *nix APIs"))
    (license (list license:expat))))

(define-public rust-cfg_aliases_0_1_1
  (package
    (name "rust-cfg_aliases")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg_aliases" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17p821nc6jm830vzl2lmwz60g3a30hcm33nk6l257i1rjdqw85px"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/katharostech/cfg_aliases")
    (synopsis "A tiny utility to help save you a lot of effort with long winded `#[cfg()]` checks.")
    (description
      (beautify-description "A tiny utility to help save you a lot of effort with long winded `#[cfg()]` checks."))
    (license (list license:expat))))

(define-public rust-crossbeam-queue_0_3_11
  (package
    (name "rust-crossbeam-queue")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-queue" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0d8y8y3z48r9javzj67v3p2yfswd278myz1j9vzc4sp7snslc0yz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-queue")
    (synopsis "Concurrent queues")
    (description
      (beautify-description "Concurrent queues"))
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlformat_0_2_3
  (package
    (name "rust-sqlformat")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlformat" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v0p70wjdshj18zgjjac9xlx8hmpx33xhq7g8x9rg4s4gjyvg0ff"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-itertools" ,rust-itertools_0_12_1)
       ("rust-nom" ,rust-nom-7)
       ("rust-unicode_categories" ,rust-unicode_categories_0_1_1))))
    (home-page "https://github.com/shssoichiro/sqlformat-rs")
    (synopsis "Formats whitespace in a SQL string to make it easier to read")
    (description
      (beautify-description "Formats whitespace in a SQL string to make it easier to read"))
    (license (list license:expat license:asl2.0))))

(define-public rust-hashlink_0_8_4
  (package
    (name "rust-hashlink")
    (version "0.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashlink" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xy8agkyp0llbqk9fcffc1xblayrrywlyrm2a7v93x8zygm4y2g8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hashbrown" ,rust-hashbrown-0.14))))
    (home-page "None")
    (synopsis "HashMap-like containers that hold their key-value pairs in a user controllable order")
    (description
      (beautify-description "HashMap-like containers that hold their key-value pairs in a user controllable order"))
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode_categories_0_1_1
  (package
    (name "rust-unicode_categories")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode_categories" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kp1d7fryxxm7hqywbk88yb9d1avsam9sg76xh36k5qx2arj9v1r"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Query Unicode category membership for chars")
    (description
      (beautify-description "Query Unicode category membership for chars"))
    (license (list license:expat license:asl2.0))))

(define-public rust-typed-builder-macro_0_18_1
  (package
    (name "rust-typed-builder-macro")
    (version "0.18.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typed-builder-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lhk4qsj9yrqkprv9wf0wdyalvl9cv7dwszkms05djcf4f43nfsn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "None")
    (synopsis "Compile-time type-checked builder derive")
    (description
      (beautify-description "Compile-time type-checked builder derive"))
    (license (list license:expat license:asl2.0))))

(define-public rust-clap_builder_4_5_1
  (package
    (name "rust-clap_builder")
    (version "4.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_builder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1syiyfi26w9rmadhhzy65sfdr8vrylczc6yy5q6gp2nnva8p6glz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-anstream" ,rust-anstream_0_6_13)
       ("rust-anstyle" ,rust-anstyle-1)
       ("rust-clap_lex" ,rust-clap_lex_0_7_0)
       ("rust-strsim" ,rust-strsim_0_11_0))))
    (home-page "None")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (list license:expat license:asl2.0))))

(define-public rust-clap_derive_4_5_0
  (package
    (name "rust-clap_derive")
    (version "4.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ixs8wzw7aqfdk4x150jd7j09r9gm5x0icwd4jw863szim9w0yrh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-heck" ,rust-heck-0.4)
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "None")
    (synopsis "Parse command line argument by defining a struct, derive crate.")
    (description
      (beautify-description "Parse command line argument by defining a struct, derive crate."))
    (license (list license:expat license:asl2.0))))

(define-public rust-strsim_0_11_0
  (package
    (name "rust-strsim")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00gsdp2x1gkkxsbjxgrjyil2hsbdg49bwv8q2y1f406dwk4p7q2y"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rapidfuzz/strsim-rs")
    (synopsis "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice.")
    (description
      (beautify-description "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice."))
    (license (list license:expat))))

(define-public rust-clap_lex_0_7_0
  (package
    (name "rust-clap_lex")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_lex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kh1sckgq71kay2rrr149pl9gbsrvyccsq6xm5xpnq0cxnyqzk4q"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Minimal, flexible command line parser")
    (description
      (beautify-description "Minimal, flexible command line parser"))
    (license (list license:expat license:asl2.0))))

(define-public rust-atomic_0_5_3
  (package
    (name "rust-atomic")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atomic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fhc6ayg4d5vw1cibqwff15d45fc5448zg9i3drk42k5phsdp6y5"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Generic Atomic\u003cT\u003e wrapper type")
    (description
      (beautify-description "Generic Atomic\u003cT\u003e wrapper type"))
    (license (list license:asl2.0 license:expat))))

(define-public rust-rayon_1_9_0
  (package
    (name "rust-rayon")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gdk945j52vq3zx5vb4yzc3yyz19bf2vs8kh47pg7r46pk8kx5p4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-either" ,rust-either-1)
       ("rust-rayon-core" ,rust-rayon-core_1_12_1))))
    (home-page "None")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description
      (beautify-description "Simple work-stealing parallelism for Rust"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_0_52_0
  (package
    (name "rust-windows")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gnh210qjlprpd1szaq04rjm1zqgdm9j7l9absg0kawi2rwm72p4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows-core" ,rust-windows-core_0_52_0)
       ("rust-windows-targets" ,rust-windows-targets_0_52_4))))
    (home-page "None")
    (synopsis "Rust for Windows")
    (description
      (beautify-description "Rust for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-rayon-core_1_12_1
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

(define-public rust-crossbeam-deque_0_8_5
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

(define-public rust-windows-core_0_52_0
  (package
    (name "rust-windows-core")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows-targets" ,rust-windows-targets_0_52_4))))
    (home-page "None")
    (synopsis "Rust for Windows")
    (description
      (beautify-description "Rust for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-targets_0_52_4
  (package
    (name "rust-windows-targets")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows-targets" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06sdd7fin3dj9cmlg6n1dw0n1l10jhn9b8ckz1cqf0drb9z7plvx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows_aarch64_gnullvm" ,rust-windows_aarch64_gnullvm_0_52_4)
       ("rust-windows_aarch64_msvc" ,rust-windows_aarch64_msvc_0_52_4)
       ("rust-windows_i686_gnu" ,rust-windows_i686_gnu_0_52_4)
       ("rust-windows_i686_msvc" ,rust-windows_i686_msvc_0_52_4)
       ("rust-windows_x86_64_gnu" ,rust-windows_x86_64_gnu_0_52_4)
       ("rust-windows_x86_64_gnullvm" ,rust-windows_x86_64_gnullvm_0_52_4)
       ("rust-windows_x86_64_msvc" ,rust-windows_x86_64_msvc_0_52_4))))
    (home-page "None")
    (synopsis "Import libs for Windows")
    (description
      (beautify-description "Import libs for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_i686_msvc_0_52_4
  (package
    (name "rust-windows_i686_msvc")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00lfzw88dkf3fdcf2hpfhp74i9pwbp7rwnj1nhy79vavksifj58m"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_aarch64_msvc_0_52_4
  (package
    (name "rust-windows_aarch64_msvc")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_aarch64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xdn6db0rk8idn7dxsyflixq2dbj9x60kzdzal5rkxmwsffjb7ys"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_x86_64_gnullvm_0_52_4
  (package
    (name "rust-windows_x86_64_gnullvm")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_gnullvm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xr13xxakp14hs4v4hg2ynjcv7wrzr3hg7zk5agglj8v8pr7kjkp"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_x86_64_gnu_0_52_4
  (package
    (name "rust-windows_x86_64_gnu")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00qs6x33bf9lai2q68faxl56cszbv7mf7zqlslmc1778j0ahkvjy"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_x86_64_msvc_0_52_4
  (package
    (name "rust-windows_x86_64_msvc")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n0yc7xiv9iki1j3xl8nxlwwkr7dzsnwwvycvgxxv81d5bjm5drj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_aarch64_gnullvm_0_52_4
  (package
    (name "rust-windows_aarch64_gnullvm")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_aarch64_gnullvm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jfam5qfngg8v1syxklnvy8la94b5igm7klkrk8z5ik5qgs6rx5w"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-windows_i686_gnu_0_52_4
  (package
    (name "rust-windows_i686_gnu")
    (version "0.52.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lq1g35sbj55ms86by4c080jcqrlfjy9bw5r4mgrkq4riwkdhx5l"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define-public rust-mio_0_8_11
  (package
    (name "rust-mio")
    (version "0.8.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_153)
       ("rust-log" ,rust-log_0_4_21)
       ("rust-wasi" ,rust-wasi-0.11)
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking I/O.")
    (description
      (beautify-description "Lightweight non-blocking I/O."))
    (license (list license:expat))))

(define-public rust-backtrace_0_3_69
  (package
    (name "rust-backtrace")
    (version "0.3.69")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dsq23dhw4pfndkx2nsa1ml2g31idm7ss7ljxp8d57avygivg290"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-addr2line" ,rust-addr2line_0_21_0)
       ("rust-cc" ,rust-cc_1_0_89)
       ("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-libc" ,rust-libc_0_2_153)
       ("rust-miniz_oxide" ,rust-miniz_oxide_0_7_2)
       ("rust-object" ,rust-object_0_32_2)
       ("rust-rustc-demangle" ,rust-rustc-demangle_0_1_23))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis "A library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (description
      (beautify-description "A library to acquire a stack trace (backtrace) at runtime in a Rust program."))
    (license (list license:expat license:asl2.0))))

(define-public rust-signal-hook-registry_1_4_1
  (package
    (name "rust-signal-hook-registry")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "signal-hook-registry" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18crkkw5k82bvcx088xlf5g4n3772m24qhzgfan80nda7d3rn8nq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_153))))
    (home-page "None")
    (synopsis "Backend crate for signal-hook")
    (description
      (beautify-description "Backend crate for signal-hook"))
    (license (list license:asl2.0 license:expat))))

(define-public rust-object_0_32_2
  (package
    (name "rust-object")
    (version "0.32.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "object" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hc4cjwyngiy6k51hlzrlsxgv5z25vv7c2cp0ky1lckfic0259m6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr-2))))
    (home-page "None")
    (synopsis "A unified interface for reading and writing object file formats.")
    (description
      (beautify-description "A unified interface for reading and writing object file formats."))
    (license (list license:asl2.0 license:expat))))

(define-public rust-addr2line_0_21_0
  (package
    (name "rust-addr2line")
    (version "0.21.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "addr2line" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jx0k3iwyqr8klqbzk6kjvr496yd94aspis10vwsj5wy7gib4c4a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-gimli" ,rust-gimli_0_28_1))))
    (home-page "None")
    (synopsis "A cross-platform symbolication library written in Rust, using `gimli`")
    (description
      (beautify-description "A cross-platform symbolication library written in Rust, using `gimli`"))
    (license (list license:asl2.0 license:expat))))

(define-public rust-cc_1_0_89
  (package
    (name "rust-cc")
    (version "1.0.89")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08ypcdgh6253hgh3z1ir7jfd9k4ff3v64546nbak0bq1m9x8zfm0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode.")
    (description
      (beautify-description "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode."))
    (license (list license:expat license:asl2.0))))

(define-public rust-miniz_oxide_0_7_2
  (package
    (name "rust-miniz_oxide")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19qlxb21s6kabgqq61mk7kd1qk2invyygj076jz6i1gj2lz1z0cx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-adler" ,rust-adler-1))))
    (home-page "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (description
      (beautify-description "DEFLATE compression and decompression library rewritten in Rust based on miniz"))
    (license (list license:expat license:zlib license:asl2.0))))

(define-public rust-rustc-demangle_0_1_23
  (package
    (name "rust-rustc-demangle")
    (version "0.1.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xnbk2bmyzshacjm2g1kd4zzv2y2az14bw3sjccq5qkpmsfvn9nn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/rustc-demangle")
    (synopsis "Rust compiler symbol demangling.")
    (description
      (beautify-description "Rust compiler symbol demangling."))
    (license (list license:expat license:asl2.0))))

(define-public rust-gimli_0_28_1
  (package
    (name "rust-gimli")
    (version "0.28.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gimli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lv23wc8rxvmjia3mcxc6hj9vkqnv1bqq0h8nzjcgf71mrxx6wa2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library for reading and writing the DWARF debugging format.")
    (description
      (beautify-description "A library for reading and writing the DWARF debugging format."))
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto_secretbox_0_1_1
  (package
    (name "rust-crypto_secretbox")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto_secretbox" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qa1w5s8dbyb88269zrmvbnillqahz394pl07bsds6gpmn3wzmmr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aead" ,rust-aead_0_5_2)
       ("rust-cipher" ,rust-cipher-0.4)
       ("rust-generic-array" ,rust-generic-array-0.14)
       ("rust-poly1305" ,rust-poly1305-0.8)
       ("rust-salsa20" ,rust-salsa20-0.10)
       ("rust-subtle" ,rust-subtle-2)
       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "None")
    (synopsis "Pure Rust implementation of the XSalsa20Poly1305 (a.k.a. NaCl crypto_secretbox)\nauthenticated encryption cipher as well as the libsodium variant of\nXChaCha20Poly1305")
    (description
      (beautify-description "Pure Rust implementation of the XSalsa20Poly1305 (a.k.a. NaCl crypto_secretbox)\nauthenticated encryption cipher as well as the libsodium variant of\nXChaCha20Poly1305"))
    (license (list license:asl2.0 license:expat))))

(define-public rust-aead_0_5_2
  (package
    (name "rust-aead")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aead" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c32aviraqag7926xcb9sybdm36v5vh9gnxpn4pxdwjc50zl28ni"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crypto-common" ,rust-crypto-common-0.1)
       ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "None")
    (synopsis "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms,\nsuch as AES-GCM as ChaCha20Poly1305, which provide a high-level API")
    (description
      (beautify-description "Traits for Authenticated Encryption with Associated Data (AEAD) algorithms,\nsuch as AES-GCM as ChaCha20Poly1305, which provide a high-level API"))
    (license (list license:expat license:asl2.0))))

(define-public rust-num_threads_0_1_7
  (package
    (name "rust-num_threads")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_threads" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ngajbmhrgyhzrlc4d5ga9ych1vrfcvfsiqz6zv0h2dpr2wrhwsw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_153))))
    (home-page "None")
    (synopsis "A minimal library that determines the number of running threads for the current process.")
    (description
      (beautify-description "A minimal library that determines the number of running threads for the current process."))
    (license (list license:expat license:asl2.0))))

(define-public rust-time-macros_0_2_17
  (package
    (name "rust-time-macros")
    (version "0.2.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x3pahhk2751c6kqqq9dk6lz0gydbnxr44q01wpjlrz687ps78vv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-conv" ,rust-num-conv_0_1_0)
       ("rust-time-core" ,rust-time-core-0.1))))
    (home-page "None")
    (synopsis "Procedural macros for the time crate.\n    This crate is an implementation detail and should not be relied upon directly.")
    (description
      (beautify-description "Procedural macros for the time crate.\n    This crate is an implementation detail and should not be relied upon directly."))
    (license (list license:expat license:asl2.0))))

(define-public rust-num-conv_0_1_0
  (package
    (name "rust-num-conv")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-conv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "`num_conv` is a crate to convert between integer types without using `as` casts. This provides\nbetter certainty when refactoring, makes the exact behavior of code more explicit, and allows using\nturbofish syntax.")
    (description
      (beautify-description "`num_conv` is a crate to convert between integer types without using `as` casts. This provides\nbetter certainty when refactoring, makes the exact behavior of code more explicit, and allows using\nturbofish syntax."))
    (license (list license:expat license:asl2.0))))

(define-public rust-deranged_0_3_11
  (package
    (name "rust-deranged")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "deranged" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-powerfmt" ,rust-powerfmt-0.2)
       ("rust-serde" ,rust-serde_1_0_197))))
    (home-page "None")
    (synopsis "Ranged integers")
    (description
      (beautify-description "Ranged integers"))
    (license (list license:expat license:asl2.0))))

(define-public rust-minspan_0_1_1
  (package
    (name "rust-minspan")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minspan" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s7lh0ryq0kk6sm6z5f2ikqq437xca0gzc61ds80pbh8qdxa2s8j"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "a package for determining the minimum span of one vector within another")
    (description
      (beautify-description "a package for determining the minimum span of one vector within another"))
    (license (list license:expat))))

(define-public rust-serde_regex_1_1_0
  (package
    (name "rust-serde_regex")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-regex" ,rust-regex_1_10_3)
       ("rust-serde" ,rust-serde_1_0_197))))
    (home-page "https://github.com/tailhook/serde-regex")
    (synopsis "A serde wrapper that (de)serializes regex as strings")
    (description
      (beautify-description "A serde wrapper that (de)serializes regex as strings"))
    (license (list license:expat license:asl2.0))))

(define-public rust-rusty_paseto_0_6_1
  (package
    (name "rust-rusty_paseto")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rusty_paseto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i1nri0khza9mi716pgx1kvnjs5f3lka1xhqmqgh61gpyjgvbb8s"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64-0.13)
       ("rust-blake2" ,rust-blake2-0.9)
       ("rust-chacha20" ,rust-chacha20_0_8_2)
       ("rust-ed25519-dalek" ,rust-ed25519-dalek_2_1_1)
       ("rust-hex" ,rust-hex-0.4)
       ("rust-iso8601" ,rust-iso8601-0.4)
       ("rust-ring" ,rust-ring_0_17_8)
       ("rust-thiserror" ,rust-thiserror_1_0_57)
       ("rust-time" ,rust-time_0_3_34)
       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "None")
    (synopsis "A type-driven, ergonomic alternative to JWT for secure stateless PASETO tokens.")
    (description
      (beautify-description "A type-driven, ergonomic alternative to JWT for secure stateless PASETO tokens."))
    (license (list license:expat license:asl2.0))))

(define-public rust-rusty_paserk_0_3_0
  (package
    (name "rust-rusty_paserk")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rusty_paserk" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v70nbn7f5dwwngnhsqm407y61rdgrjz82nrlih2wycygzh73m2n"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-argon2" ,rust-argon2_0_5_3)
       ("rust-base64" ,rust-base64-0.13)
       ("rust-base64ct" ,rust-base64ct-1)
       ("rust-blake2" ,rust-blake2-0.10)
       ("rust-chacha20" ,rust-chacha20-0.9)
       ("rust-cipher" ,rust-cipher-0.4)
       ("rust-curve25519-dalek" ,rust-curve25519-dalek_4_1_2)
       ("rust-digest" ,rust-digest-0.10)
       ("rust-ed25519-dalek" ,rust-ed25519-dalek_2_1_1)
       ("rust-generic-array" ,rust-generic-array-0.14)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-rusty_paseto" ,rust-rusty_paseto_0_6_1)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-subtle" ,rust-subtle-2))))
    (home-page "None")
    (synopsis "Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key management")
    (description
      (beautify-description "Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key management"))
    (license (list license:expat))))

(define-public rust-sql-builder_3_1_1
  (package
    (name "rust-sql-builder")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sql-builder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h5xp47zz9chv545lpmal51fq3z162z2f99mb4lhcbgcsaaqs05i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-anyhow" ,rust-anyhow_1_0_80)
       ("rust-thiserror" ,rust-thiserror_1_0_57))))
    (home-page "None")
    (synopsis "Simple SQL code generator.")
    (description
      (beautify-description "Simple SQL code generator."))
    (license (list license:expat))))

(define-public rust-serde_with_3_6_1
  (package
    (name "rust-serde_with")
    (version "3.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_with" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w6j1ch2p9cwxaaiphx4rbkz4vb04q78wasv5wiyqhfqgfcngl8m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)
       ("rust-chrono" ,rust-chrono-0.4)
       ("rust-hex" ,rust-hex-0.4)
       ("rust-indexmap" ,rust-indexmap-1)
       ("rust-indexmap" ,rust-indexmap_2_2_5)
       ("rust-serde_derive" ,rust-serde_derive_1_0_197)
       ("rust-serde_json" ,rust-serde_json_1_0_114)
       ("rust-serde_with_macros" ,rust-serde_with_macros_3_6_1)
       ("rust-time" ,rust-time_0_3_34))
      #:cargo-development-inputs
      (("rust-serde" ,rust-serde_1_0_197))))
    (home-page "None")
    (synopsis "Custom de/serialization functions for Rust\u0027s serde")
    (description
      (beautify-description "Custom de/serialization functions for Rust\u0027s serde"))
    (license (list license:expat license:asl2.0))))

(define-public rust-parse_duration_2_1_1
  (package
    (name "rust-parse_duration")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parse_duration" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pd97dmlv1i6pvr2byi65q1fzv667gvhnf3ld2lsawh17vlyadvh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)
       ("rust-num" ,rust-num-0.2)
       ("rust-regex" ,rust-regex_1_10_3))))
    (home-page "https://github.com/zeta12ti/parse_duration/")
    (synopsis "Parses a duration from a string.")
    (description
      (beautify-description "Parses a duration from a string."))
    (license (list license:expat))))

(define-public rust-ed25519-dalek_2_1_1
  (package
    (name "rust-ed25519-dalek")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ed25519-dalek" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w88cafwglg9hjizldbmlza0ns3hls81zk1bcih3m5m3h67algaa"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ed25519" ,rust-ed25519_2_2_3)
       ("rust-serde" ,rust-serde_1_0_197)
       ("rust-sha2" ,rust-sha2-0.10)
       ("rust-subtle" ,rust-subtle-2)
       ("rust-zeroize" ,rust-zeroize-1))
      #:cargo-development-inputs
      (("rust-curve25519-dalek" ,rust-curve25519-dalek_4_1_2))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis "Fast and efficient ed25519 EdDSA key generations, signing, and verification in pure Rust.")
    (description
      (beautify-description "Fast and efficient ed25519 EdDSA key generations, signing, and verification in pure Rust."))
    (license (list license:bsd-3))))

(define-public rust-ring_0_17_8
  (package
    (name "rust-ring")
    (version "0.17.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ring" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03fwlb1ssrmfxdckvqv033pfmk01rhx9ynwi7r186dcfcp5s8zy1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_89)
       ("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-getrandom" ,rust-getrandom_0_2_12)
       ("rust-spin" ,rust-spin-0.9)
       ("rust-untrusted" ,rust-untrusted-0.9)
       ("rust-windows-sys" ,rust-windows-sys-0.52))
      #:cargo-development-inputs
      (("rust-libc" ,rust-libc_0_2_153))))
    (home-page "None")
    (synopsis "Safe, fast, small crypto using Rust.")
    (description
      (beautify-description "Safe, fast, small crypto using Rust."))
    (license (list ))))

(define-public rust-chacha20_0_8_2
  (package
    (name "rust-chacha20")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chacha20" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19l0nrizh0v9mj2dcd1y0mh7nn9sjnmvvg203nwy6vx6193fb02w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-cipher" ,rust-cipher-0.3)
       ("rust-cpufeatures" ,rust-cpufeatures_0_2_12))))
    (home-page "None")
    (synopsis "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits\nfrom the RustCrypto `cipher` crate, with optional architecture-specific\nhardware acceleration (AVX2, SSE2). Additionally provides the ChaCha8, ChaCha12,\nXChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional\nrand_core-compatible RNGs based on those ciphers.")
    (description
      (beautify-description "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits\nfrom the RustCrypto `cipher` crate, with optional architecture-specific\nhardware acceleration (AVX2, SSE2). Additionally provides the ChaCha8, ChaCha12,\nXChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional\nrand_core-compatible RNGs based on those ciphers."))
    (license (list license:asl2.0 license:expat))))

(define-public rust-ed25519_2_2_3
  (package
    (name "rust-ed25519")
    (version "2.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ed25519" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-pkcs8" ,rust-pkcs8-0.10)
       ("rust-signature" ,rust-signature_2_2_0))))
    (home-page "None")
    (synopsis "Edwards Digital Signature Algorithm (EdDSA) over Curve25519 (as specified in RFC 8032)\nsupport library providing signature type definitions and PKCS#8 private key\ndecoding/encoding support")
    (description
      (beautify-description "Edwards Digital Signature Algorithm (EdDSA) over Curve25519 (as specified in RFC 8032)\nsupport library providing signature type definitions and PKCS#8 private key\ndecoding/encoding support"))
    (license (list license:asl2.0 license:expat))))

(define-public rust-curve25519-dalek_4_1_2
  (package
    (name "rust-curve25519-dalek")
    (version "4.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "curve25519-dalek" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0j7kqchcgycs4a11gvlda93h9w2jr05nn4hjpfyh2kn94a4pnrqa"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-cpufeatures" ,rust-cpufeatures_0_2_12)
       ("rust-curve25519-dalek-derive" ,rust-curve25519-dalek-derive_0_1_1)
       ("rust-digest" ,rust-digest-0.10)
       ("rust-fiat-crypto" ,rust-fiat-crypto_0_2_6)
       ("rust-platforms" ,rust-platforms_3_3_0)
       ("rust-rustc_version" ,rust-rustc_version_0_4_0)
       ("rust-subtle" ,rust-subtle-2)
       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis "A pure-Rust implementation of group operations on ristretto255 and Curve25519")
    (description
      (beautify-description "A pure-Rust implementation of group operations on ristretto255 and Curve25519"))
    (license (list license:bsd-3))))

(define-public rust-signature_2_2_0
  (package
    (name "rust-signature")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "signature" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-digest" ,rust-digest-0.10)
       ("rust-rand_core" ,rust-rand_core_0_6_4))))
    (home-page "None")
    (synopsis "Traits for cryptographic signature algorithms (e.g. ECDSA, Ed25519)")
    (description
      (beautify-description "Traits for cryptographic signature algorithms (e.g. ECDSA, Ed25519)"))
    (license (list license:asl2.0 license:expat))))

(define-public rust-rand_core_0_6_4
  (package
    (name "rust-rand_core")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-getrandom" ,rust-getrandom_0_2_12))))
    (home-page "https://rust-random.github.io/book")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license (list license:expat license:asl2.0))))

(define-public rust-curve25519-dalek-derive_0_1_1
  (package
    (name "rust-curve25519-dalek-derive")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "curve25519-dalek-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis "curve25519-dalek Derives")
    (description
      (beautify-description "curve25519-dalek Derives"))
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc_version_0_4_0
  (package
    (name "rust-rustc_version")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rpk9rcdk405xhbmgclsh4pai0svn49x35aggl4nhbkd4a2zb85z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-semver" ,rust-semver_1_0_22))))
    (home-page "None")
    (synopsis "A library for querying the version of a installed rustc compiler")
    (description
      (beautify-description "A library for querying the version of a installed rustc compiler"))
    (license (list license:expat license:asl2.0))))

(define-public rust-fiat-crypto_0_2_6
  (package
    (name "rust-fiat-crypto")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fiat-crypto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10hkkkjynhibvchznkxx81gwxqarn9i5sgz40d6xxb8xzhsz8xhn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mit-plv/fiat-crypto")
    (synopsis "Fiat-crypto generated Rust")
    (description
      (beautify-description "Fiat-crypto generated Rust"))
    (license (list license:expat license:asl2.0))))

(define-public rust-platforms_3_3_0
  (package
    (name "rust-platforms")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "platforms" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k7q6pigmnvgpfasvssb12m2pv3pc94zrhrfg9by3h3wmhyfqvb2"))))
    (build-system cargo-build-system)
    (home-page "https://rustsec.org")
    (synopsis "Rust platform registry with information about valid Rust platforms (target\ntriple, target_arch, target_os) sourced from the Rust compiler.")
    (description
      (beautify-description "Rust platform registry with information about valid Rust platforms (target\ntriple, target_arch, target_os) sourced from the Rust compiler."))
    (license (list license:asl2.0 license:expat))))

(define-public rust-serde_with_macros_3_6_1
  (package
    (name "rust-serde_with_macros")
    (version "3.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_with_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kd6w35cj542y4qhlgm3jkdcihj3j13pm2vaas8qjqry751rfpw6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling" ,rust-darling_0_20_8)
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "None")
    (synopsis "proc-macro library for serde_with")
    (description
      (beautify-description "proc-macro library for serde_with"))
    (license (list license:expat license:asl2.0))))

(define-public rust-darling_0_20_8
  (package
    (name "rust-darling")
    (version "0.20.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14a38qsi9104kvk1z11rqj0bnz1866dyhnvgvbgzz17d2g6nzqsl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling_core" ,rust-darling_core_0_20_8)
       ("rust-darling_macro" ,rust-darling_macro_0_20_8))))
    (home-page "None")
    (synopsis "A proc-macro library for reading attributes into structs when\nimplementing custom derives.")
    (description
      (beautify-description "A proc-macro library for reading attributes into structs when\nimplementing custom derives."))
    (license (list license:expat))))

(define-public rust-darling_macro_0_20_8
  (package
    (name "rust-darling_macro")
    (version "0.20.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gwkz0cjfy3fgcc1zmm7azzhj5qpja34s0cklcria4l38sjyss56"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling_core" ,rust-darling_core_0_20_8)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "None")
    (synopsis "Internal support for a proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
      (beautify-description "Internal support for a proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code."))
    (license (list license:expat))))

(define-public rust-darling_core_0_20_8
  (package
    (name "rust-darling_core")
    (version "0.20.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03x7s149p06xfwcq0lgkk4yxh6jf7jckny18nzp1yyk87b1g2b4w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-fnv" ,rust-fnv-1)
       ("rust-ident_case" ,rust-ident_case_1_0_1)
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)
       ("rust-quote" ,rust-quote-1)
       ("rust-strsim" ,rust-strsim-0.10)
       ("rust-syn" ,rust-syn_2_0_52))))
    (home-page "None")
    (synopsis "Helper crate for proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
      (beautify-description "Helper crate for proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code."))
    (license (list license:expat))))

(define-public rust-ident_case_1_0_1
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
    (license (list license:expat license:asl2.0))))
