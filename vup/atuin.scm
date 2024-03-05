(define-module (vup atuin)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (guix download)
  #:use-module ((guix import utils) #:select (beautify-description)))

(define-public atuin
  (package
    (name "rust-atuin")
    (version "18.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19kviks3561d2qdqc0667c74wx5s9rvfkpzk51r083vl098k591z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:tests? #f
      #:cargo-inputs
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-atuin-client" ,rust-atuin-client_18_0_1)        
       ("rust-atuin-common" ,rust-atuin-common_18_0_1)        
       ("rust-atuin-server" ,rust-atuin-server_18_0_1)        
       ("rust-atuin-server-postgres" ,rust-atuin-server-postgres_18_0_1)        
       ("rust-base64" ,rust-base64_0_21_7)        
       ("rust-clap" ,rust-clap_4_4_18)        
       ("rust-clap_complete" ,rust-clap_complete_4_4_9)        
       ("rust-cli-clipboard" ,rust-cli-clipboard_0_4_0)        
       ("rust-colored" ,rust-colored_2_1_0)        
       ("rust-crossterm" ,rust-crossterm-0.27)        
       ("rust-directories" ,rust-directories_5_0_1)        
       ("rust-env_logger" ,rust-env_logger_0_10_2)        
       ("rust-eyre" ,rust-eyre_0_6_11)        
       ("rust-fs-err" ,rust-fs-err_2_11_0)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)        
       ("rust-indicatif" ,rust-indicatif_0_17_7)        
       ("rust-interim" ,rust-interim_0_1_1)        
       ("rust-itertools" ,rust-itertools_0_12_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-ratatui" ,rust-ratatui_0_25_0)        
       ("rust-rpassword" ,rust-rpassword_7_3_1)        
       ("rust-runtime-format" ,rust-runtime-format_0_1_3)        
       ("rust-rustix" ,rust-rustix_0_38_30)        
       ("rust-semver" ,rust-semver_1_0_21)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tiny-bip39" ,rust-tiny-bip39_1_0_0)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-tracing-subscriber" ,rust-tracing-subscriber_0_3_18)        
       ("rust-unicode-width" ,rust-unicode-width-0.1)        
       ("rust-uuid" ,rust-uuid_1_7_0)        
       ("rust-whoami" ,rust-whoami-1))
      #:cargo-development-inputs
      (("rust-tracing-tree" ,rust-tracing-tree_0_3_0))))
    (home-page "https://atuin.sh")
    (synopsis "atuin - magical shell history")
    (description
      (beautify-description "atuin - magical shell history"))
    (license (list license:expat))))

(define rust-cli-clipboard_0_4_0
  (package
    (name "rust-cli-clipboard")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cli-clipboard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g9y1w3ln5wn202mwxwhsilhifwww2p34fan99w5k8ia98fpq204"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-clipboard-win" ,rust-clipboard-win_4_5_0)        
       ("rust-objc" ,rust-objc-0.2)        
       ("rust-objc-foundation" ,rust-objc-foundation-0.1)        
       ("rust-objc_id" ,rust-objc_id_0_1_1)        
       ("rust-wl-clipboard-rs" ,rust-wl-clipboard-rs_0_7_0)        
       ("rust-x11-clipboard" ,rust-x11-clipboard-0.7))))
    (home-page "None")
    (synopsis "cli-clipboard is a cross-platform library for getting and setting the contents of the OS-level clipboard.")
    (description
      (beautify-description "cli-clipboard is a cross-platform library for getting and setting the contents of the OS-level clipboard."))
    (license (list license:expat license:asl2.0))))

(define rust-tracing_0_1_40
  (package
    (name "rust-tracing")
    (version "0.1.40")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log-0.4)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-tracing-attributes" ,rust-tracing-attributes_0_1_27)        
       ("rust-tracing-core" ,rust-tracing-core_0_1_32))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust.")
    (description
      (beautify-description "Application-level tracing for Rust."))
    (license (list license:expat))))

(define rust-atuin-common_18_0_1
  (package
    (name "rust-atuin-common")
    (version "18.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-common" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c38j8g663apm5d8v33mbn7i6wd4lmzd2l65gf9g1md7h4fxcx63"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-eyre" ,rust-eyre_0_6_11)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-semver" ,rust-semver_1_0_21)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-sqlx" ,rust-sqlx_0_7_3)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-typed-builder" ,rust-typed-builder_0_18_1)        
       ("rust-uuid" ,rust-uuid_1_7_0))
      #:cargo-development-inputs
      (("rust-pretty_assertions" ,rust-pretty_assertions_1_4_0))))
    (home-page "https://atuin.sh")
    (synopsis "common library for atuin")
    (description
      (beautify-description "common library for atuin"))
    (license (list license:expat))))

(define rust-base64_0_21_7
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

(define rust-interim_0_1_1
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
      (("rust-logos" ,rust-logos_0_13_0)        
       ("rust-time" ,rust-time_0_3_31))))
    (home-page "None")
    (synopsis "parses simple English dates, inspired by Linux date command, and forked from chrono-english")
    (description
      (beautify-description "parses simple English dates, inspired by Linux date command, and forked from chrono-english"))
    (license (list license:expat))))

(define rust-itertools_0_12_0
  (package
    (name "rust-itertools")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c07gzdlc6a1c8p8jrvvw3gs52bss3y58cs2s21d9i978l36pnr5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-either" ,rust-either_1_9_0))))
    (home-page "None")
    (synopsis "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      (beautify-description "Extra iterator adaptors, iterator methods, free functions, and macros."))
    (license (list license:expat license:asl2.0))))

(define rust-atuin-server-postgres_18_0_1
  (package
    (name "rust-atuin-server-postgres")
    (version "18.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-server-postgres" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1505krr69fzlpc0dgp2pr4yx0dkiskvi26qkk7qa3wfxfrn1nw1z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-atuin-common" ,rust-atuin-common_18_0_1)        
       ("rust-atuin-server-database" ,rust-atuin-server-database_18_0_1)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-sqlx" ,rust-sqlx_0_7_3)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "https://atuin.sh")
    (synopsis "server postgres database library for atuin")
    (description
      (beautify-description "server postgres database library for atuin"))
    (license (list license:expat))))

(define rust-serde_1_0_195
  (package
    (name "rust-serde")
    (version "1.0.195")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00kbc86kgaihpza0zdglcd2qq5468yg0dvvdmkli2y660bs1s9k3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde_derive" ,rust-serde_derive_1_0_195))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
      (beautify-description "A generic serialization/deserialization framework"))
    (license (list license:expat license:asl2.0))))

(define rust-serde_json_1_0_111
  (package
    (name "rust-serde_json")
    (version "1.0.111")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x441azvvdy6x8am4bvkxhswhzw5cr8ml0cqspnihvri8bx4cvhp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-ryu" ,rust-ryu_1_0_16)        
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
      (beautify-description "A JSON serialization file format"))
    (license (list license:expat license:asl2.0))))

(define rust-directories_5_0_1
  (package
    (name "rust-directories")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "directories" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-dirs-sys" ,rust-dirs-sys-0.4))))
    (home-page "None")
    (synopsis "A tiny mid-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows and macOS by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
      (beautify-description "A tiny mid-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows and macOS by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS."))
    (license (list license:expat license:asl2.0))))

(define rust-fs-err_2_11_0
  (package
    (name "rust-fs-err")
    (version "2.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs-err" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg-1))))
    (home-page "None")
    (synopsis "A drop-in replacement for std::fs with more helpful error messages.")
    (description
      (beautify-description "A drop-in replacement for std::fs with more helpful error messages."))
    (license (list license:expat license:asl2.0))))

(define rust-clap_complete_4_4_9
  (package
    (name "rust-clap_complete")
    (version "4.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_complete" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01mivwak0mx6v856h0qvaqg2fdzncpdxn6kw7b6kyqgn57j1lqyz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-development-inputs
      (("rust-clap" ,rust-clap_4_4_18))))
    (home-page "None")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description
      (beautify-description "Generate shell completion scripts for your clap::Command"))
    (license (list license:expat license:asl2.0))))

(define rust-atuin-server_18_0_1
  (package
    (name "rust-atuin-server")
    (version "18.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-server" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05h1zdjyw6ii8gl0ncyy6cj0rwdis7rfmnzs3dikp6y8aihvk4p0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-argon2" ,rust-argon2_0_5_3)        
       ("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-atuin-common" ,rust-atuin-common_18_0_1)        
       ("rust-atuin-server-database" ,rust-atuin-server-database_18_0_1)        
       ("rust-axum" ,rust-axum_0_7_4)        
       ("rust-axum-server" ,rust-axum-server_0_6_0)        
       ("rust-base64" ,rust-base64_0_21_7)        
       ("rust-config" ,rust-config_0_13_4)        
       ("rust-eyre" ,rust-eyre_0_6_11)        
       ("rust-fs-err" ,rust-fs-err_2_11_0)        
       ("rust-metrics" ,rust-metrics_0_21_1)        
       ("rust-metrics-exporter-prometheus" ,rust-metrics-exporter-prometheus_0_12_2)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-reqwest" ,rust-reqwest_0_11_23)        
       ("rust-rustls" ,rust-rustls_0_21_10)        
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_2_0_0)        
       ("rust-semver" ,rust-semver_1_0_21)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tower" ,rust-tower-0.4)        
       ("rust-tower-http" ,rust-tower-http_0_5_1)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "https://atuin.sh")
    (synopsis "server library for atuin")
    (description
      (beautify-description "server library for atuin"))
    (license (list license:expat))))

(define rust-ratatui_0_25_0
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
       ("rust-indoc" ,rust-indoc_2_0_4)        
       ("rust-itertools" ,rust-itertools_0_12_0)        
       ("rust-lru" ,rust-lru_0_12_1)        
       ("rust-paste" ,rust-paste-1)        
       ("rust-stability" ,rust-stability_0_1_1)        
       ("rust-strum" ,rust-strum_0_25_0)        
       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)        
       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "None")
    (synopsis "A library that\u0027s all about cooking up terminal user interfaces")
    (description
      (beautify-description "A library that\u0027s all about cooking up terminal user interfaces"))
    (license (list license:expat))))

(define rust-async-trait_0_1_77
  (package
    (name "rust-async-trait")
    (version "0.1.77")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-trait" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1adf1jh2yg39rkpmqjqyr9xyd6849p0d95425i6imgbhx0syx069"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Type erasure for async trait methods")
    (description
      (beautify-description "Type erasure for async trait methods"))
    (license (list license:expat license:asl2.0))))

(define rust-env_logger_0_10_2
  (package
    (name "rust-env_logger")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1005v71kay9kbz1d5907l0y7vh9qn2fqsp2yfgb8bjvin6m0bm2c"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-humantime" ,rust-humantime-2)        
       ("rust-is-terminal" ,rust-is-terminal_0_4_10)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-regex" ,rust-regex_1_10_3)        
       ("rust-termcolor" ,rust-termcolor_1_4_1))))
    (home-page "None")
    (synopsis "A logging implementation for `log` which is configured via an environment\nvariable.")
    (description
      (beautify-description "A logging implementation for `log` which is configured via an environment\nvariable."))
    (license (list license:expat license:asl2.0))))

(define rust-runtime-format_0_1_3
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

(define rust-atuin-client_18_0_1
  (package
    (name "rust-atuin-client")
    (version "18.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-client" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "175lcb12hc61bskzzhi6imlhgizk5k02la7j5736id5mv8mjcswp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-atuin-common" ,rust-atuin-common_18_0_1)        
       ("rust-base64" ,rust-base64_0_21_7)        
       ("rust-clap" ,rust-clap_4_4_18)        
       ("rust-config" ,rust-config_0_13_4)        
       ("rust-crypto_secretbox" ,rust-crypto_secretbox_0_1_1)        
       ("rust-directories" ,rust-directories_5_0_1)        
       ("rust-eyre" ,rust-eyre_0_6_11)        
       ("rust-fs-err" ,rust-fs-err_2_11_0)        
       ("rust-futures" ,rust-futures_0_3_30)        
       ("rust-generic-array" ,rust-generic-array-0.14)        
       ("rust-hex" ,rust-hex-0.4)        
       ("rust-indicatif" ,rust-indicatif_0_17_7)        
       ("rust-interim" ,rust-interim_0_1_1)        
       ("rust-itertools" ,rust-itertools_0_12_0)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-minspan" ,rust-minspan_0_1_1)        
       ("rust-parse_duration" ,rust-parse_duration_2_1_1)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-regex" ,rust-regex_1_10_3)        
       ("rust-reqwest" ,rust-reqwest_0_11_23)        
       ("rust-rmp" ,rust-rmp-0.8)        
       ("rust-rusty_paserk" ,rust-rusty_paserk_0_3_0)        
       ("rust-rusty_paseto" ,rust-rusty_paseto_0_6_1)        
       ("rust-semver" ,rust-semver_1_0_21)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-serde_regex" ,rust-serde_regex_1_1_0)        
       ("rust-serde_with" ,rust-serde_with_3_5_1)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-shellexpand" ,rust-shellexpand_3_1_0)        
       ("rust-sql-builder" ,rust-sql-builder_3_1_1)        
       ("rust-sqlx" ,rust-sqlx_0_7_3)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-typed-builder" ,rust-typed-builder_0_18_1)        
       ("rust-urlencoding" ,rust-urlencoding_2_1_3)        
       ("rust-uuid" ,rust-uuid_1_7_0)        
       ("rust-whoami" ,rust-whoami-1))
      #:cargo-development-inputs
      (("rust-pretty_assertions" ,rust-pretty_assertions_1_4_0))))
    (home-page "https://atuin.sh")
    (synopsis "client library for atuin")
    (description
      (beautify-description "client library for atuin"))
    (license (list license:expat))))

(define rust-rpassword_7_3_1
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
      (("rust-libc" ,rust-libc_0_2_152)        
       ("rust-rtoolbox" ,rust-rtoolbox_0_0_2)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/conradkleinespel/rpassword")
    (synopsis "Read passwords in console applications.")
    (description
      (beautify-description "Read passwords in console applications."))
    (license (list license:asl2.0))))

(define rust-tracing-subscriber_0_3_18
  (package
    (name "rust-tracing-subscriber")
    (version "0.3.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-subscriber" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12vs1bwk4kig1l2qqjbbn2nm5amwiqmkcmnznylzmnfvjy6083xd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-matchers" ,rust-matchers-0.1)        
       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.46)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-regex" ,rust-regex_1_10_3)        
       ("rust-sharded-slab" ,rust-sharded-slab_0_1_7)        
       ("rust-thread_local" ,rust-thread_local_1_1_7)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-tracing-core" ,rust-tracing-core_0_1_32))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities for implementing and composing `tracing` subscribers.")
    (description
      (beautify-description "Utilities for implementing and composing `tracing` subscribers."))
    (license (list license:expat))))

(define rust-clap_4_4_18
  (package
    (name "rust-clap")
    (version "4.4.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p46h346y8nval6gwzh27if3icbi9dwl95fg5ir36ihrqip8smqy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-clap_builder" ,rust-clap_builder_4_4_18)        
       ("rust-clap_derive" ,rust-clap_derive_4_4_7))))
    (home-page "None")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (list license:expat license:asl2.0))))

(define rust-eyre_0_6_11
  (package
    (name "rust-eyre")
    (version "0.6.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "eyre" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16970k5si93pz1ppi3w5pihcr8qjhvym13pw9bm7k4gmlqgpl9mn"))))
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

(define rust-rustix_0_38_30
  (package
    (name "rust-rustix")
    (version "0.38.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jkb6bzrj2w9ffy35aw4q04mqk1yxqw35fz80x0c4cxgi9c988rj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_2_4_2)        
       ("rust-errno" ,rust-errno_0_3_8)        
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-linux-raw-sys" ,rust-linux-raw-sys_0_4_13)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "None")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (description
      (beautify-description "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls"))
    (license (list license:asl2.0 license:asl2.0 license:expat))))

(define rust-indicatif_0_17_7
  (package
    (name "rust-indicatif")
    (version "0.17.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indicatif" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "098ggvg7ps4097p5n9hmb3pqqy10bi8vjfzb7pci79xrklf78a7v"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-console" ,rust-console_0_15_8)        
       ("rust-instant" ,rust-instant_0_1_12)        
       ("rust-number_prefix" ,rust-number_prefix_0_4_0)        
       ("rust-portable-atomic" ,rust-portable-atomic_1_6_0)        
       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "None")
    (synopsis "A progress bar and cli reporting library for Rust")
    (description
      (beautify-description "A progress bar and cli reporting library for Rust"))
    (license (list license:expat))))

(define rust-colored_2_1_0
  (package
    (name "rust-colored")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "colored" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f4h9p64snrnz4x432iza15p4diqjcgpmpvhi956d6r1rq61bwnb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/mackwic/colored")
    (synopsis "The most simple way to add colors in your terminal")
    (description
      (beautify-description "The most simple way to add colors in your terminal"))
    (license (list license:mpl2.0))))

(define rust-tiny-bip39_1_0_0
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
      (("rust-anyhow" ,rust-anyhow_1_0_79)        
       ("rust-hmac" ,rust-hmac-0.12)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-pbkdf2" ,rust-pbkdf2-0.11)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-rustc-hash" ,rust-rustc-hash-1)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)        
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90)        
       ("rust-zeroize" ,rust-zeroize_1_7_0))))
    (home-page "https://github.com/maciejhirsz/tiny-bip39/")
    (synopsis "A fork of the bip39 crate with fixes to v0.6. Rust implementation of BIP-0039")
    (description
      (beautify-description "A fork of the bip39 crate with fixes to v0.6. Rust implementation of BIP-0039"))
    (license (list license:expat license:asl2.0))))

(define rust-semver_1_0_21
  (package
    (name "rust-semver")
    (version "1.0.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c49snqlfcx93xym1cgwx8zcspmyyxm37xa2fyfgjx1vhalxfzmr"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Parser and evaluator for Cargo\u0027s flavor of Semantic Versioning")
    (description
      (beautify-description "Parser and evaluator for Cargo\u0027s flavor of Semantic Versioning"))
    (license (list license:expat license:asl2.0))))

(define rust-tokio_1_35_1
  (package
    (name "rust-tokio")
    (version "1.35.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01613rkziqp812a288ga65aqygs254wgajdi57v8brivjkx4x6y8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-backtrace" ,rust-backtrace_0_3_69)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-mio" ,rust-mio_0_8_10)        
       ("rust-num_cpus" ,rust-num_cpus_1_16_0)        
       ("rust-parking_lot" ,rust-parking_lot_0_12_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-signal-hook-registry" ,rust-signal-hook-registry_1_4_1)        
       ("rust-socket2" ,rust-socket2_0_5_5)        
       ("rust-tokio-macros" ,rust-tokio-macros_2_2_0)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://tokio.rs")
    (synopsis "An event-driven, non-blocking I/O platform for writing asynchronous I/O\nbacked applications.")
    (description
      (beautify-description "An event-driven, non-blocking I/O platform for writing asynchronous I/O\nbacked applications."))
    (license (list license:expat))))

(define rust-uuid_1_7_0
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
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
      (beautify-description "A library to generate and parse UUIDs."))
    (license (list license:asl2.0 license:expat))))

(define rust-time_0_3_31
  (package
    (name "rust-time")
    (version "0.3.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gjqcdsdbh0r5vi4c2vrj5a6prdviapx731wwn07cvpqqd1blmzn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-deranged" ,rust-deranged_0_3_11)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-num_threads" ,rust-num_threads_0_1_6)        
       ("rust-powerfmt" ,rust-powerfmt-0.2)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-time-core" ,rust-time-core_0_1_2)        
       ("rust-time-macros" ,rust-time-macros_0_2_16))))
    (home-page "https://time-rs.github.io")
    (synopsis "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      (beautify-description "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]."))
    (license (list license:expat license:asl2.0))))

(define rust-futures-util_0_3_30
  (package
    (name "rust-futures-util")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0j0xqhcir1zf2dcbpd421kgw6wvsk0rpxflylcysn1rlp3g02r1x"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-io" ,rust-futures-io_0_3_30)        
       ("rust-futures-macro" ,rust-futures-macro_0_3_30)        
       ("rust-futures-sink" ,rust-futures-sink_0_3_30)        
       ("rust-futures-task" ,rust-futures-task_0_3_30)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-pin-utils" ,rust-pin-utils-0.1)        
       ("rust-slab" ,rust-slab_0_4_9))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Common utilities and extension traits for the futures-rs library.")
    (description
      (beautify-description "Common utilities and extension traits for the futures-rs library."))
    (license (list license:expat license:asl2.0))))

(define rust-tracing-tree_0_3_0
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
      (("rust-nu-ansi-term" ,rust-nu-ansi-term_0_49_0)        
       ("rust-tracing-core" ,rust-tracing-core_0_1_32)        
       ("rust-tracing-log" ,rust-tracing-log_0_2_0)        
       ("rust-tracing-subscriber" ,rust-tracing-subscriber_0_3_18))))
    (home-page "None")
    (synopsis "A Tracing Layer which prints a tree of spans and events.")
    (description
      (beautify-description "A Tracing Layer which prints a tree of spans and events."))
    (license (list license:expat license:asl2.0))))

(define rust-objc_id_0_1_1
  (package
    (name "rust-objc_id")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "objc_id" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fq71hnp2sdblaighjc82yrac3adfmqzhpr11irhvdfp9gdlsbf9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-objc" ,rust-objc-0.2))))
    (home-page "None")
    (synopsis "Rust smart pointers for Objective-C reference counting.")
    (description
      (beautify-description "Rust smart pointers for Objective-C reference counting."))
    (license (list license:expat))))

(define rust-clipboard-win_4_5_0
  (package
    (name "rust-clipboard-win")
    (version "4.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clipboard-win" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qh3rypkf1lazniq4nr04hxsck0d55rigb5sjvpvgnap4dyc54bi"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-error-code" ,rust-error-code_2_3_1)        
       ("rust-str-buf" ,rust-str-buf_1_0_6)        
       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "None")
    (synopsis "Provides simple way to interact with Windows clipboard.")
    (description
      (beautify-description "Provides simple way to interact with Windows clipboard."))
    (license (list license:boost1.0))))

(define rust-wl-clipboard-rs_0_7_0
  (package
    (name "rust-wl-clipboard-rs")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wl-clipboard-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n8sg981h3d08hnnlrsgs81w2qz3n8a5ml0jcsgnapdpzcyk06lq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-derive-new" ,rust-derive-new_0_5_9)        
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-nix" ,rust-nix-0.24)        
       ("rust-os_pipe" ,rust-os_pipe_1_1_5)        
       ("rust-tempfile" ,rust-tempfile_3_9_0)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-tree_magic_mini" ,rust-tree_magic_mini_3_0_3)        
       ("rust-wayland-client" ,rust-wayland-client-0.29)
       ("rust-wayland-protocols" ,rust-wayland-protocols-0.29))))
    (home-page "None")
    (synopsis "Access to the Wayland clipboard for terminal and other window-less applications.")
    (description
      (beautify-description "Access to the Wayland clipboard for terminal and other window-less applications."))
    (license (list license:expat license:asl2.0))))

(define rust-error-code_2_3_1
  (package
    (name "rust-error-code")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-code" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08baxlf8qz01lgjsdbfhs193r9y1nlc566s5xvzyf4dzwy8qkwb4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152)        
       ("rust-str-buf" ,rust-str-buf_1_0_6))))
    (home-page "None")
    (synopsis "Error code")
    (description
      (beautify-description "Error code"))
    (license (list license:boost1.0))))

(define rust-str-buf_1_0_6
  (package
    (name "rust-str-buf")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "str-buf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l7q4nha7wpsr0970bfqm773vhmpwr9l6rr8r4gwgrh46wvdh24y"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Static string buffer")
    (description
      (beautify-description "Static string buffer"))
    (license (list license:boost1.0))))

(define rust-libc_0_2_152
  (package
    (name "rust-libc")
    (version "0.2.152")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rsnma7hnw22w7jh9yqg43slddvfbnfzrvm3s7s4kinbj1jvzqqk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
      (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license (list license:expat license:asl2.0))))

(define rust-tree_magic_mini_3_0_3
  (package
    (name "rust-tree_magic_mini")
    (version "3.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tree_magic_mini" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vdazv3y1iggriwx5ksin72c2ds0xjdhx1yvmd5nxkya0w3gvbci"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytecount" ,rust-bytecount_0_6_7)        
       ("rust-fnv" ,rust-fnv-1)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-nom" ,rust-nom_7_1_3)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-petgraph" ,rust-petgraph_0_6_4))))
    (home-page "None")
    (synopsis "Determines the MIME type of a file by traversing a filetype tree.")
    (description
      (beautify-description "Determines the MIME type of a file by traversing a filetype tree."))
    (license (list license:expat))))

(define rust-tempfile_3_9_0
  (package
    (name "rust-tempfile")
    (version "3.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ypkl7rvv57n16q28psxpb61rnyhmfaif12ascdnsyljm90l3kh1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-fastrand" ,rust-fastrand-2)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_4_1)        
       ("rust-rustix" ,rust-rustix_0_38_30)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "A library for managing temporary files and directories.")
    (description
      (beautify-description "A library for managing temporary files and directories."))
    (license (list license:expat license:asl2.0))))

(define rust-thiserror_1_0_56
  (package
    (name "rust-thiserror")
    (version "1.0.56")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b9hnzngjan4d89zjs16i01bcpcnvdwklyh73lj16xk28p37hhym"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-thiserror-impl" ,rust-thiserror-impl_1_0_56))))
    (home-page "None")
    (synopsis "derive(Error)")
    (description
      (beautify-description "derive(Error)"))
    (license (list license:expat license:asl2.0))))

(define rust-derive-new_0_5_9
  (package
    (name "rust-derive-new")
    (version "0.5.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derive-new" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0d9m5kcj1rdmdjqfgj7rxxhdzx0as7p4rp1mjx5j6w5dl2f3461l"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "`#[derive(new)]` implements simple constructor functions for structs and enums.")
    (description
      (beautify-description "`#[derive(new)]` implements simple constructor functions for structs and enums."))
    (license (list license:expat))))

(define rust-os_pipe_1_1_5
  (package
    (name "rust-os_pipe")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os_pipe" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fcgfg3ddnsh6vfhkk579p7z786kh1khb1dar4g4k1iri4xrq4ap"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "None")
    (synopsis "a cross-platform library for opening OS pipes")
    (description
      (beautify-description "a cross-platform library for opening OS pipes"))
    (license (list license:expat))))

(define rust-bytecount_0_6_7
  (package
    (name "rust-bytecount")
    (version "0.6.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytecount" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "019j3basq13gzmasbqqlhf4076231aw1v63lbyp27ikgs4sz1rg1"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
    (description
      (beautify-description "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast"))
    (license (list license:asl2.0 license:expat))))

(define rust-nom_7_1_3
  (package
    (name "rust-nom")
    (version "7.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-minimal-lexical" ,rust-minimal-lexical-0.2))))
    (home-page "None")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
      (beautify-description "A byte-oriented, zero-copy, parser combinators library"))
    (license (list license:expat))))

(define rust-petgraph_0_6_4
  (package
    (name "rust-petgraph")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "petgraph" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ac6wfq5f5pzcv0nvzzfgjbwg2kwslpnzsw5wcmxlscfcb9azlz1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-fixedbitset" ,rust-fixedbitset-0.4)        
       ("rust-indexmap" ,rust-indexmap_2_1_0))))
    (home-page "None")
    (synopsis "Graph data structure library. Provides graph types and graph algorithms.")
    (description
      (beautify-description "Graph data structure library. Provides graph types and graph algorithms."))
    (license (list license:expat license:asl2.0))))

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
    (arguments
    `(#:cargo-inputs
      (("rust-spin" ,rust-spin-0.5))))
    (home-page "None")
    (synopsis "A macro for declaring lazily evaluated statics in Rust.")
    (description
      (beautify-description "A macro for declaring lazily evaluated statics in Rust."))
    (license (list license:expat license:asl2.0))))

(define rust-memchr_2_7_1
  (package
    (name "rust-memchr")
    (version "2.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jf1kicqa4vs9lyzj4v4y1p90q0dh87hvhsdd5xvhnp527sw8gaj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis "Provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32) routines for\n1, 2 or 3 byte search and single substring search.")
    (description
      (beautify-description "Provides extremely fast (uses SIMD on x86_64, aarch64 and wasm32) routines for\n1, 2 or 3 byte search and single substring search."))
    (license (list license:unlicense license:expat))))

(define rust-indexmap_2_1_0
  (package
    (name "rust-indexmap")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07rxrqmryr1xfnmhrjlz8ic6jw28v6h5cig3ws2c9d0wifhy2c6m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-equivalent" ,rust-equivalent-1)        
       ("rust-hashbrown" ,rust-hashbrown_0_14_3)        
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
      (beautify-description "A hash table with consistent order and fast iteration."))
    (license (list license:asl2.0 license:expat))))

(define rust-hashbrown_0_14_3
  (package
    (name "rust-hashbrown")
    (version "0.14.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "012nywlg0lj9kwanh69my5x67vjlfmzfi9a0rq4qvis2j8fil3r9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ahash" ,rust-ahash_0_8_7)        
       ("rust-allocator-api2" ,rust-allocator-api2_0_2_16))))
    (home-page "None")
    (synopsis "A Rust port of Google\u0027s SwissTable hash map")
    (description
      (beautify-description "A Rust port of Google\u0027s SwissTable hash map"))
    (license (list license:expat license:asl2.0))))

(define rust-allocator-api2_0_2_16
  (package
    (name "rust-allocator-api2")
    (version "0.2.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "allocator-api2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iayppgq4wqbfbfcqmsbwgamj0s65012sskfvyx07pxavk3gyhh9"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/zakarumych/allocator-api2")
    (synopsis "Mirror of Rust\u0027s allocator API")
    (description
      (beautify-description "Mirror of Rust\u0027s allocator API"))
    (license (list license:expat license:asl2.0))))

(define rust-ahash_0_8_7
  (package
    (name "rust-ahash")
    (version "0.8.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ahash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "008xw6gigwnf0q01ic4ar2y4dqfnzn3kyys6vd4cvfa3imjakhvp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-getrandom" ,rust-getrandom_0_2_12)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-version_check" ,rust-version_check_0_9_4)        
       ("rust-zerocopy" ,rust-zerocopy_0_7_32))))
    (home-page "None")
    (synopsis "A non-cryptographic hash function using AES-NI for high performance")
    (description
      (beautify-description "A non-cryptographic hash function using AES-NI for high performance"))
    (license (list license:expat license:asl2.0))))

(define rust-zerocopy_0_7_32
  (package
    (name "rust-zerocopy")
    (version "0.7.32")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zerocopy" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ghnfxw69kx5d1aqfd5fsfrra9dgpz17yqx84nd4ryjk3sbd7m3l"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-zerocopy-derive" ,rust-zerocopy-derive_0_7_32))))
    (home-page "None")
    (synopsis "Utilities for zero-copy parsing and serialization")
    (description
      (beautify-description "Utilities for zero-copy parsing and serialization"))
    (license (list license:bsd-2 license:asl2.0 license:expat))))

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
    (license (list license:expat license:asl2.0))))

(define rust-getrandom_0_2_12
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
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-wasi" ,rust-wasi-0.11))))
    (home-page "None")
    (synopsis "A small cross-platform library for retrieving random data from system source")
    (description
      (beautify-description "A small cross-platform library for retrieving random data from system source"))
    (license (list license:expat license:asl2.0))))

(define rust-zerocopy-derive_0_7_32
  (package
    (name "rust-zerocopy-derive")
    (version "0.7.32")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zerocopy-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19nj11md42aijyqnfx8pa647fjzhz537xyc624rajwwfrn6b3qcw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description
      (beautify-description "Custom derive for traits from the zerocopy crate"))
    (license (list license:bsd-2 license:asl2.0 license:expat))))

(define rust-proc-macro2_1_0_78
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

(define rust-syn_2_0_48
  (package
    (name "rust-syn")
    (version "2.0.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gqgfygmrxmp8q32lia9p294kdd501ybn6kn2h4gqza0irik2d8g"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
      (beautify-description "Parser for Rust source code"))
    (license (list license:expat license:asl2.0))))

(define rust-quote_1_0_35
  (package
    (name "rust-quote")
    (version "1.0.35")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vv8r2ncaz4pqdr78x7f138ka595sp2ncr1sa2plm4zxbsmwj7i9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78))))
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
      (beautify-description "Quasi-quoting macro quote!(...)"))
    (license (list license:expat license:asl2.0))))

(define rust-windows-sys_0_52_0
  (package
    (name "rust-windows-sys")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows-targets" ,rust-windows-targets_0_52_0))))
    (home-page "None")
    (synopsis "Rust for Windows")
    (description
      (beautify-description "Rust for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-redox_syscall_0_4_1
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

(define rust-windows-targets_0_52_0
  (package
    (name "rust-windows-targets")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows-targets" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kg7a27ynzw8zz3krdgy6w5gbqcji27j1sz4p7xk2j5j8082064a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows_aarch64_gnullvm" ,rust-windows_aarch64_gnullvm_0_52_0)        
       ("rust-windows_aarch64_msvc" ,rust-windows_aarch64_msvc_0_52_0)        
       ("rust-windows_i686_gnu" ,rust-windows_i686_gnu_0_52_0)        
       ("rust-windows_i686_msvc" ,rust-windows_i686_msvc_0_52_0)        
       ("rust-windows_x86_64_gnu" ,rust-windows_x86_64_gnu_0_52_0)        
       ("rust-windows_x86_64_gnullvm" ,rust-windows_x86_64_gnullvm_0_52_0)        
       ("rust-windows_x86_64_msvc" ,rust-windows_x86_64_msvc_0_52_0))))
    (home-page "None")
    (synopsis "Import libs for Windows")
    (description
      (beautify-description "Import libs for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-windows_aarch64_msvc_0_52_0
  (package
    (name "rust-windows_aarch64_msvc")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_aarch64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vvmy1ypvzdvxn9yf0b8ygfl85gl2gpcyvsvqppsmlpisil07amv"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-windows_x86_64_gnullvm_0_52_0
  (package
    (name "rust-windows_x86_64_gnullvm")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_gnullvm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17lllq4l2k1lqgcnw1cccphxp9vs7inq99kjlm2lfl9zklg7wr8s"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-windows_aarch64_gnullvm_0_52_0
  (package
    (name "rust-windows_aarch64_gnullvm")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_aarch64_gnullvm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1shmn1kbdc0bpphcxz0vlph96bxz0h1jlmh93s9agf2dbpin8xyb"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-windows_x86_64_gnu_0_52_0
  (package
    (name "rust-windows_x86_64_gnu")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zdy4qn178sil5sdm63lm7f0kkcjg6gvdwmcprd2yjmwn8ns6vrx"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-windows_x86_64_msvc_0_52_0
  (package
    (name "rust-windows_x86_64_msvc")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_x86_64_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "012wfq37f18c09ij5m6rniw7xxn5fcvrxbqd0wd8vgnl3hfn9yfz"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-windows_i686_msvc_0_52_0
  (package
    (name "rust-windows_i686_msvc")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_msvc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16kvmbvx0vr0zbgnaz6nsks9ycvfh5xp05bjrhq65kj623iyirgz"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-windows_i686_gnu_0_52_0
  (package
    (name "rust-windows_i686_gnu")
    (version "0.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "windows_i686_gnu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04zkglz4p3pjsns5gbz85v4s5aw102raz4spj4b0lmm33z5kg1m2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Import lib for Windows")
    (description
      (beautify-description "Import lib for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-thiserror-impl_1_0_56
  (package
    (name "rust-thiserror-impl")
    (version "1.0.56")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w9ldp8fa574ilz4dn7y7scpcq66vdjy59qal8qdpwsh7faal3zs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
      (beautify-description "Implementation detail of the `thiserror` crate"))
    (license (list license:expat license:asl2.0))))

(define rust-tracing-core_0_1_32
  (package
    (name "rust-tracing-core")
    (version "0.1.32")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m5aglin3cdwxpvbg6kz0r9r0k31j48n0kcfwsp6l49z26k3svf0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-valuable" ,rust-valuable-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing.")
    (description
      (beautify-description "Core primitives for application-level tracing."))
    (license (list license:expat))))

(define rust-tracing-attributes_0_1_27
  (package
    (name "rust-tracing-attributes")
    (version "0.1.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-attributes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rvb5dn9z6d0xdj14r403z0af0bbaqhg02hq4jc97g5wds6lqw1l"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://tokio.rs")
    (synopsis "Procedural macro attributes for automatically instrumenting functions.")
    (description
      (beautify-description "Procedural macro attributes for automatically instrumenting functions."))
    (license (list license:expat))))

(define rust-typed-builder_0_18_1
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

(define rust-pretty_assertions_1_4_0
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

(define rust-sqlx_0_7_3
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

(define rust-typed-builder-macro_0_18_1
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
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Compile-time type-checked builder derive")
    (description
      (beautify-description "Compile-time type-checked builder derive"))
    (license (list license:expat license:asl2.0))))

(define rust-sqlx-core_0_7_3
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
      (("rust-ahash" ,rust-ahash_0_8_7)        
       ("rust-atoi" ,rust-atoi_2_0_0)        
       ("rust-byteorder" ,rust-byteorder-1)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-crc" ,rust-crc_3_0_1)        
       ("rust-crossbeam-queue" ,rust-crossbeam-queue_0_3_11)        
       ("rust-dotenvy" ,rust-dotenvy-0.15)        
       ("rust-either" ,rust-either_1_9_0)        
       ("rust-event-listener" ,rust-event-listener-2)        
       ("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-intrusive" ,rust-futures-intrusive_0_5_0)        
       ("rust-futures-io" ,rust-futures-io_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-hashlink" ,rust-hashlink_0_8_4)        
       ("rust-hex" ,rust-hex-0.4)        
       ("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-paste" ,rust-paste-1)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-rustls" ,rust-rustls_0_21_10)        
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_1_0_4)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-smallvec" ,rust-smallvec_1_13_1)        
       ("rust-sqlformat" ,rust-sqlformat_0_2_3)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-stream" ,rust-tokio-stream-0.1)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-url" ,rust-url_2_5_0)        
       ("rust-uuid" ,rust-uuid_1_7_0)        
       ("rust-webpki-roots" ,rust-webpki-roots_0_25_3))))
    (home-page "None")
    (synopsis "Core of SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
      (beautify-description "Core of SQLx, the rust SQL toolkit. Not intended to be used directly."))
    (license (list license:expat license:asl2.0))))

(define rust-sqlx-mysql_0_7_3
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
      (("rust-atoi" ,rust-atoi_2_0_0)        
       ("rust-base64" ,rust-base64_0_21_7)        
       ("rust-bitflags" ,rust-bitflags_2_4_2)        
       ("rust-byteorder" ,rust-byteorder-1)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-crc" ,rust-crc_3_0_1)        
       ("rust-digest" ,rust-digest-0.10)        
       ("rust-dotenvy" ,rust-dotenvy-0.15)        
       ("rust-either" ,rust-either_1_9_0)        
       ("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-io" ,rust-futures-io_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-generic-array" ,rust-generic-array-0.14)        
       ("rust-hex" ,rust-hex-0.4)        
       ("rust-hkdf" ,rust-hkdf_0_12_4)        
       ("rust-hmac" ,rust-hmac-0.12)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-md-5" ,rust-md-5_0_10_6)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-rsa" ,rust-rsa_0_9_6)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-sha1" ,rust-sha1_0_10_6)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-smallvec" ,rust-smallvec_1_13_1)        
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)        
       ("rust-stringprep" ,rust-stringprep_0_1_4)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-uuid" ,rust-uuid_1_7_0)        
       ("rust-whoami" ,rust-whoami-1))))
    (home-page "None")
    (synopsis "MySQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
      (beautify-description "MySQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details."))
    (license (list license:expat license:asl2.0))))

(define rust-sqlx-macros_0_7_3
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
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)        
       ("rust-sqlx-macros-core" ,rust-sqlx-macros-core_0_7_3)        
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
      (beautify-description "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly."))
    (license (list license:expat license:asl2.0))))

(define rust-sqlx-postgres_0_7_3
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
      (("rust-atoi" ,rust-atoi_2_0_0)        
       ("rust-base64" ,rust-base64_0_21_7)        
       ("rust-bitflags" ,rust-bitflags_2_4_2)        
       ("rust-byteorder" ,rust-byteorder-1)        
       ("rust-crc" ,rust-crc_3_0_1)        
       ("rust-dotenvy" ,rust-dotenvy-0.15)        
       ("rust-etcetera" ,rust-etcetera_0_8_0)        
       ("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-io" ,rust-futures-io_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-hex" ,rust-hex-0.4)        
       ("rust-hkdf" ,rust-hkdf_0_12_4)        
       ("rust-hmac" ,rust-hmac-0.12)        
       ("rust-home" ,rust-home_0_5_9)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-md-5" ,rust-md-5_0_10_6)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-sha1" ,rust-sha1_0_10_6)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-smallvec" ,rust-smallvec_1_13_1)        
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)        
       ("rust-stringprep" ,rust-stringprep_0_1_4)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-uuid" ,rust-uuid_1_7_0)        
       ("rust-whoami" ,rust-whoami-1))))
    (home-page "None")
    (synopsis "PostgreSQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
      (beautify-description "PostgreSQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details."))
    (license (list license:expat license:asl2.0))))

(define rust-sqlx-sqlite_0_7_3
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
      (("rust-atoi" ,rust-atoi_2_0_0)        
       ("rust-flume" ,rust-flume_0_11_0)        
       ("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-executor" ,rust-futures-executor_0_3_30)        
       ("rust-futures-intrusive" ,rust-futures-intrusive_0_5_0)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys_0_27_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-url" ,rust-url_2_5_0)        
       ("rust-urlencoding" ,rust-urlencoding_2_1_3)        
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "None")
    (synopsis "SQLite driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details.")
    (description
      (beautify-description "SQLite driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details."))
    (license (list license:expat license:asl2.0))))

(define rust-bytes_1_5_0
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

(define rust-rustls_0_21_10
  (package
    (name "rust-rustls")
    (version "0.21.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fmpzk3axnhkd99saqkvraifdfms4pkyi56lkihf8n877j0sdmgr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log-0.4)        
       ("rust-ring" ,rust-ring_0_17_7)        
       ("rust-rustls-webpki" ,rust-rustls-webpki_0_101_7)        
       ("rust-sct" ,rust-sct_0_7_1))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Rustls is a modern TLS library written in Rust.")
    (description
      (beautify-description "Rustls is a modern TLS library written in Rust."))
    (license (list license:asl2.0 license:isc license:expat))))

(define rust-url_2_5_0
  (package
    (name "rust-url")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cs65961miawncdg2z20171w0vqrmraswv2ihdpd8lxp7cp31rii"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-form_urlencoded" ,rust-form_urlencoded_1_2_1)        
       ("rust-idna" ,rust-idna_0_5_0)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1))))
    (home-page "None")
    (synopsis "URL library for Rust, based on the WHATWG URL Standard")
    (description
      (beautify-description "URL library for Rust, based on the WHATWG URL Standard"))
    (license (list license:expat license:asl2.0))))

(define rust-percent-encoding_2_3_1
  (package
    (name "rust-percent-encoding")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Percent encoding and decoding")
    (description
      (beautify-description "Percent encoding and decoding"))
    (license (list license:expat license:asl2.0))))

(define rust-rustls-pemfile_1_0_4
  (package
    (name "rust-rustls-pemfile")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-pemfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description
      (beautify-description "Basic .pem file parser for keys and certificates"))
    (license (list license:asl2.0 license:isc license:expat))))

(define rust-crossbeam-queue_0_3_11
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
      (("rust-crossbeam-utils" ,rust-crossbeam-utils_0_8_19))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-queue")
    (synopsis "Concurrent queues")
    (description
      (beautify-description "Concurrent queues"))
    (license (list license:expat license:asl2.0))))

(define rust-sqlformat_0_2_3
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
      (("rust-itertools" ,rust-itertools_0_12_0)        
       ("rust-nom" ,rust-nom_7_1_3)        
       ("rust-unicode_categories" ,rust-unicode_categories_0_1_1))))
    (home-page "https://github.com/shssoichiro/sqlformat-rs")
    (synopsis "Formats whitespace in a SQL string to make it easier to read")
    (description
      (beautify-description "Formats whitespace in a SQL string to make it easier to read"))
    (license (list license:expat license:asl2.0))))

(define rust-hashlink_0_8_4
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
      (("rust-hashbrown" ,rust-hashbrown_0_14_3))))
    (home-page "None")
    (synopsis "HashMap-like containers that hold their key-value pairs in a user controllable order")
    (description
      (beautify-description "HashMap-like containers that hold their key-value pairs in a user controllable order"))
    (license (list license:expat license:asl2.0))))

(define rust-futures-intrusive_0_5_0
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
      (("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-lock_api" ,rust-lock_api_0_4_11)        
       ("rust-parking_lot" ,rust-parking_lot_0_12_1))))
    (home-page "https://github.com/Matthias247/futures-intrusive")
    (synopsis "Futures based on intrusive data structures - for std and no-std environments.")
    (description
      (beautify-description "Futures based on intrusive data structures - for std and no-std environments."))
    (license (list license:expat license:asl2.0))))

(define rust-smallvec_1_13_1
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

(define rust-futures-io_0_3_30
  (package
    (name "rust-futures-io")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-io" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hgh25isvsr4ybibywhr4dpys8mjnscw4wfxxwca70cn1gi26im4"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.")
    (description
      (beautify-description "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library."))
    (license (list license:expat license:asl2.0))))

(define rust-futures-core_0_3_30
  (package
    (name "rust-futures-core")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07aslayrn3lbggj54kci0ishmd1pr367fp7iks7adia1p05miinz"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.")
    (description
      (beautify-description "The core traits and types in for the `futures` library."))
    (license (list license:expat license:asl2.0))))

(define rust-futures-channel_0_3_30
  (package
    (name "rust-futures-channel")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-channel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y6b7xxqdjm9hlcjpakcg41qfl7lihf6gavk8fyqijsxhvbzgj7a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-sink" ,rust-futures-sink_0_3_30))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.")
    (description
      (beautify-description "Channels for asynchronous communication using futures-rs."))
    (license (list license:expat license:asl2.0))))

(define rust-webpki-roots_0_25_3
  (package
    (name "rust-webpki-roots")
    (version "0.25.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki-roots" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "045g7az4mj1002m55iydln4jhyah4br2n0zms3wbz41vicpa8y0p"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rustls/webpki-roots")
    (synopsis "Mozilla\u0027s CA root certificates for use with webpki")
    (description
      (beautify-description "Mozilla\u0027s CA root certificates for use with webpki"))
    (license (list license:mpl2.0))))

(define rust-atoi_2_0_0
  (package
    (name "rust-atoi")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atoi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a05h42fggmy7h0ajjv6m7z72l924i7igbx13hk9d8pyign9k3gj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "None")
    (synopsis "Parse integers directly from `[u8]` slices in safe code")
    (description
      (beautify-description "Parse integers directly from `[u8]` slices in safe code"))
    (license (list license:expat))))

(define rust-either_1_9_0
  (package
    (name "rust-either")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01qy3anr7jal5lpc20791vxrw0nl6vksb5j7x56q2fycgcyy8sm2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      (beautify-description "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases."))
    (license (list license:expat license:asl2.0))))

(define rust-crc_3_0_1
  (package
    (name "rust-crc")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zkx87a5x06xfd6xm5956w4vmdfs0wcxpsn7iwj5jbp2rcapmv46"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crc-catalog" ,rust-crc-catalog_2_4_0))))
    (home-page "None")
    (synopsis "Rust implementation of CRC with support of various standards")
    (description
      (beautify-description "Rust implementation of CRC with support of various standards"))
    (license (list license:expat license:asl2.0))))

(define rust-rustls-webpki_0_101_7
  (package
    (name "rust-rustls-webpki")
    (version "0.101.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-webpki" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ring" ,rust-ring_0_17_7)        
       ("rust-untrusted" ,rust-untrusted_0_9_0))))
    (home-page "None")
    (synopsis "Web PKI X.509 Certificate Verification.")
    (description
      (beautify-description "Web PKI X.509 Certificate Verification."))
    (license (list license:isc))))

(define rust-sct_0_7_1
  (package
    (name "rust-sct")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sct" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ring" ,rust-ring_0_17_7)        
       ("rust-untrusted" ,rust-untrusted_0_9_0))))
    (home-page "https://github.com/rustls/sct.rs")
    (synopsis "Certificate transparency SCT verification library")
    (description
      (beautify-description "Certificate transparency SCT verification library"))
    (license (list license:asl2.0 license:isc license:expat))))

(define rust-ring_0_17_7
  (package
    (name "rust-ring")
    (version "0.17.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ring" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x5vvsp2424vll571xx085qf4hzljmwpz4x8n9l0j1c3akb67338"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_83)        
       ("rust-getrandom" ,rust-getrandom_0_2_12)        
       ("rust-spin" ,rust-spin-0.9)        
       ("rust-untrusted" ,rust-untrusted_0_9_0)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))
      #:cargo-development-inputs
      (("rust-libc" ,rust-libc_0_2_152))))
    (home-page "None")
    (synopsis "Safe, fast, small crypto using Rust.")
    (description
      (beautify-description "Safe, fast, small crypto using Rust."))
    (license (list ))))

(define rust-untrusted_0_9_0
  (package
    (name "rust-untrusted")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "untrusted" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted inputs in Rust.")
    (description
      (beautify-description "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of untrusted inputs in Rust."))
    (license (list license:isc))))

(define rust-cc_1_0_83
  (package
    (name "rust-cc")
    (version "1.0.83")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l643zidlb5iy1dskc5ggqs4wqa29a02f44piczqc8zcnsq4y5zi"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152))))
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode.")
    (description
      (beautify-description "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode."))
    (license (list license:expat license:asl2.0))))

(define rust-form_urlencoded_1_2_1
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
      (("rust-percent-encoding" ,rust-percent-encoding_2_3_1))))
    (home-page "None")
    (synopsis "Parser and serializer for the application/x-www-form-urlencoded syntax, as used by HTML forms.")
    (description
      (beautify-description "Parser and serializer for the application/x-www-form-urlencoded syntax, as used by HTML forms."))
    (license (list license:expat license:asl2.0))))

(define rust-idna_0_5_0
  (package
    (name "rust-idna")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "idna" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xhjrcjqq0l5bpzvdgylvpkgk94panxgsirzhjnnqfdgc4a9nkb3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-bidi" ,rust-unicode-bidi_0_3_15)        
       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))))
    (home-page "None")
    (synopsis "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (description
      (beautify-description "IDNA (Internationalizing Domain Names in Applications) and Punycode."))
    (license (list license:expat license:asl2.0))))

(define rust-unicode-bidi_0_3_15
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

(define rust-crossbeam-utils_0_8_19
  (package
    (name "rust-crossbeam-utils")
    (version "0.8.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iakrb1b8fjqrag7wphl94d10irhbh2fw1g444xslsywqyn3p3i4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
      (beautify-description "Utilities for concurrent programming"))
    (license (list license:expat license:asl2.0))))

(define rust-unicode_categories_0_1_1
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

(define rust-lock_api_0_4_11
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

(define rust-parking_lot_0_12_1
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

(define rust-scopeguard_1_2_0
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

(define rust-parking_lot_core_0_9_9
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
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_4_1)        
       ("rust-smallvec" ,rust-smallvec_1_13_1)        
       ("rust-windows-targets" ,rust-windows-targets_0_48_5))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license (list license:expat license:asl2.0))))

(define rust-windows-targets_0_48_5
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

(define rust-windows_i686_gnu_0_48_5
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

(define rust-windows_x86_64_gnullvm_0_48_5
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

(define rust-windows_x86_64_msvc_0_48_5
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

(define rust-windows_aarch64_gnullvm_0_48_5
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

(define rust-windows_i686_msvc_0_48_5
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

(define rust-windows_aarch64_msvc_0_48_5
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

(define rust-windows_x86_64_gnu_0_48_5
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

(define rust-futures-sink_0_3_30
  (package
    (name "rust-futures-sink")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-sink" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dag8xyyaya8n8mh8smx7x6w2dpmafg2din145v973a3hw7f1f4z"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.")
    (description
      (beautify-description "The asynchronous `Sink` trait for the futures-rs library."))
    (license (list license:expat license:asl2.0))))

(define rust-crc-catalog_2_4_0
  (package
    (name "rust-crc-catalog")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc-catalog" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Catalog of CRC algorithms (generated from http://reveng.sourceforge.net/crc-catalogue) expressed as simple Rust structs.")
    (description
      (beautify-description "Catalog of CRC algorithms (generated from http://reveng.sourceforge.net/crc-catalogue) expressed as simple Rust structs."))
    (license (list license:expat license:asl2.0))))

(define rust-sha1_0_10_6
  (package
    (name "rust-sha1")
    (version "0.10.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-cpufeatures" ,rust-cpufeatures_0_2_12)        
       ("rust-digest" ,rust-digest-0.10))))
    (home-page "None")
    (synopsis "SHA-1 hash function")
    (description
      (beautify-description "SHA-1 hash function"))
    (license (list license:expat license:asl2.0))))

(define rust-stringprep_0_1_4
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
       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))))
    (home-page "None")
    (synopsis "An implementation of the stringprep algorithm")
    (description
      (beautify-description "An implementation of the stringprep algorithm"))
    (license (list license:expat license:asl2.0))))

(define rust-itoa_1_0_10
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

(define rust-hkdf_0_12_4
  (package
    (name "rust-hkdf")
    (version "0.12.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hkdf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hmac" ,rust-hmac-0.12))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
      (beautify-description "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)"))
    (license (list license:expat license:asl2.0))))

(define rust-rsa_0_9_6
  (package
    (name "rust-rsa")
    (version "0.9.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rsa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z0d1aavfm0v4pv8jqmqhhvvhvblla1ydzlvwykpc3mkzhj523jx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-const-oid" ,rust-const-oid_0_9_6)        
       ("rust-digest" ,rust-digest-0.10)        
       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)        
       ("rust-num-integer" ,rust-num-integer-0.1)        
       ("rust-num-traits" ,rust-num-traits-0.2)        
       ("rust-pkcs1" ,rust-pkcs1-0.7)        
       ("rust-pkcs8" ,rust-pkcs8-0.10)        
       ("rust-signature" ,rust-signature_2_2_0)        
       ("rust-spki" ,rust-spki_0_7_3)        
       ("rust-subtle" ,rust-subtle_2_5_0)        
       ("rust-zeroize" ,rust-zeroize_1_7_0))
      #:cargo-development-inputs
      (("rust-rand_core" ,rust-rand_core_0_6_4))))
    (home-page "None")
    (synopsis "Pure Rust RSA implementation")
    (description
      (beautify-description "Pure Rust RSA implementation"))
    (license (list license:expat license:asl2.0))))

(define rust-bitflags_2_4_2
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
      (("rust-serde" ,rust-serde_1_0_195))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
      (beautify-description "A macro to generate structures which behave like bitflags."))
    (license (list license:expat license:asl2.0))))

(define rust-md-5_0_10_6
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

(define rust-cpufeatures_0_2_12
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
      (("rust-libc" ,rust-libc_0_2_152))))
    (home-page "None")
    (synopsis "Lightweight runtime CPU feature detection for aarch64, loongarch64, and x86/x86_64 targets, \nwith no_std support and support for mobile targets including Android and iOS")
    (description
      (beautify-description "Lightweight runtime CPU feature detection for aarch64, loongarch64, and x86/x86_64 targets, \nwith no_std support and support for mobile targets including Android and iOS"))
    (license (list license:expat license:asl2.0))))

(define rust-finl_unicode_1_2_0
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

(define rust-signature_2_2_0
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

(define rust-subtle_2_5_0
  (package
    (name "rust-subtle")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "subtle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1g2yjs7gffgmdvkkq0wrrh0pxds3q0dv6dhkw9cdpbib656xdkc1"))))
    (build-system cargo-build-system)
    (home-page "https://dalek.rs/")
    (synopsis "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (description
      (beautify-description "Pure-Rust traits and utilities for constant-time cryptographic implementations."))
    (license (list license:bsd-3))))

(define rust-const-oid_0_9_6
  (package
    (name "rust-const-oid")
    (version "0.9.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const-oid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard\nas defined in ITU X.660, with support for BER/DER encoding/decoding as well as\nheapless no_std (i.e. embedded) support")
    (description
      (beautify-description "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard\nas defined in ITU X.660, with support for BER/DER encoding/decoding as well as\nheapless no_std (i.e. embedded) support"))
    (license (list license:asl2.0 license:expat))))

(define rust-zeroize_1_7_0
  (package
    (name "rust-zeroize")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zeroize" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bfvby7k9pdp6623p98yz2irqnamcyzpn7zh20nqmdn68b0lwnsj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-zeroize_derive" ,rust-zeroize_derive_1_4_2))))
    (home-page "None")
    (synopsis "Securely clear secrets from memory with a simple trait built on\nstable Rust primitives which guarantee memory is zeroed using an\noperation will not be \u0027optimized away\u0027 by the compiler.\nUses a portable pure Rust implementation that works everywhere,\neven WASM!")
    (description
      (beautify-description "Securely clear secrets from memory with a simple trait built on\nstable Rust primitives which guarantee memory is zeroed using an\noperation will not be \u0027optimized away\u0027 by the compiler.\nUses a portable pure Rust implementation that works everywhere,\neven WASM!"))
    (license (list license:asl2.0 license:expat))))

(define rust-spki_0_7_3
  (package
    (name "rust-spki")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spki" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64ct" ,rust-base64ct-1)        
       ("rust-der" ,rust-der_0_7_8))))
    (home-page "None")
    (synopsis "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their\nassociated AlgorithmIdentifiers (i.e. OIDs)")
    (description
      (beautify-description "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their\nassociated AlgorithmIdentifiers (i.e. OIDs)"))
    (license (list license:asl2.0 license:expat))))

(define rust-rand_core_0_6_4
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

(define rust-zeroize_derive_1_4_2
  (package
    (name "rust-zeroize_derive")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zeroize_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Custom derive support for zeroize")
    (description
      (beautify-description "Custom derive support for zeroize"))
    (license (list license:asl2.0 license:expat))))

(define rust-der_0_7_8
  (package
    (name "rust-der")
    (version "0.7.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "der" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "070bwiyr80800h31c5zd96ckkgagfjgnrrdmz3dzg2lccsd3dypz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-const-oid" ,rust-const-oid_0_9_6)        
       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.7)        
       ("rust-zeroize" ,rust-zeroize_1_7_0))))
    (home-page "None")
    (synopsis "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules\n(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with\nfull support for heapless no_std targets")
    (description
      (beautify-description "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules\n(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with\nfull support for heapless no_std targets"))
    (license (list license:asl2.0 license:expat))))

(define rust-sqlx-macros-core_0_7_3
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
      (("rust-atomic-write-file" ,rust-atomic-write-file_0_1_2)        
       ("rust-dotenvy" ,rust-dotenvy-0.15)        
       ("rust-either" ,rust-either_1_9_0)        
       ("rust-heck" ,rust-heck_0_4_1)        
       ("rust-hex" ,rust-hex-0.4)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-sqlx-core" ,rust-sqlx-core_0_7_3)        
       ("rust-sqlx-mysql" ,rust-sqlx-mysql_0_7_3)        
       ("rust-sqlx-postgres" ,rust-sqlx-postgres_0_7_3)        
       ("rust-sqlx-sqlite" ,rust-sqlx-sqlite_0_7_3)        
       ("rust-syn" ,rust-syn-1)        
       ("rust-tempfile" ,rust-tempfile_3_9_0)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-url" ,rust-url_2_5_0))))
    (home-page "None")
    (synopsis "Macro support core for SQLx, the Rust SQL toolkit. Not intended to be used directly.")
    (description
      (beautify-description "Macro support core for SQLx, the Rust SQL toolkit. Not intended to be used directly."))
    (license (list license:expat license:asl2.0))))

(define rust-atomic-write-file_0_1_2
  (package
    (name "rust-atomic-write-file")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atomic-write-file" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dl4x0srdwjxm3zz3fj1c7m44i3b7mjiad550fqklj1n4bfbxkgd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nix" ,rust-nix-0.27)        
       ("rust-rand" ,rust-rand-0.8))))
    (home-page "None")
    (synopsis "Write files atomically to a file system")
    (description
      (beautify-description "Write files atomically to a file system"))
    (license (list license:bsd-3))))

(define rust-heck_0_4_1
  (package
    (name "rust-heck")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      (beautify-description "heck is a case conversion library."))
    (license (list license:expat license:asl2.0))))

(define rust-etcetera_0_8_0
  (package
    (name "rust-etcetera")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "etcetera" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hxrsn75dirbjhwgkdkh0pnpqrnq17ypyhjpjaypgax1hd91nv8k"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-home" ,rust-home_0_5_9)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/lunacookies/etcetera")
    (synopsis "An unopinionated library for obtaining configuration, data, cache, \u0026 other directories")
    (description
      (beautify-description "An unopinionated library for obtaining configuration, data, cache, \u0026 other directories"))
    (license (list license:expat license:asl2.0))))

(define rust-home_0_5_9
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
      (("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "None")
    (synopsis "Shared definitions of home directories.")
    (description
      (beautify-description "Shared definitions of home directories."))
    (license (list license:expat license:asl2.0))))

(define rust-urlencoding_2_1_3
  (package
    (name "rust-urlencoding")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "urlencoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))))
    (build-system cargo-build-system)
    (home-page "https://lib.rs/urlencoding")
    (synopsis "A Rust library for doing URL percentage encoding.")
    (description
      (beautify-description "A Rust library for doing URL percentage encoding."))
    (license (list license:expat))))

(define rust-flume_0_11_0
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
      (("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-sink" ,rust-futures-sink_0_3_30)        
       ("rust-spin" ,rust-spin-0.9))))
    (home-page "None")
    (synopsis "A blazingly fast multi-producer channel")
    (description
      (beautify-description "A blazingly fast multi-producer channel"))
    (license (list license:asl2.0 license:expat))))

(define rust-libsqlite3-sys_0_27_0
  (package
    (name "rust-libsqlite3-sys")
    (version "0.27.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libsqlite3-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05pp60ncrmyjlxxjj187808jkvpxm06w5lvvdwwvxd2qrmnj4kng"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_83)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_29)        
       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "None")
    (synopsis "Native bindings to the libsqlite3 library")
    (description
      (beautify-description "Native bindings to the libsqlite3 library"))
    (license (list license:expat))))

(define rust-futures-executor_0_3_30
  (package
    (name "rust-futures-executor")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-executor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07dh08gs9vfll2h36kq32q9xd86xm6lyl9xikmmwlkqnmrrgqxm5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-task" ,rust-futures-task_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Executors for asynchronous tasks based on the futures-rs library.")
    (description
      (beautify-description "Executors for asynchronous tasks based on the futures-rs library."))
    (license (list license:expat license:asl2.0))))

(define rust-pkg-config_0_3_29
  (package
    (name "rust-pkg-config")
    (version "0.3.29")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkg-config" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jy6158v1316khkpmq2sjj1vgbnbnw51wffx7p0k0l9h9vlys019"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts.")
    (description
      (beautify-description "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts."))
    (license (list license:expat license:asl2.0))))

(define rust-futures-task_0_3_30
  (package
    (name "rust-futures-task")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-task" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "013h1724454hj8qczp8vvs10qfiqrxr937qsrv6rhii68ahlzn1q"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.")
    (description
      (beautify-description "Tools for working with tasks."))
    (license (list license:expat license:asl2.0))))

(define rust-logos_0_13_0
  (package
    (name "rust-logos")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "logos" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hfjqmmcq6fbfwpca6874b1k3lsqi75n584kkg4qmwcgj16wl060"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-logos-derive" ,rust-logos-derive_0_13_0))))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description
      (beautify-description "Create ridiculously fast Lexers"))
    (license (list license:expat license:asl2.0))))

(define rust-logos-derive_0_13_0
  (package
    (name "rust-logos-derive")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "logos-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zi6s400yfw1ma7wnawyjjgbq1nqmx0xjdh18j8dfhhzkwi0vz6v"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-logos-codegen" ,rust-logos-codegen_0_13_0))))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description
      (beautify-description "Create ridiculously fast Lexers"))
    (license (list license:expat license:asl2.0))))

(define rust-logos-codegen_0_13_0
  (package
    (name "rust-logos-codegen")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "logos-codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s7drl8vfp9viw9mfyz8dll1gfvp1dc6np82abj0402y548p6j6w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-beef" ,rust-beef_0_5_2)        
       ("rust-fnv" ,rust-fnv-1)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-regex-syntax" ,rust-regex-syntax_0_6_29)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description
      (beautify-description "Create ridiculously fast Lexers"))
    (license (list license:expat license:asl2.0))))

(define rust-beef_0_5_2
  (package
    (name "rust-beef")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "beef" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c95lbnhld96iwwbyh5kzykbpysq0fnjfhwxa1mhap5qxgrl30is"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "More compact Cow")
    (description
      (beautify-description "More compact Cow"))
    (license (list license:expat license:asl2.0))))

(define rust-regex-syntax_0_6_29
  (package
    (name "rust-regex-syntax")
    (version "0.6.29")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A regular expression parser.")
    (description
      (beautify-description "A regular expression parser."))
    (license (list license:expat license:asl2.0))))

(define rust-atuin-server-database_18_0_1
  (package
    (name "rust-atuin-server-database")
    (version "18.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-server-database" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lbr9im8pijjzmg0a29z67bfvczx9ydx6gfy8vg5rmwqrayw770h"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-atuin-common" ,rust-atuin-common_18_0_1)        
       ("rust-eyre" ,rust-eyre_0_6_11)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "https://atuin.sh")
    (synopsis "server database library for atuin")
    (description
      (beautify-description "server database library for atuin"))
    (license (list license:expat))))

(define rust-serde_derive_1_0_195
  (package
    (name "rust-serde_derive")
    (version "1.0.195")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b7ag1qm9q3fgwlmyk2ap5gjbqa9vyf2wfmj4xish6yq0f38zzj6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license (list license:expat license:asl2.0))))

(define rust-ryu_1_0_16
  (package
    (name "rust-ryu")
    (version "1.0.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k7b90xr48ag5bzmfjp82rljasw2fx28xr3bg1lrpx7b5sljm3gr"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast floating point to string conversion")
    (description
      (beautify-description "Fast floating point to string conversion"))
    (license (list license:asl2.0 license:boost1.0))))

(define rust-rustls-pemfile_2_0_0
  (package
    (name "rust-rustls-pemfile")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-pemfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x34xidvzn4br2vl8f8xwmhgbjv4lmlb0ggv5whlnk4yl87rir1m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)        
       ("rust-rustls-pki-types" ,rust-rustls-pki-types_1_1_0))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description
      (beautify-description "Basic .pem file parser for keys and certificates"))
    (license (list license:asl2.0 license:isc license:expat))))

(define rust-tower-http_0_5_1
  (package
    (name "rust-tower-http")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bmgfh1hr92blw7kv2636ykd0gf3h0aqdd89kvjk6b2fg8kr788d"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_2_4_2)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-http-body" ,rust-http-body_1_0_0)        
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-tower-layer" ,rust-tower-layer-0.3)        
       ("rust-tower-service" ,rust-tower-service-0.3)        
       ("rust-tracing" ,rust-tracing_0_1_40))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description
      (beautify-description "Tower middleware and utilities for HTTP clients and servers"))
    (license (list license:expat))))

(define rust-metrics-exporter-prometheus_0_12_2
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
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tracing" ,rust-tracing_0_1_40))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "A metrics-compatible exporter for sending metrics to Prometheus.")
    (description
      (beautify-description "A metrics-compatible exporter for sending metrics to Prometheus."))
    (license (list license:expat))))

(define rust-config_0_13_4
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
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-nom" ,rust-nom_7_1_3)        
       ("rust-pathdiff" ,rust-pathdiff-0.2)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/mehcode/config-rs")
    (synopsis "Layered configuration system for Rust applications.")
    (description
      (beautify-description "Layered configuration system for Rust applications."))
    (license (list license:expat license:asl2.0))))

(define rust-reqwest_0_11_23
  (package
    (name "rust-reqwest")
    (version "0.11.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "reqwest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hgvzb7r46656r9vqhl5qk1kbr2xzjb91yr2cb321160ka6sxc9p"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-encoding_rs" ,rust-encoding_rs_0_8_33)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-h2" ,rust-h2_0_3_24)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-http-body" ,rust-http-body_0_4_6)        
       ("rust-hyper-rustls" ,rust-hyper-rustls_0_24_2)        
       ("rust-ipnet" ,rust-ipnet_2_9_0)        
       ("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-mime" ,rust-mime_0_3_17)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-rustls" ,rust-rustls_0_21_10)        
       ("rust-rustls-native-certs" ,rust-rustls-native-certs_0_6_3)        
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_1_0_4)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-serde_urlencoded" ,rust-serde_urlencoded_0_7_1)        
       ("rust-system-configuration" ,rust-system-configuration_0_5_1)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-rustls" ,rust-tokio-rustls_0_24_1)        
       ("rust-tower-service" ,rust-tower-service-0.3)        
       ("rust-url" ,rust-url_2_5_0)        
       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures_0_4_40)        
       ("rust-web-sys" ,rust-web-sys_0_3_67)        
       ("rust-winreg" ,rust-winreg_0_50_0))
      #:cargo-development-inputs
      (("rust-hyper" ,rust-hyper_0_14_28)        
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90))))
    (home-page "None")
    (synopsis "higher level HTTP client library")
    (description
      (beautify-description "higher level HTTP client library"))
    (license (list license:expat license:asl2.0))))

(define rust-axum_0_7_4
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
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-axum-core" ,rust-axum-core_0_4_3)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-http-body" ,rust-http-body_1_0_0)        
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)        
       ("rust-hyper" ,rust-hyper_1_1_0)        
       ("rust-hyper-util" ,rust-hyper-util_0_1_2)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-matchit" ,rust-matchit_0_7_3)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-mime" ,rust-mime_0_3_17)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-serde_path_to_error" ,rust-serde_path_to_error_0_1_15)        
       ("rust-serde_urlencoded" ,rust-serde_urlencoded_0_7_1)        
       ("rust-sync_wrapper" ,rust-sync_wrapper_0_1_2)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tower-layer" ,rust-tower-layer-0.3)        
       ("rust-tower-service" ,rust-tower-service-0.3)        
       ("rust-tracing" ,rust-tracing_0_1_40))
      #:cargo-development-inputs
      (("rust-rustversion" ,rust-rustversion-1)        
       ("rust-tower" ,rust-tower-0.4))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description
      (beautify-description "Web framework that focuses on ergonomics and modularity"))
    (license (list license:expat))))

(define rust-argon2_0_5_3
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

(define rust-metrics_0_21_1
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
      (("rust-ahash" ,rust-ahash_0_8_7)        
       ("rust-metrics-macros" ,rust-metrics-macros_0_7_1)        
       ("rust-portable-atomic" ,rust-portable-atomic_1_6_0))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "A lightweight metrics facade.")
    (description
      (beautify-description "A lightweight metrics facade."))
    (license (list license:expat))))

(define rust-axum-server_0_6_0
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
      (("rust-arc-swap" ,rust-arc-swap-1)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-http-body" ,rust-http-body_1_0_0)        
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)        
       ("rust-hyper-util" ,rust-hyper-util_0_1_2)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-rustls" ,rust-rustls_0_21_10)        
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_2_0_0)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-rustls" ,rust-tokio-rustls_0_24_1)        
       ("rust-tower" ,rust-tower-0.4)        
       ("rust-tower-service" ,rust-tower-service-0.3))
      #:cargo-development-inputs
      (("rust-hyper" ,rust-hyper_1_1_0))))
    (home-page "https://github.com/programatik29/axum-server")
    (synopsis "High level server designed to be used with axum framework.")
    (description
      (beautify-description "High level server designed to be used with axum framework."))
    (license (list license:expat))))

(define rust-rustls-pki-types_1_1_0
  (package
    (name "rust-rustls-pki-types")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-pki-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ajf64gycqv02md59bpg23mg4v7b4l0q3iv04zj950g67jdrg7cy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rustls/pki-types")
    (synopsis "Shared types for the rustls PKI ecosystem")
    (description
      (beautify-description "Shared types for the rustls PKI ecosystem"))
    (license (list license:expat license:asl2.0))))

(define rust-http-body-util_0_1_0
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
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-http-body" ,rust-http-body_1_0_0)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "None")
    (synopsis "Combinators and adapters for HTTP request or response bodies.")
    (description
      (beautify-description "Combinators and adapters for HTTP request or response bodies."))
    (license (list license:expat))))

(define rust-http-body_1_0_0
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
       ("rust-http" ,rust-http_1_0_0))))
    (home-page "None")
    (synopsis "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (description
      (beautify-description "Trait representing an asynchronous, streaming, HTTP request or response body."))
    (license (list license:expat))))

(define rust-http_1_0_0
  (package
    (name "rust-http")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sllw565jn8r5w7h928nsfqq33x586pyasdfr7vid01scwwgsamk"))))
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

(define rust-metrics-util_0_15_1
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
       ("rust-crossbeam-utils" ,rust-crossbeam-utils_0_8_19)        
       ("rust-hashbrown" ,rust-hashbrown_0_13_1)        
       ("rust-metrics" ,rust-metrics_0_21_1)        
       ("rust-num_cpus" ,rust-num_cpus_1_16_0)        
       ("rust-quanta" ,rust-quanta_0_11_1)        
       ("rust-sketches-ddsketch" ,rust-sketches-ddsketch_0_2_1))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Helper types/functions used by the metrics ecosystem.")
    (description
      (beautify-description "Helper types/functions used by the metrics ecosystem."))
    (license (list license:expat))))

(define rust-hyper_0_14_28
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
       ("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-h2" ,rust-h2_0_3_24)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-http-body" ,rust-http-body_0_4_6)        
       ("rust-httparse" ,rust-httparse-1)        
       ("rust-httpdate" ,rust-httpdate_1_0_3)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-socket2" ,rust-socket2_0_5_5)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tower-service" ,rust-tower-service-0.3)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-want" ,rust-want-0.3))
      #:cargo-development-inputs
      (("rust-futures-util" ,rust-futures-util_0_3_30))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description
      (beautify-description "A fast and correct HTTP library."))
    (license (list license:expat))))

(define rust-ipnet_2_9_0
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

(define rust-quanta_0_11_1
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
      (("rust-crossbeam-utils" ,rust-crossbeam-utils_0_8_19)        
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-mach2" ,rust-mach2_0_4_2)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-raw-cpuid" ,rust-raw-cpuid-10)        
       ("rust-wasi" ,rust-wasi-0.11)        
       ("rust-web-sys" ,rust-web-sys_0_3_67)        
       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description
      (beautify-description "high-speed timing library"))
    (license (list license:expat))))

(define rust-num_cpus_1_16_0
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
      (("rust-hermit-abi" ,rust-hermit-abi_0_3_4)        
       ("rust-libc" ,rust-libc_0_2_152))))
    (home-page "None")
    (synopsis "Get the number of CPUs on a machine.")
    (description
      (beautify-description "Get the number of CPUs on a machine."))
    (license (list license:expat license:asl2.0))))

(define rust-sketches-ddsketch_0_2_1
  (package
    (name "rust-sketches-ddsketch")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sketches-ddsketch" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q873ja2yvvls9327a7yw1mcprw0ia2cjj72snfg5mrfi30hd938"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mheffner/rust-sketches-ddsketch")
    (synopsis "A direct port of the Golang DDSketch implementation.")
    (description
      (beautify-description "A direct port of the Golang DDSketch implementation."))
    (license (list license:asl2.0))))

(define rust-hashbrown_0_13_1
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
      (("rust-ahash" ,rust-ahash_0_8_7))))
    (home-page "None")
    (synopsis "A Rust port of Google\u0027s SwissTable hash map")
    (description
      (beautify-description "A Rust port of Google\u0027s SwissTable hash map"))
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
      (("rust-crossbeam-utils" ,rust-crossbeam-utils_0_8_19))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description
      (beautify-description "Epoch-based garbage collection"))
    (license (list license:expat license:asl2.0))))

(define rust-hermit-abi_0_3_4
  (package
    (name "rust-hermit-abi")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hermit-abi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07v5vbwb9kx0yxgdpx15h38ynpzhaqx5ncriryipypi5707hwgax"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Hermit system calls definitions.")
    (description
      (beautify-description "Hermit system calls definitions."))
    (license (list license:expat license:asl2.0))))

(define rust-http-body_0_4_6
  (package
    (name "rust-http-body")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http-body" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "None")
    (synopsis "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (description
      (beautify-description "Trait representing an asynchronous, streaming, HTTP request or response body."))
    (license (list license:expat))))

(define rust-http_0_2_11
  (package
    (name "rust-http")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fwz3mhh86h5kfnr5767jlx9agpdggclq7xsqx930fflzakb2iw9"))))
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

(define rust-socket2_0_5_5
  (package
    (name "rust-socket2")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sgq315f1njky114ip7wcy83qlphv9qclprfjwvxcpfblmcsqpvv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/rust-lang/socket2")
    (synopsis "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended.")
    (description
      (beautify-description "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended."))
    (license (list license:expat license:asl2.0))))

(define rust-httpdate_1_0_3
  (package
    (name "rust-httpdate")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "httpdate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "HTTP date parsing and formatting")
    (description
      (beautify-description "HTTP date parsing and formatting"))
    (license (list license:expat license:asl2.0))))

(define rust-h2_0_3_24
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
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-sink" ,rust-futures-sink_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-slab" ,rust-slab_0_4_9)        
       ("rust-tokio-util" ,rust-tokio-util_0_7_10)        
       ("rust-tracing" ,rust-tracing_0_1_40))
      #:cargo-development-inputs
      (("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "None")
    (synopsis "An HTTP/2 client and server")
    (description
      (beautify-description "An HTTP/2 client and server"))
    (license (list license:expat))))

(define rust-tokio-util_0_7_10
  (package
    (name "rust-tokio-util")
    (version "0.7.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "058y6x4mf0fsqji9rfyb77qbfyc50y4pk2spqgj6xsyr693z66al"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-sink" ,rust-futures-sink_0_3_30)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-tracing" ,rust-tracing_0_1_40))
      #:cargo-development-inputs
      (("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.")
    (description
      (beautify-description "Additional utilities for working with Tokio."))
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
      (("rust-autocfg" ,rust-autocfg-1))))
    (home-page "None")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description
      (beautify-description "Pre-allocated storage for a uniform data type"))
    (license (list license:expat))))

(define rust-web-sys_0_3_67
  (package
    (name "rust-web-sys")
    (version "0.3.67")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "web-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vfjjj3i49gy8bh8znnqhak1hx7xj9c2a3jzc0wpmgp0nqrj7kaq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/web-sys/index.html")
    (synopsis "Bindings for all Web APIs, a procedurally generated crate from WebIDL")
    (description
      (beautify-description "Bindings for all Web APIs, a procedurally generated crate from WebIDL"))
    (license (list license:expat license:asl2.0))))

(define rust-mach2_0_4_2
  (package
    (name "rust-mach2")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mach2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152))))
    (home-page "None")
    (synopsis "A Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX.")
    (description
      (beautify-description "A Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX."))
    (license (list license:bsd-2 license:expat license:asl2.0))))

(define rust-js-sys_0_3_67
  (package
    (name "rust-js-sys")
    (version "0.3.67")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "js-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lar78p13w781b4zf44a0sk26i461fczbdrhpan6kjav4gqkc7cs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bindings for all JS global objects and functions in all JS environments like\nNode.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (description
      (beautify-description "Bindings for all JS global objects and functions in all JS environments like\nNode.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate."))
    (license (list license:expat license:asl2.0))))

(define rust-wasm-bindgen_0_2_90
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.90")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01jlal3mynqwvqx4acrdnr9bvsdczaz2sy8lmmzmqh81lab348mi"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro_0_2_90))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.")
    (description
      (beautify-description "Easy support for interacting between JS and Rust."))
    (license (list license:expat license:asl2.0))))

(define rust-wasm-bindgen-macro_0_2_90
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.90")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16d980bql7y5krfqlmcr8mk1q4mrm0rmb0a99j92im5jc62j6k1y"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-quote" ,rust-quote_1_0_35)        
       ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support_0_2_90))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (description
      (beautify-description "Definition of the `#[wasm_bindgen]` attribute, an internal dependency"))
    (license (list license:expat license:asl2.0))))

(define rust-wasm-bindgen-macro-support_0_2_90
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.90")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro-support" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19r5bsyjw0fvim7dsj8pbwrq8v0ggh845lhfasgavhbdh2vapqds"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48)        
       ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend_0_2_90)        
       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared_0_2_90))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate")
    (description
      (beautify-description "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate"))
    (license (list license:expat license:asl2.0))))

(define rust-wasm-bindgen-backend_0_2_90
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.90")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kcxml9762zjdrn0h0n0qxfg1n7z1f577jcc5yimi3a0cddr7p7w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bumpalo" ,rust-bumpalo_3_14_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48)        
       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared_0_2_90))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool")
    (description
      (beautify-description "Backend code generation of the wasm-bindgen tool"))
    (license (list license:expat license:asl2.0))))

(define rust-wasm-bindgen-shared_0_2_90
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.90")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0av0m0shdg1jxhf66ymjbq03m0qb7ypm297glndm7mri3hxl34ad"))))
    (build-system cargo-build-system)
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Shared support between wasm-bindgen and wasm-bindgen cli, an internal\ndependency.")
    (description
      (beautify-description "Shared support between wasm-bindgen and wasm-bindgen cli, an internal\ndependency."))
    (license (list license:expat license:asl2.0))))

(define rust-bumpalo_3_14_0
  (package
    (name "rust-bumpalo")
    (version "3.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bumpalo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v4arnv9kwk54v5d0qqpv4vyw2sgr660nk0w3apzixi1cm3yfc3z"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A fast bump allocation arena for Rust.")
    (description
      (beautify-description "A fast bump allocation arena for Rust."))
    (license (list license:expat license:asl2.0))))

(define rust-mime_0_3_17
  (package
    (name "rust-mime")
    (version "0.3.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Strongly Typed Mimes")
    (description
      (beautify-description "Strongly Typed Mimes"))
    (license (list license:expat license:asl2.0))))

(define rust-tokio-rustls_0_24_1
  (package
    (name "rust-tokio-rustls")
    (version "0.24.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rustls" ,rust-rustls_0_21_10)        
       ("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "https://github.com/rustls/tokio-rustls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description
      (beautify-description "Asynchronous TLS/SSL streams for Tokio using Rustls."))
    (license (list license:expat license:asl2.0))))

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
      (("rust-form_urlencoded" ,rust-form_urlencoded_1_2_1)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-ryu" ,rust-ryu_1_0_16)        
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "`x-www-form-urlencoded` meets Serde")
    (description
      (beautify-description "`x-www-form-urlencoded` meets Serde"))
    (license (list license:expat license:asl2.0))))

(define rust-wasm-bindgen-futures_0_4_40
  (package
    (name "rust-wasm-bindgen-futures")
    (version "0.4.40")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-futures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qf4bzlinyg0s4b38fhzdi1cqdd7rgrywqdjr3ngmgc6xcm07qmx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90)        
       ("rust-web-sys" ,rust-web-sys_0_3_67))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Bridging the gap between Rust Futures and JavaScript Promises")
    (description
      (beautify-description "Bridging the gap between Rust Futures and JavaScript Promises"))
    (license (list license:expat license:asl2.0))))

(define rust-system-configuration_0_5_1
  (package
    (name "rust-system-configuration")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "system-configuration" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags-1)        
       ("rust-core-foundation" ,rust-core-foundation_0_9_4)        
       ("rust-system-configuration-sys" ,rust-system-configuration-sys_0_5_0))))
    (home-page "None")
    (synopsis "Bindings to SystemConfiguration framework for macOS")
    (description
      (beautify-description "Bindings to SystemConfiguration framework for macOS"))
    (license (list license:expat license:asl2.0))))

(define rust-winreg_0_50_0
  (package
    (name "rust-winreg")
    (version "0.50.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winreg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "None")
    (synopsis "Rust bindings to MS Windows Registry API")
    (description
      (beautify-description "Rust bindings to MS Windows Registry API"))
    (license (list license:expat))))

(define rust-rustls-native-certs_0_6_3
  (package
    (name "rust-rustls-native-certs")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls-native-certs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-openssl-probe" ,rust-openssl-probe_0_1_5)        
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_1_0_4)        
       ("rust-schannel" ,rust-schannel_0_1_23)        
       ("rust-security-framework" ,rust-security-framework-2))))
    (home-page "https://github.com/rustls/rustls-native-certs")
    (synopsis "rustls-native-certs allows rustls to use the platform native certificate store")
    (description
      (beautify-description "rustls-native-certs allows rustls to use the platform native certificate store"))
    (license (list license:asl2.0 license:isc license:expat))))

(define rust-hyper-rustls_0_24_2
  (package
    (name "rust-hyper-rustls")
    (version "0.24.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-hyper" ,rust-hyper_0_14_28)        
       ("rust-rustls" ,rust-rustls_0_21_10)        
       ("rust-tokio-rustls" ,rust-tokio-rustls_0_24_1))
      #:cargo-development-inputs
      (("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "https://github.com/rustls/hyper-rustls")
    (synopsis "Rustls+hyper integration for pure rust HTTPS")
    (description
      (beautify-description "Rustls+hyper integration for pure rust HTTPS"))
    (license (list license:asl2.0 license:isc license:expat))))

(define rust-encoding_rs_0_8_33
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

(define rust-core-foundation_0_9_4
  (package
    (name "rust-core-foundation")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-core-foundation-sys" ,rust-core-foundation-sys_0_8_6)        
       ("rust-libc" ,rust-libc_0_2_152))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
      (beautify-description "Bindings to Core Foundation for macOS"))
    (license (list license:expat license:asl2.0))))

(define rust-system-configuration-sys_0_5_0
  (package
    (name "rust-system-configuration-sys")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "system-configuration-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-core-foundation-sys" ,rust-core-foundation-sys_0_8_6)        
       ("rust-libc" ,rust-libc_0_2_152))))
    (home-page "None")
    (synopsis "Low level bindings to SystemConfiguration framework for macOS")
    (description
      (beautify-description "Low level bindings to SystemConfiguration framework for macOS"))
    (license (list license:expat license:asl2.0))))

(define rust-core-foundation-sys_0_8_6
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13w6sdf06r0hn7bx2b45zxsg1mm2phz34jikm6xc5qrbr6djpsh6"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
      (beautify-description "Bindings to Core Foundation for macOS"))
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
      (("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "None")
    (synopsis "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl")
    (description
      (beautify-description "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl"))
    (license (list license:expat))))

(define rust-hyper_1_1_0
  (package
    (name "rust-hyper")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xgv4bjm78w50wp2rcxc4dg69nw6blx6hyyqkqd7p4gwf4waanpv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-h2" ,rust-h2_0_4_2)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-http-body" ,rust-http-body_1_0_0)        
       ("rust-httparse" ,rust-httparse-1)        
       ("rust-httpdate" ,rust-httpdate_1_0_3)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description
      (beautify-description "A fast and correct HTTP library."))
    (license (list license:expat))))

(define rust-hyper-util_0_1_2
  (package
    (name "rust-hyper-util")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ryw1xzy1fa0cvh46s60dln2556vw80rbzccsr094nmy1nn9msmx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-http-body" ,rust-http-body_1_0_0)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-socket2" ,rust-socket2_0_5_5)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tracing" ,rust-tracing_0_1_40))
      #:cargo-development-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-hyper" ,rust-hyper_1_1_0))))
    (home-page "https://hyper.rs")
    (synopsis "hyper utilities")
    (description
      (beautify-description "hyper utilities"))
    (license (list license:expat))))

(define rust-sync_wrapper_0_1_2
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
    (synopsis "A tool for enlisting the compiler\u2019s help in proving the absence of concurrency")
    (description
      (beautify-description "A tool for enlisting the compiler\u2019s help in proving the absence of concurrency"))
    (license (list license:asl2.0))))

(define rust-axum-core_0_4_3
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
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-http-body" ,rust-http-body_1_0_0)        
       ("rust-http-body-util" ,rust-http-body-util_0_1_0)        
       ("rust-mime" ,rust-mime_0_3_17)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-rustversion" ,rust-rustversion-1)        
       ("rust-sync_wrapper" ,rust-sync_wrapper_0_1_2)        
       ("rust-tower-layer" ,rust-tower-layer-0.3)        
       ("rust-tower-service" ,rust-tower-service-0.3)        
       ("rust-tracing" ,rust-tracing_0_1_40))
      #:cargo-development-inputs
      (("rust-futures-util" ,rust-futures-util_0_3_30))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for axum")
    (description
      (beautify-description "Core types and traits for axum"))
    (license (list license:expat))))

(define rust-matchit_0_7_3
  (package
    (name "rust-matchit")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matchit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "156bgdmmlv4crib31qhgg49nsjk88dxkdqp80ha2pk2rk6n6ax0f"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A high performance, zero-copy URL router.")
    (description
      (beautify-description "A high performance, zero-copy URL router."))
    (license (list license:expat license:bsd-3))))

(define rust-serde_path_to_error_0_1_15
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
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "Path to the element that failed to deserialize")
    (description
      (beautify-description "Path to the element that failed to deserialize"))
    (license (list license:expat license:asl2.0))))

(define rust-h2_0_4_2
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
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-sink" ,rust-futures-sink_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_1_0_0)        
       ("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-slab" ,rust-slab_0_4_9)        
       ("rust-tokio-util" ,rust-tokio-util_0_7_10)        
       ("rust-tracing" ,rust-tracing_0_1_40))
      #:cargo-development-inputs
      (("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "None")
    (synopsis "An HTTP/2 client and server")
    (description
      (beautify-description "An HTTP/2 client and server"))
    (license (list license:expat))))

(define rust-portable-atomic_1_6_0
  (package
    (name "rust-portable-atomic")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "portable-atomic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h77x9qx7pns0d66vdrmdbmwpi7586h7ysnkdnhrn5mwi2cyyw3i"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Portable atomic types including support for 128-bit atomics, atomic float, etc.")
    (description
      (beautify-description "Portable atomic types including support for 128-bit atomics, atomic float, etc."))
    (license (list license:asl2.0 license:expat))))

(define rust-metrics-macros_0_7_1
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
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Macros for the metrics crate.")
    (description
      (beautify-description "Macros for the metrics crate."))
    (license (list license:expat))))

(define rust-lru_0_12_1
  (package
    (name "rust-lru")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lru" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1myillpwqfcins062g28jvj48cxw8818zcx08ydzsl6misxfx519"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hashbrown" ,rust-hashbrown_0_14_3))))
    (home-page "https://github.com/jeromefroe/lru-rs")
    (synopsis "A LRU cache implementation")
    (description
      (beautify-description "A LRU cache implementation"))
    (license (list license:expat))))

(define rust-strum_0_25_0
  (package
    (name "rust-strum")
    (version "0.25.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09g1q55ms8vax1z0mxlbva3vm8n2r1179kfvbccnkjcidzm58399"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-strum_macros" ,rust-strum_macros_0_25_3))))
    (home-page "https://github.com/Peternator7/strum")
    (synopsis "Helpful macros for working with enums and strings")
    (description
      (beautify-description "Helpful macros for working with enums and strings"))
    (license (list license:expat))))

(define rust-stability_0_1_1
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
      (("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "Rust API stability attributes for the rest of us.")
    (description
      (beautify-description "Rust API stability attributes for the rest of us."))
    (license (list license:expat))))

(define rust-indoc_2_0_4
  (package
    (name "rust-indoc")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indoc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n2z66b0y59rr6v4znpcijc2yd3yg6s40hpzv89yb140mvxnq60y"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Indented document literals")
    (description
      (beautify-description "Indented document literals"))
    (license (list license:expat license:asl2.0))))

(define rust-strum_macros_0_25_3
  (package
    (name "rust-strum_macros")
    (version "0.25.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strum_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "184y62g474zqb2f7n16x3ghvlyjbh50viw32p9w9l5lwmjlizp13"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-heck" ,rust-heck_0_4_1)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-rustversion" ,rust-rustversion-1)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://github.com/Peternator7/strum")
    (synopsis "Helpful macros for working with enums and strings")
    (description
      (beautify-description "Helpful macros for working with enums and strings"))
    (license (list license:expat))))

(define rust-termcolor_1_4_1
  (package
    (name "rust-termcolor")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi-util" ,rust-winapi-util_0_1_6))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis "A simple cross platform library for writing colored text to a terminal.")
    (description
      (beautify-description "A simple cross platform library for writing colored text to a terminal."))
    (license (list license:unlicense license:expat))))

(define rust-regex_1_10_3
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
      (("rust-aho-corasick" ,rust-aho-corasick_1_1_2)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-regex-automata" ,rust-regex-automata_0_4_5)        
       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license (list license:expat license:asl2.0))))

(define rust-is-terminal_0_4_10
  (package
    (name "rust-is-terminal")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "is-terminal" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m9la3f7cs77y85nkbcjsxkb7k861fc6bdhahyfidgh7gljh1b8b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hermit-abi" ,rust-hermit-abi_0_3_4)        
       ("rust-rustix" ,rust-rustix_0_38_30)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "None")
    (synopsis "Test whether a given stream is a terminal")
    (description
      (beautify-description "Test whether a given stream is a terminal"))
    (license (list license:expat))))

(define rust-winapi-util_0_1_6
  (package
    (name "rust-winapi-util")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15i5lm39wd44004i9d5qspry2cynkrpvwzghr6s2c3dsk28nz7pj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "A dumping ground for high level safe wrappers over winapi.")
    (description
      (beautify-description "A dumping ground for high level safe wrappers over winapi."))
    (license (list license:unlicense license:expat))))

(define rust-aho-corasick_1_1_2
  (package
    (name "rust-aho-corasick")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w510wnixvlgimkx1zjbvlxh6xps2vjgfqgwf5a6adlbjp5rv5mj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_7_1))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      (beautify-description "Fast multiple substring searching."))
    (license (list license:unlicense license:expat))))

(define rust-regex-automata_0_4_5
  (package
    (name "rust-regex-automata")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-automata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1karc80mx15z435rm1jg3sqylnc58nxi15gqypcd1inkzzpqgfav"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick_1_1_2)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "None")
    (synopsis "Automata construction and matching using regular expressions.")
    (description
      (beautify-description "Automata construction and matching using regular expressions."))
    (license (list license:expat license:asl2.0))))

(define rust-rusty_paseto_0_6_1
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
       ("rust-ed25519-dalek" ,rust-ed25519-dalek_2_1_0)        
       ("rust-hex" ,rust-hex-0.4)        
       ("rust-iso8601" ,rust-iso8601_0_4_2)        
       ("rust-ring" ,rust-ring_0_17_7)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-zeroize" ,rust-zeroize_1_7_0))))
    (home-page "None")
    (synopsis "A type-driven, ergonomic alternative to JWT for secure stateless PASETO tokens.")
    (description
      (beautify-description "A type-driven, ergonomic alternative to JWT for secure stateless PASETO tokens."))
    (license (list license:expat license:asl2.0))))

(define rust-crypto_secretbox_0_1_1
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
       ("rust-poly1305" ,rust-poly1305_0_8_0)        
       ("rust-salsa20" ,rust-salsa20-0.10)        
       ("rust-subtle" ,rust-subtle_2_5_0)        
       ("rust-zeroize" ,rust-zeroize_1_7_0))))
    (home-page "None")
    (synopsis "Pure Rust implementation of the XSalsa20Poly1305 (a.k.a. NaCl crypto_secretbox)\nauthenticated encryption cipher as well as the libsodium variant of\nXChaCha20Poly1305")
    (description
      (beautify-description "Pure Rust implementation of the XSalsa20Poly1305 (a.k.a. NaCl crypto_secretbox)\nauthenticated encryption cipher as well as the libsodium variant of\nXChaCha20Poly1305"))
    (license (list license:asl2.0 license:expat))))

(define rust-shellexpand_3_1_0
  (package
    (name "rust-shellexpand")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shellexpand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-dirs" ,rust-dirs-5))))
    (home-page "None")
    (synopsis "Shell-like expansions in strings")
    (description
      (beautify-description "Shell-like expansions in strings"))
    (license (list license:expat license:asl2.0))))

(define rust-futures_0_3_30
  (package
    (name "rust-futures")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c04g14bccmprwsvx2j9m2blhwrynq7vhl151lsvcv4gi0b6jp34"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-executor" ,rust-futures-executor_0_3_30)        
       ("rust-futures-io" ,rust-futures-io_0_3_30)        
       ("rust-futures-sink" ,rust-futures-sink_0_3_30)        
       ("rust-futures-task" ,rust-futures-task_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "An implementation of futures and streams featuring zero allocations,\ncomposability, and iterator-like interfaces.")
    (description
      (beautify-description "An implementation of futures and streams featuring zero allocations,\ncomposability, and iterator-like interfaces."))
    (license (list license:expat license:asl2.0))))

(define rust-serde_with_3_5_1
  (package
    (name "rust-serde_with")
    (version "3.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_with" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "030lwrqf57ib5vmzdiaw4rr3ndibzrw4pz9fn9srhj0an2vgvjgm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)        
       ("rust-chrono" ,rust-chrono_0_4_33)        
       ("rust-hex" ,rust-hex-0.4)        
       ("rust-indexmap" ,rust-indexmap-1)        
       ("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-serde_with_macros" ,rust-serde_with_macros_3_5_1)        
       ("rust-time" ,rust-time_0_3_31))))
    (home-page "None")
    (synopsis "Custom de/serialization functions for Rust\u0027s serde")
    (description
      (beautify-description "Custom de/serialization functions for Rust\u0027s serde"))
    (license (list license:expat license:asl2.0))))

(define rust-sql-builder_3_1_1
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
      (("rust-anyhow" ,rust-anyhow_1_0_79)        
       ("rust-thiserror" ,rust-thiserror_1_0_56))))
    (home-page "None")
    (synopsis "Simple SQL code generator.")
    (description
      (beautify-description "Simple SQL code generator."))
    (license (list license:expat))))

(define rust-rusty_paserk_0_3_0
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
       ("rust-chacha20" ,rust-chacha20_0_9_1)        
       ("rust-cipher" ,rust-cipher-0.4)        
       ("rust-curve25519-dalek" ,rust-curve25519-dalek_4_1_1)        
       ("rust-digest" ,rust-digest-0.10)        
       ("rust-ed25519-dalek" ,rust-ed25519-dalek_2_1_0)        
       ("rust-generic-array" ,rust-generic-array-0.14)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-rusty_paseto" ,rust-rusty_paseto_0_6_1)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-subtle" ,rust-subtle_2_5_0))))
    (home-page "None")
    (synopsis "Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key management")
    (description
      (beautify-description "Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key management"))
    (license (list license:expat))))

(define rust-parse_duration_2_1_1
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

(define rust-minspan_0_1_1
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

(define rust-serde_regex_1_1_0
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
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "https://github.com/tailhook/serde-regex")
    (synopsis "A serde wrapper that (de)serializes regex as strings")
    (description
      (beautify-description "A serde wrapper that (de)serializes regex as strings"))
    (license (list license:expat license:asl2.0))))

(define rust-iso8601_0_4_2
  (package
    (name "rust-iso8601")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iso8601" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15nfg6d4qlniw4gk7039s5y07lzgr1dp9snsw63lsxarnyz4zfg5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nom" ,rust-nom_7_1_3))))
    (home-page "None")
    (synopsis "Parsing ISO8601 dates using nom")
    (description
      (beautify-description "Parsing ISO8601 dates using nom"))
    (license (list license:expat))))

(define rust-ed25519-dalek_2_1_0
  (package
    (name "rust-ed25519-dalek")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ed25519-dalek" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h13qm789m9gdjl6jazss80hqi8ll37m0afwcnw23zcbqjp8wqhz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ed25519" ,rust-ed25519_2_2_3)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-sha2" ,rust-sha2-0.10)        
       ("rust-subtle" ,rust-subtle_2_5_0)        
       ("rust-zeroize" ,rust-zeroize_1_7_0))
      #:cargo-development-inputs
      (("rust-curve25519-dalek" ,rust-curve25519-dalek_4_1_1))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis "Fast and efficient ed25519 EdDSA key generations, signing, and verification in pure Rust.")
    (description
      (beautify-description "Fast and efficient ed25519 EdDSA key generations, signing, and verification in pure Rust."))
    (license (list license:bsd-3))))

(define rust-chacha20_0_8_2
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

(define rust-ed25519_2_2_3
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

(define rust-curve25519-dalek_4_1_1
  (package
    (name "rust-curve25519-dalek")
    (version "4.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "curve25519-dalek" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p7ns5917k6369gajrsbfj24llc5zfm635yh3abla7sb5rm8r6z8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-cpufeatures" ,rust-cpufeatures_0_2_12)        
       ("rust-curve25519-dalek-derive" ,rust-curve25519-dalek-derive_0_1_1)        
       ("rust-digest" ,rust-digest-0.10)        
       ("rust-fiat-crypto" ,rust-fiat-crypto_0_2_5)        
       ("rust-platforms" ,rust-platforms_3_3_0)        
       ("rust-rustc_version" ,rust-rustc_version_0_4_0)        
       ("rust-subtle" ,rust-subtle_2_5_0)        
       ("rust-zeroize" ,rust-zeroize_1_7_0))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis "A pure-Rust implementation of group operations on ristretto255 and Curve25519")
    (description
      (beautify-description "A pure-Rust implementation of group operations on ristretto255 and Curve25519"))
    (license (list license:bsd-3))))

(define rust-platforms_3_3_0
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

(define rust-curve25519-dalek-derive_0_1_1
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
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://github.com/dalek-cryptography/curve25519-dalek")
    (synopsis "curve25519-dalek Derives")
    (description
      (beautify-description "curve25519-dalek Derives"))
    (license (list license:expat license:asl2.0))))

(define rust-fiat-crypto_0_2_5
  (package
    (name "rust-fiat-crypto")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fiat-crypto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dxn0g50pv0ppal779vi7k40fr55pbhkyv4in7i13pgl4sn3wmr7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mit-plv/fiat-crypto")
    (synopsis "Fiat-crypto generated Rust")
    (description
      (beautify-description "Fiat-crypto generated Rust"))
    (license (list license:expat license:asl2.0))))

(define rust-rustc_version_0_4_0
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
      (("rust-semver" ,rust-semver_1_0_21))))
    (home-page "None")
    (synopsis "A library for querying the version of a installed rustc compiler")
    (description
      (beautify-description "A library for querying the version of a installed rustc compiler"))
    (license (list license:expat license:asl2.0))))

(define rust-aead_0_5_2
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

(define rust-poly1305_0_8_0
  (package
    (name "rust-poly1305")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "poly1305" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1grs77skh7d8vi61ji44i8gpzs3r9x7vay50i6cg8baxfa8bsnc1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cpufeatures" ,rust-cpufeatures_0_2_12)        
       ("rust-opaque-debug" ,rust-opaque-debug-0.3)        
       ("rust-universal-hash" ,rust-universal-hash_0_5_1))))
    (home-page "None")
    (synopsis "The Poly1305 universal hash function and message authentication code")
    (description
      (beautify-description "The Poly1305 universal hash function and message authentication code"))
    (license (list license:asl2.0 license:expat))))

(define rust-universal-hash_0_5_1
  (package
    (name "rust-universal-hash")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "universal-hash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sh79x677zkncasa95wz05b36134822w6qxmi1ck05fwi33f47gw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crypto-common" ,rust-crypto-common-0.1)        
       ("rust-subtle" ,rust-subtle_2_5_0))))
    (home-page "None")
    (synopsis "Traits which describe the functionality of universal hash functions (UHFs)")
    (description
      (beautify-description "Traits which describe the functionality of universal hash functions (UHFs)"))
    (license (list license:expat license:asl2.0))))

(define rust-chrono_0_4_33
  (package
    (name "rust-chrono")
    (version "0.4.33")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1szr180x4srkwvmzq5ahqnf3m7yjjllfmgp7k3hsrr556l76j4wz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-android-tzdata" ,rust-android-tzdata-0.1)        
       ("rust-iana-time-zone" ,rust-iana-time-zone_0_1_59)        
       ("rust-num-traits" ,rust-num-traits-0.2)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-windows-targets" ,rust-windows-targets_0_52_0))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description
      (beautify-description "Date and time library for Rust"))
    (license (list license:expat license:asl2.0))))

(define rust-serde_with_macros_3_5_1
  (package
    (name "rust-serde_with_macros")
    (version "3.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_with_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16427qm05i92qj3j7yzqizm3i8k51fqs1prqw6i00dmknhg3bzyv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling" ,rust-darling-0.20)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "proc-macro library for serde_with")
    (description
      (beautify-description "proc-macro library for serde_with"))
    (license (list license:expat license:asl2.0))))

(define rust-iana-time-zone_0_1_59
  (package
    (name "rust-iana-time-zone")
    (version "0.1.59")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iana-time-zone" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fbmmmrx837w9hy25lcgjm4qkzclmvmmfpp1ij946i5aw9ip79mn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-android_system_properties" ,rust-android_system_properties_0_1_5)        
       ("rust-core-foundation-sys" ,rust-core-foundation-sys_0_8_6)        
       ("rust-iana-time-zone-haiku" ,rust-iana-time-zone-haiku_0_1_2)        
       ("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90)        
       ("rust-windows-core" ,rust-windows-core_0_52_0))))
    (home-page "None")
    (synopsis "get the IANA time zone for the current system")
    (description
      (beautify-description "get the IANA time zone for the current system"))
    (license (list license:expat license:asl2.0))))

(define rust-android_system_properties_0_1_5
  (package
    (name "rust-android_system_properties")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "android_system_properties" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152))))
    (home-page "https://github.com/nical/android_system_properties")
    (synopsis "Minimal Android system properties wrapper")
    (description
      (beautify-description "Minimal Android system properties wrapper"))
    (license (list license:expat license:asl2.0))))

(define rust-iana-time-zone-haiku_0_1_2
  (package
    (name "rust-iana-time-zone-haiku")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iana-time-zone-haiku" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_83))))
    (home-page "None")
    (synopsis "iana-time-zone support crate for Haiku OS")
    (description
      (beautify-description "iana-time-zone support crate for Haiku OS"))
    (license (list license:expat license:asl2.0))))

(define rust-windows-core_0_52_0
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
      (("rust-windows-targets" ,rust-windows-targets_0_52_0))))
    (home-page "None")
    (synopsis "Rust for Windows")
    (description
      (beautify-description "Rust for Windows"))
    (license (list license:expat license:asl2.0))))

(define rust-anyhow_1_0_79
  (package
    (name "rust-anyhow")
    (version "1.0.79")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anyhow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ji5irqiwr8yprgqj8zvnli7zd7fz9kzaiddq44jnrl2l289h3h8"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description
      (beautify-description "Flexible concrete Error type built on std::error::Error"))
    (license (list license:expat license:asl2.0))))

(define rust-chacha20_0_9_1
  (package
    (name "rust-chacha20")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chacha20" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0678wipx6kghp71hpzhl2qvx80q7caz3vm8vsvd07b1fpms3yqf3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-cipher" ,rust-cipher-0.4)        
       ("rust-cpufeatures" ,rust-cpufeatures_0_2_12))))
    (home-page "None")
    (synopsis "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits\nfrom the RustCrypto `cipher` crate, with optional architecture-specific\nhardware acceleration (AVX2, SSE2). Additionally provides the ChaCha8, ChaCha12,\nXChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional\nrand_core-compatible RNGs based on those ciphers.")
    (description
      (beautify-description "The ChaCha20 stream cipher (RFC 8439) implemented in pure Rust using traits\nfrom the RustCrypto `cipher` crate, with optional architecture-specific\nhardware acceleration (AVX2, SSE2). Additionally provides the ChaCha8, ChaCha12,\nXChaCha20, XChaCha12 and XChaCha8 stream ciphers, and also optional\nrand_core-compatible RNGs based on those ciphers."))
    (license (list license:asl2.0 license:expat))))

(define rust-rtoolbox_0_0_2
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
      (("rust-libc" ,rust-libc_0_2_152)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "None")
    (synopsis "Utility functions for other crates, no backwards compatibility guarantees.")
    (description
      (beautify-description "Utility functions for other crates, no backwards compatibility guarantees."))
    (license (list license:asl2.0))))

(define rust-thread_local_1_1_7
  (package
    (name "rust-thread_local")
    (version "1.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread_local" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lp19jdgvp5m4l60cgxdnl00yw1hlqy8gcywg9bddwng9h36zp9z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-once_cell" ,rust-once_cell_1_19_0))))
    (home-page "None")
    (synopsis "Per-object thread-local storage")
    (description
      (beautify-description "Per-object thread-local storage"))
    (license (list license:expat license:asl2.0))))

(define rust-sharded-slab_0_1_7
  (package
    (name "rust-sharded-slab")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sharded-slab" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0))))
    (home-page "https://github.com/hawkw/sharded-slab")
    (synopsis "A lock-free concurrent slab.")
    (description
      (beautify-description "A lock-free concurrent slab."))
    (license (list license:expat))))

(define rust-clap_builder_4_4_18
  (package
    (name "rust-clap_builder")
    (version "4.4.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_builder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iyif47075caa4x1p3ygk18b07lb4xl4k48w4c061i2hxi0dzx2d"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-anstream" ,rust-anstream_0_6_11)        
       ("rust-anstyle" ,rust-anstyle_1_0_4)        
       ("rust-clap_lex" ,rust-clap_lex_0_6_0)        
       ("rust-strsim" ,rust-strsim-0.10))))
    (home-page "None")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (list license:expat license:asl2.0))))

(define rust-clap_derive_4_4_7
  (package
    (name "rust-clap_derive")
    (version "4.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hk4hcxl56qwqsf4hmf7c0gr19r9fbxk0ah2bgkr36pmmaph966g"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-heck" ,rust-heck_0_4_1)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Parse command line argument by defining a struct, derive crate.")
    (description
      (beautify-description "Parse command line argument by defining a struct, derive crate."))
    (license (list license:expat license:asl2.0))))

(define rust-anstyle_1_0_4
  (package
    (name "rust-anstyle")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anstyle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11yxw02b6parn29s757z96rgiqbn8qy0fk9a3p3bhczm85dhfybh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description
      (beautify-description "ANSI text styling"))
    (license (list license:expat license:asl2.0))))

(define rust-clap_lex_0_6_0
  (package
    (name "rust-clap_lex")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_lex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l8bragdvim7mva9flvd159dskn2bdkpl0jqrr41wnjfn8pcfbvh"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Minimal, flexible command line parser")
    (description
      (beautify-description "Minimal, flexible command line parser"))
    (license (list license:expat license:asl2.0))))

(define rust-anstream_0_6_11
  (package
    (name "rust-anstream")
    (version "0.6.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anstream" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19dndamalavhjwp4i74k8hdijcixb7gsfa6ycwyc1r8xn6y1wbkf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-anstyle" ,rust-anstyle_1_0_4)        
       ("rust-anstyle-parse" ,rust-anstyle-parse_0_2_3)        
       ("rust-anstyle-query" ,rust-anstyle-query_1_0_2)        
       ("rust-anstyle-wincon" ,rust-anstyle-wincon_3_0_2)        
       ("rust-colorchoice" ,rust-colorchoice-1)        
       ("rust-utf8parse" ,rust-utf8parse-0.2))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "A simple cross platform library for writing colored text to a terminal.")
    (description
      (beautify-description "A simple cross platform library for writing colored text to a terminal."))
    (license (list license:expat license:asl2.0))))

(define rust-anstyle-wincon_3_0_2
  (package
    (name "rust-anstyle-wincon")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anstyle-wincon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19v0fv400bmp4niqpzxnhg83vz12mmqv7l2l8vi80qcdxj0lpm8w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-anstyle" ,rust-anstyle_1_0_4)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "Styling legacy Windows terminals")
    (description
      (beautify-description "Styling legacy Windows terminals"))
    (license (list license:expat license:asl2.0))))

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
      (("rust-windows-sys" ,rust-windows-sys_0_52_0))))
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

(define rust-errno_0_3_8
  (package
    (name "rust-errno")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "errno" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ia28ylfsp36i27g1qih875cyyy4by2grf80ki8vhgh6vinf8n52"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "None")
    (synopsis "Cross-platform interface to the `errno` variable.")
    (description
      (beautify-description "Cross-platform interface to the `errno` variable."))
    (license (list license:expat license:asl2.0))))

(define rust-number_prefix_0_4_0
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
      (("rust-cfg-if" ,rust-cfg-if-1))))
    (home-page "None")
    (synopsis "A partial replacement for std::time::Instant that works on WASM too.")
    (description
      (beautify-description "A partial replacement for std::time::Instant that works on WASM too."))
    (license (list license:bsd-3))))

(define rust-console_0_15_8
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
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-unicode-width" ,rust-unicode-width-0.1)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "https://github.com/console-rs/console")
    (synopsis "A terminal and console abstraction for Rust")
    (description
      (beautify-description "A terminal and console abstraction for Rust"))
    (license (list license:expat))))

(define rust-encode_unicode_0_3_6
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

(define rust-tokio-macros_2_2_0
  (package
    (name "rust-tokio-macros")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fwjy4vdx1h9pi4g2nml72wi0fr27b5m954p13ji9anyy8l1x2jv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://tokio.rs")
    (synopsis "Tokio\u0027s proc macros.")
    (description
      (beautify-description "Tokio\u0027s proc macros."))
    (license (list license:expat))))

(define rust-backtrace_0_3_69
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
       ("rust-cc" ,rust-cc_1_0_83)        
       ("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-miniz_oxide" ,rust-miniz_oxide_0_7_1)        
       ("rust-object" ,rust-object_0_32_2)        
       ("rust-rustc-demangle" ,rust-rustc-demangle_0_1_23))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis "A library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (description
      (beautify-description "A library to acquire a stack trace (backtrace) at runtime in a Rust program."))
    (license (list license:expat license:asl2.0))))

(define rust-signal-hook-registry_1_4_1
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
      (("rust-libc" ,rust-libc_0_2_152))))
    (home-page "None")
    (synopsis "Backend crate for signal-hook")
    (description
      (beautify-description "Backend crate for signal-hook"))
    (license (list license:asl2.0 license:expat))))

(define rust-mio_0_8_10
  (package
    (name "rust-mio")
    (version "0.8.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02gyaxvaia9zzi4drrw59k9s0j6pa5d1y2kv7iplwjipdqlhngcg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_152)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-wasi" ,rust-wasi-0.11)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking I/O.")
    (description
      (beautify-description "Lightweight non-blocking I/O."))
    (license (list license:expat))))

(define rust-object_0_32_2
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
      (("rust-memchr" ,rust-memchr_2_7_1))))
    (home-page "None")
    (synopsis "A unified interface for reading and writing object file formats.")
    (description
      (beautify-description "A unified interface for reading and writing object file formats."))
    (license (list license:asl2.0 license:expat))))

(define rust-rustc-demangle_0_1_23
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

(define rust-miniz_oxide_0_7_1
  (package
    (name "rust-miniz_oxide")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ivl3rbbdm53bzscrd01g60l46lz5krl270487d8lhjvwl5hx0g7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-adler" ,rust-adler-1))))
    (home-page "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (description
      (beautify-description "DEFLATE compression and decompression library rewritten in Rust based on miniz"))
    (license (list license:expat license:zlib license:asl2.0))))

(define rust-addr2line_0_21_0
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

(define rust-gimli_0_28_1
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

(define rust-atomic_0_5_3
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

(define rust-time-macros_0_2_16
  (package
    (name "rust-time-macros")
    (version "0.2.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gx4ngf5g7ydqa8lf7kh9sy72rd4dhvpi31y1jvswi0288rpw696"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-time-core" ,rust-time-core_0_1_2))))
    (home-page "None")
    (synopsis "Procedural macros for the time crate.\n    This crate is an implementation detail and should not be relied upon directly.")
    (description
      (beautify-description "Procedural macros for the time crate.\n    This crate is an implementation detail and should not be relied upon directly."))
    (license (list license:expat license:asl2.0))))

(define rust-time-core_0_1_2
  (package
    (name "rust-time-core")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wx3qizcihw6z151hywfzzyd1y5dl804ydyxci6qm07vbakpr4pg"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "This crate is an implementation detail and should not be relied upon directly.")
    (description
      (beautify-description "This crate is an implementation detail and should not be relied upon directly."))
    (license (list license:expat license:asl2.0))))

(define rust-deranged_0_3_11
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
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "Ranged integers")
    (description
      (beautify-description "Ranged integers"))
    (license (list license:expat license:asl2.0))))

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
      (("rust-libc" ,rust-libc_0_2_152))))
    (home-page "None")
    (synopsis "A minimal library that determines the number of running threads for the current process.")
    (description
      (beautify-description "A minimal library that determines the number of running threads for the current process."))
    (license (list license:expat license:asl2.0))))

(define rust-futures-macro_0_3_30
  (package
    (name "rust-futures-macro")
    (version "0.3.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b49qh9d402y8nka4q6wvvj0c88qq91wbr192mdn5h54nzs0qxc7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.")
    (description
      (beautify-description "The futures-rs procedural macro implementations."))
    (license (list license:expat license:asl2.0))))

(define rust-tracing-log_0_2_0
  (package
    (name "rust-tracing-log")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log-0.4)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-tracing-core" ,rust-tracing-core_0_1_32))))
    (home-page "https://tokio.rs")
    (synopsis "Provides compatibility between tracing and the log crate.")
    (description
      (beautify-description "Provides compatibility between tracing and the log crate."))
    (license (list license:expat))))

(define rust-nu-ansi-term_0_49_0
  (package
    (name "rust-nu-ansi-term")
    (version "0.49.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nu-ansi-term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s2svfnircd9jp06wk55qcbb9v5cadkfcjfg99vm21qdjg0x6wy0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "None")
    (synopsis "Library for ANSI terminal colors and styles (bold, underline)")
    (description
      (beautify-description "Library for ANSI terminal colors and styles (bold, underline)"))
    (license (list license:expat))))
