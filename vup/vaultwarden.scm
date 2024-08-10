(define-module (vup vaultwarden)
 #:use-module (guix build-system cargo)
 #:use-module (guix build-system copy)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (gnu packages crates-crypto)
 #:use-module (gnu packages crates-windows)
 #:use-module (gnu packages crates-io)
 #:use-module (gnu packages crates-tls)
 #:use-module (gnu packages crates-web)
 #:use-module (gnu packages perl)
 #:use-module (gnu packages c)
 #:use-module (gnu packages tls)
 #:use-module (gnu packages pkg-config)
 #:use-module (guix download)
 #:use-module (guix git-download)
 #:use-module ((guix import utils) #:select (beautify-description)))

(define-public web-vault
  (let* ((version "v2024.1.2b"))
    (package
      (name "web-vault")
      (version version)
      (source
        (origin
          (method url-fetch)
          (uri (string-append "https://github.com/dani-garcia/bw_web_builds/releases/download/" version "/bw_web_" version ".tar.gz"))
          (sha256
            (base32
              "0xmvrc6r3m5153vkd3x5yqqksp0scpxmb7wmmkzzp7w9f6gkz9n9"))))
      (build-system copy-build-system)
      (home-page "https://github.com/dani-garcia/bw_web_builds")
      (synopsis "Web vault builds for vaultwarden")
      (description "Web vault builds for vaultwarden")
      (license license:gpl3))))

(define-public rust-mimalloc-0.1
  (package
    (name "rust-mimalloc")
    (version "0.1.39")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mimalloc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "176w9gf5qxs07kd2q39f0k25rzmp4kyx5r13wc8sk052bqmr40gs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libmimalloc-sys" ,rust-libmimalloc-sys-0.1))))
    (inputs (list mimalloc))
    (home-page "https://crates.io/crates/mimalloc")
    (synopsis "Performance and security oriented drop-in allocator")
    (description "This package provides a performance and security oriented
drop-in allocator.")
    (license license:expat)))

(define-public vaultwarden
  (package
    (name "rust-vaultwarden")
    (version "1.30.3")
    (source
      (origin
      (method git-fetch)
      (uri (git-reference
       (url "https://github.com/dani-garcia/vaultwarden")
       (commit version)))
      (file-name (git-file-name name version))
        (sha256
          (base32
            "1j0g93zq93k7nwfk58pwh5jl9b3z6nimlj31rmrfy50kz5dj0h5x"))))
    (build-system cargo-build-system)
    (native-inputs (list perl pkg-config))
    (inputs (list openssl))
    (arguments
    `(#:tests? #f
      #:features '("sqlite") ;  "vendored_openssl"
      #:cargo-inputs
      (("rust-argon2" ,rust-argon2_0_5_3)        
       ("rust-bigdecimal" ,rust-bigdecimal_0_4_2)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-cached" ,rust-cached_0_48_1)        
       ("rust-chrono" ,rust-chrono_0_4_33)        
       ("rust-chrono-tz" ,rust-chrono-tz_0_8_5)        
       ("rust-cookie" ,rust-cookie-0.16)        
       ("rust-cookie_store" ,rust-cookie_store_0_19_1)        
       ("rust-dashmap" ,rust-dashmap-5)        
       ("rust-data-encoding" ,rust-data-encoding_2_5_0)        
       ("rust-data-url" ,rust-data-url_0_3_1)        
       ("rust-diesel" ,rust-diesel_2_1_4)        
       ("rust-diesel_logger" ,rust-diesel_logger_0_3_0)        
       ("rust-diesel_migrations" ,rust-diesel_migrations_2_1_0)        
       ("rust-dotenvy" ,rust-dotenvy-0.15)        
       ("rust-email_address" ,rust-email_address_0_2_4)        
       ("rust-fern" ,rust-fern-0.6)        
       ("rust-futures" ,rust-futures_0_3_30)        
       ("rust-governor" ,rust-governor_0_6_0)        
       ("rust-handlebars" ,rust-handlebars_5_1_1)        
       ("rust-html5gum" ,rust-html5gum_0_5_7)        
       ("rust-job_scheduler_ng" ,rust-job_scheduler_ng_2_0_4)        
       ("rust-jsonwebtoken" ,rust-jsonwebtoken_9_2_0)        
       ("rust-lettre" ,rust-lettre_0_11_3)        
       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys_0_27_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-mimalloc" ,rust-mimalloc-0.1)        
       ("rust-num-derive" ,rust-num-derive_0_4_1)        
       ("rust-num-traits" ,rust-num-traits-0.2)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-openssl" ,rust-openssl-0.10)
       ("rust-paste" ,rust-paste-1)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-pico-args" ,rust-pico-args_0_5_0)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-regex" ,rust-regex_1_10_3)        
       ("rust-reqwest" ,rust-reqwest_0_11_23)        
       ("rust-ring" ,rust-ring_0_17_7)        
       ("rust-rmpv" ,rust-rmpv_1_0_1)        
       ("rust-rocket" ,rust-rocket_0_5_0)        
       ("rust-rocket_ws" ,rust-rocket_ws_0_1_0)        
       ("rust-rpassword" ,rust-rpassword_7_3_1)        
       ("rust-semver" ,rust-semver_1_0_21)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-syslog" ,rust-syslog-6)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite_0_20_1)        
       ("rust-totp-lite" ,rust-totp-lite_2_0_1)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-url" ,rust-url_2_5_0)        
       ("rust-uuid" ,rust-uuid_1_7_0)        
       ("rust-webauthn-rs" ,rust-webauthn-rs_0_3_2)        
       ("rust-which" ,rust-which_6_0_0)        
       ("rust-yubico" ,rust-yubico_0_11_0))))
    (home-page "None")
    (synopsis "Alternative implementation of the Bitwarden server API, compatible with the official clients")
    (description
      (beautify-description "Alternative implementation of the Bitwarden server API, compatible with the official clients"))
    (license #f)))

(define rust-html5gum_0_5_7
  (package
    (name "rust-html5gum")
    (version "0.5.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "html5gum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "057zrfis7p567s8d41z04f82idipzdcv125ygc8vln50f5hmakjc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-jetscii" ,rust-jetscii_0_5_3))))
    (home-page "None")
    (synopsis "A WHATWG-compliant HTML5 tokenizer and tag soup parser.")
    (description
      (beautify-description "A WHATWG-compliant HTML5 tokenizer and tag soup parser."))
    (license (list license:expat))))

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

(define rust-which_6_0_0
  (package
    (name "rust-which")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "070hbvl3hjxywychmz7nj5gbsprdm38rir3kqnm48zzp1g0y19bz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-either" ,rust-either_1_9_0)        
       ("rust-home" ,rust-home_0_5_9)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-rustix" ,rust-rustix_0_38_30)        
       ("rust-windows-sys" ,rust-windows-sys_0_52_0))))
    (home-page "None")
    (synopsis "A Rust equivalent of Unix command \"which\". Locate installed executable in cross platforms.")
    (description
      (beautify-description "A Rust equivalent of Unix command \"which\". Locate installed executable in cross platforms."))
    (license (list license:expat))))

(define rust-bigdecimal_0_4_2
  (package
    (name "rust-bigdecimal")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bigdecimal" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jcbzgna6292vgq0slw5iah929wl0xbps22zr63bp99y8az1jrn0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg-1)        
       ("rust-libm" ,rust-libm_0_2_8)        
       ("rust-num-bigint" ,rust-num-bigint-0.4)        
       ("rust-num-integer" ,rust-num-integer-0.1)        
       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/akubera/bigdecimal-rs")
    (synopsis "Arbitrary precision decimal numbers")
    (description
      (beautify-description "Arbitrary precision decimal numbers"))
    (license (list license:expat license:asl2.0))))

(define rust-rocket_ws_0_1_0
  (package
    (name "rust-rocket_ws")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_ws" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n85p5f5g2241v9fh3l3bilfhlw1xm32ah9ccgfp6b9fwwzpnrxn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rocket" ,rust-rocket_0_5_0)        
       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite_0_20_1))))
    (home-page "https://rocket.rs")
    (synopsis "WebSocket support for Rocket.")
    (description
      (beautify-description "WebSocket support for Rocket."))
    (license (list license:expat license:asl2.0))))

(define rust-cookie_store_0_19_1
  (package
    (name "rust-cookie_store")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19vy19rvp3a68sgxkrv6bxxlrr2ggqp0176yqb3zhmi0g4sqz8fm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cookie" ,rust-cookie-0.16)        
       ("rust-idna" ,rust-idna-0.3)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-publicsuffix" ,rust-publicsuffix-2)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_derive" ,rust-serde_derive_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-url" ,rust-url_2_5_0))))
    (home-page "None")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description
      (beautify-description "Implementation of Cookie storage and retrieval"))
    (license (list license:expat license:asl2.0))))

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

(define rust-cached_0_48_1
  (package
    (name "rust-cached")
    (version "0.48.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pshxkggxh8r32mqg04ahz26gvf2nfmlhk41js5pfn6z83jsqprm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ahash" ,rust-ahash_0_8_7)        
       ("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-cached_proc_macro" ,rust-cached_proc_macro_0_19_1)        
       ("rust-cached_proc_macro_types" ,rust-cached_proc_macro_types_0_1_1)        
       ("rust-futures" ,rust-futures_0_3_30)        
       ("rust-hashbrown" ,rust-hashbrown_0_14_3)        
       ("rust-instant" ,rust-instant_0_1_12)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license (list license:expat))))

(define rust-rocket_0_5_0
  (package
    (name "rust-rocket")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l4i93dai7pyzlkvdjkqg2g7ni1r6749cwx4nrrhsrr6rdybaywy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-stream" ,rust-async-stream_0_3_5)        
       ("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-atomic" ,rust-atomic_0_5_3)        
       ("rust-binascii" ,rust-binascii_0_1_4)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-either" ,rust-either_1_9_0)        
       ("rust-figment" ,rust-figment_0_10_14)        
       ("rust-futures" ,rust-futures_0_3_30)        
       ("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-multer" ,rust-multer_2_1_0)        
       ("rust-num_cpus" ,rust-num_cpus_1_16_0)        
       ("rust-parking_lot" ,rust-parking_lot_0_12_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-ref-cast" ,rust-ref-cast_1_0_22)        
       ("rust-rocket_codegen" ,rust-rocket_codegen_0_5_0)        
       ("rust-rocket_http" ,rust-rocket_http_0_5_0)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-state" ,rust-state_0_6_0)        
       ("rust-tempfile" ,rust-tempfile_3_9_0)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-stream" ,rust-tokio-stream-0.1)        
       ("rust-tokio-util" ,rust-tokio-util_0_7_10)        
       ("rust-ubyte" ,rust-ubyte_0_10_4)        
       ("rust-version_check" ,rust-version_check_0_9_4)        
       ("rust-yansi" ,rust-yansi_1_0_0-rc_1))))
    (home-page "https://rocket.rs")
    (synopsis "Web framework with a focus on usability, security, extensibility, and speed.")
    (description
      (beautify-description "Web framework with a focus on usability, security, extensibility, and speed."))
    (license (list license:expat license:asl2.0))))

(define rust-data-url_0_3_1
  (package
    (name "rust-data-url")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "data-url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ahclz72myi350cs1xcsxdh1v0iljpfj4ghcy2fy46mpfhf7laaw"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Processing of data: URL according to WHATWG\u2019s Fetch Standard")
    (description
      (beautify-description "Processing of data: URL according to WHATWG\u2019s Fetch Standard"))
    (license (list license:expat license:asl2.0))))

(define rust-chrono-tz_0_8_5
  (package
    (name "rust-chrono-tz")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono-tz" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dv0yqhr60wrhm4sdlralqw1jf5plcxc91q6v93hvamzk6gbgmwi"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-chrono" ,rust-chrono_0_4_33)        
       ("rust-chrono-tz-build" ,rust-chrono-tz-build_0_2_1)        
       ("rust-phf" ,rust-phf-0.11))))
    (home-page "None")
    (synopsis "TimeZone implementations for chrono from the IANA database")
    (description
      (beautify-description "TimeZone implementations for chrono from the IANA database"))
    (license (list license:expat license:asl2.0))))

(define rust-lettre_0_11_3
  (package
    (name "rust-lettre")
    (version "0.11.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lettre" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "199n82kz8ah03y9fmm9j3v62x8fp41fm1hraw590hsvbjllgdapm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-std" ,rust-async-std-1)        
       ("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-base64" ,rust-base64_0_21_7)        
       ("rust-chumsky" ,rust-chumsky_0_9_3)        
       ("rust-email-encoding" ,rust-email-encoding_0_2_0)        
       ("rust-email_address" ,rust-email_address_0_2_4)        
       ("rust-fastrand" ,rust-fastrand-2)        
       ("rust-futures-io" ,rust-futures-io_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-hostname" ,rust-hostname-0.3)        
       ("rust-httpdate" ,rust-httpdate_1_0_3)        
       ("rust-idna" ,rust-idna_0_5_0)        
       ("rust-mime" ,rust-mime_0_3_17)        
       ("rust-native-tls" ,rust-native-tls-0.2)        
       ("rust-nom" ,rust-nom_7_1_3)        
       ("rust-quoted_printable" ,rust-quoted_printable_0_5_0)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-socket2" ,rust-socket2_0_5_5)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-url" ,rust-url_2_5_0))))
    (home-page "https://lettre.rs")
    (synopsis "Email client")
    (description
      (beautify-description "Email client"))
    (license (list license:expat))))

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

(define rust-diesel_migrations_2_1_0
  (package
    (name "rust-diesel_migrations")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_migrations" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b0ld4azk73rg2axwq7a4wnpwba3085f43jp3cw62n8c2bqb6dk0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-diesel" ,rust-diesel_2_1_4)        
       ("rust-migrations_internals" ,rust-migrations_internals_2_1_0)        
       ("rust-migrations_macros" ,rust-migrations_macros_2_1_0))))
    (home-page "https://diesel.rs")
    (synopsis "Migration management for diesel")
    (description
      (beautify-description "Migration management for diesel"))
    (license (list license:expat license:asl2.0))))

(define rust-handlebars_5_1_1
  (package
    (name "rust-handlebars")
    (version "5.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "handlebars" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yd3amchdpv3l8x1jxnnx2gyh4jzwfs12h60kfzv8zz6j72nccf7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log-0.4)        
       ("rust-pest" ,rust-pest_2_7_6)        
       ("rust-pest_derive" ,rust-pest_derive_2_7_6)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/sunng87/handlebars-rust")
    (synopsis "Handlebars templating implemented in Rust.")
    (description
      (beautify-description "Handlebars templating implemented in Rust."))
    (license (list license:expat))))

(define rust-data-encoding_2_5_0
  (package
    (name "rust-data-encoding")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "data-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rcbnwfmfxhlshzbn3r7srm3azqha3mn33yxyqxkzz2wpqcjm5ky"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Efficient and customizable data-encoding functions like base64, base32, and hex")
    (description
      (beautify-description "Efficient and customizable data-encoding functions like base64, base32, and hex"))
    (license (list license:expat))))

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
      (("rust-getrandom" ,rust-getrandom_0_2_12))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
      (beautify-description "A library to generate and parse UUIDs."))
    (license (list license:asl2.0 license:expat))))

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
      (("rust-base64" ,rust-base64-0.13)        
       ("rust-form_urlencoded" ,rust-form_urlencoded_1_2_1)        
       ("rust-futures" ,rust-futures_0_3_30)        
       ("rust-hmac" ,rust-hmac-0.12)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-reqwest" ,rust-reqwest_0_11_23)        
       ("rust-sha1" ,rust-sha1_0_10_6)        
       ("rust-threadpool" ,rust-threadpool-1))))
    (home-page "None")
    (synopsis "Yubikey client API library")
    (description
      (beautify-description "Yubikey client API library"))
    (license (list license:expat license:asl2.0))))

(define rust-email_address_0_2_4
  (package
    (name "rust-email_address")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "email_address" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04ni69f0qfydw5knkj9x5sah864h9lcy5hxxphaxn2dw7vc3n5g2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "A Rust crate providing an implementation of an RFC-compliant `EmailAddress` newtype.")
    (description
      (beautify-description "A Rust crate providing an implementation of an RFC-compliant `EmailAddress` newtype."))
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

(define rust-job_scheduler_ng_2_0_4
  (package
    (name "rust-job_scheduler_ng")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "job_scheduler_ng" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bnambdhbgx647hpa15kl1pwfrfjaxq8n8b68qzybfqkam2dzfqh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-chrono" ,rust-chrono_0_4_33)        
       ("rust-cron" ,rust-cron_0_12_0)        
       ("rust-uuid" ,rust-uuid_1_7_0))))
    (home-page "None")
    (synopsis "A simple cron-like job scheduling library for Rust (Updated since 2022).")
    (description
      (beautify-description "A simple cron-like job scheduling library for Rust (Updated since 2022)."))
    (license (list license:expat license:asl2.0))))

(define rust-jsonwebtoken_9_2_0
  (package
    (name "rust-jsonwebtoken")
    (version "9.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jsonwebtoken" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x530g2kd41qyv4nd35m0qixcvq3pb36v6qqbwbmq1awgi5a0zjw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)        
       ("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-pem" ,rust-pem_3_0_3)        
       ("rust-ring" ,rust-ring_0_17_7)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-simple_asn1" ,rust-simple_asn1_0_6_2))))
    (home-page "https://github.com/Keats/jsonwebtoken")
    (synopsis "Create and decode JWTs in a strongly typed way.")
    (description
      (beautify-description "Create and decode JWTs in a strongly typed way."))
    (license (list license:expat))))

(define rust-governor_0_6_0
  (package
    (name "rust-governor")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "governor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d2fs5z7dhfig3fsmka9ns0bs2wm4ak1z430f0m3xwigczjkj4l2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-dashmap" ,rust-dashmap-5)        
       ("rust-futures" ,rust-futures_0_3_30)        
       ("rust-futures-timer" ,rust-futures-timer-3)        
       ("rust-no-std-compat" ,rust-no-std-compat-0.4)        
       ("rust-nonzero_ext" ,rust-nonzero_ext_0_3_0)        
       ("rust-parking_lot" ,rust-parking_lot_0_12_1)        
       ("rust-quanta" ,rust-quanta_0_11_1)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-smallvec" ,rust-smallvec_1_13_1))))
    (home-page "https://github.com/boinkor-net/governor")
    (synopsis "A rate-limiting implementation in Rust")
    (description
      (beautify-description "A rate-limiting implementation in Rust"))
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

(define rust-pico-args_0_5_0
  (package
    (name "rust-pico-args")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pico-args" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05d30pvxd6zlnkg2i3ilr5a70v3f3z2in18m67z25vinmykngqav"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An ultra simple CLI arguments parser.")
    (description
      (beautify-description "An ultra simple CLI arguments parser."))
    (license (list license:expat))))

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

(define rust-rmpv_1_0_1
  (package
    (name "rust-rmpv")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rmpv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v01az3ahglmag94d75qgdygfcgwj9bh5921rvn49d52lha043if"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-traits" ,rust-num-traits-0.2)        
       ("rust-rmp" ,rust-rmp-0.8))))
    (home-page "None")
    (synopsis "Value variant for RMP")
    (description
      (beautify-description "Value variant for RMP"))
    (license (list license:expat))))

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

(define rust-totp-lite_2_0_1
  (package
    (name "rust-totp-lite")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "totp-lite" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hvnpv7nl79jp96w6g2j7l6xskl5qlx3h0qqf9zry68pvcs33r7q"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-digest" ,rust-digest-0.10)        
       ("rust-hmac" ,rust-hmac-0.12)        
       ("rust-sha1" ,rust-sha1_0_10_6)        
       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/fosskers/totp-lite")
    (synopsis "A simple, correct TOTP library.")
    (description
      (beautify-description "A simple, correct TOTP library."))
    (license (list license:expat))))

(define rust-diesel_2_1_4
  (package
    (name "rust-diesel")
    (version "2.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n1ihxd5zfqg94zy01zf85ml716fqmsivj7pxiw8qzzi8bwgrik2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bigdecimal" ,rust-bigdecimal_0_4_2)        
       ("rust-bitflags" ,rust-bitflags_2_4_2)        
       ("rust-byteorder" ,rust-byteorder-1)        
       ("rust-chrono" ,rust-chrono_0_4_33)        
       ("rust-diesel_derives" ,rust-diesel_derives_2_1_2)        
       ("rust-itoa" ,rust-itoa_1_0_10)        
       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys_0_27_0)        
       ("rust-mysqlclient-sys" ,rust-mysqlclient-sys-0.2)        
       ("rust-num-bigint" ,rust-num-bigint-0.4)        
       ("rust-num-integer" ,rust-num-integer-0.1)        
       ("rust-num-traits" ,rust-num-traits-0.2)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-pq-sys" ,rust-pq-sys_0_4_8)        
       ("rust-r2d2" ,rust-r2d2-0.8)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-url" ,rust-url_2_5_0))))
    (home-page "https://diesel.rs")
    (synopsis "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL")
    (description
      (beautify-description "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL"))
    (license (list license:expat license:asl2.0))))

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
      (("rust-base64" ,rust-base64-0.13)        
       ("rust-nom" ,rust-nom_7_1_3)        
       ("rust-openssl" ,rust-openssl-0.10)
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_cbor" ,rust-serde_cbor_0_11_2)        
       ("rust-serde_derive" ,rust-serde_derive_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-url" ,rust-url_2_5_0))))
    (home-page "None")
    (synopsis "Webauthn Framework for Rust Web Servers")
    (description
      (beautify-description "Webauthn Framework for Rust Web Servers"))
    (license (list license:mpl2.0))))

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

(define rust-diesel_logger_0_3_0
  (package
    (name "rust-diesel_logger")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bsz6x2y68knd4rjf8w59kfx7lkgswv5zcqzn74rs4hpfm80n093"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-diesel" ,rust-diesel_2_1_4)        
       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/shssoichiro/diesel-logger")
    (synopsis "Times and logs queries executed by diesel")
    (description
      (beautify-description "Times and logs queries executed by diesel"))
    (license (list license:expat))))

(define rust-num-derive_0_4_1
  (package
    (name "rust-num-derive")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04j7mndk9p6nzl9j6zrf49r2cq3250h4ldcx40jv3y48mxwpddyg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://github.com/rust-num/num-derive")
    (synopsis "Numeric syntax extensions")
    (description
      (beautify-description "Numeric syntax extensions"))
    (license (list license:expat license:asl2.0))))

(define rust-tokio-tungstenite_0_20_1
  (package
    (name "rust-tokio-tungstenite")
    (version "0.20.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-tungstenite" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v1v24l27hxi5hlchs7hfd5rgzi167x0ygbw220nvq0w5b5msb91"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tungstenite" ,rust-tungstenite_0_20_1))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
      (beautify-description "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation"))
    (license (list license:expat))))

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
      (("rust-async-compression" ,rust-async-compression_0_4_6)        
       ("rust-base64" ,rust-base64_0_21_7)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-cookie" ,rust-cookie-0.16)        
       ("rust-cookie_store" ,rust-cookie_store_0_16_2)        
       ("rust-encoding_rs" ,rust-encoding_rs_0_8_33)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-h2" ,rust-h2_0_3_24)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-http-body" ,rust-http-body_0_4_6)        
       ("rust-hyper-tls" ,rust-hyper-tls-0.5)        
       ("rust-ipnet" ,rust-ipnet_2_9_0)        
       ("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-mime" ,rust-mime_0_3_17)        
       ("rust-native-tls" ,rust-native-tls-0.2)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-serde_urlencoded" ,rust-serde_urlencoded_0_7_1)        
       ("rust-system-configuration" ,rust-system-configuration_0_5_1)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)        
       ("rust-tokio-socks" ,rust-tokio-socks-0.5)        
       ("rust-tokio-util" ,rust-tokio-util_0_7_10)        
       ("rust-tower-service" ,rust-tower-service-0.3)        
       ("rust-trust-dns-resolver" ,rust-trust-dns-resolver_0_23_2)        
       ("rust-url" ,rust-url_2_5_0)        
       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures_0_4_40)        
       ("rust-wasm-streams" ,rust-wasm-streams_0_3_0)        
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

;; (define rust-openssl_0_10_63
;;   (package
;;     (name "rust-openssl")
;;     (version "0.10.63")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (crate-uri "openssl" version))
;;         (file-name
;;           (string-append name "-" version ".tar.gz"))
;;         (sha256
;;           (base32
;;             "1j03p28gh2idm6pfb12il1360w0qqn7gwz019n6mcabsv2fxdj8m"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;     `(#:cargo-inputs
;;       (("rust-bitflags" ,rust-bitflags_2_4_2)
;;        ("rust-cfg-if" ,rust-cfg-if-1)
;;        ("rust-foreign-types" ,rust-foreign-types-0.3)
;;        ("rust-libc" ,rust-libc_0_2_152)
;;        ("rust-once_cell" ,rust-once_cell_1_19_0)
;;        ("rust-openssl-macros" ,rust-openssl-macros_0_1_1)
;;        ("rust-openssl-sys" ,rust-openssl-sys_0_9_99))))
;;     (home-page "None")
;;     (synopsis "OpenSSL bindings")
;;     (description
;;       (beautify-description "OpenSSL bindings"))
;;     (license (list license:asl2.0))))

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
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "URL library for Rust, based on the WHATWG URL Standard")
    (description
      (beautify-description "URL library for Rust, based on the WHATWG URL Standard"))
    (license (list license:expat license:asl2.0))))

(define rust-jetscii_0_5_3
  (package
    (name "rust-jetscii")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jetscii" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pppbawc1v6lshz6zi3d4bkz7xbalph9sd78a5299jd94kz45wa7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A tiny library to efficiently search strings and byte slices for sets of ASCII characters or bytes.")
    (description
      (beautify-description "A tiny library to efficiently search strings and byte slices for sets of ASCII characters or bytes."))
    (license (list license:expat license:asl2.0))))

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
    (home-page "None")
    (synopsis "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      (beautify-description "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases."))
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
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
      (beautify-description "A macro to generate structures which behave like bitflags."))
    (license (list license:expat license:asl2.0))))

(define rust-libm_0_2_8
  (package
    (name "rust-libm")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n4hk1rs8pzw8hdfmwn96c4568s93kfxqgcqswr7sajd2diaihjf"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "libm in pure Rust")
    (description
      (beautify-description "libm in pure Rust"))
    (license (list license:expat license:asl2.0))))

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
       ("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-libc" ,rust-libc_0_2_152)        
       ("rust-wasi" ,rust-wasi-0.11)        
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90))))
    (home-page "None")
    (synopsis "A small cross-platform library for retrieving random data from system source")
    (description
      (beautify-description "A small cross-platform library for retrieving random data from system source"))
    (license (list license:expat license:asl2.0))))

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
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-version_check" ,rust-version_check_0_9_4)        
       ("rust-zerocopy" ,rust-zerocopy_0_7_32))))
    (home-page "None")
    (synopsis "A non-cryptographic hash function using AES-NI for high performance")
    (description
      (beautify-description "A non-cryptographic hash function using AES-NI for high performance"))
    (license (list license:expat license:asl2.0))))

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

(define rust-cached_proc_macro_0_19_1
  (package
    (name "rust-cached_proc_macro")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached_proc_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18g6fv3f70rqi8x30b4jaq2jkxm9ck4shv45jsr7bj6bywkgallx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling" ,rust-darling_0_14_4)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license (list license:expat))))

(define rust-cached_proc_macro_types_0_1_1
  (package
    (name "rust-cached_proc_macro_types")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached_proc_macro_types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h3gw61v1inay4g3b8pirxlz18m81k63dw2q18zj9fnmidmkds5d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license (list license:expat))))

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

(define rust-darling_0_14_4
  (package
    (name "rust-darling")
    (version "0.14.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l1qrn805bsxa0iy7x8bmdwr8c10hlw0yiqs8ckv7lbz86rhqxbv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling_core" ,rust-darling_core_0_14_4)        
       ("rust-darling_macro" ,rust-darling_macro_0_14_4))))
    (home-page "None")
    (synopsis "A proc-macro library for reading attributes into structs when\nimplementing custom derives.")
    (description
      (beautify-description "A proc-macro library for reading attributes into structs when\nimplementing custom derives."))
    (license (list license:expat))))

(define rust-darling_core_0_14_4
  (package
    (name "rust-darling_core")
    (version "0.14.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w4b2ndxmkwghwq84yphk8x15jnpivm08w596g12ry5pwsk1r70h"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-fnv" ,rust-fnv-1)        
       ("rust-ident_case" ,rust-ident_case_1_0_1)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-strsim" ,rust-strsim-0.10)        
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "Helper crate for proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
      (beautify-description "Helper crate for proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code."))
    (license (list license:expat))))

(define rust-darling_macro_0_14_4
  (package
    (name "rust-darling_macro")
    (version "0.14.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13mlyd5w275c815k0ijf6g4c446hs8b3m2h4an5isqgpr7dv9am4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling_core" ,rust-darling_core_0_14_4)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn-1))))
    (home-page "None")
    (synopsis "Internal support for a proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
      (beautify-description "Internal support for a proc-macro library for reading attributes into structs when\nimplementing custom derives. Use https://crates.io/crates/darling in your code."))
    (license (list license:expat))))

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

(define rust-rocket_http_0_5_0
  (package
    (name "rust-rocket_http")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17iq208zf9rfxdnx8hfjxnn51074cc9li99yjigzwnfhjhv6d89p"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cookie" ,rust-cookie_0_18_0)        
       ("rust-either" ,rust-either_1_9_0)        
       ("rust-futures" ,rust-futures_0_3_30)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-hyper" ,rust-hyper_0_14_28)        
       ("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-pear" ,rust-pear_0_2_8)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-ref-cast" ,rust-ref-cast_1_0_22)        
       ("rust-rustls" ,rust-rustls_0_21_10)        
       ("rust-rustls-pemfile" ,rust-rustls-pemfile_1_0_4)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-smallvec" ,rust-smallvec_1_13_1)        
       ("rust-stable-pattern" ,rust-stable-pattern_0_1_0)        
       ("rust-state" ,rust-state_0_6_0)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-rustls" ,rust-tokio-rustls_0_24_1)        
       ("rust-uncased" ,rust-uncased_0_9_10))))
    (home-page "https://rocket.rs")
    (synopsis "Types, traits, and parsers for HTTP requests, responses, and headers.")
    (description
      (beautify-description "Types, traits, and parsers for HTTP requests, responses, and headers."))
    (license (list license:expat license:asl2.0))))

(define rust-ref-cast_1_0_22
  (package
    (name "rust-ref-cast")
    (version "1.0.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ref-cast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gs7m6rikdf1k0vk6irnf9g0vwpf4ilzg2pg7cd1nwnia166v164"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ref-cast-impl" ,rust-ref-cast-impl_1_0_22))))
    (home-page "None")
    (synopsis "Safely cast \u0026T to \u0026U where the struct U contains a single field of type T.")
    (description
      (beautify-description "Safely cast \u0026T to \u0026U where the struct U contains a single field of type T."))
    (license (list license:expat license:asl2.0))))

(define rust-yansi_1_0_0-rc_1
  (package
    (name "rust-yansi")
    (version "1.0.0-rc.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "yansi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xr3n41j5v00scfkac2d6vhkxiq9nz3l5j6vw8f3g3bqixdjjrqk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-is-terminal" ,rust-is-terminal_0_4_10))))
    (home-page "None")
    (synopsis "A dead simple ANSI terminal color painting library.")
    (description
      (beautify-description "A dead simple ANSI terminal color painting library."))
    (license (list license:expat license:asl2.0))))

(define rust-rocket_codegen_0_5_0
  (package
    (name "rust-rocket_codegen")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k6hdf9s9y73kzj89qs688gnfjj1sl4imp6pdjz22pzpmdk808x2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-devise" ,rust-devise_0_4_1)        
       ("rust-glob" ,rust-glob-0.3)        
       ("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-rocket_http" ,rust-rocket_http_0_5_0)        
       ("rust-syn" ,rust-syn_2_0_48)        
       ("rust-unicode-xid" ,rust-unicode-xid-0.2)        
       ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework.")
    (description
      (beautify-description "Procedural macros for the Rocket web framework."))
    (license (list license:expat license:asl2.0))))

(define rust-figment_0_10_14
  (package
    (name "rust-figment")
    (version "0.10.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "figment" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09j0hadrq8nz1msixs5lixljikply1napk568l6hvmjrpp3mnvib"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atomic" ,rust-atomic_0_6_0)        
       ("rust-pear" ,rust-pear_0_2_8)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-toml" ,rust-toml_0_8_8)        
       ("rust-uncased" ,rust-uncased_0_9_10)        
       ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "A configuration library so con-free, it\u0027s unreal.")
    (description
      (beautify-description "A configuration library so con-free, it\u0027s unreal."))
    (license (list license:expat license:asl2.0))))

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
    (license (list license:expat))))

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

(define rust-state_0_6_0
  (package
    (name "rust-state")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "state" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n3n2h324h1y5zhaajh6kplvzfvg1l6hsr8siggmf4yq8m24m31b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-loom" ,rust-loom-0.5))))
    (home-page "None")
    (synopsis "A library for safe and effortless global and thread-local state management.")
    (description
      (beautify-description "A library for safe and effortless global and thread-local state management."))
    (license (list license:expat license:asl2.0))))

(define rust-ubyte_0_10_4
  (package
    (name "rust-ubyte")
    (version "0.10.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ubyte" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1spj3k9sx6xvfn7am9vm1b463hsr79nyvj8asi2grqhyrvvdw87p"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "A simple, complete, const-everything, saturating, human-friendly, no_std library for byte units.")
    (description
      (beautify-description "A simple, complete, const-everything, saturating, human-friendly, no_std library for byte units."))
    (license (list license:expat license:asl2.0))))

(define rust-multer_2_1_0
  (package
    (name "rust-multer")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "multer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hjiphaypj3phqaj5igrzcia9xfmf4rr4ddigbh8zzb96k1bvb01"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-encoding_rs" ,rust-encoding_rs_0_8_33)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-httparse" ,rust-httparse-1)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-mime" ,rust-mime_0_3_17)        
       ("rust-spin" ,rust-spin-0.9)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tokio-util" ,rust-tokio-util_0_7_10)        
       ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "https://github.com/rousan/multer-rs")
    (synopsis "An async parser for `multipart/form-data` content-type in Rust.")
    (description
      (beautify-description "An async parser for `multipart/form-data` content-type in Rust."))
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

(define rust-async-stream_0_3_5
  (package
    (name "rust-async-stream")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-stream" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l8sjq1rylkb1ak0pdyjn83b3k6x36j22myngl4sqqgg7whdsmnd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-stream-impl" ,rust-async-stream-impl_0_3_5)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "None")
    (synopsis "Asynchronous streams using async \u0026 await notation")
    (description
      (beautify-description "Asynchronous streams using async \u0026 await notation"))
    (license (list license:expat))))

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

(define rust-cookie_0_18_0
  (package
    (name "rust-cookie")
    (version "0.18.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y2ywf9isq0dwpj7m7jq7r1g9cs3xr2i6qipw5v030hj2kv1rn9w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-percent-encoding" ,rust-percent-encoding_2_3_1)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars.")
    (description
      (beautify-description "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars."))
    (license (list license:expat license:asl2.0))))

(define rust-pear_0_2_8
  (package
    (name "rust-pear")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pear" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1j03s6m80iqldnm6jzh3k1fbyk0lxirx8bi4ivgq3k3sq7va1k2c"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-inlinable_string" ,rust-inlinable_string_0_1_15)        
       ("rust-pear_codegen" ,rust-pear_codegen_0_2_8)        
       ("rust-yansi" ,rust-yansi_1_0_0-rc_1))))
    (home-page "None")
    (synopsis "A pear is a fruit.")
    (description
      (beautify-description "A pear is a fruit."))
    (license (list license:expat license:asl2.0))))

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

(define rust-uncased_0_9_10
  (package
    (name "rust-uncased")
    (version "0.9.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uncased" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15q6r6g4fszr8c2lzg9z9k9g52h8g29h24awda3d72cyw37qzf71"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_195)        
       ("rust-version_check" ,rust-version_check_0_9_4))))
    (home-page "None")
    (synopsis "Case-preserving, ASCII case-insensitive, no_std string types.")
    (description
      (beautify-description "Case-preserving, ASCII case-insensitive, no_std string types."))
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
      (("rust-memchr" ,rust-memchr_2_7_1))))
    (home-page "None")
    (synopsis "Stable port of std::str::Pattern and friends.")
    (description
      (beautify-description "Stable port of std::str::Pattern and friends."))
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
    (license (list license:asl2.0 license:expat))))

(define rust-pear_codegen_0_2_8
  (package
    (name "rust-pear_codegen")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pear_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xrwnlncg7l64gfy82vf6kq55ww7p6krq6bc3pqwymxpiq76f8if"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics_0_10_1)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "A (codegen) pear is a fruit.")
    (description
      (beautify-description "A (codegen) pear is a fruit."))
    (license (list license:expat license:asl2.0))))

(define rust-proc-macro2-diagnostics_0_10_1
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2-diagnostics" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1j48ipc80pykvhx6yhndfa774s58ax1h6sm6mlhf09ls76f6l1mg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48)        
       ("rust-version_check" ,rust-version_check_0_9_4)        
       ("rust-yansi" ,rust-yansi_1_0_0-rc_1))))
    (home-page "https://github.com/SergioBenitez/proc-macro2-diagnostics")
    (synopsis "Diagnostics for proc-macro2.")
    (description
      (beautify-description "Diagnostics for proc-macro2."))
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

(define rust-ref-cast-impl_1_0_22
  (package
    (name "rust-ref-cast-impl")
    (version "1.0.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ref-cast-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k5w51zyy06kcnr0vw71395mx1xrlxlpma35zjx2w2lvv7wb9paz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Derive implementation for ref_cast::RefCast.")
    (description
      (beautify-description "Derive implementation for ref_cast::RefCast."))
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

(define rust-devise_0_4_1
  (package
    (name "rust-devise")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y45iag4hyvspkdsf6d856hf0ihf9vjnaga3c7y6c72l7zywxsnn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-devise_codegen" ,rust-devise_codegen_0_4_1)        
       ("rust-devise_core" ,rust-devise_core_0_4_1))))
    (home-page "None")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      (beautify-description "A library for devising derives and other procedural macros."))
    (license (list license:expat license:asl2.0))))

(define rust-devise_core_0_4_1
  (package
    (name "rust-devise_core")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sp5idq0idng9i5kwjd8slvc724s97r28arrhyqq1jpx1ax0vd9m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_2_4_2)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics_0_10_1)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      (beautify-description "A library for devising derives and other procedural macros."))
    (license (list license:expat license:asl2.0))))

(define rust-devise_codegen_0_4_1
  (package
    (name "rust-devise_codegen")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mpy5mmsigkj5f72gby82yk4advcqj97am2wzn0dwkj8vnwg934w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-devise_core" ,rust-devise_core_0_4_1)        
       ("rust-quote" ,rust-quote_1_0_35))))
    (home-page "None")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      (beautify-description "A library for devising derives and other procedural macros."))
    (license (list license:expat license:asl2.0))))

(define rust-atomic_0_6_0
  (package
    (name "rust-atomic")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atomic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15193mfhmrq3p6vi1a10hw3n6kvzf5h32zikhby3mdj0ww1q10cd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bytemuck" ,rust-bytemuck_1_14_1))))
    (home-page "None")
    (synopsis "Generic Atomic\u003cT\u003e wrapper type")
    (description
      (beautify-description "Generic Atomic\u003cT\u003e wrapper type"))
    (license (list license:asl2.0 license:expat))))

(define rust-toml_0_8_8
  (package
    (name "rust-toml")
    (version "0.8.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ddbahcrrxf9374mkn3c1h2a2g6a883qx23kywl6k8lxikn9b8d1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde_spanned" ,rust-serde_spanned_0_6_5)        
       ("rust-toml_datetime" ,rust-toml_datetime_0_6_5)        
       ("rust-toml_edit" ,rust-toml_edit_0_21_0))
      #:cargo-development-inputs
      (("rust-serde" ,rust-serde_1_0_195))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "A native Rust encoder and decoder of TOML-formatted files and streams. Provides\nimplementations of the standard Serialize/Deserialize traits for TOML data to\nfacilitate deserializing and serializing Rust structures.")
    (description
      (beautify-description "A native Rust encoder and decoder of TOML-formatted files and streams. Provides\nimplementations of the standard Serialize/Deserialize traits for TOML data to\nfacilitate deserializing and serializing Rust structures."))
    (license (list license:expat license:asl2.0))))

(define rust-bytemuck_1_14_1
  (package
    (name "rust-bytemuck")
    (version "1.14.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytemuck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n9bjrxhngiv0lq05f7kl0jw5wyms4z1vqv7q6a2nks01xh9097d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A crate for mucking around with piles of bytes.")
    (description
      (beautify-description "A crate for mucking around with piles of bytes."))
    (license (list license:zlib license:asl2.0 license:expat))))

(define rust-toml_edit_0_21_0
  (package
    (name "rust-toml_edit")
    (version "0.21.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml_edit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00xa3qfk34qazvnkfxyyyqqc6nyl2ksks1c5bd53n5has0y3hkfk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-indexmap" ,rust-indexmap_2_1_0)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_spanned" ,rust-serde_spanned_0_6_5)        
       ("rust-toml_datetime" ,rust-toml_datetime_0_6_5)        
       ("rust-winnow" ,rust-winnow_0_5_35))))
    (home-page "None")
    (synopsis "Yet another format-preserving TOML parser.")
    (description
      (beautify-description "Yet another format-preserving TOML parser."))
    (license (list license:expat license:asl2.0))))

(define rust-serde_spanned_0_6_5
  (package
    (name "rust-serde_spanned")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_spanned" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hgh6s3jjwyzhfk3xwb6pnnr1misq9nflwq0f026jafi37s24dpb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_195))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "Serde-compatible spanned Value")
    (description
      (beautify-description "Serde-compatible spanned Value"))
    (license (list license:expat license:asl2.0))))

(define rust-toml_datetime_0_6_5
  (package
    (name "rust-toml_datetime")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml_datetime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wds4pm2cn6agd38f0ivm65xnc7c7bmk9m0fllcaq82nd3lz8l1m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_195))))
    (home-page "https://github.com/toml-rs/toml")
    (synopsis "A TOML-compatible datetime type")
    (description
      (beautify-description "A TOML-compatible datetime type"))
    (license (list license:expat license:asl2.0))))

(define rust-winnow_0_5_35
  (package
    (name "rust-winnow")
    (version "0.5.35")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winnow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bbanirar6xpnh889fhfwfr4kklhyyqkniag2fh1v1kkkj5dfc8r"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_7_1))))
    (home-page "None")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
      (beautify-description "A byte-oriented, zero-copy, parser combinators library"))
    (license (list license:expat))))

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

(define rust-async-stream-impl_0_3_5
  (package
    (name "rust-async-stream-impl")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-stream-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14q179j4y8p2z1d0ic6aqgy9fhwz8p9cai1ia8kpw4bw7q12mrhn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "proc macros for async-stream crate")
    (description
      (beautify-description "proc macros for async-stream crate"))
    (license (list license:expat))))

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

(define rust-chrono-tz-build_0_2_1
  (package
    (name "rust-chrono-tz-build")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono-tz-build" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03rmzd69cn7fp0fgkjr5042b3g54s2l941afjm3001ls7kqkjgj3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-parse-zoneinfo" ,rust-parse-zoneinfo-0.3)        
       ("rust-phf" ,rust-phf-0.11)        
       ("rust-phf_codegen" ,rust-phf_codegen_0_11_2))))
    (home-page "None")
    (synopsis "internal build script for chrono-tz")
    (description
      (beautify-description "internal build script for chrono-tz"))
    (license (list license:expat license:asl2.0))))

(define rust-phf_codegen_0_11_2
  (package
    (name "rust-phf_codegen")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nia6h4qfwaypvfch3pnq1nd2qj64dif4a6kai3b7rjrsf49dlz8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-phf_generator" ,rust-phf_generator_0_11_2)        
       ("rust-phf_shared" ,rust-phf_shared_0_11_2))))
    (home-page "None")
    (synopsis "Codegen library for PHF types")
    (description
      (beautify-description "Codegen library for PHF types"))
    (license (list license:expat))))

(define rust-phf_generator_0_11_2
  (package
    (name "rust-phf_generator")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c14pjyxbcpwkdgw109f7581cc5fa3fnkzdq1ikvx7mdq9jcrr28"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-phf_shared" ,rust-phf_shared_0_11_2)        
       ("rust-rand" ,rust-rand-0.8))))
    (home-page "None")
    (synopsis "PHF generation logic")
    (description
      (beautify-description "PHF generation logic"))
    (license (list license:expat))))

(define rust-phf_shared_0_11_2
  (package
    (name "rust-phf_shared")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0azphb0a330ypqx3qvyffal5saqnks0xvl8rj73jlk3qxxgbkz4h"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-siphasher" ,rust-siphasher_0_3_11))))
    (home-page "None")
    (synopsis "Support code shared by PHF libraries")
    (description
      (beautify-description "Support code shared by PHF libraries"))
    (license (list license:expat))))

(define rust-siphasher_0_3_11
  (package
    (name "rust-siphasher")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "siphasher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))))
    (build-system cargo-build-system)
    (home-page "https://docs.rs/siphasher")
    (synopsis "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (description
      (beautify-description "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust"))
    (license (list license:expat license:asl2.0))))

(define rust-quoted_printable_0_5_0
  (package
    (name "rust-quoted_printable")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quoted_printable" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c4fkkm95ff59vjmk6qi6p7sawlfschw3rcgq5l4nhvvi0p2iv3r"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/staktrace/quoted-printable/blob/master/README.md")
    (synopsis "A simple encoder/decoder for quoted-printable data")
    (description
      (beautify-description "A simple encoder/decoder for quoted-printable data"))
    (license (list license:bsd-0))))

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

(define rust-chumsky_0_9_3
  (package
    (name "rust-chumsky")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chumsky" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jcnafc8rjfs1al08gqzyn0kpbaizgdwrd0ajqafspd18ikxdswf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hashbrown" ,rust-hashbrown_0_14_3)        
       ("rust-stacker" ,rust-stacker-0.1))))
    (home-page "None")
    (synopsis "A parser library for humans with powerful error recovery")
    (description
      (beautify-description "A parser library for humans with powerful error recovery"))
    (license (list license:expat))))

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

(define rust-email-encoding_0_2_0
  (package
    (name "rust-email-encoding")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "email-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xgg9xn88jaiz83dfjwjd46w83mwm84r2mdqvi4a7xwchywj3yyv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)        
       ("rust-memchr" ,rust-memchr_2_7_1))))
    (home-page "None")
    (synopsis "Low level email encoding RFCs implementations")
    (description
      (beautify-description "Low level email encoding RFCs implementations"))
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

(define rust-migrations_internals_2_1_0
  (package
    (name "rust-migrations_internals")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "migrations_internals" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nnsr5d4h2kcvmz2j4l8g8n8r1zm7nngwmp842q58lh1h0azf8qg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_195)        
       ("rust-toml" ,rust-toml-0.7))))
    (home-page "https://diesel.rs")
    (synopsis "Internal implementation of diesels migration mechanism")
    (description
      (beautify-description "Internal implementation of diesels migration mechanism"))
    (license (list license:expat license:asl2.0))))

(define rust-migrations_macros_2_1_0
  (package
    (name "rust-migrations_macros")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "migrations_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "027gd4s58fmh7jil9rz0ckdsdw81mqfa6dyqbfmvnrqfqxd35qyc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-migrations_internals" ,rust-migrations_internals_2_1_0)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35))))
    (home-page "https://diesel.rs")
    (synopsis "Codegeneration macros for diesels embedded migrations")
    (description
      (beautify-description "Codegeneration macros for diesels embedded migrations"))
    (license (list license:expat license:asl2.0))))

(define rust-pest_2_7_6
  (package
    (name "rust-pest")
    (version "2.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01mdyqmjm6iyrbx4dkays4ss0b3mk5i93lb42yr4ajn4hf6hs80z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-ucd-trie" ,rust-ucd-trie_0_1_6))))
    (home-page "https://pest.rs/")
    (synopsis "The Elegant Parser")
    (description
      (beautify-description "The Elegant Parser"))
    (license (list license:expat license:asl2.0))))

(define rust-pest_derive_2_7_6
  (package
    (name "rust-pest_derive")
    (version "2.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pngjfj66x759llhxmpn2f4zxswj65lhx4ky094kmnxv6q9apmmw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-pest" ,rust-pest_2_7_6)        
       ("rust-pest_generator" ,rust-pest_generator_2_7_6))))
    (home-page "https://pest.rs/")
    (synopsis "pest\u0027s derive macro")
    (description
      (beautify-description "pest\u0027s derive macro"))
    (license (list license:expat license:asl2.0))))

(define rust-ucd-trie_0_1_6
  (package
    (name "rust-ucd-trie")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-trie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ff4yfksirqs37ybin9aw71aa5gva00hw7jdxbw8w668zy964r7d"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "A trie for storing Unicode codepoint sets and maps.")
    (description
      (beautify-description "A trie for storing Unicode codepoint sets and maps."))
    (license (list license:expat license:asl2.0))))

(define rust-pest_generator_2_7_6
  (package
    (name "rust-pest_generator")
    (version "2.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x9jdm3s1wd0pp2xp3qklwjv602ak73zggimlxinijgz0l1r8c9a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-pest" ,rust-pest_2_7_6)        
       ("rust-pest_meta" ,rust-pest_meta_2_7_6)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://pest.rs/")
    (synopsis "pest code generator")
    (description
      (beautify-description "pest code generator"))
    (license (list license:expat license:asl2.0))))

(define rust-pest_meta_2_7_6
  (package
    (name "rust-pest_meta")
    (version "2.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_meta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zabpc2mvpjmdqx7jzfghmrsxkywvhf9967q3a6vflwy4psn5zx7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-pest" ,rust-pest_2_7_6)        
       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://pest.rs/")
    (synopsis "pest meta language parser and validator")
    (description
      (beautify-description "pest meta language parser and validator"))
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

(define rust-cron_0_12_0
  (package
    (name "rust-cron")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cron" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dza4ng021s3zl4xljpjx1x5cz7fgdbf2ri8zlmwas60wi8npxqz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-chrono" ,rust-chrono_0_4_33)        
       ("rust-nom" ,rust-nom_7_1_3)        
       ("rust-once_cell" ,rust-once_cell_1_19_0))))
    (home-page "None")
    (synopsis "A cron expression parser and schedule explorer.")
    (description
      (beautify-description "A cron expression parser and schedule explorer."))
    (license (list license:expat license:asl2.0))))

(define rust-simple_asn1_0_6_2
  (package
    (name "rust-simple_asn1")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "simple_asn1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11d0l3l7lppzr1wxhvsbmjmw6s2vy3v7b8ygz500z4di9qhfbi5d"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-bigint" ,rust-num-bigint-0.4)        
       ("rust-num-traits" ,rust-num-traits-0.2)        
       ("rust-thiserror" ,rust-thiserror_1_0_56))
      #:cargo-development-inputs
      (("rust-time" ,rust-time_0_3_31))))
    (home-page "None")
    (synopsis "A simple DER/ASN.1 encoding/decoding library.")
    (description
      (beautify-description "A simple DER/ASN.1 encoding/decoding library."))
    (license (list license:isc))))

(define rust-pem_3_0_3
  (package
    (name "rust-pem")
    (version "3.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pem" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0463ya67xrxaqn4qs9iz7rsx4parcasd78pd9fv7yd1m81wwr3qv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_21_7)        
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "https://github.com/jcreekmore/pem-rs.git")
    (synopsis "Parse and encode PEM-encoded data.")
    (description
      (beautify-description "Parse and encode PEM-encoded data."))
    (license (list license:expat))))

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
    (license (list license:asl2.0))))

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
       ("rust-wasi" ,rust-wasi-0.11)        
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking I/O.")
    (description
      (beautify-description "Lightweight non-blocking I/O."))
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

(define rust-diesel_derives_2_1_2
  (package
    (name "rust-diesel_derives")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_derives" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i1bzp6rxnrrlgz1y946ap3203vjvack9a05h135mxblfmrkg0zg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-diesel_table_macro_syntax" ,rust-diesel_table_macro_syntax_0_1_0)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://diesel.rs")
    (synopsis "You should not use this crate directly, it is internal to Diesel.")
    (description
      (beautify-description "You should not use this crate directly, it is internal to Diesel."))
    (license (list license:expat license:asl2.0))))

(define rust-pq-sys_0_4_8
  (package
    (name "rust-pq-sys")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pq-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gfygvp69i5i6vxbi9qp2xaf75x09js9wy1hpl67r6fz4qj0bh1i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "None")
    (synopsis "Auto-generated rust bindings for libpq")
    (description
      (beautify-description "Auto-generated rust bindings for libpq"))
    (license (list license:expat license:asl2.0))))

(define rust-diesel_table_macro_syntax_0_1_0
  (package
    (name "rust-diesel_table_macro_syntax")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_table_macro_syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i9115qgsnargr6a707lqcjc45wqzq351a2gbvnnyw2kqkpmfmgw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-syn" ,rust-syn_2_0_48))))
    (home-page "https://diesel.rs")
    (synopsis "Internal diesel crate")
    (description
      (beautify-description "Internal diesel crate"))
    (license (list license:expat license:asl2.0))))

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
      (("rust-half" ,rust-half-1)        
       ("rust-serde" ,rust-serde_1_0_195))))
    (home-page "None")
    (synopsis "CBOR support for serde.")
    (description
      (beautify-description "CBOR support for serde."))
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
      (("rust-powerfmt" ,rust-powerfmt-0.2))))
    (home-page "None")
    (synopsis "Ranged integers")
    (description
      (beautify-description "Ranged integers"))
    (license (list license:expat license:asl2.0))))

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

(define rust-tungstenite_0_20_1
  (package
    (name "rust-tungstenite")
    (version "0.20.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tungstenite" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fbgcv3h4h1bhhf5sqbwqsp7jnc44bi4m41sgmhzdsk2zl8aqgcy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder-1)        
       ("rust-bytes" ,rust-bytes_1_5_0)        
       ("rust-data-encoding" ,rust-data-encoding_2_5_0)        
       ("rust-http" ,rust-http_0_2_11)        
       ("rust-httparse" ,rust-httparse-1)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-sha1" ,rust-sha1_0_10_6)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-url" ,rust-url_2_5_0)        
       ("rust-utf-8" ,rust-utf-8-0.7))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description
      (beautify-description "Lightweight stream-based WebSocket implementation"))
    (license (list license:expat license:asl2.0))))

(define rust-trust-dns-resolver_0_23_2
  (package
    (name "rust-trust-dns-resolver")
    (version "0.23.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-resolver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rhbwg7v93yvl3p64skwhkx2zfh2abrx35g3fcy8nwgimz1yd8qh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-ipconfig" ,rust-ipconfig_0_3_2)        
       ("rust-lru-cache" ,rust-lru-cache-0.1)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-parking_lot" ,rust-parking_lot_0_12_1)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-resolv-conf" ,rust-resolv-conf-0.7)        
       ("rust-smallvec" ,rust-smallvec_1_13_1)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-trust-dns-proto" ,rust-trust-dns-proto_0_23_2))))
    (home-page "https://trust-dns.org/")
    (synopsis "Trust-DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries.")
    (description
      (beautify-description "Trust-DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries."))
    (license (list license:expat license:asl2.0))))

(define rust-async-compression_0_4_6
  (package
    (name "rust-async-compression")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-compression" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b6874q56g1cx8ivs9j89d757rsh9kyrrwlp1852094jjrmg85m1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-brotli" ,rust-brotli_3_4_0)        
       ("rust-flate2" ,rust-flate2_1_0_28)        
       ("rust-futures-core" ,rust-futures-core_0_3_30)        
       ("rust-memchr" ,rust-memchr_2_7_1)        
       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)        
       ("rust-tokio" ,rust-tokio_1_35_1))))
    (home-page "None")
    (synopsis "Adaptors between compression crates and Rust\u0027s modern asynchronous IO types.")
    (description
      (beautify-description "Adaptors between compression crates and Rust\u0027s modern asynchronous IO types."))
    (license (list license:expat license:asl2.0))))

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

(define rust-cookie_store_0_16_2
  (package
    (name "rust-cookie_store")
    (version "0.16.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ahx7xyrx6rbm13pmd0w2qy6f4vzrc2nqlr0vc2cy4rflvxx01nn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cookie" ,rust-cookie-0.16)        
       ("rust-idna" ,rust-idna-0.2)        
       ("rust-log" ,rust-log-0.4)        
       ("rust-publicsuffix" ,rust-publicsuffix-2)        
       ("rust-serde" ,rust-serde_1_0_195)        
       ("rust-serde_derive" ,rust-serde_derive_1_0_195)        
       ("rust-serde_json" ,rust-serde_json_1_0_111)        
       ("rust-time" ,rust-time_0_3_31)        
       ("rust-url" ,rust-url_2_5_0))))
    (home-page "None")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description
      (beautify-description "Implementation of Cookie storage and retrieval"))
    (license (list license:expat license:asl2.0))))

(define rust-wasm-streams_0_3_0
  (package
    (name "rust-wasm-streams")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-streams" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iqa4kmhbsjj8k4q15i1x0x4p3xda0dhbg7zw51mydr4g129sq5l"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-js-sys" ,rust-js-sys_0_3_67)        
       ("rust-wasm-bindgen" ,rust-wasm-bindgen_0_2_90)        
       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures_0_4_40))
      #:cargo-development-inputs
      (("rust-web-sys" ,rust-web-sys_0_3_67))))
    (home-page "None")
    (synopsis "Bridging between web streams and Rust streams using WebAssembly")
    (description
      (beautify-description "Bridging between web streams and Rust streams using WebAssembly"))
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

(define rust-trust-dns-proto_0_23_2
  (package
    (name "rust-trust-dns-proto")
    (version "0.23.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-proto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x6kaa9vdzq5j6yx6ik0kmp76nd4d9c1x81ii54g8my1a4k1269i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait_0_1_77)        
       ("rust-cfg-if" ,rust-cfg-if-1)        
       ("rust-data-encoding" ,rust-data-encoding_2_5_0)        
       ("rust-enum-as-inner" ,rust-enum-as-inner_0_6_0)        
       ("rust-futures-channel" ,rust-futures-channel_0_3_30)        
       ("rust-futures-io" ,rust-futures-io_0_3_30)        
       ("rust-futures-util" ,rust-futures-util_0_3_30)        
       ("rust-idna" ,rust-idna-0.4)        
       ("rust-ipnet" ,rust-ipnet_2_9_0)        
       ("rust-once_cell" ,rust-once_cell_1_19_0)        
       ("rust-rand" ,rust-rand-0.8)        
       ("rust-smallvec" ,rust-smallvec_1_13_1)        
       ("rust-thiserror" ,rust-thiserror_1_0_56)        
       ("rust-tinyvec" ,rust-tinyvec-1)        
       ("rust-tokio" ,rust-tokio_1_35_1)        
       ("rust-tracing" ,rust-tracing_0_1_40)        
       ("rust-url" ,rust-url_2_5_0))))
    (home-page "https://trust-dns.org/")
    (synopsis "Trust-DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Trust-DNS projects.")
    (description
      (beautify-description "Trust-DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Trust-DNS projects."))
    (license (list license:expat license:asl2.0))))

(define rust-ipconfig_0_3_2
  (package
    (name "rust-ipconfig")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ipconfig" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zwr0x3jnqmjdqqbzhb0nid011qyhcyfdfqv32cdw85pjqpvk3dm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-socket2" ,rust-socket2_0_5_5)        
       ("rust-widestring" ,rust-widestring-1)        
       ("rust-windows-sys" ,rust-windows-sys-0.48)        
       ("rust-winreg" ,rust-winreg_0_50_0))))
    (home-page "https://github.com/liranringel/ipconfig")
    (synopsis "Get network adapters information and network configuration for windows.")
    (description
      (beautify-description "Get network adapters information and network configuration for windows."))
    (license (list license:expat license:asl2.0))))

(define rust-enum-as-inner_0_6_0
  (package
    (name "rust-enum-as-inner")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "enum-as-inner" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sjl5z0ycicpxg88qnn57m6sxi3ny9fl7b7vz0pb61bcjsvcpz2z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-heck" ,rust-heck_0_4_1)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "A proc-macro for deriving inner field accessor functions on enums.")
    (description
      (beautify-description "A proc-macro for deriving inner field accessor functions on enums."))
    (license (list license:expat license:asl2.0))))

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
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      (beautify-description "heck is a case conversion library."))
    (license (list license:expat license:asl2.0))))

(define rust-flate2_1_0_28
  (package
    (name "rust-flate2")
    (version "1.0.28")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flate2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03llhsh4gqdirnfxxb9g2w9n0721dyn4yjir3pz7z4vjaxb3yc26"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crc32fast" ,rust-crc32fast-1)        
       ("rust-miniz_oxide" ,rust-miniz_oxide_0_7_1))))
    (home-page "https://github.com/rust-lang/flate2-rs")
    (synopsis "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.\nSupports miniz_oxide and multiple zlib implementations. Supports zlib, gzip,\nand raw deflate streams.")
    (description
      (beautify-description "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.\nSupports miniz_oxide and multiple zlib implementations. Supports zlib, gzip,\nand raw deflate streams."))
    (license (list license:expat license:asl2.0))))

(define rust-brotli_3_4_0
  (package
    (name "rust-brotli")
    (version "3.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "brotli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03qhcq09a6f8y4gm0bmsn7jrq5804cwpkcx3fyay1g7lgsj78q2i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib_2_0_4)        
       ("rust-alloc-stdlib" ,rust-alloc-stdlib_0_2_2)        
       ("rust-brotli-decompressor" ,rust-brotli-decompressor_2_5_1))))
    (home-page "https://github.com/dropbox/rust-brotli")
    (synopsis "A brotli compressor and decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. All included code is safe.")
    (description
      (beautify-description "A brotli compressor and decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. All included code is safe."))
    (license (list license:bsd-3 license:expat))))

(define rust-alloc-no-stdlib_2_0_4
  (package
    (name "rust-alloc-no-stdlib")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alloc-no-stdlib" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cy6r2sfv5y5cigv86vms7n5nlwhx1rbyxwcraqnmm1rxiib2yyc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dropbox/rust-alloc-no-stdlib")
    (synopsis "A dynamic allocator that may be used with or without the stdlib. This allows a package with nostd to allocate memory dynamically and be used either with a custom allocator, items on the stack, or by a package that wishes to simply use Box\u003c\u003e. It also provides options to use calloc or a mutable global variable for pre-zeroed memory")
    (description
      (beautify-description "A dynamic allocator that may be used with or without the stdlib. This allows a package with nostd to allocate memory dynamically and be used either with a custom allocator, items on the stack, or by a package that wishes to simply use Box\u003c\u003e. It also provides options to use calloc or a mutable global variable for pre-zeroed memory"))
    (license (list license:bsd-3))))

(define rust-brotli-decompressor_2_5_1
  (package
    (name "rust-brotli-decompressor")
    (version "2.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "brotli-decompressor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kyyh9701dwqzwvn2frff4ww0zibikqd1s1xvl7n1pfpc3z4lbjf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib_2_0_4)        
       ("rust-alloc-stdlib" ,rust-alloc-stdlib_0_2_2))))
    (home-page "https://github.com/dropbox/rust-brotli-decompressor")
    (synopsis "A brotli decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. Alternatively, --features=unsafe turns off array bounds checks and memory initialization but provides a safe interface for the caller.  Without adding the --features=unsafe argument, all included code is safe. For compression in addition to this library, download https://github.com/dropbox/rust-brotli")
    (description
      (beautify-description "A brotli decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib\u0027s allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. Alternatively, --features=unsafe turns off array bounds checks and memory initialization but provides a safe interface for the caller.  Without adding the --features=unsafe argument, all included code is safe. For compression in addition to this library, download https://github.com/dropbox/rust-brotli"))
    (license (list license:bsd-3 license:expat))))

(define rust-alloc-stdlib_0_2_2
  (package
    (name "rust-alloc-stdlib")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "alloc-stdlib" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kkfbld20ab4165p29v172h8g0wvq8i06z8vnng14whw0isq5ywl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib_2_0_4))))
    (home-page "https://github.com/dropbox/rust-alloc-no-stdlib")
    (synopsis "A dynamic allocator example that may be used with the stdlib")
    (description
      (beautify-description "A dynamic allocator example that may be used with the stdlib"))
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

(define rust-openssl-macros_0_1_1
  (package
    (name "rust-openssl-macros")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_78)        
       ("rust-quote" ,rust-quote_1_0_35)        
       ("rust-syn" ,rust-syn_2_0_48))))
    (home-page "None")
    (synopsis "Internal macros used by the openssl crate.")
    (description
      (beautify-description "Internal macros used by the openssl crate."))
    (license (list license:expat license:asl2.0))))

;; (define rust-openssl-sys_0_9_99
;;   (package
;;     (name "rust-openssl-sys")
;;     (version "0.9.99")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (crate-uri "openssl-sys" version))
;;         (file-name
;;           (string-append name "-" version ".tar.gz"))
;;         (sha256
;;           (base32
;;             "1bjl4jczvc2zk15gd5pqrnm2apf04iw7j3s66948w2868chvzq92"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;     `(#:cargo-inputs
;;       (("rust-cc" ,rust-cc_1_0_83)
;;        ("rust-libc" ,rust-libc_0_2_152)
;;        ("rust-openssl-src" ,rust-openssl-src_300_2_1+3_2_0)
;;        ("rust-pkg-config" ,rust-pkg-config_0_3_29)
;;        ("rust-vcpkg" ,rust-vcpkg-0.2))))
;;     (home-page "None")
;;     (synopsis "FFI bindings to OpenSSL")
;;     (description
;;       (beautify-description "FFI bindings to OpenSSL"))
;;     (license (list license:expat))))

(define rust-openssl-src_300_2_1+3_2_0
  (package
    (name "rust-openssl-src")
    (version "300.2.1+3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-src" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lrj0fsa8y3yc7aszxrfxaxspfw5c3lrgikk286wm9cijz17dr1z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_83))))
    (home-page "None")
    (synopsis "Source of OpenSSL and logic to build it.")
    (description
      (beautify-description "Source of OpenSSL and logic to build it."))
    (license (list license:expat license:asl2.0))))
