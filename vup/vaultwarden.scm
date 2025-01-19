(define-module (vup vaultwarden)
 #:use-module (guix build-system cargo)
 #:use-module (guix build-system copy)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (gnu packages crates-crypto)
 #:use-module (gnu packages crates-windows)
 #:use-module (gnu packages crates-io)
 #:use-module (gnu packages crates-tls)
 #:use-module (gnu packages crates-database)
 #:use-module (gnu packages crates-web)
 #:use-module (gnu packages perl)
 #:use-module (gnu packages c)
 #:use-module (gnu packages tls)
 #:use-module (gnu packages pkg-config)
 #:use-module (guix download)
 #:use-module (guix git-download)
 #:use-module ((guix import utils) #:select (beautify-description)))

(define-public web-vault
  (let* ((version "v2024.6.2c"))
    (package
      (name "web-vault")
      (version version)
      (source
        (origin
          (method url-fetch)
          (uri (string-append "https://github.com/dani-garcia/bw_web_builds/releases/download/" version "/bw_web_" version ".tar.gz"))
          (sha256
            (base32
              "0m68wjq0awmbarknrmgcz1csm6d556q824w9jgdxfrzndy5jc9pz"))))
      (build-system copy-build-system)
      (home-page "https://github.com/dani-garcia/bw_web_builds")
      (synopsis "Web vault builds for vaultwarden")
      (description "Web vault builds for vaultwarden")
      (license license:gpl3))))

;; (define-public rust-mimalloc-0.1
;;   (package
;;     (name "rust-mimalloc")
;;     (version "0.1.39")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (crate-uri "mimalloc" version))
;;        (file-name (string-append name "-" version ".tar.gz"))
;;        (sha256
;;         (base32
;;          "176w9gf5qxs07kd2q39f0k25rzmp4kyx5r13wc8sk052bqmr40gs"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:cargo-inputs
;;        (("rust-libmimalloc-sys" ,rust-libmimalloc-sys-0.1))))
;;     (inputs (list mimalloc))
;;     (home-page "https://crates.io/crates/mimalloc")
;;     (synopsis "Performance and security oriented drop-in allocator")
;;     (description "This package provides a performance and security oriented
;; drop-in allocator.")
;;     (license license:expat)))

(define-public vaultwarden
  (package
    (name "rust-vaultwarden")
    (version "1.32.7")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
        (url "https://github.com/dani-garcia/vaultwarden")
        (commit version)))
       (file-name (git-file-name name version))
         (sha256
           (base32
             "11ryf8hkinl61fhc7q7vrmpfdgvg8igl80cspmr3kwx74vam05lv"))))
    (build-system cargo-build-system)
    (native-inputs (list perl pkg-config))
    (inputs (list openssl))
    (arguments
    `(#:tests? #f
      #:features '("sqlite") ;  "vendored_openssl"
      #:cargo-inputs
      (("rust-argon2" ,rust-argon2-0.5)
       ("rust-bigdecimal" ,rust-bigdecimal-0.4)
       ("rust-bytes" ,rust-bytes-1)
       ("rust-cached" ,rust-cached_0_54_0)
       ("rust-chrono" ,rust-chrono-0.4)
       ("rust-chrono-tz" ,rust-chrono-tz-0.10)
       ("rust-cookie" ,rust-cookie_0_18_1)
       ("rust-cookie_store" ,rust-cookie_store_0_21_1)
       ("rust-dashmap" ,rust-dashmap-6)
       ("rust-data-encoding" ,rust-data-encoding-2)
       ("rust-data-url" ,rust-data-url-0.3)
       ("rust-diesel" ,rust-diesel_2_2_6)
       ("rust-diesel_logger" ,rust-diesel_logger_0_4_0)
       ("rust-diesel_migrations" ,rust-diesel_migrations_2_2_0)
       ("rust-dotenvy" ,rust-dotenvy-0.15)
       ("rust-email_address" ,rust-email_address_0_2_9)
       ("rust-fern" ,rust-fern_0_7_1)
       ("rust-futures" ,rust-futures-0.3)
       ("rust-governor" ,rust-governor_0_8_0)
       ("rust-grass_compiler" ,rust-grass_compiler_0_13_4)
       ("rust-handlebars" ,rust-handlebars_6_2_0)
       ("rust-hickory-resolver" ,rust-hickory-resolver_0_24_2)
       ("rust-html5gum" ,rust-html5gum_0_7_0)
       ("rust-job_scheduler_ng" ,rust-job_scheduler_ng_2_0_5)
       ("rust-jsonwebtoken" ,rust-jsonwebtoken-9)
       ("rust-lettre" ,rust-lettre_0_11_11)
       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.30)
       ("rust-log" ,rust-log-0.4)
       ("rust-mimalloc" ,rust-mimalloc-0.1)
       ("rust-num-derive" ,rust-num-derive-0.4)
       ("rust-num-traits" ,rust-num-traits-0.2)
       ("rust-once_cell" ,rust-once_cell_1_20_2)
       ("rust-openssl" ,rust-openssl-0.10)
       ("rust-paste" ,rust-paste-1)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-pico-args" ,rust-pico-args-0.5)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-regex" ,rust-regex-1)
       ("rust-reqwest" ,rust-reqwest-0.12)
       ("rust-ring" ,rust-ring-0.17)
       ("rust-rmpv" ,rust-rmpv-1)
       ("rust-rocket" ,rust-rocket-0.5)
       ("rust-rocket_ws" ,rust-rocket_ws_0_1_1)
       ("rust-rpassword" ,rust-rpassword_7_3_1)
       ("rust-semver" ,rust-semver-1)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde_json" ,rust-serde_json_1_0_133)
       ("rust-syslog" ,rust-syslog-7)
       ("rust-time" ,rust-time_0_3_37)
       ("rust-tokio" ,rust-tokio-1)
       ("rust-totp-lite" ,rust-totp-lite-2)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-url" ,rust-url-2)
       ("rust-uuid" ,rust-uuid-1)
       ("rust-webauthn-rs" ,rust-webauthn-rs_0_3_2)
       ("rust-which" ,rust-which_7_0_0)
       ("rust-yubico" ,rust-yubico_0_12_0))
      #:phases (modify-phases %standard-phases
                 (replace 'build
                   (lambda _
                     (substitute* "Cargo.toml"
                        (("yubico = \\{ git")
                         "# "))
                    (invoke "cargo" "build" "--features" "sqlite"))))))
    (home-page "None")
    (synopsis "Alternative implementation of the Bitwarden server API, compatible with the official clients")
    (description
      (beautify-description "Alternative implementation of the Bitwarden server API, compatible with the official clients"))
    (license #f)))

(define-public rust-lettre_0_11_11
  (package
    (name "rust-lettre")
    (version "0.11.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lettre" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xfns3a5lxqzxdi1fgfsclv5h2z9bjzyh1ycbs5gjggpgwb9lk5b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-std" ,rust-async-std-1)
       ("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-base64" ,rust-base64-0.22)
       ("rust-chumsky" ,rust-chumsky_0_9_3)
       ("rust-email-encoding" ,rust-email-encoding_0_3_1)
       ("rust-email_address" ,rust-email_address_0_2_9)
       ("rust-fastrand" ,rust-fastrand-2)
       ("rust-futures-io" ,rust-futures-io-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-hostname" ,rust-hostname-0.4)
       ("rust-httpdate" ,rust-httpdate-1)
       ("rust-idna" ,rust-idna-1)
       ("rust-mime" ,rust-mime-0.3)
       ("rust-native-tls" ,rust-native-tls-0.2)
       ("rust-nom" ,rust-nom-7)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-quoted_printable" ,rust-quoted_printable_0_5_1)
       ("rust-serde" ,rust-serde-1)
       ("rust-socket2" ,rust-socket2-0.5)
       ("rust-tokio" ,rust-tokio-1)
       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-url" ,rust-url-2))))
    (home-page "https://lettre.rs")
    (synopsis "Email client")
    (description
      (beautify-description "Email client"))
    (license (list license:expat))))

(define-public rust-handlebars_6_2_0
  (package
    (name "rust-handlebars")
    (version "6.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "handlebars" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05g3q5msrabnmqslny15qpvc0cfz6570syv3383rl7w32bhcsk7x"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log-0.4)
       ("rust-num-order" ,rust-num-order_1_2_0)
       ("rust-pest" ,rust-pest_2_7_15)
       ("rust-pest_derive" ,rust-pest_derive_2_7_15)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde_json" ,rust-serde_json_1_0_133)
       ("rust-thiserror" ,rust-thiserror-1)
       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/sunng87/handlebars-rust")
    (synopsis "Handlebars templating implemented in Rust.")
    (description
      (beautify-description "Handlebars templating implemented in Rust."))
    (license (list license:expat))))

(define-public rust-cached_0_54_0
  (package
    (name "rust-cached")
    (version "0.54.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1biwqcfkb7s4a3nk7mwfpv8hxl8d6ixzjdnpdyjyis9g99n8064p"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ahash" ,rust-ahash-0.8)
       ("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-cached_proc_macro" ,rust-cached_proc_macro_0_23_0)
       ("rust-cached_proc_macro_types" ,rust-cached_proc_macro_types_0_1_1)
       ("rust-futures" ,rust-futures-0.3)
       ("rust-hashbrown" ,rust-hashbrown-0.14)
       ("rust-once_cell" ,rust-once_cell_1_20_2)
       ("rust-thiserror" ,rust-thiserror-1)
       ("rust-tokio" ,rust-tokio-1)
       ("rust-web-time" ,rust-web-time-1))))
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license (list license:expat))))

(define-public rust-diesel_2_2_6
  (package
    (name "rust-diesel")
    (version "2.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04kxz9gss7wzis30bcgplxx8xkm635dx2vd30hr69ffdckgvxwfc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bigdecimal" ,rust-bigdecimal-0.4)
       ("rust-bitflags" ,rust-bitflags-2)
       ("rust-byteorder" ,rust-byteorder-1)
       ("rust-chrono" ,rust-chrono-0.4)
       ("rust-diesel_derives" ,rust-diesel_derives_2_2_3)
       ("rust-itoa" ,rust-itoa-1)
       ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.30)
       ("rust-mysqlclient-sys" ,rust-mysqlclient-sys_0_4_2)
       ("rust-num-bigint" ,rust-num-bigint-0.4)
       ("rust-num-integer" ,rust-num-integer-0.1)
       ("rust-num-traits" ,rust-num-traits-0.2)
       ("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-pq-sys" ,rust-pq-sys_0_6_3)
       ("rust-r2d2" ,rust-r2d2-0.8)
       ("rust-time" ,rust-time_0_3_37)
       ("rust-url" ,rust-url-2))))
    (home-page "https://diesel.rs")
    (synopsis "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL")
    (description
      (beautify-description "A safe, extensible ORM and Query Builder for PostgreSQL, SQLite, and MySQL"))
    (license (list license:expat license:asl2.0))))

(define-public rust-job_scheduler_ng_2_0_5
  (package
    (name "rust-job_scheduler_ng")
    (version "2.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "job_scheduler_ng" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0siw182x6b8cka89d65a7v6hixpqryyrwxc7s2b2jgijgwh55hl7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-chrono" ,rust-chrono-0.4)
       ("rust-cron" ,rust-cron_0_12_1)
       ("rust-uuid" ,rust-uuid-1))))
    (home-page "None")
    (synopsis "A simple cron-like job scheduling library for Rust (Updated since 2022).")
    (description
      (beautify-description "A simple cron-like job scheduling library for Rust (Updated since 2022)."))
    (license (list license:expat license:asl2.0))))

(define-public rust-governor_0_8_0
  (package
    (name "rust-governor")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "governor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l19nipyqazwnx33g70csqnh1jigr7nrdn3aayhyc0ffg62wfbc4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-dashmap" ,rust-dashmap-6)
       ("rust-futures-sink" ,rust-futures-sink-0.3)
       ("rust-futures-timer" ,rust-futures-timer-3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-no-std-compat" ,rust-no-std-compat-0.4)
       ("rust-nonzero_ext" ,rust-nonzero_ext_0_3_0)
       ("rust-parking_lot" ,rust-parking_lot_0_12_3)
       ("rust-portable-atomic" ,rust-portable-atomic_1_10_0)
       ("rust-quanta" ,rust-quanta_0_12_4)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-smallvec" ,rust-smallvec-1)
       ("rust-spinning_top" ,rust-spinning_top_0_3_0))))
    (home-page "https://github.com/boinkor-net/governor")
    (synopsis "A rate-limiting implementation in Rust")
    (description
      (beautify-description "A rate-limiting implementation in Rust"))
    (license (list license:expat))))

(define-public rust-diesel_migrations_2_2_0
  (package
    (name "rust-diesel_migrations")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_migrations" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xn12ny9m1ci74iqpvhcfyhapr6wj56k3wxz07q32hmd9dqcwwwa"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-diesel" ,rust-diesel_2_2_6)
       ("rust-migrations_internals" ,rust-migrations_internals_2_2_0)
       ("rust-migrations_macros" ,rust-migrations_macros_2_2_0))))
    (home-page "https://diesel.rs")
    (synopsis "Migration management for diesel")
    (description
      (beautify-description "Migration management for diesel"))
    (license (list license:expat license:asl2.0))))

(define-public rust-time_0_3_37
  (package
    (name "rust-time")
    (version "0.3.37")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08bvydyc14plkwhchzia5bcdbmm0mk5fzilsdpjx06w6hf48drrm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-deranged" ,rust-deranged-0.3)
       ("rust-itoa" ,rust-itoa-1)
       ("rust-libc" ,rust-libc_0_2_168)
       ("rust-num-conv" ,rust-num-conv-0.1)
       ("rust-num_threads" ,rust-num_threads_0_1_7)
       ("rust-powerfmt" ,rust-powerfmt-0.2)
       ("rust-serde" ,rust-serde-1)
       ("rust-time-core" ,rust-time-core-0.1)
       ("rust-time-macros" ,rust-time-macros_0_2_19))))
    (home-page "https://time-rs.github.io")
    (synopsis "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      (beautify-description "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]."))
    (license (list license:expat license:asl2.0))))

(define-public rust-yubico_0_12_0
  (package
    (name "rust-yubico")
    (version "0.12.0")
    (source
      (origin
       (method url-fetch)
       (uri "https://codeload.github.com/blackdex/yubico-rs/tar.gz/00df14811f58155c0f02e3ab10f1570ed3e115c6")
        ;;     (git-reference
        ;; (url "https://github.com/BlackDex/yubico-rs")
        ;; (commit "00df14811f58155c0f02e3ab10f1570ed3e115c6")))
       ;; (file-name (git-file-name name version))
       (sha256
         (base32
             "00id2cm99806am8x3y9sfknfhpaba4nd9x5ybb08nq3sbys1w0jz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64-0.22)
       ("rust-form_urlencoded" ,rust-form_urlencoded_1_2_1)
       ("rust-futures" ,rust-futures-0.3)
       ("rust-hmac" ,rust-hmac-0.12)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-reqwest" ,rust-reqwest-0.12)
       ("rust-sha1" ,rust-sha1-0.10)
       ("rust-threadpool" ,rust-threadpool-1))))
    (home-page "None")
    (synopsis "Yubikey client API library")
    (description
      (beautify-description "Yubikey client API library"))
    (license #f)))

(define-public rust-cookie_store_0_21_1
  (package
    (name "rust-cookie_store")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y9ydb52bcd1zc7r0mppy8c8l541p459a006xr0m52pq50c91b1f"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cookie" ,rust-cookie_0_18_1)
       ("rust-document-features" ,rust-document-features-0.2)
       ("rust-idna" ,rust-idna-1)
       ("rust-log" ,rust-log-0.4)
       ("rust-publicsuffix" ,rust-publicsuffix_2_3_0)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde_derive" ,rust-serde_derive_1_0_216)
       ("rust-serde_json" ,rust-serde_json_1_0_133)
       ("rust-time" ,rust-time_0_3_37)
       ("rust-url" ,rust-url-2))))
    (home-page "None")
    (synopsis "Implementation of Cookie storage and retrieval")
    (description
      (beautify-description "Implementation of Cookie storage and retrieval"))
    (license (list license:expat license:asl2.0))))

(define-public rust-webauthn-rs_0_3_2
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
       ("rust-nom" ,rust-nom-7)
       ("rust-openssl" ,rust-openssl-0.10)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde_cbor" ,rust-serde_cbor_0_11_2)
       ("rust-serde_derive" ,rust-serde_derive_1_0_216)
       ("rust-serde_json" ,rust-serde_json_1_0_133)
       ("rust-thiserror" ,rust-thiserror-1)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/kanidm/webauthn-rs")
    (synopsis "Webauthn Framework for Rust Web Servers")
    (description
      (beautify-description "Webauthn Framework for Rust Web Servers"))
    (license (list license:mpl2.0))))

(define-public rust-grass_compiler_0_13_4
  (package
    (name "rust-grass_compiler")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grass_compiler" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xngzb4h04fkdhyagrwcqanrl7arghj3v5sl84cfab12y3vkv7id"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-codemap" ,rust-codemap_0_1_3)
       ("rust-indexmap" ,rust-indexmap-2)
       ("rust-lasso" ,rust-lasso_0_7_3)
       ("rust-once_cell" ,rust-once_cell_1_20_2)
       ("rust-phf" ,rust-phf-0.11))))
    (home-page "None")
    (synopsis "Internal implementation of the grass compiler")
    (description
      (beautify-description "Internal implementation of the grass compiler"))
    (license (list license:expat))))

(define-public rust-diesel_logger_0_4_0
  (package
    (name "rust-diesel_logger")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15dgmrqgywvl1jw0nlnx90g9f0jzyqj92rpflqicyxdnzwzq6x40"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-diesel" ,rust-diesel_2_2_6)
       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/shssoichiro/diesel-logger")
    (synopsis "Times and logs queries executed by diesel")
    (description
      (beautify-description "Times and logs queries executed by diesel"))
    (license (list license:expat))))

(define-public rust-hickory-resolver_0_24_2
  (package
    (name "rust-hickory-resolver")
    (version "0.24.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hickory-resolver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d243gxb2c2nlm73c2vb4yp908z8mi6ixkriglkfb71qkjx2lbha"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-hickory-proto" ,rust-hickory-proto_0_24_2)
       ("rust-ipconfig" ,rust-ipconfig-0.3)
       ("rust-lru-cache" ,rust-lru-cache-0.1)
       ("rust-once_cell" ,rust-once_cell_1_20_2)
       ("rust-parking_lot" ,rust-parking_lot_0_12_3)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-resolv-conf" ,rust-resolv-conf-0.7)
       ("rust-smallvec" ,rust-smallvec-1)
       ("rust-thiserror" ,rust-thiserror-1)
       ("rust-tokio" ,rust-tokio-1)
       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://hickory-dns.org/")
    (synopsis "Hickory DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries.")
    (description
      (beautify-description "Hickory DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries."))
    (license (list license:expat license:asl2.0))))

(define-public rust-fern_0_7_1
  (package
    (name "rust-fern")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fern" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a9v59vcq2fgd6bwgbfl7q6b0zzgxn85y6g384z728wvf1gih5j3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_168)
       ("rust-log" ,rust-log-0.4)
       ("rust-reopen" ,rust-reopen-1)
       ("rust-syslog" ,rust-syslog-7))))
    (home-page "None")
    (synopsis "Simple, efficient logging")
    (description
      (beautify-description "Simple, efficient logging"))
    (license (list license:expat))))

(define-public rust-rocket_ws_0_1_1
  (package
    (name "rust-rocket_ws")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_ws" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bkqm2rwr5ps0wfzma2bpg9nqm9w70hryd3w2w0vfdy9d1v8gw95"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rocket" ,rust-rocket-0.5)
       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.21))))
    (home-page "https://rocket.rs")
    (synopsis "WebSocket support for Rocket.")
    (description
      (beautify-description "WebSocket support for Rocket."))
    (license (list license:expat license:asl2.0))))

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
      (("rust-libc" ,rust-libc_0_2_168)
       ("rust-rtoolbox" ,rust-rtoolbox_0_0_2)
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/conradkleinespel/rpassword")
    (synopsis "Read passwords in console applications.")
    (description
      (beautify-description "Read passwords in console applications."))
    (license (list license:asl2.0))))

(define-public rust-which_7_0_0
  (package
    (name "rust-which")
    (version "7.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0swcnfhsbpzaakfq6l6madmdshvk3mj1lwrqdswlcwyyk8kx7jn9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-either" ,rust-either-1)
       ("rust-home" ,rust-home_0_5_9)
       ("rust-rustix" ,rust-rustix-0.38)
       ("rust-winsafe" ,rust-winsafe-0.0.19))))
    (home-page "FILLMEIN")
    (synopsis "FILLMEIN")
    (description
      (beautify-description "FILLMEIN"))
    (license (list license:expat))))

(define-public rust-serde_json_1_0_133
  (package
    (name "rust-serde_json")
    (version "1.0.133")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xz3bswa527wln3fy0qb7y081nx3cp5yy1ggjhi6n5mrfcjfpz67"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-itoa" ,rust-itoa-1)
       ("rust-memchr" ,rust-memchr-2)
       ("rust-ryu" ,rust-ryu-1)
       ("rust-serde" ,rust-serde-1))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
      (beautify-description "A JSON serialization file format"))
    (license (list license:expat license:asl2.0))))

(define-public rust-html5gum_0_7_0
  (package
    (name "rust-html5gum")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "html5gum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wpzigh6g3hmshqybi3sm33n5r8vnn3aj796ayvn266n6rgqp4dk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-jetscii" ,rust-jetscii-0.5))))
    (home-page "None")
    (synopsis "A WHATWG-compliant HTML5 tokenizer and tag soup parser.")
    (description
      (beautify-description "A WHATWG-compliant HTML5 tokenizer and tag soup parser."))
    (license (list license:expat))))

(define-public rust-email_address_0_2_9
  (package
    (name "rust-email_address")
    (version "0.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "email_address" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jf4v3npa524c7npy7w3jl0a6gng26f51a4bgzs3jqna12dz2yg0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde-1))))
    (home-page "None")
    (synopsis "A Rust crate providing an implementation of an RFC-compliant `EmailAddress` newtype.")
    (description
      (beautify-description "A Rust crate providing an implementation of an RFC-compliant `EmailAddress` newtype."))
    (license (list license:expat))))

(define-public rust-once_cell_1_20_2
  (package
    (name "rust-once_cell")
    (version "1.20.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "once_cell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xb7rw1aqr7pa4z3b00y7786gyf8awx2gca3md73afy76dzgwq8j"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Single assignment cells and lazy values.")
    (description
      (beautify-description "Single assignment cells and lazy values."))
    (license (list license:expat license:asl2.0))))

(define-public rust-cookie_0_18_1
  (package
    (name "rust-cookie")
    (version "0.18.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iy749flficrlvgr3hjmf3igr738lk81n5akzf4ym4cs6cxg7pjd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-percent-encoding" ,rust-percent-encoding-2)
       ("rust-time" ,rust-time_0_3_37)
       ("rust-version_check" ,rust-version_check_0_9_5))))
    (home-page "None")
    (synopsis "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars.")
    (description
      (beautify-description "HTTP cookie parsing and cookie jar management. Supports signed and private\n(encrypted, authenticated) jars."))
    (license (list license:expat license:asl2.0))))

(define-public rust-chumsky_0_9_3
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
      (("rust-hashbrown" ,rust-hashbrown-0.14)
       ("rust-stacker" ,rust-stacker_0_1_17))))
    (home-page "None")
    (synopsis "A parser library for humans with powerful error recovery")
    (description
      (beautify-description "A parser library for humans with powerful error recovery"))
    (license (list license:expat))))

(define-public rust-quoted_printable_0_5_1
  (package
    (name "rust-quoted_printable")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quoted_printable" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wvwq6w6rdsx1yxzr7ckspff0qk0q9252dzmxrd4c0kv97c9n334"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/staktrace/quoted-printable/blob/master/README.md")
    (synopsis "A simple encoder/decoder for quoted-printable data")
    (description
      (beautify-description "A simple encoder/decoder for quoted-printable data"))
    (license (list license:bsd-0))))

(define-public rust-email-encoding_0_3_1
  (package
    (name "rust-email-encoding")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "email-encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k4xkmdm8j8vdaq9c3nx6qx14jzjdgab5yb5n9v48cdbpd5qjgga"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64-0.22)
       ("rust-memchr" ,rust-memchr-2))))
    (home-page "None")
    (synopsis "Low level email encoding RFCs implementations")
    (description
      (beautify-description "Low level email encoding RFCs implementations"))
    (license (list license:expat license:asl2.0))))

(define-public rust-stacker_0_1_17
  (package
    (name "rust-stacker")
    (version "0.1.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stacker" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yya93mkaxidcxcc1jhfvb58xpmnp4ikyyqsmyc5xnxbalyqi73r"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc-1)
       ("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-libc" ,rust-libc_0_2_168)
       ("rust-psm" ,rust-psm_0_1_24)
       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/rust-lang/stacker")
    (synopsis "A stack growth library useful when implementing deeply recursive algorithms that\nmay accidentally blow the stack.")
    (description
      (beautify-description "A stack growth library useful when implementing deeply recursive algorithms that\nmay accidentally blow the stack."))
    (license (list license:expat license:asl2.0))))

(define-public rust-psm_0_1_24
  (package
    (name "rust-psm")
    (version "0.1.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "psm" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0428cnwx8i9dhkcjwzap0amg9cjk8nhj0xr5hkhm6zl543r9y2r0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc-1))))
    (home-page "None")
    (synopsis "Portable Stack Manipulation: stack manipulation and introspection routines")
    (description
      (beautify-description "Portable Stack Manipulation: stack manipulation and introspection routines"))
    (license (list license:expat license:asl2.0))))

(define-public rust-libc_0_2_168
  (package
    (name "rust-libc")
    (version "0.2.168")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vab4inpw0dz78nii02hsxp1skqn06xzh64psw8wl1h63scb5bjs"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
      (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license (list license:expat license:asl2.0))))

(define-public rust-pest_2_7_15
  (package
    (name "rust-pest")
    (version "2.7.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p4rq45xprw9cx0pb8mmbfa0ih49l0baablv3cpfdy3c1pkayz4b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr-2)
       ("rust-thiserror" ,rust-thiserror_2_0_7)
       ("rust-ucd-trie" ,rust-ucd-trie_0_1_7))))
    (home-page "https://pest.rs/")
    (synopsis "The Elegant Parser")
    (description
      (beautify-description "The Elegant Parser"))
    (license (list license:expat license:asl2.0))))

(define-public rust-num-order_1_2_0
  (package
    (name "rust-num-order")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-order" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dhvdncf91ljxh9sawnfxcbiqj1gnag08lyias0cy3y4jxmmjysk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-modular" ,rust-num-modular_0_6_1))))
    (home-page "None")
    (synopsis "Numerically consistent `Eq`, `Ord` and `Hash` implementations for various `num` types (`u32`, `f64`, `num_bigint::BigInt`, etc.)")
    (description
      (beautify-description "Numerically consistent `Eq`, `Ord` and `Hash` implementations for various `num` types (`u32`, `f64`, `num_bigint::BigInt`, etc.)"))
    (license (list license:asl2.0))))

(define-public rust-pest_derive_2_7_15
  (package
    (name "rust-pest_derive")
    (version "2.7.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zpmcd1jv1c53agad5b3jb66ylxlzyv43x1bssh8fs7w3i11hrc1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-pest" ,rust-pest_2_7_15)
       ("rust-pest_generator" ,rust-pest_generator_2_7_15))))
    (home-page "https://pest.rs/")
    (synopsis "pest\u0027s derive macro")
    (description
      (beautify-description "pest\u0027s derive macro"))
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror_2_0_7
  (package
    (name "rust-thiserror")
    (version "2.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rrp8k3y2m87df7zrmd3ks2y2zpvk9c9sjdb2raihs6nrcw58q4k"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-thiserror-impl" ,rust-thiserror-impl_2_0_7))))
    (home-page "None")
    (synopsis "derive(Error)")
    (description
      (beautify-description "derive(Error)"))
    (license (list license:expat license:asl2.0))))

(define-public rust-ucd-trie_0_1_7
  (package
    (name "rust-ucd-trie")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-trie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "A trie for storing Unicode codepoint sets and maps.")
    (description
      (beautify-description "A trie for storing Unicode codepoint sets and maps."))
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl_2_0_7
  (package
    (name "rust-thiserror-impl")
    (version "2.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0diy4yillp5czqgycdhyp3i3z2iln49cspvscwbj3bri8ndp9n71"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-2))))
    (home-page "None")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
      (beautify-description "Implementation detail of the `thiserror` crate"))
    (license (list license:expat license:asl2.0))))

(define-public rust-num-modular_0_6_1
  (package
    (name "rust-num-modular")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-modular" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zv4miws3q1i93a0bd9wgc4njrr5j5786kr99hzxi9vgycdjdfqp"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Implementation of efficient integer division and modular arithmetic operations with generic number types.\nSupports various backends including num-bigint, etc..")
    (description
      (beautify-description "Implementation of efficient integer division and modular arithmetic operations with generic number types.\nSupports various backends including num-bigint, etc.."))
    (license (list license:asl2.0))))

(define-public rust-pest_generator_2_7_15
  (package
    (name "rust-pest_generator")
    (version "2.7.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yrpk5ymc56pffv7gqr5rkv92p3dc6s73lb8hy1wf3w77byrc4vx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-pest" ,rust-pest_2_7_15)
       ("rust-pest_meta" ,rust-pest_meta_2_7_15)
       ("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-2))))
    (home-page "https://pest.rs/")
    (synopsis "pest code generator")
    (description
      (beautify-description "pest code generator"))
    (license (list license:expat license:asl2.0))))

(define-public rust-pest_meta_2_7_15
  (package
    (name "rust-pest_meta")
    (version "2.7.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_meta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1skx7gm932bp77if63f7d72jrk5gygj39d8zsfzigmr5xa4q1rg1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-once_cell" ,rust-once_cell_1_20_2)
       ("rust-pest" ,rust-pest_2_7_15)
       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://pest.rs/")
    (synopsis "pest meta language parser and validator")
    (description
      (beautify-description "pest meta language parser and validator"))
    (license (list license:expat license:asl2.0))))

(define-public rust-cached_proc_macro_0_23_0
  (package
    (name "rust-cached_proc_macro")
    (version "0.23.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cached_proc_macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ajxgl0w9vm55dk47qb0cq1akzncrwqcy78y37idq41dxm2s2hig"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling" ,rust-darling-0.20)
       ("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-2))))
    (home-page "None")
    (synopsis "Generic cache implementations and simplified function memoization")
    (description
      (beautify-description "Generic cache implementations and simplified function memoization"))
    (license (list license:expat))))

(define-public rust-cached_proc_macro_types_0_1_1
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

(define-public rust-pq-sys_0_6_3
  (package
    (name "rust-pq-sys")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pq-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14w71gm61khqiaraw88qj4j48r6hxplyx5ki263h284mxbbhbk7n"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "None")
    (synopsis "Auto-generated rust bindings for libpq")
    (description
      (beautify-description "Auto-generated rust bindings for libpq"))
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel_derives_2_2_3
  (package
    (name "rust-diesel_derives")
    (version "2.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_derives" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "191iw5ja7s1gjy9ymjvv91ghzbvs2fb5ca28lvr6pfp2a7gc7wp7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-diesel_table_macro_syntax" ,rust-diesel_table_macro_syntax_0_2_0)
       ("rust-dsl_auto_type" ,rust-dsl_auto_type_0_1_2)
       ("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-2))))
    (home-page "https://diesel.rs")
    (synopsis "You should not use this crate directly, it is internal to Diesel.")
    (description
      (beautify-description "You should not use this crate directly, it is internal to Diesel."))
    (license (list license:expat license:asl2.0))))

(define-public rust-mysqlclient-sys_0_4_2
  (package
    (name "rust-mysqlclient-sys")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mysqlclient-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w1qslisdfcp2vmfjiwdrvmw3v1f30z13619h3jwvi4qgc0rpfvb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-pkg-config" ,rust-pkg-config-0.3)
       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "None")
    (synopsis "Auto-generated rust bindings for libmysqlclient")
    (description
      (beautify-description "Auto-generated rust bindings for libmysqlclient"))
    (license (list license:expat license:asl2.0))))

(define-public rust-diesel_table_macro_syntax_0_2_0
  (package
    (name "rust-diesel_table_macro_syntax")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diesel_table_macro_syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09gvkyljhchbxfkxlkkrdcqcmcxwsim9sfljqilbq4x485b77710"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-syn" ,rust-syn-2))))
    (home-page "https://diesel.rs")
    (synopsis "Internal diesel crate")
    (description
      (beautify-description "Internal diesel crate"))
    (license (list license:expat license:asl2.0))))

(define-public rust-dsl_auto_type_0_1_2
  (package
    (name "rust-dsl_auto_type")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dsl_auto_type" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01xng43pn2dlc5k422is20dapq14w9x1p46qq968c0s167kapnf5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-darling" ,rust-darling-0.20)
       ("rust-either" ,rust-either-1)
       ("rust-heck" ,rust-heck-0.5)
       ("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-2))))
    (home-page "https://diesel.rs")
    (synopsis "Automatically expand query fragment types for factoring as functions")
    (description
      (beautify-description "Automatically expand query fragment types for factoring as functions"))
    (license (list license:expat license:asl2.0))))

(define-public rust-cron_0_12_1
  (package
    (name "rust-cron")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cron" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01qc1cnhibxh55pwv3mwaxvfgbjpgk1lfl7an5m4ljvv0xrkx33g"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-chrono" ,rust-chrono-0.4)
       ("rust-nom" ,rust-nom-7)
       ("rust-once_cell" ,rust-once_cell_1_20_2))))
    (home-page "None")
    (synopsis "A cron expression parser and schedule explorer.")
    (description
      (beautify-description "A cron expression parser and schedule explorer."))
    (license (list license:expat license:asl2.0))))

(define-public rust-nonzero_ext_0_3_0
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

(define-public rust-quanta_0_12_4
  (package
    (name "rust-quanta")
    (version "0.12.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quanta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rngcl6ar7v5n8442dxxcpry28z2jkz6ylz31gr7xg5r1f6ycg3p"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
       ("rust-libc" ,rust-libc_0_2_168)
       ("rust-once_cell" ,rust-once_cell_1_20_2)
       ("rust-raw-cpuid" ,rust-raw-cpuid_11_2_0)
       ("rust-wasi" ,rust-wasi-0.11)
       ("rust-web-sys" ,rust-web-sys-0.3)
       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description
      (beautify-description "high-speed timing library"))
    (license (list license:expat))))

(define-public rust-parking_lot_0_12_3
  (package
    (name "rust-parking_lot")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lock_api" ,rust-lock_api_0_4_12)
       ("rust-parking_lot_core" ,rust-parking_lot_core_0_9_10))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license (list license:expat license:asl2.0))))

(define-public rust-portable-atomic_1_10_0
  (package
    (name "rust-portable-atomic")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "portable-atomic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rjfim62djiakf5rcq3r526hac0d1dd9hwa1jmiin7q7ad2c4398"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Portable atomic types including support for 128-bit atomics, atomic float, etc.")
    (description
      (beautify-description "Portable atomic types including support for 128-bit atomics, atomic float, etc."))
    (license (list license:asl2.0 license:expat))))

(define-public rust-spinning_top_0_3_0
  (package
    (name "rust-spinning_top")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spinning_top" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "001kjbiz1gg111rsqxc4pq9a1izx7wshkk38f69h1dbgf4fjsvfr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lock_api" ,rust-lock_api_0_4_12))))
    (home-page "None")
    (synopsis "A simple spinlock crate based on the abstractions provided by `lock_api`.")
    (description
      (beautify-description "A simple spinlock crate based on the abstractions provided by `lock_api`."))
    (license (list license:expat license:asl2.0))))

(define-public rust-raw-cpuid_11_2_0
  (package
    (name "rust-raw-cpuid")
    (version "11.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "raw-cpuid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c77cmsn7rj6knwwrg2y9nl46wss5p9jq3wzxvr1a5k6bhql1chs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags-2))))
    (home-page "https://github.com/gz/rust-cpuid")
    (synopsis "A library to parse the x86 CPUID instruction, written in rust with no external dependencies. The implementation closely resembles the Intel CPUID manual description. The library does only depend on libcore.")
    (description
      (beautify-description "A library to parse the x86 CPUID instruction, written in rust with no external dependencies. The implementation closely resembles the Intel CPUID manual description. The library does only depend on libcore."))
    (license (list license:expat))))

(define-public rust-parking_lot_core_0_9_10
  (package
    (name "rust-parking_lot_core")
    (version "0.9.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-libc" ,rust-libc_0_2_168)
       ("rust-redox_syscall" ,rust-redox_syscall_0_5_8)
       ("rust-smallvec" ,rust-smallvec-1)
       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license (list license:expat license:asl2.0))))

(define-public rust-lock_api_0_4_12
  (package
    (name "rust-lock_api")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg-1)
       ("rust-scopeguard" ,rust-scopeguard-1))))
    (home-page "None")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      (beautify-description "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std."))
    (license (list license:expat license:asl2.0))))

(define-public rust-redox_syscall_0_5_8
  (package
    (name "rust-redox_syscall")
    (version "0.5.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0d48ylyd6gsamynyp257p6n2zl4dw2fhnn5z9y3nhgpri6rn5a03"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags-2))))
    (home-page "None")
    (synopsis "A Rust library to access raw Redox system calls")
    (description
      (beautify-description "A Rust library to access raw Redox system calls"))
    (license (list license:expat))))

(define-public rust-migrations_internals_2_2_0
  (package
    (name "rust-migrations_internals")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "migrations_internals" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zw0lf2lw3wlmyb0kv68cnr3ya2n80svpavf0jcqfbz8a6c060gx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde-1)
       ("rust-toml" ,rust-toml-0.8))))
    (home-page "https://diesel.rs")
    (synopsis "Internal implementation of diesels migration mechanism")
    (description
      (beautify-description "Internal implementation of diesels migration mechanism"))
    (license (list license:expat license:asl2.0))))

(define-public rust-migrations_macros_2_2_0
  (package
    (name "rust-migrations_macros")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "migrations_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z9p2ag0fnnh0m7z8qfncwyjc0pgschca7vzlixb6v0pfb663cgz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-migrations_internals" ,rust-migrations_internals_2_2_0)
       ("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1))))
    (home-page "https://diesel.rs")
    (synopsis "Codegeneration macros for diesels embedded migrations")
    (description
      (beautify-description "Codegeneration macros for diesels embedded migrations"))
    (license (list license:expat license:asl2.0))))

(define-public rust-time-macros_0_2_19
  (package
    (name "rust-time-macros")
    (version "0.2.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pl558z26pp342l5y91n6dxb60xwhar975wk6jc4npiygq0ycd18"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-conv" ,rust-num-conv-0.1)
       ("rust-time-core" ,rust-time-core-0.1))))
    (home-page "None")
    (synopsis "Procedural macros for the time crate.\n    This crate is an implementation detail and should not be relied upon directly.")
    (description
      (beautify-description "Procedural macros for the time crate.\n    This crate is an implementation detail and should not be relied upon directly."))
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
      (("rust-libc" ,rust-libc_0_2_168))))
    (home-page "None")
    (synopsis "A minimal library that determines the number of running threads for the current process.")
    (description
      (beautify-description "A minimal library that determines the number of running threads for the current process."))
    (license (list license:expat license:asl2.0))))

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

(define-public rust-serde_derive_1_0_216
  (package
    (name "rust-serde_derive")
    (version "1.0.216")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pm5bm4354n40ir12bbs829arlqwjrw0wmzd4xk5r1kkpzdmky26"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license (list license:expat license:asl2.0))))

(define-public rust-publicsuffix_2_3_0
  (package
    (name "rust-publicsuffix")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "publicsuffix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pzsm61x49zgdcb14b5fnmjj36k129gc37lpfrgk6q5bdi2flhkg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-idna" ,rust-idna-1)
       ("rust-psl-types" ,rust-psl-types-2))))
    (home-page "None")
    (synopsis "Extract root domain and suffix from a domain name")
    (description
      (beautify-description "Extract root domain and suffix from a domain name"))
    (license (list license:expat license:asl2.0))))

(define-public rust-serde_cbor_0_11_2
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
       ("rust-serde" ,rust-serde-1))))
    (home-page "None")
    (synopsis "CBOR support for serde.")
    (description
      (beautify-description "CBOR support for serde."))
    (license (list license:expat license:asl2.0))))

(define-public rust-lasso_0_7_3
  (package
    (name "rust-lasso")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lasso" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yz92fy2zv6wslfwwf3j7lw1wxja8d91rrcwgfzv751l1ajys53f"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hashbrown" ,rust-hashbrown-0.14))))
    (home-page "None")
    (synopsis "A multithreaded and single threaded string interner that allows strings to be cached with a\nminimal memory footprint, associating them with a unique key that can be used to retrieve them at any time.")
    (description
      (beautify-description "A multithreaded and single threaded string interner that allows strings to be cached with a\nminimal memory footprint, associating them with a unique key that can be used to retrieve them at any time."))
    (license (list license:expat license:asl2.0))))

(define-public rust-codemap_0_1_3
  (package
    (name "rust-codemap")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "codemap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "091azkslwkcijj3lp9ymb084y9a0wm4fkil7m613ja68r2snkrxr"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A data structure for efficiently storing source code position and span information\n(e.g. in a compiler AST), and mapping it back to file/line/column locations for error\nreporting and suggestions.")
    (description
      (beautify-description "A data structure for efficiently storing source code position and span information\n(e.g. in a compiler AST), and mapping it back to file/line/column locations for error\nreporting and suggestions."))
    (license (list license:expat license:asl2.0))))

(define-public rust-hickory-proto_0_24_2
  (package
    (name "rust-hickory-proto")
    (version "0.24.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hickory-proto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1df1gg333sgjicmf8nbsxlhffp18kdfwcvdgaald1fdgp36zsyj4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-async-trait" ,rust-async-trait-0.1)
       ("rust-cfg-if" ,rust-cfg-if-1)
       ("rust-data-encoding" ,rust-data-encoding-2)
       ("rust-enum-as-inner" ,rust-enum-as-inner-0.6)
       ("rust-futures-channel" ,rust-futures-channel-0.3)
       ("rust-futures-io" ,rust-futures-io-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-idna" ,rust-idna-1)
       ("rust-ipnet" ,rust-ipnet-2)
       ("rust-once_cell" ,rust-once_cell_1_20_2)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-thiserror" ,rust-thiserror-1)
       ("rust-tinyvec" ,rust-tinyvec_1_8_0)
       ("rust-tokio" ,rust-tokio-1)
       ("rust-tracing" ,rust-tracing-0.1)
       ("rust-url" ,rust-url-2))))
    (home-page "https://hickory-dns.org/")
    (synopsis "Hickory DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Hickory DNS projects.")
    (description
      (beautify-description "Hickory DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Hickory DNS projects."))
    (license (list license:expat license:asl2.0))))

(define-public rust-tinyvec_1_8_0
  (package
    (name "rust-tinyvec")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tinyvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f5rf6a2wzyv6w4jmfga9iw7rp9fp5gf4d604xgjsf3d9wgqhpj4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-tinyvec_macros" ,rust-tinyvec_macros_0_1_1))))
    (home-page "None")
    (synopsis "`tinyvec` provides 100% safe vec-like data structures.")
    (description
      (beautify-description "`tinyvec` provides 100% safe vec-like data structures."))
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-tinyvec_macros_0_1_1
  (package
    (name "rust-tinyvec_macros")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tinyvec_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Some macros for tiny containers")
    (description
      (beautify-description "Some macros for tiny containers"))
    (license (list license:expat license:asl2.0 license:zlib))))

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
      (("rust-libc" ,rust-libc_0_2_168)
       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "None")
    (synopsis "Utility functions for other crates, no backwards compatibility guarantees.")
    (description
      (beautify-description "Utility functions for other crates, no backwards compatibility guarantees."))
    (license (list license:asl2.0))))

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

(define-public rust-version_check_0_9_5
  (package
    (name "rust-version_check")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Tiny crate to check the version of the installed/running rustc.")
    (description
      (beautify-description "Tiny crate to check the version of the installed/running rustc."))
    (license (list license:expat license:asl2.0))))

vaultwarden
