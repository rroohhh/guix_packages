(define-module (vup exa)
  #:use-module (guix build-system cargo)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix import utils) 
             #:select (beautify-description spdx-string->license)))

(define rust-aho-corasick_0_7_6
  (package
    (name "rust-aho-corasick")
    (version "0.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b8dh20fhdc59dhhnfi89n2bi80a8zbagzd5c122hf1vv2amxysq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_2_1))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      (beautify-description "Fast multiple substring searching."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-ansi_term_0_12_0
  (package
    (name "rust-ansi_term")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dmvziqx1j06xbv3zx62k7w81dyaqviag1rk5a0iynjqqdk2g9za"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles (bold, underline)")
    (description
      (beautify-description "Library for ANSI terminal colours and styles (bold, underline)"))
    (license (spdx-string->license "MIT"))))

(define rust-atty_0_2_13
  (package
    (name "rust-atty")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "140sswp1bwqwc4zk80bxkbnfb3g936hgrb77g9g0k1zcld3wc0qq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
      (beautify-description "A simple interface for querying atty"))
    (license (spdx-string->license "MIT"))))

(define rust-autocfg_0_1_6
  (package
    (name "rust-autocfg")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x8q946yy321rlpxhqf3mkd965x8kbjs2jwcw55dsmxlf7xwhwdn"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Automatic cfg for Rust compiler features")
    (description
      (beautify-description "Automatic cfg for Rust compiler features"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-bitflags_1_1_0
  (package
    (name "rust-bitflags")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zc1qb1hwsnl2d8rhzicsv9kqd5b2hwbrscrcfw5as4sfr35659x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
      (beautify-description "A macro to generate structures which behave like bitflags."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-byteorder_1_3_2
  (package
    (name "rust-byteorder")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byteorder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xbwjlmq2ziqjmjvkqxdx1yh136xxhilxd40bky1w4d7hn4xvhx7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/byteorder")
    (synopsis "Library for reading/writing numbers in big-endian and little-endian.")
    (description
      (beautify-description "Library for reading/writing numbers in big-endian and little-endian."))
    (license (spdx-string->license "Unlicense OR MIT"))))

(define rust-cc_1_0_41
  (package
    (name "rust-cc")
    (version "1.0.41")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zxzd559dbbf1iwdzmkj7czapzccs17kqqmsj9ayijpdix5rrbld"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis "A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.")
    (description
      (beautify-description "A build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-cfg-if_0_1_9
  (package
    (name "rust-cfg-if")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0csygklgz3ybpr0670rkip49zh76m43ar3k7xgypkzbzrwycx1ml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "A macro to ergonomically define an item depending on a large number of #[cfg]
parameters. Structured like an if-else chain, the first matching branch is the
item that gets emitted.")
    (description
      (beautify-description "A macro to ergonomically define an item depending on a large number of #[cfg]
parameters. Structured like an if-else chain, the first matching branch is the
item that gets emitted."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-datetime_0_4_7
  (package
    (name "rust-datetime")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "datetime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fd74bq48xg8ki5yw1mr1pa5hd3j5lbk4iqc5r0kh3l62b0vci2w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-iso8601" ,rust-iso8601_0_1_1)        
       ("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-locale" ,rust-locale_0_2_2)        
       ("rust-num-traits" ,rust-num-traits_0_1_43)        
       ("rust-pad" ,rust-pad_0_1_5)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56)        
       ("rust-winapi" ,rust-winapi_0_2_8))))
    (home-page "https://github.com/rust-datetime/datetime")
    (synopsis "Library for date and time formatting and arithmetic")
    (description
      (beautify-description "Library for date and time formatting and arithmetic"))
    (license (spdx-string->license "MIT"))))

(define rust-env_logger_0_6_2
  (package
    (name "rust-env_logger")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lx2s5nk96xx4i3m4zc4ghqgi8kb07dsnyiv8jk2clhax42dxz5a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atty" ,rust-atty_0_2_13)        
       ("rust-humantime" ,rust-humantime_1_2_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-regex" ,rust-regex_1_2_1)        
       ("rust-termcolor" ,rust-termcolor_1_0_5))))
    (home-page "None")
    (synopsis "A logging implementation for `log` which is configured via an environment
variable.")
    (description
      (beautify-description "A logging implementation for `log` which is configured via an environment
variable."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-exa_0_9_0
  (package
    (name "rust-exa")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "exa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s902xgplz1167k0r7x235p914lprpsqy2if0kpa1mlb0fswqqq4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:phases (modify-phases %standard-phases
                  (delete 'check))
      #:cargo-inputs
      (("rust-ansi_term" ,rust-ansi_term_0_12_0)        
       ("rust-datetime" ,rust-datetime_0_4_7)        
       ("rust-env_logger" ,rust-env_logger_0_6_2)        
       ("rust-git2" ,rust-git2_0_9_2)        
       ("rust-glob" ,rust-glob_0_3_0)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-locale" ,rust-locale_0_2_2)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-natord" ,rust-natord_1_0_9)        
       ("rust-num_cpus" ,rust-num_cpus_1_10_1)        
       ("rust-number_prefix" ,rust-number_prefix_0_3_0)        
       ("rust-scoped_threadpool" ,rust-scoped_threadpool_0_1_9)        
       ("rust-term_grid" ,rust-term_grid_0_1_7)        
       ("rust-term_size" ,rust-term_size_0_3_1)        
       ("rust-unicode-width" ,rust-unicode-width_0_1_6)        
       ("rust-users" ,rust-users_0_9_1)        
       ("rust-zoneinfo_compiled" ,rust-zoneinfo_compiled_0_4_8))))
    (home-page "https://the.exa.website/")
    (synopsis "A modern replacement for ls")
    (description
      (beautify-description "A modern replacement for ls"))
    (license (spdx-string->license "MIT"))))

(define rust-git2_0_9_2
  (package
    (name "rust-git2")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "git2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09lj6i26yial0drdbmfh36avz6wizaxqb0k41sqn2kca1qv01d4c"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-libgit2-sys" ,rust-libgit2-sys_0_8_2)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-openssl-sys" ,rust-openssl-sys_0_9_49)        
       ("rust-url" ,rust-url_2_1_0))))
    (home-page "None")
    (synopsis "Bindings to libgit2 for interoperating with git repositories. This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.")
    (description
      (beautify-description "Bindings to libgit2 for interoperating with git repositories. This library is
both threadsafe and memory safe and allows both reading and writing git
repositories."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-humantime_1_2_0
  (package
    (name "rust-humantime")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "humantime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "057ilhy6vc9iqhhby5ymh45m051pgxwq2z437gwkbnqhw7rfb9rw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-quick-error" ,rust-quick-error_1_2_2))))
    (home-page "https://github.com/tailhook/humantime")
    (synopsis "A parser and formatter for std::time::{Duration, SystemTime}")
    (description
      (beautify-description "A parser and formatter for std::time::{Duration, SystemTime}"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-idna_0_2_0
  (package
    (name "rust-idna")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "idna" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a9066imqpdrm1aavfasdyb1zahqaz8jmdcwdawvb1pf60y6gqh2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-matches" ,rust-matches_0_1_8)        
       ("rust-unicode-bidi" ,rust-unicode-bidi_0_3_4)        
       ("rust-unicode-normalization" ,rust-unicode-normalization_0_1_8))))
    (home-page "None")
    (synopsis "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (description
      (beautify-description "IDNA (Internationalizing Domain Names in Applications) and Punycode."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-iso8601_0_1_1
  (package
    (name "rust-iso8601")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iso8601" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xy48qyfmirslaj4dy6n4g8b564jap3cjiql35fmj5vgii7ldp0i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nom" ,rust-nom_1_2_4))))
    (home-page "None")
    (synopsis "Parsing ISO8601 dates using nom")
    (description
      (beautify-description "Parsing ISO8601 dates using nom"))
    (license (spdx-string->license "MIT"))))

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
    (license (spdx-string->license "MIT"))))

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
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-libc_0_2_62
  (package
    (name "rust-libc")
    (version "0.2.62")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fh69kpjg8hqff36kdczx7sax98gk4qs4ws1dwvjz0rgip0d5z1l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
      (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-libgit2-sys_0_8_2
  (package
    (name "rust-libgit2-sys")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y2mibmx7wy91s2kmb2gfb29mrqlqaxpy5wcwr8s1lwws7b9w5sc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-libz-sys" ,rust-libz-sys_1_0_25)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15))))
    (home-page "None")
    (synopsis "Native bindings to the libgit2 library")
    (description
      (beautify-description "Native bindings to the libgit2 library"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-libz-sys_1_0_25
  (package
    (name "rust-libz-sys")
    (version "1.0.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libz-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gjycyl2283525abks98bhxa4r259m617xfm5z52p3p3c8ry9d9f"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15)        
       ("rust-vcpkg" ,rust-vcpkg_0_2_7))))
    (home-page "None")
    (synopsis "Bindings to the system libz library (also known as zlib).")
    (description
      (beautify-description "Bindings to the system libz library (also known as zlib)."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-locale_0_2_2
  (package
    (name "rust-locale")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "locale" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z87wc7z6889x1pqlrwjw8f1crshzi15q5m102lqs8y0m69f9nsz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/ogham/rust-locale")
    (synopsis "Library for basic localisation. Warning: Major rewrite pending for 0.3!")
    (description
      (beautify-description "Library for basic localisation. Warning: Major rewrite pending for 0.3!"))
    (license (spdx-string->license "MIT"))))

(define rust-log_0_4_8
  (package
    (name "rust-log")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xz18ixccl5c6np4linv3ypc7hpmmgpc5zzd2ymp2ssfx0mhbdhl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9))))
    (home-page "None")
    (synopsis "A lightweight logging facade for Rust")
    (description
      (beautify-description "A lightweight logging facade for Rust"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-matches_0_1_8
  (package
    (name "rust-matches")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matches" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "020axl4q7rk9vz90phs7f8jas4imxal9y9kxl4z4v7a6719mrz3z"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A macro to evaluate, as a boolean, whether an expression matches a pattern.")
    (description
      (beautify-description "A macro to evaluate, as a boolean, whether an expression matches a pattern."))
    (license (spdx-string->license "MIT"))))

(define rust-memchr_2_2_1
  (package
    (name "rust-memchr")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13j6ji9x9ydpi9grbss106gqqr3xn3bcfp28aydqfa4751qrfmw8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description
      (beautify-description "Safe interface to memchr."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-natord_1_0_9
  (package
    (name "rust-natord")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "natord" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z75spwag3ch20841pvfwhh3892i2z2sli4pzp1jgizbipdrd39h"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/lifthrasiir/rust-natord")
    (synopsis "Natural ordering for Rust")
    (description
      (beautify-description "Natural ordering for Rust"))
    (license (spdx-string->license "MIT"))))

(define rust-nom_1_2_4
  (package
    (name "rust-nom")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kjh42w67z1hh1dw3jrilgqrf54jk2xcvhw4rcdm4wclzmbc5f55"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
      (beautify-description "A byte-oriented, zero-copy, parser combinators library"))
    (license (spdx-string->license "MIT"))))

(define rust-num-traits_0_1_43
  (package
    (name "rust-num-traits")
    (version "0.1.43")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-traits" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c9whknf2dm74a3cqirafy6gj83a76gl56g4v3g19k6lkwz13rcj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description
      (beautify-description "Numeric traits for generic mathematics"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-num-traits_0_2_8
  (package
    (name "rust-num-traits")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-traits" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0clvrm34rrqc8p6gq5ps5fcgws3kgq5knh7nlqxf2ayarwks9abb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6))))
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description
      (beautify-description "Numeric traits for generic mathematics"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-num_cpus_1_10_1
  (package
    (name "rust-num_cpus")
    (version "1.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_cpus" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wrj3zvj6h3q26sqj9zxpd59frjb54n7jhjwf307clq31ic47vxw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Get the number of CPUs on a machine.")
    (description
      (beautify-description "Get the number of CPUs on a machine."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-number_prefix_0_3_0
  (package
    (name "rust-number_prefix")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "number_prefix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0slm4mqmpgs6hvz22ycny9lvyvl9ivs80a1lncslp7lszz02zc0p"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Library for formatting numeric prefixes: kilo, giga, kibi.")
    (description
      (beautify-description "Library for formatting numeric prefixes: kilo, giga, kibi."))
    (license (spdx-string->license "MIT"))))

(define rust-openssl-src_111_5_0+1_1_1c
  (package
    (name "rust-openssl-src")
    (version "111.5.0+1.1.1c")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-src" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17h4jwa3n1i91h0q8g72c1d9xzm97bnkxn1s7rljyghp94zvzpjb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41))))
    (home-page "None")
    (synopsis "Source of OpenSSL and logic to build it.")
    (description
      (beautify-description "Source of OpenSSL and logic to build it."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-openssl-sys_0_9_49
  (package
    (name "rust-openssl-sys")
    (version "0.9.49")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1168vivyqbzaxl48bvv9r1x714c03f5c1za8pv5x8fyj9gjxkypl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6)        
       ("rust-cc" ,rust-cc_1_0_41)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-openssl-src" ,rust-openssl-src_111_5_0+1_1_1c)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15)        
       ("rust-vcpkg" ,rust-vcpkg_0_2_7))))
    (home-page "None")
    (synopsis "FFI bindings to OpenSSL")
    (description
      (beautify-description "FFI bindings to OpenSSL"))
    (license (spdx-string->license "MIT"))))

(define rust-pad_0_1_5
  (package
    (name "rust-pad")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pad" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fjnjn8464rcxjbpqgl5pdi91r5di6xpa79fwdri16356kiqv6ww"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-width" ,rust-unicode-width_0_1_6))))
    (home-page "https://github.com/ogham/rust-pad")
    (synopsis "Library for padding strings at runtime")
    (description
      (beautify-description "Library for padding strings at runtime"))
    (license (spdx-string->license "MIT"))))

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
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-pkg-config_0_3_15
  (package
    (name "rust-pkg-config")
    (version "0.3.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkg-config" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1byjfivxlpbh549scss9kp893pzwfig93w14bwxxn557lp7x5hd7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts.")
    (description
      (beautify-description "A library to run the pkg-config system tool at build time in order to be used in
Cargo build scripts."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-quick-error_1_2_2
  (package
    (name "rust-quick-error")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w6kgwwv7p7zr0yyg5rb315lkk24bimywklwx7fsvsbwi10bjx4j"))))
    (build-system cargo-build-system)
    (home-page "http://github.com/tailhook/quick-error")
    (synopsis "A macro which makes error types pleasant to write.")
    (description
      (beautify-description "A macro which makes error types pleasant to write."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-redox_syscall_0_1_56
  (package
    (name "rust-redox_syscall")
    (version "0.1.56")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "110y7dyfm2vci4x5vk7gr0q551dvp31npl99fnsx2fb17wzwcf94"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A Rust library to access raw Redox system calls")
    (description
      (beautify-description "A Rust library to access raw Redox system calls"))
    (license (spdx-string->license "MIT"))))

(define rust-regex_1_2_1
  (package
    (name "rust-regex")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09jww0faqvdprr9482ppxm1asbp6lhihr8zl9ma5sa4474cxkhw8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick_0_7_6)        
       ("rust-memchr" ,rust-memchr_2_2_1)        
       ("rust-regex-syntax" ,rust-regex-syntax_0_6_11)        
       ("rust-thread_local" ,rust-thread_local_0_3_6))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-regex-syntax_0_6_11
  (package
    (name "rust-regex-syntax")
    (version "0.6.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0grli4djafrkckh4ilvcw5z3wwqmasqzi3lqf5b6vrd55kmwqhxi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      (beautify-description "A regular expression parser."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-scoped_threadpool_0_1_9
  (package
    (name "rust-scoped_threadpool")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped_threadpool" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a26d3lk40s9mrf4imhbik7caahmw2jryhhb6vqv6fplbbgzal8x"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library for scoped and cached threadpools.")
    (description
      (beautify-description "A library for scoped and cached threadpools."))
    (license (spdx-string->license "MIT"))))

(define rust-smallvec_0_6_10
  (package
    (name "rust-smallvec")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dyl43rgzny79jjpgzi07y0ly2ggx1xwsn64csxj0j91bsf6lq5b"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "'Small vector' optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "'Small vector' optimization: store up to a small number of items on the stack"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-term_grid_0_1_7
  (package
    (name "rust-term_grid")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "term_grid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kq2sy3b8329jrsrpcvijvyz4gbqjyvyy6c3n0wmmvda9y03w393"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-width" ,rust-unicode-width_0_1_6))))
    (home-page "https://github.com/ogham/rust-term-grid")
    (synopsis "Library for formatting strings into a grid layout")
    (description
      (beautify-description "Library for formatting strings into a grid layout"))
    (license (spdx-string->license "MIT"))))

(define rust-term_size_0_3_1
  (package
    (name "rust-term_size")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "term_size" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09wk3173ngmb710qs9rwgibq4w250q8lgnwjvb9cypc1vdk9lnwy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-winapi" ,rust-winapi_0_2_8))))
    (home-page "None")
    (synopsis "functions for determining terminal sizes and dimensions")
    (description
      (beautify-description "functions for determining terminal sizes and dimensions"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-termcolor_1_0_5
  (package
    (name "rust-termcolor")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vjfsn1a8zvqhnrbygrz1id6yckwv1dncw3w4zj65qdx0f00kmln"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-wincolor" ,rust-wincolor_1_0_2))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis "A simple cross platform library for writing colored text to a terminal.")
    (description
      (beautify-description "A simple cross platform library for writing colored text to a terminal."))
    (license (spdx-string->license "Unlicense OR MIT"))))

(define rust-thread_local_0_3_6
  (package
    (name "rust-thread_local")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread_local" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06rzik99p8c5js8238yhc8rk6np543ylb1dy9nrw5v80j0r3xdf6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0))))
    (home-page "None")
    (synopsis "Per-object thread-local storage")
    (description
      (beautify-description "Per-object thread-local storage"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-unicode-bidi_0_3_4
  (package
    (name "rust-unicode-bidi")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-bidi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1malx8ljgm7v1gbaazkn7iicy5wj0bwcyadj3l727a38ch6bvwj9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-matches" ,rust-matches_0_1_8))))
    (home-page "None")
    (synopsis "Implementation of the Unicode Bidirectional Algorithm")
    (description
      (beautify-description "Implementation of the Unicode Bidirectional Algorithm"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-unicode-normalization_0_1_8
  (package
    (name "rust-unicode-normalization")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-normalization" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09i49va90rvia1agvgni4gicnqv50y5zy1naw8mr8bcqifh3j4ql"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-smallvec" ,rust-smallvec_0_6_10))))
    (home-page "https://github.com/unicode-rs/unicode-normalization")
    (synopsis "This crate provides functions for normalization of
Unicode strings, including Canonical and Compatible
Decomposition and Recomposition, as described in
Unicode Standard Annex #15.")
    (description
      (beautify-description "This crate provides functions for normalization of
Unicode strings, including Canonical and Compatible
Decomposition and Recomposition, as described in
Unicode Standard Annex #15."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-unicode-width_0_1_6
  (package
    (name "rust-unicode-width")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-width" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "082f9hv1r3gcd1xl33whjhrm18p0w9i77zhhhkiccb5r47adn1vh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width of `char` and `str` types
according to Unicode Standard Annex #11 rules.")
    (description
      (beautify-description "Determine displayed width of `char` and `str` types
according to Unicode Standard Annex #11 rules."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-url_2_1_0
  (package
    (name "rust-url")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qcxx9kr0wfawsr83h0kfgxl6dxw4cgrbgxsz7bpkj34qkv19d3m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-idna" ,rust-idna_0_2_0)        
       ("rust-matches" ,rust-matches_0_1_8)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_1_0))))
    (home-page "None")
    (synopsis "URL library for Rust, based on the WHATWG URL Standard")
    (description
      (beautify-description "URL library for Rust, based on the WHATWG URL Standard"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-users_0_9_1
  (package
    (name "rust-users")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "users" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kxl3y2hcrqqip7jpqn5mz7xlpbwmmpfmaza0xnyrhx0mrkl4by7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Library for getting information on Unix users and groups")
    (description
      (beautify-description "Library for getting information on Unix users and groups"))
    (license (spdx-string->license "MIT"))))

(define rust-vcpkg_0_2_7
  (package
    (name "rust-vcpkg")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vcpkg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15dzk1b96q946v9aisbd1bbhi33n93wvgziwh1shmscn1xflbp9k"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library to find native dependencies in a vcpkg tree at build
time in order to be used in Cargo build scripts.")
    (description
      (beautify-description "A library to find native dependencies in a vcpkg tree at build
time in order to be used in Cargo build scripts."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (license (spdx-string->license "MIT"))))

(define rust-winapi_0_3_8
  (package
    (name "rust-winapi")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ii9j9lzrhwri0902652awifzx9fpayimbp6hfhhc296xcg0k4w0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi-i686-pc-windows-gnu" ,rust-winapi-i686-pc-windows-gnu_0_4_0)        
       ("rust-winapi-x86_64-pc-windows-gnu" ,rust-winapi-x86_64-pc-windows-gnu_0_4_0))))
    (home-page "None")
    (synopsis "Raw FFI bindings for all of Windows API.")
    (description
      (beautify-description "Raw FFI bindings for all of Windows API."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (license (spdx-string->license "MIT"))))

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
    (synopsis "Import libraries for the i686-pc-windows-gnu target. Please don't use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the i686-pc-windows-gnu target. Please don't use this crate directly, depend on winapi instead."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-winapi-util_0_1_2
  (package
    (name "rust-winapi-util")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1j839dc6y8vszvrsb7yk0qvs0w6asnahxzbyans37vnsw6vbls3i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "A dumping ground for high level safe wrappers over winapi.")
    (description
      (beautify-description "A dumping ground for high level safe wrappers over winapi."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

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
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target. Please don't use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the x86_64-pc-windows-gnu target. Please don't use this crate directly, depend on winapi instead."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-wincolor_1_0_2
  (package
    (name "rust-wincolor")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wincolor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1agaf3hcav113i86912ajnw6jxcy4rvkrgyf8gdj8kc031mh3xcn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_8)        
       ("rust-winapi-util" ,rust-winapi-util_0_1_2))))
    (home-page "https://github.com/BurntSushi/termcolor/tree/master/wincolor")
    (synopsis "A simple Windows specific API for controlling text color in a Windows console.")
    (description
      (beautify-description "A simple Windows specific API for controlling text color in a Windows console."))
    (license (spdx-string->license "Unlicense OR MIT"))))

(define rust-zoneinfo_compiled_0_4_8
  (package
    (name "rust-zoneinfo_compiled")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zoneinfo_compiled" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bnm19w791q6kp79s0zl1cj9w51bw5xrifrxfy3g1p05i676y4vf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder_1_3_2)        
       ("rust-datetime" ,rust-datetime_0_4_7))))
    (home-page "https://github.com/rust-datetime/zoneinfo-compiled/")
    (synopsis "Library for parsing compiled zoneinfo files")
    (description
      (beautify-description "Library for parsing compiled zoneinfo files"))
    (license (spdx-string->license "MIT"))))
