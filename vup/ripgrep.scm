(define-module (vup ripgrep)
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

(define rust-base64_0_10_1
  (package
    (name "rust-base64")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13k6bvd3n6dm7jqn9x918w65dd9xhx454bqphbnv0bkd6n9dj98b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder_1_3_2))))
    (home-page "None")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description
      (beautify-description "encodes and decodes base64 as bytes or utf8"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define rust-bstr_0_2_7
  (package
    (name "rust-bstr")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bstr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1plsr948padkqi42m502r67m82n7iyzjmgjx3xn5ck79ny7ggkcl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-memchr" ,rust-memchr_2_2_1)        
       ("rust-regex-automata" ,rust-regex-automata_0_1_8))))
    (home-page "https://github.com/BurntSushi/bstr")
    (synopsis "A string type that is not required to be valid UTF-8.")
    (description
      (beautify-description "A string type that is not required to be valid UTF-8."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-bytecount_0_5_1
  (package
    (name "rust-bytecount")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytecount" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z6a280kiy4kg5v3qw97pbyvwycr17fsm41804i8zpq7nmads3xy"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
    (description
      (beautify-description "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

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

(define rust-clap_2_33_0
  (package
    (name "rust-clap")
    (version "2.33.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nf6ld3bims1n5vfzhkvcb55pdzh04bbhzf8nil5vvw05nxzarsh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-strsim" ,rust-strsim_0_8_0)        
       ("rust-textwrap" ,rust-textwrap_0_11_0)        
       ("rust-unicode-width" ,rust-unicode-width_0_1_6))))
    (home-page "https://clap.rs/")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (spdx-string->license "MIT"))))

(define rust-crossbeam-channel_0_3_9
  (package
    (name "rust-crossbeam-channel")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-channel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ylyzb1m9qbvd1nd3vy38x9073wdmcy295ncjs7wf7ap476pzv68"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-utils" ,rust-crossbeam-utils_0_6_6))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-channel")
    (synopsis "Multi-producer multi-consumer channels for message passing")
    (description
      (beautify-description "Multi-producer multi-consumer channels for message passing"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0 AND BSD-2-Clause")))))

(define rust-crossbeam-utils_0_6_6
  (package
    (name "rust-crossbeam-utils")
    (version "0.6.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rk0r9n04bmq4a3g2q5qhvvlmrmx780gc6h9lmc94mwndslkz5q4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
      (beautify-description "Utilities for concurrent programming"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-encoding_rs_0_8_17
  (package
    (name "rust-encoding_rs")
    (version "0.8.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v902qqnbd37vdq4rjvp6k05wmghrasfdcjy30gp1xpjg5f7hma1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-packed_simd" ,rust-packed_simd_0_3_3))))
    (home-page "https://docs.rs/encoding_rs/")
    (synopsis "A Gecko-oriented implementation of the Encoding Standard")
    (description
      (beautify-description "A Gecko-oriented implementation of the Encoding Standard"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-encoding_rs_io_0_1_6
  (package
    (name "rust-encoding_rs_io")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_rs_io" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b7k9p7inkrcanh7h6q4m278y05gmcwi8p5r43h7grzl5dxfw6cn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-encoding_rs" ,rust-encoding_rs_0_8_17))))
    (home-page "None")
    (synopsis "Streaming transcoding for encoding_rs")
    (description
      (beautify-description "Streaming transcoding for encoding_rs"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-fnv_1_0_6
  (package
    (name "rust-fnv")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fnv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ww56bi1r5b8id3ns9j3qxbi7w5h005rzhiryy0zi9h97raqbb9g"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fowler–Noll–Vo hash function")
    (description
      (beautify-description "Fowler–Noll–Vo hash function"))
    (license `((spdx-string->license "Apache-2.0 ")
               (spdx-string->license " MIT")))))

(define rust-fs_extra_1_1_0
  (package
    (name "rust-fs_extra")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs_extra" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x6675wdhsx277k1k1235jwcv38naf20d8kwrk948ds26hh4lajz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/webdesus/fs_extra")
    (synopsis "Expanding opportunities standard library std::fs and std::io. Recursively copy folders with recept information about process and much more.")
    (description
      (beautify-description "Expanding opportunities standard library std::fs and std::io. Recursively copy folders with recept information about process and much more."))
    (license (spdx-string->license "MIT"))))

(define rust-globset_0_4_4
  (package
    (name "rust-globset")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "globset" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wnqxq91liknmr2w93wjq2spyxbrd1pmnhd4nbi3921dr35a4nlj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick_0_7_6)        
       ("rust-bstr" ,rust-bstr_0_2_7)        
       ("rust-fnv" ,rust-fnv_1_0_6)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-regex" ,rust-regex_1_2_1))))
    (home-page "https://github.com/BurntSushi/ripgrep/tree/master/globset")
    (synopsis "Cross platform single glob and glob set matching. Glob set matching is the
process of matching one or more glob patterns against a single candidate path
simultaneously, and returning all of the globs that matched.")
    (description
      (beautify-description "Cross platform single glob and glob set matching. Glob set matching is the
process of matching one or more glob patterns against a single candidate path
simultaneously, and returning all of the globs that matched."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-grep_0_2_4
  (package
    (name "rust-grep")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grep" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pkhjladybzzciwg0mjk3vjz5fyi76hk0d3hgyzv2jxlyp8v4fyc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-grep-cli" ,rust-grep-cli_0_1_3)        
       ("rust-grep-matcher" ,rust-grep-matcher_0_1_3)        
       ("rust-grep-pcre2" ,rust-grep-pcre2_0_1_3)        
       ("rust-grep-printer" ,rust-grep-printer_0_1_3)        
       ("rust-grep-regex" ,rust-grep-regex_0_1_5)        
       ("rust-grep-searcher" ,rust-grep-searcher_0_1_6))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Fast line oriented regex searching as a library.")
    (description
      (beautify-description "Fast line oriented regex searching as a library."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-grep-cli_0_1_3
  (package
    (name "rust-grep-cli")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grep-cli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05a502x5m4fijwx7zj9icxna2dx86scm76ap80zr89pnvpbfk1hp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atty" ,rust-atty_0_2_13)        
       ("rust-bstr" ,rust-bstr_0_2_7)        
       ("rust-globset" ,rust-globset_0_4_4)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-regex" ,rust-regex_1_2_1)        
       ("rust-same-file" ,rust-same-file_1_0_5)        
       ("rust-termcolor" ,rust-termcolor_1_0_5)        
       ("rust-winapi-util" ,rust-winapi-util_0_1_2))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Utilities for search oriented command line applications.")
    (description
      (beautify-description "Utilities for search oriented command line applications."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-grep-matcher_0_1_3
  (package
    (name "rust-grep-matcher")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grep-matcher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "113lafx3abrr96ahpz6yn905ian1w3qsr5hijbb909p2j0xgmhkm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_2_1))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "A trait for regular expressions, with a focus on line oriented search.")
    (description
      (beautify-description "A trait for regular expressions, with a focus on line oriented search."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-grep-pcre2_0_1_3
  (package
    (name "rust-grep-pcre2")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grep-pcre2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wjc3gsan20gapga8nji6jcrmwn9n85q5zf2yfq6g50c7abkc2ql"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-grep-matcher" ,rust-grep-matcher_0_1_3)        
       ("rust-pcre2" ,rust-pcre2_0_2_1))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Use PCRE2 with the 'grep' crate.")
    (description
      (beautify-description "Use PCRE2 with the 'grep' crate."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-grep-printer_0_1_3
  (package
    (name "rust-grep-printer")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grep-printer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mxc1yx5sx89f00imlm5d3hxwdgglv9rzwdki8ba50gvq8a2nr8m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_10_1)        
       ("rust-bstr" ,rust-bstr_0_2_7)        
       ("rust-grep-matcher" ,rust-grep-matcher_0_1_3)        
       ("rust-grep-searcher" ,rust-grep-searcher_0_1_6)        
       ("rust-serde" ,rust-serde_1_0_99)        
       ("rust-serde_derive" ,rust-serde_derive_1_0_99)        
       ("rust-serde_json" ,rust-serde_json_1_0_40)        
       ("rust-termcolor" ,rust-termcolor_1_0_5))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "An implementation of the grep crate's Sink trait that provides standard
printing of search results, similar to grep itself.")
    (description
      (beautify-description "An implementation of the grep crate's Sink trait that provides standard
printing of search results, similar to grep itself."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-grep-regex_0_1_5
  (package
    (name "rust-grep-regex")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grep-regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0afl67ikb42phn6fryxv2mmj97zb75llynr92fgzhin879c4vmm0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick_0_7_6)        
       ("rust-grep-matcher" ,rust-grep-matcher_0_1_3)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-regex" ,rust-regex_1_2_1)        
       ("rust-regex-syntax" ,rust-regex-syntax_0_6_11)        
       ("rust-thread_local" ,rust-thread_local_0_3_6))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Use Rust's regex library with the 'grep' crate.")
    (description
      (beautify-description "Use Rust's regex library with the 'grep' crate."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-grep-searcher_0_1_6
  (package
    (name "rust-grep-searcher")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "grep-searcher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09ag16im12v6k0lzkyvbvamn1iw15kfx1jbfldb7z5xa7208l04a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bstr" ,rust-bstr_0_2_7)        
       ("rust-bytecount" ,rust-bytecount_0_5_1)        
       ("rust-encoding_rs" ,rust-encoding_rs_0_8_17)        
       ("rust-encoding_rs_io" ,rust-encoding_rs_io_0_1_6)        
       ("rust-grep-matcher" ,rust-grep-matcher_0_1_3)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-memmap" ,rust-memmap_0_7_0))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Fast line oriented regex searching as a library.")
    (description
      (beautify-description "Fast line oriented regex searching as a library."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-ignore_0_4_10
  (package
    (name "rust-ignore")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ignore" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19w4iwq1f01v1wd9s5afg06hcnycqgrp70pgm9qxa2c44lr6ih8f"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-channel" ,rust-crossbeam-channel_0_3_9)        
       ("rust-globset" ,rust-globset_0_4_4)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-memchr" ,rust-memchr_2_2_1)        
       ("rust-regex" ,rust-regex_1_2_1)        
       ("rust-same-file" ,rust-same-file_1_0_5)        
       ("rust-thread_local" ,rust-thread_local_0_3_6)        
       ("rust-walkdir" ,rust-walkdir_2_2_9)        
       ("rust-winapi-util" ,rust-winapi-util_0_1_2))))
    (home-page "https://github.com/BurntSushi/ripgrep/tree/master/ignore")
    (synopsis "A fast library for efficiently matching ignore files such as `.gitignore`
against file paths.")
    (description
      (beautify-description "A fast library for efficiently matching ignore files such as `.gitignore`
against file paths."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-itoa_0_4_4
  (package
    (name "rust-itoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zvg2d9qv3avhf3d8ggglh6fdyw8kkwqg3r4622ly5yhxnvnc4jh"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast functions for printing integer primitives to an io::Write")
    (description
      (beautify-description "Fast functions for printing integer primitives to an io::Write"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-jemalloc-sys_0_3_2
  (package
    (name "rust-jemalloc-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jemalloc-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ify9vlql01qhfxlj7d4p9jvcp90mj2h69nkbq7slccvbhzryfqd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-fs_extra" ,rust-fs_extra_1_1_0)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis "Rust FFI bindings to jemalloc")
    (description
      (beautify-description "Rust FFI bindings to jemalloc"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-jemallocator_0_3_2
  (package
    (name "rust-jemallocator")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jemallocator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sabfa5118b7l4ars5n36s2fjyfn59w4d6mjs6rrmsa5zky67bj3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-jemalloc-sys" ,rust-jemalloc-sys_0_3_2)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis "A Rust allocator backed by jemalloc")
    (description
      (beautify-description "A Rust allocator backed by jemalloc"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define rust-memmap_0_7_0
  (package
    (name "rust-memmap")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ns7kkd1h4pijdkwfvw4qlbbmqmlmzwlq3g2676dcl5vwyazv1b5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "Cross-platform Rust API for memory-mapped file IO")
    (description
      (beautify-description "Cross-platform Rust API for memory-mapped file IO"))
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

(define rust-packed_simd_0_3_3
  (package
    (name "rust-packed_simd")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "packed_simd" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0822wqf6kzw4ig9ykndg348w2bxkhs3x64brzsvdxh2a1pyajpm8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9))))
    (home-page "https://github.com/rust-lang-nursery/packed_simd")
    (synopsis "Portable Packed SIMD vectors")
    (description
      (beautify-description "Portable Packed SIMD vectors"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-pcre2_0_2_1
  (package
    (name "rust-pcre2")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pcre2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "103i66a998g1fjrqf9sdyvi8qi83hwglz3pjdcq9n2r207hsagb0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-pcre2-sys" ,rust-pcre2-sys_0_2_2)        
       ("rust-thread_local" ,rust-thread_local_0_3_6))))
    (home-page "https://github.com/BurntSushi/rust-pcre2")
    (synopsis "High level wrapper library for PCRE2.")
    (description
      (beautify-description "High level wrapper library for PCRE2."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-pcre2-sys_0_2_2
  (package
    (name "rust-pcre2-sys")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pcre2-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nwdvc43dkb89qmm5q8gw1zyll0wsfqw7kczpn23mljra3874v47"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15))))
    (home-page "https://github.com/BurntSushi/rust-pcre2")
    (synopsis "Low level bindings to PCRE2.")
    (description
      (beautify-description "Low level bindings to PCRE2."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

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

(define rust-proc-macro2_1_0_1
  (package
    (name "rust-proc-macro2")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00n8db56q9m6qfcla8ddxi6m9qvmjygbx63nwnm6z1w8ms026p2c"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-xid" ,rust-unicode-xid_0_2_0))))
    (home-page "https://github.com/alexcrichton/proc-macro2")
    (synopsis "A stable implementation of the upcoming new `proc_macro` API. Comes with an
option, off by default, to also reimplement itself in terms of the upstream
unstable API.")
    (description
      (beautify-description "A stable implementation of the upcoming new `proc_macro` API. Comes with an
option, off by default, to also reimplement itself in terms of the upstream
unstable API."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-quote_1_0_2
  (package
    (name "rust-quote")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zkc46ryacf2jdkc6krsy2z615xbk1x8kp1830rcxz3irj5qqfh5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_1))))
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
      (beautify-description "Quasi-quoting macro quote!(...)"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

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

(define rust-regex-automata_0_1_8
  (package
    (name "rust-regex-automata")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-automata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y89vkwd9z7797lsdsizvhw4lw7i1mhfx97a8315bhkh2wm3rdwj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder_1_3_2))))
    (home-page "https://github.com/BurntSushi/regex-automata")
    (synopsis "Automata construction and matching using regular expressions.")
    (description
      (beautify-description "Automata construction and matching using regular expressions."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

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

(define-public rust-ripgrep_11_0_2
  (package
    (name "rust-ripgrep")
    (version "11.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ripgrep" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vqjr96s2rs45715hzf0g0wjahig4zjyiqfijmzzg4jyh9ni80yr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bstr" ,rust-bstr_0_2_7)        
       ("rust-clap" ,rust-clap_2_33_0)        
       ("rust-grep" ,rust-grep_0_2_4)        
       ("rust-ignore" ,rust-ignore_0_4_10)        
       ("rust-jemallocator" ,rust-jemallocator_0_3_2)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-num_cpus" ,rust-num_cpus_1_10_1)        
       ("rust-regex" ,rust-regex_1_2_1)        
       ("rust-serde_json" ,rust-serde_json_1_0_40)        
       ("rust-termcolor" ,rust-termcolor_1_0_5))
      #:cargo-development-inputs
      (("rust-serde" ,rust-serde_1_0_99)        
       ("rust-serde_derive" ,rust-serde_derive_1_0_99))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "ripgrep is a line-oriented search tool that recursively searches your current
directory for a regex pattern while respecting your gitignore rules. ripgrep
has first class support on Windows, macOS and Linux.")
    (description
      (beautify-description "ripgrep is a line-oriented search tool that recursively searches your current
directory for a regex pattern while respecting your gitignore rules. ripgrep
has first class support on Windows, macOS and Linux."))
    (license (spdx-string->license "Unlicense OR MIT"))))

(define rust-ryu_1_0_0
  (package
    (name "rust-ryu")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15r9z2wzgbj04pks4jz7y6wif5xqhf1wqkl2nd7qrvn08ys68969"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast floating point to string conversion")
    (description
      (beautify-description "Fast floating point to string conversion"))
    (license (spdx-string->license "Apache-2.0 OR BSL-1.0"))))

(define rust-same-file_1_0_5
  (package
    (name "rust-same-file")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "same-file" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08a4zy10pjindf2rah320s6shgswk13mqw7s61m8i1y1xpf8spjq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi-util" ,rust-winapi-util_0_1_2))))
    (home-page "https://github.com/BurntSushi/same-file")
    (synopsis "A simple crate for determining whether two file paths point to the same file.")
    (description
      (beautify-description "A simple crate for determining whether two file paths point to the same file."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-serde_1_0_99
  (package
    (name "rust-serde")
    (version "1.0.99")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17ysh94r1443sf4ixhb0mf5m5rkmxr9wm28vlb4hs0bdnlg8bhpy"))))
    (build-system cargo-build-system)
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
      (beautify-description "A generic serialization/deserialization framework"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-serde_derive_1_0_99
  (package
    (name "rust-serde_derive")
    (version "1.0.99")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09dll6gd5fnma52mgf71w4vl6br3l3x9hv11k3f0hsr0c66c2kfb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_1)        
       ("rust-quote" ,rust-quote_1_0_2)        
       ("rust-syn" ,rust-syn_1_0_5))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-serde_json_1_0_40
  (package
    (name "rust-serde_json")
    (version "1.0.40")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "010pa89zx07aqx1cwgw2a603wcp3q5n2iy0k71ppqbr8kwi4j705"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-itoa" ,rust-itoa_0_4_4)        
       ("rust-ryu" ,rust-ryu_1_0_0)        
       ("rust-serde" ,rust-serde_1_0_99))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
      (beautify-description "A JSON serialization file format"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-strsim_0_8_0
  (package
    (name "rust-strsim")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Implementations of string similarity metrics.
Includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler.")
    (description
      (beautify-description "Implementations of string similarity metrics.
Includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler."))
    (license (spdx-string->license "MIT"))))

(define rust-syn_1_0_5
  (package
    (name "rust-syn")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gw03w7lzrlqmp2vislcybikgl5wkhrqi6sy70w93xss2abhx1b6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_1)        
       ("rust-quote" ,rust-quote_1_0_2)        
       ("rust-unicode-xid" ,rust-unicode-xid_0_2_0))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
      (beautify-description "Parser for Rust source code"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

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

(define rust-textwrap_0_11_0
  (package
    (name "rust-textwrap")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-width" ,rust-unicode-width_0_1_6))))
    (home-page "None")
    (synopsis "Textwrap is a small library for word wrapping, indenting, and
dedenting strings.

You can use it to format strings (such as help and error messages) for
display in commandline applications. It is designed to be efficient
and handle Unicode characters correctly.")
    (description
      (beautify-description "Textwrap is a small library for word wrapping, indenting, and
dedenting strings.

You can use it to format strings (such as help and error messages) for
display in commandline applications. It is designed to be efficient
and handle Unicode characters correctly."))
    (license (spdx-string->license "MIT"))))

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

(define rust-unicode-xid_0_2_0
  (package
    (name "rust-unicode-xid")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z09fn515xm7zyr0mmdyxa9mx2f7azcpv74pqmg611iralwpcvl2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine whether characters have the XID_Start
or XID_Continue properties according to
Unicode Standard Annex #31.")
    (description
      (beautify-description "Determine whether characters have the XID_Start
or XID_Continue properties according to
Unicode Standard Annex #31."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-walkdir_2_2_9
  (package
    (name "rust-walkdir")
    (version "2.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "walkdir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07ppalpvxkf8cnqr64np422792y4z5bs9m8b4nrflh5rm17wjn4n"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-same-file" ,rust-same-file_1_0_5)        
       ("rust-winapi" ,rust-winapi_0_3_8)        
       ("rust-winapi-util" ,rust-winapi-util_0_1_2))))
    (home-page "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory.")
    (description
      (beautify-description "Recursively walk a directory."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

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
