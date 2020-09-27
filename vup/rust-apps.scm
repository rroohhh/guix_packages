(define-module (vup rust-apps)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix import utils) #:select (beautify-description spdx-string->license))
  #:use-module (gnu packages crates-io))

(define-public rust-autocfg_1_0_1
  (package
    (name "rust-autocfg")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jj6i9zn4gjl03kjvziqdji6rwx8ykz8zk2ngpc331z2g3fk3c6d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Automatic cfg for Rust compiler features")
    (description
      (beautify-description "Automatic cfg for Rust compiler features"))
    (license (spdx-string->license "Apache-2.0 OR MIT"))))

(define-public rust-bitflags_1_2_1
  (package
    (name "rust-bitflags")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14qnd5nq8p2almk79m4m8ydqhd413yaxsyjp5xd19g3mikzf47fg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.")
    (description
      (beautify-description "A macro to generate structures which behave like bitflags."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-cfg-if_0_1_10
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
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-chrono_0_4_18
  (package
    (name "rust-chrono")
    (version "0.4.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wjpipv40xfp40a9y0qcazc72m896hzsi96g18vk9rykggdzs8fh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_77)        
       ("rust-num-integer" ,rust-num-integer_0_1_43)        
       ("rust-num-traits" ,rust-num-traits_0_2_12)        
       ("rust-time" ,rust-time_0_1_44)        
       ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description
      (beautify-description "Date and time library for Rust"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-clap_2_33_3
  (package
    (name "rust-clap")
    (version "2.33.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00i065a58987k1sbzqmlz721rw521zcg08jmsh40gi3khp3qmr9p"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_2_1)        
       ("rust-textwrap" ,rust-textwrap_0_11_0)        
       ("rust-unicode-width" ,rust-unicode-width_0_1_8))))
    (home-page "https://clap.rs/")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (spdx-string->license "MIT"))))

(define-public rust-error-chain_0_11_0
  (package
    (name "rust-error-chain")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-chain" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wykkr0naizbkwxjwia1rch8xhwvgij9khqvjzs07mrmqifislgz"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Yet another error boilerplate library.")
    (description
      (beautify-description "Yet another error boilerplate library."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-fern_0_5_9
  (package
    (name "rust-fern")
    (version "0.5.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fern" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1anslk0hx9an4ypcaxqff080hgbcxm7ji7d4qf4f6qx1mkav16p6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-chrono" ,rust-chrono_0_4_18)        
       ("rust-log" ,rust-log_0_4_11))))
    (home-page "None")
    (synopsis "Simple, efficient logging")
    (description
      (beautify-description "Simple, efficient logging"))
    (license (spdx-string->license "MIT"))))

(define-public rust-float-cmp_0_3_0
  (package
    (name "rust-float-cmp")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "float-cmp" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c0hmj46xma5aysz0qb49padhc26aw875whx6q6rglsj5dqpds1b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num" ,rust-num_0_1_42))))
    (home-page "None")
    (synopsis "Floating point approximate comparison traits")
    (description
      (beautify-description "Floating point approximate comparison traits"))
    (license (spdx-string->license "MIT"))))

(define-public rust-libc_0_2_77
  (package
    (name "rust-libc")
    (version "0.2.77")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dc2z75prvi9vgg7djzy4nkb61vish01p5knis50hq15xh86pygj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
      (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-log_0_4_11
  (package
    (name "rust-log")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12xzqaflpiljn5cmxsbnbv9sjaj13ykhwsvll0gysbx4blbyvasg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_10))))
    (home-page "None")
    (synopsis "A lightweight logging facade for Rust")
    (description
      (beautify-description "A lightweight logging facade for Rust"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-num_0_1_42
  (package
    (name "rust-num")
    (version "0.1.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vhmyvfan380f86895z0f8rjscjc6qvwcmyvm15370ik2mjas0s7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-integer" ,rust-num-integer_0_1_43)        
       ("rust-num-iter" ,rust-num-iter_0_1_41)        
       ("rust-num-traits" ,rust-num-traits_0_2_12))))
    (home-page "https://github.com/rust-num/num")
    (synopsis "A collection of numeric types and traits for Rust, including bigint,\ncomplex, rational, range iterators, generic integers, and more!")
    (description
      (beautify-description "A collection of numeric types and traits for Rust, including bigint,\ncomplex, rational, range iterators, generic integers, and more!"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-num-integer_0_1_43
  (package
    (name "rust-num-integer")
    (version "0.1.43")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-integer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nw79ynfvw8br6yncv27pw65y2vw2z7m3kv9g2hinm1dcrz4ancd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_1_0_1)        
       ("rust-num-traits" ,rust-num-traits_0_2_12))))
    (home-page "https://github.com/rust-num/num-integer")
    (synopsis "Integer traits and functions")
    (description
      (beautify-description "Integer traits and functions"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-num-iter_0_1_41
  (package
    (name "rust-num-iter")
    (version "0.1.41")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-iter" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17sb142lhmpsq17cf9wrffjh8vjk901axxf55565r6cgfiy6nvks"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_1_0_1)        
       ("rust-num-integer" ,rust-num-integer_0_1_43)        
       ("rust-num-traits" ,rust-num-traits_0_2_12))))
    (home-page "https://github.com/rust-num/num-iter")
    (synopsis "External iterators for generic mathematics")
    (description
      (beautify-description "External iterators for generic mathematics"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-num-traits_0_2_12
  (package
    (name "rust-num-traits")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-traits" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04fnzwlnn6fcy09jjbi9l7bj5dvg657x5c2sjgwfb3pl0z67n9mc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_1_0_1))))
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description
      (beautify-description "Numeric traits for generic mathematics"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-phf_0_7_24
  (package
    (name "rust-phf")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "066xwv4dr6056a9adlkarwp4n94kbpwngbmd47ngm3cfbyw49nmk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-phf_shared" ,rust-phf_shared_0_7_24))))
    (home-page "None")
    (synopsis "Runtime support for perfect hash function data structures")
    (description
      (beautify-description "Runtime support for perfect hash function data structures"))
    (license (spdx-string->license "MIT"))))

(define-public rust-phf_shared_0_7_24
  (package
    (name "rust-phf_shared")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18371fla0vsj7d6d5rlfb747xbr2in11ar9vgv5qna72bnhp2kr3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-siphasher" ,rust-siphasher_0_2_3))))
    (home-page "None")
    (synopsis "Support code shared by PHF libraries")
    (description
      (beautify-description "Support code shared by PHF libraries"))
    (license (spdx-string->license "MIT"))))

(define-public rust-simplecss_0_1_0
  (package
    (name "rust-simplecss")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "simplecss" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03hc4qab1vilpvh7q219v1v4zacl7qj8mqinvxkl19l5g84qamhk"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A simple CSS 2 parser and selector.")
    (description
      (beautify-description "A simple CSS 2 parser and selector."))
    (license (spdx-string->license "MPL-2.0"))))

(define-public rust-siphasher_0_2_3
  (package
    (name "rust-siphasher")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "siphasher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b53m53l24lyhr505lwqzrpjyq5qfnic71mynrcfvm43rybf938b"))))
    (build-system cargo-build-system)
    (home-page "https://docs.rs/siphasher")
    (synopsis "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (description
      (beautify-description "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-svgcleaner_0_9_5
  (package
    (name "rust-svgcleaner")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "svgcleaner" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xxsr4bwyz9qxmvsizk4hswc8mnwv4y1v0x6x6s0f5444lmsi9jm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-clap" ,rust-clap_2_33_3)        
       ("rust-error-chain" ,rust-error-chain_0_11_0)        
       ("rust-fern" ,rust-fern_0_5_9)        
       ("rust-log" ,rust-log_0_4_11)        
       ("rust-svgdom" ,rust-svgdom_0_10_5))))
    (home-page "None")
    (synopsis "svgcleaner could help you to clean up your SVG files from the unnecessary data.")
    (description
      (beautify-description "svgcleaner could help you to clean up your SVG files from the unnecessary data."))
    (license (spdx-string->license "GPL-2.0"))))

(define-public rust-svgdom_0_10_5
  (package
    (name "rust-svgdom")
    (version "0.10.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "svgdom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10rfrdwdvzrysddf69x14xiv4in5w1zp7psvp5pjdd2is8sx5ifs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-error-chain" ,rust-error-chain_0_11_0)        
       ("rust-float-cmp" ,rust-float-cmp_0_3_0)        
       ("rust-log" ,rust-log_0_4_11)        
       ("rust-simplecss" ,rust-simplecss_0_1_0)        
       ("rust-svgparser" ,rust-svgparser_0_6_4))))
    (home-page "None")
    (synopsis "[DEPRECATED] Library to represent an SVG as a DOM.")
    (description
      (beautify-description "[DEPRECATED] Library to represent an SVG as a DOM."))
    (license (spdx-string->license "MPL-2.0"))))

(define-public rust-svgparser_0_6_4
  (package
    (name "rust-svgparser")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "svgparser" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jx30w022dqva6zia5aq6agmf1jf49dgndp4a33jcmfjyzas9clh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-error-chain" ,rust-error-chain_0_11_0)        
       ("rust-log" ,rust-log_0_4_11)        
       ("rust-phf" ,rust-phf_0_7_24)        
       ("rust-xmlparser" ,rust-xmlparser_0_1_2))))
    (home-page "None")
    (synopsis "Featureful, pull-based, zero-allocation SVG parser.")
    (description
      (beautify-description "Featureful, pull-based, zero-allocation SVG parser."))
    (license (spdx-string->license "MPL-2.0"))))

(define-public rust-textwrap_0_11_0
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
      (("rust-unicode-width" ,rust-unicode-width_0_1_8))))
    (home-page "None")
    (synopsis "Textwrap is a library for word wrapping, indenting, and dedenting\nstrings.\n\nYou can use it to format strings (such as help and error messages) for\ndisplay in commandline applications. It is designed to be efficient\nand handle Unicode characters correctly.")
    (description
      (beautify-description "Textwrap is a library for word wrapping, indenting, and dedenting\nstrings.\n\nYou can use it to format strings (such as help and error messages) for\ndisplay in commandline applications. It is designed to be efficient\nand handle Unicode characters correctly."))
    (license (spdx-string->license "MIT"))))

(define-public rust-time_0_1_44
  (package
    (name "rust-time")
    (version "0.1.44")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m9jwy2pcmk232r3b9r80fs12mkckfjffjha4qfaxcdq9a8ydfbd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_77)        
       ("rust-wasi" ,rust-wasi_0_10_0+wasi-snapshot-preview1))
      #:cargo-development-inputs
      (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
      (beautify-description "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-unicode-width_0_1_8
  (package
    (name "rust-unicode-width")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-width" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qxizyi6xbcqyi4z79p523ywvmgsfcgfqb3zv3c8i6x1jcc5jdwk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width of `char` and `str` types\naccording to Unicode Standard Annex #11 rules.")
    (description
      (beautify-description "Determine displayed width of `char` and `str` types\naccording to Unicode Standard Annex #11 rules."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-wasi_0_10_0+wasi-snapshot-preview1
  (package
    (name "rust-wasi")
    (version "0.10.0+wasi-snapshot-preview1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07y3l8mzfzzz4cj09c8y90yak4hpsi9g7pllyzpr6xvwrabka50s"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Experimental WASI API bindings for Rust")
    (description
      (beautify-description "Experimental WASI API bindings for Rust"))
    (license (spdx-string->license "Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT"))))

(define-public rust-winapi_0_3_9
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
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-winapi-i686-pc-windows-gnu_0_4_0
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
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-winapi-x86_64-pc-windows-gnu_0_4_0
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
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-xmlparser_0_1_2
  (package
    (name "rust-xmlparser")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xmlparser" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1anl2lz0xxkcbzydk3jcr2v1gfhvr14f7sjiwxam1kwgwyvqryx4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-error-chain" ,rust-error-chain_0_11_0)        
       ("rust-log" ,rust-log_0_4_11))))
    (home-page "None")
    (synopsis "Pull-based, zero-allocation XML parser.")
    (description
      (beautify-description "Pull-based, zero-allocation XML parser."))
    (license (spdx-string->license "MIT"))))
