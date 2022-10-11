(define-module (vup rust-apps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix import utils) #:select (beautify-description))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages tls)
  #:use-module (vup rust-nightly))

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
    (license license:asl2.0)))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license license:expat)))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license license:expat)))

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
    (license license:expat)))

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
    (license license:expat)))

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
    (license license:expat)))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license license:expat)))

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
    (license license:expat)))

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
    (license license:mpl2.0)))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license license:gpl2)))

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
    (license license:mpl2.0)))

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
    (license license:mpl2.0)))

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
    (license license:expat)))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license license:asl2.0)))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license `(license:expat
               license:asl2.0))))

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
    (license license:expat)))

(define-public rust-hacksaw
  (package
    (name "rust-hacksaw")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hacksaw" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xvcfx0mrbab3hs0lmcrh5grxdsr3y0awv2inwqgv3c35dj5278p"))))
    (inputs `(("python" ,python) ("libxcb" ,libxcb)))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-structopt" ,rust-structopt-0.3)
         ("rust-xcb" ,rust-xcb-0.9))))
    (home-page
      "https://github.com/neXromancers/hacksaw")
    (synopsis
      "Lightweight selection tool for usage in screenshot scripts etc.")
    (description
      "Lightweight selection tool for usage in screenshot scripts etc.")
    (license #f)))

(define-public rust-shotgun
  (package
    (name "rust-shotgun")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shotgun" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17jk6xy7fhj6iiz5rjsyprr8xhvn7w24l8fc6rxmrrwjjz5wjl5i"))))
    (inputs `(("git" ,git) ("pkg-config" ,pkg-config) ("libx11" ,libx11)
              ("libxrandr" ,libxrandr)))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getopts" ,rust-getopts-0.2)
         ("rust-image" ,rust-image-0.22)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-time" ,rust-time-0.1)
         ("rust-x11" ,rust-x11-2))))
    (home-page
      "https://github.com/neXromancers/shotgun")
    (synopsis "Minimal X screenshot utility")
    (description "Minimal X screenshot utility")
    (license license:mpl2.0)))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.79")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "158xd1swdvw6y59bx4avb8vdpj727n54r77xw5f7c15kqfjrz3cf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-indexmap" ,rust-indexmap-1)
         ("rust-itoa" ,rust-itoa-1)
         ("rust-ryu" ,rust-ryu-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.136")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1yb28smlymba4qbj2bn4c4myvblypqvkxv9q33s0dlzwa9qpwn88"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.136")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "12a791cbdd3gi08536i4frrqsps0ak8gvhpijvgj9rg1055y4cff"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
      "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-linebreak-0.1
  (package
    (name "rust-unicode-linebreak")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-linebreak" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0grq6bsn967q4vpifld53s7a140nlmpq5vy8ghgr73f4n2mdqlis"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/axelf4/unicode-linebreak")
    (synopsis "Implementation of the Unicode Line Breaking Algorithm")
    (description "Implementation of the Unicode Line Breaking Algorithm")
    (license license:asl2.0)))

(define-public rust-terminal-size-0.1
  (package
    (name "rust-terminal-size")
    (version "0.1.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "terminal_size" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1pq60ng1a7fjp597ifk1cqlz8fv9raz9xihddld1m1pfdia1lg33"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2) ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/eminence/terminal-size")
    (synopsis "Gets the size of your Linux or Windows terminal")
    (description "Gets the size of your Linux or Windows terminal")
    (license (list license:expat license:asl2.0))))

(define-public rust-openblas-src-0.9
  (package
    (name "rust-openblas-src")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openblas-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0syy38a5bgv5mj6mb1n1zk1d6l5gqqrswvbmwkwx6h4z9wfrsql4"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/blas-lapack-rs/openblas-src")
    (synopsis "The package provides a source of BLAS and LAPACK via OpenBLAS.")
    (description
      "The package provides a source of BLAS and LAPACK via OpenBLAS.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-netlib-src-0.8
  (package
    (name "rust-netlib-src")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "netlib-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "04l2ggdaq0bjc64prsw2f8ddxn84m1rmpnkjb9nr0ijdpcv1zx1r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-cmake" ,rust-cmake-0.1))))
    (home-page "https://github.com/blas-lapack-rs/netlib-src")
    (synopsis "The package provides a source of BLAS and LAPACK via Netlib.")
    (description
      "The package provides a source of BLAS and LAPACK via Netlib.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-intel-mkl-tool-0.1
  (package
    (name "rust-intel-mkl-tool")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "intel-mkl-tool" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1myyrxvmyij4c60w9x15npwzhlbjm8y8c94lvfsnrl5pbyakz8md"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-curl" ,rust-curl-0.4)
         ("rust-dirs" ,rust-dirs-2)
         ("rust-env-logger" ,rust-env-logger-0.7)
         ("rust-failure" ,rust-failure-0.1)
         ("rust-glob" ,rust-glob-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-structopt" ,rust-structopt-0.3)
         ("rust-tar" ,rust-tar-0.4)
         ("rust-zstd" ,rust-zstd-0.5))))
    (home-page "https://github.com/rust-math/intel-mkl-src")
    (synopsis "CLI utility for redistributiing Intel(R) MKL")
    (description "CLI utility for redistributiing Intel(R) MKL")
    (license license:expat)))

(define-public rust-intel-mkl-src-0.5
  (package
    (name "rust-intel-mkl-src")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "intel-mkl-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "155q49a7nfbq1lllchsyx8jv2q2pijrjh1w08awvrbjyfcxb6q3j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-intel-mkl-tool" ,rust-intel-mkl-tool-0.1))))
    (home-page "https://github.com/rust-math/intel-mkl-src")
    (synopsis "Redistribution of Intel(R) MKL as a crate")
    (description "Redistribution of Intel(R) MKL as a crate")
    (license #f)))

(define-public rust-accelerate-src-0.3
  (package
    (name "rust-accelerate-src")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "accelerate-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "17fiqyq7f9k41pbsyrvk9pxyx9z6fw399wq036cvwkbmb14xcpj1"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/blas-lapack-rs/accelerate-src")
    (synopsis
      "The package provides a source of BLAS and LAPACK via the Accelerate framework.")
    (description
      "The package provides a source of BLAS and LAPACK via the Accelerate framework.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-blas-src-0.6
  (package
    (name "rust-blas-src")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blas-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0a134wadi4rslfqk4mafi6y7bbvacjh12x87621w4vyc3dni6px2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-accelerate-src" ,rust-accelerate-src-0.3)
         ("rust-intel-mkl-src" ,rust-intel-mkl-src-0.5)
         ("rust-netlib-src" ,rust-netlib-src-0.8)
         ("rust-openblas-src" ,rust-openblas-src-0.9))))
    (home-page "https://github.com/blas-lapack-rs/blas-src")
    (synopsis "The package provides a BLAS source of choice.")
    (description "The package provides a BLAS source of choice.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ndarray-0.14
  (package
    (name "rust-ndarray")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndarray" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "011wqzmrd9gpfcfvy1xfbskqfiahn96pmi2d0r9x34d682amq3bc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-approx" ,rust-approx-0.4)
         ("rust-blas-src" ,rust-blas-src-0.6)
         ("rust-cblas-sys" ,rust-cblas-sys-0.1)
         ("rust-matrixmultiply" ,rust-matrixmultiply-0.2)
         ("rust-num-complex" ,rust-num-complex-0.3)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-rawpointer" ,rust-rawpointer-0.2)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
      "An n-dimensional array for general elements and for numerics. Lightweight array views and slicing; views support chunking and splitting.")
    (description
      "An n-dimensional array for general elements and for numerics.  Lightweight array
views and slicing; views support chunking and splitting.")
    (license (list license:expat license:asl2.0))))

(define-public rust-smawk-0.3
  (package
    (name "rust-smawk")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smawk" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0hv0q1mw1r1brk7v3g4a80j162p7g1dri4bdidykrakzfqjd4ypn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-ndarray" ,rust-ndarray-0.14))))
    (home-page "https://github.com/mgeisler/smawk")
    (synopsis "Functions for finding row-minima in a totally monotone matrix.")
    (description
      "This package provides functions for finding row-minima in a totally monotone
matrix.")
    (license license:expat)))

(define-public rust-hyphenation-commons-0.8
  (package
    (name "rust-hyphenation-commons")
    (version "0.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyphenation_commons" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1gq59h9h8597k04yl53an0j56cvb0in98pxpp27dkiz5mnifgssz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-fst" ,rust-fst-0.4) ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tapeinosyne/hyphenation")
    (synopsis "Proemial code for the `hyphenation` library")
    (description "Proemial code for the `hyphenation` library")
    (license (list license:asl2.0 license:expat))))

(define-public rust-fst-0.4
  (package
    (name "rust-fst")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fst" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "06mnksicgv9rp8b7w0ykkshf355l05zym3ygm74qr5z30ndmpf3s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-utf8-ranges" ,rust-utf8-ranges-1))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).
")
    (description
      "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).")
    (license (list license:unlicense license:expat))))

(define-public rust-hyphenation-0.8
  (package
    (name "rust-hyphenation")
    (version "0.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyphenation" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1w2hib167vpz7jbg3zs92ifihj4akirlhb5509aib1df8i6dvx5w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bincode" ,rust-bincode-1)
         ("rust-bincode" ,rust-bincode-1)
         ("rust-fst" ,rust-fst-0.4)
         ("rust-fst" ,rust-fst-0.4)
         ("rust-hyphenation-commons" ,rust-hyphenation-commons-0.8)
         ("rust-hyphenation-commons" ,rust-hyphenation-commons-0.8)
         ("rust-pocket-resources" ,rust-pocket-resources-0.3)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))))
    (home-page "https://github.com/tapeinosyne/hyphenation")
    (synopsis "Knuth-Liang hyphenation for a variety of languages")
    (description "Knuth-Liang hyphenation for a variety of languages")
    (license (list license:asl2.0 license:expat))))

(define-public rust-textwrap-0.15
  (package
    (name "rust-textwrap")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1yw513k61lfiwgqrfvsjw1a5wpvm0azhpjr2kr0jhnq9c56is55i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-hyphenation" ,rust-hyphenation-0.8)
         ("rust-smawk" ,rust-smawk-0.3)
         ("rust-terminal-size" ,rust-terminal-size-0.1)
         ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
         ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/mgeisler/textwrap")
    (synopsis
      "Powerful library for word wrapping, indenting, and dedenting strings")
    (description
      "Powerful library for word wrapping, indenting, and dedenting strings")
    (license license:expat)))

(define-public rust-uniquote-3
  (package
    (name "rust-uniquote")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uniquote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gqwq3kbzdsj5qsc8jfm5v4qwzgnp4rrfvdpm71ch1593h22y664"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/dylni/uniquote")
    (synopsis "Quote strings for clear display in output
")
    (description "Quote strings for clear display in output")
    (license (list license:expat license:asl2.0))))

(define-public rust-print-bytes-0.5
  (package
    (name "rust-print-bytes")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "print_bytes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0d4i9y3jx1chi6w97a8rgdbwm9g3cppr53rw53zl6fcaq31qx0b6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/dylni/print_bytes")
    (synopsis "Print bytes as losslessly as possible
")
    (description "Print bytes as losslessly as possible")
    (license (list license:expat license:asl2.0))))

(define-public rust-os-str-bytes-6
  (package
    (name "rust-os-str-bytes")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os_str_bytes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0r5z5xds2wzzqlqjaw96dpjsz5nqyzc1rflm4mh09aa32qyl88lf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-memchr" ,rust-memchr-2)
         ("rust-print-bytes" ,rust-print-bytes-0.5)
         ("rust-uniquote" ,rust-uniquote-3))))
    (home-page "https://github.com/dylni/os_str_bytes")
    (synopsis
      "Utilities for converting between byte sequences and platform-native strings
")
    (description
      "Utilities for converting between byte sequences and platform-native strings")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-3
  (package
    (name "rust-clap-derive")
    (version "3.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "05mz2y6k73wc1gvv9r4mllfqslzvlwkvx77lk7769ag1xlwd15fs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-heck" ,rust-heck-0.4)
         ("rust-proc-macro-error" ,rust-proc-macro-error-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
      "Parse command line argument by defining a struct, derive crate.")
    (description
      "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-3
  (package
    (name "rust-clap")
    (version "3.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "04lkhpmbzn3601scwrjqj2kxpggnf66zrmlqkqhw2469aln8klff"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-atty" ,rust-atty-0.2)
         ("rust-backtrace" ,rust-backtrace-0.3)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-clap-derive" ,rust-clap-derive-3)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-os-str-bytes" ,rust-os-str-bytes-6)
         ("rust-regex" ,rust-regex-1)
         ("rust-strsim" ,rust-strsim-0.10)
         ("rust-termcolor" ,rust-termcolor-1)
         ("rust-terminal-size" ,rust-terminal-size-0.1)
         ("rust-textwrap" ,rust-textwrap-0.15)
         ("rust-unicase" ,rust-unicase-2)
         ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
      "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-anyhow-1
  (package
    (name "rust-anyhow")
    (version "1.0.55")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anyhow" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kc4wsayyp6drb8ybisll07jvfy8wkm28hlg0sdf2052ydmbi6qm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-backtrace" ,rust-backtrace-0.3))))
    (home-page "https://github.com/dtolnay/anyhow")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description "Flexible concrete Error type built on std::error::Error")
    (license (list license:expat license:asl2.0))))

(define-public ra-multiplex-0.2
  (package
    (name "ra-multiplex")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ra-multiplex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "084gm3vfag4pbpi8rpf22agh0ag9sxwljl57vrgg67wbg3q95pdj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-nightly-1.60
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-directories" ,rust-directories-4)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/pr2502/ra-multiplex")
    (synopsis
     "share one rust-analyzer server instance between multiple LSP clients to save resources")
    (description
     "share one rust-analyzer server instance between multiple LSP clients to save
resources")
    (license license:expat)))


(define-public rust-selenium-rs-0.1
  (package
    (name "rust-selenium-rs")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "selenium-rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "020i6177xpxphzwmv5fkxg63j6faa3cn1rm56jjsykz1fk93l5q1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-reqwest" ,rust-reqwest-0.9)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-url" ,rust-url-1))))
    (home-page "https://github.com/saresend/selenium-rs")
    (synopsis
      "selenium-rs is a client for the selenium webdriver spec (https://www.w3.org/TR/webdriver1/). It 
is aimed to simplify behavior driven testing in rust, and for automating browser interaction. 
")
    (description
      "selenium-rs is a client for the selenium webdriver spec
(https://www.w3.org/TR/webdriver1/).  It  is aimed to simplify behavior driven
testing in rust, and for automating browser interaction. ")
    (license license:expat)))

(define-public rust-tower-util-0.1
  (package
    (name "rust-tower-util")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1626nvwdjfbp1hqs8rrlcq4sjk50i5d0amc6ap9bag89mhpk94j7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tokio-io" ,rust-tokio-io-0.1)
         ("rust-tower-layer" ,rust-tower-layer-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Utilities for working with `Service`.
")
    (description "Utilities for working with `Service`.")
    (license license:expat)))

(define-public rust-tower-timeout-0.1
  (package
    (name "rust-tower-timeout")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-timeout" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1abx8h96cyjsfnd9j8iwv8v7smclq466z30ajh8ghmnhzg1bn1jw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tokio-timer" ,rust-tokio-timer-0.2)
         ("rust-tower-layer" ,rust-tower-layer-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
      "Apply a timeout to requests, ensuring completion within a fixed time duration.
")
    (description
      "Apply a timeout to requests, ensuring completion within a fixed time duration.")
    (license license:expat)))

(define-public rust-tower-retry-0.1
  (package
    (name "rust-tower-retry")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-retry" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1xrn23vz8biqn5jjkfrd19biwhcbk0whjyraxmvg4qah2a40bs09"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tokio-timer" ,rust-tokio-timer-0.2)
         ("rust-tower-layer" ,rust-tower-layer-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Retry failed requests.
")
    (description "Retry failed requests.")
    (license license:expat)))

(define-public rust-tower-load-shed-0.1
  (package
    (name "rust-tower-load-shed")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-load-shed" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sfpikmg7x9nrinp1f5vy89rcm31xjpb5fc7vc245n33zddszyq4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tower-layer" ,rust-tower-layer-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
      "Immediately reject requests if the inner service is not ready. This is also
known as load-shedding.
")
    (description
      "Immediately reject requests if the inner service is not ready.  This is also
known as load-shedding.")
    (license license:expat)))

(define-public rust-tower-limit-0.1
  (package
    (name "rust-tower-limit")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-limit" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "13zinv0l1ag1r0yzlz8lfl8akqmnlx40bblirmilpmcgl0ssh6y2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tokio-sync" ,rust-tokio-sync-0.1)
         ("rust-tokio-timer" ,rust-tokio-timer-0.2)
         ("rust-tower-layer" ,rust-tower-layer-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2)
         ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Limit maximum request rate to a `Service`.
")
    (description "Limit maximum request rate to a `Service`.")
    (license license:expat)))

(define-public rust-tower-discover-0.1
  (package
    (name "rust-tower-discover")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-discover" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0iyy5g61aqryqp4ab6j8ca9xxb070dbhxz8qbpb690gphqi679vk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Abstracts over service discovery strategies.
")
    (description "Abstracts over service discovery strategies.")
    (license license:expat)))

(define-public rust-tower-service-0.2
  (package
    (name "rust-tower-service")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-service" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ykvhag9p3rc8r34l2z2p71mj5cz9i4idzbdvvw34dyj6y3ckh1c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-futures" ,rust-futures-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
      "Trait representing an asynchronous, request / response based, client or server.
")
    (description
      "Trait representing an asynchronous, request / response based, client or server.")
    (license license:expat)))

(define-public rust-tower-layer-0.1
  (package
    (name "rust-tower-layer")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-layer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0a434nxhhfcy2n2s7f0fny2imvvdqrnh7pm63pscip071khhgpqd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
      "Decorates a `Service` to allow easy composition between `Service`s.
")
    (description
      "Decorates a `Service` to allow easy composition between `Service`s.")
    (license license:expat)))

(define-public rust-tower-buffer-0.1
  (package
    (name "rust-tower-buffer")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-buffer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1q3hfcqpd9fvlafz488slp3ywawfgg0f4snx178kvcpmrkhq6yrw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tokio-executor" ,rust-tokio-executor-0.1)
         ("rust-tokio-sync" ,rust-tokio-sync-0.1)
         ("rust-tower-layer" ,rust-tower-layer-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2)
         ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis "Buffer requests before dispatching to a `Service`.
")
    (description "Buffer requests before dispatching to a `Service`.")
    (license license:expat)))

(define-public rust-tower-0.1
  (package
    (name "rust-tower")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0zwig916i1qv42wzviqfby2ax2q12gizqyh3y2fmrivjd8xz6wnw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-tower-buffer" ,rust-tower-buffer-0.1)
         ("rust-tower-discover" ,rust-tower-discover-0.1)
         ("rust-tower-layer" ,rust-tower-layer-0.1)
         ("rust-tower-limit" ,rust-tower-limit-0.1)
         ("rust-tower-load-shed" ,rust-tower-load-shed-0.1)
         ("rust-tower-retry" ,rust-tower-retry-0.1)
         ("rust-tower-service" ,rust-tower-service-0.2)
         ("rust-tower-timeout" ,rust-tower-timeout-0.1)
         ("rust-tower-util" ,rust-tower-util-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
      "Tower is a library of modular and reusable components for building robust
clients and servers.
")
    (description
      "Tower is a library of modular and reusable components for building robust
clients and servers.")
    (license license:expat)))

(define-public rust-tokio-serde-0.1
  (package
    (name "rust-tokio-serde")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-serde" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1734s21v0w091r83kfpgm5hrjnhld4xrbzs448m8d02g7hcnhhc9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-0.4) ("rust-futures" ,rust-futures-0.1))))
    (home-page "https://github.com/carllerche/tokio-serde")
    (synopsis
      "Send and receive Serde encodable types over the network using Tokio.

This library is used as a building block for serialization format specific
libraries.
")
    (description
      "Send and receive Serde encodable types over the network using Tokio.

This library is used as a building block for serialization format specific
libraries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-case-0.1
  (package
    (name "rust-case")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "case" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hgc6fdg01qfh0qx5c50n717vh0xqvrlvxix8ksng5p291mid2z8"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/SkylerLipthay/case")
    (synopsis "A set of letter case string helpers")
    (description "This package provides a set of letter case string helpers")
    (license license:expat)))

(define-public rust-derive-error-0.0.3
  (package
    (name "rust-derive-error")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derive-error" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1lvnx2bgp45wv9m908mivvvn8i7mzxv4d8l5r891jyffmfrip7v2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-case" ,rust-case-0.1)
         ("rust-quote" ,rust-quote-0.3)
         ("rust-syn" ,rust-syn-0.11))))
    (home-page "https://github.com/rushmorem/derive-error")
    (synopsis "Derive macro for Error using macros 1.1")
    (description "Derive macro for Error using macros 1.1")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-serde-bincode-0.1
  (package
    (name "rust-tokio-serde-bincode")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-serde-bincode" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1a3wc1k58kghhxnagc0pcv9sdy2nkqq2lmnx63xprs55c26mrqq2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bincode" ,rust-bincode-0.8)
         ("rust-bytes" ,rust-bytes-0.4)
         ("rust-derive-error" ,rust-derive-error-0.0.3)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-serde" ,rust-serde-1)
         ("rust-tokio-serde" ,rust-tokio-serde-0.1))))
    (home-page "https://github.com/luben/tokio-serde-bincode")
    (synopsis
      "Tokio bindings to quickly turn a stream of bytes to a stream of
parsed/serialized items using the bincode format.
")
    (description
      "Tokio bindings to quickly turn a stream of bytes to a stream of
parsed/serialized items using the bincode format.")
    (license license:expat)))

(define-public rust-tokio-timer-0.2
  (package
    (name "rust-tokio-timer")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-timer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "15pjjj6daks3sii8p24a509b0dapl2kyk740nwfgz59w64nly14k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-tokio-executor" ,rust-tokio-executor-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Timer facilities for Tokio
")
    (description "Timer facilities for Tokio")
    (license license:expat)))

(define-public rust-tokio-reactor-0.1
  (package
    (name "rust-tokio-reactor")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-reactor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0l8klnd41q55f3ialzz0lb7s5bfwa38nh86sa9vai2xsqh75kg09"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
         ("rust-futures" ,rust-futures-0.1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-mio" ,rust-mio-0.6)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-parking-lot" ,rust-parking-lot-0.9)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-tokio-executor" ,rust-tokio-executor-0.1)
         ("rust-tokio-io" ,rust-tokio-io-0.1)
         ("rust-tokio-sync" ,rust-tokio-sync-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Event loop that drives Tokio I/O resources.
")
    (description "Event loop that drives Tokio I/O resources.")
    (license license:expat)))

(define-public rust-tokio-executor-0.1
  (package
    (name "rust-tokio-executor")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-executor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0w8n78d2vixs1vghqc4wy9w0d1h6qkli51c1yzhzbns88n7inbgv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.7)
         ("rust-futures" ,rust-futures-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Future execution primitives
")
    (description "Future execution primitives")
    (license license:expat)))

(define-public rust-tokio-compat-0.1
  (package
    (name "rust-tokio-compat")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-compat" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08n7lkf5l2drb7hph1r3s6jj9cbarbbcr69dvnbr4yxa6m8n4yqh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.1)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1)
         ("rust-tokio-executor" ,rust-tokio-executor-0.1)
         ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
         ("rust-tokio-timer" ,rust-tokio-timer-0.2))))
    (home-page "https://tokio.rs")
    (synopsis "Compatibility between `tokio` 0.2 and legacy versions.
")
    (description "Compatibility between `tokio` 0.2 and legacy versions.")
    (license license:expat)))

(define-public rust-syslog-5
  (package
    (name "rust-syslog")
    (version "5.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syslog" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0paii62qnwjnfliygdal1x3hqxjkci1nlczfydv7kh3rnvqqwpcs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-error-chain" ,rust-error-chain-0.12)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-time" ,rust-time-0.1))))
    (home-page "https://github.com/Geal/rust-syslog")
    (synopsis "Send log messages to syslog")
    (description "Send log messages to syslog")
    (license license:expat)))

(define-public rust-brotli-3
  (package
    (name "rust-brotli")
    (version "3.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "brotli" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0f8wzcb6ig7rs31w89fzmjvykb6n9l1807rpaajqynhx8mxf8f7q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
         ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2)
         ("rust-brotli-decompressor" ,rust-brotli-decompressor-2)
         ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
         ("rust-sha2" ,rust-sha2-0.8))))
    (home-page "https://github.com/dropbox/rust-brotli")
    (synopsis
      "A brotli compressor and decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib's allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. All included code is safe.")
    (description
      "This package provides a brotli compressor and decompressor that with an
interface avoiding the rust stdlib.  This makes it suitable for embedded devices
and kernels.  It is designed with a pluggable allocator so that the standard
lib's allocator may be employed.  The default build also includes a stdlib
allocator and stream interface.  Disable this with --features=no-stdlib.  All
included code is safe.")
    (license (list license:bsd-3 license:expat))))

(define-public rust-rouille-3
  (package
    (name "rust-rouille")
    (version "3.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rouille" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "01adygif44h8wqay40q1b7n0s7l09hbql8jzifig83ji8863ichq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-brotli" ,rust-brotli-3)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-deflate" ,rust-deflate-0.9)
         ("rust-filetime" ,rust-filetime-0.2)
         ("rust-multipart" ,rust-multipart-0.18)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-sha1" ,rust-sha1-0.6)
         ("rust-threadpool" ,rust-threadpool-1)
         ("rust-time" ,rust-time-0.3)
         ("rust-tiny-http" ,rust-tiny-http-0.8)
         ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/tomaka/rouille")
    (synopsis "High-level idiomatic web framework.")
    (description "High-level idiomatic web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-retry-1
  (package
    (name "rust-retry")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "retry" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1xm3p41ygijbjpyj81psqhb2r3rdcqwlk5pl48lgsqwsjh5cd5dc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/jimmycuadra/retry")
    (synopsis "Utilities for retrying operations that can fail.")
    (description "Utilities for retrying operations that can fail.")
    (license license:expat)))

(define-public rust-crc16-0.4
  (package
    (name "rust-crc16")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc16" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zzwb5iv51wnh96532cxkk4aa8ys47rhzrjy98wqcys25ks8k01k"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/blackbeam/rust-crc16")
    (synopsis "A CRC16 implementation")
    (description "This package provides a CRC16 implementation")
    (license license:expat)))

(define-public rust-redis-0.17
  (package
    (name "rust-redis")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redis" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1b193x2w3ma3vz603rmq8jzwrqagvyl9hg5ra53cbas04sppqdcm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-arc-swap" ,rust-arc-swap-0.4)
         ("rust-async-native-tls" ,rust-async-native-tls-0.3)
         ("rust-async-std" ,rust-async-std-1)
         ("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-bytes" ,rust-bytes-0.5)
         ("rust-combine" ,rust-combine-4)
         ("rust-crc16" ,rust-crc16-0.4)
         ("rust-dtoa" ,rust-dtoa-0.4)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-itoa" ,rust-itoa-0.4)
         ("rust-native-tls" ,rust-native-tls-0.2)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.1)
         ("rust-r2d2" ,rust-r2d2)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-sha1" ,rust-sha1-0.6)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-tokio-tls" ,rust-tokio-tls-0.3)
         ("rust-tokio-util" ,rust-tokio-util-0.3)
         ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/mitsuhiko/redis-rs")
    (synopsis "Redis driver for Rust.")
    (description "Redis driver for Rust.")
    (license license:bsd-3)))

(define-public rust-number-prefix-0.4
  (package
    (name "rust-number-prefix")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "number_prefix" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/ogham/rust-number-prefix")
    (synopsis "Library for numeric prefixes (kilo, giga, kibi).")
    (description "Library for numeric prefixes (kilo, giga, kibi).")
    (license license:expat)))

(define-public rust-conhash-0.4
  (package
    (name "rust-conhash")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "conhash" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1jhzkf744si69mrvg4il1p8pqdysh9cgl530igcx0y47096kdmlr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4) ("rust-md5" ,rust-md5-0.3))))
    (home-page "https://github.com/zonyitoo/conhash-rs")
    (synopsis "Consistent Hashing library in Rust")
    (description "Consistent Hashing library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-memcached-rs-0.4
  (package
    (name "rust-memcached-rs")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memcached-rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "078bxv85gm7b5g2z8mrdjm1bcdnz3wc5mv5ksqwwsmmmm11ld073"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bufstream" ,rust-bufstream-0.1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-conhash" ,rust-conhash-0.4)
         ("rust-log" ,rust-log-0.4)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-semver" ,rust-semver-0.9)
         ("rust-unix-socket" ,rust-unix-socket-0.5))))
    (home-page "https://github.com/zonyitoo/memcached-rs")
    (synopsis "A MemCached Library in Rust")
    (description "This package provides a MemCached Library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-pulldown-cmark-0.0.3
  (package
    (name "rust-pulldown-cmark")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "01vc9rw3dwrwq0qwfrfsh1qnhaqpxj3y8l29n11jdq6jfqayhqc3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-getopts" ,rust-getopts-0.2))))
    (home-page "https://github.com/raphlinus/pulldown-cmark")
    (synopsis "A pull parser for CommonMark")
    (description "This package provides a pull parser for CommonMark")
    (license license:asl2.0)))

(define-public rust-skeptic-0.4
  (package
    (name "rust-skeptic")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "skeptic" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0si5hbzfb68ipbx30lcpmq3nkvy4mbvpmg2vmrhsx2szdyhgisr4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pulldown-cmark" ,rust-pulldown-cmark-0.0.3)
         ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/budziq/rust-skeptic")
    (synopsis "Test your Rust markdown documentation via Cargo")
    (description "Test your Rust markdown documentation via Cargo")
    (license (list license:expat license:asl2.0))))

(define-public rust-local-encoding-0.2
  (package
    (name "rust-local-encoding")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "local-encoding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0rhsb8x10i0959ry38da3j1avnmihqwmyygr7wpy8ypz747v5kp1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
         ("rust-skeptic" ,rust-skeptic-0.4)
         ("rust-winapi" ,rust-winapi-0.2))))
    (home-page "https://github.com/bozaro/local-encoding-rs")
    (synopsis
      "Rust library for encoding/decoding string with local charset. It usefull for work with ANSI strings on Windows.")
    (description
      "Rust library for encoding/decoding string with local charset.  It usefull for
work with ANSI strings on Windows.")
    (license license:expat)))

(define-public rust-libmount-0.1
  (package
    (name "rust-libmount")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libmount" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0py3kl473jgfwnfajzr0xi9xs2lk8npks3320md2zgaw5nnw5i13"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-nix" ,rust-nix-0.14)
         ("rust-quick-error" ,rust-quick-error-1))))
    (home-page "http://github.com/tailhook/libmount")
    (synopsis "    The type-safe wrapper around mount system call
")
    (description "    The type-safe wrapper around mount system call")
    (license (list license:expat license:asl2.0))))

(define-public rust-simple-asn1-0.4
  (package
    (name "rust-simple-asn1")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "simple_asn1" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jxy9as8nj65c2n27j843g4fpb95x4fjz31w6qx63q3wwlys2b39"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-chrono" ,rust-chrono-0.4)
         ("rust-num-bigint" ,rust-num-bigint-0.2)
         ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/acw/simple_asn1")
    (synopsis "A simple DER/ASN.1 encoding/decoding library.")
    (description
      "This package provides a simple DER/ASN.1 encoding/decoding library.")
    (license license:isc)))

(define-public rust-pem-0.8
  (package
    (name "rust-pem")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pem" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1sqkzp87j6s79sjxk4n913gcmalzb2fdc75l832d0j7a3z9cnmpx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/jcreekmore/pem-rs.git")
    (synopsis "Parse and encode PEM-encoded data.")
    (description "Parse and encode PEM-encoded data.")
    (license license:expat)))

(define-public rust-jsonwebtoken-7
  (package
    (name "rust-jsonwebtoken")
    (version "7.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jsonwebtoken" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ciz205wcjcn7n6i871zz5xlbzk863b0ybgiqi7li9ipwhawraxg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.12)
         ("rust-pem" ,rust-pem-0.8)
         ("rust-ring" ,rust-ring-0.16)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-simple-asn1" ,rust-simple-asn1-0.4))))
    (home-page "https://github.com/Keats/jsonwebtoken")
    (synopsis "Create and decode JWTs in a strongly typed way.")
    (description "Create and decode JWTs in a strongly typed way.")
    (license license:expat)))

(define-public rust-hyperx-0.12
  (package
    (name "rust-hyperx")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyperx" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "19pdq1avrrk0g81pwl549z4j9jcgqi4dyfpwc2ar45vs7ljx5qkq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.9)
         ("rust-bytes" ,rust-bytes-0.4)
         ("rust-http" ,rust-http-0.1)
         ("rust-httparse" ,rust-httparse-1)
         ("rust-language-tags" ,rust-language-tags-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-mime" ,rust-mime-0.3)
         ("rust-percent-encoding" ,rust-percent-encoding-1)
         ("rust-time" ,rust-time-0.1)
         ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/dekellum/hyperx")
    (synopsis "Hyper's typed header module, eXtracted and improved")
    (description "Hyper's typed header module, eXtracted and improved")
    (license license:expat)))

(define-public rust-counted-array-0.1
  (package
    (name "rust-counted-array")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "counted-array" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zj8yr39pb0q2v4gskbcr9rs3lh90xrpn4p0nqh0k2aw2x9qqkrq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-0.2))))
    (home-page "https://github.com/durka/counted-array")
    (synopsis
      "Macro for declaring fixed-size arrays without counting elements by hand. Supports lazy_static.")
    (description
      "Macro for declaring fixed-size arrays without counting elements by hand.
Supports lazy_static.")
    (license license:expat)))

(define-public rust-ar-0.8
  (package
    (name "rust-ar")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ar" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qajmxnl762bxnn0w7m8qabp6ya7ph64gzxvda0k5vkvizspa1a5"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/mdsteele/rust-ar")
    (synopsis "A library for encoding/decoding Unix archive files.")
    (description
      "This package provides a library for encoding/decoding Unix archive files.")
    (license license:expat)))

(define-public rust-zstd-0.6
  (package
    (inherit rust-zstd-0.8)
    (name "rust-zstd")
    (version "0.6.1+zstd.1.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18riavdrq4cy10ygvrxby87nxyxbazpy53qvavc0bwlqyxvmxrax"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-zstd-safe" ,rust-zstd-safe-3))))))


(define-public rust-zstd-safe-4
  (package
    (name "rust-zstd-safe")
    (version "4.1.1+zstd.1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yghr94blhnfigzsynm2km3g93886z49612y7rh07c4kqpr90769"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-zstd-sys" ,rust-zstd-sys-1))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Safe low-level bindings to the zstd compression library")
    (description
     "This package provides safe low-level bindings to the zstd compression
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zstd-safe-3
  (package
    (inherit rust-zstd-safe-4)
    (name "rust-zstd-safe")
    (version "3.0.1+zstd.1.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-safe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "133gassn5zy4vf0hhgsff3gxv1q3nc0bzi3qrqq7n4iqv6ycm1qk"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-zstd-sys" ,rust-zstd-sys-1))))))

;; TODO: Unbundle zstd.
(define-public rust-zstd-sys-1
  (package
    (name "rust-zstd-sys")
    (version "1.4.20+zstd.1.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zstd-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13kc3crvqg021fya48jw0spfbxdli5anmry3w93r8bfgswrvgmgb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.58)
        ("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Low-level bindings to the zstd compression library")
    (description "This package provides low-level Rust bindings to the zstd
compression library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sccache-0.2
  (package
    (name "rust-sccache")
    (version "0.2.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sccache" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xzbphkx516vi09grmzfcjnxsn0xnb0zmhgwygdwrgi4cbaij6yd"))))
    (build-system cargo-build-system)
    (inputs `(("openssl" ,openssl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-ar" ,rust-ar-0.8)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-blake3" ,rust-blake3-0.3)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-counted-array" ,rust-counted-array-0.1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-daemonize" ,rust-daemonize-0.4)
        ("rust-directories" ,rust-directories-3)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-hmac" ,rust-hmac-0.10)
        ("rust-http" ,rust-http-0.1)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-hyperx" ,rust-hyperx-0.12)
        ("rust-jobserver" ,rust-jobserver-0.1)
        ("rust-jsonwebtoken" ,rust-jsonwebtoken-7)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libmount" ,rust-libmount-0.1)
        ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
        ("rust-local-encoding" ,rust-local-encoding-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-memcached-rs" ,rust-memcached-rs-0.4)
        ("rust-nix" ,rust-nix-0.19)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-number-prefix" ,rust-number-prefix-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-redis" ,rust-redis-0.17)
        ("rust-regex" ,rust-regex-1)
        ("rust-reqwest" ,rust-reqwest-0.9)
        ("rust-retry" ,rust-retry-1)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rouille" ,rust-rouille-3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.1)
        ("rust-syslog" ,rust-syslog-5)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-tokio-compat" ,rust-tokio-compat-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-named-pipes" ,rust-tokio-named-pipes-0.1)
        ("rust-tokio-process" ,rust-tokio-process-0.2)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-serde-bincode" ,rust-tokio-serde-bincode-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-tokio-uds" ,rust-tokio-uds-0.2)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-tower" ,rust-tower-0.1)
        ("rust-untrusted" ,rust-untrusted-0.7)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-0.8)
        ("rust-version-compare" ,rust-version-compare-0.0.11)
        ("rust-void" ,rust-void-1)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-which" ,rust-which-4)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-zip" ,rust-zip-0.5)
        ("rust-zstd" ,rust-zstd-0.6))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-cc" ,rust-cc-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-predicates" ,rust-predicates-1)
        ("rust-selenium-rs" ,rust-selenium-rs-0.1))))
    (home-page "https://github.com/mozilla/sccache/")
    (synopsis
     "Sccache is a ccache-like tool. It is used as a compiler wrapper and avoids compilation when possible, storing a cache in a remote storage using the S3 API.")
    (description
     "Sccache is a ccache-like tool.  It is used as a compiler wrapper and avoids
compilation when possible, storing a cache in a remote storage using the S3 API.")
    (license license:asl2.0)))

(define-public rust-indicatif-0.16
  (package
    (name "rust-indicatif")
    (version "0.16.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indicatif" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "06xyjs0kzqiqkjn60n1miwm2l87sa9p2lmzz0ymq18y72z37s81d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-console" ,rust-console-0.15)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-number-prefix" ,rust-number-prefix-0.4)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-regex" ,rust-regex-1)
         ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
         ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/console-rs/indicatif")
    (synopsis "A progress bar and cli reporting library for Rust")
    (description
      "This package provides a progress bar and cli reporting library for Rust")
    (license license:expat)))

(define-public rust-crossbeam-channel-0.5
  (package
    (name "rust-crossbeam-channel")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-channel" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0lvcpv6hg1g1r6aamiq9b4958p4hjy8dsqzrnmj6hp36zgappajs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-channel")
    (synopsis "Multi-producer multi-consumer channels for message passing")
    (description "Multi-producer multi-consumer channels for message passing")
    (license (list license:expat license:asl2.0))))

(define-public rust-termcolor-1
  (package
    (name "rust-termcolor")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0mbpflskhnz3jf312k50vn0hqbql8ga2rk0k79pkgchip4q4vcms"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-winapi-util" ,rust-winapi-util-0.1))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis
      "A simple cross platform library for writing colored text to a terminal.
")
    (description
      "This package provides a simple cross platform library for writing colored text
to a terminal.")
    (license (list license:unlicense license:expat))))

(define-public rust-csv-1
  (package
    (name "rust-csv")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "csv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1q9nqn0qlamwl18v57p82c8yhxy43lkzf2z1mndmycsvqinkm092"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bstr" ,rust-bstr-0.2)
         ("rust-csv-core" ,rust-csv-core-0.1)
         ("rust-itoa" ,rust-itoa-0.4)
         ("rust-ryu" ,rust-ryu-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/BurntSushi/rust-csv")
    (synopsis "Fast CSV parsing with support for serde.")
    (description "Fast CSV parsing with support for serde.")
    (license (list license:unlicense license:expat))))

(define-public rust-cli-table-derive-0.4
  (package
    (name "rust-cli-table-derive")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cli-table-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m4sh8z0b8q8bhxljdfl9rvk654jcdwzn93n8rn0lyv2vawvzwra"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/devashishdxt/cli-table")
    (synopsis "A crate for printing tables on command line")
    (description
      "This package provides a crate for printing tables on command line")
    (license (list license:expat license:asl2.0))))

(define-public rust-cli-table-0.4
  (package
    (name "rust-cli-table")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cli-table" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "179pvik96qavn84rd74n3v0i4msnxq5hq39n25qbxi72v4bb3yxd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cli-table-derive" ,rust-cli-table-derive-0.4)
         ("rust-csv" ,rust-csv-1)
         ("rust-termcolor" ,rust-termcolor-1)
         ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/devashishdxt/cli-table")
    (synopsis "A crate for printing tables on command line")
    (description
      "This package provides a crate for printing tables on command line")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-complete-3
  (package
    (name "rust-clap-complete")
    (version "3.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_complete" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1a5xidmxbkw4k1hd0wz8jzbp0qb5p8gv7xria4xabxv6x1zbh1hm"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t #:cargo-inputs (("rust-clap" ,rust-clap-3))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_complete")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description "Generate shell completion scripts for your clap::Command")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-lex-0.1
  (package
    (name "rust-clap-lex")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_lex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sb6jq722h33vrnyfa4iv6q3gaa22wvm8q38wwshp9rjblxxv78q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-os-str-bytes" ,rust-os-str-bytes-6))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
    (synopsis "Minimal, flexible command line parser")
    (description "Minimal, flexible command line parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-3
  (package
    (name "rust-clap-derive")
    (version "3.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hck94hzb8h9gbgggiv95j6pj73dfm7f352pmyd80fq89rrv9am3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-heck" ,rust-heck-0.4)
         ("rust-proc-macro-error" ,rust-proc-macro-error-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
      "Parse command line argument by defining a struct, derive crate.")
    (description
      "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-3
  (package
    (name "rust-clap")
    (version "3.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ns6s6yiy96rri2mv87zdlm0s0nxrrqcifw7zlrmzz1a6hvpw5kw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-atty" ,rust-atty-0.2)
         ("rust-backtrace" ,rust-backtrace-0.3)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-clap-derive" ,rust-clap-derive-3)
         ("rust-clap-lex" ,rust-clap-lex-0.1)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-regex" ,rust-regex-1)
         ("rust-strsim" ,rust-strsim-0.10)
         ("rust-termcolor" ,rust-termcolor-1)
         ("rust-terminal-size" ,rust-terminal-size-0.1)
         ("rust-textwrap" ,rust-textwrap-0.15)
         ("rust-unicase" ,rust-unicase-2)
         ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
      "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-tower-http-0.2
  (package
    (name "rust-tower-http")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-http" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1n2cddayd0kczcg48ifb28fds4vhh45c4kskx3x43yzpmgpz78xb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-compression" ,rust-async-compression-0.3)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-http" ,rust-http-0.2)
         ("rust-http-body" ,rust-http-body-0.4)
         ("rust-http-range-header" ,rust-http-range-header-0.3)
         ("rust-httpdate" ,rust-httpdate-1)
         ("rust-iri-string" ,rust-iri-string-0.4)
         ("rust-mime" ,rust-mime-0.3)
         ("rust-mime-guess" ,rust-mime-guess-2)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-util" ,rust-tokio-util-0.7)
         ("rust-tower" ,rust-tower-0.4)
         ("rust-tower-layer" ,rust-tower-layer-0.3)
         ("rust-tower-service" ,rust-tower-service-0.3)
         ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description "Tower middleware and utilities for HTTP clients and servers")
    (license license:expat)))

(define-public rust-chronoutil-0.2
  (package
    (name "rust-chronoutil")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chronoutil" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zc33vbn7p93kk43nvafqrsh5dj6ihqgbb533lhalwmp9f98r9a3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4))))
    (home-page "https://github.com/olliemath/chronoutil")
    (synopsis "Powerful extensions to rust's Chrono crate")
    (description "Powerful extensions to rust's Chrono crate")
    (license license:expat)))

(define-public rust-iri-string-0.4
  (package
    (name "rust-iri-string")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iri-string" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0y2z4f5y87hnff2d5lcl811hp7iv2f5qri7x3fgm48z2q4w7c3wg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-nom" ,rust-nom-7) ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/lo48576/iri-string")
    (synopsis "IRI as string types")
    (description "IRI as string types")
    (license (list license:expat license:asl2.0))))

(define-public rust-http-range-header-0.3
  (package
    (name "rust-http-range-header")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http-range-header" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0aas8c5dagfhcqpmqq9xw6a8nkl3lfg4g4mpddvyz1cj1bnqxzhb"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/MarcusGrass/parse-range-headers")
    (synopsis "No-dep range header parser")
    (description "No-dep range header parser")
    (license license:expat)))

(define-public rust-tower-http-0.3
  (package
    (name "rust-tower-http")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-http" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zb6jcc20q20kvz91xpjfvwbn63280r06lp69x5hq91q3h93gpbr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-compression" ,rust-async-compression-0.3)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-http" ,rust-http-0.2)
         ("rust-http-body" ,rust-http-body-0.4)
         ("rust-http-range-header" ,rust-http-range-header-0.3)
         ("rust-httpdate" ,rust-httpdate-1)
         ("rust-iri-string" ,rust-iri-string-0.4)
         ("rust-mime" ,rust-mime-0.3)
         ("rust-mime-guess" ,rust-mime-guess-2)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-util" ,rust-tokio-util-0.7)
         ("rust-tower" ,rust-tower-0.4)
         ("rust-tower-layer" ,rust-tower-layer-0.3)
         ("rust-tower-service" ,rust-tower-service-0.3)
         ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower-http")
    (synopsis "Tower middleware and utilities for HTTP clients and servers")
    (description "Tower middleware and utilities for HTTP clients and servers")
    (license license:expat)))

(define-public rust-tower-service-0.3
  (package
    (name "rust-tower-service")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower-service" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1iih764s3f6vlkspfmr72fkrs2lw1v3wiqmc6bd5zq1hdlfzs39n"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
      "Trait representing an asynchronous, request / response based, client or server.
")
    (description
      "Trait representing an asynchronous, request / response based, client or server.")
    (license license:expat)))

(define-public rust-slab-0.4
  (package
    (name "rust-slab")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slab" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0cmvcy9ppsh3dz8mi6jljx7bxyknvgpas4aid2ayxk1vjpz3qw7b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tokio-rs/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description "Pre-allocated storage for a uniform data type")
    (license license:expat)))

(define-public rust-tokio-util-0.7
  (package
    (name "rust-tokio-util")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0r0p83nisf732qydg23qvmdd6gbrvyr1qvfs8hhbl7a1cyqdxpqf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-io" ,rust-futures-io-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Additional utilities for working with Tokio.
")
    (description "Additional utilities for working with Tokio.")
    (license license:expat)))

(define-public rust-hdrhistogram-7
  (package
    (name "rust-hdrhistogram")
    (version "7.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hdrhistogram" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1h0905yk0pxgxfk4kzlfmnglm6ky1ssbrpf4ars4yb5y25q2nrri"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
         ("rust-flate2" ,rust-flate2-1)
         ("rust-nom" ,rust-nom-7)
         ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/HdrHistogram/HdrHistogram_rust")
    (synopsis "A port of HdrHistogram to Rust")
    (description "This package provides a port of HdrHistogram to Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-tower-0.4
  (package
    (name "rust-tower")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tower" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gkisq1mcfyw2i9aq7d1d4y52x35506v8pfzh9sp7pvammizv2cs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-hdrhistogram" ,rust-hdrhistogram-7)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-pin-project" ,rust-pin-project-1)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-stream" ,rust-tokio-stream-0.1)
         ("rust-tokio-util" ,rust-tokio-util-0.7)
         ("rust-tower-layer" ,rust-tower-layer-0.3)
         ("rust-tower-service" ,rust-tower-service-0.3)
         ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/tower-rs/tower")
    (synopsis
      "Tower is a library of modular and reusable components for building robust
clients and servers.
")
    (description
      "Tower is a library of modular and reusable components for building robust
clients and servers.")
    (license license:expat)))

(define-public rust-tungstenite-0.17
  (package
    (name "rust-tungstenite")
    (version "0.17.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tungstenite" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1x848392ihy5mh098sns0lcmb5rdwkxpmdcfya108mz783m2ssnr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-http" ,rust-http-0.2)
         ("rust-httparse" ,rust-httparse-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-native-tls" ,rust-native-tls-0.2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-rustls" ,rust-rustls-0.20)
         ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
         ("rust-sha-1" ,rust-sha-1-0.10)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-url" ,rust-url-2)
         ("rust-utf-8" ,rust-utf-8-0.7)
         ("rust-webpki" ,rust-webpki-0.22)
         ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description "Lightweight stream-based WebSocket implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-rustls-0.23
  (package
    (name "rust-tokio-rustls")
    (version "0.23.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-rustls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "17iqy9a8x0d8ydl5r28w8z9akhnwp74wyjxks055b617ryhgsla1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustls" ,rust-rustls-0.20)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-webpki" ,rust-webpki-0.22))))
    (home-page "https://github.com/tokio-rs/tls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-tungstenite-0.17
  (package
    (name "rust-tokio-tungstenite")
    (version "0.17.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-tungstenite" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1bi1z1l8392v20mg24gryw5jrm0166wxa155z138qma958is3k86"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-native-tls" ,rust-native-tls-0.2)
         ("rust-rustls" ,rust-rustls-0.20)
         ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
         ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
         ("rust-tungstenite" ,rust-tungstenite-0.17)
         ("rust-webpki" ,rust-webpki-0.22)
         ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
      "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
      "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket
implementation")
    (license license:expat)))

(define-public rust-sync-wrapper-0.1
  (package
    (name "rust-sync-wrapper")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sync_wrapper" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1a59lwsw52d1a64l2y1m7npfw6xjvrjf96c5014g1b69lkj8yl90"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://docs.rs/sync_wrapper")
    (synopsis
      "A tool for enlisting the compilerâ\x80\x99s help in proving the absence of concurrency")
    (description
      "This package provides a tool for enlisting the compilerâ\x80\x99s help in proving the
absence of concurrency")
    (license license:asl2.0)))

(define-public rust-matchit-0.5
  (package
    (name "rust-matchit")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matchit" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1swbyfxyz6nh8df514dqgds6al8lrrcxynhpbbgn5dvijrwvmjvk"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/ibraheemdev/matchit")
    (synopsis "A blazing fast URL router.")
    (description "This package provides a blazing fast URL router.")
    (license license:expat)))

(define-public rust-http-0.2
  (package
    (name "rust-http")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "00zxqk6m9qksxmlajmhnhgryw6xmqn9riimwx87nz1l4cmscdx1i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
         ("rust-fnv" ,rust-fnv-1)
         ("rust-itoa" ,rust-itoa-1))))
    (home-page "https://github.com/hyperium/http")
    (synopsis "A set of types for representing HTTP requests and responses.
")
    (description
      "This package provides a set of types for representing HTTP requests and
responses.")
    (license (list license:expat license:asl2.0))))

(define-public rust-axum-core-0.2
  (package
    (name "rust-axum-core")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "axum-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1rp1bs2r0lnz857ds5ggf95rsl54dwrqldh0fa52zqqn3dw1kp1v"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-http" ,rust-http-0.2)
         ("rust-http-body" ,rust-http-body-0.4)
         ("rust-mime" ,rust-mime-0.3))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Core types and traits for axum")
    (description "Core types and traits for axum")
    (license license:expat)))

(define-public rust-axum-0.5
  (package
    (name "rust-axum")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "axum" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "16k8cxdkf3v5v6x4pdk576qqnv3a47863s5cl7rw250jzi3p9bzl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-axum-core" ,rust-axum-core-0.2)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-headers" ,rust-headers-0.3)
         ("rust-http" ,rust-http-0.2)
         ("rust-http-body" ,rust-http-body-0.4)
         ("rust-hyper" ,rust-hyper-0.14)
         ("rust-itoa" ,rust-itoa-1)
         ("rust-matchit" ,rust-matchit-0.5)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-mime" ,rust-mime-0.3)
         ("rust-multer" ,rust-multer-2)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
         ("rust-sha-1" ,rust-sha-1-0.10)
         ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.17)
         ("rust-tower" ,rust-tower-0.4)
         ("rust-tower-http" ,rust-tower-http-0.3)
         ("rust-tower-layer" ,rust-tower-layer-0.3)
         ("rust-tower-service" ,rust-tower-service-0.3))))
    (home-page "https://github.com/tokio-rs/axum")
    (synopsis "Web framework that focuses on ergonomics and modularity")
    (description "Web framework that focuses on ergonomics and modularity")
    (license license:expat)))

(define-public rust-atuin-server-0.9
  (package
    (name "rust-atuin-server")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-server" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0zvlsfa2vkg1j5p8kav73fm5371ybhjp1a59m3d9m4x154i0z0hl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-atuin-common" ,rust-atuin-common-0.9)
         ("rust-axum" ,rust-axum-0.5)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-chronoutil" ,rust-chronoutil-0.2)
         ("rust-config" ,rust-config-0.13)
         ("rust-eyre" ,rust-eyre-0.6)
         ("rust-fs-err" ,rust-fs-err-2)
         ("rust-http" ,rust-http-0.2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-rust-crypto" ,rust-rust-crypto-0.2)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-sodiumoxide" ,rust-sodiumoxide-0.2)
         ("rust-sqlx" ,rust-sqlx-0.5)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tower" ,rust-tower-0.4)
         ("rust-tower-http" ,rust-tower-http-0.2)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-uuid" ,rust-uuid-1)
         ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://atuin.sh")
    (synopsis "server library for atuin")
    (description "server library for atuin")
    (license license:expat)))

(define-public rust-urlencoding-2
  (package
    (name "rust-urlencoding")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "urlencoding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08cq5w84imxrpyifhmx719026dzjih29gdq0ncsb1fcs08qhkfb8"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://lib.rs/urlencoding")
    (synopsis "A Rust library for doing URL percentage encoding.")
    (description
      "This package provides a Rust library for doing URL percentage encoding.")
    (license license:expat)))

(define-public rust-quote-1
  (package
    (name "rust-quote")
    (version "1.0.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1lca4xnwdc2sp76bf4n50kifmi5phhxr9520w623mfcksr7bbzm1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.37")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ldg6l97xlr4dal4kmk0c4l8kn7nn8w1a17wd8hdlpwd8cc74xgc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid_0_2_2))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
      "A substitute implementation of the compiler's `proc_macro` API to decouple
token-based libraries from the procedural macro use case.
")
    (description
      "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-macros-0.5
  (package
    (name "rust-sqlx-macros")
    (version "0.5.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qfjkvv0frk90hmmj3wz6ygb9z67ljxghik0zq0gq8df1hmvl3xw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-dotenv" ,rust-dotenv-0.15)
         ("rust-either" ,rust-either-1)
         ("rust-heck" ,rust-heck-0.4)
         ("rust-hex" ,rust-hex-0.4)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-sha2" ,rust-sha2-0.10)
         ("rust-sqlx-core" ,rust-sqlx-core-0.5)
         ("rust-sqlx-rt" ,rust-sqlx-rt-0.5)
         ("rust-syn" ,rust-syn-1)
         ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
      "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
      "Macros for SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-whoami-1
  (package
    (name "rust-whoami")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "whoami" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1s355zs8ir1li29cwvzgaqm6jyb51svmhqyx2hqgp8i0bbx5hjsj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
         ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/libcala/whoami/blob/main/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment.")
    (description "Retrieve the current user and environment.")
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror-impl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jviwmvx6wzawsj6c9msic7h419wmsbjagl9dzhpydkzc8zzscma"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description "Implementation detail of the `thiserror` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thiserror" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "05y4wm29ck8flwq5k1q6nhwh00a3b30cz3xr0qvnbwad5vjsnjw5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-thiserror-impl" ,rust-thiserror-impl-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "derive(Error)")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-rustls-0.2
  (package
    (name "rust-async-rustls")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-rustls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0y7kad5byac4mqyqkbrw26qjxrd7qvhajlcjdlnkwkssplxg71lw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-lite" ,rust-futures-lite-1)
         ("rust-rustls" ,rust-rustls-0.19)
         ("rust-webpki" ,rust-webpki-0.21))))
    (home-page "https://github.com/stjepang/async-rustls")
    (synopsis "Async TLS/SSL streams using rustls")
    (description "Async TLS/SSL streams using rustls")
    (license (list license:asl2.0 license:expat))))

(define-public rust-socket2-0.4
  (package
    (name "rust-socket2")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1q71bsw7sqr3nq71gszywgymxxfv311a3w1aia4k5binjisjpmv6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2) ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rust-lang/socket2")
    (synopsis
      "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.
")
    (description
      "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sc-0.2
  (package
    (name "rust-sc")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "12x3c3mn36am3jfamswqfsd0vpr0hz3kdck6wskla7gx7fyih3h1"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/japaric/syscall.rs")
    (synopsis "Raw system calls")
    (description "Raw system calls")
    (license (list license:expat license:asl2.0))))

(define-public rust-io-uring-0.5
  (package
    (name "rust-io-uring")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "io-uring" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1a6da1jwxg115qa6cch6nnscxjl4kmxy97q3j1nanyrpv6g84xcd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.59)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-sc" ,rust-sc-0.2))))
    (home-page "https://github.com/tokio-rs/io-uring")
    (synopsis "The low-level `io_uring` userspace interface for Rust")
    (description "The low-level `io_uring` userspace interface for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-uring-0.3
  (package
    (name "rust-tokio-uring")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-uring" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1j22wyryp8ami8gq9cgh3wqd7g5gklqzdrxdj3cq8jc7757lkbfk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
         ("rust-io-uring" ,rust-io-uring-0.5)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-scoped-tls" ,rust-scoped-tls-1)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-socket2" ,rust-socket2-0.4)
         ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://tokio.rs")
    (synopsis "io-uring support for the Tokio asynchronous runtime.
")
    (description "io-uring support for the Tokio asynchronous runtime.")
    (license license:expat)))

(define-public rust-actix-macros-0.2
  (package
    (name "rust-actix-macros")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1dhk2bdp6rj67j5zgi4b76hpy2xw567js0hig28n1fb9rxr62nj6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-quote" ,rust-quote-1) ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis "Macros for Actix system and runtime")
    (description "Macros for Actix system and runtime")
    (license (list license:expat license:asl2.0))))

(define-public rust-actix-rt-2
  (package
    (name "rust-actix-rt")
    (version "2.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-rt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "000hxsbaxgd8jdmnw4dnlff4xdhggprnw2lk67pmiscqa4lnr8by"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-actix-macros" ,rust-actix-macros-0.2)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-uring" ,rust-tokio-uring-0.3))))
    (home-page "https://actix.rs")
    (synopsis
      "Tokio-based single-threaded async runtime for the Actix ecosystem")
    (description
      "Tokio-based single-threaded async runtime for the Actix ecosystem")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-rt-0.5
  (package
    (name "rust-sqlx-rt")
    (version "0.5.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-rt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1blslzshfsgk1glnk9p2dvv43n0bjq06my9rbzw7i4257v6hidsd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-actix-rt" ,rust-actix-rt-2)
         ("rust-async-native-tls" ,rust-async-native-tls-0.3)
         ("rust-async-rustls" ,rust-async-rustls-0.2)
         ("rust-async-std" ,rust-async-std-1)
         ("rust-native-tls" ,rust-native-tls-0.2)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
         ("rust-tokio-rustls" ,rust-tokio-rustls-0.22))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
      "Runtime abstraction used by SQLx, the Rust SQL toolkit. Not intended to be used directly.")
    (description
      "Runtime abstraction used by SQLx, the Rust SQL toolkit.  Not intended to be used
directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlformat-0.1
  (package
    (name "rust-sqlformat")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlformat" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m7z9g1yi3jszbl1c6vay3s49mmx70zm49g11f871vhpw0mr5dxl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-itertools" ,rust-itertools-0.10)
         ("rust-nom" ,rust-nom-7)
         ("rust-unicode-categories" ,rust-unicode-categories-0.1))))
    (home-page "https://github.com/shssoichiro/sqlformat-rs")
    (synopsis "Formats whitespace in a SQL string to make it easier to read")
    (description
      "Formats whitespace in a SQL string to make it easier to read")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-0.19
  (package
    (name "rust-rustls")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1mx6nzbplydy9khll4clsl35m6c1a2cgz9czr74swfgfzrsvdv9m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-log" ,rust-log-0.4)
         ("rust-ring" ,rust-ring-0.16)
         ("rust-sct" ,rust-sct-0.6)
         ("rust-webpki" ,rust-webpki-0.21))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Rustls is a modern TLS library written in Rust.")
    (description "Rustls is a modern TLS library written in Rust.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-ubyte-0.10
  (package
    (name "rust-ubyte")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ubyte" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zp8x55w57dkcy20vnc50izsjcgz7mj9b0d9z3i5v188wywnnxa2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/SergioBenitez/ubyte")
    (synopsis
      "A simple, complete, const-everything, saturating, human-friendly, no_std library for byte units.
")
    (description
      "This package provides a simple, complete, const-everything, saturating,
human-friendly, no_std library for byte units.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-stream-0.1
  (package
    (name "rust-tokio-stream")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-stream" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qwq0y21xprsql4v9y1cm1ymhgw66rznjmnjrjsii27zxy25852h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-util" ,rust-tokio-util-0.6))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.
")
    (description "Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))

(define-public rust-state-0.5
  (package
    (name "rust-state")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "state" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1rbd5zg3zsj95di88h4my346llaiyj89cp1nbr5h9lz6d59lzkw7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-loom" ,rust-loom-0.5))))
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
      `(#:skip-build? #t #:cargo-inputs (("rust-memchr" ,rust-memchr-2))))
    (home-page "https://github.com/SergioBenitez/stable-pattern")
    (synopsis "Stable port of std::str::Pattern and friends.")
    (description "Stable port of std::str::Pattern and friends.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-http-0.5
  (package
    (name "rust-rocket-http")
    (version "0.5.0-rc.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_http" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1gayw8bglx9kyph9xk76k55q8wvmwv6r1rgb2qisrz6j2bavgj13"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cookie" ,rust-cookie-0.15)
         ("rust-either" ,rust-either-1)
         ("rust-http" ,rust-http-0.2)
         ("rust-hyper" ,rust-hyper-0.14)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-mime" ,rust-mime-0.3)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-pear" ,rust-pear-0.2)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-ref-cast" ,rust-ref-cast-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-stable-pattern" ,rust-stable-pattern-0.1)
         ("rust-state" ,rust-state-0.5)
         ("rust-time" ,rust-time-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
         ("rust-uncased" ,rust-uncased-0.9)
         ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://rocket.rs")
    (synopsis
      "Types, traits, and parsers for HTTP requests, responses, and headers.
")
    (description
      "Types, traits, and parsers for HTTP requests, responses, and headers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-devise-core-0.3
  (package
    (name "rust-devise-core")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1l00qiih4z14ai0c3s16nlvw0kv4p07ygi6a0ms0knc78xpz87l4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.9)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-devise-codegen-0.3
  (package
    (name "rust-devise-codegen")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise_codegen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1cp7nnfwvjp6wfq11n0ffjjrwfa1wbsb58g1bz3ha6z5lvkp6g0j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-devise-core" ,rust-devise-core-0.3)
         ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-devise-0.3
  (package
    (name "rust-devise")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "15dmibnykic2a1ndi66shyvxmpfysnhf05lg2iv8871g0w5miish"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-devise-codegen" ,rust-devise-codegen-0.3)
         ("rust-devise-core" ,rust-devise-core-0.3))))
    (home-page "https://github.com/SergioBenitez/Devise")
    (synopsis "A library for devising derives and other procedural macros.")
    (description
      "This package provides a library for devising derives and other procedural
macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rocket-codegen-0.5
  (package
    (name "rust-rocket-codegen")
    (version "0.5.0-rc.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket_codegen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1fx8mr2ybxvsjnqnz9wzh1cvvfvlsz2if33im2xmifby5x3gmxb6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-devise" ,rust-devise-0.3)
         ("rust-glob" ,rust-glob-0.3)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-rocket-http" ,rust-rocket-http-0.5)
         ("rust-syn" ,rust-syn-1)
         ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://rocket.rs")
    (synopsis "Procedural macros for the Rocket web framework.")
    (description "Procedural macros for the Rocket web framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-multer-2
  (package
    (name "rust-multer")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "multer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0diqknyfg0m131bm19rll4abg34ad7k122arcwb5q7anhzk3b3sz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
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
         ("rust-tokio-util" ,rust-tokio-util-0.6)
         ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/rousan/multer-rs")
    (synopsis
      "An async parser for `multipart/form-data` content-type in Rust.")
    (description
      "An async parser for `multipart/form-data` content-type in Rust.")
    (license license:expat)))

(define-public rust-proc-macro2-diagnostics-0.9
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2-diagnostics" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1nmazlb1dkznjds7qwms7yxhi33ajc3isji2lsgx8r3lsqk9gwjb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1)
         ("rust-version-check" ,rust-version-check-0.9)
         ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "")
    (synopsis "Diagnostics for proc-macro2.")
    (description "Diagnostics for proc-macro2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pear-codegen-0.2
  (package
    (name "rust-pear-codegen")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pear_codegen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1l4209fi1n0wj110l12l4xpy32d1xffm61nm82vyq0r37ijcm9c2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.9)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
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
      `(#:skip-build? #t #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/fitzgen/inlinable_string")
    (synopsis
      "The `inlinable_string` crate provides the `InlinableString` type -- an owned, grow-able UTF-8 string that stores small strings inline and avoids heap-allocation -- and the `StringExt` trait which abstracts string operations over both `std::string::String` and `InlinableString` (or even your own custom string type).")
    (description
      "The `inlinable_string` crate provides the `InlinableString` type -- an owned,
grow-able UTF-8 string that stores small strings inline and avoids
heap-allocation -- and the `StringExt` trait which abstracts string operations
over both `std::string::String` and `InlinableString` (or even your own custom
string type).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pear-0.2
  (package
    (name "rust-pear")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pear" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "00l7llav8cidhclx0m2gxm267pfa90c7r2x7xbinij74qm0l5r0m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-inlinable-string" ,rust-inlinable-string-0.1)
         ("rust-pear-codegen" ,rust-pear-codegen-0.2)
         ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "")
    (synopsis "A pear is a fruit.")
    (description "This package provides a pear is a fruit.")
    (license (list license:expat license:asl2.0))))

(define-public rust-figment-0.10
  (package
    (name "rust-figment")
    (version "0.10.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "figment" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1pr2w6pldkkjavj1sacn9xiibzhlf13ply0gnnxan616qy9442vr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-atomic" ,rust-atomic-0.5)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-pear" ,rust-pear-0.2)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-serde-yaml" ,rust-serde-yaml-0.8)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-toml" ,rust-toml-0.5)
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
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/naim94a/binascii-rs")
    (synopsis
      "Useful no-std binascii operations including base64, base32 and base16 (hex)")
    (description
      "Useful no-std binascii operations including base64, base32 and base16 (hex)")
    (license license:expat)))

(define-public rust-rocket-0.5
  (package
    (name "rust-rocket")
    (version "0.5.0-rc.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rocket" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1wni9nf9f0d7hvsalh9adybff4dd1npir0qn72zibsx08a6c2w8a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-stream" ,rust-async-stream-0.3)
         ("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-atomic" ,rust-atomic-0.5)
         ("rust-atty" ,rust-atty-0.2)
         ("rust-binascii" ,rust-binascii-0.1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-either" ,rust-either-1)
         ("rust-figment" ,rust-figment-0.10)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-multer" ,rust-multer-2)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-ref-cast" ,rust-ref-cast-1)
         ("rust-rmp-serde" ,rust-rmp-serde-0.15)
         ("rust-rocket-codegen" ,rust-rocket-codegen-0.5)
         ("rust-rocket-http" ,rust-rocket-http-0.5)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-state" ,rust-state-0.5)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-time" ,rust-time-0.2)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-stream" ,rust-tokio-stream-0.1)
         ("rust-tokio-util" ,rust-tokio-util-0.6)
         ("rust-ubyte" ,rust-ubyte-0.10)
         ("rust-uuid" ,rust-uuid-0.8)
         ("rust-version-check" ,rust-version-check-0.9)
         ("rust-yansi" ,rust-yansi-0.5)
         ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://rocket.rs")
    (synopsis
      "Web framework with a focus on usability, security, extensibility, and speed.
")
    (description
      "Web framework with a focus on usability, security, extensibility, and speed.")
    (license (list license:expat license:asl2.0))))

(define-public rust-borsh-schema-derive-internal-0.9
  (package
    (name "rust-borsh-schema-derive-internal")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "borsh-schema-derive-internal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1h2i37xrbhxvdl32v94j2k8vlf45l4aaffgyv59iv8mzv2b5dgfd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "http://borsh.io")
    (synopsis "Schema Generator for Borsh
")
    (description "Schema Generator for Borsh")
    (license license:asl2.0)))

(define-public rust-borsh-derive-internal-0.9
  (package
    (name "rust-borsh-derive-internal")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "borsh-derive-internal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ra0qkc3a2ah08y4z8b40zximiwv2fzji2iab4g2sbrmgf5c4jal"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license license:asl2.0)))

(define-public rust-borsh-derive-0.9
  (package
    (name "rust-borsh-derive")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "borsh-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0xb7wkfa4l2lw6gi4lkfsfqa4b2dj5vpcdycwcc5sdrhy99cahb4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-borsh-derive-internal" ,rust-borsh-derive-internal-0.9)
         ("rust-borsh-schema-derive-internal"
          ,rust-borsh-schema-derive-internal-0.9)
         ("rust-proc-macro-crate" ,rust-proc-macro-crate-0.1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license license:asl2.0)))

(define-public rust-borsh-0.9
  (package
    (name "rust-borsh")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "borsh" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ylvjdlyfyfscyq5phmvgbh7vlgvy485wn8mj2lzz2qd4183dgqm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-borsh-derive" ,rust-borsh-derive-0.9)
         ("rust-hashbrown" ,rust-hashbrown-0.11))))
    (home-page "http://borsh.io")
    (synopsis "Binary Object Representation Serializer for Hashing
")
    (description "Binary Object Representation Serializer for Hashing")
    (license (list license:expat license:asl2.0))))

(define-public rust-rust-decimal-1
  (package
    (name "rust-rust-decimal")
    (version "1.23.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust_decimal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1y703zhl8qvvq4yrdi0i53nfzfpdqv01h16jp0823vphvgm6kp12"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-arbitrary" ,rust-arbitrary-1)
         ("rust-arrayvec" ,rust-arrayvec-0.7)
         ("rust-borsh" ,rust-borsh-0.9)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-diesel" ,rust-diesel-1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-postgres" ,rust-postgres-0.19)
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

(define-public rust-sha2-0.9
  (package
    (name "rust-sha2")
    (version "0.9.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "006q2f0ar26xcjxqz8zsncfgz86zqa5dkwlwv03rhx1rpzhs2n2d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-block-buffer" ,rust-block-buffer-0.9)
         ("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-cpufeatures" ,rust-cpufeatures-0.2)
         ("rust-digest" ,rust-digest-0.9)
         ("rust-opaque-debug" ,rust-opaque-debug-0.3)
         ("rust-sha2-asm" ,rust-sha2-asm-0.6))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis
      "Pure Rust implementation of the SHA-2 hash function family
including SHA-224, SHA-256, SHA-384, and SHA-512.
")
    (description
      "Pure Rust implementation of the SHA-2 hash function family including SHA-224,
SHA-256, SHA-384, and SHA-512.")
    (license (list license:expat license:asl2.0))))

(define-public rust-spki-0.5
  (package
    (name "rust-spki")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spki" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "09qaddm4kw01xm9638910bm4yqnshzh2p38lvc3kxkvc5b01ml24"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64ct" ,rust-base64ct-1)
         ("rust-der" ,rust-der-0.5)
         ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/spki")
    (synopsis
      "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their
associated AlgorithmIdentifiers (i.e. OIDs)
")
    (description
      "X.509 Subject Public Key Info (RFC5280) describing public keys as well as their
associated AlgorithmIdentifiers (i.e.  OIDs)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sha-1-0.9
  (package
    (name "rust-sha-1")
    (version "0.9.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha-1" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "19jibp8l9k5v4dnhj5kfhaczdfd997h22qz0hin6pw9wvc9ngkcr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-block-buffer" ,rust-block-buffer-0.9)
         ("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-cpufeatures" ,rust-cpufeatures-0.2)
         ("rust-digest" ,rust-digest-0.9)
         ("rust-opaque-debug" ,rust-opaque-debug-0.3)
         ("rust-sha1-asm" ,rust-sha1-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-1 hash function")
    (description "SHA-1 hash function")
    (license (list license:expat license:asl2.0))))

(define-public rust-pkcs5-0.4
  (package
    (name "rust-pkcs5")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0xhyi3k5p6lxb28ivcd1f3skdbmhzk0gamfry7q56pifx9xi8g6n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aes" ,rust-aes-0.7)
         ("rust-block-modes" ,rust-block-modes-0.8)
         ("rust-der" ,rust-der-0.5)
         ("rust-des" ,rust-des-0.7)
         ("rust-hmac" ,rust-hmac-0.11)
         ("rust-pbkdf2" ,rust-pbkdf2-0.9)
         ("rust-scrypt" ,rust-scrypt-0.8)
         ("rust-sha-1" ,rust-sha-1-0.9)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-spki" ,rust-spki-0.5))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs5")
    (synopsis
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #5:
Password-Based Cryptography Specification Version 2.1 (RFC 8018)
")
    (description
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #5:
Password-Based Cryptography Specification Version 2.1 (RFC 8018)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs8-0.8
  (package
    (name "rust-pkcs8")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs8" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1l29h4mrgi2kpsl98jzky3ni5by3xa1sc6db9yd8l1i1p0zxmavw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-der" ,rust-der-0.5)
         ("rust-pkcs5" ,rust-pkcs5-0.4)
         ("rust-rand-core" ,rust-rand-core-0.6)
         ("rust-spki" ,rust-spki-0.5)
         ("rust-subtle" ,rust-subtle-2)
         ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs8")
    (synopsis
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #8:
Private-Key Information Syntax Specification (RFC 5208), with additional
support for PKCS#8v2 asymmetric key packages (RFC 5958)
")
    (description
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #8:
Private-Key Information Syntax Specification (RFC 5208), with additional support
for PKCS#8v2 asymmetric key packages (RFC 5958)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pem-rfc7468-0.3
  (package
    (name "rust-pem-rfc7468")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pem-rfc7468" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0c7vrrksg8fqzxb7q4clzl14f0qnqky7jqspjqi4pailiybmvph1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pem-rfc7468")
    (synopsis
      "PEM Encoding (RFC 7468) for PKIX, PKCS, and CMS Structures, implementing a
strict subset of the original Privacy-Enhanced Mail encoding intended
specifically for use with cryptographic keys, certificates, and other messages.
Provides a no_std-friendly, constant-time implementation suitable for use with
cryptographic private keys.
")
    (description
      "PEM Encoding (RFC 7468) for PKIX, PKCS, and CMS Structures, implementing a
strict subset of the original Privacy-Enhanced Mail encoding intended
specifically for use with cryptographic keys, certificates, and other messages.
Provides a no_std-friendly, constant-time implementation suitable for use with
cryptographic private keys.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-der-derive-0.5
  (package
    (name "rust-der-derive")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "der_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zw4p6yqklv4i76ms2a0gcmna648337r379d5ljgpbir5cyqylrs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-error" ,rust-proc-macro-error-1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der/derive")
    (synopsis
      "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (description
      "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (license (list license:asl2.0 license:expat))))

(define-public rust-crypto-bigint-0.3
  (package
    (name "rust-crypto-bigint")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-bigint" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08gx92sj93hk2smqy4nvk8lmpjjjqm7a9ps22q3pxqqxzbas3ih3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-generic-array" ,rust-generic-array-0.14)
         ("rust-rand-core" ,rust-rand-core-0.6)
         ("rust-rlp" ,rust-rlp-0.5)
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

(define-public rust-const-oid-0.7
  (package
    (name "rust-const-oid")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const-oid" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1wwl3cncd8p2fa54vzmghflh4nh9ml02xfbv38nf5ziifh28riz4"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/RustCrypto/formats/tree/master/const-oid")
    (synopsis
      "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard
as defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e. embedded) support
")
    (description
      "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard as
defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e.  embedded) support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-der-0.5
  (package
    (name "rust-der")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "der" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0p3h7nszn7jhjacpmkjrcyx5g8p3ma1qhxfy3397m7l3fdfq26b9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-const-oid" ,rust-const-oid-0.7)
         ("rust-crypto-bigint" ,rust-crypto-bigint-0.3)
         ("rust-der-derive" ,rust-der-derive-0.5)
         ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.3)
         ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der")
    (synopsis
      "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets
")
    (description
      "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pkcs1-0.3
  (package
    (name "rust-pkcs1")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs1" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0813szfx13n4xl6l19m3lwj7pqgljqwc6ipxhr2dv0yc9k06d3x7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-der" ,rust-der-0.5)
         ("rust-pkcs8" ,rust-pkcs8-0.8)
         ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/pkcs1")
    (synopsis
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #1:
RSA Cryptography Specifications Version 2.2 (RFC 8017)
")
    (description
      "Pure Rust implementation of Public-Key Cryptography Standards (PKCS) #1: RSA
Cryptography Specifications Version 2.2 (RFC 8017)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-num-bigint-dig-0.8
  (package
    (name "rust-num-bigint-dig")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-bigint-dig" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "04g0blkdy0r347qk2mk0wk581kd2sljhka8hanyay1ll5wxifvan"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libm" ,rust-libm-0.2)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-iter" ,rust-num-iter-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-serde" ,rust-serde-1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/dignifiedquire/num-bigint")
    (synopsis "Big integer implementation for Rust")
    (description "Big integer implementation for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-rsa-0.6
  (package
    (name "rust-rsa")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rsa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02viiiylxpk2hx5h5qrpm4lcd8ildvafbw0rn6rx44wnqia2gwjc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-digest" ,rust-digest-0.10)
         ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-iter" ,rust-num-iter-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-pkcs1" ,rust-pkcs1-0.3)
         ("rust-pkcs8" ,rust-pkcs8-0.8)
         ("rust-rand-core" ,rust-rand-core-0.6)
         ("rust-serde" ,rust-serde-1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-subtle" ,rust-subtle-2)
         ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/RSA")
    (synopsis "Pure Rust RSA implementation")
    (description "Pure Rust RSA implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11kjfh41h7i33sskb8i36kl03260rrjw74nb2njhbzr5ddxn848s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aho-corasick" ,rust-aho-corasick-0.7)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-regex-syntax" ,rust-regex-syntax-0.6))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
      "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (description
      "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-paste-1
  (package
    (name "rust-paste")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1z15h1rnq1wcacpcvgm77djl3413gs1nlhmn90qpcvjx2c2hwlhc"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis "Macros for all your token pasting needs")
    (description "Macros for all your token pasting needs")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-bigint-0.3
  (package
    (name "rust-num-bigint")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-bigint" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hxjawydfbnxqyqza0n2sh5sk9rslm0wsn7xdh53dwnby8rphvsz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-arbitrary" ,rust-arbitrary-0.4)
         ("rust-autocfg" ,rust-autocfg-1)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-quickcheck" ,rust-quickcheck-0.9)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-num/num-bigint")
    (synopsis "Big integer implementation for Rust")
    (description "Big integer implementation for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-mac-address-1
  (package
    (name "rust-mac-address")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mac_address" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jhhi0liylrnbaq0gnlzd3j7zxh7z8mkmifc4l3xcja5130in7fz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-nix" ,rust-nix-0.23)
         ("rust-serde" ,rust-serde-1)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rep-nop/mac_address")
    (synopsis "Cross-platform retrieval of a network interface MAC address.")
    (description
      "Cross-platform retrieval of a network interface MAC address.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libsqlite3-sys-0.24
  (package
    (name "rust-libsqlite3-sys")
    (version "0.24.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libsqlite3-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "054xxfz7w4xy2iyqpv4g9vajwrkcxc0sgi7vq4y4bl67f3jlb1w9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.59)
         ("rust-cc" ,rust-cc-1)
         ("rust-openssl-sys" ,rust-openssl-sys-0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/rusqlite/rusqlite")
    (synopsis "Native bindings to the libsqlite3 library")
    (description "Native bindings to the libsqlite3 library")
    (license license:expat)))

(define-public rust-hmac-0.12
  (package
    (name "rust-hmac")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-digest" ,rust-digest-0.10))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis
      "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
      "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (license (list license:expat license:asl2.0))))

(define-public rust-hkdf-0.12
  (package
    (name "rust-hkdf")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hkdf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0dyl16cf15hka32hv3l7dwgr3xj3brpfr27iyrbpdhlzdfgh46kr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-hmac" ,rust-hmac-0.12))))
    (home-page "https://github.com/RustCrypto/KDFs/")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
      "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-libgit2-sys-0.12
  (package
    (name "rust-libgit2-sys")
    (version "0.12.26+1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "153l8nvz9p8vyd5840xi6fwblvhpn3c33jwdwsznyq4f4jcwiq8r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
         ("rust-libz-sys" ,rust-libz-sys-1)
         ("rust-openssl-sys" ,rust-openssl-sys-0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Native bindings to the libgit2 library")
    (description "Native bindings to the libgit2 library")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-0.13
  (package
    (name "rust-git2")
    (version "0.13.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "git2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1mppxyjzi69m879mwpin4d9jljanwaijlx3f5w3fdh143g62k4pj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libgit2-sys" ,rust-libgit2-sys-0.12)
         ("rust-log" ,rust-log-0.4)
         ("rust-openssl-probe" ,rust-openssl-probe-0.1)
         ("rust-openssl-sys" ,rust-openssl-sys-0.9)
         ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis
      "Bindings to libgit2 for interoperating with git repositories. This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.
")
    (description
      "Bindings to libgit2 for interoperating with git repositories.  This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-intrusive-0.4
  (package
    (name "rust-futures-intrusive")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-intrusive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gmnn86ifc2ngmwf3mpiw00kmxm8m2wxxxqnchmpraj6mj97a032"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-lock-api" ,rust-lock-api-0.4)
         ("rust-parking-lot" ,rust-parking-lot-0.11))))
    (home-page "https://github.com/Matthias247/futures-intrusive")
    (synopsis
      "Futures based on intrusive data structures - for std and no-std environments.
")
    (description
      "Futures based on intrusive data structures - for std and no-std environments.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-macro-0.3
  (package
    (name "rust-futures-macro")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-macro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "04pmj5xfk5rdhlj69wc7w3zvdg3xardg8srig96lszrk00wf3h9k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The futures-rs procedural macro implementations.
")
    (description "The futures-rs procedural macro implementations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-io-0.3
  (package
    (name "rust-futures-io")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-io" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0swn29fysas36ikk5aw55104fi98117amvgxw9g96pjs5ab4ah7w"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
      "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the futures-rs library.
")
    (description
      "The `AsyncRead`, `AsyncWrite`, `AsyncSeek`, and `AsyncBufRead` traits for the
futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-util-0.3
  (package
    (name "rust-futures-util")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sh3wqi8p36csjffy0irq8nlx9shqxp7z4dsih6bknarsvaspdyq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures" ,rust-futures-0.1)
         ("rust-futures-channel" ,rust-futures-channel-0.3)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-io" ,rust-futures-io-0.3)
         ("rust-futures-macro" ,rust-futures-macro-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3)
         ("rust-futures-task" ,rust-futures-task-0.3)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-pin-utils" ,rust-pin-utils-0.1)
         ("rust-slab" ,rust-slab-0.4)
         ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
      "Common utilities and extension traits for the futures-rs library.
")
    (description
      "Common utilities and extension traits for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-task-0.3
  (package
    (name "rust-futures-task")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-task" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0skpiz2ljisywajv79p70yapfwhkqhb39wxy3f09v47mdfbnmijp"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Tools for working with tasks.
")
    (description "Tools for working with tasks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-executor-0.3
  (package
    (name "rust-futures-executor")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-executor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "19mq96kwgf06axgdc2fbrjhqzdnxww9vw6cz8b82gqr9z86bj84l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-task" ,rust-futures-task-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis
      "Executors for asynchronous tasks based on the futures-rs library.
")
    (description
      "Executors for asynchronous tasks based on the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-sink-0.3
  (package
    (name "rust-futures-sink")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-sink" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0s58gx5yw1a21xviw2qgc0wzk225vgn4kbzddrp141m3kw9kw5i1"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The asynchronous `Sink` trait for the futures-rs library.
")
    (description "The asynchronous `Sink` trait for the futures-rs library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-core-0.3
  (package
    (name "rust-futures-core")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1lqhc6mqklh5bmkpr77p42lqwjj8gaskk5ba2p3kl1z4nw2gs28c"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "The core traits and types in for the `futures` library.
")
    (description "The core traits and types in for the `futures` library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-channel-0.3
  (package
    (name "rust-futures-channel")
    (version "0.3.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-channel" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0420lz2fmxa356ax1rp2sqi7b27ykfhvq4w9f1sla4hlp7j3q263"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3))))
    (home-page "https://rust-lang.github.io/futures-rs")
    (synopsis "Channels for asynchronous communication using futures-rs.
")
    (description "Channels for asynchronous communication using futures-rs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zeroize-1
  (package
    (name "rust-zeroize")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zeroize" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "05q3ynz820xxpx0zqcf28mgx7bpzak2x9qcwhq52hgzxia5p5dby"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis
      "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (description
      "Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
'optimized away' by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))

(define-public rust-nanorand-0.7
  (package
    (name "rust-nanorand")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nanorand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1hr60b8zlfy7mxjcwx2wfmhpkx7vfr3v9x12shmv1c10b0y32lba"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-getrandom" ,rust-getrandom-0.2)
         ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/Absolucy/nanorand-rs")
    (synopsis "A tiny, fast, zero-dep library for random number generation.")
    (description
      "This package provides a tiny, fast, zero-dep library for random number
generation.")
    (license license:zlib)))

(define-public rust-flume-0.10
  (package
    (name "rust-flume")
    (version "0.10.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flume" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0sn7hx83gajfifp93xn4pj5c4x3ja06sr47aq55sa30cklch6g44"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3)
         ("rust-nanorand" ,rust-nanorand-0.7)
         ("rust-pin-project" ,rust-pin-project-1)
         ("rust-spin" ,rust-spin-0.9))))
    (home-page "https://github.com/zesterer/flume")
    (synopsis "A blazingly fast multi-producer channel")
    (description
      "This package provides a blazingly fast multi-producer channel")
    (license (list license:asl2.0 license:expat))))

(define-public rust-event-listener-2
  (package
    (name "rust-event-listener")
    (version "2.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "event-listener" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wgbyhw4piqrrln6s61y3sc06wd3liwzzz18hazji3wk2ya31wvp"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/smol-rs/event-listener")
    (synopsis "Notify async tasks or threads")
    (description "Notify async tasks or threads")
    (license (list license:asl2.0 license:expat))))

(define-public rust-encoding-rs-0.8
  (package
    (name "rust-encoding-rs")
    (version "0.8.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0azc6rblf75vd862ymjahdfch27j1sshb7zynshrx7ywi5an6llq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-packed-simd-2" ,rust-packed-simd-2-0.3)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://docs.rs/encoding_rs/")
    (synopsis "A Gecko-oriented implementation of the Encoding Standard")
    (description
      "This package provides a Gecko-oriented implementation of the Encoding Standard")
    (license license:bsd-3)))

(define-public rust-dirs-4
  (package
    (name "rust-dirs")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0n8020zl4f0frfnzvgb9agvk4a14i1kjz4daqnxkgslndwmaffna"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-dirs-sys" ,rust-dirs-sys-0.3))))
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis
      "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
      "This package provides a tiny low-level library that provides platform-specific
standard locations of directories for config, cache and other data on Linux,
Windows, macOS and Redox by leveraging the mechanisms defined by the XDG
base/user directory specifications on Linux, the Known Folder API on Windows,
and the Standard Directory guidelines on macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-valuable-derive-0.1
  (package
    (name "rust-valuable-derive")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "valuable-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0cjvqljzsj891cjzlwv0ihrv4m0n5211a6pr6b7cz42ich66ji4x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/tokio-rs/valuable")
    (synopsis "Macros for the `valuable` crate.")
    (description "Macros for the `valuable` crate.")
    (license license:expat)))

(define-public rust-valuable-0.1
  (package
    (name "rust-valuable")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "valuable" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0v9gp3nkjbl30z0fd56d8mx7w1csk86wwjhfjhr400wh9mfpw2w3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-valuable-derive" ,rust-valuable-derive-0.1))))
    (home-page "https://github.com/tokio-rs/valuable")
    (synopsis
      "Object-safe value inspection, used to pass un-typed structured data across trait-object boundaries.
")
    (description
      "Object-safe value inspection, used to pass un-typed structured data across
trait-object boundaries.")
    (license license:expat)))

(define-public rust-tracing-core-0.1
  (package
    (name "rust-tracing-core")
    (version "0.1.26")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0bq7c1y28hi7mli25pj9iljam4vcnlqk7zf2k3a8c67822kqqk7m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-valuable" ,rust-valuable-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing.
")
    (description "Core primitives for application-level tracing.")
    (license license:expat)))

(define-public rust-tracing-attributes-0.1
  (package
    (name "rust-tracing-attributes")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-attributes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0z2bjwkh0azvxw0fqcn36iy7r33wgaq559xp3n5gk6blav9qlsyc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://tokio.rs")
    (synopsis
      "Procedural macro attributes for automatically instrumenting functions.
")
    (description
      "Procedural macro attributes for automatically instrumenting functions.")
    (license license:expat)))

(define-public rust-tracing-0.1
  (package
    (name "rust-tracing")
    (version "0.1.34")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02bx698j7p50dcg01s3x26swpjs2lcrly32ghklhz7x78k5ws3jx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
         ("rust-tracing-attributes" ,rust-tracing-attributes-0.1)
         ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust.
")
    (description "Application-level tracing for Rust.")
    (license license:expat)))

(define-public rust-generator-0.7
  (package
    (name "rust-generator")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generator" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1vhj3f0rf4mlh5vz7pz5rxmgry1cc62x21mf9ld1r292m2f2gnf1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-rustversion" ,rust-rustversion-1)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Xudong-Huang/generator-rs.git")
    (synopsis "Stackfull Generator Library in Rust")
    (description "Stackfull Generator Library in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-loom-0.5
  (package
    (name "rust-loom")
    (version "0.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "loom" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02a30cv9l2afjq5bg42hgcjspx8fgwyij0cf9saw8b73539wgigd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-generator" ,rust-generator-0.7)
         ("rust-pin-utils" ,rust-pin-utils-0.1)
         ("rust-scoped-tls" ,rust-scoped-tls-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tracing" ,rust-tracing-0.1)
         ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/tokio-rs/loom")
    (synopsis "Permutation testing for concurrent code")
    (description "Permutation testing for concurrent code")
    (license license:expat)))

(define-public rust-crossbeam-utils-0.8
  (package
    (name "rust-crossbeam-utils")
    (version "0.8.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0f6b3xrbyc3yx0qa1digmy48mxmh58359kv34qy6ws5p433j9w8b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-loom" ,rust-loom-0.5))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description "Utilities for concurrent programming")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-queue-0.3
  (package
    (name "rust-crossbeam-queue")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-queue" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "18kpa83m1ivz3230f4ax95f9pgcclj227rg4y1w5fyja1x0dh98z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-queue")
    (synopsis "Concurrent queues")
    (description "Concurrent queues")
    (license (list license:expat license:asl2.0))))

(define-public rust-crc-catalog-1
  (package
    (name "rust-crc-catalog")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc-catalog" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "00qlxgzg15fnyx6nwviibz94rjw803l2avi2k3shjfx0dnsyvbnc"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/akhilles/crc-catalog.git")
    (synopsis
      "Catalog of CRC algorithms (generated from http://reveng.sourceforge.net/crc-catalogue) expressed as simple Rust structs.")
    (description
      "Catalog of CRC algorithms (generated from
http://reveng.sourceforge.net/crc-catalogue) expressed as simple Rust structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crc-2
  (package
    (name "rust-crc")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08qfahmly0n5j27g1vkqx9s6mxhm8k4dsp61ykskazyabdlrmz29"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crc-catalog" ,rust-crc-catalog-1))))
    (home-page "https://github.com/mrhooray/crc-rs.git")
    (synopsis
      "Rust implementation of CRC(16, 32, 64) with support of various standards")
    (description
      "Rust implementation of CRC(16, 32, 64) with support of various standards")
    (license (list license:expat license:asl2.0))))

(define-public rust-bstr-0.2
  (package
    (name "rust-bstr")
    (version "0.2.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bstr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08rjbhysy6gg27db2h3pnhvr2mlr5vkj797i9625kwg8hgrnjdds"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-regex-automata" ,rust-regex-automata-0.1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/BurntSushi/bstr")
    (synopsis "A string type that is not required to be valid UTF-8.")
    (description
      "This package provides a string type that is not required to be valid UTF-8.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bigdecimal-0.2
  (package
    (name "rust-bigdecimal")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bigdecimal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0mkja6m149apsnbw5narzcxmb9b4ish58gn4qvvys03jwdi0brfi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-bigint" ,rust-num-bigint-0.3)
         ("rust-num-integer" ,rust-num-integer-0.1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/akubera/bigdecimal-rs")
    (synopsis "Arbitrary precision decimal numbers")
    (description "Arbitrary precision decimal numbers")
    (license (list license:expat license:asl2.0))))

(define-public rust-atoi-0.4
  (package
    (name "rust-atoi")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atoi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "19fdm7gxwp98y67ghyis841vy5ka7hc1afm9cfa69qn0bzh9cs31"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/pacman82/atoi-rs")
    (synopsis "Parse integers directly from `[u8]` slices in safe code")
    (description "Parse integers directly from `[u8]` slices in safe code")
    (license license:expat)))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.124")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0l0f1gvhxp9xpx5w5bd8aj55x8sg59idlqfiqsqpmwlqkpniz911"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-getrandom-0.2
  (package
    (name "rust-getrandom")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getrandom" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1b8588g2z36s1082licl623lclbdz9jp03gnz39bi0qwjnc0rrwv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
         ("rust-js-sys" ,rust-js-sys-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
         ("rust-wasi" ,rust-wasi_0_10_0+wasi-snapshot-preview1)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/rust-random/getrandom")
    (synopsis
      "A small cross-platform library for retrieving random data from system source")
    (description
      "This package provides a small cross-platform library for retrieving random data
from system source")
    (license (list license:expat license:asl2.0))))

(define-public rust-ahash-0.7
  (package
    (name "rust-ahash")
    (version "0.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ahash" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0isw672fiwx8cjl040jrck6pi85xcszkz6q0xsqkiy6qjl31mdgw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-const-random" ,rust-const-random-0.1)
         ("rust-getrandom" ,rust-getrandom-0.2)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
      "A non-cryptographic hash function using AES-NI for high performance")
    (description
      "This package provides a non-cryptographic hash function using AES-NI for high
performance")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-core-0.5
  (package
    (name "rust-sqlx-core")
    (version "0.5.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1icx9llk1j5g1ncqw7lya1y00fqpwpimkka2lfnxqpfg3ja63374"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ahash" ,rust-ahash-0.7)
         ("rust-atoi" ,rust-atoi-0.4)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-bigdecimal" ,rust-bigdecimal-0.2)
         ("rust-bit-vec" ,rust-bit-vec-0.6)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-bstr" ,rust-bstr-0.2)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-crc" ,rust-crc-2)
         ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
         ("rust-digest" ,rust-digest-0.10)
         ("rust-dirs" ,rust-dirs-4)
         ("rust-either" ,rust-either-1)
         ("rust-encoding-rs" ,rust-encoding-rs-0.8)
         ("rust-event-listener" ,rust-event-listener-2)
         ("rust-flume" ,rust-flume-0.10)
         ("rust-futures-channel" ,rust-futures-channel-0.3)
         ("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-executor" ,rust-futures-executor-0.3)
         ("rust-futures-intrusive" ,rust-futures-intrusive-0.4)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-generic-array" ,rust-generic-array-0.14)
         ("rust-git2" ,rust-git2-0.13)
         ("rust-hashlink" ,rust-hashlink-0.7)
         ("rust-hex" ,rust-hex-0.4)
         ("rust-hkdf" ,rust-hkdf-0.12)
         ("rust-hmac" ,rust-hmac-0.12)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-ipnetwork" ,rust-ipnetwork-0.17)
         ("rust-itoa" ,rust-itoa-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libsqlite3-sys" ,rust-libsqlite3-sys-0.24)
         ("rust-log" ,rust-log-0.4)
         ("rust-mac-address" ,rust-mac-address-1)
         ("rust-md-5" ,rust-md-5-0.10)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-num-bigint" ,rust-num-bigint-0.3)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-paste" ,rust-paste-1)
         ("rust-percent-encoding" ,rust-percent-encoding-2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-regex" ,rust-regex-1)
         ("rust-rsa" ,rust-rsa-0.6)
         ("rust-rust-decimal" ,rust-rust-decimal-1)
         ("rust-rustls" ,rust-rustls-0.19)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-sha-1" ,rust-sha-1-0.10)
         ("rust-sha2" ,rust-sha2-0.10)
         ("rust-smallvec" ,rust-smallvec_1_7_0)
         ("rust-sqlformat" ,rust-sqlformat-0.1)
         ("rust-sqlx-rt" ,rust-sqlx-rt-0.5)
         ("rust-stringprep" ,rust-stringprep-0.1)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-time" ,rust-time-0.2)
         ("rust-tokio-stream" ,rust-tokio-stream-0.1)
         ("rust-url" ,rust-url-2)
         ("rust-uuid" ,rust-uuid-0.8)
         ("rust-webpki" ,rust-webpki-0.21)
         ("rust-webpki-roots" ,rust-webpki-roots-0.21)
         ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
      "Core of SQLx, the rust SQL toolkit. Not intended to be used directly.")
    (description
      "Core of SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-0.5
  (package
    (name "rust-sqlx")
    (version "0.5.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sqlx" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "16q1cqlhjwan1sbzbsz2z2x9dpacic7vnnzc5y8hvfjjas07662m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-sqlx-core" ,rust-sqlx-core-0.5)
         ("rust-sqlx-macros" ,rust-sqlx-macros-0.5))))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
      "ð\x9f§° The Rust SQL Toolkit. An async, pure Rust SQL crate featuring compile-time checked queries without a DSL. Supports PostgreSQL, MySQL, and SQLite.")
    (description
      "ð\x9f§° The Rust SQL Toolkit.  An async, pure Rust SQL crate featuring compile-time
checked queries without a DSL.  Supports PostgreSQL, MySQL, and SQLite.")
    (license (list license:expat license:asl2.0))))

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
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-anyhow" ,rust-anyhow-1) ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/perdumonocle/sql-builder.git")
    (synopsis "Simple SQL code generator.")
    (description "Simple SQL code generator.")
    (license license:expat)))

(define-public rust-libsodium-sys-0.2
  (package
    (name "rust-libsodium-sys")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libsodium-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zcjka23grayr8kjrgbada6vwagp0kkni9m45v0gpbanrn3r6xvb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-pkg-config" ,rust-pkg-config-0.3)
         ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/sodiumoxide/sodiumoxide.git")
    (synopsis "FFI binding to libsodium")
    (description "FFI binding to libsodium")
    (license (list license:expat license:asl2.0))))

(define-public rust-sodiumoxide-0.2
  (package
    (name "rust-sodiumoxide")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sodiumoxide" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0a00rcp2vphrs8qh0477rzs6lhsng1m5i0l4qamagnf2nsnf6sz2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ed25519" ,rust-ed25519-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libsodium-sys" ,rust-libsodium-sys-0.2)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/sodiumoxide/sodiumoxide")
    (synopsis "Fast cryptographic library for Rust (bindings to libsodium)")
    (description "Fast cryptographic library for Rust (bindings to libsodium)")
    (license (list license:expat license:asl2.0))))

(define-public rust-rmp-0.8
  (package
    (name "rust-rmp")
    (version "0.8.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rmp" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "17rw803xv84csxgd654g7q64kqf9zgkvhsn8as3dbmlg6mr92la4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Pure Rust MessagePack serialization implementation")
    (description "Pure Rust MessagePack serialization implementation")
    (license license:expat)))

(define-public rust-rmp-serde-1
  (package
    (name "rust-rmp-serde")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rmp-serde" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0zilg9zhx2pmyd889hnnzcfzf34hk49g7wynldgij4314w6nny15"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-rmp" ,rust-rmp-0.8)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Serde bindings for RMP")
    (description "Serde bindings for RMP")
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
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-1)
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
    (arguments `(#:skip-build? #t))
    (home-page "")
    (synopsis
      "a package for determining the minimum span of one vector within another")
    (description
      "a package for determining the minimum span of one vector within another")
    (license license:expat)))

(define-public rust-itertools-0.10
  (package
    (name "rust-itertools")
    (version "0.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qy55fqbaisr9qgbn7cvdvqlfqbh1f4ddf99zwan56z7l6gx3ad9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-either" ,rust-either-1))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fs-err-2
  (package
    (name "rust-fs-err")
    (version "2.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs-err" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0bk5fmwyk8b3lgfl5mi133s743hwq3z6awgvi6pd75d48nirzmsv"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/andrewhickman/fs-err")
    (synopsis
      "A drop-in replacement for std::fs with more helpful error messages.")
    (description
      "This package provides a drop-in replacement for std::fs with more helpful error
messages.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hashbrown-0.12
  (package
    (name "rust-hashbrown")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0n0pvw03ljspflqwkybjavdi2mfphyzvvhg3qskacbxrhw2x88cc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ahash" ,rust-ahash-0.7)
         ("rust-bumpalo" ,rust-bumpalo-3)
         ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
         ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "A Rust port of Google's SwissTable hash map")
    (description
      "This package provides a Rust port of Google's SwissTable hash map")
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
    (arguments `(#:skip-build? #t))
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
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-dlv-list" ,rust-dlv-list-0.3)
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
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-ordered-multimap" ,rust-ordered-multimap-0.4)
         ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/zonyitoo/rust-ini")
    (synopsis "An Ini configuration file parsing library in Rust")
    (description "An Ini configuration file parsing library in Rust")
    (license license:expat)))

(define-public rust-ron-0.7
  (package
    (name "rust-ron")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ron" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0y6n0cpgkv9cnj411ipk86gvwrhxs1hb64m5hrwcjfp4mp51x1hv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-bitflags" ,rust-bitflags-1)
         ("rust-indexmap" ,rust-indexmap-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/ron-rs/ron")
    (synopsis "Rusty Object Notation")
    (description "Rusty Object Notation")
    (license (list license:expat license:asl2.0))))

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
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pest" ,rust-pest-2)
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
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "config" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "06xk2846zsa239h2jr34jbnz9d8hyz4d6m9v9q1bbpvf9fviga9y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
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

(define-public rust-scanlex-0.1
  (package
    (name "rust-scanlex")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scanlex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1nrkq1kjwf3v084pndiq18yx6vqsivlqr6jllyg94911axqmv308"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/stevedonovan/scanlex.git")
    (synopsis "a simple lexical scanner for parsing text into tokens")
    (description "a simple lexical scanner for parsing text into tokens")
    (license license:expat)))

(define-public rust-chrono-english-0.1
  (package
    (name "rust-chrono-english")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono-english" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0vqdl2bfyv224xv2xnqa9rsnbn89pjhzbhvrqs47sjpblyfr0ggp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-chrono" ,rust-chrono-0.4) ("rust-scanlex" ,rust-scanlex-0.1))))
    (home-page "https://github.com/stevedonovan/chrono-english.git")
    (synopsis "parses simple English dates, inspired by Linux date command")
    (description "parses simple English dates, inspired by Linux date command")
    (license license:expat)))

(define-public rust-zerocopy-derive-0.3
  (package
    (name "rust-zerocopy-derive")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zerocopy-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "17rab2i1vwmxcr7c6r6xv55nhy41wlay0lpfcyl4vqpgh8mwiyx0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-syn" ,rust-syn-1)
         ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page
      "https://fuchsia.googlesource.com/fuchsia/+/HEAD/src/lib/zerocopy/zerocopy-derive")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description "Custom derive for traits from the zerocopy crate")
    (license #f)))

(define-public rust-zerocopy-0.6
  (package
    (name "rust-zerocopy")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zerocopy" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0dpj4nd9v56wy93ahjkp95znjzj91waqvidqch8gxwdwq661hbrk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-zerocopy-derive" ,rust-zerocopy-derive-0.3))))
    (home-page
      "https://fuchsia.googlesource.com/fuchsia/+/HEAD/src/lib/zerocopy")
    (synopsis "Utilities for zero-copy parsing and serialization")
    (description "Utilities for zero-copy parsing and serialization")
    (license #f)))

(define-public rust-uuid-macro-internal-1
  (package
    (name "rust-uuid-macro-internal")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uuid-macro-internal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "06r6ngqrrr76jlcl904kir76fgbz3xskr1rbriqmfxmgbm77nd8r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "")
    (synopsis "Private implementation details of the uuid! macro.")
    (description "Private implementation details of the uuid! macro.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sha1-smol-1
  (package
    (name "rust-sha1-smol")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha1_smol" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "04nhbhvsk5ms1zbshs80iq5r1vjszp2xnm9f0ivj38q3dhc4f6mf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/mitsuhiko/sha1-smol")
    (synopsis "Minimal dependency free implementation of SHA1 for Rust.")
    (description "Minimal dependency free implementation of SHA1 for Rust.")
    (license license:bsd-3)))

(define-public rust-md5-asm-0.5
  (package
    (name "rust-md5-asm")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md5-asm" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1ixmkg8j7sqy9zln6pz9xi2dl2d9zpm8pz6p49za47n1bvradfbk"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of MD5 compression function")
    (description "Assembly implementation of MD5 compression function")
    (license license:expat)))

(define-public rust-typenum-1
  (package
    (name "rust-typenum")
    (version "1.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typenum" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11yrvz1vd43gqv738yw1v75rzngjbs7iwcgzjy3cq5ywkv2imy6w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-scale-info" ,rust-scale-info-1))))
    (home-page "https://github.com/paholg/typenum")
    (synopsis
      "Typenum is a Rust library for type-level numbers evaluated at
    compile time. It currently supports bits, unsigned integers, and signed
    integers. It also provides a type-level array of type-level numbers, but its
    implementation is incomplete.")
    (description
      "Typenum is a Rust library for type-level numbers evaluated at     compile time.
It currently supports bits, unsigned integers, and signed     integers.  It also
provides a type-level array of type-level numbers, but its     implementation is
incomplete.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-common-0.1
  (package
    (name "rust-crypto-common")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-common" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1s1wpm88qlrp079mzh3dlxm9vbqs4ch016yp9pzhcdjygfi2r5ap"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-generic-array" ,rust-generic-array-0.14)
         ("rust-rand-core" ,rust-rand-core-0.6)
         ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Common cryptographic traits")
    (description "Common cryptographic traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-digest-0.10
  (package
    (name "rust-digest")
    (version "0.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "digest" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "01nmj9cci5qdm4q4wlmz104rzr68d5m823kdzd95bypslq68dyzj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-blobby" ,rust-blobby-0.3)
         ("rust-block-buffer" ,rust-block-buffer-0.10)
         ("rust-crypto-common" ,rust-crypto-common-0.1)
         ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for cryptographic hash functions")
    (description "Traits for cryptographic hash functions")
    (license (list license:expat license:asl2.0))))

(define-public rust-md-5-0.10
  (package
    (name "rust-md-5")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md-5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "10h5kna43cpggp9hy1hz4zb1qpixdl4anf3hdj3gfwhb3sr4d1k5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-digest" ,rust-digest-0.10)
         ("rust-md5-asm" ,rust-md5-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "MD5 hash function")
    (description "MD5 hash function")
    (license (list license:expat license:asl2.0))))

(define-public rust-atomic-0.5
  (package
    (name "rust-atomic")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atomic" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0k135q1qfmxxyzrlhr47r0j38r5fnd4163rgl552qxyagrk853dq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1))))
    (home-page "https://github.com/Amanieu/atomic-rs")
    (synopsis "Generic Atomic<T> wrapper type")
    (description "Generic Atomic<T> wrapper type")
    (license (list license:asl2.0 license:expat))))

(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uuid" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1w668dw9jq0dz24smh1v3w3ilzi6fcs45vc702hnwkbc8lcx7z4c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-arbitrary" ,rust-arbitrary-1)
         ("rust-atomic" ,rust-atomic-0.5)
         ("rust-getrandom" ,rust-getrandom-0.2)
         ("rust-md-5" ,rust-md-5-0.10)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-serde" ,rust-serde-1)
         ("rust-sha1-smol" ,rust-sha1-smol-1)
         ("rust-slog" ,rust-slog-2)
         ("rust-uuid-macro-internal" ,rust-uuid-macro-internal-1)
         ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
      "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-atuin-common-0.9
  (package
    (name "rust-atuin-common")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-common" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0lccxsngrdrzyks67s7cbjg9070x3ah2a5bb487n2xc0i4lcb1cv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-chrono" ,rust-chrono-0.4)
         ("rust-rust-crypto" ,rust-rust-crypto-0.2)
         ("rust-serde" ,rust-serde-1)
         ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://atuin.sh")
    (synopsis "common library for atuin")
    (description "common library for atuin")
    (license license:expat)))

(define-public rust-atuin-client-0.9
  (package
    (name "rust-atuin-client")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin-client" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0hiil83mmvw10rgrhrwrs52rsabxpdavda38i8pkfdhidnz4ixx8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-atuin-common" ,rust-atuin-common-0.9)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-chrono-english" ,rust-chrono-english-0.1)
         ("rust-config" ,rust-config-0.13)
         ("rust-directories" ,rust-directories-4)
         ("rust-eyre" ,rust-eyre-0.6)
         ("rust-fs-err" ,rust-fs-err-2)
         ("rust-itertools" ,rust-itertools-0.10)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-minspan" ,rust-minspan-0.1)
         ("rust-parse-duration" ,rust-parse-duration-2)
         ("rust-regex" ,rust-regex-1)
         ("rust-reqwest" ,rust-reqwest-0.11)
         ("rust-rmp-serde" ,rust-rmp-serde-1)
         ("rust-rust-crypto" ,rust-rust-crypto-0.2)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-shellexpand" ,rust-shellexpand-2)
         ("rust-sodiumoxide" ,rust-sodiumoxide-0.2)
         ("rust-sql-builder" ,rust-sql-builder-3)
         ("rust-sqlx" ,rust-sqlx-0.5)
         ("rust-urlencoding" ,rust-urlencoding-2)
         ("rust-uuid" ,rust-uuid-1)
         ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://atuin.sh")
    (synopsis "client library for atuin")
    (description "client library for atuin")
    (license license:expat)))

(define-public rust-syn-1
  (package
    (name "rust-syn")
    (version "1.0.91")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0kflvi0r456s42n3z0d5snilsab2q9ns0dkwnwwg9vn84nwb50xn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-trait-0.1
  (package
    (name "rust-async-trait")
    (version "0.1.53")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-trait" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "003nzwgwb2apz6nww5wmh66k8f47npifll8c33zgkz1d999a6spd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/async-trait")
    (synopsis "Type erasure for async trait methods")
    (description "Type erasure for async trait methods")
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-0.9
  (package
    (name "rust-atuin")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atuin" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0yxa7hs9ah5n2bs60kspk185g63h2px043v3qjlmr9jfmjwcahwm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:rust ,rust-nightly-1.59
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-atuin-client" ,rust-atuin-client-0.9)
         ("rust-atuin-common" ,rust-atuin-common-0.9)
         ("rust-atuin-server" ,rust-atuin-server-0.9)
         ("rust-base64" ,rust-base64-0.13)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-chrono-english" ,rust-chrono-english-0.1)
         ("rust-clap" ,rust-clap-3)
         ("rust-clap-complete" ,rust-clap-complete-3)
         ("rust-cli-table" ,rust-cli-table-0.4)
         ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
         ("rust-directories" ,rust-directories-4)
         ("rust-eyre" ,rust-eyre-0.6)
         ("rust-fs-err" ,rust-fs-err-2)
         ("rust-humantime" ,rust-humantime-2)
         ("rust-indicatif" ,rust-indicatif-0.16)
         ("rust-itertools" ,rust-itertools-0.10)
         ("rust-log" ,rust-log-0.4)
         ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tabwriter" ,rust-tabwriter-1)
         ("rust-termion" ,rust-termion-1)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
         ("rust-tui" ,rust-tui-0.16)
         ("rust-unicode-width" ,rust-unicode-width-0.1)
         ("rust-whoami" ,rust-whoami-1))))
    (home-page "https://atuin.sh")
    (synopsis "atuin - magical shell history")
    (description "atuin - magical shell history")
    (license license:expat)))

(define-public rust-smallvec_1_7_0
  (package
    (name "rust-smallvec")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02gka690j8l12gl50ifg7axqnx1m6v6d1byaq0wl3fx66p3vdjhy"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack"))
    (license `(license:expat
               license:asl2.0))))

(define-public rust-unicode-xid_0_2_2
  (package
    (name "rust-unicode-xid")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wrkgcw557v311dkdb6n2hrix9dm2qdsb1zpw7pn79l03zb85jwc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31.")
    (description
      (beautify-description "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31."))
    (license license:expat)))

ra-multiplex-0.2
