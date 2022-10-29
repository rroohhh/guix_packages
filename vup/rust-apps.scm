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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
    (license (list license:expat
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
