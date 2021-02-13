(define-module (vup prjoxide)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system cargo)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix import utils) #:select (beautify-description spdx-string->license)))

(define-public rust-aho-corasick_0_7_15
  (package
    (name "rust-aho-corasick")
    (version "0.7.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rb8gzhljl8r87dpf2n5pnqnkl694casgns4ma0sqzd4zazzw13l"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_3_4))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      (beautify-description "Fast multiple substring searching."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define-public rust-anyhow_1_0_38
  (package
    (name "rust-anyhow")
    (version "1.0.38")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anyhow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c8ls4w26bjwvjvb6a3s4bpk28raljy3a2pmwrvby3d843szgpdg"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description
      (beautify-description "Flexible concrete Error type built on std::error::Error"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-atty_0_2_14
  (package
    (name "rust-atty")
    (version "0.2.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-hermit-abi" ,rust-hermit-abi_0_1_18)        
       ("rust-libc" ,rust-libc_0_2_86)        
       ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
      (beautify-description "A simple interface for querying atty"))
    (license (spdx-string->license "MIT"))))

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

(define-public rust-az_1_1_0
  (package
    (name "rust-az")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "az" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12pc997lxgs65bzm0ligiwyff7hlcs3fz5fvmnk9amzwgf81sknq"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Casts and checked casts")
    (description
      (beautify-description "Casts and checked casts"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-base64_0_10_1
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
      (("rust-byteorder" ,rust-byteorder_1_4_2))))
    (home-page "None")
    (synopsis "encodes and decodes base64 as bytes or utf8")
    (description
      (beautify-description "encodes and decodes base64 as bytes or utf8"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define-public rust-byteorder_1_4_2
  (package
    (name "rust-byteorder")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byteorder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0srh0h0594jmsnbvm7n0g8xabhla8lwb3gn8s0fzd7d1snix2i5f"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/byteorder")
    (synopsis "Library for reading/writing numbers in big-endian and little-endian.")
    (description
      (beautify-description "Library for reading/writing numbers in big-endian and little-endian."))
    (license (spdx-string->license "Unlicense OR MIT"))))

(define-public rust-cfg-if_1_0_0
  (package
    (name "rust-cfg-if")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted.")
    (description
      (beautify-description "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-clap_3_0_0-beta_2
  (package
    (name "rust-clap")
    (version "3.0.0-beta.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hm1kivw6190rxbfqhdr4hqwlrijvwh90i3d9dyyw0d5k0chdlab"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atty" ,rust-atty_0_2_14)        
       ("rust-bitflags" ,rust-bitflags_1_2_1)        
       ("rust-clap_derive" ,rust-clap_derive_3_0_0-beta_2)        
       ("rust-indexmap" ,rust-indexmap_1_6_1)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-os_str_bytes" ,rust-os_str_bytes_2_4_0)        
       ("rust-strsim" ,rust-strsim_0_10_0)        
       ("rust-termcolor" ,rust-termcolor_1_1_2)        
       ("rust-textwrap" ,rust-textwrap_0_12_1)        
       ("rust-unicode-width" ,rust-unicode-width_0_1_8)        
       ("rust-vec_map" ,rust-vec_map_0_8_2))))
    (home-page "https://clap.rs/")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-clap_derive_3_0_0-beta_2
  (package
    (name "rust-clap_derive")
    (version "3.0.0-beta.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18cn82jhcha7m0nkpi1a03jx8k7aaq5kxfcxnsqpaa8ih5dp23rp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-heck" ,rust-heck_0_3_2)        
       ("rust-proc-macro-error" ,rust-proc-macro-error_1_0_4)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "https://clap.rs/")
    (synopsis "Parse command line argument by defining a struct, derive crate.")
    (description
      (beautify-description "Parse command line argument by defining a struct, derive crate."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-ctor_0_1_19
  (package
    (name "rust-ctor")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ctor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06bbsv7lm9c9mfdg896vdxmdkkammc0sa56n2x4fzg0psjd5vx78"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "None")
    (synopsis "__attribute__((constructor)) for Rust")
    (description
      (beautify-description "__attribute__((constructor)) for Rust"))
    (license (spdx-string->license "Apache-2.0 OR MIT"))))

(define-public rust-either_1_6_1
  (package
    (name "rust-either")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mwl9vngqf5jvrhmhn9x60kr5hivxyjxbmby2pybncxfqhf4z3g7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      (beautify-description "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-getopts_0_2_21
  (package
    (name "rust-getopts")
    (version "0.2.21")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getopts" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mgb3qvivi26gs6ihqqhh8iyhp3vgxri6vwyrwg28w0xqzavznql"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-width" ,rust-unicode-width_0_1_8))))
    (home-page "https://github.com/rust-lang/getopts")
    (synopsis "getopts-like option parsing.")
    (description
      (beautify-description "getopts-like option parsing."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-ghost_0_1_2
  (package
    (name "rust-ghost")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ghost" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yalg3g1g3cz63n3phy7cdhh7p2qd220mrpxy96alwxbpqdwynqs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "None")
    (synopsis "Define your own PhantomData")
    (description
      (beautify-description "Define your own PhantomData"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-glob_0_3_0
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

(define-public rust-gmp-mpfr-sys_1_4_2
  (package
    (name "rust-gmp-mpfr-sys")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gmp-mpfr-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vpiyx6qizqxlq8mb7ch6dz91bj0s8761vgxn4hk10s9klrxnzx5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_86)        
       ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "Rust FFI bindings for GMP, MPFR and MPC")
    (description
      (beautify-description "Rust FFI bindings for GMP, MPFR and MPC"))
    (license (spdx-string->license "LGPL-3.0+"))))

(define-public rust-hashbrown_0_9_1
  (package
    (name "rust-hashbrown")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "016dsm9s4xmxlkw2jfikm54qlz6vyk0qr280gab7kzp342jf9byp"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A Rust port of Google\u0027s SwissTable hash map")
    (description
      (beautify-description "A Rust port of Google\u0027s SwissTable hash map"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define-public rust-heck_0_3_2
  (package
    (name "rust-heck")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b56s2c1ymdd0qmy31bw0ndhm31hcdamnhg3npp7ssrmc1ag9jw7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-segmentation" ,rust-unicode-segmentation_1_7_1))))
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      (beautify-description "heck is a case conversion library."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-hermit-abi_0_1_18
  (package
    (name "rust-hermit-abi")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hermit-abi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p6czgbk1izviwxzm6ypy3vz2wqj1yd3ab03wp82xqjng7klsbrj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_86))))
    (home-page "None")
    (synopsis "hermit-abi is small interface to call functions from the unikernel RustyHermit.\nIt is used to build the target `x86_64-unknown-hermit`.")
    (description
      (beautify-description "hermit-abi is small interface to call functions from the unikernel RustyHermit.\nIt is used to build the target `x86_64-unknown-hermit`."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-include_dir_0_6_0
  (package
    (name "rust-include_dir")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "include_dir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06n6x9l8jnc3laifs8hgbp1h86bqcc84n23c228lc71bnbg8pm93"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-glob" ,rust-glob_0_3_0)        
       ("rust-include_dir_impl" ,rust-include_dir_impl_0_6_0)        
       ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Embed the contents of a directory in your binary")
    (description
      (beautify-description "Embed the contents of a directory in your binary"))
    (license (spdx-string->license "MIT"))))

(define-public rust-include_dir_impl_0_6_0
  (package
    (name "rust-include_dir_impl")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "include_dir_impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05bjyp1wa9f7kpx9gdws2llls52n324mr8nws4j9v0bl0nbnjy1j"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-anyhow" ,rust-anyhow_1_0_38)        
       ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "None")
    (synopsis "Implementation crate for include_dir")
    (description
      (beautify-description "Implementation crate for include_dir"))
    (license (spdx-string->license "MIT"))))

(define-public rust-indexmap_1_6_1
  (package
    (name "rust-indexmap")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0friqyzr4ssyayks7nirqbc36zcsf8fdi67jmvl4vpjh8a9zmcag"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_1_0_1)        
       ("rust-hashbrown" ,rust-hashbrown_0_9_1))))
    (home-page "None")
    (synopsis "A hash table with consistent order and fast iteration.\n\nThe indexmap is a hash table where the iteration order of the key-value\npairs is independent of the hash values of the keys. It has the usual\nhash table functionality, it preserves insertion order except after\nremovals, and it allows lookup of its elements by either hash table key\nor numerical index. A corresponding hash set type is also provided.\n\nThis crate was initially published under the name ordermap, but it was renamed to\nindexmap.")
    (description
      (beautify-description "A hash table with consistent order and fast iteration.\n\nThe indexmap is a hash table where the iteration order of the key-value\npairs is independent of the hash values of the keys. It has the usual\nhash table functionality, it preserves insertion order except after\nremovals, and it allows lookup of its elements by either hash table key\nor numerical index. A corresponding hash set type is also provided.\n\nThis crate was initially published under the name ordermap, but it was renamed to\nindexmap."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define-public rust-indoc_0_3_6
  (package
    (name "rust-indoc")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indoc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n2fd2wm1h005hd7pjgx4gv5ymyq4sxqn8z0ssw6xchgqs5ilx27"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-indoc-impl" ,rust-indoc-impl_0_3_6)        
       ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Indented document literals")
    (description
      (beautify-description "Indented document literals"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-indoc-impl_0_3_6
  (package
    (name "rust-indoc-impl")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indoc-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w58yg249kmzsn75kcj34qaxqh839l1hsaj3bzggy3q03wb6s16f"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60)        
       ("rust-unindent" ,rust-unindent_0_1_7))))
    (home-page "None")
    (synopsis "Indented document literals")
    (description
      (beautify-description "Indented document literals"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-instant_0_1_9
  (package
    (name "rust-instant")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "instant" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v659qqm55misvjijfbl1p7azjp4yynjbwldan8836ynpgp4w4k1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "A partial replacement for std::time::Instant that works on WASM too.")
    (description
      (beautify-description "A partial replacement for std::time::Instant that works on WASM too."))
    (license (spdx-string->license "BSD-3-Clause"))))

(define-public rust-inventory_0_1_10
  (package
    (name "rust-inventory")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inventory" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zzz5sgrkxv1rpim4ihaidzf6jgha919xm4svcrmxjafh3xpw3qg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ctor" ,rust-ctor_0_1_19)        
       ("rust-ghost" ,rust-ghost_0_1_2)        
       ("rust-inventory-impl" ,rust-inventory-impl_0_1_10))))
    (home-page "None")
    (synopsis "Typed distributed plugin registration")
    (description
      (beautify-description "Typed distributed plugin registration"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-inventory-impl_0_1_10
  (package
    (name "rust-inventory-impl")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inventory-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lgs8kia3284s34g7078j820cn2viyb6cij86swklwhn93lr9h3m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "None")
    (synopsis "Implementation of macros for the `inventory` crate")
    (description
      (beautify-description "Implementation of macros for the `inventory` crate"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-itertools_0_8_2
  (package
    (name "rust-itertools")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1154j48aw913v5jnyhpxialxhdn2sfpl4d7bwididyb1r05jsspm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-either" ,rust-either_1_6_1))))
    (home-page "None")
    (synopsis "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      (beautify-description "Extra iterator adaptors, iterator methods, free functions, and macros."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-itoa_0_4_7
  (package
    (name "rust-itoa")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0di7fggbknwfjcw8cgzm1dnm3ik32l2m1f7nmyh8ipmh45h069fx"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast functions for printing integer primitives to an io::Write")
    (description
      (beautify-description "Fast functions for printing integer primitives to an io::Write"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

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
    (home-page "None")
    (synopsis "A macro for declaring lazily evaluated statics in Rust.")
    (description
      (beautify-description "A macro for declaring lazily evaluated statics in Rust."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-libc_0_2_86
  (package
    (name "rust-libc")
    (version "0.2.86")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "074b38mq1rx3dgg7sf952d4ccywq450zymk7gzn5q9z39f92sa5p"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
      (beautify-description "Raw FFI bindings to platform libraries like libc."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-lock_api_0_4_2
  (package
    (name "rust-lock_api")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04hkhfq308agxg9wwmzh7ncfiyyyhn0d49n07abppzdj6p8zz5nx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-scopeguard" ,rust-scopeguard_1_1_0))))
    (home-page "None")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      (beautify-description "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define-public rust-log_0_4_14
  (package
    (name "rust-log")
    (version "0.4.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04175hv0v62shd82qydq58a48k3bjijmk54v38zgqlbxqkkbpfai"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_1_0_0))))
    (home-page "None")
    (synopsis "A lightweight logging facade for Rust")
    (description
      (beautify-description "A lightweight logging facade for Rust"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-memchr_2_3_4
  (package
    (name "rust-memchr")
    (version "2.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "098m9clfs495illlw00hv2gg67mhm7jflld3msyclvi5m9xc9q8f"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description
      (beautify-description "Safe interface to memchr."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define-public rust-multimap_0_8_2
  (package
    (name "rust-multimap")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "multimap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cq3hlqwyxz0hmcpbajghhc832ln6h0qszvf89kv8fx875hhfm8j"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde" ,rust-serde_1_0_123))))
    (home-page "None")
    (synopsis "A multimap implementation.")
    (description
      (beautify-description "A multimap implementation."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-once_cell_1_5_2
  (package
    (name "rust-once_cell")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "once_cell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "183zs1dbmsv24mkafjypf9qwjrx46an58vb004a162l113sl3g8k"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Single assignment cells and lazy values.")
    (description
      (beautify-description "Single assignment cells and lazy values."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-os_str_bytes_2_4_0
  (package
    (name "rust-os_str_bytes")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os_str_bytes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11agh8n3x2l4sr3sxvx6byc1j3ryb1g6flb1ywn0qhq7xv1y3cmg"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Traits for converting between byte sequences and platform-native strings")
    (description
      (beautify-description "Traits for converting between byte sequences and platform-native strings"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-parking_lot_0_11_1
  (package
    (name "rust-parking_lot")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sqmgaia8zfd5fbnqw2w13ijh7crk3lf9vw4cb52vwlx0an48xvd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-instant" ,rust-instant_0_1_9)        
       ("rust-lock_api" ,rust-lock_api_0_4_2)        
       ("rust-parking_lot_core" ,rust-parking_lot_core_0_8_3))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define-public rust-parking_lot_core_0_8_3
  (package
    (name "rust-parking_lot_core")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "065hkylji0g0fkh1vqp7kzs74vclhsxcczwhwqzpcig770lphyps"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
       ("rust-instant" ,rust-instant_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_86)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_2_4)        
       ("rust-smallvec" ,rust-smallvec_1_6_1)        
       ("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define-public rust-paste_0_1_18
  (package
    (name "rust-paste")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10587zrlmzhq66yhd0z36fzglf32m1nlhi9bxxm6dgl0gp3j1jj5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-paste-impl" ,rust-paste-impl_0_1_18)        
       ("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Macros for all your token pasting needs")
    (description
      (beautify-description "Macros for all your token pasting needs"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-paste-impl_0_1_18
  (package
    (name "rust-paste-impl")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dlqzk05cx74522s4iyhyzzhszig4n401pp6r1qg6zmr02r7snnr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro-hack" ,rust-proc-macro-hack_0_5_19))))
    (home-page "None")
    (synopsis "Implementation detail of the `paste` crate")
    (description
      (beautify-description "Implementation detail of the `paste` crate"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-prjoxide
  (package
    (name "rust-prjoxide")
    (version "0.1.0")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gatecat/prjoxide")
                      (commit "6a5a2f554ec85bef0880842993f741f27a1d608b")
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n0hww5di4rlbv120914x71igshlj7ihk8p69lshrkwhg4vy305g"))))
    (build-system cargo-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("gettext" ,gnu-gettext)
                     ("texinfo" ,texinfo)))
    (arguments
    `(#:cargo-inputs
      (("rust-clap" ,rust-clap_3_0_0-beta_2)        
       ("rust-include_dir" ,rust-include_dir_0_6_0)        
       ("rust-itertools" ,rust-itertools_0_8_2)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-log" ,rust-log_0_4_14)        
       ("rust-multimap" ,rust-multimap_0_8_2)        
       ("rust-pulldown-cmark" ,rust-pulldown-cmark_0_6_1)        
       ("rust-regex" ,rust-regex_1_4_3)        
       ("rust-ron" ,rust-ron_0_5_1)        
       ("rust-rug" ,rust-rug_1_11_0)        
       ("rust-serde" ,rust-serde_1_0_123)        
       ("rust-serde_json" ,rust-serde_json_1_0_62))
      #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'change-directory
                    (lambda _
                      (delete-file "libprjoxide/Cargo.toml")
                      (chdir "libprjoxide/prjoxide")
                      (setenv "CONFIG_SHELL" (which "sh"))
                      (display "changed to")
                      (display (getcwd))
                      #t)))))
    (home-page "FILLMEIN")
    (synopsis "")
    (description
      (beautify-description ""))
    (license #f)))

(define-public rust-proc-macro-error_1_0_4
  (package
    (name "rust-proc-macro-error")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro-error-attr" ,rust-proc-macro-error-attr_1_0_4)        
       ("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60)        
       ("rust-version_check" ,rust-version_check_0_9_2))))
    (home-page "None")
    (synopsis "Almost drop-in replacement to panics in proc-macros")
    (description
      (beautify-description "Almost drop-in replacement to panics in proc-macros"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-proc-macro-error-attr_1_0_4
  (package
    (name "rust-proc-macro-error-attr")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-error-attr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-version_check" ,rust-version_check_0_9_2))))
    (home-page "None")
    (synopsis "Attribute macro for proc-macro-error crate")
    (description
      (beautify-description "Attribute macro for proc-macro-error crate"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-proc-macro-hack_0_5_19
  (package
    (name "rust-proc-macro-hack")
    (version "0.5.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-hack" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rg0kzsj7lj00qj602d3h77spwfz48vixn1wbjp7a4yrq65w9w6v"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Procedural macros in expression position")
    (description
      (beautify-description "Procedural macros in expression position"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-proc-macro2_1_0_24
  (package
    (name "rust-proc-macro2")
    (version "1.0.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wcabxzrddcjmryndw8fpyxcq6rw63m701vx86xxf03y3bp081qy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-xid" ,rust-unicode-xid_0_2_1))))
    (home-page "None")
    (synopsis "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple\ntoken-based libraries from the procedural macro use case.")
    (description
      (beautify-description "A substitute implementation of the compiler\u0027s `proc_macro` API to decouple\ntoken-based libraries from the procedural macro use case."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-pulldown-cmark_0_6_1
  (package
    (name "rust-pulldown-cmark")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17lx5cy8rdqdrw0fh3x8zxkpr0ag64q6fs2h5m75kwql4b45q80w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_2_1)        
       ("rust-getopts" ,rust-getopts_0_2_21)        
       ("rust-memchr" ,rust-memchr_2_3_4)        
       ("rust-unicase" ,rust-unicase_2_6_0))))
    (home-page "None")
    (synopsis "A pull parser for CommonMark")
    (description
      (beautify-description "A pull parser for CommonMark"))
    (license (spdx-string->license "MIT"))))

(define-public rust-pyo3_0_13_2
  (package
    (name "rust-pyo3")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hq965lgi25dn578fpn9hjva6zjr1c8rl7lxywijq44aw7lbhds8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_1_0_0)        
       ("rust-ctor" ,rust-ctor_0_1_19)        
       ("rust-indoc" ,rust-indoc_0_3_6)        
       ("rust-inventory" ,rust-inventory_0_1_10)        
       ("rust-libc" ,rust-libc_0_2_86)        
       ("rust-parking_lot" ,rust-parking_lot_0_11_1)        
       ("rust-paste" ,rust-paste_0_1_18)        
       ("rust-pyo3-macros" ,rust-pyo3-macros_0_13_2)        
       ("rust-unindent" ,rust-unindent_0_1_7))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Bindings to Python interpreter")
    (description
      (beautify-description "Bindings to Python interpreter"))
    (license (spdx-string->license "Apache-2.0"))))

(define-public rust-pyo3-macros_0_13_2
  (package
    (name "rust-pyo3-macros")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fxi5lx5dl7xh469gr5xckyjy3r3c5dqypzxcj0fbhzf1hq2qzx4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-pyo3-macros-backend" ,rust-pyo3-macros-backend_0_13_2)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Proc macros for PyO3 package")
    (description
      (beautify-description "Proc macros for PyO3 package"))
    (license (spdx-string->license "Apache-2.0"))))

(define-public rust-pyo3-macros-backend_0_13_2
  (package
    (name "rust-pyo3-macros-backend")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pyo3-macros-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rjxayd78l10hnyphk03bcvhm0jpsvnzn07lczhy7jsgv3jrgc47"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Code generation for PyO3 package")
    (description
      (beautify-description "Code generation for PyO3 package"))
    (license (spdx-string->license "Apache-2.0"))))

(define-public rust-quote_1_0_9
  (package
    (name "rust-quote")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19rjmfqzk26rxbgxy5j2ckqc2v12sw2xw8l4gi8bzpn2bmsbkl63"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_24))))
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
      (beautify-description "Quasi-quoting macro quote!(...)"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-redox_syscall_0_2_4
  (package
    (name "rust-redox_syscall")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w254gyccyinrzhgd562ddrhgcpwswy700mmc9qa6pkc86lqrv05"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_2_1))))
    (home-page "None")
    (synopsis "A Rust library to access raw Redox system calls")
    (description
      (beautify-description "A Rust library to access raw Redox system calls"))
    (license (spdx-string->license "MIT"))))

(define-public rust-regex_1_4_3
  (package
    (name "rust-regex")
    (version "1.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12llbg82js69mdl50lav4yn1iqlx71ckb18dww467q99w4wi49fr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick_0_7_15)        
       ("rust-memchr" ,rust-memchr_2_3_4)        
       ("rust-regex-syntax" ,rust-regex-syntax_0_6_22)        
       ("rust-thread_local" ,rust-thread_local_1_1_3))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-regex-syntax_0_6_22
  (package
    (name "rust-regex-syntax")
    (version "0.6.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10b56ylil35jkb4nwqxm8hbyx3zq7fws0wpydjln165s8xql3sxm"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      (beautify-description "A regular expression parser."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-ron_0_5_1
  (package
    (name "rust-ron")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ron" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mb2bavvp8jg5wx0kx9n45anrsbjwhjzddim987bjaa11hg45kif"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_10_1)        
       ("rust-bitflags" ,rust-bitflags_1_2_1)        
       ("rust-serde" ,rust-serde_1_0_123))))
    (home-page "https://github.com/ron-rs/ron")
    (synopsis "Rusty Object Notation")
    (description
      (beautify-description "Rusty Object Notation"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-rug_1_11_0
  (package
    (name "rust-rug")
    (version "1.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rug" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0idw05zz1i2l747arvhz3b3nxj1dggk24qvymj5f9a2hlh6x0f75"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-az" ,rust-az_1_1_0)        
       ("rust-gmp-mpfr-sys" ,rust-gmp-mpfr-sys_1_4_2)        
       ("rust-libc" ,rust-libc_0_2_86))))
    (home-page "None")
    (synopsis "Arbitrary-precision integers, rational, floating-point and complex numbers based on GMP, MPFR and MPC")
    (description
      (beautify-description "Arbitrary-precision integers, rational, floating-point and complex numbers based on GMP, MPFR and MPC"))
    (license (spdx-string->license "LGPL-3.0+"))))

(define-public rust-ryu_1_0_5
  (package
    (name "rust-ryu")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vpqv1dj7fksa6hm3zpk5rbsjs0ifbfy7xwzsyyil0rx37a03lvi"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast floating point to string conversion")
    (description
      (beautify-description "Fast floating point to string conversion"))
    (license (spdx-string->license "Apache-2.0 OR BSL-1.0"))))

(define-public rust-scopeguard_1_1_0
  (package
    (name "rust-scopeguard")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kbqm85v43rq92vx7hfiay6pmcga03vrjbbfwqpyj3pwsg3b16nj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies.")
    (description
      (beautify-description "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-serde_1_0_123
  (package
    (name "rust-serde")
    (version "1.0.123")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bk9733mgiv5sg8yb19y8mc85fb2aaqp1k02v10alavj688idmcj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-serde_derive" ,rust-serde_derive_1_0_123))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
      (beautify-description "A generic serialization/deserialization framework"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-serde_derive_1_0_123
  (package
    (name "rust-serde_derive")
    (version "1.0.123")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ccg4m7ww6mfs5vjdbdifri2kf1wyd4difjnqnraph2gssaw54ck"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-syn" ,rust-syn_1_0_60))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      (beautify-description "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-serde_json_1_0_62
  (package
    (name "rust-serde_json")
    (version "1.0.62")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11hlwc4glh244sk6ncnvj3n01pi538iv2qygyxyfllj5g59n277a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-itoa" ,rust-itoa_0_4_7)        
       ("rust-ryu" ,rust-ryu_1_0_5)        
       ("rust-serde" ,rust-serde_1_0_123))))
    (home-page "None")
    (synopsis "A JSON serialization file format")
    (description
      (beautify-description "A JSON serialization file format"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-smallvec_1_6_1
  (package
    (name "rust-smallvec")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kk08axr0ybfbjzk65a41k84mb6sfhyajmfndaka9igkx34kf3zy"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-strsim_0_10_0
  (package
    (name "rust-strsim")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice.")
    (description
      (beautify-description "Implementations of string similarity metrics. Includes Hamming, Levenshtein,\nOSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and S\u00f8rensen-Dice."))
    (license (spdx-string->license "MIT"))))

(define-public rust-syn_1_0_60
  (package
    (name "rust-syn")
    (version "1.0.60")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1080gw6mlja7yl26crya3k403wjdp7v3wx9mxcmpcnlar9z5j067"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_1_0_24)        
       ("rust-quote" ,rust-quote_1_0_9)        
       ("rust-unicode-xid" ,rust-unicode-xid_0_2_1))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
      (beautify-description "Parser for Rust source code"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-termcolor_1_1_2
  (package
    (name "rust-termcolor")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1x65i1ny4m6z1by62ra6wdcrd557p2ysm866x0pg60zby2cxizid"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi-util" ,rust-winapi-util_0_1_5))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis "A simple cross platform library for writing colored text to a terminal.")
    (description
      (beautify-description "A simple cross platform library for writing colored text to a terminal."))
    (license (spdx-string->license "Unlicense OR MIT"))))

(define-public rust-textwrap_0_12_1
  (package
    (name "rust-textwrap")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12978qmkl5gcp94lxndpvp9qxq8mxp7hm9xbrw3422dgikchhc10"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-width" ,rust-unicode-width_0_1_8))))
    (home-page "None")
    (synopsis "Powerful library for word wrapping, indenting, and dedenting strings")
    (description
      (beautify-description "Powerful library for word wrapping, indenting, and dedenting strings"))
    (license (spdx-string->license "MIT"))))

(define-public rust-thread_local_1_1_3
  (package
    (name "rust-thread_local")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread_local" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gccp3grndpi6dyhzylz4hkqnkzc1xyri98n0xwwhnn90i7d4640"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-once_cell" ,rust-once_cell_1_5_2))))
    (home-page "None")
    (synopsis "Per-object thread-local storage")
    (description
      (beautify-description "Per-object thread-local storage"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define-public rust-unicase_2_6_0
  (package
    (name "rust-unicase")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicase" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xmlbink4ycgxrkjspp0mf7pghcx4m7vxq7fpfm04ikr2zk7pwsh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-version_check" ,rust-version_check_0_9_2))))
    (home-page "None")
    (synopsis "A case-insensitive wrapper around strings.")
    (description
      (beautify-description "A case-insensitive wrapper around strings."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-unicode-segmentation_1_7_1
  (package
    (name "rust-unicode-segmentation")
    (version "1.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-segmentation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15n736z0pbj30pj44jb9s9rjavzrmx8v8pzdgsl5yfmfwrxjw3dv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis "This crate provides Grapheme Cluster, Word and Sentence boundaries\naccording to Unicode Standard Annex #29 rules.")
    (description
      (beautify-description "This crate provides Grapheme Cluster, Word and Sentence boundaries\naccording to Unicode Standard Annex #29 rules."))
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

(define-public rust-unicode-xid_0_2_1
  (package
    (name "rust-unicode-xid")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r6mknipyy9vpz8mwmxvkx65ff2ha1n2pxqjj6f46lcn8yrhpzpp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31.")
    (description
      (beautify-description "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-unindent_0_1_7
  (package
    (name "rust-unindent")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unindent" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1is1gmx1l89z426rn3xsi0mii4vhy2imhqmhx8x2pd8mji6y0kpi"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Remove a column of leading whitespace from a string")
    (description
      (beautify-description "Remove a column of leading whitespace from a string"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define-public rust-vec_map_0_8_2
  (package
    (name "rust-vec_map")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vec_map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1481w9g1dw9rxp3l6snkdqihzyrd2f8vispzqmwjwsdyhw8xzggi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/contain-rs/vec-map")
    (synopsis "A simple map based on a vector for small integer keys")
    (description
      (beautify-description "A simple map based on a vector for small integer keys"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define-public rust-version_check_0_9_2
  (package
    (name "rust-version_check")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vbaqdf802qinsq8q20w8w0qn2pv0rkq5p73ijcblrwxcvjp5adm"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Tiny crate to check the version of the installed/running rustc.")
    (description
      (beautify-description "Tiny crate to check the version of the installed/running rustc."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define-public rust-winapi-util_0_1_5
  (package
    (name "rust-winapi-util")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y71bp7f6d536czj40dhqk0d55wfbbwqfp2ymqf1an5ibgl6rv3h"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_9))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "A dumping ground for high level safe wrappers over winapi.")
    (description
      (beautify-description "A dumping ground for high level safe wrappers over winapi."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

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
