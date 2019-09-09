(define-module (vup alacritty)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix import utils)
                #:select (beautify-description spdx-string->license))
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages elf)
  #:use-module (guix build rpath)
  #:use-module (gnu packages xdisorg))

(define rust-adler32_1_0_3
  (package
    (name "rust-adler32")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "adler32" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p7fxlnks9l7p7rwfqi7aqgnk2bps5zc0rjiw00mdw19nnbjjlky"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Minimal Adler32 implementation for Rust.")
    (description
      (beautify-description "Minimal Adler32 implementation for Rust."))
    (license (spdx-string->license "BSD-3-Clause AND Zlib"))))

(define rust-aho-corasick_0_6_10
  (package
    (name "rust-aho-corasick")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19f8v503ibvlyr824g5ynicrh1lsmp2i0zmpszr8lqay0qw3vkl1"))))
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

(define-public rust-alacritty_0_3_3
  (package
    (name "rust-alacritty")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jwilm/alacritty")
               (commit "2f93fb34b1b237bea4d1bea67a67c7c8efe6e5dc")))
        (file-name (git-file-name name version))
        (sha256
          (base32 
            "1kf5zkcadg6vmqz4fmxyj7nhybsz4kdkf5x5dwha8fkqha237a4i"))))
    (build-system cargo-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cmake" ,cmake)
                     ("patchelf" ,patchelf)
                     ("python" ,python)
                     ("ncurses" ,ncurses)
                     ("make" ,gnu-make)))
    (inputs `(("libxcb" ,libxcb)
              ("freetype" ,freetype)
              ("libX11" ,libx11)
              ("libXcursor" ,libxcursor)
              ("libXrandr" ,libxrandr)
              ("libXi" ,libxi)
              ("mesa" ,mesa)
              ("libxkbcommon" ,libxkbcommon)
              ("wayland" ,wayland)
              ("fontconfig" ,fontconfig)))
    (arguments
    `(#:modules ((guix build cargo-build-system)
                           (guix build rpath)
                           (guix build utils))
      #:imported-modules (,@%cargo-build-system-modules
                             (guix build rpath))
      #:phases (modify-phases %standard-phases
                  (replace 'install 
                    (lambda* (#:key inputs outputs skip-build? #:allow-other-keys)
                      "Install a given Cargo package."
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (mesa_lib (string-append (assoc-ref inputs "mesa") "/lib"))
                             (xkbcommon_lib (string-append (assoc-ref inputs "libxkbcommon") "/lib"))
                             (wayland_lib (string-append (assoc-ref inputs "wayland") "/lib")))

                        (install-file "./target/release/alacritty" bin)
                        (augment-rpath (string-append bin "/alacritty") mesa_lib)
                        (augment-rpath (string-append bin "/alacritty") wayland_lib)
                        (augment-rpath (string-append bin "/alacritty") xkbcommon_lib)

                        (install-file "extra/linux/alacritty.desktop" (string-append out "/share/applications"))

                        (copy-file "extra/linux/alacritty.desktop" "Alacritty.svg")
                        (install-file "Alacritty.svg" (string-append out "/share/icons/hicolor/scalable/apps"))

                        (install-file "extra/completions/_alacritty" (string-append out "/share/zsh/site-functions"))
                        (install-file "extra/completions/alacritty.bash" (string-append out "/etc/bash_completion.d"))
                        (install-file "extra/completions/alacritty.fish" (string-append out "/share/fish/vendor_completions.d"))

                        (copy-file "extra/alacritty.man" "alacritty.1")
                        (install-file "alacritty.1" (string-append out "/share/man/man1"))

                        (mkdir-p (string-append out "/share/terminfo/a"))
                        (system* (string-append (assoc-ref inputs "ncurses") "/bin/tic") "-x" "-o" (string-append out "/share/terminfo") "extra/alacritty.info"))
                      #t)))
      #:cargo-inputs
      (("rust-alacritty_terminal" ,rust-alacritty_terminal_0_3_3)        
       ("rust-clap" ,rust-clap_2_33_0)        
       ("rust-crossbeam-channel" ,rust-crossbeam-channel_0_3_9)        
       ("rust-dirs" ,rust-dirs_1_0_5)        
       ("rust-env_logger" ,rust-env_logger_0_6_2)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-rustc_tools_util" ,rust-rustc_tools_util_0_2_0)        
       ("rust-serde_json" ,rust-serde_json_1_0_40)        
       ("rust-serde_yaml" ,rust-serde_yaml_0_8_9)        
       ("rust-time" ,rust-time_0_1_42)        
       ("rust-winapi" ,rust-winapi_0_3_8)        
       ("rust-xdg" ,rust-xdg_2_2_0))))
    (home-page "https://github.com/jwilm/alacritty")
    (synopsis "GPU-accelerated terminal emulator")
    (description
      (beautify-description "GPU-accelerated terminal emulator"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-alacritty_terminal_0_3_3
  (package
    (name "rust-alacritty_terminal")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jwilm/alacritty")
               (commit "2f93fb34b1b237bea4d1bea67a67c7c8efe6e5dc")))
        (file-name (git-file-name name version))
        (sha256
          (base32 
            "1kf5zkcadg6vmqz4fmxyj7nhybsz4kdkf5x5dwha8fkqha237a4i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_10_1)        
       ("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-copypasta" ,rust-copypasta_0_6_0)        
       ("rust-crossbeam-channel" ,rust-crossbeam-channel_0_3_9)        
       ("rust-dunce" ,rust-dunce_1_0_0)        
       ("rust-fnv" ,rust-fnv_1_0_6)        
       ("rust-font" ,rust-font_0_1_0)        
       ("rust-gl_generator" ,rust-gl_generator_0_13_1)        
       ("rust-glutin" ,rust-glutin_0_21_1)        
       ("rust-image" ,rust-image_0_21_3)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-mio" ,rust-mio_0_6_19)        
       ("rust-mio-anonymous-pipes" ,rust-mio-anonymous-pipes_0_1_0)        
       ("rust-mio-extras" ,rust-mio-extras_2_0_5)        
       ("rust-mio-named-pipes" ,rust-mio-named-pipes_0_1_6)        
       ("rust-miow" ,rust-miow_0_3_3)        
       ("rust-nix" ,rust-nix_0_14_1)        
       ("rust-notify" ,rust-notify_4_0_12)        
       ("rust-objc" ,rust-objc_0_2_6)        
       ("rust-parking_lot" ,rust-parking_lot_0_8_0)        
       ("rust-rfind_url" ,rust-rfind_url_0_4_2)        
       ("rust-serde" ,rust-serde_1_0_99)        
       ("rust-serde_derive" ,rust-serde_derive_1_0_99)        
       ("rust-serde_json" ,rust-serde_json_1_0_40)        
       ("rust-serde_yaml" ,rust-serde_yaml_0_8_9)        
       ("rust-signal-hook" ,rust-signal-hook_0_1_10)        
       ("rust-static_assertions" ,rust-static_assertions_0_3_4)        
       ("rust-terminfo" ,rust-terminfo_0_6_1)        
       ("rust-unicode-width" ,rust-unicode-width_0_1_6)        
       ("rust-url" ,rust-url_2_1_0)        
       ("rust-vte" ,rust-vte_0_3_3)        
       ("rust-widestring" ,rust-widestring_0_4_0)        
       ("rust-winapi" ,rust-winapi_0_3_8)        
       ("rust-winpty" ,rust-winpty_0_1_0)        
       ("rust-x11-dl" ,rust-x11-dl_2_18_4))))
    (home-page "https://github.com/jwilm/alacritty")
    (synopsis "Library for writing terminal emulators")
    (description
      (beautify-description "Library for writing terminal emulators"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-andrew_0_2_1
  (package
    (name "rust-andrew")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "andrew" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pmklwcwy8g1jras46fz8xcny779zfqpg4riksrbdhkjk3w0jzwv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-line_drawing" ,rust-line_drawing_0_7_0)        
       ("rust-rusttype" ,rust-rusttype_0_7_7)        
       ("rust-walkdir" ,rust-walkdir_2_2_9)        
       ("rust-xdg" ,rust-xdg_2_2_0)        
       ("rust-xml-rs" ,rust-xml-rs_0_8_0))))
    (home-page "None")
    (synopsis "The andrew crate provides convenient drawing of objects such as shapes, lines and text to buffers")
    (description
      (beautify-description "The andrew crate provides convenient drawing of objects such as shapes, lines and text to buffers"))
    (license (spdx-string->license "MIT"))))

(define rust-android_glue_0_2_3
  (package
    (name "rust-android_glue")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "android_glue" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01y495x4i9vqkwmklwn2xk7sqg666az2axjcpkr4iwngdwi48100"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Glue for the Android JNI")
    (description
      (beautify-description "Glue for the Android JNI"))
    (license (spdx-string->license "MIT"))))

(define rust-ansi_term_0_11_0
  (package
    (name "rust-ansi_term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles (bold, underline)")
    (description
      (beautify-description "Library for ANSI terminal colours and styles (bold, underline)"))
    (license (spdx-string->license "MIT"))))

(define rust-approx_0_3_2
  (package
    (name "rust-approx")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "approx" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hx580xjdxl3766js9b49rnbnmr8gw8c060809l43k9f0xshprph"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "https://github.com/brendanzab/approx")
    (synopsis "Approximate floating point equality comparisons and assertions.")
    (description
      (beautify-description "Approximate floating point equality comparisons and assertions."))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-arc-swap_0_4_2
  (package
    (name "rust-arc-swap")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arc-swap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f9h24z60i30x47anzdq348j1ii00c1rshxjkx8r1km0ywlxwkl5"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Atomically swappable Arc")
    (description
      (beautify-description "Atomically swappable Arc"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-arrayref_0_3_5
  (package
    (name "rust-arrayref")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arrayref" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vphy316jbgmgckk4z7m8csvlyc8hih9w95iyq48h8077xc2wf0d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Macros to take array references of slices")
    (description
      (beautify-description "Macros to take array references of slices"))
    (license (spdx-string->license "BSD-2-Clause"))))

(define rust-arrayvec_0_4_11
  (package
    (name "rust-arrayvec")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arrayvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fmhq4ljxr954mdyazaqa9kdxryl5d2ggr5rialylrd6xndkzmxq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nodrop" ,rust-nodrop_0_1_13))))
    (home-page "None")
    (synopsis "A vector with fixed capacity, backed by an array (it can be stored on the stack too). Implements fixed capacity ArrayVec and ArrayString.")
    (description
      (beautify-description "A vector with fixed capacity, backed by an array (it can be stored on the stack too). Implements fixed capacity ArrayVec and ArrayString."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define rust-backtrace_0_3_35
  (package
    (name "rust-backtrace")
    (version "0.3.35")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mfwbb6832rh1za304w8x37bvs9fjbybpmmz0iksqfzsaf108w8k"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-backtrace-sys" ,rust-backtrace-sys_0_1_31)        
       ("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-rustc-demangle" ,rust-rustc-demangle_0_1_16))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis "A library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (description
      (beautify-description "A library to acquire a stack trace (backtrace) at runtime in a Rust program."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-backtrace-sys_0_1_31
  (package
    (name "rust-backtrace-sys")
    (version "0.1.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0as2pk77br4br04daywhivpi1ixxb8y2c7f726kj849dxys31a42"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/alexcrichton/backtrace-rs")
    (synopsis "Bindings to the libbacktrace gcc library")
    (description
      (beautify-description "Bindings to the libbacktrace gcc library"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define rust-bindgen_0_33_2
  (package
    (name "rust-bindgen")
    (version "0.33.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vnw5fb74gl9pgnimgbrkac1xgwrjz86pqilx20rbkia77cdhgk0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cexpr" ,rust-cexpr_0_2_3)        
       ("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-clang-sys" ,rust-clang-sys_0_22_0)        
       ("rust-env_logger" ,rust-env_logger_0_5_13)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-peeking_take_while" ,rust-peeking_take_while_0_1_2)        
       ("rust-quote" ,rust-quote_0_3_15)        
       ("rust-regex" ,rust-regex_0_2_11)        
       ("rust-which" ,rust-which_1_0_5))
      #:cargo-development-inputs
      (("rust-clap" ,rust-clap_2_33_0))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      (beautify-description "Automatically generates Rust FFI bindings to C and C++ libraries."))
    (license (spdx-string->license "BSD-3-Clause"))))

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

(define rust-blake2b_simd_0_5_7
  (package
    (name "rust-blake2b_simd")
    (version "0.5.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blake2b_simd" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1684dlyqv3ifz3q75g0xs51qmc672q1w42hpzwh4wiidpf0mlxxz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-arrayref" ,rust-arrayref_0_3_5)        
       ("rust-arrayvec" ,rust-arrayvec_0_4_11)        
       ("rust-constant_time_eq" ,rust-constant_time_eq_0_1_4))))
    (home-page "None")
    (synopsis "a pure Rust BLAKE2b implementation with dynamic SIMD")
    (description
      (beautify-description "a pure Rust BLAKE2b implementation with dynamic SIMD"))
    (license (spdx-string->license "MIT"))))

(define rust-block_0_1_6
  (package
    (name "rust-block")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Rust interface for Apple\u0027s C language extension of blocks.")
    (description
      (beautify-description "Rust interface for Apple\u0027s C language extension of blocks."))
    (license (spdx-string->license "MIT"))))

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

(define rust-bzip2_0_3_3
  (package
    (name "rust-bzip2")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bzip2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fvfwanp42j1zpig880jhb5mc0na50bijmwd6211p77sy35w7ds2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bzip2-sys" ,rust-bzip2-sys_0_1_7)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/alexcrichton/bzip2-rs")
    (synopsis "Bindings to libbzip2 for bzip2 compression and decompression exposed as\nReader/Writer streams.")
    (description
      (beautify-description "Bindings to libbzip2 for bzip2 compression and decompression exposed as\nReader/Writer streams."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-bzip2-sys_0_1_7
  (package
    (name "rust-bzip2-sys")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bzip2-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pz2mdhkk8yphiqdh2kghdxb60kqyd10lfrjym3r4k5dylvam135"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/alexcrichton/bzip2-rs")
    (synopsis "Bindings to libbzip2 for bzip2 compression and decompression exposed as\nReader/Writer streams.")
    (description
      (beautify-description "Bindings to libbzip2 for bzip2 compression and decompression exposed as\nReader/Writer streams."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-c2-chacha_0_2_2
  (package
    (name "rust-c2-chacha")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "c2-chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00a11qdc8mg3z0k613rhprkc9p6xz0y7b1681x32ixg0hr3x0r3x"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-ppv-lite86" ,rust-ppv-lite86_0_2_5))))
    (home-page "None")
    (synopsis "The ChaCha family of stream ciphers")
    (description
      (beautify-description "The ChaCha family of stream ciphers"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (arguments
    `(#:cargo-inputs
      (("rust-rayon" ,rust-rayon_1_1_0))))
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode.")
    (description
      (beautify-description "A build-time dependency for Cargo build scripts to assist in invoking the native\nC compiler to compile native C code into a static archive to be linked into Rust\ncode."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-cexpr_0_2_3
  (package
    (name "rust-cexpr")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cexpr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0v1xa3758czmj8h97gh548mr8g0v13ixxvrlm1s79nb7jmgc9aj2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nom" ,rust-nom_3_2_1))))
    (home-page "None")
    (synopsis "A C expression parser and evaluator")
    (description
      (beautify-description "A C expression parser and evaluator"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

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
    (synopsis "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted.")
    (description
      (beautify-description "A macro to ergonomically define an item depending on a large number of #[cfg]\nparameters. Structured like an if-else chain, the first matching branch is the\nitem that gets emitted."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-cgl_0_2_3
  (package
    (name "rust-cgl")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cgl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0j8ayr8pbwvyv6l8r7m5z197rs3pqn97085w9j4rfn7yfh5yrrsm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-gleam" ,rust-gleam_0_6_19)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Rust bindings for CGL on Mac")
    (description
      (beautify-description "Rust bindings for CGL on Mac"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-clang-sys_0_22_0
  (package
    (name "rust-clang-sys")
    (version "0.22.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f65dw1ydnzq4wrv894fql78n4ikb53jjp53xck0s4hb64s1m6lk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-glob" ,rust-glob_0_2_11)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-libloading" ,rust-libloading_0_5_2))))
    (home-page "None")
    (synopsis "Rust bindings for libclang.")
    (description
      (beautify-description "Rust bindings for libclang."))
    (license (spdx-string->license "Apache-2.0"))))

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
      (("rust-ansi_term" ,rust-ansi_term_0_11_0)        
       ("rust-atty" ,rust-atty_0_2_13)        
       ("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-strsim" ,rust-strsim_0_8_0)        
       ("rust-textwrap" ,rust-textwrap_0_11_0)        
       ("rust-unicode-width" ,rust-unicode-width_0_1_6)        
       ("rust-vec_map" ,rust-vec_map_0_8_1))))
    (home-page "https://clap.rs/")
    (synopsis "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      (beautify-description "A simple to use, efficient, and full-featured Command Line Argument Parser"))
    (license (spdx-string->license "MIT"))))

(define rust-clipboard-win_2_2_0
  (package
    (name "rust-clipboard-win")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clipboard-win" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0svqk0lrw66abaxd6h7l4k4g2s5vd1dcipy34kzfan6mzvb97873"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "Provides simple way to interact with Windows clipboard.")
    (description
      (beautify-description "Provides simple way to interact with Windows clipboard."))
    (license (spdx-string->license "MIT"))))

(define rust-cloudabi_0_0_3
  (package
    (name "rust-cloudabi")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cloudabi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kxcg83jlihy0phnd2g8c2c303px3l2p3pkjz357ll6llnd5pz6x"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0))))
    (home-page "https://nuxi.nl/cloudabi/")
    (synopsis "Low level interface to CloudABI. Contains all syscalls and related types.")
    (description
      (beautify-description "Low level interface to CloudABI. Contains all syscalls and related types."))
    (license (spdx-string->license "BSD-2-Clause"))))

(define rust-cmake_0_1_42
  (package
    (name "rust-cmake")
    (version "0.1.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cmake" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qkwibkvx5xjazvv9v8gvdlpky2jhjxvcz014nrixgzqfyv2byw1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41))))
    (home-page "https://github.com/alexcrichton/cmake-rs")
    (synopsis "A build dependency for running `cmake` to build a native library")
    (description
      (beautify-description "A build dependency for running `cmake` to build a native library"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-cocoa_0_18_4
  (package
    (name "rust-cocoa")
    (version "0.18.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cocoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yyprmkqy16s329m4wcn2jsyczdq04val1jkwl3fyp8yw6jdlyfg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-block" ,rust-block_0_1_6)        
       ("rust-core-foundation" ,rust-core-foundation_0_6_4)        
       ("rust-core-graphics" ,rust-core-graphics_0_17_3)        
       ("rust-foreign-types" ,rust-foreign-types_0_3_2)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-objc" ,rust-objc_0_2_6))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa for macOS")
    (description
      (beautify-description "Bindings to Cocoa for macOS"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-color_quant_1_0_1
  (package
    (name "rust-color_quant")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "color_quant" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ga56jrafnjm80903nnqjkyii4bwd6a7visxh0g8hgi6cmrvbfqd"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Color quantization library to reduce n colors to 256 colors.")
    (description
      (beautify-description "Color quantization library to reduce n colors to 256 colors."))
    (license (spdx-string->license "MIT"))))

(define rust-constant_time_eq_0_1_4
  (package
    (name "rust-constant_time_eq")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "constant_time_eq" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "083icpr9xb72rrdxw3p4068dcspn6ai22jy7rhl2a8grfz448nlr"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Compares two equal-sized byte strings in constant time.")
    (description
      (beautify-description "Compares two equal-sized byte strings in constant time."))
    (license (spdx-string->license "CC0-1.0"))))

(define rust-copypasta_0_6_0
  (package
    (name "rust-copypasta")
    (version "0.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jwilm/alacritty")
               (commit "2f93fb34b1b237bea4d1bea67a67c7c8efe6e5dc")))
        (file-name (git-file-name name version))
        (sha256
          (base32 
            "1kf5zkcadg6vmqz4fmxyj7nhybsz4kdkf5x5dwha8fkqha237a4i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-andrew" ,rust-andrew_0_2_1)        
       ("rust-clipboard-win" ,rust-clipboard-win_2_2_0)        
       ("rust-objc" ,rust-objc_0_2_6)        
       ("rust-objc-foundation" ,rust-objc-foundation_0_1_1)        
       ("rust-objc_id" ,rust-objc_id_0_1_1)        
       ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit_0_6_4)        
       ("rust-smithay-clipboard" ,rust-smithay-clipboard_0_3_4)        
       ("rust-wayland-client" ,rust-wayland-client_0_23_5)        
       ("rust-x11-clipboard" ,rust-x11-clipboard_0_3_3))))
    (home-page "https://github.com/jwilm/alacritty")
    (synopsis "copypasta is a cross-platform library for getting and setting the contents of the OS-level clipboard.")
    (description
      (beautify-description "copypasta is a cross-platform library for getting and setting the contents of the OS-level clipboard."))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-core-foundation_0_6_4
  (package
    (name "rust-core-foundation")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0va97wf49c8dzm9c8pgyk1jn7z21rl0bj1syf2zz5m2z2hzy1f95"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-core-foundation-sys" ,rust-core-foundation-sys_0_6_2)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
      (beautify-description "Bindings to Core Foundation for macOS"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-core-foundation-sys_0_6_2
  (package
    (name "rust-core-foundation-sys")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fzsw1j9g1x598yhwklg59l15hwzc0pyvs01w9fg2kin4598mjp7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for OS X")
    (description
      (beautify-description "Bindings to Core Foundation for OS X"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-core-graphics_0_17_3
  (package
    (name "rust-core-graphics")
    (version "0.17.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-graphics" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1acm3vygngnilzlr6klym5ywh7kfzh2xxrh2l41152hwmdl0jyan"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-core-foundation" ,rust-core-foundation_0_6_4)        
       ("rust-foreign-types" ,rust-foreign-types_0_3_2)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/servo/core-graphics-rs")
    (synopsis "Bindings to Core Graphics for OS X")
    (description
      (beautify-description "Bindings to Core Graphics for OS X"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-core-text_13_3_0
  (package
    (name "rust-core-text")
    (version "13.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-text" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0h56y8a71dv5yqi4ahhqprxxk4azfzxji5mll805dj8lnd1l4s0j"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-core-foundation" ,rust-core-foundation_0_6_4)        
       ("rust-core-graphics" ,rust-core-graphics_0_17_3)        
       ("rust-foreign-types" ,rust-foreign-types_0_3_2)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Bindings to the Core Text framework.")
    (description
      (beautify-description "Bindings to the Core Text framework."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-crc32fast_1_2_0
  (package
    (name "rust-crc32fast")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc32fast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c9dhkvf3brrzzplcijaywxi2w8wv5578i0ryhcm7x8dmzi5s4ms"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9))))
    (home-page "None")
    (synopsis "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
    (description
      (beautify-description "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

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

(define rust-crossbeam-deque_0_6_3
  (package
    (name "rust-crossbeam-deque")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-deque" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04rcpgjs6ns57vag8a3dzx26190dhbvy2l0p9n22b9p1yf64pr05"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-epoch" ,rust-crossbeam-epoch_0_7_2)        
       ("rust-crossbeam-utils" ,rust-crossbeam-utils_0_6_6))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description
      (beautify-description "Concurrent work-stealing deque"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-crossbeam-epoch_0_7_2
  (package
    (name "rust-crossbeam-epoch")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-epoch" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a9prma2nalqvys7f8wrazkdzh26w3mi5gzrk8mdmwrp5rvxdp7y"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-arrayvec" ,rust-arrayvec_0_4_11)        
       ("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-crossbeam-utils" ,rust-crossbeam-utils_0_6_6)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-memoffset" ,rust-memoffset_0_5_1)        
       ("rust-scopeguard" ,rust-scopeguard_1_0_0))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description
      (beautify-description "Epoch-based garbage collection"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-crossbeam-queue_0_1_2
  (package
    (name "rust-crossbeam-queue")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-queue" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jsa9dbxnwqcxfws09vaschf92d4imlbbikmcn4ka8z7rzb9r5vw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-utils" ,rust-crossbeam-utils_0_6_6))))
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Concurrent queues")
    (description
      (beautify-description "Concurrent queues"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define rust-deflate_0_7_20
  (package
    (name "rust-deflate")
    (version "0.7.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "deflate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d7d9fpmgjnznrksmd3vlv3dyw01wsrm11ifil6ag22871xnlyvh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-adler32" ,rust-adler32_1_0_3)        
       ("rust-byteorder" ,rust-byteorder_1_3_2))))
    (home-page "https://github.com/oyvindln/deflate-rs")
    (synopsis "A DEFLATE, zlib and gzip encoder written in rust.")
    (description
      (beautify-description "A DEFLATE, zlib and gzip encoder written in rust."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-derivative_1_0_2
  (package
    (name "rust-derivative")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derivative" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "164q0yqpgk1p2bp83ls1bipwh8zxrsvy6qzbpbmdvgdydmkyjwv0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-syn" ,rust-syn_0_15_44))))
    (home-page "None")
    (synopsis "A set of alternative `derive` attributes for Rust")
    (description
      (beautify-description "A set of alternative `derive` attributes for Rust"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-dirs_1_0_5
  (package
    (name "rust-dirs")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "009rrhzj9pxyncmm2vhlj70npg0cgggv2hjbbkiwdl9vccq8kmrz"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-redox_users" ,rust-redox_users_0_3_1)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
      (beautify-description "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-dlib_0_4_1
  (package
    (name "rust-dlib")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dlib" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0smp2cdvy12xfw26qyqms273w5anszfadv73g75s88yqm54i5rbp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libloading" ,rust-libloading_0_5_2))))
    (home-page "None")
    (synopsis "Helper macros for handling manually loading optional system libraries.")
    (description
      (beautify-description "Helper macros for handling manually loading optional system libraries."))
    (license (spdx-string->license "MIT"))))

(define rust-downcast-rs_1_0_4
  (package
    (name "rust-downcast-rs")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "downcast-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03q2pqjk1wik3agbwgsypah7qziqbpwp41bmpw62cx9gbkyjvfgj"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Trait object downcasting support using only safe Rust. It supports type\nparameters, associated types, and type constraints.")
    (description
      (beautify-description "Trait object downcasting support using only safe Rust. It supports type\nparameters, associated types, and type constraints."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-dtoa_0_4_4
  (package
    (name "rust-dtoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dtoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0phbm7i0dpn44gzi07683zxaicjap5064w62pidci4fhhciv8mza"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Fast functions for printing floating-point primitives to an io::Write")
    (description
      (beautify-description "Fast functions for printing floating-point primitives to an io::Write"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-dunce_1_0_0
  (package
    (name "rust-dunce")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dunce" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kys739zvwcvsngspa4lw2dksigiima17i25c09d2j45m3v6pbfh"))))
    (build-system cargo-build-system)
    (home-page "https://crates.rs/crates/dunce")
    (synopsis "Normalize Windows paths to the most compatible format, avoiding UNC where possible")
    (description
      (beautify-description "Normalize Windows paths to the most compatible format, avoiding UNC where possible"))
    (license (spdx-string->license "CC0-1.0"))))

(define rust-dwrote_0_9_0
  (package
    (name "rust-dwrote")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dwrote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03gzl5pd90nlkmwqmbmjmyz47h7wlblbqrwv5a29npnv0ag3dl8b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-serde" ,rust-serde_1_0_99)        
       ("rust-serde_derive" ,rust-serde_derive_1_0_99)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "Lightweight binding to DirectWrite.")
    (description
      (beautify-description "Lightweight binding to DirectWrite."))
    (license (spdx-string->license "MPL-2.0"))))

(define rust-either_1_5_2
  (package
    (name "rust-either")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yyggfd5yq9hyyp0bd5jj0fgz3rwws42d19ri0znxwwqs3hcy9sm"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      (beautify-description "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-embed-resource_1_3_0
  (package
    (name "rust-embed-resource")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "embed-resource" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wx418iqn4bzlnhz5qz4gm1p9ibclg18qwv5x35i2gqv07xrqhg1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-vswhom" ,rust-vswhom_0_1_0)        
       ("rust-winreg" ,rust-winreg_0_5_1))))
    (home-page "None")
    (synopsis "A Cargo library to handle compilation and inclusion of Windows resources in the most resilient fashion imaginable")
    (description
      (beautify-description "A Cargo library to handle compilation and inclusion of Windows resources in the most resilient fashion imaginable"))
    (license (spdx-string->license "MIT"))))

(define rust-env_logger_0_5_13
  (package
    (name "rust-env_logger")
    (version "0.5.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f0c4i4c65jh8lci0afl5yg74ac0lbnpxcp81chj114zwg9a9c0m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-atty" ,rust-atty_0_2_13)        
       ("rust-humantime" ,rust-humantime_1_2_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-regex" ,rust-regex_1_2_1)        
       ("rust-termcolor" ,rust-termcolor_1_0_5))))
    (home-page "None")
    (synopsis "A logging implementation for `log` which is configured via an environment\nvariable.")
    (description
      (beautify-description "A logging implementation for `log` which is configured via an environment\nvariable."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (synopsis "A logging implementation for `log` which is configured via an environment\nvariable.")
    (description
      (beautify-description "A logging implementation for `log` which is configured via an environment\nvariable."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-euclid_0_20_1
  (package
    (name "rust-euclid")
    (version "0.20.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "euclid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m4jr8v2s541pw103vzb7lbicyc5lqg7gc0psn2jfskxwnj7kj49"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "None")
    (synopsis "Geometry primitives")
    (description
      (beautify-description "Geometry primitives"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-expat-sys_2_1_6
  (package
    (name "rust-expat-sys")
    (version "2.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "expat-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yj5pqynds776ay8wg9mhi3hvna4fv7vf244yr1864r0i5r1k3v5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cmake" ,rust-cmake_0_1_42)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15))))
    (home-page "http://www.libexpat.org/")
    (synopsis "XML parser library written in C")
    (description
      (beautify-description "XML parser library written in C"))
    (license (spdx-string->license "MIT"))))

(define rust-failure_0_1_5
  (package
    (name "rust-failure")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "failure" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qppmgv4i5jj6vrss91qackqnl0a12h7lnby4l7j5fdy78yxhnvr"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-backtrace" ,rust-backtrace_0_3_35)        
       ("rust-failure_derive" ,rust-failure_derive_0_1_5))))
    (home-page "https://rust-lang-nursery.github.io/failure/")
    (synopsis "Experimental error handling abstraction.")
    (description
      (beautify-description "Experimental error handling abstraction."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-failure_derive_0_1_5
  (package
    (name "rust-failure_derive")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "failure_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q97n7dp51j5hndzic9ng2fgn6f3z5ya1992w84l7vypby8n647a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-syn" ,rust-syn_0_15_44)        
       ("rust-synstructure" ,rust-synstructure_0_10_2))))
    (home-page "https://rust-lang-nursery.github.io/failure/")
    (synopsis "derives for the failure crate")
    (description
      (beautify-description "derives for the failure crate"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-filetime_0_2_5
  (package
    (name "rust-filetime")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "filetime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17a213hfrmvslspn5h04p4r08la2mly5nl24ywggb8fb7w1n731g"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56))))
    (home-page "https://github.com/alexcrichton/filetime")
    (synopsis "Platform-agnostic accessors of timestamps in File metadata")
    (description
      (beautify-description "Platform-agnostic accessors of timestamps in File metadata"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-flate2_1_0_11
  (package
    (name "rust-flate2")
    (version "1.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flate2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10j6bpgpipywmrsxxmp1q48qd9vp1c4fs64y2hv02r48cfxgznia"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crc32fast" ,rust-crc32fast_1_2_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-miniz_oxide" ,rust-miniz_oxide_0_3_2))))
    (home-page "https://github.com/alexcrichton/flate2-rs")
    (synopsis "Bindings to miniz.c for DEFLATE compression and decompression exposed as\nReader/Writer streams. Contains bindings for zlib, deflate, and gzip-based\nstreams.")
    (description
      (beautify-description "Bindings to miniz.c for DEFLATE compression and decompression exposed as\nReader/Writer streams. Contains bindings for zlib, deflate, and gzip-based\nstreams."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (synopsis "Fowler\u2013Noll\u2013Vo hash function")
    (description
      (beautify-description "Fowler\u2013Noll\u2013Vo hash function"))
    (license `((spdx-string->license "Apache-2.0 ")
               (spdx-string->license " MIT")))))

(define rust-font_0_1_0
  (package
    (name "rust-font")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jwilm/alacritty")
               (commit "2f93fb34b1b237bea4d1bea67a67c7c8efe6e5dc")))
        (file-name (git-file-name name version))
        (sha256
          (base32 
            "1kf5zkcadg6vmqz4fmxyj7nhybsz4kdkf5x5dwha8fkqha237a4i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-core-foundation" ,rust-core-foundation_0_6_4)        
       ("rust-core-foundation-sys" ,rust-core-foundation-sys_0_6_2)        
       ("rust-core-graphics" ,rust-core-graphics_0_17_3)        
       ("rust-core-text" ,rust-core-text_13_3_0)        
       ("rust-dwrote" ,rust-dwrote_0_9_0)        
       ("rust-euclid" ,rust-euclid_0_20_1)        
       ("rust-foreign-types" ,rust-foreign-types_0_4_0)        
       ("rust-freetype-rs" ,rust-freetype-rs_0_19_1)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-servo-fontconfig" ,rust-servo-fontconfig_0_4_0))))
    (home-page "FILLMEIN")
    (synopsis "Font rendering using the best available solution per platform")
    (description
      (beautify-description "Font rendering using the best available solution per platform"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-foreign-types_0_3_2
  (package
    (name "rust-foreign-types")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-foreign-types-shared" ,rust-foreign-types-shared_0_1_1))))
    (home-page "None")
    (synopsis "A framework for Rust wrappers over C APIs")
    (description
      (beautify-description "A framework for Rust wrappers over C APIs"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-foreign-types_0_4_0
  (package
    (name "rust-foreign-types")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ca4i38yrf9iy5k47lr1ylb3rvcbn36d81k5pr5kzf6kmj6p111n"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-foreign-types-macros" ,rust-foreign-types-macros_0_1_0)        
       ("rust-foreign-types-shared" ,rust-foreign-types-shared_0_2_0))))
    (home-page "None")
    (synopsis "A framework for Rust wrappers over C APIs")
    (description
      (beautify-description "A framework for Rust wrappers over C APIs"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-foreign-types-macros_0_1_0
  (package
    (name "rust-foreign-types-macros")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16yjigjcsklcwy2ad32l24k1nwm9n3bsnyhxc3z9whjbsrj60qk6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-syn" ,rust-syn_0_15_44))))
    (home-page "None")
    (synopsis "An internal crate used by foreign-types")
    (description
      (beautify-description "An internal crate used by foreign-types"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-foreign-types-shared_0_1_1
  (package
    (name "rust-foreign-types-shared")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An internal crate used by foreign-types")
    (description
      (beautify-description "An internal crate used by foreign-types"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-foreign-types-shared_0_2_0
  (package
    (name "rust-foreign-types-shared")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kanxlif1vp0ffh2r9l610jqbkmb3183yqykxq1z5w1vay2rn7y6"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An internal crate used by foreign-types")
    (description
      (beautify-description "An internal crate used by foreign-types"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-freetype-rs_0_19_1
  (package
    (name "rust-freetype-rs")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "freetype-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01g78ib2n1sdrbc6hp08nqgdrbl3k8zjd7i7whqvdrqf0jkr5k18"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-freetype-sys" ,rust-freetype-sys_0_7_1)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/PistonDevelopers/freetype-rs")
    (synopsis "Bindings for FreeType font library")
    (description
      (beautify-description "Bindings for FreeType font library"))
    (license (spdx-string->license "MIT"))))

(define rust-freetype-sys_0_7_1
  (package
    (name "rust-freetype-sys")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "freetype-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f2x5cqlv7fbjpsf1hxlh0jbkfmzzr3kcqlhl98nxkz7rik8d70g"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-libz-sys" ,rust-libz-sys_1_0_25)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15))))
    (home-page "https://github.com/PistonDevelopers/freetype-sys")
    (synopsis "Low level binding for FreeType font library")
    (description
      (beautify-description "Low level binding for FreeType font library"))
    (license (spdx-string->license "MIT"))))

(define rust-fsevent_0_4_0
  (package
    (name "rust-fsevent")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fsevent" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1djxnc2fmv265xqf1iyfz56smh13v9r1p0w9125wjg6k3fyx3dss"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-fsevent-sys" ,rust-fsevent-sys_2_0_1))))
    (home-page "None")
    (synopsis "Rust bindings to the fsevent-sys macOS API for file changes notifications")
    (description
      (beautify-description "Rust bindings to the fsevent-sys macOS API for file changes notifications"))
    (license (spdx-string->license "MIT"))))

(define rust-fsevent-sys_2_0_1
  (package
    (name "rust-fsevent-sys")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fsevent-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18246vxk7rqn52m0sfrhivxq802i34p2wqqx5zsa0pamjj5086zl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Rust bindings to the fsevent macOS API for file changes notifications")
    (description
      (beautify-description "Rust bindings to the fsevent macOS API for file changes notifications"))
    (license (spdx-string->license "MIT"))))

(define rust-fuchsia-cprng_0_1_1
  (package
    (name "rust-fuchsia-cprng")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-cprng" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fnkqrbz7ixxzsb04bsz9p0zzazanma8znfdqjvh39n14vapfvx0"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Rust crate for the Fuchsia cryptographically secure pseudorandom number generator")
    (description
      (beautify-description "Rust crate for the Fuchsia cryptographically secure pseudorandom number generator"))
    (license (spdx-string->license "non-standard"))))

(define rust-fuchsia-zircon_0_3_3
  (package
    (name "rust-fuchsia-zircon")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10jxc5ks1x06gpd0xg51kcjrxr35nj6qhx2zlc5n7bmskv3675rf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys_0_3_3))))
    (home-page "None")
    (synopsis "Rust bindings for the Zircon kernel")
    (description
      (beautify-description "Rust bindings for the Zircon kernel"))
    (license (spdx-string->license "BSD-3-Clause"))))

(define rust-fuchsia-zircon-sys_0_3_3
  (package
    (name "rust-fuchsia-zircon-sys")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19zp2085qsyq2bh1gvcxq1lb8w6v6jj9kbdkhpdjrl95fypakjix"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Low-level Rust bindings for the Zircon kernel")
    (description
      (beautify-description "Low-level Rust bindings for the Zircon kernel"))
    (license (spdx-string->license "BSD-3-Clause"))))

(define rust-getrandom_0_1_11
  (package
    (name "rust-getrandom")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getrandom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wiq4i9vh4zwc4q4kbvr4sa45k51n2wy4pwb3q9yp3w6sc14nd7w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-wasi" ,rust-wasi_0_5_0))))
    (home-page "None")
    (synopsis "A small cross-platform library for retrieving random data from system source")
    (description
      (beautify-description "A small cross-platform library for retrieving random data from system source"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-gif_0_10_2
  (package
    (name "rust-gif")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gif" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s7mm8i971i0clm8xs6kvbl8yins7cib4isrxs35rq6njysz5hl6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-color_quant" ,rust-color_quant_1_0_1)        
       ("rust-lzw" ,rust-lzw_0_10_0))))
    (home-page "https://github.com/image-rs/image-gif")
    (synopsis "GIF de- and encoder")
    (description
      (beautify-description "GIF de- and encoder"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-gl_generator_0_11_0
  (package
    (name "rust-gl_generator")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gl_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gdchvay0k0g931b2ki33mkfixcw4radk5b8sqsm29rahxg3v8ir"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-khronos_api" ,rust-khronos_api_3_1_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-xml-rs" ,rust-xml-rs_0_8_0))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (description
      (beautify-description "Code generators for creating bindings to the Khronos OpenGL APIs."))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-gl_generator_0_13_1
  (package
    (name "rust-gl_generator")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gl_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jpqjqpyrl73sf8y20p5rv50qz8glnsvv9infg8h4vi52zgbp66a"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-khronos_api" ,rust-khronos_api_3_1_0)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-xml-rs" ,rust-xml-rs_0_8_0))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (description
      (beautify-description "Code generators for creating bindings to the Khronos OpenGL APIs."))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-gleam_0_6_19
  (package
    (name "rust-gleam")
    (version "0.6.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gleam" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iazvk3kvw3620gm6x8hy2x1lz51k04acl78cr3ppryhk5y0vqfa"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-gl_generator" ,rust-gl_generator_0_13_1))))
    (home-page "None")
    (synopsis "Generated OpenGL bindings and wrapper for Servo.")
    (description
      (beautify-description "Generated OpenGL bindings and wrapper for Servo."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-glob_0_2_11
  (package
    (name "rust-glob")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ysvi72slkw784fcsymgj4308c3y03gwjjzqxp80xdjnkbh8vqcb"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/glob")
    (synopsis "Support for matching file paths against Unix shell style patterns.")
    (description
      (beautify-description "Support for matching file paths against Unix shell style patterns."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-glutin_0_21_1
  (package
    (name "rust-glutin")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jcr3fg5wmq32db4jjvrs9867d61z6ivwcv12qsibzmvn6ifg34k"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-android_glue" ,rust-android_glue_0_2_3)        
       ("rust-cgl" ,rust-cgl_0_2_3)        
       ("rust-cocoa" ,rust-cocoa_0_18_4)        
       ("rust-core-foundation" ,rust-core-foundation_0_6_4)        
       ("rust-core-graphics" ,rust-core-graphics_0_17_3)        
       ("rust-derivative" ,rust-derivative_1_0_2)        
       ("rust-glutin_egl_sys" ,rust-glutin_egl_sys_0_1_3)        
       ("rust-glutin_emscripten_sys" ,rust-glutin_emscripten_sys_0_1_0)        
       ("rust-glutin_gles2_sys" ,rust-glutin_gles2_sys_0_1_3)        
       ("rust-glutin_glx_sys" ,rust-glutin_glx_sys_0_1_5)        
       ("rust-glutin_wgl_sys" ,rust-glutin_wgl_sys_0_1_3)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libloading" ,rust-libloading_0_5_2)        
       ("rust-objc" ,rust-objc_0_2_6)        
       ("rust-osmesa-sys" ,rust-osmesa-sys_0_1_2)        
       ("rust-parking_lot" ,rust-parking_lot_0_9_0)        
       ("rust-wayland-client" ,rust-wayland-client_0_21_13)        
       ("rust-winapi" ,rust-winapi_0_3_8)        
       ("rust-winit" ,rust-winit_0_19_3))))
    (home-page "None")
    (synopsis "Cross-platform OpenGL context provider.")
    (description
      (beautify-description "Cross-platform OpenGL context provider."))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-glutin_egl_sys_0_1_3
  (package
    (name "rust-glutin_egl_sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin_egl_sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09nk7nknjsw2svzqrxmggc53h37xl9a9xd83v4dbdckcmf3qkx13"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-gl_generator" ,rust-gl_generator_0_11_0)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "The egl bindings for glutin")
    (description
      (beautify-description "The egl bindings for glutin"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-glutin_emscripten_sys_0_1_0
  (package
    (name "rust-glutin_emscripten_sys")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin_emscripten_sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ix0jmm8p5if4qarzdfl5mz9rbq4hhgqarakb3bzwvyz13dkynr4"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The emscripten bindings for glutin")
    (description
      (beautify-description "The emscripten bindings for glutin"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-glutin_gles2_sys_0_1_3
  (package
    (name "rust-glutin_gles2_sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin_gles2_sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pswvl5zyqmqwzjr674yzslj0al2xbqsp2ai9ggb9qbshlq6r6c9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-gl_generator" ,rust-gl_generator_0_11_0)        
       ("rust-objc" ,rust-objc_0_2_6))))
    (home-page "None")
    (synopsis "The gles2 bindings for glutin")
    (description
      (beautify-description "The gles2 bindings for glutin"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-glutin_glx_sys_0_1_5
  (package
    (name "rust-glutin_glx_sys")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin_glx_sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mxs3mil68xqqb49466n5rpwpcllj6fwqjgrcrzzmz26bv5ab40j"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-gl_generator" ,rust-gl_generator_0_11_0)        
       ("rust-x11-dl" ,rust-x11-dl_2_18_4))))
    (home-page "None")
    (synopsis "The glx bindings for glutin")
    (description
      (beautify-description "The glx bindings for glutin"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-glutin_wgl_sys_0_1_3
  (package
    (name "rust-glutin_wgl_sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin_wgl_sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08chlfzpj59q36qm212i4k879gvjzha7i90q90fds8pw3v4vn0gq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-gl_generator" ,rust-gl_generator_0_11_0))))
    (home-page "None")
    (synopsis "The wgl bindings for glutin")
    (description
      (beautify-description "The wgl bindings for glutin"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-http_req_0_5_3
  (package
    (name "rust-http_req")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http_req" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v5kvanljbzks56bvymc9b6g2mbhmdb7k58r8jwfwfm9gf83acks"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-native-tls" ,rust-native-tls_0_2_3)        
       ("rust-unicase" ,rust-unicase_2_4_0))))
    (home-page "None")
    (synopsis "simple and lightweight HTTP client with built-in HTTPS support")
    (description
      (beautify-description "simple and lightweight HTTP client with built-in HTTPS support"))
    (license (spdx-string->license "MIT"))))

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

(define rust-image_0_21_3
  (package
    (name "rust-image")
    (version "0.21.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "image" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sv534xp8yyn7jj0q6yn2bgng1350f962g81sv8v7c6pgi31wdrm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder_1_3_2)        
       ("rust-gif" ,rust-gif_0_10_2)        
       ("rust-jpeg-decoder" ,rust-jpeg-decoder_0_1_16)        
       ("rust-lzw" ,rust-lzw_0_10_0)        
       ("rust-num-iter" ,rust-num-iter_0_1_39)        
       ("rust-num-rational" ,rust-num-rational_0_2_2)        
       ("rust-num-traits" ,rust-num-traits_0_2_8)        
       ("rust-png" ,rust-png_0_14_1)        
       ("rust-scoped_threadpool" ,rust-scoped_threadpool_0_1_9)        
       ("rust-tiff" ,rust-tiff_0_2_2))))
    (home-page "https://github.com/image-rs/image")
    (synopsis "Imaging library written in Rust. Provides basic filters and decoders for the most common image formats.")
    (description
      (beautify-description "Imaging library written in Rust. Provides basic filters and decoders for the most common image formats."))
    (license (spdx-string->license "MIT"))))

(define rust-inflate_0_4_5
  (package
    (name "rust-inflate")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inflate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zxjdn8iwa0ssxrnjmywm3r1v284wryvzrf8vkc7nyf5ijbjknqw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-adler32" ,rust-adler32_1_0_3))))
    (home-page "None")
    (synopsis "DEFLATE decoding")
    (description
      (beautify-description "DEFLATE decoding"))
    (license (spdx-string->license "MIT"))))

(define rust-inotify_0_6_1
  (package
    (name "rust-inotify")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inotify" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0627k5aq44knjlrc09hl017nxap3svpl79przf26y3ciycwlbda0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-inotify-sys" ,rust-inotify-sys_0_1_3)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Idiomatic wrapper for inotify")
    (description
      (beautify-description "Idiomatic wrapper for inotify"))
    (license (spdx-string->license "ISC"))))

(define rust-inotify-sys_0_1_3
  (package
    (name "rust-inotify-sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "inotify-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1h2nwgajz80qddjm4mpma94zahxw84nscbycy9pgzbjrgjl1ljp7"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "inotify bindings for the Rust programming language")
    (description
      (beautify-description "inotify bindings for the Rust programming language"))
    (license (spdx-string->license "ISC"))))

(define rust-iovec_0_1_2
  (package
    (name "rust-iovec")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iovec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "025vi072m22299z3fg73qid188z2iip7k41ba6v5v5yhwwby9rnv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-winapi" ,rust-winapi_0_2_8))))
    (home-page "https://github.com/carllerche/iovec")
    (synopsis "Portable buffer type for scatter/gather I/O operations")
    (description
      (beautify-description "Portable buffer type for scatter/gather I/O operations"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define rust-jpeg-decoder_0_1_16
  (package
    (name "rust-jpeg-decoder")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jpeg-decoder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1flj2wq4xdzv6nqs3vk2l3jsg4lpwiz6lfrccb30kr7azs7y3an1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder_1_3_2)        
       ("rust-rayon" ,rust-rayon_1_1_0))))
    (home-page "None")
    (synopsis "JPEG decoder")
    (description
      (beautify-description "JPEG decoder"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

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

(define rust-khronos_api_3_1_0
  (package
    (name "rust-khronos_api")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "khronos_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "The Khronos XML API Registry, exposed as byte string constants.")
    (description
      (beautify-description "The Khronos XML API Registry, exposed as byte string constants."))
    (license (spdx-string->license "Apache-2.0"))))

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

(define rust-lazycell_1_2_1
  (package
    (name "rust-lazycell")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazycell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gvqycmpv7parc98i6y64ai7rvxrn1947z2a6maa02g4kvxdd55j"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A library providing a lazily filled Cell struct")
    (description
      (beautify-description "A library providing a lazily filled Cell struct"))
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

(define rust-libloading_0_5_2
  (package
    (name "rust-libloading")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libloading" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lyply8rcqc8agajzxs7bq6ivba9dnn1i68kgb9z2flnfjh13cgj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "A safer binding to platform\u2019s dynamic library loading utilities")
    (description
      (beautify-description "A safer binding to platform\u2019s dynamic library loading utilities"))
    (license (spdx-string->license "ISC"))))

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

(define rust-line_drawing_0_7_0
  (package
    (name "rust-line_drawing")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "line_drawing" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fcna7hq1g1kkkqy07hydscx5d2zgb6gskz3vnsvsif8h8ysvisw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "None")
    (synopsis "A collection of line-drawing algorithms for use in graphics and video games.")
    (description
      (beautify-description "A collection of line-drawing algorithms for use in graphics and video games."))
    (license (spdx-string->license "MIT"))))

(define rust-linked-hash-map_0_5_2
  (package
    (name "rust-linked-hash-map")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "linked-hash-map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10qgbvh00q36ql0jh00rxh2jlq6qvl11n6mig0cvkpf4xf5bd4df"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/contain-rs/linked-hash-map")
    (synopsis "A HashMap wrapper that holds key-value pairs in insertion order")
    (description
      (beautify-description "A HashMap wrapper that holds key-value pairs in insertion order"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-lock_api_0_2_0
  (package
    (name "rust-lock_api")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zx7pksmgyggpczgw4qrr4vj2nkdk5lipgiysvr20slm552nv57d"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-scopeguard" ,rust-scopeguard_1_0_0))))
    (home-page "None")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      (beautify-description "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-lock_api_0_3_1
  (package
    (name "rust-lock_api")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p04271jikw69ja0ap0plrfwm9incf1iny48g0b3ma9k4mw2x4gq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-scopeguard" ,rust-scopeguard_1_0_0))))
    (home-page "None")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      (beautify-description "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

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

(define rust-lzw_0_10_0
  (package
    (name "rust-lzw")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lzw" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1170dfskhzlh8h2bm333811hykjvpypgnvxyhhm1rllyi2xpr53x"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "LZW compression and decompression.")
    (description
      (beautify-description "LZW compression and decompression."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-malloc_buf_0_0_6
  (package
    (name "rust-malloc_buf")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "malloc_buf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Structs for handling malloc\u0027d memory passed to Rust.")
    (description
      (beautify-description "Structs for handling malloc\u0027d memory passed to Rust."))
    (license (spdx-string->license "MIT"))))

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

(define rust-maybe-uninit_2_0_0
  (package
    (name "rust-maybe-uninit")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "maybe-uninit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "004y0nzmpfdrhz251278341z6ql34iv1k6dp1h6af7d6nd6jwc30"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "MaybeUninit for friends of backwards compatibility")
    (description
      (beautify-description "MaybeUninit for friends of backwards compatibility"))
    (license (spdx-string->license "Apache-2.0 OR MIT"))))

(define rust-memchr_1_0_2
  (package
    (name "rust-memchr")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yjyja34pzhipdl855q3m21w1lyih4lw79x2dp3czwdla4pap3ql"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description
      (beautify-description "Safe interface to memchr."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

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

(define rust-memoffset_0_5_1
  (package
    (name "rust-memoffset")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memoffset" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zqpz1apkxvzbi41q07vaxpn3bmvhqqkmg8bbbpbgfrv0gdpaq6f"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rustc_version" ,rust-rustc_version_0_2_3))))
    (home-page "None")
    (synopsis "offset_of functionality for Rust structs.")
    (description
      (beautify-description "offset_of functionality for Rust structs."))
    (license (spdx-string->license "MIT"))))

(define rust-miniz_oxide_0_3_2
  (package
    (name "rust-miniz_oxide")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "041s41l5w7z8pkp93pdzn8rngxr93q4wxp034pr0cvc7bgway23i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-adler32" ,rust-adler32_1_0_3))))
    (home-page "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "DEFLATE compression and decompression library rewritten in Rust based on miniz")
    (description
      (beautify-description "DEFLATE compression and decompression library rewritten in Rust based on miniz"))
    (license (spdx-string->license "MIT"))))

(define rust-mio_0_6_19
  (package
    (name "rust-mio")
    (version "0.6.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08zzs227vrnyz5kvws6awzlgzb8zqpnihs71hkqlw07dlfb1kxc3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-fuchsia-zircon" ,rust-fuchsia-zircon_0_3_3)        
       ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys_0_3_3)        
       ("rust-iovec" ,rust-iovec_0_1_2)        
       ("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-miow" ,rust-miow_0_2_1)        
       ("rust-net2" ,rust-net2_0_2_33)        
       ("rust-slab" ,rust-slab_0_4_2)        
       ("rust-winapi" ,rust-winapi_0_2_8))))
    (home-page "https://github.com/carllerche/mio")
    (synopsis "Lightweight non-blocking IO")
    (description
      (beautify-description "Lightweight non-blocking IO"))
    (license (spdx-string->license "MIT"))))

(define rust-mio-anonymous-pipes_0_1_0
  (package
    (name "rust-mio-anonymous-pipes")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio-anonymous-pipes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bqs8wncd73q4pnbiwskhgds57hyr8g89vfpqmw1vk9dqp1p9hpq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-mio" ,rust-mio_0_6_19)        
       ("rust-miow" ,rust-miow_0_3_3)        
       ("rust-spsc-buffer" ,rust-spsc-buffer_0_1_1)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "Asynchronous wrapper for windows synchronous pipes")
    (description
      (beautify-description "Asynchronous wrapper for windows synchronous pipes"))
    (license (spdx-string->license "MIT"))))

(define rust-mio-extras_2_0_5
  (package
    (name "rust-mio-extras")
    (version "2.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio-extras" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0h6fc7pmvsh5r1vpg5xz48lyraalsmb4s4q2v2w50qpsq823mrs6"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazycell" ,rust-lazycell_1_2_1)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-mio" ,rust-mio_0_6_19)        
       ("rust-slab" ,rust-slab_0_4_2))))
    (home-page "None")
    (synopsis "Extra components for use with Mio")
    (description
      (beautify-description "Extra components for use with Mio"))
    (license (spdx-string->license "MIT"))))

(define rust-mio-named-pipes_0_1_6
  (package
    (name "rust-mio-named-pipes")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio-named-pipes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cwwfx1yr9vws8x971x34ijnirs377vcxi47frdirki5yppp9qzm"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log_0_4_8)        
       ("rust-mio" ,rust-mio_0_6_19)        
       ("rust-miow" ,rust-miow_0_3_3)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/alexcrichton/mio-named-pipes")
    (synopsis "Windows named pipe bindings for mio.")
    (description
      (beautify-description "Windows named pipe bindings for mio."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-miow_0_2_1
  (package
    (name "rust-miow")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06g9b8sqlh5gxakwqq4rrib07afwanfnxgxajrldwcgk3hxjy7wc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
       ("rust-net2" ,rust-net2_0_2_33)        
       ("rust-winapi" ,rust-winapi_0_2_8)        
       ("rust-ws2_32-sys" ,rust-ws2_32-sys_0_2_1))))
    (home-page "https://github.com/alexcrichton/miow")
    (synopsis "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O\nabstractions.")
    (description
      (beautify-description "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O\nabstractions."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-miow_0_3_3
  (package
    (name "rust-miow")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09ljvx6wg30f2xlv7b7hhpkw7k312n3hjgmrbhwzhz9x03ra0sir"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-socket2" ,rust-socket2_0_3_11)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/alexcrichton/miow")
    (synopsis "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O\nabstractions.")
    (description
      (beautify-description "A zero overhead I/O library for Windows, focusing on IOCP and Async I/O\nabstractions."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-named_pipe_0_3_0
  (package
    (name "rust-named_pipe")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "named_pipe" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vgsdvg9zhg4s6fjfwz5h75fnbalblfjrcajapbybxzmqid0mlcf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
       ("rust-winapi" ,rust-winapi_0_2_8))))
    (home-page "None")
    (synopsis "Wrapper for overlapped (asyncronous) IO of Windows\u0027s named pipes")
    (description
      (beautify-description "Wrapper for overlapped (asyncronous) IO of Windows\u0027s named pipes"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-native-tls_0_2_3
  (package
    (name "rust-native-tls")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "native-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ki7cj4wzyd2nach4qdjly69sp7rs0yz3n3z2ii4mm1gqajg2bab"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-openssl" ,rust-openssl_0_10_24)        
       ("rust-openssl-probe" ,rust-openssl-probe_0_1_2)        
       ("rust-openssl-sys" ,rust-openssl-sys_0_9_49)        
       ("rust-schannel" ,rust-schannel_0_1_15)        
       ("rust-security-framework" ,rust-security-framework_0_3_1)        
       ("rust-security-framework-sys" ,rust-security-framework-sys_0_3_1)        
       ("rust-tempfile" ,rust-tempfile_3_1_0))))
    (home-page "None")
    (synopsis "A wrapper over a platform\u0027s native TLS implementation")
    (description
      (beautify-description "A wrapper over a platform\u0027s native TLS implementation"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-net2_0_2_33
  (package
    (name "rust-net2")
    (version "0.2.33")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "net2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "126g3fgfxp06zimc1l9iyxnn9cif1hjsg7sd81nlls5nnyghsma2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/rust-lang-nursery/net2-rs")
    (synopsis "Extensions to the standard library\u0027s networking types as proposed in RFC 1158.")
    (description
      (beautify-description "Extensions to the standard library\u0027s networking types as proposed in RFC 1158."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-nix_0_14_1
  (package
    (name "rust-nix")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kmxdlmvnmq8cfpmr3g6wk37rwi2ybdvp1z6z3831m1p23p2nwkc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-cc" ,rust-cc_1_0_41)        
       ("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-void" ,rust-void_1_0_2))))
    (home-page "None")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description
      (beautify-description "Rust friendly bindings to *nix APIs"))
    (license (spdx-string->license "MIT"))))

(define rust-nodrop_0_1_13
  (package
    (name "rust-nodrop")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nodrop" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0if9ifn6rvar5jirx4b3qh4sl5kjkmcifycvzhxa9j3crkfng5ig"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A wrapper type to inhibit drop (destructor). Use std::mem::ManuallyDrop instead!")
    (description
      (beautify-description "A wrapper type to inhibit drop (destructor). Use std::mem::ManuallyDrop instead!"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-nom_3_2_1
  (package
    (name "rust-nom")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yr8fazcspgawl6s7wmx5llz61s68jl88cnrph18fa7xf06cbbh5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_1_0_2))))
    (home-page "None")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
      (beautify-description "A byte-oriented, zero-copy, parser combinators library"))
    (license (spdx-string->license "MIT"))))

(define rust-nom_4_2_3
  (package
    (name "rust-nom")
    (version "4.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mkvby8b4m61p4g1px0pwr58yfkphyp1jcfbp4qfp7l6iqdaklia"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-memchr" ,rust-memchr_2_2_1)        
       ("rust-version_check" ,rust-version_check_0_1_5))))
    (home-page "None")
    (synopsis "A byte-oriented, zero-copy, parser combinators library")
    (description
      (beautify-description "A byte-oriented, zero-copy, parser combinators library"))
    (license (spdx-string->license "MIT"))))

(define rust-notify_4_0_12
  (package
    (name "rust-notify")
    (version "4.0.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "notify" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hwrljl7936v7phgm8gp9j9pbak4slgrgkdccwcd93pa2cgxfwim"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-filetime" ,rust-filetime_0_2_5)        
       ("rust-fsevent" ,rust-fsevent_0_4_0)        
       ("rust-fsevent-sys" ,rust-fsevent-sys_2_0_1)        
       ("rust-inotify" ,rust-inotify_0_6_1)        
       ("rust-kernel32-sys" ,rust-kernel32-sys_0_2_2)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-mio" ,rust-mio_0_6_19)        
       ("rust-mio-extras" ,rust-mio-extras_2_0_5)        
       ("rust-walkdir" ,rust-walkdir_2_2_9)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/passcod/notify")
    (synopsis "Cross-platform filesystem notification library")
    (description
      (beautify-description "Cross-platform filesystem notification library"))
    (license (spdx-string->license "CC0-1.0"))))

(define rust-num-derive_0_2_5
  (package
    (name "rust-num-derive")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wnv7776fh4i40r3zfxcxcmm0dh029skx7gp4sjknz2kqm2hpzga"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-syn" ,rust-syn_0_15_44))))
    (home-page "https://github.com/rust-num/num-derive")
    (synopsis "Numeric syntax extensions")
    (description
      (beautify-description "Numeric syntax extensions"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-num-integer_0_1_41
  (package
    (name "rust-num-integer")
    (version "0.1.41")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-integer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02dwjjpfbi16c71fq689s4sw3ih52cvfzr5z5gs6qpr5z0g58pmq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6)        
       ("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "https://github.com/rust-num/num-integer")
    (synopsis "Integer traits and functions")
    (description
      (beautify-description "Integer traits and functions"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-num-iter_0_1_39
  (package
    (name "rust-num-iter")
    (version "0.1.39")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-iter" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bhk2qbr3261r6zvfc58lz4spfqjhvdripxgz5mks5rd85r55gbn"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6)        
       ("rust-num-integer" ,rust-num-integer_0_1_41)        
       ("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "https://github.com/rust-num/num-iter")
    (synopsis "External iterators for generic mathematics")
    (description
      (beautify-description "External iterators for generic mathematics"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-num-rational_0_2_2
  (package
    (name "rust-num-rational")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-rational" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m5l76rdzzq98cfhnbjsxfngz6w75pal5mnfflpxqapysmw5527j"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6)        
       ("rust-num-integer" ,rust-num-integer_0_1_41)        
       ("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "https://github.com/rust-num/num-rational")
    (synopsis "Rational numbers implementation for Rust")
    (description
      (beautify-description "Rational numbers implementation for Rust"))
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

(define rust-objc_0_2_6
  (package
    (name "rust-objc")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "objc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03ar7qxhailxgb0zi5lszv7fhwl6b1xkas5y4m8wy1vyng90zlii"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-malloc_buf" ,rust-malloc_buf_0_0_6))))
    (home-page "None")
    (synopsis "Objective-C Runtime bindings and wrapper for Rust.")
    (description
      (beautify-description "Objective-C Runtime bindings and wrapper for Rust."))
    (license (spdx-string->license "MIT"))))

(define rust-objc-foundation_0_1_1
  (package
    (name "rust-objc-foundation")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "objc-foundation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y9bwb3m5fdq7w7i4bnds067dhm4qxv4m1mbg9y61j9nkrjipp8s"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-block" ,rust-block_0_1_6)        
       ("rust-objc" ,rust-objc_0_2_6)        
       ("rust-objc_id" ,rust-objc_id_0_1_1))))
    (home-page "None")
    (synopsis "Rust wrapper for Objective-C\u0027s Foundation framework.")
    (description
      (beautify-description "Rust wrapper for Objective-C\u0027s Foundation framework."))
    (license (spdx-string->license "MIT"))))

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
      (("rust-objc" ,rust-objc_0_2_6))))
    (home-page "None")
    (synopsis "Rust smart pointers for Objective-C reference counting.")
    (description
      (beautify-description "Rust smart pointers for Objective-C reference counting."))
    (license (spdx-string->license "MIT"))))

(define rust-openssl_0_10_24
  (package
    (name "rust-openssl")
    (version "0.10.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05dg25rmg17rl3ykfl2yf69ghfd5z6zf6di38qw1awjvkddbnll1"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-foreign-types" ,rust-foreign-types_0_3_2)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-openssl-sys" ,rust-openssl-sys_0_9_49))))
    (home-page "None")
    (synopsis "OpenSSL bindings")
    (description
      (beautify-description "OpenSSL bindings"))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-openssl-probe_0_1_2
  (package
    (name "rust-openssl-probe")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-probe" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pijrdifgsdwd45b08c2g0dsmnhz7c3kmagb70839ngrd7d29bvp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/openssl-probe")
    (synopsis "Tool for helping to find SSL certificate locations on the system for OpenSSL")
    (description
      (beautify-description "Tool for helping to find SSL certificate locations on the system for OpenSSL"))
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
       ("rust-pkg-config" ,rust-pkg-config_0_3_15)        
       ("rust-vcpkg" ,rust-vcpkg_0_2_7))))
    (home-page "None")
    (synopsis "FFI bindings to OpenSSL")
    (description
      (beautify-description "FFI bindings to OpenSSL"))
    (license (spdx-string->license "MIT"))))

(define rust-ordered-float_1_0_2
  (package
    (name "rust-ordered-float")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ordered-float" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0625x96987kspdxbikry5mb7hsf5pdc5bbanxd8wjwqlx0ar71hq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "None")
    (synopsis "Wrappers for total ordering on floats")
    (description
      (beautify-description "Wrappers for total ordering on floats"))
    (license (spdx-string->license "MIT"))))

(define rust-osmesa-sys_0_1_2
  (package
    (name "rust-osmesa-sys")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "osmesa-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fq1q1zcgfb0qydrg9r2738jlwc4hqxgb9vj11z72bjxx7kfrkw8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-shared_library" ,rust-shared_library_0_1_9))))
    (home-page "None")
    (synopsis "OSMesa library bindings for Rust")
    (description
      (beautify-description "OSMesa library bindings for Rust"))
    (license (spdx-string->license "CC0-1.0"))))

(define rust-parking_lot_0_8_0
  (package
    (name "rust-parking_lot")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rrcdalr8l5zx3bw28l376321l6dnd6rqnsqsl0ygk01fy0nfxzs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lock_api" ,rust-lock_api_0_2_0)        
       ("rust-parking_lot_core" ,rust-parking_lot_core_0_5_0)        
       ("rust-rustc_version" ,rust-rustc_version_0_2_3))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-parking_lot_0_9_0
  (package
    (name "rust-parking_lot")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lk2vq3hp88ygpgsrypdr3ss71fidnqbykva0csgxhmn5scb2hpq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lock_api" ,rust-lock_api_0_3_1)        
       ("rust-parking_lot_core" ,rust-parking_lot_core_0_6_2)        
       ("rust-rustc_version" ,rust-rustc_version_0_2_3))))
    (home-page "None")
    (synopsis "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      (beautify-description "More compact and efficient implementations of the standard synchronization primitives."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-parking_lot_core_0_5_0
  (package
    (name "rust-parking_lot_core")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1317j5a1yd03baza2kqqrxb4kr1vxa7rckw4frksl2vrncfcp26b"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-cloudabi" ,rust-cloudabi_0_0_3)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-rand" ,rust-rand_0_6_5)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56)        
       ("rust-rustc_version" ,rust-rustc_version_0_2_3)        
       ("rust-smallvec" ,rust-smallvec_0_6_10)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-parking_lot_core_0_6_2
  (package
    (name "rust-parking_lot_core")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ay67dpnrn68ryyvp720m9i8hzp189fd4d6slrs1lvmcwywv2xmq"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-cloudabi" ,rust-cloudabi_0_0_3)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56)        
       ("rust-rustc_version" ,rust-rustc_version_0_2_3)        
       ("rust-smallvec" ,rust-smallvec_0_6_10)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "An advanced API for creating custom synchronization primitives.")
    (description
      (beautify-description "An advanced API for creating custom synchronization primitives."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-peeking_take_while_0_1_2
  (package
    (name "rust-peeking_take_while")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "peeking_take_while" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16bhqr6rdyrp12zv381cxaaqqd0pwysvm1q8h2ygihvypvfprc8r"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Like `Iterator::take_while`, but calls the predicate on a peeked value. This allows you to use `Iterator::by_ref` and `Iterator::take_while` together, and still get the first value for which the `take_while` predicate returned false after dropping the `by_ref`.")
    (description
      (beautify-description "Like `Iterator::take_while`, but calls the predicate on a peeked value. This allows you to use `Iterator::by_ref` and `Iterator::take_while` together, and still get the first value for which the `take_while` predicate returned false after dropping the `by_ref`."))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

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

(define rust-phf_0_7_24
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

(define rust-phf_codegen_0_7_24
  (package
    (name "rust-phf_codegen")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zjiblicfm0nrmr2xxrs6pnf6zz2394wgch6dcbd8jijkq98agmh"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-phf_generator" ,rust-phf_generator_0_7_24)        
       ("rust-phf_shared" ,rust-phf_shared_0_7_24))))
    (home-page "None")
    (synopsis "Codegen library for PHF types")
    (description
      (beautify-description "Codegen library for PHF types"))
    (license (spdx-string->license "MIT"))))

(define rust-phf_generator_0_7_24
  (package
    (name "rust-phf_generator")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qi62gxk3x3whrmw5c4i71406icqk11qmpgln438p6qm7k4lqdh9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-phf_shared" ,rust-phf_shared_0_7_24)        
       ("rust-rand" ,rust-rand_0_6_5))))
    (home-page "None")
    (synopsis "PHF generation logic")
    (description
      (beautify-description "PHF generation logic"))
    (license (spdx-string->license "MIT"))))

(define rust-phf_shared_0_7_24
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
    (synopsis "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts.")
    (description
      (beautify-description "A library to run the pkg-config system tool at build time in order to be used in\nCargo build scripts."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-png_0_14_1
  (package
    (name "rust-png")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "png" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nf3a8r9p9zrj4x30b48f7yv18dz9xkmrq9b3lnzmpnhzn0z9nk3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-deflate" ,rust-deflate_0_7_20)        
       ("rust-inflate" ,rust-inflate_0_4_5)        
       ("rust-num-iter" ,rust-num-iter_0_1_39))))
    (home-page "None")
    (synopsis "PNG decoding and encoding library in pure Rust")
    (description
      (beautify-description "PNG decoding and encoding library in pure Rust"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-podio_0_1_6
  (package
    (name "rust-podio")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "podio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ga5arhwakj5rwrqzf9410zrbwnf24jd59af8kr9rgwbd6vb83vq"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Additional trait for Read and Write to read and write Plain Old Data")
    (description
      (beautify-description "Additional trait for Read and Write to read and write Plain Old Data"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-ppv-lite86_0_2_5
  (package
    (name "rust-ppv-lite86")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ppv-lite86" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06snnv338w341nicfqba2jgln5dsla72ndkgrw7h1dfdb3vgkjz3"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description
      (beautify-description "Implementation of the crypto-simd API for x86"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-proc-macro2_0_4_30
  (package
    (name "rust-proc-macro2")
    (version "0.4.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nd71fl24sys066jrha6j7i34nfkjv44yzw8yww9742wmc8j0gfg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-unicode-xid" ,rust-unicode-xid_0_1_0))))
    (home-page "https://github.com/alexcrichton/proc-macro2")
    (synopsis "A stable implementation of the upcoming new `proc_macro` API. Comes with an\noption, off by default, to also reimplement itself in terms of the upstream\nunstable API.")
    (description
      (beautify-description "A stable implementation of the upcoming new `proc_macro` API. Comes with an\noption, off by default, to also reimplement itself in terms of the upstream\nunstable API."))
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
    (synopsis "A stable implementation of the upcoming new `proc_macro` API. Comes with an\noption, off by default, to also reimplement itself in terms of the upstream\nunstable API.")
    (description
      (beautify-description "A stable implementation of the upcoming new `proc_macro` API. Comes with an\noption, off by default, to also reimplement itself in terms of the upstream\nunstable API."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

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

(define rust-quote_0_3_15
  (package
    (name "rust-quote")
    (version "0.3.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yhnnix4dzsv8y4wwz4csbnqjfh73al33j35msr10py6cl5r4vks"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
      (beautify-description "Quasi-quoting macro quote!(...)"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-quote_0_6_13
  (package
    (name "rust-quote")
    (version "0.6.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qgqq48jymp5h4y082aanf25hrw6bpb678xh3zw993qfhxmkpqkc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30))))
    (home-page "None")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description
      (beautify-description "Quasi-quoting macro quote!(...)"))
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

(define rust-rand_0_6_5
  (package
    (name "rust-rand")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jl4449jcl4wgmzld6ffwqj5gwxrp8zvx8w573g1z368qg6xlwbd"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-rand_chacha" ,rust-rand_chacha_0_1_1)        
       ("rust-rand_core" ,rust-rand_core_0_4_2)        
       ("rust-rand_hc" ,rust-rand_hc_0_1_0)        
       ("rust-rand_isaac" ,rust-rand_isaac_0_1_1)        
       ("rust-rand_jitter" ,rust-rand_jitter_0_1_4)        
       ("rust-rand_os" ,rust-rand_os_0_1_3)        
       ("rust-rand_pcg" ,rust-rand_pcg_0_1_2)        
       ("rust-rand_xorshift" ,rust-rand_xorshift_0_1_1)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://crates.io/crates/rand")
    (synopsis "Random number generators and other randomness functionality.")
    (description
      (beautify-description "Random number generators and other randomness functionality."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_0_7_0
  (package
    (name "rust-rand")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b05gwx8nnxr9bydyjxd1rszdvqnm946ky15z103ssfrhc7anznl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-getrandom" ,rust-getrandom_0_1_11)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-rand_chacha" ,rust-rand_chacha_0_2_1)        
       ("rust-rand_core" ,rust-rand_core_0_5_1)
       ("rust-rand_hc" ,rust-rand_hc_0_2_0))))
    (home-page "https://crates.io/crates/rand")
    (synopsis "Random number generators and other randomness functionality.")
    (description
      (beautify-description "Random number generators and other randomness functionality."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_chacha_0_1_1
  (package
    (name "rust-rand_chacha")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vxwyzs4fy1ffjc8l00fsyygpiss135irjf7nyxgq2v0lqf3lvam"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6)        
       ("rust-rand_core" ,rust-rand_core_0_3_1))))
    (home-page "https://crates.io/crates/rand_chacha")
    (synopsis "ChaCha random number generator")
    (description
      (beautify-description "ChaCha random number generator"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_chacha_0_2_1
  (package
    (name "rust-rand_chacha")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lv8imzzl4h2glm6sjj8mkvasgi8jym23ya48dakyln7m06sk8h3"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-c2-chacha" ,rust-c2-chacha_0_2_2)        
       ("rust-rand_core" ,rust-rand_core_0_5_1))))
    (home-page "https://crates.io/crates/rand_chacha")
    (synopsis "ChaCha random number generator")
    (description
      (beautify-description "ChaCha random number generator"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_core_0_3_1
  (package
    (name "rust-rand_core")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jzdgszfa4bliigiy4hi66k7fs3gfwi2qxn8vik84ph77fwdwvvs"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rand_core" ,rust-rand_core_0_4_2))))
    (home-page "https://crates.io/crates/rand_core")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_core_0_4_2
  (package
    (name "rust-rand_core")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p09ynysrq1vcdlmcqnapq4qakl2yd1ng3kxh3qscpx09k2a6cww"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_core")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_core_0_5_1
  (package
    (name "rust-rand_core")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06bdvx08v3rkz451cm7z59xwwqn1rkfh6v9ay77b14f8dwlybgch"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-getrandom" ,rust-getrandom_0_1_11))))
    (home-page "https://crates.io/crates/rand_core")
    (synopsis "Core random number generator traits and tools for implementation.")
    (description
      (beautify-description "Core random number generator traits and tools for implementation."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-rand_hc_0_1_0
  (package
    (name "rust-rand_hc")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_hc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i0vl8q5ddvvy0x8hf1zxny393miyzxkwqnw31ifg6p0gdy6fh3v"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rand_core" ,rust-rand_core_0_3_1))))
    (home-page "https://crates.io/crates/rand_hc")
    (synopsis "HC128 random number generator")
    (description
      (beautify-description "HC128 random number generator"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_hc_0_2_0
  (package
    (name "rust-rand_hc")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_hc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g31sqwpmsirdlwr0svnacr4dbqyz339im4ssl9738cjgfpjjcfa"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rand_core" ,rust-rand_core_0_5_1))))
    (home-page "https://crates.io/crates/rand_hc")
    (synopsis "HC128 random number generator")
    (description
      (beautify-description "HC128 random number generator"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_isaac_0_1_1
  (package
    (name "rust-rand_isaac")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_isaac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "027flpjr4znx2csxk7gxb7vrf9c7y5mydmvg5az2afgisp4rgnfy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rand_core" ,rust-rand_core_0_3_1))))
    (home-page "https://crates.io/crates/rand_isaac")
    (synopsis "ISAAC random number generator")
    (description
      (beautify-description "ISAAC random number generator"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_jitter_0_1_4
  (package
    (name "rust-rand_jitter")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_jitter" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16z387y46bfz3csc42zxbjq89vcr1axqacncvv8qhyy93p4xarhi"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-rand_core" ,rust-rand_core_0_4_2)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "Random number generator based on timing jitter")
    (description
      (beautify-description "Random number generator based on timing jitter"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-rand_os_0_1_3
  (package
    (name "rust-rand_os")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_os" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wahppm0s64gkr2vmhcgwc0lij37in1lgfxg5rbgqlz0l5vgcxbv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cloudabi" ,rust-cloudabi_0_0_3)        
       ("rust-fuchsia-cprng" ,rust-fuchsia-cprng_0_1_1)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-rand_core" ,rust-rand_core_0_4_2)        
       ("rust-rdrand" ,rust-rdrand_0_4_0)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://crates.io/crates/rand_os")
    (synopsis "OS backed Random Number Generator")
    (description
      (beautify-description "OS backed Random Number Generator"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_pcg_0_1_2
  (package
    (name "rust-rand_pcg")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_pcg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i0bdla18a8x4jn1w0fxsbs3jg7ajllz6azmch1zw33r06dv1ydb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-autocfg" ,rust-autocfg_0_1_6)        
       ("rust-rand_core" ,rust-rand_core_0_4_2))))
    (home-page "https://crates.io/crates/rand_pcg")
    (synopsis "Selected PCG random number generators")
    (description
      (beautify-description "Selected PCG random number generators"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rand_xorshift_0_1_1
  (package
    (name "rust-rand_xorshift")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_xorshift" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p2x8nr00hricpi2m6ca5vysiha7ybnghz79yqhhx6sl4gkfkxyb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rand_core" ,rust-rand_core_0_3_1))))
    (home-page "https://crates.io/crates/rand_xorshift")
    (synopsis "Xorshift random number generator")
    (description
      (beautify-description "Xorshift random number generator"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rayon_1_1_0
  (package
    (name "rust-rayon")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "190hkbcdfvcphyyzkdg52zdia2y9d9yanpm072bmnzbn49p1ic54"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-deque" ,rust-crossbeam-deque_0_6_3)        
       ("rust-either" ,rust-either_1_5_2)        
       ("rust-rayon-core" ,rust-rayon-core_1_5_0))))
    (home-page "None")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description
      (beautify-description "Simple work-stealing parallelism for Rust"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-rayon-core_1_5_0
  (package
    (name "rust-rayon-core")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ljva6blaf1wmzvg77h1i9pd0hsmsbbcmdk7sjbw7h2s8gw0vgpb"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-crossbeam-deque" ,rust-crossbeam-deque_0_6_3)        
       ("rust-crossbeam-queue" ,rust-crossbeam-queue_0_1_2)        
       ("rust-crossbeam-utils" ,rust-crossbeam-utils_0_6_6)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-num_cpus" ,rust-num_cpus_1_10_1))))
    (home-page "None")
    (synopsis "Core APIs for Rayon")
    (description
      (beautify-description "Core APIs for Rayon"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-rdrand_0_4_0
  (package
    (name "rust-rdrand")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rdrand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cjq0kwx1bk7jx3kzyciiish5gqsj7620dm43dc52sr8fzmm9037"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-rand_core" ,rust-rand_core_0_3_1))))
    (home-page "None")
    (synopsis "An implementation of random number generator based on rdrand and rdseed instructions")
    (description
      (beautify-description "An implementation of random number generator based on rdrand and rdseed instructions"))
    (license (spdx-string->license "ISC"))))

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

(define rust-redox_users_0_3_1
  (package
    (name "rust-redox_users")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_users" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vdn688q9wg997b1x5abx2gf7406rn1lvd62ypcgh1gj7g5dpkjf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-failure" ,rust-failure_0_1_5)        
       ("rust-rand_os" ,rust-rand_os_0_1_3)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56)        
       ("rust-rust-argon2" ,rust-rust-argon2_0_5_1))))
    (home-page "None")
    (synopsis "A Rust library to access Redox users and groups functionality")
    (description
      (beautify-description "A Rust library to access Redox users and groups functionality"))
    (license (spdx-string->license "MIT"))))

(define rust-regex_0_2_11
  (package
    (name "rust-regex")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1163ir1k5zjspirfjl4wqbviwrxlhmfwy95xxb69y4irkv4snack"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-aho-corasick" ,rust-aho-corasick_0_6_10)        
       ("rust-memchr" ,rust-memchr_2_2_1)        
       ("rust-regex-syntax" ,rust-regex-syntax_0_5_6)        
       ("rust-thread_local" ,rust-thread_local_0_3_6)        
       ("rust-utf8-ranges" ,rust-utf8-ranges_1_0_4))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (synopsis "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs.")
    (description
      (beautify-description "An implementation of regular expressions for Rust. This implementation uses\nfinite automata and guarantees linear time matching on all inputs."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-regex-syntax_0_5_6
  (package
    (name "rust-regex-syntax")
    (version "0.5.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19zp25jr3dhmclg3qqjk3bh1yrn7bqi05zgr5v52szv3l97plw3x"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-ucd-util" ,rust-ucd-util_0_1_5))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      (beautify-description "A regular expression parser."))
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

(define rust-remove_dir_all_0_5_2
  (package
    (name "rust-remove_dir_all")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "remove_dir_all" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bkrlyg26mgizpiy1yb2hhpgscxcag8r5fnckqsvk25608vzm0sa"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "A safe, reliable implementation of remove_dir_all for Windows")
    (description
      (beautify-description "A safe, reliable implementation of remove_dir_all for Windows"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rfind_url_0_4_2
  (package
    (name "rust-rfind_url")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rfind_url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0syk8q2qbf9f4px8lp3g9214px65r4xrflhd563bksl6chrgdx64"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Parser to search strings for URLs in reverse order")
    (description
      (beautify-description "Parser to search strings for URLs in reverse order"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rust-argon2_0_5_1
  (package
    (name "rust-rust-argon2")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust-argon2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1krjkmyfn37hy7sfs6lqia0fsvw130nn1z2850glsjcva7pym92c"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64_0_10_1)        
       ("rust-blake2b_simd" ,rust-blake2b_simd_0_5_7)        
       ("rust-crossbeam-utils" ,rust-crossbeam-utils_0_6_6))))
    (home-page "https://github.com/sru-systems/rust-argon2")
    (synopsis "Rust implementation of the Argon2 password hashing function.")
    (description
      (beautify-description "Rust implementation of the Argon2 password hashing function."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rustc-demangle_0_1_16
  (package
    (name "rust-rustc-demangle")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10qp42sl1wrdbgbbh8rnay2grm976z7hqgz32c4y09l1c071qsac"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/rustc-demangle")
    (synopsis "Rust compiler symbol demangling.")
    (description
      (beautify-description "Rust compiler symbol demangling."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rustc_tools_util_0_2_0
  (package
    (name "rust-rustc_tools_util")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc_tools_util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vj4ymv29igs7n52m12k138zbsn5k5d7ya4sys6lig7sx7ddl9dp"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "small helper to generate version information for git packages")
    (description
      (beautify-description "small helper to generate version information for git packages"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rustc_version_0_2_3
  (package
    (name "rust-rustc_version")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-semver" ,rust-semver_0_9_0))))
    (home-page "None")
    (synopsis "A library for querying the version of a installed rustc compiler")
    (description
      (beautify-description "A library for querying the version of a installed rustc compiler"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-rusttype_0_7_7
  (package
    (name "rust-rusttype")
    (version "0.7.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rusttype" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f8ip0xkdsnzv4763whm07yg08713icgcz0hi8k4n1q53bb06hb5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-approx" ,rust-approx_0_3_2)        
       ("rust-arrayvec" ,rust-arrayvec_0_4_11)        
       ("rust-ordered-float" ,rust-ordered-float_1_0_2)        
       ("rust-stb_truetype" ,rust-stb_truetype_0_2_6))))
    (home-page "https://gitlab.redox-os.org/redox-os/rusttype")
    (synopsis "A pure Rust alternative to libraries like FreeType.\n\nRustType provides an API for loading, querying and rasterising TrueType fonts.\n\nIt also provides an implementation of a dynamic GPU glyph cache for hardware font rendering.")
    (description
      (beautify-description "A pure Rust alternative to libraries like FreeType.\n\nRustType provides an API for loading, querying and rasterising TrueType fonts.\n\nIt also provides an implementation of a dynamic GPU glyph cache for hardware font rendering."))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

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

(define rust-schannel_0_1_15
  (package
    (name "rust-schannel")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "schannel" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f9k4pm8yc3z0n1n8hazvnrvg52f0sfxjc91bhf3r76rb3rapxpj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl")
    (description
      (beautify-description "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl"))
    (license (spdx-string->license "MIT"))))

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

(define rust-scopeguard_1_0_0
  (package
    (name "rust-scopeguard")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03aay84r1f6w87ckbpj6cc4rnsxkxcfs13n5ynxjia0qkgjiabml"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies.")
    (description
      (beautify-description "A RAII scope guard that will run a given closure when it goes out of scope,\neven if the code between panics (assuming unwinding panic).\n\nDefines the macros `defer!`, `defer_on_unwind!`, `defer_on_success!` as\nshorthands for guards with one of the implemented strategies."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-security-framework_0_3_1
  (package
    (name "rust-security-framework")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hmdsdj061wk76g3fajbfjnw74p0q45hy8hfngp7diwy987kvrpf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-core-foundation" ,rust-core-foundation_0_6_4)        
       ("rust-core-foundation-sys" ,rust-core-foundation-sys_0_6_2)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-security-framework-sys" ,rust-security-framework-sys_0_3_1))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description
      (beautify-description "Security.framework bindings for macOS and iOS"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-security-framework-sys_0_3_1
  (package
    (name "rust-security-framework-sys")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mlsakq9kmqyc0fg2hcbgm6rjk55mb0rhjw2wid3hqdzkjcghdln"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-core-foundation-sys" ,rust-core-foundation-sys_0_6_2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description
      (beautify-description "Apple `Security.framework` low-level FFI bindings"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-semver_0_9_0
  (package
    (name "rust-semver")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-semver-parser" ,rust-semver-parser_0_7_0))))
    (home-page "https://docs.rs/crate/semver/")
    (synopsis "Semantic version parsing and comparison.")
    (description
      (beautify-description "Semantic version parsing and comparison."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-semver-parser_0_7_0
  (package
    (name "rust-semver-parser")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/steveklabnik/semver-parser")
    (synopsis "Parsing of the semver spec.")
    (description
      (beautify-description "Parsing of the semver spec."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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

(define rust-serde_yaml_0_8_9
  (package
    (name "rust-serde_yaml")
    (version "0.8.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_yaml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10mmjpnshgrwij01a13679nxy1hnh5yfr0343kh0y9p5j2d8mc1q"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-dtoa" ,rust-dtoa_0_4_4)        
       ("rust-linked-hash-map" ,rust-linked-hash-map_0_5_2)        
       ("rust-serde" ,rust-serde_1_0_99)        
       ("rust-yaml-rust" ,rust-yaml-rust_0_4_3))))
    (home-page "None")
    (synopsis "YAML support for Serde")
    (description
      (beautify-description "YAML support for Serde"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-servo-fontconfig_0_4_0
  (package
    (name "rust-servo-fontconfig")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "servo-fontconfig" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nach6s4hdf86jz5hlm4p5r7vin91cs7gg89mr533id5fpbzi250"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-servo-fontconfig-sys" ,rust-servo-fontconfig-sys_4_0_7))))
    (home-page "None")
    (synopsis "Rust bindings for fontconfig")
    (description
      (beautify-description "Rust bindings for fontconfig"))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

(define rust-servo-fontconfig-sys_4_0_7
  (package
    (name "rust-servo-fontconfig-sys")
    (version "4.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "servo-fontconfig-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13bv3b9x1mfk3dhiw3qb9s1461y4k0fjilcag70jbgfgvld20vdl"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-expat-sys" ,rust-expat-sys_2_1_6)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15)        
       ("rust-servo-freetype-sys" ,rust-servo-freetype-sys_4_0_3))))
    (home-page "http://fontconfig.org")
    (synopsis "Font configuration and customization library")
    (description
      (beautify-description "Font configuration and customization library"))
    (license (spdx-string->license "MIT"))))

(define rust-servo-freetype-sys_4_0_3
  (package
    (name "rust-servo-freetype-sys")
    (version "4.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jwilm/alacritty")
               (commit "2f93fb34b1b237bea4d1bea67a67c7c8efe6e5dc")))
        (file-name (git-file-name name version))
        (sha256
          (base32 
            "1kf5zkcadg6vmqz4fmxyj7nhybsz4kdkf5x5dwha8fkqha237a4i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-freetype-sys" ,rust-freetype-sys_0_7_1))))
    (home-page "FILLMEIN")
    (synopsis "")
    (description
      (beautify-description ""))
    (license #f)))

(define rust-shared_library_0_1_9
  (package
    (name "rust-shared_library")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shared_library" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04fs37kdak051hm524a360978g58ayrcarjsbf54vqps5c7px7js"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Easily bind to and load shared libraries")
    (description
      (beautify-description "Easily bind to and load shared libraries"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-signal-hook_0_1_10
  (package
    (name "rust-signal-hook")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "signal-hook" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s6wysqm7i0hgag6hlfnswpkcj5sh2dhlid6pdhrzaiskzaw8qag"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-mio" ,rust-mio_0_6_19)        
       ("rust-signal-hook-registry" ,rust-signal-hook-registry_1_1_1))))
    (home-page "None")
    (synopsis "Unix signal handling")
    (description
      (beautify-description "Unix signal handling"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-signal-hook-registry_1_1_1
  (package
    (name "rust-signal-hook-registry")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "signal-hook-registry" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p2q8cdrkq6xcjjj097vrsrz9y98k7kkakmiif8465pr727x95qp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-arc-swap" ,rust-arc-swap_0_4_2)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Backend crate for signal-hook")
    (description
      (beautify-description "Backend crate for signal-hook"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-siphasher_0_2_3
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
    (synopsis "SipHash functions from rust-core \u003c 1.13")
    (description
      (beautify-description "SipHash functions from rust-core \u003c 1.13"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-slab_0_4_2
  (package
    (name "rust-slab")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slab" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y59xsa27jk84sxzswjk60xcjf8b4fm5960jwpznrrcmasyva4f1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/carllerche/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description
      (beautify-description "Pre-allocated storage for a uniform data type"))
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
    (synopsis "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack")
    (description
      (beautify-description "\u0027Small vector\u0027 optimization: store up to a small number of items on the stack"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-smithay-client-toolkit_0_4_6
  (package
    (name "rust-smithay-client-toolkit")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smithay-client-toolkit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yj8yzd0lhqpsgq0x4iikl9a02q2hnkky81brk938alv0ibqrjrc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-andrew" ,rust-andrew_0_2_1)        
       ("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-dlib" ,rust-dlib_0_4_1)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-memmap" ,rust-memmap_0_7_0)        
       ("rust-nix" ,rust-nix_0_14_1)        
       ("rust-wayland-client" ,rust-wayland-client_0_21_13)        
       ("rust-wayland-commons" ,rust-wayland-commons_0_21_13)        
       ("rust-wayland-protocols" ,rust-wayland-protocols_0_21_13))))
    (home-page "None")
    (synopsis "Toolkit for making client wayland applications.")
    (description
      (beautify-description "Toolkit for making client wayland applications."))
    (license (spdx-string->license "MIT"))))

(define rust-smithay-client-toolkit_0_6_4
  (package
    (name "rust-smithay-client-toolkit")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smithay-client-toolkit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m20687zs36l6xak2s5k9s7qp78ly8xfjpbmrhacp7whfn4hx5lk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-andrew" ,rust-andrew_0_2_1)        
       ("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-dlib" ,rust-dlib_0_4_1)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-memmap" ,rust-memmap_0_7_0)        
       ("rust-nix" ,rust-nix_0_14_1)        
       ("rust-wayland-client" ,rust-wayland-client_0_23_5)        
       ("rust-wayland-protocols" ,rust-wayland-protocols_0_23_5))))
    (home-page "None")
    (synopsis "Toolkit for making client wayland applications.")
    (description
      (beautify-description "Toolkit for making client wayland applications."))
    (license (spdx-string->license "MIT"))))

(define rust-smithay-clipboard_0_3_4
  (package
    (name "rust-smithay-clipboard")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smithay-clipboard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i79aqjzadj53lrx9m3cxqmfjizmzgkk1izm5mf8bdkxhyry15pk"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nix" ,rust-nix_0_14_1)        
       ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit_0_6_4))))
    (home-page "None")
    (synopsis "Provides access to the wayland clipboard for client applications.")
    (description
      (beautify-description "Provides access to the wayland clipboard for client applications."))
    (license (spdx-string->license "MIT"))))

(define rust-socket2_0_3_11
  (package
    (name "rust-socket2")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11bdcz04i106g4q7swkll0qxrb4287srqd2k3aq2q6i22zjlvdz8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/alexcrichton/socket2-rs")
    (synopsis "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended.")
    (description
      (beautify-description "Utilities for handling networking sockets with a maximal amount of configuration\npossible intended."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-spsc-buffer_0_1_1
  (package
    (name "rust-spsc-buffer")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spsc-buffer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fsv5zpxkax2n46flxhyajq1yblgh8f33la39gp86hksqcwkyv5y"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Single-producer single-consumer lock-free buffer")
    (description
      (beautify-description "Single-producer single-consumer lock-free buffer"))
    (license (spdx-string->license "MIT"))))

(define rust-static_assertions_0_3_4
  (package
    (name "rust-static_assertions")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "static_assertions" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lw33i89888yb3x29c6dv4mrkg3534n0rlg3r7qzh4p58xmv6gkz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nvzqz/static-assertions-rs")
    (synopsis "Compile-time assertions to ensure that invariants are met.")
    (description
      (beautify-description "Compile-time assertions to ensure that invariants are met."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

(define rust-stb_truetype_0_2_6
  (package
    (name "rust-stb_truetype")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stb_truetype" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fk8ar6wn7vnxfcqvg8lhbh47dg544s6kr4bzxa1vs5qbm8dzdv9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder_1_3_2))))
    (home-page "None")
    (synopsis "A straight translation of the font loading code in stb_truetype.h from C to Rust.")
    (description
      (beautify-description "A straight translation of the font loading code in stb_truetype.h from C to Rust."))
    (license `((spdx-string->license "MIT ")
               (spdx-string->license " Apache-2.0")))))

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
    (synopsis "Implementations of string similarity metrics.\nIncludes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler.")
    (description
      (beautify-description "Implementations of string similarity metrics.\nIncludes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro, and Jaro-Winkler."))
    (license (spdx-string->license "MIT"))))

(define rust-syn_0_15_44
  (package
    (name "rust-syn")
    (version "0.15.44")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1id5g6x6zihv3j7hwrw3m1jp636bg8dpi671r7zy3jvpkavb794w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-unicode-xid" ,rust-unicode-xid_0_1_0))))
    (home-page "None")
    (synopsis "Parser for Rust source code")
    (description
      (beautify-description "Parser for Rust source code"))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

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

(define rust-synstructure_0_10_2
  (package
    (name "rust-synstructure")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synstructure" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0grirdkgh2wl4hf9a3nbiazpgccxgq54kn52ms0xrr6njvgkwd82"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-syn" ,rust-syn_0_15_44)        
       ("rust-unicode-xid" ,rust-unicode-xid_0_1_0))))
    (home-page "None")
    (synopsis "Helper methods and macros for custom derives")
    (description
      (beautify-description "Helper methods and macros for custom derives"))
    (license (spdx-string->license "MIT"))))

(define rust-tempfile_3_1_0
  (package
    (name "rust-tempfile")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a9cfdqw70n7bcnkx05aih9xdba8lqazmqlkjpkmn2la6gcj8vks"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if_0_1_9)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-rand" ,rust-rand_0_7_0)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56)        
       ("rust-remove_dir_all" ,rust-remove_dir_all_0_5_2)        
       ("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "http://stebalien.com/projects/tempfile-rs")
    (synopsis "A library for managing temporary files and directories.")
    (description
      (beautify-description "A library for managing temporary files and directories."))
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

(define rust-terminfo_0_6_1
  (package
    (name "rust-terminfo")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "terminfo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17k8vqvicd6yg0iqmkjnxjhz8h8pknv86r03nq3f3ayjmxdhclcf"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-fnv" ,rust-fnv_1_0_6)        
       ("rust-nom" ,rust-nom_4_2_3)        
       ("rust-phf" ,rust-phf_0_7_24)        
       ("rust-phf_codegen" ,rust-phf_codegen_0_7_24))))
    (home-page "None")
    (synopsis "Terminal information.")
    (description
      (beautify-description "Terminal information."))
    (license (spdx-string->license "WTFPL"))))

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
    (synopsis "Textwrap is a small library for word wrapping, indenting, and\ndedenting strings.\n\nYou can use it to format strings (such as help and error messages) for\ndisplay in commandline applications. It is designed to be efficient\nand handle Unicode characters correctly.")
    (description
      (beautify-description "Textwrap is a small library for word wrapping, indenting, and\ndedenting strings.\n\nYou can use it to format strings (such as help and error messages) for\ndisplay in commandline applications. It is designed to be efficient\nand handle Unicode characters correctly."))
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

(define rust-tiff_0_2_2
  (package
    (name "rust-tiff")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tiff" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kn7psgpacns337vvqh272rkqwnakmjd51rc7ygwnc03ibr38j0y"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-byteorder" ,rust-byteorder_1_3_2)        
       ("rust-lzw" ,rust-lzw_0_10_0)        
       ("rust-num-derive" ,rust-num-derive_0_2_5)        
       ("rust-num-traits" ,rust-num-traits_0_2_8))))
    (home-page "None")
    (synopsis "TIFF decoding and encoding library in pure Rust")
    (description
      (beautify-description "TIFF decoding and encoding library in pure Rust"))
    (license (spdx-string->license "MIT"))))

(define rust-time_0_1_42
  (package
    (name "rust-time")
    (version "0.1.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vsbvsz0ryxb35dy9j4anxvy8zlaplmjmi0a4z4l64bc135cz3fv"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-redox_syscall" ,rust-redox_syscall_0_1_56))
      #:cargo-development-inputs
      (("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "https://github.com/rust-lang/time")
    (synopsis "Utilities for working with time-related functions in Rust.")
    (description
      (beautify-description "Utilities for working with time-related functions in Rust."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-ucd-util_0_1_5
  (package
    (name "rust-ucd-util")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-util" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x088q5z0m09a2jqcfgsnq955y8syn1mgn35cl78qinkxm4kp6zs"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "A small utility library for working with the Unicode character database.")
    (description
      (beautify-description "A small utility library for working with the Unicode character database."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-unicase_2_4_0
  (package
    (name "rust-unicase")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicase" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xmpmkakhhblq7dzab1kwyv925kv7fqjkjsxjspg6ix9n88makm8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-version_check" ,rust-version_check_0_1_5))))
    (home-page "None")
    (synopsis "A case-insensitive wrapper around strings.")
    (description
      (beautify-description "A case-insensitive wrapper around strings."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

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
    (synopsis "This crate provides functions for normalization of\nUnicode strings, including Canonical and Compatible\nDecomposition and Recomposition, as described in\nUnicode Standard Annex #15.")
    (description
      (beautify-description "This crate provides functions for normalization of\nUnicode strings, including Canonical and Compatible\nDecomposition and Recomposition, as described in\nUnicode Standard Annex #15."))
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
    (synopsis "Determine displayed width of `char` and `str` types\naccording to Unicode Standard Annex #11 rules.")
    (description
      (beautify-description "Determine displayed width of `char` and `str` types\naccording to Unicode Standard Annex #11 rules."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-unicode-xid_0_1_0
  (package
    (name "rust-unicode-xid")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z57lqh4s18rr4x0j4fw4fmp9hf9346h0kmdgqsqx0fhjr3k0wpw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31.")
    (description
      (beautify-description "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31."))
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
    (synopsis "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31.")
    (description
      (beautify-description "Determine whether characters have the XID_Start\nor XID_Continue properties according to\nUnicode Standard Annex #31."))
    (license (spdx-string->license "MIT OR Apache-2.0"))))

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

(define rust-utf8-ranges_1_0_4
  (package
    (name "rust-utf8-ranges")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "utf8-ranges" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fpc32znar5v02nwsw7icl41jzzzzhy0si6ngqjylzrbxxpi3bml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/utf8-ranges")
    (synopsis "DEPRECATED. Use regex-syntax::utf8 submodule instead.")
    (description
      (beautify-description "DEPRECATED. Use regex-syntax::utf8 submodule instead."))
    (license `((spdx-string->license "Unlicense")
               (spdx-string->license "MIT")))))

(define rust-utf8parse_0_1_1
  (package
    (name "rust-utf8parse")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "utf8parse" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zamsj2986shm4x9zncjf2m5qy9scaw7qnxw4f89b2afpg6a8wl7"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Table-driven UTF-8 parser")
    (description
      (beautify-description "Table-driven UTF-8 parser"))
    (license (spdx-string->license "Apache-2.0 OR MIT"))))

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
    (synopsis "A library to find native dependencies in a vcpkg tree at build\ntime in order to be used in Cargo build scripts.")
    (description
      (beautify-description "A library to find native dependencies in a vcpkg tree at build\ntime in order to be used in Cargo build scripts."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-vec_map_0_8_1
  (package
    (name "rust-vec_map")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vec_map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06n8hw4hlbcz328a3gbpvmy0ma46vg1lc0r5wf55900szf3qdiq5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/contain-rs/vec-map")
    (synopsis "A simple map based on a vector for small integer keys")
    (description
      (beautify-description "A simple map based on a vector for small integer keys"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-version_check_0_1_5
  (package
    (name "rust-version_check")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pf91pvj8n6akh7w6j5ypka6aqz08b3qpzgs0ak2kjf4frkiljwi"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Tiny crate to check the version of the installed/running rustc.")
    (description
      (beautify-description "Tiny crate to check the version of the installed/running rustc."))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-void_1_0_2
  (package
    (name "rust-void")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "void" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zc8f0ksxvmhvgx4fdg0zyn6vdnbxd2xv9hfx4nhzg6kbs4f80ka"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "The uninhabited void type for use in statically impossible cases.")
    (description
      (beautify-description "The uninhabited void type for use in statically impossible cases."))
    (license (spdx-string->license "MIT"))))

(define rust-vswhom_0_1_0
  (package
    (name "rust-vswhom")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vswhom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12v0fjjzxdc3y5c0lcwycfhphz7zf2s06hl5krwhawah0xzrp5xy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-vswhom-sys" ,rust-vswhom-sys_0_1_0))))
    (home-page "None")
    (synopsis "FFI to Jon Blow\u0027s VS discovery script")
    (description
      (beautify-description "FFI to Jon Blow\u0027s VS discovery script"))
    (license (spdx-string->license "MIT"))))

(define rust-vswhom-sys_0_1_0
  (package
    (name "rust-vswhom-sys")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vswhom-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0clm4dx4amwlhg5lkh52fmvvwq6c7s7b9xqljw39mryhsc158bzw"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc_1_0_41)        
       ("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "Pure FFI to Jon Blow\u0027s VS discovery script")
    (description
      (beautify-description "Pure FFI to Jon Blow\u0027s VS discovery script"))
    (license (spdx-string->license "MIT"))))

(define rust-vte_0_3_3
  (package
    (name "rust-vte")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vte" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kz8svnqnxclllsgh0ck20rplw3qzp46b5v30yscnzrgw8vgahjg"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-utf8parse" ,rust-utf8parse_0_1_1))))
    (home-page "None")
    (synopsis "Parser for implementing terminal emulators")
    (description
      (beautify-description "Parser for implementing terminal emulators"))
    (license (spdx-string->license "Apache-2.0 OR MIT"))))

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

(define rust-wasi_0_5_0
  (package
    (name "rust-wasi")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ir3pd4phdfml0cbziw9bqp7mnk0vfp9biy8bh25lln6raml4m7x"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "Experimental WASI API bindings for Rust")
    (description
      (beautify-description "Experimental WASI API bindings for Rust"))
    (license (spdx-string->license "Apache-2.0 WITH LLVM-exception"))))

(define rust-wayland-client_0_21_13
  (package
    (name "rust-wayland-client")
    (version "0.21.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-client" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04r7dy074hhdalsi1day482wvmczr40hg7qvrnzkgxpakrgkx5j9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-downcast-rs" ,rust-downcast-rs_1_0_4)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-nix" ,rust-nix_0_14_1)        
       ("rust-wayland-commons" ,rust-wayland-commons_0_21_13)        
       ("rust-wayland-scanner" ,rust-wayland-scanner_0_21_13)        
       ("rust-wayland-sys" ,rust-wayland-sys_0_21_13))))
    (home-page "None")
    (synopsis "Bindings to the standard C implementation of the wayland protocol, client side.")
    (description
      (beautify-description "Bindings to the standard C implementation of the wayland protocol, client side."))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-client_0_23_5
  (package
    (name "rust-wayland-client")
    (version "0.23.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-client" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q04lmvm4rpwnjy9bwzcmh9jlh1ry08y28glhqk7gp8ncsx0knc0"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-downcast-rs" ,rust-downcast-rs_1_0_4)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-nix" ,rust-nix_0_14_1)        
       ("rust-wayland-commons" ,rust-wayland-commons_0_23_5)        
       ("rust-wayland-scanner" ,rust-wayland-scanner_0_23_5)        
       ("rust-wayland-sys" ,rust-wayland-sys_0_23_5))))
    (home-page "None")
    (synopsis "Bindings to the standard C implementation of the wayland protocol, client side.")
    (description
      (beautify-description "Bindings to the standard C implementation of the wayland protocol, client side."))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-commons_0_21_13
  (package
    (name "rust-wayland-commons")
    (version "0.21.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-commons" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v1jpcsnn6cwwy5ii5pdl58i6b9slmi8mn4my4fpwrlbfsb8ih20"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nix" ,rust-nix_0_14_1)        
       ("rust-wayland-sys" ,rust-wayland-sys_0_21_13))))
    (home-page "None")
    (synopsis "Common types and structures used by wayland-client and wayland-server.")
    (description
      (beautify-description "Common types and structures used by wayland-client and wayland-server."))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-commons_0_23_5
  (package
    (name "rust-wayland-commons")
    (version "0.23.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-commons" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n21m99041k7374ca2wb8p18kv4v64s4j4r1x58q8pyvjnwn6163"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-nix" ,rust-nix_0_14_1)        
       ("rust-wayland-sys" ,rust-wayland-sys_0_23_5))))
    (home-page "None")
    (synopsis "Common types and structures used by wayland-client and wayland-server.")
    (description
      (beautify-description "Common types and structures used by wayland-client and wayland-server."))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-protocols_0_21_13
  (package
    (name "rust-wayland-protocols")
    (version "0.21.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-protocols" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i91yh3nxk9llhly2ly3nvlfx0lbpvyq919cgmnyx3j25bmf5zaa"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-wayland-client" ,rust-wayland-client_0_21_13)        
       ("rust-wayland-commons" ,rust-wayland-commons_0_21_13)        
       ("rust-wayland-scanner" ,rust-wayland-scanner_0_21_13)        
       ("rust-wayland-sys" ,rust-wayland-sys_0_21_13))))
    (home-page "None")
    (synopsis "Generated API for the officials wayland protocol extensions")
    (description
      (beautify-description "Generated API for the officials wayland protocol extensions"))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-protocols_0_23_5
  (package
    (name "rust-wayland-protocols")
    (version "0.23.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-protocols" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rynmxa107fiz4g3y4nq8hdicwpcqja3z79xcxhd1dkb8qnz39fc"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-wayland-client" ,rust-wayland-client_0_23_5)        
       ("rust-wayland-commons" ,rust-wayland-commons_0_23_5)        
       ("rust-wayland-scanner" ,rust-wayland-scanner_0_23_5))))
    (home-page "None")
    (synopsis "Generated API for the officials wayland protocol extensions")
    (description
      (beautify-description "Generated API for the officials wayland protocol extensions"))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-scanner_0_21_13
  (package
    (name "rust-wayland-scanner")
    (version "0.21.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-scanner" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17mp49v7w0p0x5ry628lj2llljnwkr9aj9g4bqqhfibid32jhf5z"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-xml-rs" ,rust-xml-rs_0_8_0))))
    (home-page "None")
    (synopsis "Wayland Scanner for generating rust APIs from XML wayland protocol files. Intented for use with wayland-sys. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings.")
    (description
      (beautify-description "Wayland Scanner for generating rust APIs from XML wayland protocol files. Intented for use with wayland-sys. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings."))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-scanner_0_23_5
  (package
    (name "rust-wayland-scanner")
    (version "0.23.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-scanner" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qnfiycki3hypi2r7lfjniqy6fch5r9y42aa7fmp07hf0ncc8aw4"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-proc-macro2" ,rust-proc-macro2_0_4_30)        
       ("rust-quote" ,rust-quote_0_6_13)        
       ("rust-xml-rs" ,rust-xml-rs_0_8_0))))
    (home-page "None")
    (synopsis "Wayland Scanner for generating rust APIs from XML wayland protocol files. Intented for use with wayland-sys. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings.")
    (description
      (beautify-description "Wayland Scanner for generating rust APIs from XML wayland protocol files. Intented for use with wayland-sys. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings."))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-sys_0_21_13
  (package
    (name "rust-wayland-sys")
    (version "0.21.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a0ndgkg98pvmkv44yya4f7mxzjaxylknqh64bpa05w0azyv02jj"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-dlib" ,rust-dlib_0_4_1)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0))))
    (home-page "None")
    (synopsis "FFI bindings to the various libwayland-*.so libraries. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings.")
    (description
      (beautify-description "FFI bindings to the various libwayland-*.so libraries. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings."))
    (license (spdx-string->license "MIT"))))

(define rust-wayland-sys_0_23_5
  (package
    (name "rust-wayland-sys")
    (version "0.23.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wayland-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0541h70wvr2mnl5ppjyba7cwvy6xff85qq8z9mif35i7qvm6lrxp"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-dlib" ,rust-dlib_0_4_1)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0))))
    (home-page "None")
    (synopsis "FFI bindings to the various libwayland-*.so libraries. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings.")
    (description
      (beautify-description "FFI bindings to the various libwayland-*.so libraries. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings."))
    (license (spdx-string->license "MIT"))))

(define rust-which_1_0_5
  (package
    (name "rust-which")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cjwa57kzfgzs681a27m5pjmq580pv3hkcg23smf270bgqz60jp8"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62))))
    (home-page "None")
    (synopsis "A Rust equivalent of Unix command \"which\". Locate installed execuable in cross platforms.")
    (description
      (beautify-description "A Rust equivalent of Unix command \"which\". Locate installed execuable in cross platforms."))
    (license (spdx-string->license "MIT"))))

(define rust-widestring_0_4_0
  (package
    (name "rust-widestring")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "widestring" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dhx6dndjsz1y7c9w06922412kdxyrrkqblvggm76mh8z17hxz7g"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "A wide string FFI library for converting to and from wide strings, such as those often used in Windows API or other FFI libaries. Both UTF-16 and UTF-32 types are provided.")
    (description
      (beautify-description "A wide string FFI library for converting to and from wide strings, such as those often used in Windows API or other FFI libaries. Both UTF-16 and UTF-32 types are provided."))
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
    (synopsis "Import libraries for the i686-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the i686-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead."))
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
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead.")
    (description
      (beautify-description "Import libraries for the x86_64-pc-windows-gnu target. Please don\u0027t use this crate directly, depend on winapi instead."))
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

(define rust-winit_0_19_3
  (package
    (name "rust-winit")
    (version "0.19.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pwzniwm2nn68w4ppixn4pizbnh93vm8nvyaqmajvr8swq2sj3bx"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-android_glue" ,rust-android_glue_0_2_3)        
       ("rust-backtrace" ,rust-backtrace_0_3_35)        
       ("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-cocoa" ,rust-cocoa_0_18_4)        
       ("rust-core-foundation" ,rust-core-foundation_0_6_4)        
       ("rust-core-graphics" ,rust-core-graphics_0_17_3)        
       ("rust-image" ,rust-image_0_21_3)        
       ("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-log" ,rust-log_0_4_8)        
       ("rust-objc" ,rust-objc_0_2_6)        
       ("rust-parking_lot" ,rust-parking_lot_0_9_0)        
       ("rust-percent-encoding" ,rust-percent-encoding_2_1_0)        
       ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit_0_4_6)        
       ("rust-wayland-client" ,rust-wayland-client_0_21_13)        
       ("rust-winapi" ,rust-winapi_0_3_8)        
       ("rust-x11-dl" ,rust-x11-dl_2_18_4))))
    (home-page "None")
    (synopsis "Cross-platform window creation library.")
    (description
      (beautify-description "Cross-platform window creation library."))
    (license (spdx-string->license "Apache-2.0"))))

(define rust-winpty_0_1_0
  (package
    (name "rust-winpty")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jwilm/alacritty")
               (commit "2f93fb34b1b237bea4d1bea67a67c7c8efe6e5dc")))
        (file-name (git-file-name name version))
        (sha256
          (base32 
            "1kf5zkcadg6vmqz4fmxyj7nhybsz4kdkf5x5dwha8fkqha237a4i"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags_1_1_0)        
       ("rust-embed-resource" ,rust-embed-resource_1_3_0)        
       ("rust-http_req" ,rust-http_req_0_5_3)        
       ("rust-named_pipe" ,rust-named_pipe_0_3_0)        
       ("rust-tempfile" ,rust-tempfile_3_1_0)        
       ("rust-widestring" ,rust-widestring_0_4_0)        
       ("rust-winapi" ,rust-winapi_0_3_8)        
       ("rust-winpty-sys" ,rust-winpty-sys_0_4_3)        
       ("rust-zip" ,rust-zip_0_5_3))))
    (home-page "FILLMEIN")
    (synopsis "Safe rust bindings for winpty")
    (description
      (beautify-description "Safe rust bindings for winpty"))
    (license (spdx-string->license "MIT"))))

(define rust-winpty-sys_0_4_3
  (package
    (name "rust-winpty-sys")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winpty-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s5m2vvlw7wphc466s47zfmp08zk00wzj999l1w3ajqlxbnfgb9x"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bindgen" ,rust-bindgen_0_33_2)        
       ("rust-cc" ,rust-cc_1_0_41))))
    (home-page "None")
    (synopsis "Rust winpty bindings")
    (description
      (beautify-description "Rust winpty bindings"))
    (license (spdx-string->license "MIT"))))

(define rust-winreg_0_5_1
  (package
    (name "rust-winreg")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winreg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jkh4jj2g8g0bl7r1xvq9vv9hr4gdzphg9ndqm65q6f1jn9paym2"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_3_8))))
    (home-page "None")
    (synopsis "Rust bindings to MS Windows Registry API")
    (description
      (beautify-description "Rust bindings to MS Windows Registry API"))
    (license (spdx-string->license "MIT"))))

(define rust-ws2_32-sys_0_2_1
  (package
    (name "rust-ws2_32-sys")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ws2_32-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-winapi" ,rust-winapi_0_2_8)        
       ("rust-winapi-build" ,rust-winapi-build_0_1_1))))
    (home-page "None")
    (synopsis "Contains function definitions for the Windows API library ws2_32. See winapi for types and constants.")
    (description
      (beautify-description "Contains function definitions for the Windows API library ws2_32. See winapi for types and constants."))
    (license (spdx-string->license "MIT"))))

(define rust-x11-clipboard_0_3_3
  (package
    (name "rust-x11-clipboard")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "x11-clipboard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1smwyr23jns0dncm6bwv00xfxxy99bv6qlx6df7dkdcydk04kgc9"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-xcb" ,rust-xcb_0_8_2))))
    (home-page "None")
    (synopsis "x11 clipboard support for Rust.")
    (description
      (beautify-description "x11 clipboard support for Rust."))
    (license (spdx-string->license "MIT"))))

(define rust-x11-dl_2_18_4
  (package
    (name "rust-x11-dl")
    (version "2.18.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "x11-dl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n1w837xagxqgwx2880d7c9ks6l3g1kk00yd75afdaiv58sf2rdy"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-lazy_static" ,rust-lazy_static_1_4_0)        
       ("rust-libc" ,rust-libc_0_2_62)        
       ("rust-maybe-uninit" ,rust-maybe-uninit_2_0_0)        
       ("rust-pkg-config" ,rust-pkg-config_0_3_15))))
    (home-page "None")
    (synopsis "X11 library bindings for Rust")
    (description
      (beautify-description "X11 library bindings for Rust"))
    (license (spdx-string->license "CC0-1.0"))))

(define rust-xcb_0_8_2
  (package
    (name "rust-xcb")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xcb" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ph27r9nxyfy3hh1c7x85g6dciwxcinf6514pvw9ybhl4hzpm4ay"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-libc" ,rust-libc_0_2_62)        
       ("rust-log" ,rust-log_0_4_8))))
    (home-page "None")
    (synopsis "Rust bindings and wrappers for XCB")
    (description
      (beautify-description "Rust bindings and wrappers for XCB"))
    (license (spdx-string->license "MIT"))))

(define rust-xdg_2_2_0
  (package
    (name "rust-xdg")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xdg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mws8a0fr3cqk5nh7aq9lmkmhzghvasqy4mhw6nnza06l4d6i2fh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/whitequark/rust-xdg")
    (synopsis "A library for storing and retrieving files according to XDG Base Directory specification")
    (description
      (beautify-description "A library for storing and retrieving files according to XDG Base Directory specification"))
    (license `((spdx-string->license "Apache-2.0")
               (spdx-string->license "MIT")))))

(define rust-xml-rs_0_8_0
  (package
    (name "rust-xml-rs")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xml-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1db4v716rbpgjiasaim2s17rmvsfcq1qzwg6nji6mdf5k34i46sl"))))
    (build-system cargo-build-system)
    (home-page "None")
    (synopsis "An XML library in pure Rust")
    (description
      (beautify-description "An XML library in pure Rust"))
    (license (spdx-string->license "MIT"))))

(define rust-yaml-rust_0_4_3
  (package
    (name "rust-yaml-rust")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "yaml-rust" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ka3qhqc5lvk3hz14wmsj32jhmh44blcbfrx5hfxli2gg38kv4k5"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-linked-hash-map" ,rust-linked-hash-map_0_5_2))))
    (home-page "http://chyh1990.github.io/yaml-rust/")
    (synopsis "The missing YAML 1.2 parser for rust")
    (description
      (beautify-description "The missing YAML 1.2 parser for rust"))
    (license `((spdx-string->license "MIT")
               (spdx-string->license "Apache-2.0")))))

(define rust-zip_0_5_3
  (package
    (name "rust-zip")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zip" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ilzgvacszn9n9in5q67983m2bxnmnivvxa7l0ixhazs190vn89w"))))
    (build-system cargo-build-system)
    (arguments
    `(#:cargo-inputs
      (("rust-bzip2" ,rust-bzip2_0_3_3)        
       ("rust-crc32fast" ,rust-crc32fast_1_2_0)        
       ("rust-flate2" ,rust-flate2_1_0_11)        
       ("rust-podio" ,rust-podio_0_1_6)        
       ("rust-time" ,rust-time_0_1_42))))
    (home-page "None")
    (synopsis "Library to support the reading and writing of zip files.")
    (description
      (beautify-description "Library to support the reading and writing of zip files."))
    (license (spdx-string->license "MIT"))))
