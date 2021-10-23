(define-module (vup tmp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages video)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages base)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages image)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages animation)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module ((guix licenses) #:prefix license:))

;; TODO(robin): this is hell, I should make a list with all of the fixed crates and make fix-rust-crate apply it recursively
(define* (fix-rust-crate crate)
  (let* ((source (package-source crate)))
    (package
      (inherit crate)
      (name (string-append (package-name crate) "-fixed"))
      (source
       (origin
         (inherit source)
         (file-name (string-append (package-name crate) "-" (package-version crate) ".tar.gz")))))))


(define rust-lazy-static-1-fixed
  ((package-input-rewriting/spec
    `(("rust-spin" . ,(const (fix-rust-crate rust-spin-0.5)))))
   rust-lazy-static-1))

(define rust-proc-macro2-1-fixed
  ((package-input-rewriting/spec
    `(("rust-unicode-xid" . ,(const (fix-rust-crate rust-unicode-xid-0.2)))))
   rust-proc-macro2-1))

(define rust-quote-1-fixed
  ((package-input-rewriting/spec
     `(("rust-proc-marco2" . ,(const (fix-rust-crate rust-proc-macro2-1-fixed)))))
   rust-quote-1))

(define rust-wayland-scanner-0.28-fixed
  ((package-input-rewriting/spec
    `(("rust-proc-macro2" . ,(const (fix-rust-crate rust-proc-macro2-1-fixed)))
      ("rust-lazy-static" . ,(const (fix-rust-crate rust-lazy-static-1-fixed)))
      ("rust-quote" . ,(const (fix-rust-crate rust-quote-1-fixed)))
      ("rust-unicode-xid" . ,(const (fix-rust-crate rust-unicode-xid-0.2)))))
   rust-wayland-scanner-0.28))

(define rust-wayland-client-0.28-fixed
  ((package-input-rewriting/spec
    `(("rust-wayland-scanner" . ,(const (fix-rust-crate rust-wayland-scanner-0.28-fixed)))
      ("rust-spin" . ,(const (fix-rust-crate rust-spin-0.5)))
      ("rust-lazy-static" . ,(const (fix-rust-crate rust-lazy-static-1-fixed)))
      ("rust-proc-marco2" . ,(const (fix-rust-crate rust-proc-macro2-1-fixed)))
      ("rust-bitflags" . ,(const (fix-rust-crate rust-bitflags-1)))
      ("rust-scoped-tls" . ,(const (fix-rust-crate rust-scoped-tls-1)))
      ("rust-cc" . ,(const (fix-rust-crate rust-cc-1)))
      ("rust-cfg-if" . ,(const (fix-rust-crate rust-cfg-if-0.1)))
      ("rust-pkg-config" . ,(const (fix-rust-crate rust-pkg-config-0.3)))
      ("rust-winapi" . ,(const (fix-rust-crate rust-winapi-0.3))))
     #:deep? #t)
   rust-wayland-client-0.28))

(define-public alacritty-fixed
  ((package-input-rewriting/spec
    `(("rust-bitflags" . ,(const (fix-rust-crate rust-bitflags-1)))
      ("rust-winapi" . ,(const (fix-rust-crate rust-winapi-0.3)))
      ("rust-cc" . ,(const (fix-rust-crate rust-cc-1)))
      ("rust-scoped-tls" . ,(const (fix-rust-crate rust-scoped-tls-1)))
      ("rust-cfg-if" . ,(const (fix-rust-crate rust-cfg-if-0.1)))
      ("rust-pkg-config" . ,(const (fix-rust-crate rust-pkg-config-0.3)))
      ("rust-quote" . ,(const (fix-rust-crate rust-quote-1-fixed)))
      ("rust-spin" . ,(const (fix-rust-crate rust-spin-0.5)))
      ("rust-unicode-xid" . ,(const (fix-rust-crate rust-unicode-xid-0.2)))
      ("rust-lazy-static" . ,(const (fix-rust-crate rust-lazy-static-1-fixed)))
      ("rust-proc-marco2" . ,(const (fix-rust-crate rust-proc-macro2-1-fixed)))
      ("rust-wayland-client" . ,(const (fix-rust-crate rust-wayland-client-0.28-fixed)))))
   alacritty))

(define-public webrtc-for-telegram-desktop
  (let ((commit "a19877363082da634a3c851a4698376504d2eaee")
        (revision "83"))
    (hidden-package
     (package
       (name "webrtc-for-telegram-desktop")
       (version
        (git-version "0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/tg_owt.git")
            (commit commit)))
          (file-name
           (git-file-name name version))
          (sha256
           (base32 "0961zm1m1mc2kh54dx5ax95q8sw13impvpjvg9jv12bmfkgm17wr"))
          (modules '((guix build utils)
                     (ice-9 ftw)
                     (srfi srfi-1)))
          (snippet
           `(begin
              (let ((keep
                     '( ;; Custom forks which are incompatible with the ones in Guix.
                       "abseil-cpp" "libsrtp" "openh264" "rnnoise"
                       ;; Not available in Guix.
                       "pffft" "usrsctp"
                       ;; Has cmake support files for libvpx input.
                       "libvpx")))
                (with-directory-excursion "src/third_party"
                  (for-each delete-file-recursively
                            (lset-difference string=?
                                             (scandir ".")
                                             (cons* "." ".." keep))))
                #t)))))
       (build-system cmake-build-system)
       (arguments
        `(#:tests? #f                   ; No target
          #:configure-flags
          (list
           "-DCMAKE_C_FLAGS=-fPIC"
           "-DCMAKE_CXX_FLAGS=-fPIC")
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'copy-inputs
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((libvpx-from (assoc-ref inputs "libvpx"))
                       (libyuv-from (assoc-ref inputs "libyuv"))
                       (libvpx-to (string-append (getcwd)
                                                 "/src/third_party/libvpx/source/libvpx"))
                       (libyuv-to (string-append (getcwd)
                                                 "/src/third_party/libyuv")))
                  (copy-recursively libvpx-from libvpx-to)
                  (copy-recursively libyuv-from libyuv-to))
                #t)))))
       (native-inputs
        `(("gcc" ,gcc-9) ; keep in line with telegram-desktop
          ("perl" ,perl)
          ("pkg-config" ,pkg-config)
          ("python" ,python-wrapper)
          ("yasm" ,yasm)))
       (inputs
        `(("alsa" ,alsa-lib)
          ("ffmpeg" ,ffmpeg)
          ("libjpeg" ,libjpeg-turbo)
          ("libvpx"
           ,(origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://chromium.googlesource.com/webm/libvpx")
                (commit "5b63f0f821e94f8072eb483014cfc33b05978bb9")))
              (file-name
               (git-file-name "libvpx-for-webrtc-for-telegram-desktop" version))
              (sha256
               (base32 "1psvxaddihlw1k5n0anxif3qli6zyw2sa2ywn6mkb8six9myrp68"))))
          ("libyuv"
           ,(origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://chromium.googlesource.com/libyuv/libyuv")
                (commit "ad890067f661dc747a975bc55ba3767fe30d4452")))
              (file-name
               (git-file-name "libyuv-for-webrtc-for-telegram-desktop" version))
              (sha256
               (base32 "01knnk4h247rq536097n9n3s3brxlbby3nv3ppdgsqfda3k159ll"))))
          ("openssl" ,openssl)
          ("opus" ,opus)
          ("protobuf" ,protobuf)
          ("pulseaudio" ,pulseaudio)
          ("x11" ,libx11)
          ("xext" ,libxext)
          ("xtst" ,libxtst)))
       (synopsis "WebRTC support for Telegram Desktop")
       (description "WebRTC-for-Telegram-Desktop is a custom WebRTC fork by
Telegram project, for its use in telegram desktop client.")
       (home-page "https://github.com/desktop-app/tg_owt")
       (license
        (list
         ;; Abseil-CPP
         license:asl2.0
         ;; LibYuv
         (license:non-copyleft "file:///src/third_party/libyuv/LICENSE")
         ;; OpenH264
         license:bsd-2
         ;; PFFFT
         (license:non-copyleft "file:///src/third_party/pffft/LICENSE")
         ;; RnNoise
         license:gpl3
         ;; LibSRTP, LibVPx, UsrSCTP and Others
         license:bsd-3))))))


(define-public libtgvoip-for-telegram-desktop
  (let ((commit "13a5fcb16b04472d808ce122abd695dbf5d206cd")
        (revision "88"))
    (hidden-package
     (package
       (inherit libtgvoip)
       (version
        (git-version "2.4.4" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/telegramdesktop/libtgvoip.git")
            (commit commit)))
          (file-name
           (git-file-name "libtgvoip-for-telegram-desktop" version))
          (sha256
           (base32 "12p6s7vxkf1gh1spdckkdxrx7bjzw881ds9bky7l5fw751cwb3xd"))))
       (arguments
        `(#:configure-flags
          (list
           "--disable-static"
           "--disable-dsp"              ; FIXME
           "--enable-audio-callback"
           "--with-alsa"
           "--with-pulse")
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'patch-linkers
              (lambda _
                (substitute* "Makefile.am"
                  (("\\$\\(CRYPTO_LIBS\\) \\$\\(OPUS_LIBS\\)")
                   "$(CRYPTO_LIBS) $(OPUS_LIBS) $(ALSA_LIBS) $(PULSE_LIBS)"))
                (substitute* "tgvoip.pc.in"
                  (("libcrypto opus")
                   "libcrypto opus alsa libpulse"))
                #t)))))
       (native-inputs
        `(("autoconf" ,autoconf)
          ("automake" ,automake)
          ("libtool" ,libtool)
          ("pkg-config" ,pkg-config)))))))

(define-public rlottie-for-telegram-desktop
  (let ((commit "cbd43984ebdf783e94c8303c41385bf82aa36d5b")
        (revision "671"))
    (hidden-package
     (package
       (inherit rlottie)
       (version
        (git-version "0.0.1" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/rlottie.git")
            (commit commit)))
          (file-name
           (git-file-name "rlottie-for-telegram-desktop" version))
          (sha256
           (base32 "1lxpbgbhps9rmck036mgmiknqrzpjxpas8n7qxykv6pwzn0c8n0c"))))
       (arguments
        `(#:configure-flags
          (list
           "-Dlog=true"
           "-Ddumptree=true"
           "-Dtest=true")
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'patch-cxx-flags
              (lambda _
                (substitute* "meson.build"
                  (("werror=true")
                   "werror=false"))
                #t)))))))))

(define-public telegram-desktop
  (package
    (name "telegram-desktop")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/telegramdesktop/tdesktop.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1lqs06scqvzg37a2py8jk7nnlvk42jjifcpnhdd5rgd5biw70nyx"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        `(begin
           (let ((keep
                  '( ;; Not available in Guix.
                    "SPMediaKeyTap" "statusnotifieritem" "tgcalls")))
             (with-directory-excursion "Telegram/ThirdParty"
               (for-each delete-file-recursively
                         (lset-difference string=?
                                          (scandir ".")
                                          (cons* "." ".." keep))))
             #t)))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:imported-modules
       (,@%qt-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build qt-build-system)
        ((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build utils)
        (ice-9 match))
       #:configure-flags
       (list
        ;; Client applications must provide their own API-ID and API-HASH,
        ;; see also <https://core.telegram.org/api/obtaining_api_id>.
        ;; In case, that the credentials below fail to work, contact
        ;;   Raghav Gururajan <rg@raghavgururajan.name>
        "-DTDESKTOP_API_ID=2791056"
        "-DTDESKTOP_API_HASH=582d6d0b44f7a2de949e99271fd8b3f2"
        ;; Use bundled fonts as fallback.
        "-DDESKTOP_APP_USE_PACKAGED_FONTS=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'make-writable 'copy-inputs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each
              (match-lambda
                ((dst src)
                 (copy-recursively src dst)
                 (for-each make-file-writable (find-files dst))))
              `(("cmake" ,(assoc-ref inputs "cmake-helpers"))
                ("Telegram/codegen" ,(assoc-ref inputs "codegen-source"))
                ("Telegram/lib_base" ,(assoc-ref inputs "lib-base-source"))
                ("Telegram/lib_crl" ,(assoc-ref inputs "lib-crl-source"))
                ("Telegram/lib_lottie"
                 ,(assoc-ref inputs "lib-lottie-source"))
                ("Telegram/lib_qr" ,(assoc-ref inputs "lib-qr-source"))
                ("Telegram/lib_rlottie"
                 ,(assoc-ref inputs "lib-rlottie-source"))
                ("Telegram/lib_rpl" ,(assoc-ref inputs "lib-rpl-source"))
                ("Telegram/lib_spellcheck"
                 ,(assoc-ref inputs "lib-spellcheck-source"))
                ("Telegram/lib_storage"
                 ,(assoc-ref inputs "lib-storage-source"))
                ("Telegram/lib_tl" ,(assoc-ref inputs "lib-tl-source"))
                ("Telegram/lib_ui" ,(assoc-ref inputs "lib-ui-source"))
                ("Telegram/lib_webrtc" ,(assoc-ref inputs "lib-webrtc-source"))
                ("Telegram/ThirdParty/tgcalls"
                 ,(assoc-ref inputs "tgcalls-source"))))
             #t))
         (add-before 'configure 'patch-cxx-flags
           (lambda _
             (substitute* "cmake/options_linux.cmake"
               (("class-memaccess") "all"))
             #t))
         (add-after 'qt-wrap 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("cmake-helpers"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/cmake_helpers.git")
             (commit "695fabda6830b58bdc02d09db70531d5dececcd0")))
           (file-name
            (git-file-name "cmake-helpers-for-telegram-desktop" version))
           (sha256
            (base32 "1j3ppgfmihcjl22w5jk8jhwif10i9wbycq5zqnssn6pnhnj7di5i"))))
       ("cmake-shared" ,cmake-shared)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("gcc" ,gcc-9)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("qttools" ,qttools)))
    (inputs
     `(("alsa" ,alsa-lib)
       ("c++-gsl" ,c++-gsl)
       ("catch" ,catch-framework2)
       ("codegen-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/codegen.git")
             (commit "127968de8129e8ccfa6ac50721c70415a5a087c3")))
           (file-name
            (git-file-name "codegen" version))
           (sha256
            (base32 "036hzjrsk134ky62192nra43rsln5kh5gz20q1920s922661zky2"))))
       ("expected" ,libexpected)
       ("fcitx-qt5" ,fcitx-qt5)
       ("fcitx5-qt" ,fcitx5-qt)
       ("ffmpeg" ,ffmpeg)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("hime" ,hime)
       ("hunspell" ,hunspell)
       ("iconv" ,libiconv)
       ("kwayland" ,kwayland)
       ("lib-base-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_base.git")
             (commit "f1e4168081428fa451d2f50eee7b1c448268c43a")))
           (file-name
            (git-file-name "lib-base-for-telegram-desktop" version))
           (sha256
            (base32 "0piqp7llwi7sfy4c15g0p8ihr90rz1qps6q5fkl1iasrf5ysw8qc"))))
       ("lib-crl-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_crl.git")
             (commit "16150bf71d79382738114b913f137ec1a1a7630c")))
           (file-name
            (git-file-name "lib-crl-for-telegram-desktop" version))
           (sha256
            (base32 "0qhagdr26aqb9w7wnchcmk1j7ln28x3wbkkkm06b8h0mybksbj7q"))))
       ("lib-lottie-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_lottie.git")
             (commit "fb40f379d82ffa1fc7506e9a8dddcf48847715ae")))
           (file-name
            (git-file-name "lib-lottie-for-telegram-desktop" version))
           (sha256
            (base32 "1vq0mqxcrrv7akcqk9cl4mm61zw6dcfmy8adl0pcp49kynm64saw"))))
       ("lib-qr-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_qr.git")
             (commit "92ce41a690a463eb462089a4eb1e51e019308018")))
           (file-name
            (git-file-name "lib-qr-for-telegram-desktop" version))
           (sha256
            (base32 "182939nv7xs9b3bgah3gl5y9hx5r59mabd2jw3z6717vc96qi2pj"))))
       ("lib-rlottie-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_rlottie.git")
             (commit "0671bf70547381effcf442ec9618e04502a8adbc")))
           (file-name
            (git-file-name "lib-rlottie-for-telegram-desktop" version))
           (sha256
            (base32 "05qnza7j15356s8jq16pkbyp4zr586lssmd86lz5jq23lcb3raxv"))))
       ("lib-rpl-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_rpl.git")
             (commit "e1b96399d9031c4ef0354631e6bb375029d29d9f")))
           (file-name
            (git-file-name "lib-rpl-for-telegram-desktop" version))
           (sha256
            (base32 "1wvqazljd2kq1fxlj250jhjrig529499bym9p81dx33kh1l9dgss"))))
       ("lib-spellcheck-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_spellcheck.git")
             (commit "1b540b38ed78e9a3cba93e9ba4ce4525ab692277")))
           (file-name
            (git-file-name "lib-spellcheck-for-telegram-desktop" version))
           (sha256
            (base32 "0a7042h5zrdvgs7v153ral2dh1zj84di5yjcmgcry5k4s1im9di7"))))
       ("lib-storage-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_storage.git")
             (commit "cbe51722b73cfa9ff27bd59294b08aa5ee33c936")))
           (file-name
            (git-file-name "lib-storage-for-telegram-desktop" version))
           (sha256
            (base32 "045l5xsyagyz17gbhmmvl2miss4nb92p0dmza7yfs9pkg9gs0f87"))))
       ("lib-tl-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_tl.git")
             (commit "404c83d77e5edb8a39f8e9f56a6340960fe5070e")))
           (file-name
            (git-file-name "lib-tl-for-telegram-desktop" version))
           (sha256
            (base32 "1k34nkvvcjqw5q81n1qmklid60cvzjk4lmn9qjimk437m6wbii7f"))))
       ("lib-ui-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_ui.git")
             (commit "e14bc4681d69c1b538b8c5af51501077ae5a8a86")))
           (file-name
            (git-file-name "lib-ui-for-telegram-desktop" version))
           (sha256
            (base32 "04b1x4bswk3bxqrwpv5g7w4frkprrwf0px6aibh6z4drinv08wsv"))))
       ("lib-webrtc-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_webrtc.git")
             (commit "60d5c43daf882a6c03944a3e6198b5f35b654a0e")))
           (file-name
            (git-file-name "lib-webrtc-for-telegram-desktop" version))
           (sha256
            (base32 "0mxmbw8i37axllg9h976p6np2gcfyci6xwwl9hc9mhs49vwwsw5s"))))
       ("libdbusmenu-qt" ,libdbusmenu-qt)
       ("libjpeg" ,libjpeg-turbo)
       ("libtgvoip" ,libtgvoip-for-telegram-desktop)
       ("lz4" ,lz4)
       ("materialdecoration" ,materialdecoration)
       ("minizip" ,minizip)
       ("nimf" ,nimf)
       ("openal" ,openal)
       ("openssl" ,openssl)
       ("opus" ,opus)
       ("pulseaudio" ,pulseaudio)
       ("qrcodegen" ,qrcodegen-cpp)
       ("qt" ,qtbase-5)
       ("qt5ct" ,qt5ct)
       ("qtimageformats" ,qtimageformats)
       ("qtwayland" ,qtwayland)
       ("range-v3" ,range-v3)
       ("rlottie" ,rlottie-for-telegram-desktop)
       ("tgcalls-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/TelegramMessenger/tgcalls.git")
             (commit "71addf5b41cb6bb6844f75e977edae0020938930")))
           (file-name
            (git-file-name "tgcalls-for-telegram-desktop" version))
           (sha256
            (base32 "1zrjxf03n3ad8b95gwjarmq4gj5i5cwhlg93qcjv2232kksh29iy"))))
       ("webrtc" ,webrtc-for-telegram-desktop)
       ("x11" ,libx11)
       ("xcb" ,libxcb)
       ("xcb-keysyms" ,xcb-util-keysyms)
       ("xxhash" ,xxhash)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (synopsis "Telegram Desktop")
    (description "Telegram desktop is the official desktop version of the
Telegram instant messenger.")
    (home-page "https://desktop.telegram.org/")
    (license
     (list
      ;; ThirdParty
      license:lgpl2.1+
      ;; Others
      license:gpl3+))))

alacritty-fixed
