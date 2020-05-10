(define-module (vup telegram)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages check)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages python)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages qt))

(define-public tl-expected
  (let ((commit "1d9c5d8c0da84b8ddc54bd3d90d632eec95c1f13"))
    (package
      (name "tl-expected")
      (version (string-append "2019.11.11-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/TartanLlama/expected")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0rzfn9yyg70zwpxbmv22qy0015baymi2rdd65ixmcb31fgnap68i"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags (list
                            "-DEXPECTED_BUILD_TESTS=OFF")))
      (synopsis "C++11/14/17 std::expected with functional-style extensions")
      (description "C++11/14/17 std::expected with functional-style extensions")
      (home-page "https://tl.tartanllama.xyz/en/latest/api/expected.html")
      (license license:cc0))))

(define-public microsoft-gsl
  (let ((version "2.1.0"))
    (package
      (name "GSL")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Microsoft/GSL")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09f08lxqm00152bx9yrizlgabzpzxlpbv06h00z4w78yxywgxlgx"))))
      (build-system cmake-build-system)
      (inputs `(("catch2" ,catch-framework2)))
      (synopsis "C++ Core Guideline support library")
      (description "The Guideline Support Library (GSL) contains functions and types that are suggested for
 use by the C++ Core Guidelines maintained by the Standard C++ Foundation.
 This package contains Microsoft's implementation of GSL.")
      (home-page "https://github.com/Microsoft/GSL")
      (license "MIT"))))

(define-public range-v3
  (let ((version "0.10.0"))
    (package
      (name "range-v3")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ericniebler/range-v3")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1h9h5j7pdi0afpip9ncq76h1xjhvb8bnm585q17afz2l4fydy8qj"))))
      (build-system cmake-build-system)
      (synopsis "Experimental range library for C++11/14/17")
      (description "Experimental range library for C++11/14/17")
      (home-page "https://github.com/ericniebler/range-v3")
      (license license:boost1.0))))

(define-public rlottie
  (let ((version "2020.01.20")
        (commit "ee86b0dc56a6bb6284a721fd505930f1ba566e50"))
    (package
      (name "rlottie")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Samsung/rlottie") ; there is also https://github.com/desktop-app/rlottie no idea what the proper source is...
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07z21zv07957lq3s171528ljg9acz8d34g0phhi70svg88pszna6"))))
      (build-system cmake-build-system)
      (inputs `(("gtest" ,googletest)))
      (arguments
       `(#:configure-flags `("-DLOTTIE_TEST=ON"
                             "-DLIB_INSTALL_DIR=lib"))) ; why can they not just use CMAKE_INSTALL_LIBDIR :(
      (synopsis "A platform independent standalone library that plays Lottie Animation.")
      (description "rlottie is a platform independent standalone c++ library for rendering vector based animations and art in realtime.")
      (home-page "https://github.com/Samsung/rlottie")
      (license license:lgpl2.1))))

                                        ; add pkg-config support
(define-public libtgvoip-tdesktop
  (let ((commit "522550a1e975b17e9048d7a2ab2d5b97cfc2f5d4")
        (version "2.4.4"))
    (package
      (inherit libtgvoip)
      (version (string-append version "-" (string-take commit 9)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (delete-file "configure")
               (invoke "autoreconf" "-vfi")
               #t)))))
      (source (origin
                (inherit (package-source libtgvoip))
                (uri (git-reference
                      (url "https://github.com/telegramdesktop/libtgvoip")
                      (commit commit)))
                (file-name (git-file-name (package-name libtgvoip) version))
                (patches `())
                (sha256
                 (base32
                  "0rpvxsgjzhqm5xcffdsyws7cf3awv98p1y1zmkdsnq80h4v046lv")))))))

(define-public telegram-desktop
  (let ((version "2.1.0"))
    (package
      (name "telegram-desktop")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/telegramdesktop/tdesktop")
                      (commit (string-append "v" version))
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "116vbi1x9sr85p2r7q611mnbz18kcwqm4kasvn0zp2yyy6b4wqcx"))))
      (inputs `(("qtbase" ,qtbase)
                ("qtimageformats" ,qtimageformats)
                ("hunspell" ,hunspell)
                ("gtk3" ,gtk+)
                ("libdbusmenu-qt" ,libdbusmenu-qt)
                ("enchant" ,enchant)
                ("lz4" ,lz4)
                ("xxhash" ,xxhash)
                ("ffmpeg" ,ffmpeg)
                ("minizip" ,minizip)
                ("OpenAL" ,openal)
                ("opus" ,opus)
                ("alsa",alsa-lib)
                ("pulseaudio",pulseaudio)
                ("tl-expected" ,tl-expected)
                ("range-v3" ,range-v3)
                ("GSL" ,microsoft-gsl)
                ("openssl" ,openssl)
                ("rlottie" ,rlottie)
                ("python" ,python)
                ("tgvoip" ,libtgvoip-tdesktop)
                ("pkg-config" ,pkg-config)))
      (native-inputs `(("gcc" ,gcc-9)))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'check)
                    (add-after 'install 'wrap-executable
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let ((out (assoc-ref outputs "out")))
                          (wrap-program (string-append out "/bin/telegram-desktop")
                            `("QT_PLUGIN_PATH" ":" prefix
                              ,(map (lambda (label)
                                      (string-append (assoc-ref inputs label)
                                                     "/lib/qt5/plugins"))
                                    '("qtbase" "qtimageformats"))))
                          #t))))
         #:configure-flags `("-DDESKTOP_APP_USE_PACKAGED_VARIANT=OFF"
                             "-DTDESKTOP_API_ID=17349"
                             "-DTDESKTOP_API_HASH=344583e45741c457fe1862106095a5eb")))
      (synopsis "Telegram Desktop messaging app")
      (description "Telegram Desktop messaging app")
      (home-page "https://desktop.telegram.org/")
      (license license:gpl3))))
