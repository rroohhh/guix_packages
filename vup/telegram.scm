(define-module (vup telegram)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages image)
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
  (let ((version "3.0.1"))
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
                  "1msr3rsnqbmp6vg86wyhny5cjqy291hd9nkyx91512ir3n77j4j0"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags `("-DGSL_TEST=OFF")
         #:phases (modify-phases %standard-phases
                    (delete 'check))))
      (inputs `(("gtest" ,googletest)))
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
  (let ((version "2020.09.20")
        (commit "839dcab7f083a51b8130061ea5ec245195af6c58"))
    (package
      (name "rlottie")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/desktop-app/rlottie") ; there is also https://github.com/Samsung/rlottie no idea what the proper source is...
                      (commit commit)))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* "CMakeLists.txt"
                      (("-Werror") "")) ;; whyyyyyyy????? does anybody use Werror for release?????
                    #t))
                (sha256
                 (base32
                  "0ivlyx2kmpwwpdqkp3hkhb997knr69b3yaz08lq47ia37w6fzdvw"))))
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
  (let* ((commit "8682c5c22e9c3a28ee3aacfd1d529db07ea914bf")
         (version (string-append "2.4.4+" (string-take commit 9))))
    (package
      (inherit libtgvoip)
      (version (string-append version "-" (string-take commit 9)))
      (inputs
       `(("pulseaudio" ,pulseaudio)
         ("openssl" ,openssl)
         ("alsa",alsa-lib)
         ("opus" ,opus)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("gcc" ,gcc-9)
         ("pkg-config" ,pkg-config)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (delete-file "configure")
               (invoke "autoreconf" "-vfi")
               #t)))))
      ;; (add-after 'unpack 'set-c++17
      ;;     (lambda _
      ;;       (setenv "CFLAGS" "-std=c++17")
      ;;       (setenv "CXXFLAGS" "-std=c++17")
      ;;       #t)))))
      (source (origin
                (inherit (package-source libtgvoip))
                (uri (git-reference
                      (url "https://github.com/telegramdesktop/libtgvoip")
                      (commit commit)))
                (file-name (git-file-name (package-name libtgvoip) version))
                (patches `())
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* "Makefile.am"
                      (("gnu\\+\\+0x") "gnu++17"))
                    #t))
                (sha256
                 (base32
                  "13br0dsnmgjamsql9hrj3hgdi9a6psbwjb17g03r841c4w1pjbr4")))))))

(define-public tg_owt
  (let ((version "0.0.1"))
    (package
      (name "tg_owt")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/desktop-app/tg_owt")
                      (commit "c73a4718cbff7048373a63db32068482e5fd11ef")))
                (file-name (git-file-name name version))
                (patches (search-patches "tg_owt_install.patch"))
                (sha256
                 (base32
                  "0nr20mvvmmg8ii8f2rljd7iv2szplcfjn40rpy6llkmf705mwr1k"))))
      (build-system cmake-build-system)
      (inputs `(("openssl" ,openssl) ("libjpeg" ,libjpeg-turbo) ("pkg-config" ,pkg-config)
                ("ffmpeg" ,ffmpeg) ("opus" ,opus) ("alsa" ,alsa-lib) ("pulseaudio" ,pulseaudio)
                ("yasm" ,yasm)))
      (arguments `(#:phases (modify-phases %standard-phases
                              (delete 'check)))) ;; no tests
      (synopsis "???")
      (description "???")
      (home-page "https://github.com/desktop-app/tg_owt")
      (license license:bsd-3))))

(define-public telegram-desktop
  (let ((version "2.4.3"))
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
                (modules '((guix build utils)))
                ;; (snippet
                ;;  '(begin
                ;;     (substitute* "cmake/external/qt/package.cmake"
                ;;       (("Core Gui Widgets Network") "Core Gui Widgets Network XkbCommonSupport"))
                ;;     (substitute* "cmake/external/qt/CMakeLists.txt"
                ;;       (("Qt5::Network") "Qt5::Network\nQt5::XkbCommonSupport")
                ;;       (("\\$\\{Qt5Core_PRIVATE_INCLUDE_DIRS\\}") "${Qt5Core_PRIVATE_INCLUDE_DIRS}\n${Qt5XkbCommonSupport_PRIVATE_INCLUDE_DIRS}"))
                ;;     #t))
                ;; (patches (search-patches "random_fuckup_new.patch"))
                ;; (patches `("random_fuckup_new.patch"))
                (sha256
                 (base32
                  "04jgi4k0j02nn4s720d6gnxw89k8clgx2x2z7ymxb7rssz72pxh5"))))
      (inputs `(("qtbase" ,qtbase)
                ("qtimageformats" ,qtimageformats)
                ("qtwayland" ,qtwayland)
                ("hunspell" ,hunspell)
                ("gtk3" ,gtk+)
                ("libdbusmenu-qt" ,libdbusmenu-qt)
                ("enchant" ,enchant)
                ("lz4" ,lz4)
                ("xxhash" ,xxhash)
                ("ffmpeg" ,ffmpeg)
                ("tg_owt" ,tg_owt)
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
