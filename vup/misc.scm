(define-module (vup misc)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system maven)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages boost)
  #:use-module (vup mesa))

(define-public mbuffer
  (let* ((version "20210209"))
    (package
      (name "mbuffer")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://www.maier-komor.de/software/mbuffer/mbuffer-" version ".tgz"))
         (sha256
          (base32 "034si7ym7ckl2lq7yhwihqr14vdc375z47hq93w207v2wa42f7z8"))))
      (build-system gnu-build-system)
      (arguments '(#:phases
                   (modify-phases %standard-phases
                     (delete 'check)))) ;; too lazy to include test deps
      (home-page "https://www.maier-komor.de/mbuffer.html")
      (synopsis "mbuffer is a tool for buffering data streams with a large set of unique features")
      (description "mbuffer is a tool for buffering data streams with a large set of unique features")
      (license licenses:gpl3))))

(define-public libmodbus
  (let* ((version "3.1.6"))
    (package
      (name "libmodbus")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://libmodbus.org/releases/libmodbus-" version ".tar.gz"))
         (sha256
          (base32 "05kwz0n5gn9m33cflzv87lz3zp502yp8fpfzbx70knvfl6agmnfp"))))
      (build-system gnu-build-system)
      (home-page "https://libmodbus.org/")
      (synopsis "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Win32")
      (description "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Win32")
      (license licenses:gpl3))))

(define-public mbpoll
  (let* ((version "1.4.11"))
    (package
      (name "mbpoll")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/epsilonrt/mbpoll/archive/v" version ".tar.gz"))
         (sha256
          (base32 "00dh65jky7a97r538nb5n0pgy3r175s41hmbh3yasqri867jwcsx"))))
      (build-system cmake-build-system)
      (inputs `(("libmodbus" ,libmodbus)
                ("pkg-config" ,pkg-config)))
      (arguments '(#:phases
                   (modify-phases %standard-phases
                     (delete 'check)))) ;; no tests
      (home-page "https://github.com/epsilonrt/mbpoll")
      (synopsis "mbpoll is a command line utility to communicate with ModBus slave (RTU or TCP).")
      (description "mbpoll is a command line utility to communicate with ModBus slave (RTU or TCP).")
      (license licenses:gpl3))))

(define-public antpm
  (let* ((version "1.20"))
    (package
      (name "antpm")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/ralovich/antpm/archive/v" version ".tar.gz"))
         (sha256
          (base32 "1rwp707fcg5w5qfhmadbjrbia8arjfz8knb7pvcycxl6f4hz3sn8"))))
      (build-system cmake-build-system)
      (inputs `(("libusb" ,libusb) ("boost" ,boost) ("libxml2" ,libxml2)))
      (arguments '(#:configure-flags `("-DUSE_BOOST_STATIC_LINK=False")
                   #:phases
                   (modify-phases %standard-phases
                     (add-before 'configure 'cd-to-src
                       (lambda _
                         (chdir "src")
                         #t))
                     (delete 'check)))) ;; no tests
      (home-page "https://github.com/ralovich/antpm")
      (synopsis "ANT+minus (ANT / ANT+ / ANT-FS)")
      (description "ANT+minus (ANT / ANT+ / ANT-FS)")
      (license licenses:gpl3))))

(define-public rdfind
  (let* ((version "1.4.1"))
    (package
      (name "rdfind")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/pauldreik/rdfind/archive/releases/" version ".tar.gz"))
         (sha256
          (base32 "1126rpn0ld6a8b3szmml8gsg3x68m03n3dycjyqk81xjcm3sxs9y"))))
      (build-system gnu-build-system)
      (inputs `(("automake" ,automake) ("nettle" ,nettle)
                ("autoconf" ,autoconf) ("autoconf-archive" ,autoconf-archive)))
      (arguments '(#:phases
                   (modify-phases %standard-phases
                     (delete 'check)))) ;; too lazy to make tests work
      (home-page "https://github.com/pauldreik/rdfind")
      (synopsis "find duplicate files utility")
      (description "find duplicate files utility")
      (license licenses:gpl2+))))

(define-public xrestop
  (let* ((version "0.4"))
    (package
      (name "xrestop")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "http://downloads.yoctoproject.org/releases/xrestop/xrestop-" version ".tar.gz"))
         (sha256
          (base32 "0mz27jpij8am1s32i63mdm58znfijcpfhdqq1npbmvgclyagrhk7"))))
      (build-system gnu-build-system)
      (inputs `(("libxres" ,libxres) ("libx11" ,libx11) ("libxext" ,libxext)
                ("ncurses" ,ncurses)))
      (home-page "http://freedesktop.org/wiki/Software/xrestop")
      (synopsis "Uses the X-Resource extension to provide 'top' like statistics")
      (description "Uses the X-Resource extension to provide 'top' like statistics")
      (license licenses:gpl2+))))

(define-public libsigrok-master
  (package
    (inherit libsigrok)
    (version "0.5.2-1c5d5905")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://repo.or.cz/libsigrok.git")
                    (commit "1c5d5905a44f8e3abbb9327cb47e80d09eb2dc6a")
                    (recursive? #t))) ; for prjtrellis-db
              (file-name (git-file-name "libsigrok" version))
              (sha256
               (base32 "07wvy06a1w8dir6d8ha7crcvnpayv7z4y2wpjmz6k0y50wdbbm5q"))))
    (inputs
     `(("python" ,python)
       ("zlib" ,zlib)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))))

(define-public pulseview-libsigrok-master
  (package
    (inherit
     ((package-input-rewriting/spec
       `(("libsigrok" . ,(const libsigrok-master)))) pulseview))
    (version (string-append (package-version pulseview) "-89b7b94a0"))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://sigrok.org/pulseview.git")
             (commit "89b7b94a048ec53e82f38412a4b65cabb609f395")))
       (file-name (git-file-name (package-name pulseview) version))
       (sha256
        (base32 "1vvkm30gw8wy8a3j73npzn0fybqskhx3mv3wb13zlhyvy3k1hmvz"))))))

(define-public ofono
  (package
    (name "ofono")
    (version "1.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.kernel.org/pub/scm/network/ofono/ofono.git")
             (commit "285fad8f39d46a5f0a0f9d194789978227558d1e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f8ivncndjq13gn0nmrz0zm51nhnqm2rg2nr5fxzcwv6i2bcvg7z"))
       (patches `("0001-Search-connectors-in-OFONO_PLUGIN_PATH.patch"))))
    (build-system gnu-build-system)
    (inputs `(("automake" ,automake) ("nettle" ,nettle) ("libtool" ,libtool)
              ("autoconf" ,autoconf) ("autoconf-archive" ,autoconf-archive)
              ("pkg-config" ,pkg-config) ("glib" ,glib) ("dbus" ,dbus) ("ell" ,ell)
              ("udev" ,eudev) ("mobile-broadband-provider-info" ,mobile-broadband-provider-info)
              ("bluez" ,bluez)))
    (arguments '(#:configure-flags
                 (list
                  "--enable-external-ell"
                  (string-append
                   "--with-dbusconfdir=" (assoc-ref %outputs "out") "/etc")
                  (string-append
                   "--with-dbusdatadir=" (assoc-ref %outputs "out") "/share"))
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'check)))) ;; there are no tests
    (home-page "https://01.org/ofono")
    (synopsis "ofono")
    (description "ofono")
    (license licenses:gpl2)))

(define %common-gstreamer-phases
  '((add-after 'unpack 'increase-test-timeout
      (lambda _
        (substitute* "tests/check/meson.build"
          (("'CK_DEFAULT_TIMEOUT', '[0-9]*'")
           "'CK_DEFAULT_TIMEOUT', '600'")
          (("timeout ?: .*\\)")
           "timeout: 90 * 60)"))
        #t))))

(define-public gstreamer-vaapi
  (package
    (name "gstreamer-vaapi")
    (version "1.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/" name "/"
                    name "-" version ".tar.xz"))
              (patches (search-patches "gstreamer_vaapi.patch"))
              (sha256
               (base32
                "1sm6x2qa7ng78w0w8q4mjs7pbpbbk8qkfgzhdmbb8l0bh513q3a0"))))
    (build-system meson-build-system)
    (arguments
     ;; FIXME: 16/22 failing tests.
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  ,@%common-gstreamer-phases)))
    (inputs
     `(("libva" ,libva)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-bad", gst-plugins-bad)
       ("libdrm", libdrm)
       ("mesa" ,mesa-fixed)))
    (native-inputs
     `(; ("flex" ,flex)
                                        ; ("gst-plugins-bad" ,gst-plugins-bad)
                                        ; ("gst-plugins-good" ,gst-plugins-good)
                                        ; ("perl" ,perl)
       ("pkg-config" ,pkg-config)
                                        ; ("python" ,python)
       ))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer library for vaapi")
    (description
     "Hardware-accelerated video decoding, encoding and processing on Intel graphics through VA-API")
    (license licenses:gpl2+)))

(define-public carla-2.2
  (package
    (inherit carla)
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/falkTX/Carla")
         (commit (string-append "v" version))))
       (file-name (git-file-name (package-name carla) version))
       (sha256
        (base32
         "0p289d1aj5ymmd7724fdcjq34drrn7xg33qnyvrq4ns4wd36i307"))))))
