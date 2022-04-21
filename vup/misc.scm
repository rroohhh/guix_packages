(define-module (vup misc)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system maven)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages nettle)
  #:use-module ((gnu packages animation) #:prefix guix:)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages check)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages man)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages protobuf)
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

(define-public libsigrokdecode-master
  (package
    (inherit libsigrokdecode)
    (version "0.5.3-02aa01a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://repo.or.cz/libsigrokdecode.git")
                    (commit "02aa01ad5f05f2730309200abda0ac75d3721e1d")
                    (recursive? #t)))
              (file-name (git-file-name "libsigrok" version))
              (sha256
               (base32 "054p2sja32d5shlbsvrpaw3pq7gg4n03327ml1dn53pjnsl0wbjz"))))
    (native-inputs (append (package-native-inputs libsigrokdecode)
                       `(("autoconf" ,autoconf)
                         ("automake" ,automake)
                         ("libtool" ,libtool))))))

(define-public libsigrok-master
  (package
    (inherit libsigrok)
    (version "0.5.3-e972674d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://repo.or.cz/libsigrok.git")
                    (commit "e972674d0b30b98dcc354b707a80b6bfc1aeb532")
                    (recursive? #t)))
              (file-name (git-file-name "libsigrok" version))
              (sha256
               (base32 "0sp9y0wb6caw6d69h0z10hd6vgjgmi8z1a93i3yjbzxx8a48iyzg"))))
    (inputs (append (package-inputs libsigrok)
                `(("glibmm" ,glibmm-2.64))))
    (native-inputs (append (package-native-inputs libsigrok)
                    `(("autoconf" ,autoconf)
                      ("automake" ,automake)
                      ("libtool" ,libtool))))))


(define-public pulseview-libsigrok-master
  (package
    (inherit
     ((package-input-rewriting/spec
       `(("libsigrok" . ,(const libsigrok-master))
         ("libsigrokdecode" . ,(const libsigrokdecode-master)))) pulseview))
    (version (string-append (package-version pulseview) "-a6fa4d47"))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://sigrok.org/pulseview.git")
             (commit "a6fa4d477d783478935a78c1b70596e38ae8ca64")))
       (file-name (git-file-name (package-name pulseview) version))
       (sha256
        (base32 "1j5g8w74zmskq1r0rj68yz4xqv4z9j91v2hwr3i2jyk4g3yfxvd3"))))
    (inputs (append (package-inputs pulseview)
              `(("glibmm" ,glibmm-2.64))))))

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
              (patches
                (list
                 (origin (method url-fetch)
                         (uri "https://raw.githubusercontent.com/rroohhh/guix_packages/e99ecbb/gstreamer_vaapi.patch")
                         (sha256 "06sk93zy9ddq3iynswjjiq4gv7kn5qgy5rnygjld34jxvmp2gyl6"))))
              ;; (patches (search-patches "gstreamer_vaapi.patch"))
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
       ("mesa" ,mesa)))
    (native-inputs
     `(; ("flex" ,flex)
                                        ; ("gst-plugins-bad" ,gst-plugins-bad)
                                        ; ("gst-plugins-good" ,gst-plugins-good)
                                        ; ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
                                        ; ("python" ,python)

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

(define-public kicad-nightly
  (package
   (inherit kicad)
   (name "kicad-nightly")
   (version "6.0.0-fe6cc0c3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.com/kicad/code/kicad.git")
              
           (commit "fe6cc0c3d8a8784e061d0b77643e8334ed4d14ac")))
     (sha256
      (base32 "0yv54lz9jr48mz7qngfi31r04l852nfv8n0h6np7pkp48lqysqig"))
     (file-name (git-file-name name version))))
   (inputs (append `(("occ" ,opencascade-occt) ("gtk+3" ,gtk+)) (alist-delete  "opencascade-oce" (package-inputs kicad))))
   (arguments
     (substitute-keyword-arguments (package-arguments kicad)
       ((#:configure-flags flags)
        `(append `("-DKICAD_USE_OCC=true" ,(string-append "-DOCC_INCLUDE_DIR=" (assoc-ref %build-inputs "occ") "/include/opencascade")) ,flags))))))
   

(define synfig-version "1.4.0")
(define-public etl
  (package
    (inherit guix:etl)
    (version synfig-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/synfig/releases/"
                                  version "/source/ETL-" version ".tar.gz"))
              (sha256
               (base32
                "04d0s40z4g5ndnj90ma7sn42az14ay96l8b96iqi8q9mmk09ccyl"))))))

(define-public synfig
  (package
    (inherit guix:synfig)
    (version synfig-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/synfig/releases/"
                                  version "/source/synfig-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "170c10rzz7kalharjffryavqv845hn8fd2dfvvhmkjcxp9zdadkz"))))
    (propagated-inputs (append `(("ffmpeg" ,ffmpeg)) (package-propagated-inputs guix:synfig)))
    (inputs (append `(("etl" ,etl)) (alist-delete  "etl" (package-inputs guix:synfig))))))

(define-public synfigstudio
  (package
    (inherit guix:synfigstudio)
    (version synfig-version)
    (source (origin
              (modules '((guix build utils)))
              (snippet
                '(begin
                  (substitute* "src/gui/pluginmanager.cpp"
                    (("xmlpp::Node\\* n =")    "const xmlpp::Node* n =")
                    (("xmlpp::Node::NodeList") "xmlpp::Node::const_NodeList"))
                  #t))
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/synfig/releases/"
                                      version "/source/synfigstudio-" version
                                      ".tar.gz"))
              (sha256
                (base32
                  "0mmx7z4p5vfdmbzkv7g1rsb9s1q5w2aijvap9abmfk16waiv27na"))))
    (inputs (append `(("etl" ,etl) ("synfig" ,synfig)) (alist-delete  "synfig" (package-inputs guix:synfigstudio))))))

(define-public fwupd
  (package
    (name "fwupd")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/fwupd/fwupd")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k4wcncyaynwzaz96rdf9q60hn0zgjhfjcn83ky108lwcvr5jsf7"))
       (patches '("fwupd-do-not-write-to-var.patch"
                  "fwupd-add-option-for-installation-sysconfdir.patch"
                  "fwupd-installed-tests-path.patch"))))
    (build-system meson-build-system)
    (outputs (list "out" "installed-tests"))
    (arguments
     `(#:configure-flags
       (list "--wrap-mode=nofallback"
             "-Dsystemd=false"
             (string-append
              "-Defi-libdir=" (assoc-ref %build-inputs "gnu-efi") "/lib")
             (string-append
              "-Defi-ldsdir=" (assoc-ref %build-inputs "gnu-efi") "/lib")
             (string-append
              "-Defi-includedir=" (assoc-ref %build-inputs "gnu-efi")
              "/include/efi")
             (string-append
              "-Dudevdir=" (assoc-ref %outputs "out") "/lib/udev")
             "--localstatedir=/var"
             "--sysconfdir=/etc"
             (string-append
              "-Dsysconfdir_install=" (assoc-ref %outputs "out") "/etc")
             (string-append
              "--libexecdir=" (assoc-ref %outputs "out") "/libexec")
             "-Dsupported_build=true"
             (string-append
              "-Dinstalled_test_prefix="
              (assoc-ref %outputs "installed-tests")))
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-before 'install 'no-polkit-magic
           ;; Meson ‘magically’ invokes pkexec, which fails (not setuid).
           (lambda _
             (setenv "PKEXEC_UID" "something")
             #t)))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("python-pygobject" ,python-pygobject)
       ("python-pillow" ,python-pillow)
       ("python-pycairo" ,python-pycairo)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("umockdev" ,umockdev)
       ("glib:bin" ,glib "bin")
       ("help2man" ,help2man)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("glib" ,glib)
       ("libgudev" ,libgudev)
       ("libxmlb" ,libxmlb)
       ("gusb" ,gusb)
       ("sqlite" ,sqlite)
       ("libarchive" ,libarchive)
       ("libjcat" ,libjcat)
       ("json-glib" ,json-glib)
       ("curl" ,curl)
       ("polkit" ,polkit)
       ("eudev" ,eudev)
       ("gcab" ,gcab)
       ("gnutls" ,gnutls)
       ("libelf" ,libelf)
       ("tpm2-tss" ,tpm2-tss)
       ("cairo" ,cairo)
       ("efivar" ,efivar)
       ("pango" ,pango)
       ("mingw-w64-tools", mingw-w64-tools)
       ("libsmbios" ,libsmbios)
       ("gnu-efi" ,gnu-efi)))
    (home-page "https://fwupd.org/")
    (synopsis "A simple daemon to allow session software to update firmware")
    (description "This package aims to make updating firmware on Linux
automatic, safe and reliable.")
    (license licenses:lgpl2.1+)))

(define-public xdg-desktop-portal-wlr
  (package
    (name "xdg-desktop-portal-wlr")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18nlkqqxgxh7k0r2nk867wnp2nmaiinl6z67lrfv7rmiym0x82p8"))))
    (build-system meson-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("pipewire" ,pipewire-0.3) ("wayland" ,wayland)
              ("wayland-protocols" ,wayland-protocols) ("iniparser" ,iniparser)
              ("elogind" ,elogind)))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DESKTOP_PORTAL_DIR")
            (files '("share/xdg-desktop-portal/portals")))))
    (home-page "https://github.com/flatpak/xdg-desktop-portal-gtk")
    (synopsis "xdg-desktop-portal backend for wlroots")
    (description
     "xdg-desktop-portal backend for wlroots")
    (license #f)))

(define-public btrbk
  (package
    (name "btrbk")
    (version "0.31.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/digint/btrbk")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wlr0nikc8grskb574sfxpi1rd5v8vmaxbvpna37g8kxs1al10ld"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("python" ,python)
              ("btrfs" ,btrfs-progs)
              ("bash" ,bash)
              ("mbuffer" ,mbuffer)
              ("openssh" ,openssh)
              ("coreutils" ,coreutils)))
    (native-inputs `(("asciidoctor" ,ruby-asciidoctor)))
    (arguments `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                                    (string-append "CONFDIR=" (assoc-ref %outputs "out") "/etc"))
                 #:phases (modify-phases %standard-phases
                            (delete 'configure)
                            (delete 'check)
                            (add-after 'install 'wrap
                              (lambda* (#:key outputs inputs #:allow-other-keys)
                                (wrap-program (string-append (assoc-ref outputs "out") "/bin/btrbk")
                                  `("PATH" ":" = (,(string-append (assoc-ref inputs "btrfs") "/bin")
                                                  ,(string-append (assoc-ref inputs "bash") "/bin")
                                                  ,(string-append (assoc-ref inputs "mbuffer") "/bin")
                                                  ,(string-append (assoc-ref inputs "openssh") "/bin")
                                                  ,(string-append (assoc-ref inputs "coreutils") "/bin")))))))))
    (home-page "https://github.com/digint/btrbk")
    (synopsis "Tool for creating snapshots and remote backups of btrfs subvolumes")
    (description
     "Btrbk is a backup tool for btrfs subvolumes, taking advantage of btrfs specific capabilities to create atomic snapshots and transfer them incrementally to your backup locations.")
    (license licenses:gpl3)))

(define-public gst-rtsp-server
  (package
    (name "gst-rtsp-server")
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/" name "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qjlp7az0hkzxvq53hwnp55sp6xhbybfwzaj66hp45jslsmj4fcp"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ;; 17/17 failing ^^
       #:phases (modify-phases %standard-phases
                  ,@%common-gstreamer-phases)))
    (inputs
     `(("glib" ,glib) ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-bad" ,gst-plugins-bad)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "RTSP server based on GStreamer")
    (description
     "gst-rtsp-server is a library on top of GStreamer for building an RTSP server")
    (license licenses:gpl2+)))

(define-public whatsapp-for-linux
  (let ((commit "1.3.0"))
    (package
      (name "whatsapp-for-linux")
      (version (string-append commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/eneshecan/whatsapp-for-linux")
                      (commit (string-append "v" commit))
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dsqzai59k4x69dzc46r94vnpvhr85fg8iwdapczza4r767h5nam"))))
      (build-system cmake-build-system)
      (arguments '(#:phases
                   (modify-phases %standard-phases
                     (delete 'check))))
      (inputs `(("gtkmm" ,gtkmm-3) ("webkitgtk" ,webkitgtk) ("libappindicator" ,libappindicator)
                ("glib:bin" ,glib "bin")))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (synopsis "An unofficial WhatsApp desktop application for Linux.")
      (description "An unofficial WhatsApp desktop application for Linux.")
      (home-page "https://github.com/eneshecan/whatsapp-for-linux")
      (license licenses:gpl3))))

(define-public glslang-11.8
  (package
    (inherit glslang)
    (version "11.8.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/glslang")
            (commit version)))
      (sha256
       (base32 "0xxi792f0lkxvvm9c2zaidhi078cb2hbq7rikflr0vxygir4xi10"))
      (file-name (git-file-name (package-name glslang) version))))))


(define-public spirv-headers-2022
  (package
    (inherit spirv-headers)
    (version "1.3.204.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/SPIRV-Headers")
            (commit (string-append "sdk-" version))))
      (sha256
       (base32 "1pd2gzq4sh67qffc91apq7z3fi8fhwcfbwzfzgpfb3vb7q54kkwj"))
      (file-name (git-file-name (package-name spirv-headers) version))))))


(define-public spirv-tools-2022
  (package
    (inherit spirv-tools)
    (version "2022.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/SPIRV-Tools")
            (commit (string-append "v" version))))
      (sha256
       (base32 "1k2nl8drdskfnr1n3avhgdmlnjrbqdn0aw9ph1cdcaj0h4ng625s"))
      (file-name (git-file-name (package-name spirv-tools) version))))
    (inputs (modify-inputs (package-inputs spirv-tools) (replace "spirv-headers" spirv-headers-2022)))))


(define-public shaderc-2022
  (package
    (inherit shaderc)
    (version "2022.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/shaderc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "shaderc" version))
              (sha256
               (base32 "0v4mvrw8gl3xxr4d7qlfmgmprbyj9xc50wgk1lpm5icxkjyb0rr9"))))
    (inputs 
      (modify-inputs (package-inputs shaderc)
                     (replace "glslang" glslang-11.8)
                     (replace "spirv-tools" spirv-tools-2022)
                     (replace "spirv-headers" spirv-headers-2022)))))


(define-public zfp
  (let* ((version "0.5.5"))
    (package
      (name "zfp")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/LLNL/zfp/releases/download/" version "/zfp-" version ".tar.gz"))
         (sha256
          (base32 "1ickfca08ga6scgabf4gmlby894cv67h9hp2rzffbx5ip94bkxzx"))))
      (inputs (list python python-numpy python-cython))
      (build-system cmake-build-system)
      (arguments '(#:configure-flags `("-DBUILD_ZFPY=YES")))
      (home-page "https://computing.llnl.gov/projects/zfp")
      (synopsis "zfp: Compressed Floating-Point and Integer Arrays")
      (description "zfp: Compressed Floating-Point and Integer Arrays")
      (license licenses:bsd-3))))

(define-public std_compat
  (let* ((version "0.0.14")
         (commit "cd2dcc2963c59c76f853f689d4e43b37caea1277"))
    (package
      (name "std_compat")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/robertu94/std_compat")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0a6wcy3pbcv7j48dxkp9m9970yzkrfdy849y6iskbdgc3rn9a51h"))))
      (build-system cmake-build-system)
      (arguments '(#:configure-flags `("-DBUILD_TESTING=NO")
                   #:phases (modify-phases %standard-phases
                                           (delete 'check))))
      (home-page "https://github.com/robertu94/std_compat")
      (synopsis "compatability header for older c++")
      (description "compatability header for older c++")
      (license #f))))

(define-public sz
  (let* ((version "2.1.12.2"))
    (package
      (name "sz")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/szcompressor/SZ/releases/download/v" version "/SZ-" version ".tar.gz"))
         (sha256
          (base32 "1ikd0sv0mivnd22idp9mby6xgihrda7gy2iyw5b0l6zd3wz2czj2"))))
      (inputs (list python python-numpy python-cython zlib zstd-cmake))
      (build-system cmake-build-system)
      (home-page "https://github.com/szcompressor/SZ")
      (synopsis "SZ2: Error-bounded Lossy Compressor for HPC Data")
      (description "SZ2: Error-bounded Lossy Compressor for HPC Data")
      (license #f))))

(define-public SZauto
  (let* ((version "1.2.1"))
    (package
      (name "SZauto")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/szcompressor/SZauto/releases/download/" version "/SZauto-" version ".tar.gz"))
         (sha256
          (base32 "0fmh141swmfqsaghi32rwl0hbpq7g4jajr7q9rl4z1rsvxc8ziam"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake))
      (home-page "https://github.com/szcompressor/SZauto")
      (synopsis "SZauto: SZ C++ Version that Supports Second-Order Prediction and Parameter Optimization")
      (description "SZauto: SZ C++ Version that Supports Second-Order Prediction and Parameter Optimization")
      (license #f))))

(define-public SZ3
  (let* ((version "3.1.4"))
    (package
      (name "SZ3")
      (version version)
      (source
       (origin
         (method url-fetch)
         (patches (search-patches "sz3_zstd_cmake.patch"))
         (uri (string-append "https://github.com/szcompressor/SZ3/archive/refs/tags/v." version ".tar.gz"))
         (sha256
          (base32 "0jrzpingv09kb6xigc923y60b6x9vqz7fx39a2gad3c7z17rv3w5"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake gsl))
      (home-page "https://github.com/szcompressor/SZ3")
      (synopsis "SZ3: A Modular Error-bounded Lossy Compression Framework for Scientific Datasets")
      (description "SZ3: A Modular Error-bounded Lossy Compression Framework for Scientific Datasets")
      (license #f))))

(define-public digitroundingZ
  (let* ((version "0.1")
         (commit "68555fade9ecb1b123f29436461e02f7acb4f738"))
    (package
      (name "digitroundingZ")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/disheng222/digitroundingZ")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1i8l41k6mm9hfipamhdm1sw35zxwcfh7p64h3chdsq37023hvyl6"))))
      (build-system cmake-build-system)
      (inputs (list zlib))
      (home-page "https://github.com/disheng222/digitroundingZ")
      (synopsis "The standalone digit rounding compressor which will be convenient for evaluation")
      (description "The standalone digit rounding compressor which will be convenient for evaluation")
      (license licenses:lgpl3))))

(define-public bitgroomingZ
  (let* ((version "0.1")
         (commit "4816b7f1b92765cac57bd01cd4e3cde1b8bdb65f"))
    (package
      (name "bitgroomingZ")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/disheng222/bitgroomingZ")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1yab8x15067z97559azqns7v57ldzsawgnvyr1nd80qzivyxmbs6"))))
      (build-system cmake-build-system)
      (inputs (list zlib))
      (home-page "https://github.com/disheng222/BitGroomingZ")
      (synopsis "BGZ: Bit Grooming Compressor")
      (description "BGZ: Bit Grooming Compressor")
      (license #f))))

(define-public mgard
  (let* ((version "1.0.0")
         (commit "0f6cdf9b59e837547e3298be7187de8df376bb18"))
    (package
      (name "mgard")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CODARcode/MGARD")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0382ijm5pc8cvrkqzx9hv4g8pzk2r5vbn6gih374r3as04zj9swj"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake zlib python protobuf pkg-config))
      (arguments `(#:configure-flags '("-DMGARD_ENABLE_CLI=YES")))
      (home-page "https://github.com/CODARcode/MGARD")
      (synopsis "MGARD: MultiGrid Adaptive Reduction of Data")
      (description "MGARD: MultiGrid Adaptive Reduction of Data")
      (license #f))))

(define-public mgard-0.1
  (let* ((version "0.1.0")
         (commit "7d9715e612488731dfa5f8e488e7976539464c3e"))
    (package
      (name "mgard")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CODARcode/MGARD")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0pyk3p3q2jamc0i84g9pg4m5qgk8ic7b90y1j1fmfqqygb7whzim"))))
      (build-system cmake-build-system)
      (inputs (list zstd-cmake zlib python protobuf pkg-config))
      (arguments `(#:configure-flags '("-DMGARD_ENABLE_CLI=YES")
                   #:phases (modify-phases %standard-phases (delete 'check))))
      (home-page "https://github.com/CODARcode/MGARD")
      (synopsis "MGARD: MultiGrid Adaptive Reduction of Data")
      (description "MGARD: MultiGrid Adaptive Reduction of Data")
      (license #f))))

(define-public ndzip
  (let* ((version "0.1")
         (commit "4e6e38e40af7f44fda05569a976445b226275997"))
    (package
      (name "ndzip")
      (version (string-append version "-" (string-take commit 9)))
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fknorr/ndzip")
                    (commit commit)))
              (file-name (git-file-name name version))
              (patches (search-patches "ndzip_install.patch"))
              (sha256
               (base32 "0iahng8k6mhdg2xf3ric5zv2wdhcffz9sjvlix7v4cxixl846xi0"))))
      (build-system cmake-build-system)
      (inputs (list boost))
      (arguments `(#:phases (modify-phases %standard-phases (delete 'check))))
      (home-page "https://github.com/fknorr/ndzip")
      (synopsis "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (description "A High-Throughput Parallel Lossless Compressor for Scientific Data")
      (license licenses:expat))))

(define-public fpzip
  (let* ((version "1.3.0"))
    (package
      (name "fpzip")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/LLNL/fpzip/releases/download/" version "/fpzip-" version ".tar.gz"))
         (sha256
          (base32 "0v0ky3sdqwg13k2zvy786kbzrhvp59mrb5s79jmgxqsr8bcgg394"))))
      (build-system cmake-build-system)
      (home-page "https://github.com/LLNL/fpzip")
      (synopsis "Lossless compressor of multidimensional floating-point arrays")
      (description "Lossless compressor of multidimensional floating-point arrays")
      (license licenses:bsd-3))))

(define-public sol2
  (let* ((version "3.2.2"))
    (package
      (name "sol2")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/ThePhD/sol2/archive/refs/tags/v" version ".tar.gz"))
         (sha256
          (base32 "1nmjlfqfi3fqqwzgqpl48ljsg6z47m1raddcvg91v0n1w3d905ql"))))
      (build-system cmake-build-system)
      (arguments `(#:phases (modify-phases %standard-phases (delete 'check))))
      (home-page "https://github.com/ThePhD/sol2")
      (synopsis "sol2 is a C++ library binding to Lua")
      (description "sol2 is a C++ library binding to Lua")
      (license licenses:expat))))

(define-public zstd-cmake
  (package
    (inherit zstd)
    (build-system cmake-build-system)
    (outputs '("out"))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check)
                            (add-before 'configure 'chdir
                              (lambda* _
                                (chdir "build/cmake")
                                #t)))))))

(define-public libpressio
  (let* ((version "0.83.4"))
    (package
      (name "libpressio")
      (version version)
      (source (origin
              (modules '((guix build utils)))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/robertu94/libpressio")
                    (commit version)))
              (file-name (git-file-name name version))
              (patches (search-patches "libpressio_python_install_path.patch"))
              (snippet
                '(begin
                  (substitute* "CMakeLists.txt"
                    (("CMP0069 NEW)") "SET CMP0069 NEW)
find_package(\"zstd\")
")
                    (("NDZip") "ndzip"))
                  #t))
              (sha256
               (base32 "0hh9y6qj4mr7zgw8p80gd8vxfw2vlwh4gcc6s5s16nj8cjj025zz"))))
      (build-system cmake-build-system)
      (arguments 
        '(#:configure-flags 
          `("-DBUILD_TESTING=NO"
            "-DLIBPRESSIO_INTERPROCEDURAL_OPTIMIZATION=YES"
            "-DLIBPRESSIO_HAS_OPENMP=YES"
            "-DLIBPRESSIO_HAS_SZ_AUTO=YES"
            "-DLIBPRESSIO_HAS_ZFP=YES"
            "-DLIBPRESSIO_HAS_SZ=YES"
            "-DLIBPRESSIO_HAS_BLOSC=YES"
            "-DLIBPRESSIO_HAS_MAGICK=YES"
            "-DLIBPRESSIO_HAS_HDF=YES"
            "-DLIBPRESSIO_HAS_FPZIP=YES"
            "-DLIBPRESSIO_HAS_PETSC=YES"
            "-DLIBPRESSIO_HAS_LUA=YES"
            "-DLIBPRESSIO_HAS_JSON=YES"
            "-DBUILD_PYTHON_WRAPPER=YES"
            "-DLIBPRESSIO_HAS_DIGIT_ROUNDING=YES"
            "-DLIBPRESSIO_HAS_BIT_GROOMING=YES"
            "-DLIBPRESSIO_HAS_LINUX=YES"
            "-DLIBPRESSIO_BUILD_MODE=FULL"
            "-DLIBPRESSIO_HAS_BZIP2=YES"
            "-DLIBPRESSIO_HAS_SZ3=YES"
            "-DLIBPRESSIO_HAS_NETCDF=YES"
            "-DLIBPRESSIO_HAS_NDZIP=YES"
            "-DLIBPRESSIO_HAS_MGARD=NO")
          #:phases (modify-phases %standard-phases (delete 'check))))
      (inputs (list pkg-config std_compat boost zfp c-blosc hdf5 imagemagick sz
                    digitroundingZ zlib bitgroomingZ SZauto zstd-cmake
                    protobuf SZ3 ndzip netcdf fpzip sol2 luajit json-modern-cxx 
                    python-numpy python swig petsc))
      (home-page "https://github.com/CODARcode/libpressio")
      (synopsis "LibPressio is a C++ library with C compatible bindings to abstract between different lossless and lossy compressors and their configurations")
      (description "LibPressio is a C++ library with C compatible bindings to abstract between different lossless and lossy compressors and their configurations")
      (license #f))))


(define-public libuv-for-neovim
  (let* ((version "1.43.0"))
    (package
     (inherit libuv)
     (version version)
     (source (origin
               (method url-fetch)
               (uri (string-append "https://dist.libuv.org/dist/v" version
                                       "/libuv-v" version ".tar.gz"))
               (sha256
                 (base32
                   "194kwq3jfj9s628kzkchdca534rikjw0xiyas0cjbphqmsvjpmwh")))))))

(define-public lua5.1-luv-for-neovim
  (let* ((version "1.43.0-0"))
    (package
     (inherit lua5.1-luv)
     (version version)
     (source (origin
              ;; The release tarball includes the sources of libuv but does
              ;; not include the pkg-config files.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/luvit/luv")
                    (commit version)))
              (file-name (git-file-name (package-name lua5.1-luv) version))
              (sha256
               (base32
                "1yzi4bm845vl84wyv2qw4z1n1v285lgwm681swmp84brfy2s7czp"))))
     (arguments
       (substitute-keyword-arguments (package-arguments lua5.1-luv)
         ((#:phases phases)
           `(modify-phases ,phases
             (delete 'copy-lua-compat)
             (add-after 'unpack 'copy-lua-compat
               (lambda* (#:key inputs #:allow-other-keys)
                 (copy-recursively (assoc-ref inputs "lua-compat")
                                   "lua-compat")
                 (setenv "CPATH"
                         (string-append (getcwd) "/lua-compat/c-api:"
                                        (or (getenv "CPATH") "")))
                 #t))))))
     (inputs (modify-inputs (package-inputs lua5.1-luv) (replace "libuv" libuv-for-neovim)))
     (native-inputs
       (modify-inputs
         (package-native-inputs lua5.1-luv)
         (replace "lua-compat"
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/keplerproject/lua-compat-5.3")
                   (commit "e245d3a18957e43ef902a59a72c8902e2e4435b9")))
             (file-name "lua-compat-5.3-checkout")
             (sha256
              (base32
                "1caxn228gx48g6kymp9w7kczgxcg0v0cd5ixsx8viybzkd60dcn4")))))))))

(define-public neovim-latest
  (let* ((version "0.7.0"))
    (package
     (inherit neovim)
     (version version)
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neovim/neovim")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name neovim) version))
       (sha256
        (base32 "1m7xmry66pn27gvk7qj9di83xa1h7zjp4c6ygnf218pqhr08x06g"))))
     (inputs
       (modify-inputs
         (package-inputs neovim)
         (replace "lua-luv" lua5.1-luv-for-neovim)
         (replace "libuv" libuv-for-neovim))))))
