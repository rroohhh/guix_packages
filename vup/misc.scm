(define-module (vup misc)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix download)
  #:use-module (guix utils)
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
  #:use-module (gnu packages man)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages engineering)
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
  #:use-module (gnu packages samba)
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
    (version "1.5.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/fwupd/fwupd")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrxp3hl1sm0gb1200qwyb330pknxicf5p0x5vgdv8ha4jf9zygc"))
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
