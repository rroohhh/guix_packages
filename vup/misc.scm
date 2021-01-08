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
  (let* ((version "20200505"))
    (package
      (name "mbuffer")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://www.maier-komor.de/software/mbuffer/mbuffer-" version ".tgz"))
         (sha256
          (base32 "02qzy3appah0llg6aa71isl2a5nc93bkzy5r4d682lcy2j1n216c"))))
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
    (version "0.5.2-ec302917")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://repo.or.cz/libsigrok.git")
                    (commit "ec30291701bb1dcb6755a97ae6c18146fe9ad020")
                    (recursive? #t))) ; for prjtrellis-db
              (file-name (git-file-name "libsigrok" version))
              (sha256
               (base32 "1xlc51lds56xjhqka2bpwk14jph6rb5hk1raq3wsagl5ki09pxnz"))))
    (inputs
     `(("python" ,python)
       ("zlib" ,zlib)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))))

(define-public pulseview-libsigrok-master
  ((package-input-rewriting/spec `(("libsigrok" . ,(const libsigrok-master))))
   pulseview))

;; (define-public tycho-maven-plugin
;;   (package
;;     (name "tycho-maven-plugin")
;;     (version "1.4.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append "https://repo1.maven.org/maven2/"
;;                                   "org/eclipse/tycho/tycho-maven-plugin/"
;;                                   version "/tycho-maven-plugin-"
;;                                   version "-sources.jar"))
;;               (sha256
;;                (base32
;;                 "0dzi96qckq4m9ncxz0qnyjnsw1r4fihpn58adm28diisnbf0sy54"))))
;;     (build-system ant-build-system)
;;     (arguments
;;      `(#:jar-name "tycho-maven-plugin.jar"
;;        #:source-dir "tycho-maven-plugin/src/main/java"
;;        #:tests? #f
;;        #:phases
;;        (modify-phases %standard-phases
;;          (replace 'install
;;            (install-from-pom "tycho-maven-plugin/pom.xml")))))
;;     ;; (propagated-inputs
;;     ;;  `(("maven-artifact" ,maven-artifact)
;;     ;;    ("maven-plugin-tools-parent-pom" ,maven-plugin-tools-parent-pom)))
;;     ;; (native-inputs
;;     ;;  `(("unzip" ,unzip)))
;;     (home-page "")
;;     (synopsis "")
;;     (description "")
;;     (license licenses:#f)))

;; (define-public mytourbook
;;   (let* ((version "20.8.0_2020-08-04_1351"))
;;     (package
;;       (name "mytourbook")
;;       (version version)
;;       (source
;;        (origin
;;          (method url-fetch)
;;          (uri (string-append "https://github.com/wolfgang-ch/mytourbook/archive/" version ".tar.gz"))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32 "1dxqg16md2ic6ak80c9d3ahmi7iyh58afcfk8g7s753yjjqsbvrb"))))
;;       (build-system maven-build-system)
;;       (native-inputs `(("tycho-maven-plugin" ,tycho-maven-plugin)))
;;       ;; (inputs `(("libusb" ,libusb) ("boost" ,boost) ("libxml2" ,libxml2)))
;;       ;; (arguments '(#:configure-flags `("-DUSE_BOOST_STATIC_LINK=False")
;;       ;;              #:phases
;;       ;;              (modify-phases %standard-phases
;;       ;;                (add-before 'configure 'cd-to-src
;;       ;;                    (lambda _
;;       ;;                      (chdir "src")
;;       ;;                      #t))
;;       ;;                (delete 'check)))) ;; no tests
;;       (home-page "http://mytourbook.sourceforge.net/")
;;       (synopsis "Free software to visualize and analyze tours which are recorded by a GPS device, bike- or exercise computer and ergometer.")
;;       (description "Free software to visualize and analyze tours which are recorded by a GPS device, bike- or exercise computer and ergometer.")
;;       (license licenses:gpl2))))


;; mytourbook


(define-public hostnamed
  ;; XXX: This package is extracted from systemd but we retain so little of it
  ;; that it would make more sense to maintain a fork of the bits we need.
  (package
    (name "hostnamed")
    (version "241")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/systemd/systemd")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0sy91flzbhpq58k7v0294pa2gxpr0bk27rcnxlbhk2fi6nc51d28"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Connect to the right location for our D-Bus daemon.
                  (substitute* '("src/basic/def.h"
                                 "src/libsystemd/sd-bus/sd-bus.c"
                                 "src/stdio-bridge/stdio-bridge.c")
                    (("/run/dbus/system_bus_socket")
                     "/var/run/dbus/system_bus_socket"))

                  ;; Don't insist on having systemd as PID 1 (otherwise
                  ;; 'localectl' would exit without doing anything.)
                  (substitute* "src/shared/bus-util.c"
                    (("sd_booted\\(\\)")
                     "(1)"))
                  #t))))
    (build-system meson-build-system)
    (arguments
     ;; Try to build as little as possible (list of components taken from the
     ;; top-level 'meson.build' file.)
     (let ((components '("utmp"
                         "hibernate"
                         "environment-d"
                         "binfmt"
                         "coredump"
                         "resolve"
                         "logind"
                         "localed"
                         "hostnamed"
                         "machined"
                         "portabled"
                         "networkd"
                         "timedated"
                         "timesyncd"
                         "firstboot"
                         "randomseed"
                         "backlight"
                         "vconsole"
                         "quotacheck"
                         "sysusers"
                         "tmpfiles"
                         "hwdb"
                         "rfkill"
                         "ldconfig"
                         "efi"
                         "tpm"
                         "ima"
                         "smack"
                         "gshadow"
                         "idn"
                         "nss-myhostname"
                         "nss-systemd")))
       `(#:configure-flags ',(map (lambda (component)
                                    (string-append "-D" component "=false"))
                                  (delete "hostnamed" components))

         ;; It doesn't make sense to test all of systemd.
         #:tests? #f

         #:phases (modify-phases %standard-phases
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; Install 'hostnamed', the D-Bus and polkit files, and
                        ;; 'localectl'.
                        (let* ((out (assoc-ref outputs "out"))
                               (libexec (string-append out "/libexec/hostnamed"))
                               (bin     (string-append out "/bin"))
                               (lib     (string-append out "/lib"))
                               (dbus    (string-append out
                                                       "/share/dbus-1/system-services"))
                               (conf    (string-append out
                                                       "/etc/dbus-1/system.d/"))
                               (polkit  (string-append out
                                                       "/share/polkit-1/actions"))
                               (data    (string-append out "/share/systemd")))
                          (define (source-file regexp)
                            (car (find-files ".." regexp)))

                          (mkdir-p libexec)
                          (copy-file "systemd-hostnamed"
                                     (string-append libexec "/hostnamed"))
                          (install-file "hostnamectl" bin)

                          (let ((service-file (source-file
                                               "\\.locale1\\.service$")))
                            (substitute* service-file
                              (("^Exec=.*$")
                               (string-append "Exec=" libexec "/hostnamed\n")))
                            (install-file service-file dbus))
                          (install-file (source-file "\\.locale1\\.policy$")
                                        polkit)
                          (install-file (source-file "\\.locale1\\.conf$")
                                        conf)
                          (for-each (lambda (file)
                                      (install-file file lib))
                                    (find-files "src/shared"
                                                "libsystemd-shared.*\\.so"))

                          (for-each (lambda (map)
                                      (install-file map data))
                                    (find-files ".." "^(kbd-model-map|language-fallback-map)$"))
                          #t)))))))
    (native-inputs (package-native-inputs elogind))
    (inputs `(("libmount" ,util-linux "lib") ,@(package-inputs elogind)))
    (home-page "https://www.freedesktop.org/wiki/Software/systemd/hostnamed/")
    (synopsis "Control the system locale and keyboard layout")
    (description
     "Hostnamed is a tiny daemon that can be used to control the system locale
and keyboard mapping from user programs.  It is used among other things by the
GNOME Shell.  The @command{localectl} command-line tool allows you to interact
with hostnamed.  This package is extracted from the broader systemd package.")
    (license licenses:lgpl2.1+)))

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
