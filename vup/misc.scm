(define-module (vup misc)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system maven)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages boost))

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
