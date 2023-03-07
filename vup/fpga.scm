(define-module (vup fpga)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages fpga) #:prefix guix:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages serialization)
  #:use-module (vup prjoxide)
  #:use-module (vup python-xyz))


;; kept in lockstep with yosys upstream for reproducability
(define-public abc-for-yosys
  (let ((commit "be9a35c0363174a7cef21d55ed80d92a9ef95ab1")
        (revision "1"))
    (package
      (inherit guix:abc)
      (version (string-append "0.0-" revision "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/abc")
                      (commit commit)))
                (file-name (git-file-name (package-name guix:abc) version))
                (sha256
                 (base32
                  "05ky82k0ix66d009cf8z5h0xa3f42zkl6dpxaf2z3p2zdy8rdbzv")))))))


(define-public yosys-git
  (let ((commit "3ebc50dee4007f8cca4ffc0e850bc3e86f7641f4")
        (version "0.24+10"))
    ((package-input-rewriting/spec `(("abc" . ,(const abc-for-yosys))))
     (package
       (inherit guix:yosys)
       (version (string-append version "+" (string-take commit 9)))
       (source (origin
                 (inherit (package-source guix:yosys))
                 (uri (git-reference
                       (url "https://github.com/yosyshq/yosys.git")
                       (commit commit)))
                 (sha256
                  (base32
                   "1dpv1zaz00j29pk4fg3fjk7nrfrbqwmw83bd26lw77rjh2gwilnq"))
                 (file-name (git-file-name (package-name guix:yosys) version))))
       (inputs (append (package-inputs guix:yosys) `(("zlib" ,zlib))))))))


(define-public icestorm
  (let ((commit "a545498d6fd0a28a006976293917115037d4628c")
        (revision "8"))
    (package
      (inherit guix:icestorm)
      (version (string-append "0.0-" revision "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/icestorm")
                      (commit commit)))
                (file-name (git-file-name (package-name guix:icestorm) version))
                (sha256
                 (base32
                  "0b2zc5v2b1fqcbpw14jghsvayhijd185gfwfxv241mk2kjk2plz6")))))))

(define-public trellis
  (let ((commit "35f5affe10a2995bdace49e23fcbafb5723c5347"))
    (package
      (name "trellis")
      (version (string-append "1.2-1-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/prjtrellis")
                      (commit commit)
                      (recursive? #t))) ; for prjtrellis-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "00vv9cpw3zdpl6h3al5b4n313prawyii9319gkmhv84npss99way"))))
      (build-system cmake-build-system)
      (inputs `(("python" ,python) ("boost" ,boost)))
      (arguments
       `(#:configure-flags (list (string-append "-DCURRENT_GIT_VERSION=" ,version))
         #:out-of-source? #f
         #:phases (modify-phases %standard-phases
                    (delete 'check) ; no test
                    (add-before 'configure 'pre-configure
                      (lambda* (#:key outputs inputs #:allow-other-keys)
                        (system* "source" "environment.sh")
                        (chdir "libtrellis")
                        #t)))))
      (synopsis "Documentation and bitstream tools for Lattice ECP5 FPGAs")
      (description "Project Trellis documents the Lattice ECP5 architecture
to enable development of open-source tools. Its goal is
to provide sufficient information to develop a free and
open Verilog to bitstream toolchain for these devices.")
      (home-page "https://github.com/symbiflow/prjtrellis")
      (license license:isc))))

(define-public mistral
  (let ((commit "d6bd02cd1eccb4b8f410d074ea96c31966fb1079"))
    (package
      (name "mistral")
      (version (string-append "0.0-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Ravenslofty/mistral")
                      (commit commit)
                      (recursive? #t))) ; for prjtrellis-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "02xxnj36xhhzrgjgw58kdxqk04yw1293m82k9gc7pwl64p1gjhxk"))))
      (build-system cmake-build-system)
      (inputs (list python))
      (arguments `(#:phases (modify-phases %standard-phases
                              (delete 'check))))
      (synopsis "Mistral - A Cyclone V bitstream library")
      (description "It's the very first version of a library/command line utility to compile and decompile Cyclone V bitstreams, as used in the de-10 nano (used in MiSTer) and the future Analogue Pocket.")
      (home-page "https://github.com/Ravenslofty/mistral")
      (license license:bsd-3))))


(define-public python-crcmod
  (package
    (name "python-crcmod")
    (version "1.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "crcmod" version))
        (sha256
          (base32
            "07k0hgr42vw2j92cln3klxka81f33knd7459cn3d8aszvfh52w6w"))))
    (build-system python-build-system)
    (home-page "http://crcmod.sourceforge.net/")
    (synopsis "CRC Generator")
    (description "CRC Generator")
    (license license:expat)))

(define-public python-apycula
  (package
    (name "python-apycula")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Apycula" version))
        (sha256
          (base32
            "1zkh8czmj196rb5r7n0748wpks1l8jq7b9rqd51j1k6bix1kyx7x"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
      `(("python-crcmod" ,python-crcmod)
        ("python-numpy" ,python-numpy)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page "https://github.com/YosysHQ/apicula")
    (synopsis "Open Source tools for Gowin FPGAs")
    (description "Open Source tools for Gowin FPGAs")
    (license #f)))


(define-public nextpnr
  (let ((commit "76fea8268ca7c3f9a9f4f610951f9ea84993e974"))
    (package
      (name "nextpnr")
      (version (string-append "0.4-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/nextpnr")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xqy4qmcb0l5h4n72k50syhpr12spm29g5yydjrhhr36mn7gfcb7"))))
      (build-system cmake-build-system)
      (inputs `(("python" ,python)
                ("boost" ,boost)
                ("qtbase" ,qtbase-5)
                ("trellis" ,trellis)
                ("icestorm" ,icestorm)
                ("prjoxide" ,rust-prjoxide)
                ("apicula" ,python-apycula)
                ("mistral" ,(package-source mistral))
                ("tcl" ,tcl)
                ("zlib" ,zlib)
                ("capnproto" ,capnproto)
                ("pkgconfig" ,pkg-config)
                ("eigen" ,eigen)))
      (arguments
       `(#:configure-flags (list
                            "-DARCH=generic;ice40;ecp5;nexus;gowin;machxo2;mistral" ; TODO(robin): fpga_interchange
                            "-DBUILD_TESTS=ON"
                            "-DUSE_OPENMP=ON"
                            "-DBUILD_GUI=ON"
                            "-DSERIALIZE_CHIPDBS=FALSE" ; high memory requirements
                            (string-append "-DICESTORM_INSTALL_PREFIX=" (assoc-ref %build-inputs "icestorm"))
                            (string-append "-DTRELLIS_INSTALL_PREFIX=" (assoc-ref %build-inputs "trellis"))
                            (string-append "-DMISTRAL_ROOT=" (assoc-ref %build-inputs "mistral"))
                            (string-append "-DOXIDE_INSTALL_PREFIX=" (assoc-ref %build-inputs "prjoxide"))
                            (string-append "-DGOWIN_BBA_EXECUTABLE=" (assoc-ref %build-inputs "apicula") "/bin/gowin_bba"))))
      (synopsis "nextpnr -- a portable FPGA place and route tool")
      (description "nextpnr aims to be a vendor neutral, timing driven, FOSS FPGA place and route tool.")
      (home-page "https://github.com/yosyshq/nextpnr")
      (license license:isc))))

(define-public gtkwave-gtk3
  (package
    (name "gtkwave-gtk3")
    (version "3.3.111")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/gtkwave/"
                                 "gtkwave-gtk3-" version "/"
                                 "gtkwave-gtk3-" version ".tar.gz")
                  (string-append "http://gtkwave.sourceforge.net/"
                                 "gtkwave-gtk3-" version ".tar.gz")))
       (sha256
        (base32 "0cv222qhgldfniz6zys52zhrynfsp5v0h8ia857lng7v33vw5qdl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gperf pkg-config `(,glib "bin")))
    (inputs
     `(("tcl" ,tcl)
       ("tk" ,tk)
       ("gtk+-3" ,gtk+)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-after 'wrap-python 'wrap-glib-or-gtk
          (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))
       #:modules ((guix build gnu-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build glib-or-gtk-build-system))
       #:configure-flags
       (list "--enable-gtk3" "--with-gsettings" 
             (string-append "--with-tcl="
                            (assoc-ref %build-inputs "tcl")
                            "/lib")
             (string-append "--with-tk="
                            (assoc-ref %build-inputs "tk")
                            "/lib"))))

    (synopsis "Waveform viewer for FPGA simulator trace files")
    (description "This package is a waveform viewer for FPGA
simulator trace files (@dfn{FST}).")
    (home-page "http://gtkwave.sourceforge.net/")
    ;; Exception against free government use in tcl_np.c and tcl_np.h.
    (license (list license:gpl2+ license:expat license:tcl/tk))))
