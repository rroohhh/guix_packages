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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages serialization)
  #:use-module (vup prjoxide))


;; kept in lockstep with yosys upstream for reproducability
(define-public abc-for-yosys
  (let ((commit "4f5f73d18b137930fb3048c0b385c82fa078db38")
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
                  "0z1kp223kix7i4r7mbj2bzawkdzc55nsgc41m85dmbajl9fsj1m0")))))))


(define-public yosys-git
  (let ((commit "e2c9580024563be385ac9e892a978be3384990a8")
        (version "0.9+4081"))
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
                   "0328sypjf8nw5235h09b81f4j1wbgh586gmz258ivk7q0qqqqisi"))
                 (file-name (git-file-name (package-name guix:yosys) version))))
       (inputs (append (package-inputs guix:yosys) `(("zlib" ,zlib))))))))


(define-public icestorm
  (let ((commit "c495861c19bd0976c88d4964f912abe76f3901c3")
        (revision "6"))
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
                  "1r98scq08kk5lspz53makagjac8f0scmrjyns13srz6z39bq46vw")))))))

(define-public trellis
  (let ((commit "0e6a3204aa418a5ba3ad1030f9bb8cc359fc0158"))
    (package
      (name "trellis")
      (version (string-append "1.0-73-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/prjtrellis")
                      (commit commit)
                      (recursive? #t))) ; for prjtrellis-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kixwh53v2njhrg6pdl1ha6n9zpkcbxlld378wfqyg6g5vphqwda"))))
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
    (version "0.0.1a8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Apycula" version))
        (sha256
          (base32
            "1l0cydqk00zr1acbznfm3zxclhxj2sn0ji67rbklk2p70g5ak7ay"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
      `(("python-crcmod" ,python-crcmod)
        ("python-numpy" ,python-numpy)
        ("python-openpyxl" ,python-openpyxl)
        ("python-pandas" ,python-pandas)
        ("python-pillow" ,python-pillow)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page "https://github.com/YosysHQ/apicula")
    (synopsis "Open Source tools for Gowin FPGAs")
    (description "Open Source tools for Gowin FPGAs")
    (license #f)))


(define-public nextpnr
  (let ((commit "0f9a88b2cd84c561df11690c07af12373bf0941f"))
    (package
      (name "nextpnr")
      (version (string-append "2021.06.17-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/nextpnr")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mdf5kg100pd6jxqyzx6yfdzgv1c3fnxypdkwhnc9c04pix3qzl2"))))
      (build-system cmake-build-system)
      (inputs `(("python" ,python)
                ("boost" ,boost)
                ("qtbase" ,qtbase-5)
                ("trellis" ,trellis)
                ("icestorm" ,icestorm)
                ("prjoxide" ,rust-prjoxide)
                ("apicula" ,python-apycula)
                ("tcl" ,tcl)
                ("zlib" ,zlib)
                ("capnproto" ,capnproto)
                ("pkgconfig" ,pkg-config)
                ("eigen" ,eigen)))
      (arguments
       `(#:configure-flags (list
                            "-DARCH=generic;ice40;ecp5;nexus;gowin;machxo2"
                            "-DBUILD_TESTS=ON"
                            "-DUSE_OPENMP=ON"
                            "-DBUILD_GUI=ON"
                            "-DSERIALIZE_CHIPDBS=FALSE" ; high memory requirements
                            (string-append "-DICESTORM_INSTALL_PREFIX=" (assoc-ref %build-inputs "icestorm"))
                            (string-append "-DTRELLIS_INSTALL_PREFIX=" (assoc-ref %build-inputs "trellis"))
                            (string-append "-DOXIDE_INSTALL_PREFIX=" (assoc-ref %build-inputs "prjoxide"))
                            (string-append "-DGOWIN_BBA_EXECUTABLE=" (assoc-ref %build-inputs "apicula") "/bin/gowin_bba"))))
      (synopsis "nextpnr -- a portable FPGA place and route tool")
      (description "nextpnr aims to be a vendor neutral, timing driven, FOSS FPGA place and route tool.")
      (home-page "https://github.com/yosyshq/nextpnr")
      (license license:isc))))
