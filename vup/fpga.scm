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
  #:use-module (gnu packages bison)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages serialization)
  #:use-module (vup prjoxide)
  #:use-module (vup python-xyz))


;; kept in lockstep with yosys upstream for reproducability
(define-public abc-for-yosys
  (let ((commit "f6fa2ddcfc89099726d60386befba874c7ac1e0d")
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
                  "1nskfzwshb77vy90ih9nqji3912xvqfjyhir3azljbx4kwywrfyj")))))))

(define-public iverilog-11
  (package
    (name "iverilog")
    (version "11.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://ftp.icarus.com/pub/eda/verilog/v11/"
                              "verilog-" version ".tar.gz"))
              (sha256
               (base32
                "1mamlrkpb2gb00g7xdddaknrvwi4jr4ng6cfjhwngzk3ddhqaiym"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)
       ("ghostscript" ,ghostscript)))   ; ps2pdf
    (arguments
     `(#:make-flags (list "CC=gcc")))
    (home-page "http://iverilog.icarus.com/")
    (synopsis "FPGA Verilog simulation and synthesis tool")
    (description "Icarus Verilog is a Verilog simulation and synthesis tool.
It operates as a compiler, compiling source code written in Verilog
(IEEE-1364) into some target format.
For batch simulation, the compiler can generate an intermediate form
called vvp assembly.
This intermediate form is executed by @command{vvp}.
For synthesis, the compiler generates netlists in the desired format.")
    ;; GPL2 only because of:
    ;; - ./driver/iverilog.man.in
    ;; - ./iverilog-vpi.man.in
    ;; - ./tgt-fpga/iverilog-fpga.man
    ;; - ./vvp/vvp.man.in
    ;; Otherwise would be GPL2+.
    ;; You have to accept both GPL2 and LGPL2.1+.
    (license (list license:gpl2 license:lgpl2.1+))))


(define-public yosys-git
  (let ((commit "0feba821a8aeeea3f5b027df9badb320cb7dc5fa")
        (version "0.12+57"))
    ((package-input-rewriting/spec `(("abc" . ,(const abc-for-yosys))
                                     ("iverilog" . ,(const iverilog-11))))
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
                   "0q44373bp4r88c1lp5yj9bzjgc4yyw4ddlr1v52kzj10f2pil0fd"))
                 (file-name (git-file-name (package-name guix:yosys) version))))
       (inputs (append (package-inputs guix:yosys) `(("zlib" ,zlib))))))))


(define-public icestorm
  (let ((commit "3b7b1991318860997ef589112b3debb24eb4912d")
        (revision "7"))
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
                  "017hfir4pda22qd3hams60ql8siy651a1gcgc0qvi09fhqz43jsf")))))))

(define-public trellis
  (let ((commit "2f06397673bbca3da11928d538b8ab7d01c944c6"))
    (package
      (name "trellis")
      (version (string-append "1.1-0-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/prjtrellis")
                      (commit commit)
                      (recursive? #t))) ; for prjtrellis-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zwki0p06pfnf2q37lm68vh1kjcbni23lawg0c30676i2a8w1nns"))))
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
    (version "0.2a2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Apycula" version))
        (sha256
          (base32
            "16afs288zrgnmi7rv1730c7g1j7ic8dgsjb0p7rdi9v1c1h6iid5"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
      `(("python-crcmod" ,python-crcmod)
        ("python-numpy" ,python-numpy)
        ("python-openpyxl" ,python-openpyxl)
        ("python-pandas" ,python-pandas-fixed)
        ("python-pillow" ,python-pillow)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page "https://github.com/YosysHQ/apicula")
    (synopsis "Open Source tools for Gowin FPGAs")
    (description "Open Source tools for Gowin FPGAs")
    (license #f)))


(define-public nextpnr
  (let ((commit "3d24583b914bac37d9c22931e6fcee2e1408b284"))
    (package
      (name "nextpnr")
      (version (string-append "0.1-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/nextpnr")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qv315zppjl87cv86mqwdwpg0grxzafy64fal5dw059981vhl15n"))))
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
