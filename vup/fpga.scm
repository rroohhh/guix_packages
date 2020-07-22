(define-module (vup fpga)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages fpga) #:prefix guix:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression))


;; kept in lockstep with yosys upstream for reproducability
(define-public abc-for-yosys
  (let ((commit "341db25668f3054c87aa3372c794e180f629af5d")
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
                  "14cgv34vz5ljkcms6nrv19vqws2hs8bgjgffk5q03cbxnm2jxv5s")))))))


(define-public yosys-git
  (let ((commit "a87e338381507b372bb4ad6fad6ad959e0139649")
        (version "0.9"))
    ((package-input-rewriting/spec `(("abc" . ,(const abc-for-yosys))))
     (package
       (inherit guix:yosys)
       (version (string-append version "+" (string-take commit 9)))
       (source (origin
                 (inherit (package-source guix:yosys))
                 (uri (git-reference
                       (url "https://github.com/cliffordwolf/yosys.git")
                       (commit commit)))
                 (sha256
                  (base32
                   "12ls3dqma8aa8ka026sh5vik5hpaj68i3lfdrc1ia6nqp8srmgfk"))
                 (file-name (git-file-name (package-name guix:yosys) version))))
       (inputs (append (package-inputs guix:yosys) `(("zlib" ,zlib))))))))


(define-public icestorm
  (let ((commit "d12308775684cf43ab923227235b4ad43060015e")
        (revision "2"))
    (package
      (inherit guix:icestorm)
      (version (string-append "0.0-" revision "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/cliffordwolf/icestorm")
                      (commit commit)))
                (file-name (git-file-name (package-name guix:icestorm) version))
                (sha256
                 (base32
                  "18ykv6np8sp7rb7c1cm3ha3qnj280gpkyn476faahb15jh0nbjmw")))))))

(define-public trellis
  (let ((commit "fef7e5fd16354c2911673635dd78e2dae3a775c0"))
    (package
      (name "trellis")
      (version (string-append "1.0-71-g" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/prjtrellis")
                      (commit commit)
                      (recursive? #t))) ; for prjtrellis-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1p44p7zhzf18pzl07hgw0iz045109m2s16nkv91qyd4rx83s3jmr"))))
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


(define-public nextpnr
  (let ((commit "44007eab6f6a7f20c257bb109bbc8f66a5fde12d"))
    (package
      (name "nextpnr")
      (version (string-append "2020.07.16" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/nextpnr")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pgdpgyk65l703hhshp6mmzh23wzcjgndd1q5i5kx8vbgkgyg0bg"))))
      (build-system cmake-build-system)
      (inputs `(("python" ,python)
                ("boost" ,boost)
                ("qtbase" ,qtbase)
                ("trellis" ,trellis)
                ("icestorm" ,icestorm)
                ("eigen" ,eigen)))
      (arguments
       `(#:configure-flags (list
                            "-DARCH=generic;ice40;ecp5"
                            "-DBUILD_TESTS=ON"
                            "-DUSE_OPENMP=ON"
                            "-DSERIALIZE_CHIPDB=ON" ; high memory requirements
                            (string-append "-DICESTORM_INSTALL_PREFIX=" (assoc-ref %build-inputs "icestorm"))
                            (string-append "-DTRELLIS_INSTALL_PREFIX=" (assoc-ref %build-inputs "trellis")))))
      (synopsis "nextpnr -- a portable FPGA place and route tool")
      (description "nextpnr aims to be a vendor neutral, timing driven, FOSS FPGA place and route tool.")
      (home-page "https://github.com/yosyshq/nextpnr")
      (license license:isc))))
