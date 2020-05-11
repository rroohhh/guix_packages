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
  (let ((commit "ed90ce20df9c7c4d6e1db5d3f786f9b52e06bab1")
        (revision "1"))
    (package
      (inherit guix:abc)
      (version (string-append "0.0-" revision "-" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/berkeley-abc/abc")
                      (commit commit)))
                (file-name (git-file-name (package-name guix:abc) version))
                (sha256
                 (base32
                  "01sw67pkrb6wzflkxbkxzwsnli3nvp0yxwp3j1ngb3c0j86ri437")))))))


(define-public yosys-git
  (let ((commit "868b6b1b0dfa7ca1f10392678d7c3c29db37c60d")
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
                   "0p3dp2jb669m17vcjqsrcv9g3y9vxhhfa1j1v46yvmdalh5vc4zm"))
                 (file-name (git-file-name (package-name guix:yosys) version))))
       (inputs (append (package-inputs guix:yosys) `(("zlib" ,zlib))))))))


(define-public icestorm
  (let ((commit "cd2610e0fa1c6a90e8e4e4cfe06db1b474e752bb")
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
                  "05ckmmvgymr7vhrpnqsiafwm8z5rhc3h91v506lzi6jpjzcs23hj")))))))

(define-public trellis
  (let ((commit "d4760738c653338114d62204ace87a57c8f774e0"))
    (package
      (name "trellis")
      (version (string-append "1.0-71-g" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/symbiflow/prjtrellis")
                      (commit commit)
                      (recursive? #t))) ; for prjtrellis-db
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1nya4axblppzmmf1p3agln1q4nrl22n0pj0bybqyixyg5iwv1f9m"))))
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
  (let ((commit "5c6b2cbafef7435bd697cedf30436bf16e70dc15"))
    (package
      (name "nextpnr")
      (version (string-append "2020.04.26" (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yosyshq/nextpnr")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1h3whykj63q7hp2kf46p0v6wr9dayiz5wairwhd9za6739lycngh"))))
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
                            (string-append "-DICEBOX_ROOT=" (assoc-ref %build-inputs "icestorm") "/share/icebox")
                            (string-append "-DTRELLIS_INSTALL_PREFIX=" (assoc-ref %build-inputs "trellis")))))
                                        ;                           (string-append "-DPYTRELLIS_LIBDIR=" (assoc-ref %build-inputs "trellis") "/lib/trellis"))))
      (synopsis "nextpnr -- a portable FPGA place and route tool")
      (description "nextpnr aims to be a vendor neutral, timing driven, FOSS FPGA place and route tool.")
      (home-page "https://github.com/yosyshq/nextpnr")
      (license license:isc))))
