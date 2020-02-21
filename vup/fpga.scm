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


; kept in lockstep with yosys upstream for reproducability
(define-public abc-for-yosys
  (let ((commit "71f2b40320127561175ad60f6f2428f3438e5243")
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
				"15sn146ajxql7l1h8rsag5lhn4spwvgjhwzqawfr78snzadw8by3")))))))


(define-public yosys-git
  (let ((commit "cd044a2bb6adf7a5e00d4a6c075e9489d852d733")
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
			   "05gmzlbkj8z13yqg6jx2i8015yd8dqn5lpjlsd55rwvmjjizh65c"))
			 (file-name (git-file-name (package-name guix:yosys) version))))
	(inputs (append (package-inputs guix:yosys) `(("zlib" ,zlib))))))))


(define-public icestorm
  (let ((commit "0ec00d892a91cc68e45479b46161f649caea2933")
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
				"1qlh99fafb7xga702k64fmc9m700nsddrfgcq4x8qn8fplsb64f1")))))))

(define-public boost-python3
   (package
	 (inherit boost)
	 (native-inputs
	  `(("perl" ,perl)
		("python" ,python)
		("tcsh" ,tcsh)))
	 (arguments
       (substitute-keyword-arguments (package-arguments boost)
		 ((#:phases phases)
		 `(modify-phases ,phases
			(replace 'configure
			  (lambda* (#:key inputs outputs #:allow-other-keys)
				(let ((icu (assoc-ref inputs "icu4c"))
					  (python (assoc-ref inputs "python"))
					  (out (assoc-ref outputs "out")))
				  (substitute* '("libs/config/configure"
								 "libs/spirit/classic/phoenix/test/runtest.sh"
								 "tools/build/src/engine/execunix.c"
								 "tools/build/src/engine/Jambase"
								 "tools/build/src/engine/jambase.c")
					(("/bin/sh") (which "sh")))

				  (setenv "SHELL" (which "sh"))
				  (setenv "CONFIG_SHELL" (which "sh"))
				  (setenv "CPATH" (string-append (getenv "CPATH") ":" python "/include/python3.7m")) ;  very shit but what can i do

				  (invoke "./bootstrap.sh"
						  (string-append "--prefix=" out)
						  ;; Auto-detection looks for ICU only in traditional
						  ;; install locations.
						  (string-append "--with-icu=" icu)
						  "--with-toolset=gcc"
						  (string-append "--with-python=" python "/bin/python3")))))
			(replace 'provide-libboost_python
			  (lambda* (#:key outputs #:allow-other-keys)
				(let ((out (assoc-ref outputs "out")))
				  ;; Boost can build support for both Python 2 and Python 3 since
				  ;; version 1.67.0, and suffixes each library with the Python
				  ;; version.  Many consumers only check for libboost_python
				  ;; however, so we provide it here as suggested in
				  ;; <https://github.com/boostorg/python/issues/203>.
				  (with-directory-excursion (string-append out "/lib")
					(symlink "libboost_python37.so" "libboost_python.so"))
				  #t)))))))))

(define-public trellis
  (let ((commit "4e4b95c8e03583d48d76d1229f9c7825e2ee5be1"))
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
				"0p8yqdm9z39ny3brnnrajq7pw46vbrny1syxxig0m7hw41k4lb4f"))))
	 (build-system cmake-build-system)
	 (inputs `(("python" ,python) ("boost" ,boost-python3)))
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
  (let ((commit "affb12cc27ebf409eade062c4c59bb98569d8147"))
	(package
	 (name "nextpnr")
	 (version (string-append "2020.02.20-" (string-take commit 9)))
	 (source (origin
			  (method git-fetch)
			  (uri (git-reference
					(url "https://github.com/yosyshq/nextpnr")
					(commit commit)))
			  (file-name (git-file-name name version))
			  (sha256
			   (base32
				"0ap7rgrkg5lzvmmm5x55l8gwm9zn2h9011hmafjn2x3778vk2pmq"))))
	 (build-system cmake-build-system)
	 (inputs `(("python" ,python)
			   ("boost" ,boost-python3)
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
;						   (string-append "-DPYTRELLIS_LIBDIR=" (assoc-ref %build-inputs "trellis") "/lib/trellis"))))
	 (synopsis "nextpnr -- a portable FPGA place and route tool")
	 (description "nextpnr aims to be a vendor neutral, timing driven, FOSS FPGA place and route tool.")
	 (home-page "https://github.com/yosyshq/nextpnr")
	 (license license:isc))))
