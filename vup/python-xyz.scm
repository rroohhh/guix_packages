(define-module (vup python-xyz)
  #:use-module (guix utils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages check)
  #:use-module (vup fpga)
  #:use-module (vup smt)
  #:use-module (nonfree packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public python-bitarray
  (package
	(name "python-bitarray")
	(version "1.0.1")
	(source
	 (origin
	   (method url-fetch)
	   (uri (pypi-uri "bitarray" version))
	   (sha256
        (base32
		 "1810zxflkyqc7yh0d13g02ngpw6vjl0yxjrg8wa9xqfdp7w01d9y"))))
	(build-system python-build-system)
	(home-page
	 "https://github.com/ilanschnell/bitarray")
	(synopsis
	 "efficient arrays of booleans -- C extension")
	(description
	 "efficient arrays of booleans -- C extension")
	(license #f)))

(define-public python-pyvcd
  (package
	(name "python-pyvcd")
	(version "0.1.4")
	(source
	 (origin
	   (method url-fetch)
	   (uri (pypi-uri "pyvcd" version))
	   (sha256
        (base32
		 "0dv9wac5y5z9j54ypyc59csxdiy9ybpphw9ipxp1k8nfg65q9jxx"))))
	(build-system python-build-system)
	(propagated-inputs `(("python-six" ,python-six) ("python-setuptools-scm" ,python-setuptools-scm)))
	(home-page
	 "http://pyvcd.readthedocs.io/en/latest/")
	(synopsis "Python VCD file support.")
	(description "Python VCD file support.")
	(license license:expat)))

(define-public python-fitsio
  (package
	(name "python-fitsio")
	(version "1.0.5")
	(source
	 (origin
	   (method url-fetch)
	   (uri (pypi-uri "fitsio" version))
	   (sha256
        (base32
		 "0njd7akc9ba847mkbjks0k0bda3p1zf125bz029g8cv747cchnnv"))))
	(build-system python-build-system)
	(inputs `(("cfitsio" ,cfitsio)))
	(propagated-inputs
	 `(("python-numpy" ,python-numpy)))
	(home-page "https://github.com/esheldon/fitsio")
	(synopsis
	 "A full featured python library to read from and write to FITS files.")
	(description
	 "A full featured python library to read from and write to FITS files.")
	(license license:gpl3)
	(arguments
	 '(#:phases
	   (modify-phases %standard-phases
		 (add-before 'patch-source-shebangs 'patch-bin-sh
		   (lambda _
			 (substitute* '("cfitsio3430/Makefile.in"
							"cfitsio3430/configure")
			   (("/bin/sh") (which "sh")))
			 #t)))))))

(define-public python-orthopy
  (package
	(name "python-orthopy")
	(version "0.6.1")
	(source
	 (origin
	   (method url-fetch)
	   (uri (pypi-uri "orthopy" version))
	   (sha256
        (base32
		 "03k103jwncfnvjf6fpv2kfv9shff1hlws8msczm2s2h5g9x2fmcp"))))
	(build-system python-build-system)
	(propagated-inputs
	 `(("python-numpy" ,python-numpy)
	   ("python-scipy" ,python-scipy)
	   ("python-sympy" ,python-sympy)))
	(home-page "https://github.com/nschloe/orthopy")
	(synopsis
	 "Tools for orthogonal polynomials, Gaussian quadrature")
	(description
	 "Tools for orthogonal polynomials, Gaussian quadrature")
	(license #f)
	(arguments '(#:phases
				 (modify-phases %standard-phases
				   (delete 'check))))))

(define-public python-quadpy
  (package
	(name "python-quadpy")
	(version "0.13.2")
	(source
	 (origin
	   (method url-fetch)
	   (uri (pypi-uri "quadpy" version))
	   (sha256
        (base32
		 "0c69g21p7dw92bd29flxv8lj732vm82jbapv9q1xlw2z76ban7xa"))))
	(build-system python-build-system)
	(propagated-inputs
	 `(("python-numpy" ,python-numpy)
	   ("python-orthopy" ,python-orthopy)
	   ("python-scipy" ,python-scipy)
	   ("python-sympy" ,python-sympy)))
	(home-page "https://github.com/nschloe/quadpy")
	(synopsis
	 "Numerical integration, quadrature for various domains")
	(description
	 "Numerical integration, quadrature for various domains")
	(license #f)
	(arguments '(#:phases
				 (modify-phases %standard-phases
				   (delete 'check))))))

(define-public python-ordereddict
  (package
    (name "python-ordereddict")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ordereddict" version))
        (sha256
          (base32
            "07qvy11nvgxpzarrni3wrww3vpc9yafgi2bch4j2vvvc42nb8d8w"))))
    (build-system python-build-system)
    (home-page "UNKNOWN")
    (synopsis
      "A drop-in substitute for Py2.7's new collections.OrderedDict that works in Python 2.4-2.6.")
    (description
      "A drop-in substitute for Py2.7's new collections.OrderedDict that works in Python 2.4-2.6.")
    (license #f)))

(define-public python-funcsigs
  (package
    (name "python-funcsigs")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "funcsigs" version))
        (sha256
          (base32
            "0l4g5818ffyfmfs1a924811azhjj8ax9xd1cffr1mzd3ycn0zfx7"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-ordereddict" ,python-ordereddict)))
    (home-page "http://funcsigs.readthedocs.org")
	(arguments
	 `(#:phases
	   (modify-phases %standard-phases
					  (delete 'check))))
    (synopsis
      "Python function signatures from PEP362 for Python 2.6, 2.7 and 3.2+")
    (description
      "Python function signatures from PEP362 for Python 2.6, 2.7 and 3.2+")
    (license #f)))

(define-public python-pint
  (package
    (name "python-pint")
    (version "0.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Pint" version))
        (sha256
          (base32
            "1qp43xb8m9hhk1yi4ibdla0wx7b78avv65009hcq2krzsslskn1j"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-funcsigs" ,python-funcsigs)))
    (home-page "https://github.com/hgrecco/pint")
    (synopsis "Physical quantities module")
    (description "Physical quantities module")
    (license license:bsd-3)))

(define-public python-colorhash
  (package
    (name "python-colorhash")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://files.pythonhosted.org/packages/bf/e9/b1d948c518ac6f683711a38a9c5bb84597f417ac2fc9296a49c40c1b2dab/colorhash-1.0.2.tar.bz2")
        (sha256
          (base32
            "0r777ry4c8d24j7g7p8b95smnfr64liljfc0zvjxcpidv31jbyg0"))))
    (build-system python-build-system)
    (home-page
      "https://bitbucket.org/fk/python-color-hash")
    (synopsis "Generate a color based on a value")
    (description "Generate a color based on a value")
    (license license:expat)))

(define-public python-arpeggio
  (package
    (name "python-arpeggio")
    (version "1.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Arpeggio" version))
        (sha256
          (base32
            "0aq2pmhfyq7vhbhyq8jgxiphncy1s79rmrsggz4p52m4cdhy134l"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/textX/Arpeggio")
    (synopsis "Packrat parser interpreter")
    (description "Packrat parser interpreter")
    (license license:expat)
	(arguments
	 '(#:phases
	   (modify-phases %standard-phases
		 (delete 'check))))))

(define-public python-textx
  (package
    (name "python-textx")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "textX" version))
        (sha256
          (base32
            "075ngfrpm322azsd3136p5y6k3f41g94r94znc70j7s0k07p370m"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-arpeggio" ,python-arpeggio)
        ("python-click" ,python-click)))
    (home-page "https://github.com/textX/textX")
    (synopsis
      "Meta-language for DSL implementation inspired by Xtext")
    (description
      "Meta-language for DSL implementation inspired by Xtext")
    (license license:expat)
	(arguments
	 '(#:phases
	   (modify-phases %standard-phases
		 (delete 'check))))))

(define (make-linux-module-builder linux)
  (package
    (inherit linux)
    (name (string-append (package-name linux) "-module-builder"))
    (native-inputs
     `(("linux" ,linux)
       ,@(package-native-inputs linux)))
    (arguments
     (substitute-keyword-arguments (package-arguments linux)
      ((#:phases phases)
       `(modify-phases ,phases
          (replace 'build
            (lambda _
              (invoke "make" "modules_prepare")))
          (delete 'strip) ; faster.
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (out-lib-build (string-append out "/lib/modules/build")))
                ; TODO: Only preserve the minimum, i.e. [Kbuild], Kconfig, scripts, include, ".config".
                (copy-recursively "." out-lib-build)
                (let* ((linux (assoc-ref inputs "linux")))
                  (install-file (string-append linux "/System.map")
                                out-lib-build)
                  (let ((source (string-append linux "/Module.symvers")))
                    (if (file-exists? source)
                        (install-file source out-lib-build))))
                #t)))))))))

(define-public python-chipsec
  (package
    (name "python-chipsec")
    (version "1.4.7")
    (source
      (origin
        (method git-fetch)
		(uri (git-reference
			  (url "https://github.com/chipsec/chipsec")
			  (commit "1.4.7")))
		(file-name (git-file-name name version))
        (sha256
          (base32
            "11qi4m4hqkylf1wd7f921r0p7xg5prpmfkmb7l9nn7sb95zz0sjr"))))
    (build-system python-build-system)
	(inputs
	 `(("linux" ,linux-nonfree)
	   ("nasm" ,nasm)
	   ("linux-module-builder" ,(make-linux-module-builder linux-nonfree))))
    (home-page "https://github.com/chipsec/chipsec")
    (synopsis
      "CHIPSEC: Platform Security Assessment Framework")
    (description
      "CHIPSEC: Platform Security Assessment Framework")
    (license #f)
	(arguments
	 `(#:phases
	   (modify-phases %standard-phases
		 (add-after 'unpack 'set-kernel-src-dir
			 (lambda* (#:key inputs #:allow-other-keys)
			   (let* ((kernel-dir (assoc-ref inputs "linux-module-builder"))
					  (kernel-src-dir (string-append kernel-dir "/lib/modules/build")))
				 (setenv "KERNEL_SRC_DIR" kernel-src-dir)))))))))

(define-public python-nmigen
  (package
    (name "python-nmigen")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nmigen" version))
        (sha256
          (base32
            "177y9pq6389wswiag4v7w2x9vjg42l8f1srkighf8vpvf2axmxn8"))))
    (build-system python-build-system)
	(inputs `(("yosys" ,yosys-git)
			  ("symbiyosys" ,symbiyosys)))
    (propagated-inputs
      `(("python-jinja2" ,python-jinja2)
        ("python-pyvcd" ,python-pyvcd)
        ("python-setuptools" ,python-setuptools)))
    (home-page "")
    (synopsis
      "Python toolbox for building complex digital hardware")
    (description
      "Python toolbox for building complex digital hardware")
    (license license:bsd-3)))

(define-public python-nmigen-boards
  (package
    (name "python-nmigen-boards")
    (version "0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nmigen-boards" version))
        (sha256
          (base32
            "1zzq6kh1aycrxszbdcwmyhhpnivmfa4l5jvvkcf3kyk9qv5c05zf"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-nmigen" ,python-nmigen)
        ("python-setuptools" ,python-setuptools)))
    (home-page "")
    (synopsis
      "Board and connector definitions for nMigen")
    (description
      "Board and connector definitions for nMigen")
    (license license:bsd-3)))

(define-public python-nmigen-stdio
  (package
    (name "python-nmigen-stdio")
    (version "0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nmigen-stdio" version))
        (sha256
          (base32
            "0xv356lpwd3cb8cv70l0jgbz775pd7hk8ad2rzp92pysiykcnq08"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-nmigen" ,python-nmigen)))
    (home-page "")
    (synopsis "Industry standard I/O for nMigen")
    (description "Industry standard I/O for nMigen")
    (license license:bsd-3)))

(define-public python-nmigen-soc
  (package
    (name "python-nmigen-soc")
    (version "0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nmigen-soc" version))
        (sha256
          (base32
            "0bgf9kar86nd70c0dizxf3l3rcrrddyw9kg9r8cn36ir6rkj4az4"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-nmigen" ,python-nmigen)))
    (home-page "")
    (synopsis "System on Chip toolkit for nMigen")
    (description "System on Chip toolkit for nMigen")
    (license license:bsd-3)))

(define-public python-varname
  (package
    (name "python-varname")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-varname" version))
        (sha256
          (base32
            "044wz39rbd60qy9f27v179q98c1sbjcba2rf71g9m475ls3m5vqh"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/pwwang/python-varname")
    (synopsis
      "Retrieving variable names of function or class calls.")
    (description
      "Retrieving variable names of function or class calls.")
    (license license:expat)))

(define-public symbiyosys
  (let ((commit "0a7013017f9d583ef6cc8d10712f4bf11cf6e024"))
    (package
	  (name "symbiyosys")
	  (version (string-append "2020.02.20-" (string-take commit 9)))
      (source
        (origin
          (method git-fetch)
		  (uri (git-reference
				(url "https://github.com/YosysHQ/SymbiYosys")
				(commit commit)))
		  (file-name (git-file-name name version))
		  (sha256
		   (base32
			"08xz8sgvs1qy7jxp8ma5yl49i6nl7k6bkhry4afdvwg3fvwis39c"))))
	  (inputs `(("python" ,python)
				("yosys" ,yosys-git)))
	  (propagated-inputs `(("yices" ,yices)))
	  (arguments
	   `(#:make-flags `(,(string-append "PREFIX=" %output))
		 #:phases (modify-phases %standard-phases
					(add-before 'install 'patch-yosys
					  (lambda* (#:key inputs outputs #:allow-other-keys)
						(let ((out (assoc-ref outputs "out"))
							  (yosys (assoc-ref inputs "yosys")))
						  (substitute* '("sbysrc/sby.py" "sbysrc/sby_core.py")
						 	(("##yosys-sys-path##") (string-append "sys.path += [p + \"/share/yosys/python3/\" for p in [\"" out "\", \"" yosys "\"]]"))
							(("/usr/bin/env\", \"bash") (which "bash"))))
						#t))
					(delete 'configure) ; no configure
					(delete 'build)     ; no compilation
					(delete 'check))))  ; no tests
	  (build-system gnu-build-system)
	  (home-page "https://github.com/YosysHQ/SymbiYosys")
	  (synopsis "SymbiYosys (sby) -- Front-end for Yosys-based formal verification flows")
	  (description "SymbiYosys (sby) is a front-end driver program for Yosys-based formal hardware verification flows.")
	  (license license:isc))))
