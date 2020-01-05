(define-module (vup python-xyz)
  #:use-module (guix utils)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages astronomy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
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
