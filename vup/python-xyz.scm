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
