(define-module (vup smt)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages multiprecision)
  #:use-module ((guix licenses) #:prefix license:))

(define-public libpoly
  (let ((version "0.1.8"))
    (package
    	(name "libpoly")
    	(version version)
    	(source (origin
          (method git-fetch)
    	  (uri (git-reference
    		(url "https://github.com/SRI-CSL/libpoly")
    		(commit (string-append "v" version))))
    	  (file-name (git-file-name name version))
    	  (sha256
    	   (base32
    		"1n3gijksnl2ybznq4lkwm2428f82423sxq18gnb2g1kiwqlzdaa3"))))
		(inputs `(("python" ,python-2)
				  ("gmp" ,gmp)))
		(build-system cmake-build-system)
		(home-page "https://github.com/SRI-CSL/libpoly")
		(synopsis "C library for manipulating polynomials")
		(description "C library for manipulating polynomials")
		(license license:lgpl3))))

(define-public yices
  (let ((version "2.6.1"))
    (package
    	(name "yices")
    	(version version)
    	(source (origin
          (method git-fetch)
    	  (uri (git-reference
    		(url "https://github.com/SRI-CSL/yices2")
    		(commit (string-append "Yices-" version))))
    	  (file-name (git-file-name name version))
    	  (sha256
    	   (base32
    		"04vf468spsh00jh7gj94cjnq8kjyfwy9l6r4z7l2pm0zgwkqgyhm"))))
		(inputs `(("autoconf" ,autoconf)
				  ("automake" ,automake)
				  ("gmp" ,gmp)
				  ("libpoly" ,libpoly)
				  ("gperf" ,gperf)))
		(arguments
		 `(#:make-flags `("LDCONFIG=true")
		   #:configure-flags `(,(string-append "--with-static-gmp=" (assoc-ref %build-inputs "gmp") "/lib/libgmp.a")
							   ,(string-append "--with-static-gmp-include-dir=" (assoc-ref %build-inputs "gmp") "/include")
							   "--enable-mcsat")
		   #:phases (modify-phases %standard-phases
					  (add-after 'configure 'patch-makefile-SHELL
						(lambda _
						   (patch-makefile-SHELL "Makefile.build")
						   #t)))
		   #:build #f))
		(build-system gnu-build-system)
		(home-page "http://yices.csl.sri.com")
		(synopsis "A high-performance theorem prover and SMT solver")
		(description "A high-performance theorem prover and SMT solver")
		(license license:gpl3))))
