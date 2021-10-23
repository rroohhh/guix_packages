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
  (let ((version "0.1.11"))
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
                  "0qylmg30rklvg00a0h1b3pb52cj9ki98yd27cylihjhq2klh3dmy"))))
      (inputs `(("python" ,python-2)
                ("gmp" ,gmp)))
      (build-system cmake-build-system)
      (home-page "https://github.com/SRI-CSL/libpoly")
      (synopsis "C library for manipulating polynomials")
      (description "C library for manipulating polynomials")
      (license license:lgpl3))))

(define-public cudd
  (let ((version "3.0.0"))
    (package
      (name "cudd")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ivmai/cudd")
                      (commit (string-append "cudd-" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hyw9q42ir92vcaa7bwv6f631n85rfsxp463rnmklniq1wf6dyn9"))))
      (build-system gnu-build-system)
      (arguments `(#:make-flags (list "CFLAGS=-Wall -Wextra -g -O3 -fPIC" "CXXFLAGS=-Wall -Wextra -g -O3 -fPIC" "LDFLAGS=-fPIC")))
      (home-page "https://github.com/ivmai/cudd")
      (synopsis "CUDD: CU Decision Diagram package")
      (description "CUDD: CU Decision Diagram package")
      (license license:bsd-3))))

(define-public yices
  (let ((version "2.6.3"))
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
                  "01fi818lbkwilfcf1dz2dpxkcc1kh8ls0sl5aynyx9pwfn2v03zl"))))
      (inputs `(("autoconf" ,autoconf)
                ("automake" ,automake)
                ("gmp" ,gmp)
                ("libpoly" ,libpoly)
                ("cudd" ,cudd)
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
