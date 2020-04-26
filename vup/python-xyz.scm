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
  #:use-module (vup linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public python-pyvcd
  (package
    (name "python-pyvcd")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvcd" version))
       (sha256
        (base32
         "1aw3y841jk50bb62hkk51860a97rl5jz9n9zlil9j138rpibkmps"))))
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
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fitsio" version))
       (sha256
        (base32
         "1an0jy70a400y10y8n0ci3jdw6yfh2k42fqwj6kk9vfqz4a85f22"))))
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
             (substitute* '("cfitsio3470/Makefile.in"
                            "cfitsio3470/configure")
               (("/bin/sh") (which "sh")))
             #t)))))))

(define-public python-setuptools42
  (package
    (inherit python-setuptools)
    (version "42.0.2")
    (source
     (origin
       (inherit (package-source python-setuptools))
       (uri (pypi-uri "setuptools" version ".zip"))
       (sha256
        (base32
         "0h5rsh8cpq9mh6kzr5xgcmyqyl8ym5r6i9m6g770k1vw1l4p5cy5"))))))

(define-public python-orthopy
  (package
    (name "python-orthopy")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "orthopy" version))
       (sha256
        (base32
         "1c8j9qyzjijf75pxw4bwc3hlaikz8wyvajr512yg3cv8xwlkpvcs"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-sympy" ,python-sympy)
       ("python-wheel" ,python-wheel)
       ("python-setuptools" ,python-setuptools42)))
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
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "quadpy" version))
       (sha256
        (base32
         "13fpl8wpdaxan05w8s7hd2hgvnvp7lhdqh42chakxss54blgyxka"))))
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

(define-public python-xarray
  (package
    (name "python-xarray")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xarray" version))
       (sha256
        (base32
         "1yx8j66b7rn10m2l6gmn8yr9cn38pi5cj0x0wwpy4hdnhy6i7qv4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-setuptools" ,python-setuptools42)
       ("python-wheel" ,python-wheel)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (arguments
     `(#:phases (modify-phases %standard-phases (delete 'check)))) ; no tests
    (home-page "https://github.com/pydata/xarray")
    (synopsis
     "N-D labeled arrays and datasets in Python")
    (description
     "N-D labeled arrays and datasets in Python")
    (license #f)))

(define-public python-sparse
  (package
    (name "python-sparse")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sparse" version))
       (sha256
        (base32
         "04gfwm1y9knryx992biniqa3978n3chr38iy3y4i2b8wy52fzy3d"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numba" ,python-numba)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-black" ,python-pytest-black)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/pydata/sparse/")
    (synopsis "Sparse n-dimensional arrays")
    (description "Sparse n-dimensional arrays")
    (license #f)))

(define-public python-pint
  (package
    (name "python-pint")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pint" version))
       (sha256
        (base32
         "0kfgnmcs6z9ndhzvwg2xzhpwxgyyagdsdz5dns1jy40fa1q113rh"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-funcsigs" ,python-funcsigs)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-sparse" ,python-sparse)
       ("python-xarray" ,python-xarray)
       ("python-pytest" ,python-pytest)))
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
    (version "1.4.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chipsec/chipsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1p6w8294w5z2f4jwc22mqaggv5qajvmf9iifv7fl7wdz3wsvskrk"))))
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

(define (setuptools-scm-version-setter v)
  `(lambda* (#:key inputs #:allow-other-keys)
     (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,v)
     #t))

(define-public python-nmigen
  (let ((commit "ba79b0cdc67505ac24f0a06fbd449b952aa33247"))
    (package
      (name "python-nmigen")
      (version (string-append "0.2+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nmigen/nmigen")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "195lp7w721s4brj5aps2i256rhgd786ib589gmhvz9f0vii03f87"))))
      (build-system python-build-system)
      (inputs `(("yosys" ,yosys-git)
                ("symbiyosys" ,symbiyosys)))
      (propagated-inputs
       `(("python-jinja2" ,python-jinja2)
         ("python-pyvcd" ,python-pyvcd)
         ("python-setuptools" ,python-setuptools)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "")
      (synopsis
       "Python toolbox for building complex digital hardware")
      (description
       "Python toolbox for building complex digital hardware")
      (license license:bsd-3))))


(define-public python-nmigen-boards
  (let ((commit "f6b28cef8d92b3f2beaf171f16867999989e388b"))
    (package
      (name "python-nmigen-boards")
      (version (string-append "0.0+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nmigen/nmigen-boards")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1izlhaqsmpwnabsnzzxpmlapggsf9sfzqfsx0k8wdpl3855jpb67"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-nmigen" ,python-nmigen)
         ("python-setuptools" ,python-setuptools)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "")
      (synopsis
       "Board and connector definitions for nMigen")
      (description
       "Board and connector definitions for nMigen")
      (license license:bsd-3))))

(define-public python-nmigen-stdio
  (let ((commit "b5ff8b8f8b7a77b6116607a07c461bcd21018539"))
    (package
      (name "python-nmigen-stdio")
      (version (string-append "0.0+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nmigen/nmigen-stdio")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1pxj6msf1m4if5wpp85slin7syq66ksjg98i0gjmmzp04w36jhna"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-nmigen" ,python-nmigen)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "")
      (synopsis "Industry standard I/O for nMigen")
      (description "Industry standard I/O for nMigen")
      (license license:bsd-3))))

(define-public python-nmigen-soc
  (let ((commit "425692af0554bb1d9df7a593823f1fa23a3cdb19"))
    (package
      (name "python-nmigen-soc")
      (version (string-append "0.0+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nmigen/nmigen-soc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "17mpmj33kzg513lsk798bnh69snw61mqshlmfgdf6jy459s39k48"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-nmigen" ,python-nmigen)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "")
      (synopsis "System on Chip toolkit for nMigen")
      (description "System on Chip toolkit for nMigen")
      (license license:bsd-3))))

(define-public python-executing
  (package
    (name "python-executing")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "executing" version))
        (sha256
          (base32
            "1f00yzljlyd4j3iajnhqvjm2n1vkkcwg4cinhvzj9fiyc0rafcm2"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/alexmojaki/executing")
    (synopsis
      "Get the currently executing AST node of a frame, and other information")
    (description
      "Get the currently executing AST node of a frame, and other information")
    (license license:expat)))

(define-public python-varname
  (package
    (name "python-varname")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-varname" version))
       (sha256
        (base32
         "12glk17k11apva7n92knqplhvfbvmsjgg1ydcrlsgxr2p0nkx8pd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-executing" ,python-executing)))
    (home-page
     "https://github.com/pwwang/python-varname")
    (synopsis
     "Retrieving variable names of function or class calls.")
    (description
     "Retrieving variable names of function or class calls.")
    (license license:expat)))

(define-public symbiyosys
  (let ((commit "b26d4e63629355a4c51220662dafdf81839ce1fe"))
    (package
      (name "symbiyosys")
      (version (string-append "2020.04.26-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/SymbiYosys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0iiiw72nwk19nw2y6n4b9zfqh2f0k9bw648bhpkg998k0wxldv2q"))))
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

(define-public python-skyfield
  (package
    (name "python-skyfield")
    (version "1.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skyfield" version))
       (sha256
        (base32
         "0lm4j6zdavvw0wgfv5g2rdk77n3dh2b096z44halfcjcf4k4dg2x"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (home-page
     "http://github.com/brandon-rhodes/python-skyfield/")
    (synopsis "Elegant astronomy for Python")
    (description "Elegant astronomy for Python")
    (license license:expat)))

(define-public python-astropy
  (package
    (name "python-astropy")
    (version "4.0.1.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy" version))
       (sha256
        (base32
         "1da4xj793ldck29aajyb514wpz330cml26f3gdp45jj531n4lc2w"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-ipython" ,python-ipython)
       ("python-objgraph" ,python-objgraph)
       ("python-skyfield" ,python-skyfield)))
    (home-page "http://astropy.org")
    (synopsis
     "Community-developed python astronomy tools")
    (description
     "Community-developed python astronomy tools")
    (license #f)))

(define-public python-toposort
  (package
    (name "python-toposort")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "toposort" version))
       (sha256
        (base32
         "1papqmv5930xl3d5mx2drnwdxg7y1y3l1ij2n0vvzqwnaa2ax9fv"))))
    (build-system python-build-system)
    (home-page
     "https://bitbucket.org/ericvsmith/toposort")
    (synopsis
     "Implements a topological sort algorithm.")
    (description
     "Implements a topological sort algorithm.")
    (license #f)))

(define-public python-sexpdata
  (package
    (name "python-sexpdata")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sexpdata" version))
       (sha256
        (base32
         "1q4lsjyzzqrdv64l0pv4ij9nd8gqhvxqcrpxc2xpxs652sk2gj0s"))))
    (build-system python-build-system)
    (home-page "https://github.com/tkf/sexpdata")
    (synopsis "S-expression parser for Python")
    (description "S-expression parser for Python")
    (license license:bsd-3)))

(define-public python-epc
  (package
    (name "python-epc")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "epc" version))
       (sha256
        (base32
         "09bx1ln1bwa00917dndlgs4k589h8qx2x080xch5m58p92kjwkd1"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sexpdata" ,python-sexpdata)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/tkf/python-epc")
    (synopsis
     "EPC (RPC stack for Emacs Lisp) implementation in Python")
    (description
     "EPC (RPC stack for Emacs Lisp) implementation in Python")
    (license #f)))

;; TODO(robin): broken
;; (define-public python-frida
;;   (package
;;     (name "python-frida")
;;     (version "12.8.20")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (pypi-uri "frida" version))
;;         (sha256
;;           (base32
;;             "126faax3zzdfkzjlm4q20iyaznz79jjyxh0m2m85jcxfacnhry7h"))))
;;     (build-system python-build-system)
;;     (home-page "https://www.frida.re")
;;     (synopsis
;;       "Dynamic instrumentation toolkit for developers, reverse-engineers, and security researchers")
;;     (description
;;       "Dynamic instrumentation toolkit for developers, reverse-engineers, and security researchers")
;;     (license #f)))

;; (define-public python-frida-tools
;;   (package
;;     (name "python-frida-tools")
;;     (version "7.2.0")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (pypi-uri "frida-tools" version))
;;         (sha256
;;           (base32
;;             "0rnb8ycjmnf54p7ibca9rykc7wpfiv5navvy6qhqm3hw1wnbma7z"))))
;;     (build-system python-build-system)
;;     (propagated-inputs
;;       `(("python-colorama" ,python-colorama)
;;         ("python-frida" ,python-frida)
;;         ("python-prompt-toolkit" ,python-prompt-toolkit)
;;         ("python-pygments" ,python-pygments)))
;;     (home-page "https://www.frida.re")
;;     (synopsis "Frida CLI tools")
;;     (description "Frida CLI tools")
;;     (license #f)))
