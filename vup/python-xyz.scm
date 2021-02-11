(define-module (vup python-xyz)
  #:use-module (guix utils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages libusb)
  #:use-module (vup fpga)
  #:use-module (vup smt)
  #:use-module (vup linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public python-pyvcd
  (package
    (name "python-pyvcd")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvcd" version))
       (sha256
        (base32
         "1pk7sqff4xhz3pycim6cvlria39mn331zm4xylri10iy2hhp7zf0"))))
    (build-system python-build-system)
    (inputs `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs `(("python-six" ,python-six)))
    (home-page
     "http://pyvcd.readthedocs.io/en/latest/")
    (synopsis "Python VCD file support.")
    (description "Python VCD file support.")
    (license license:expat)))

(define-public python-fitsio
  (package
    (name "python-fitsio")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fitsio" version))
       (sha256
        (base32
         "0bq0hmyza56q5hz5w11disk1mv87fi71bxx1qk35zgwcpfyqkri0"))))
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
       ("python-sympy" ,python-sympy)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools42)
       ("python-wheel" ,python-wheel)))
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
    (native-inputs
     `(("python-setuptools" ,python-setuptools42)
       ("python-wheel" ,python-wheel)))
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
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xarray" version))
       (sha256
        (base32
         "1js3xr3fl9bwid8hl3w2pnigqzjd2rvkncald5x1x5fg7wjy8pb6"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools42)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-wheel" ,python-wheel)))
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
    (version "0.10.0")
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
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pint" version))
       (sha256
        (base32
         "0wkzb7g20wzpqr3xaqpq96dlfv6irw202icsz81ys8npp7mm194s"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (arguments
     `(#:phases
      (modify-phases %standard-phases
        (delete `check))))
    ;; (propagated-inputs
    ;;  `(("python-funcsigs" ,python-funcsigs)
    ;;    ("python-setuptools-scm" ,python-setuptools-scm)
    ;;    ("python-matplotlib" ,python-matplotlib)
    ;;    ("python-numpy" ,python-numpy)
    ;;    ("python-sparse" ,python-sparse)
    ;;    ("python-xarray" ,python-xarray)
    ;;    ("python-pytest" ,python-pytest)))
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
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chipsec/chipsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rxr9i08a22m15slvlkrhnki30jixi2ds096kmmc2nqzfr9yibmb"))))
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
               (setenv "KERNEL_SRC_DIR" kernel-src-dir))))
         (delete 'check)))))) ; broken for some reason

(define (setuptools-scm-version-setter v)
  `(lambda* (#:key inputs #:allow-other-keys)
     (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,v)
     #t))

(define-public python-nmigen
  (let ((commit "ca6fa036f6cd057c997460cf5898bbf791c4546a"))
    (package
      (name "python-nmigen")
      (version (string-append "0.3+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nmigen/nmigen")
               (commit commit)))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (substitute* "setup.py"
               (("if git.exact:")
                "if git is not None and git.exact:")) ; we have no git folde
             #t))
         (sha256
          (base32
           "0d69gvax1fp3min87q0a3fh4rajckn0dark8xgmdwjwh1yhs1y2i"))))
      (build-system python-build-system)
      (inputs `(("yosys" ,yosys-git)
                ("symbiyosys" ,symbiyosys)
                ("python-setuptools-scm" ,python-setuptools-scm)
                ("python-wheel" ,python-wheel)
                ("python-setuptools" ,python-setuptools)))
      (propagated-inputs
       `(("python-jinja2" ,python-jinja2)
         ("python-pyvcd" ,python-pyvcd)
         ("python-importlib-resources" ,python-importlib-resources)
         ("python-pyvcd" ,python-pyvcd)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "https://nmigen.info/nmigen/")
      (synopsis
       "Python toolbox for building complex digital hardware")
      (description
       "Python toolbox for building complex digital hardware")
      (license license:bsd-3))))


(define-public python-nmigen-boards
  (let ((commit "bcc14672994a3cdd04b7c0e1620d6f5e05145d94"))
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
           "0b90bv0q4vs9dm48660pll0xq4qi92257b02yfyblrx1kybgxzs0"))))
      (build-system python-build-system)
      (inputs
       `(("python-setuptools" ,python-setuptools)
         ("python-setuptools-scm" ,python-setuptools-scm)
         ("python-wheel" ,python-wheel)))
      (propagated-inputs
       `(("python-nmigen" ,python-nmigen)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "https://nmigen.info/nmigen/")
      (synopsis
       "Board and connector definitions for nMigen")
      (description
       "Board and connector definitions for nMigen")
      (license license:bsd-3))))

(define-public python-nmigen-stdio
  (let ((commit "01eb8fd32046d7b4726a7df2cc8e811409ba453c"))
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
           "1fwav66vd4lccdc438prwvpp7drs556j2gjx5d6li07zmp8v1z7b"))))
      (build-system python-build-system)
      (inputs
       `(("python-setuptools-scm" ,python-setuptools-scm)
         ("python-wheel" ,python-wheel)))
      (propagated-inputs
       `(("python-nmigen" ,python-nmigen)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "https://nmigen.info/nmigen/")
      (synopsis "Industry standard I/O for nMigen")
      (description "Industry standard I/O for nMigen")
      (license license:bsd-3))))

(define-public python-nmigen-soc
  (let ((commit "ecfad4d9abacf903a525f0a252c38844eda0d2dd"))
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
           "0afmnfs1ms7p1r4c1nc0sfvlcq36zjwaim7775v5i2vajcn3020c"))))
      (build-system python-build-system)
      (inputs
       `(("python-setuptools-scm" ,python-setuptools-scm)
         ("python-wheel" ,python-wheel)))
      (propagated-inputs
       `(("python-nmigen" ,python-nmigen)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-setuptools-scm-version
                      ,(setuptools-scm-version-setter version)))))
      (home-page "https://nmigen.info/nmigen/")
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
  (let ((commit "091222b87febb10fad87fcbe98a57599a54c5fd3"))
    (package
      (name "symbiyosys")
      (version (string-append "2020.10.23-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/SymbiYosys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jdfg38rk25qgjjp79y4pm1d9j34h9wbzlyjj6ipxzagp3kga3yk"))))
      (inputs `(("python" ,python)
                ("yosys" ,yosys-git)))
      (propagated-inputs `(("yices" ,yices)))
      (arguments
       `(#:make-flags `(,(string-append "PREFIX=" %output))
         #:phases (modify-phases %standard-phases
                    (add-before 'install 'fix-permissions
                                (lambda _
                                  (for-each make-file-writable (find-files "." ".*"))
                                  #t))
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
                    (delete 'check))))  ; no tests (make test instead of make check)
      (build-system gnu-build-system)
      (home-page "https://github.com/YosysHQ/SymbiYosys")
      (synopsis "SymbiYosys (sby) -- Front-end for Yosys-based formal verification flows")
      (description "SymbiYosys (sby) is a front-end driver program for Yosys-based formal hardware verification flows.")
      (license license:isc))))

(define-public python-skyfield
  (package
    (name "python-skyfield")
    (version "1.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skyfield" version))
       (sha256
        (base32
         "0gjm9jqj5ayvlcmd46iyrhr56pvpl8d28gnw6cqbm2dvbpf5rkln"))))
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

(define-public python-pytest-helpers-namespace
  (package
    (name "python-pytest-helpers-namespace")
    (version "2019.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-helpers-namespace" version))
       (sha256
        (base32
         "1kkifxcnv2s2136p0zg9h15f3lq1i7xgppzncq0hqhcjkyhj7zsf"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page
     "https://github.com/saltstack/pytest-helpers-namespace")
    (synopsis "PyTest Helpers Namespace")
    (description "PyTest Helpers Namespace")
    (license #f)))

(define-public python-pytest-xdist-1.28.0
  (package
    (inherit python-pytest-xdist)
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xdist" version))
       (sha256
        (base32
         "0v2jpdk2gdybay453v3xrb2k59bk4kqs71asi9yda7z8jd94hfpq"))))))

(define-public python-pytest-5.3.4
  (package
    (inherit python-pytest)
    (version "5.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32
         "005n7718iv0sm4w7yr347lqihc5svj2jsbpqasg706jdwn5jw4hx"))))))

(define-public python-xmp-toolkit
  (package
    (name "python-xmp-toolkit")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-xmp-toolkit" version))
       (sha256
        (base32
         "0f4s86hji6idyfg9007jncl366gasjjmldbwbknldzgrdya15ngq"))))
    (build-system python-build-system)
    (inputs
     `(("exempi" ,exempi)))
    (propagated-inputs
     `(("python-pytz" ,python-pytz)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-exempi-location
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((exempi (assoc-ref inputs "exempi")))
               (substitute* '("libxmp/exempi.py")
                 (("ctypes.util.find_library\\('exempi'\\)") (string-append "\"" exempi "/lib/libexempi.so\""))))
             #t))
         (delete 'check)))) ; fails and I don't care
    (home-page
     "https://github.com/python-xmp-toolkit/python-xmp-toolkit")
    (synopsis
     "Python XMP Toolkit for working with metadata.")
    (description
     "Python XMP Toolkit for working with metadata.")
    (license #f)))

(define-public python-pikepdf
  (package
    (name "python-pikepdf")
    (version "1.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pikepdf" version))
       (sha256
        (base32
         "152cva29vpzwd9ksfwmp3whg57km0nfj8vf7s6nxaxxk8r5h0rgv"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools-scm-git-archive" ,python-setuptools-scm-git-archive)))
    (inputs
     `(("pybind11" ,pybind11)
       ("qpdf" ,qpdf)))
    (propagated-inputs
     `(("python-lxml" ,python-lxml)))
    ;; ("attrs" ,python-attrs)
    ;; ("python-pytest" ,python-pytest-5.3.4)
    ;; ("python-pytest-helpers-namespace" ,python-pytest-helpers-namespace)
    ;; ("python-pytest-timeout" ,python-pytest-timeout)
    ;; ("python-pytest-xdist" ,python-pytest-xdist-1.28.0)
    ;; ("python-pytest-runner" ,python-pytest-runner)
    ;; ("python-pytest-forked" ,python-pytest-forked)
    ;; ("python-pillow" ,python-pillow)
    ;; ("python-xmp-toolkit" ,python-xmp-toolkit)
    ;; ("python-hypothesis" ,python-hypothesis)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")))
         (delete 'check)))) ; pytest version conflict
    (home-page "https://github.com/pikepdf/pikepdf")
    (synopsis
     "Read and write PDFs with Python, powered by qpdf")
    (description
     "Read and write PDFs with Python, powered by qpdf")
    (license #f)))

(define-public python-bluepy
  (package
    (name "python-bluepy")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edwios/bluepy")
             (commit "10f1cee90afb416f139949b86b491e4cfa98c886")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zgrpnjvi51xd0q2zb3pxpqsni2r59cvpl3f3i6xdi0lqk3drp8f"))))
    (build-system python-build-system)
    (home-page "https://github.com/IanHarvey/bluepy")
    (inputs `(("glib" ,glib)
              ("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-HOME
           (lambda _
             (begin
               (map (lambda (f) (chmod f #o666)) (find-files "." ".*"))
               #t)))
         (add-after 'unpack 'set-cc
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CC" "gcc")))
         (delete 'check)))) ; broken for some reason
    (synopsis
     "Python module for interfacing with BLE devices through Bluez")
    (description
     "Python module for interfacing with BLE devices through Bluez")
    (license #f)))

(define-public python-pymodbus
  (package
    (name "python-pymodbus")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pymodbus" version))
       (sha256
        (base32
         "0i64jr426rqx1di07z7ixjwi8j79majbl1501zlzi79zvjfd8ncf"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyserial" ,python-pyserial)
       ("python-six" ,python-six)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (home-page
     "https://github.com/riptideio/pymodbus/")
    (synopsis
     "A fully featured modbus protocol stack in python")
    (description
     "A fully featured modbus protocol stack in python")
    (license #f)))

(define-public python-solaredge-modbus
  (package
    (name "python-solaredge-modbus")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "solaredge_modbus" version))
       (sha256
        (base32
         "0z8rsapnzvmgph45rx5qczbp7kwqlmk025z7g736rn83jniwsi1f"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pymodbus" ,python-pymodbus)))
    (home-page
     "https://github.com/nmakel/solaredge_modbus")
    (synopsis "SolarEdge Modbus parser library")
    (description "SolarEdge Modbus parser library")
    (license license:expat)))

(define-public python-importlib-resources
  (package
    (name "python-importlib-resources")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://files.pythonhosted.org/packages/46/0b/7973c432eae095b60db1a37d6bf2c693cb4290b4d38d92f1bafe503c853d/importlib_resources-3.0.0.tar.gz") ; (pypi-uri "importlib-resources" version))
       (sha256
        (base32
         "1hq626mx5jl9zfl0wdrjkxsnh8qd98fqv322n68b9251xjk4bxqr"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-contextlib2" ,python-contextlib2)
       ("python-pathlib2" ,python-pathlib2)
       ("python-singledispatch" ,python-singledispatch)
       ("python-toml" ,python-toml)
       ("python-zipp" ,python-zipp)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check))))
    ;; (delete 'check))
    (home-page
     "http://importlib-resources.readthedocs.io/")
    (synopsis "Read resources from Python packages")
    (description
     "Read resources from Python packages")
    (license #f)))


(define-public python-openant
  (let* ((commit "ae9e7366fb78b2c39467e1a028b34cfb0c64ffd7")
         (version (string-append "0.4+" (string-take commit 9))))
    (package
      (name "python-openant")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Tigge/openant")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0lmcwrv0c9id081radsr4s5lyqay0mspyr7vc5d9a6q3p26wi6aw"))))
      (build-system python-build-system)
      (propagated-inputs `(("python-pyusb" ,python-pyusb)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'build 'patch-install
             (lambda _
               (substitute* '("setup.py")
                 (("install_udev_rules\\(True\\)") "install_udev_rules(False)"))
               #t)))))
      (home-page "https://github.com/Tigge/openant")
      (synopsis
       "ANT and ANT-FS Python Library")
      (description
       "ANT and ANT-FS Python Library")
      (license #f))))

(define-public python-openant/udev
  (package
    (inherit python-openant)
    (name "python-openant-udev")
    (build-system trivial-build-system)
    (propagated-inputs '())
    (native-inputs `(("source" ,(package-source python-openant))))
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (install-file (string-append (assoc-ref %build-inputs "source") "/resources/42-ant-usb-sticks.rules")
                       (string-append %output "/lib/udev/rules.d")))))))

(define-public python-antfs-cli
  (let* ((version "0.4")
         (commit "1fc1ee4fd05f786cd02294e9d7fc3a079a61bc10"))
    (package
      (name "python-antfs-cli")
      (version (string-append version "+" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Tigge/antfs-cli")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "118ml5f3p1f92d2xgr4ywsadyyrhhsh6gyypwp8l08hc92i102y4"))))
      (build-system python-build-system)
      (propagated-inputs `(("python-openant" ,python-openant)))
      (home-page "https://github.com/Tigge/antfs-cli")
      (synopsis
       "Extracts FIT files from ANT-FS based sport watches such as Garmin Forerunner 60, 405CX, 310XT, 610 and 910XT.")
      (description
       "Extracts FIT files from ANT-FS based sport watches such as Garmin Forerunner 60, 405CX, 310XT, 610 and 910XT.")
      (license #f))))

(define-public python-flameprof
  (package
    (name "python-flameprof")
    (version "0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flameprof" version))
        (sha256
          (base32
            "18vmg22j1m22xz486kak4y7175nvv520z970y4jadfybj10nvj6v"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/baverman/flameprof/")
    (synopsis "cProfile flamegraph generator")
    (description "cProfile flamegraph generator")
    (license license:expat)))


(define-public python-shapely-fixed
  (package
    (inherit python-shapely)
    (name "python-shapely-fixed")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check) ; broken
         (add-after 'unpack 'patch-geos-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((geos (assoc-ref inputs "geos"))
                   (glibc (assoc-ref inputs ,(if (%current-target-system)
                                                 "cross-libc" "libc"))))
               (for-each
                (lambda (file)
                  (substitute* file
                    (("_lgeos = load_dll\\('geos_c', fallbacks=.*\\)")
                     (string-append "_lgeos = load_dll('geos_c', fallbacks=['"
                                    geos "/lib/libgeos_c.so'])"))
                    (("free = load_dll\\('c'\\)\\.free")
                     (string-append "free = load_dll('c', fallbacks=['"
                                    glibc "/lib/libc.so.6']).free"))))
                '("shapely/geos.py" "shapely/_buildcfg.py"))
               )
             #t)))))))

(define-public python-flask-cors
  (package
    (name "python-flask-cors")
    (version "3.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-Cors" version))
        (sha256
          (base32
            "1f36hkaxc92zn12f88fkzwifdvlvsnmlp1dv3p5inpcc500c3kvb"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-flask" ,python-flask)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/corydolphin/flask-cors")
    (synopsis
      "A Flask extension adding a decorator for CORS support")
    (description
      "A Flask extension adding a decorator for CORS support")
    (license license:expat)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))

(define-public python-descartes
  (package
    (name "python-descartes")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "descartes" version))
        (sha256
          (base32
            "0nq36w9ylvfwmwn5qd9c8fsp2jzsqpmy4xcr6pzxcpmg8qhm0nhk"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-shapely" ,python-shapely-fixed)))
    (home-page
      "http://bitbucket.org/sgillies/descartes/")
    (synopsis
      "Use geometric objects as matplotlib paths and patches")
    (description
      "Use geometric objects as matplotlib paths and patches")
    (license license:bsd-3)))



(define-public python-loky
  (package
    (name "python-loky")
    (version "2.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "loky" version))
        (sha256
          (base32
            "1gy594v2nsyc6v62qyxizvsgh6a938hi4zmg1zdw4avc9djg16sa"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cloudpickle" ,python-cloudpickle)))
    (home-page "https://github.com/joblib/loky/")
    (synopsis
      "A robust implementation of concurrent.futures.ProcessPoolExecutor")
    (description
      "A robust implementation of concurrent.futures.ProcessPoolExecutor")
    (license license:bsd-3)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))




(define-public python-pybind11
  (package
    (name "python-pybind11")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pybind11" version))
        (sha256
          (base32
            "1balzcacb6i296rfq0p1vwvdz4jqxk5ilvw2cda924h1i1x4wnpa"))))
    (build-system python-build-system)
    (home-page "https://github.com/pybind/pybind11")
    (synopsis
      "Seamless operability between C++11 and Python")
    (description
      "Seamless operability between C++11 and Python")
    (license license:bsd-3)))

(define-public python-ninja
  (package
    (name "python-ninja")
    (version "1.10.0.post2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ninja" version))
        (sha256
          (base32
            "1y0jv150arz1rhrzi5z7sss80rgrksi33ig8hb5z1gm92csxf7v2"))))
    (build-system python-build-system)
    (inputs `(("skbuild" ,python-scikit-build)))
    (home-page "http://ninja-build.org/")
    (synopsis
      "Ninja is a small build system with a focus on speed")
    (description
      "Ninja is a small build system with a focus on speed")
    (license license:asl2.0)))

(define-public python-cmake
  (package
    (name "python-cmake")
    (version "3.18.2.post1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cmake" version))
        (sha256
          (base32
            "08h83c7ha29c10bd730s6jj0a4nrwjk7r4w8bsb61m6ndsg3rnp8"))))
    (build-system python-build-system)
    (inputs `(("skbuild" ,python-scikit-build)
              ("cmake" ,cmake)
              ("libuv" ,libuv)))
    (home-page "http://cmake.org/")
    (synopsis
      "CMake is an open-source, cross-platform family of tools designed to build, test and package software")
    (description
      "CMake is an open-source, cross-platform family of tools designed to build, test and package software")
    (license license:asl2.0)))

(define-public python-scikit-build
  (package
    (name "python-scikit-build")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "scikit-build" version))
        (sha256
          (base32
            "0p4smkl2rbpl00m5va5qa8hp2hqb3284p2cs6k8zlmi4kgbdyh6s"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-distro" ,python-distro)
        ("python-packaging" ,python-packaging)
        ("python-setuptools" ,python-setuptools)
        ("python-wheel" ,python-wheel)))
    (home-page
      "https://github.com/scikit-build/scikit-build")
    (synopsis
      "Improved build system generator for Python C/C++/Fortran/Cython extensions")
    (description
      "Improved build system generator for Python C/C++/Fortran/Cython extensions")
    (license license:expat)))

(define-public python-xdoctest
  (package
    (name "python-xdoctest")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xdoctest" version))
        (sha256
          (base32
            "0wf37s03lk6vwki4m5qy21cnrjcq7jqss6pcxdkb2s9v816ih2kz"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-six" ,python-six)))
    (native-inputs
      `(; ("python-cmake" ,python-cmake)
        ("python-codecov" ,python-codecov)
        ("python-ipykernel" ,python-ipykernel)
        ("python-ipython" ,python-ipython)
        ("python-jupyter-client" ,python-jupyter-client)
        ("python-nbconvert" ,python-nbconvert)
        ("python-nbformat" ,python-nbformat)
        ; ("python-ninja" ,python-ninja)
        ("python-pybind11" ,python-pybind11)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-scikit-build" ,python-scikit-build)))
    (home-page
      "https://github.com/Erotemic/xdoctest")
    (synopsis
      "A rewrite of the builtin doctest module")
    (description
      "A rewrite of the builtin doctest module")
    (license #f)))



(define-public python-setuptools-scm-3.5
  (package
    (inherit python-setuptools-scm)
    (version "3.5.0")
    (source
     (origin
       (inherit (package-source python-setuptools))
       (uri "https://files.pythonhosted.org/packages/b2/f7/60a645aae001a2e06cf4b8db2fba9d9f36b8fd378f10647e3e218b61b74b/setuptools_scm-3.5.0.tar.gz") ; (pypi-uri "setuptools-scm" version ".zip"))
       (sha256
        (base32
         "11qs1jvfgflx1msv39jgc6bj9d9a300ra35fwypkr44jayh23psv"))))))

(define-public python-pytest-mypy
  (package
    (name "python-pytest-mypy")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-mypy" version))
        (sha256
          (base32
            "0gb7hn4yraji94873rzvxb1y59i334i1yhclljrrigv65fd7srjs"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-filelock" ,python-filelock)
        ("python-mypy" ,python-mypy)
        ("python-pytest" ,python-pytest)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm-3.5)))
    (home-page
      "https://github.com/dbader/pytest-mypy")
    (synopsis
      "Mypy static type checker plugin for Pytest")
    (description
      "Mypy static type checker plugin for Pytest")
    (license license:expat)))

(define-public python-path
  (package
    (name "python-path")
    (version "15.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "path" version))
        (sha256
          (base32
            "14nsvz46b3srcbjhh59ik8mxdxf0rwj9am0mpkvxp8xf5xis06jj"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-appdirs" ,python-appdirs)
        ("python-packaging" ,python-packaging)
        ("python-pygments" ,python-pygments)
        ("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page "https://github.com/jaraco/path")
    (synopsis "A module wrapper for os.path")
    (description "A module wrapper for os.path")
    (license #f)))

(define-public python-path.py
  (package
    (name "python-path.py")
    (version "12.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "path.py" version))
        (sha256
          (base32
            "1a198l1zz2rh21l16ap46pq02d4ljzyy151xf02x1blp4j5mx24d"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-path" ,python-path)))
    (native-inputs
      `(("python-appdirs" ,python-appdirs)
        ("python-packaging" ,python-packaging)
        ("python-pygments" ,python-pygments)
        ("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page "https://github.com/jaraco/path")
    (synopsis "A module wrapper for os.path")
    (description "A module wrapper for os.path")
    (license #f)))

(define-public python-jaraco.structures
  (package
    (name "python-jaraco.structures")
    (version "2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.structures" version))
        (sha256
          (base32
            "0wcmywzagybxvwl5d6ix7d655swi8sa41hcgb7zz723my7r9mapm"))))
    (build-system python-build-system)
    (inputs `(("python-setuptools-scm" ,python-setuptools-scm)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.structures")
    (synopsis "Data structures by jaraco")
    (description "Data structures by jaraco")
    (license #f)))

(define-public python-jaraco.collections
  (package
    (name "python-jaraco.collections")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.collections" version))
        (sha256
          (base32
            "08a7appg3yaz7kkj8ijpan90mmv3z8w54fa9fishnag7ybs0wmxy"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.classes" ,python-jaraco.classes)
        ("python-jaraco.text" ,python-jaraco.text)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.collections")
    (synopsis
      "Collection objects similar to those in stdlib by jaraco")
    (description
      "Collection objects similar to those in stdlib by jaraco")
    (license #f)))

(define-public python-jaraco.functools
  (package
    (name "python-jaraco.functools")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.functools" version))
        (sha256
          (base32
            "1x4l7d4mvr94nfzh4zgvkdcglvvagbx2y6ryw2ijql8p66zc9vcz"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-more-itertools" ,python-more-itertools)))
    (native-inputs
      `(("python-jaraco.classes" ,python-jaraco.classes)
        ("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.functools")
    (synopsis "Functools like those found in stdlib")
    (description
      "Functools like those found in stdlib")
    (license #f)))

(define-public python-jaraco.text
  (package
    (name "python-jaraco.text")
    (version "3.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.text" version))
        (sha256
          (base32
            "1v0hz3h74m31jlbc5bxwkvrx1h2n7887bajrg1n1c3yc4q8qn1z5"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-importlib-resources"
         ,python-importlib-resources)
        ("python-jaraco.functools"
         ,python-jaraco.functools)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.text")
    (synopsis "Module for text manipulation")
    (description "Module for text manipulation")
    (license #f)))

(define-public python-pytest-black-multipy
  (package
    (name "python-pytest-black-multipy")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-black-multipy" version))
        (sha256
          (base32
            "1ciwa99fnz3ngbsvcjvxqz4k1vwfmvpxaj7qf5vxkx0awvczhsyd"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pytest-black" ,python-pytest-black)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page "https://github.com/jaraco/skeleton")
    (synopsis "Allow '--black' on older Pythons")
    (description "Allow '--black' on older Pythons")
    (license #f)))

(define-public python-jaraco.classes
  (package
    (name "python-jaraco.classes")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.classes" version))
        (sha256
          (base32
            "1avsxzm5mwylmy2zbxq3xvn48z5djb0qy3hwv4ryncprivzri1n3"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-more-itertools" ,python-more-itertools)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black-multipy"
         ,python-pytest-black-multipy)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.classes")
    (synopsis
      "Utility functions for Python class constructs")
    (description
      "Utility functions for Python class constructs")
    (license #f)))

(define-public python-jaraco.ui
  (package
    (name "python-jaraco.ui")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.ui" version))
        (sha256
          (base32
            "0di9fnjgwyyk6jq6gvwiz8qy1f6znwq96m416d9q8vr36hkmayzi"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.classes" ,python-jaraco.classes)
        ("python-jaraco.text" ,python-jaraco.text)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page "https://github.com/jaraco/jaraco.ui")
    (synopsis
      "User-Interface tools (mainly command-line)")
    (description
      "User-Interface tools (mainly command-line)")
    (license #f)))

(define-public python-jaraco.windows
  (package
    (name "python-jaraco.windows")
    (version "5.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.windows" version))
        (sha256
          (base32
            "1zxcvmjd2w9n11b3wpdsmrzmlpxj1g2fj9qx6zzbbfzgswqkzg11"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.collections"
         ,python-jaraco.collections)
        ("python-jaraco.structures"
         ,python-jaraco.structures)
        ("python-jaraco.text" ,python-jaraco.text)
        ("python-jaraco.ui" ,python-jaraco.ui)
        ("python-more-itertools" ,python-more-itertools)
        ("python-path.py" ,python-path.py)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-setuptools-scm" ,python-setuptools-scm)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/jaraco.windows")
    (synopsis "Windows Routines by Jason R. Coombs")
    (description
      "Windows Routines by Jason R. Coombs")
    (license #f)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))

(define-public python-ordered-set
  (package
    (name "python-ordered-set")
    (version "4.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ordered-set" version))
        (sha256
          (base32
            "159syfbqnwqnivzjfn3x7ak3xwrxmnzbji7c2qhj1jjv0pgv54xs"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-pytest" ,python-pytest)))
    (home-page
      "https://github.com/LuminosoInsight/ordered-set")
    (synopsis
      "A set that remembers its order, and allows looking up its items by their index in that order.")
    (description
      "A set that remembers its order, and allows looking up its items by their index in that order.")
    (license #f)))

(define-public python-ubelt
  (package
    (name "python-ubelt")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ubelt" version))
        (sha256
          (base32
            "0dvjwq9j1h04g9r2a0kc1vg1ycgi2kbmfa5z7wrp7lrx2q293qzl"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.windows" ,python-jaraco.windows)
        ("python-ordered-set" ,python-ordered-set)
        ("python-six" ,python-six)))
        ;; ("python-typing" ,python-typing)))
    (native-inputs
      `(("python-codecov" ,python-codecov)
        ("python-coverage" ,python-coverage)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-timeout" ,python-pytest-timeout)
        ("python-xdoctest" ,python-xdoctest)))
    (home-page "https://github.com/Erotemic/ubelt")
    (synopsis
      "A Python utility belt containing simple tools, a stdlib like feel, and extra batteries.")
    (description
      "A Python utility belt containing simple tools, a stdlib like feel, and extra batteries.")
    (license #f)))

(define-public python-line-profiler
  (package
    (name "python-line-profiler")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://files.pythonhosted.org/packages/98/bd/4ff4e59f97897d21b3b7d0c97ac77cedab23e6137a8c8fb3cbc9ee9d1f71/line_profiler-3.0.2.tar.gz") ; (pypi-uri "line-profiler" version))
        (sha256
          (base32
            "0hficwvgdfz7zwg6ajbbsqbg041rjc4g2jwp26r4k1hzv1mss63j"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-ipython" ,python-ipython)))
    (native-inputs
      `(("python-codecov" ,python-codecov)
        ("python-coverage" ,python-coverage)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("skbuild" ,python-scikit-build)
        ("python-ubelt" ,python-ubelt)
        ("python-cython" ,python-cython)
        ("cmake" ,cmake)))
    (home-page
      "https://github.com/pyutils/line_profiler")
    (synopsis "Line-by-line profiler.")
    (description "Line-by-line profiler.")
    (license license:bsd-3)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))


(define-public python-scikit-build
  (package
    (name "python-scikit-build")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "scikit-build" version))
        (sha256
          (base32
            "0p4smkl2rbpl00m5va5qa8hp2hqb3284p2cs6k8zlmi4kgbdyh6s"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-distro" ,python-distro)
        ("python-packaging" ,python-packaging)
        ("python-setuptools" ,python-setuptools)
        ("python-wheel" ,python-wheel)))
    ;; (native-inputs
    ;;  `(("python-pytest" ,python-pytest)
    ;;    ("python-virtualenv" ,python-virtualenv)
    ;;    ("python-pytest-runner" ,python-pytest-runner)))
    (home-page
      "https://github.com/scikit-build/scikit-build")
    (synopsis
      "Improved build system generator for Python C/C++/Fortran/Cython extensions")
    (description
      "Improved build system generator for Python C/C++/Fortran/Cython extensions")
    (license license:expat)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))

(define-public python-snakeviz
  (package
    (name "python-snakeviz")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "snakeviz" version))
        (sha256
          (base32
            "0s6byw23hr2khqx2az36hpi52fk4v6bfm1bb7biaf0d2nrpqgbcj"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-tornado" ,python-tornado)))
    (home-page
      "https://github.com/jiffyclub/snakeviz")
    (synopsis
      "A web-based viewer for Python profiler output")
    (description
      "A web-based viewer for Python profiler output")
    (license #f)))


(define-public python-numba-fixed
  (package
    (inherit python-numba)
    (name "python-numba-fixed")
    (arguments
     (substitute-keyword-arguments (package-arguments python-numba)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'check)))))))


(define-public python-pythran
  (package
    (name "python-pythran")
    (version "0.9.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pythran" version))
        (sha256
          (base32
            "04hhghf0m23xrbrija15zl2mqz1y1nwmhlaqmq6wiqv2v15sjdmc"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-beniget" ,python-beniget)
        ("python-decorator" ,python-decorator)
        ("python-gast" ,python-gast-0.4.0)
        ("python-networkx" ,python-networkx)
        ("python-numpy" ,python-numpy)
        ("python-ply" ,python-ply)
        ("python-six" ,python-six)
        ("python-pytest" ,python-pytest)
        ("python-pytest-runner" ,python-pytest-runner)))
    (home-page
      "https://github.com/serge-sans-paille/pythran")
    (synopsis
      "Ahead of Time compiler for numeric kernels")
    (description
      "Ahead of Time compiler for numeric kernels")
    (license #f)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))

(define-public python-commonmark
  (package
    (name "python-commonmark")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "commonmark" version))
        (sha256
          (base32
            "0q7d39lm8kcingpmykk5r959hrwwj6v2icyw3mihczxyb749sbs5"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-future" ,python-future)))
    (native-inputs
      `(("python-flake8" ,python-flake8)
        ("python-hypothesis" ,python-hypothesis)))
    (home-page
      "https://github.com/rtfd/commonmark.py")
    (synopsis
      "Python parser for the CommonMark Markdown spec")
    (description
      "Python parser for the CommonMark Markdown spec")
    (license #f)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))

(define-public python-recommonmark
  (package
    (name "python-recommonmark")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "recommonmark" version))
        (sha256
          (base32
            "1szg87jj1aqazmchdya7sgsf0cclxydddwil6riqq9n5nsp4zk99"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-commonmark" ,python-commonmark)
        ("python-docutils" ,python-docutils)
        ("python-sphinx" ,python-sphinx)))
    (home-page
      "https://github.com/rtfd/recommonmark")
    (synopsis
      "A docutils-compatibility bridge to CommonMark, enabling you to write CommonMark inside of Docutils & Sphinx projects.")
    (description
      "A docutils-compatibility bridge to CommonMark, enabling you to write CommonMark inside of Docutils & Sphinx projects.")
    (license license:expat)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))

(define-public python-nbsphinx
  (package
    (name "python-nbsphinx")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nbsphinx" version))
        (sha256
          (base32
            "0j56bxdj08vn3q1804qwb1ywhga1mdg1awgm7i64wfpfwi8df2zm"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-docutils" ,python-docutils)
        ("python-jinja2" ,python-jinja2)
        ("python-nbconvert" ,python-nbconvert)
        ("python-nbformat" ,python-nbformat)
        ("python-sphinx" ,python-sphinx)
        ("python-traitlets" ,python-traitlets)))
    (home-page "https://nbsphinx.readthedocs.io/")
    (synopsis "Jupyter Notebook Tools for Sphinx")
    (description "Jupyter Notebook Tools for Sphinx")
    (license license:expat)))

(define-public python-jupyter
  (package
    (name "python-jupyter")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyter" version))
        (sha256
          (base32
            "0pwf3pminkzyzgx5kcplvvbvwrrzd3baa7lmh96f647k30rlpp6r"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-ipykernel" ,python-ipykernel)
        ("python-ipywidgets" ,python-ipywidgets)
        ("python-jupyter-console"
         ,python-jupyter-console)
        ("python-nbconvert" ,python-nbconvert)
        ("python-notebook" ,python-notebook)
        ("python-qtconsole" ,python-qtconsole)))
    (home-page "http://jupyter.org")
    (synopsis
      "Jupyter metapackage. Install all the Jupyter components in one go.")
    (description
      "Jupyter metapackage. Install all the Jupyter components in one go.")
    (license license:bsd-3)))

(define-public python-beniget
  (package
    (name "python-beniget")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "beniget" version))
        (sha256
          (base32
            "1w8gk9wcxza5rm91jnf10xqwd0adrqf085gv8hqpry6dx4xqjb06"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-gast" ,python-gast-0.4.0)))
    (home-page
      "https://github.com/serge-sans-paille/beniget/")
    (synopsis
      "Extract semantic information about static Python code")
    (description
      "Extract semantic information about static Python code")
    (license #f)))

(define-public python-transonic
  (package
    (name "python-transonic")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "transonic" version))
        (sha256
          (base32
            "0h7r9k0dx5w6mvlyxa7anml86ipqp3bsabrpkmq95m2ar7qf2k0k"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-astunparse" ,python-astunparse)
        ("python-autopep8" ,python-autopep8)
        ("python-beniget" ,python-beniget)
        ("python-gast" ,python-gast-0.4.0)
        ("python-numpy" ,python-numpy)))
    (native-inputs
      `(("python-black" ,python-black)
        ("python-cython" ,python-cython)
        ("python-flake8" ,python-flake8)
        ("python-ipython" ,python-ipython)
        ("python-jupyter" ,python-jupyter)
        ("python-mpi4py" ,python-mpi4py)
        ("python-nbsphinx" ,python-nbsphinx)
        ("python-nbval" ,python-nbval)
        ("python-numpydoc" ,python-numpydoc)
        ("python-pylint" ,python-pylint)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pythran" ,python-pythran)
        ("python-recommonmark" ,python-recommonmark)
        ("python-scipy" ,python-scipy)
        ("python-sphinx" ,python-sphinx)
        ("python-sphinx-rtd-theme"
         ,python-sphinx-rtd-theme)))
    (home-page "")
    (synopsis
      "Make your Python code fly at transonic speeds!")
    (description
      "Make your Python code fly at transonic speeds!")
    (license license:bsd-3)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))



(define-public python-gast-0.4.0
  (package
    (inherit python-gast)
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://files.pythonhosted.org/packages/83/4a/07c7e59cef23fb147454663c3271c21da68ba2ab141427c20548ae5a8a4d/gast-0.4.0.tar.gz") ; (pypi-uri "pytest-gast" version))
       (sha256
        (base32
         "1ha27g42n0dsy53k0nky54cdnzl5idbd295jb9c8ais3p2wbgzj0"))))))

(define-public python-pmbootstrap
  (package
    (name "python-pmbootstrap")
    (version "1.23.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pmbootstrap" version))
        (sha256
          (base32
            "0f1mmhdg06ckjk6r4rdwq74aiw9s9a36jmcpfm1cg4f5my647dfa"))))
    (build-system python-build-system)
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page "https://www.postmarketos.org")
    (synopsis
      "A sophisticated chroot / build / flash tool to develop and install postmarketOS")
    (description
      "A sophisticated chroot / build / flash tool to develop and install postmarketOS")
    (license #f)))

(define-public python-bitstring
  (package
    (name "python-bitstring")
    (version "3.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "bitstring" version))
        (sha256
          (base32
            "0jl6192dwrlm5ybkbh7ywmyaymrc3cmz9y07nm7qdli9n9rfpwzx"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/scott-griffiths/bitstring")
    (synopsis
      "Simple construction, analysis and modification of binary data.")
    (description
      "Simple construction, analysis and modification of binary data.")
    (license #f)))


(define-public python-sonic-client
  (package
    (name "python-sonic-client")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sonic-client" version))
        (sha256
          (base32
            "1scnpi17y520134j2k2ja1nbb04izf1f6xwfrpy2r82yxb672w74"))))
    (build-system python-build-system)
    (home-page "https://github.com/cyprx/pysonic")
    (synopsis "Python client for Sonic Search DB")
    (description "Python client for Sonic Search DB")
    (license license:expat)))


(define-public python-meilisearch
  (package
    (name "python-meilisearch")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "meilisearch" version))
        (sha256
          (base32
            "1h4qb88l0xa8q8ccvrn22q1rcrar06bl8068pfzn13bvklnn7z84"))))
    (build-system python-build-system)
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (propagated-inputs
      `(("python-requests" ,python-requests)))
    (home-page
      "https://github.com/meilisearch/meilisearch-python")
    (synopsis
      "The python client for MeiliSearch API.")
    (description
      "The python client for MeiliSearch API.")
    (license #f)))

(define-public python-ipympl
  (package
    (name "python-ipympl")
    (version "0.5.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ipympl" version))
        (sha256
          (base32
            "10n3llpwnx9c70gnx0m3maqkd4icap43z6dp4hasdzid19a2wbqf"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-ipykernel" ,python-ipykernel)
        ("python-ipywidgets" ,python-ipywidgets)
        ("python-jupyter-packaging" ,python-jupyter-packaging)
        ("python-matplotlib" ,python-matplotlib)))
    (home-page "http://matplotlib.org")
    (synopsis "Matplotlib Jupyter Extension")
    (description "Matplotlib Jupyter Extension")
    (license license:bsd-3)))


(define-public python-jupyter-packaging
  (package
    (name "python-jupyter-packaging")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyter-packaging" version))
        (sha256
          (base32
            "0ma4dsi2rjha1d592wkranbz4ppzwpvakgmybxzx3bqgdpi6w9gh"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-packaging" ,python-packaging)))
    (native-inputs
      `(("python-pytest" ,python-pytest)))
    (home-page "http://jupyter.org")
    (synopsis "Jupyter Packaging Utilities")
    (description "Jupyter Packaging Utilities")
    (license license:bsd-3)))

(define-public python-nbconvert-new
  (package
    (name "python-nbconvert")
    (version "5.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbconvert" version))
       (sha256
        (base32
         "08xm2sz7fslp2cm87k38anv3jxwxcld44hii1sx84gml03kliyr1"))))
    (build-system python-build-system)
    (arguments
     `(;; The "bdist_egg" target is disabled by default, causing the installation
       ;; to fail.
       #:configure-flags (list "bdist_egg")
       ;; FIXME: 5 failures, 40 errors.
       #:tests? #f))
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       (zero? (system* "py.test" "-v")))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-bleach" ,python-bleach)
       ("python-entrypoints" ,python-entrypoints)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-mistune" ,python-mistune)
       ("python-nbformat" ,python-nbformat)
       ("python-pygments" ,python-pygments)
       ("python-traitlets" ,python-traitlets)))
    (home-page "http://jupyter.org")
    (synopsis "Converting Jupyter Notebooks")
    (description "The @code{nbconvert} tool, @{jupyter nbconvert}, converts
notebooks to various other formats via Jinja templates.  It allows you to
convert an @code{.ipynb} notebook file into various static formats including:

@enumerate
@item HTML
@item LaTeX
@item PDF
@item Reveal JS
@item Markdown (md)
@item ReStructured Text (rst)
@item executable script
@end enumerate\n")
    (license license:bsd-3)))

(define-public jupyter
  (package
    (name "jupyter")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter" version))
       (sha256
        (base32
         "0pwf3pminkzyzgx5kcplvvbvwrrzd3baa7lmh96f647k30rlpp6r"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; there are none.
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipywidgets" ,python-ipywidgets)
       ("python-jupyter-console" ,python-jupyter-console)
       ("python-nbconvert" ,python-nbconvert-new)
       ("python-notebook" ,python-notebook)
       ("python-qtconsole" ,python-qtconsole)))
    (native-search-paths
     (list (search-path-specification
            (variable "JUPYTER_PATH")
            (files '("share/jupyter")))))
    (home-page "https://jupyter.org")
    (synopsis "Web application for interactive documents")
    (description
     "The Jupyter Notebook is a web application that allows you to create and
share documents that contain live code, equations, visualizations and
explanatory text.  Uses include: data cleaning and transformation, numerical
simulation, statistical modeling, machine learning and much more.")
    (license license:bsd-3)))

(define-public python-huffman
  (package
    (name "python-huffman")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nicktimko/huffman")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1yfiymnl2nkww14185mm4axhh2h0lpr6hqgc3m8g6ld61yxr11mr"))))
    (build-system python-build-system)
    (home-page "https://github.com/nicktimko/huffman")
    (synopsis "Generate Huffman codes with Python")
    (description
     "Generate Huffman codes with Python")
    (license #f)))
