(define-module (vup python-xyz)
  #:use-module (guix utils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
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
  (let ((commit "e46118dac0df315694b0fc6b9367d285a8fc12dd"))
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
           "0rp6c99zwi2as9hfj2x7zbm0cgggckqrg49v2nr33jg39sfl0cvr"))))
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
  (let ((commit "d20fb96e358994eb06295b3b64ee1efe13d86004"))
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
           "0jxpi82zxxr9d91m11818k6ib2pc4jz0paf4sgwl27plw559bd73"))))
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
  (let ((commit "b4058808a2fcfb56e64eef5ebfe028d3a659e038"))
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
           "046kxvf9ird2q1lfz69hcx7mibjzdk16hlvvdjzs02v4ynzmx8j5"))))
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
  (let ((commit "b172357161ea16a0734be6782b0744cf6b7108b6"))
    (package
      (name "symbiyosys")
      (version (string-append "2020.07.17-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/SymbiYosys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yz7w2zr4531zvlisxdnfb9qyrx8p9xzmh4ya3drvh1zx8g68wh3"))))
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
