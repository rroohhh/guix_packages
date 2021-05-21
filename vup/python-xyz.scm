(define-module (vup python-xyz)
  #:use-module (guix utils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages libffi)
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
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvcd" version))
       (sha256
        (base32
         "0pa2zb86kqcxcw0nz9aknqh8pf9raqc7iw0j59das2196sl527h7"))))
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
    (version "0.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xarray" version))
       (sha256
        (base32
         "1r01gnfnwljk5lk8gjahi333lr70z3d4k8zwq2vxbg4idjfl7s1q"))))
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
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sparse" version))
       (sha256
        (base32
         "0kw6ha83gk0dymayrxhh16xxk7aykiyiz3ksxdzj6hhjr3dkap5w"))))
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
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pint" version))
       (sha256
        (base32
         "0fn81s2z804vsbg8vpwib4d1wn6q43cqrpqgnrw4j5h3w2d2wfnl"))))
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
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colorhash" version))
       (sha256
        (base32
         "1frqvbrz7iyv3zsknz1jmj9v9mv06akr60kknypvfw0c44k75jhw"))))
    (build-system python-build-system)
    (home-page
     "https://bitbucket.org/fk/python-color-hash")
    (synopsis "Generate a color based on a value")
    (description "Generate a color based on a value")
    (license license:expat)))

(define-public python-arpeggio
  (package
    (name "python-arpeggio")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Arpeggio" version))
       (sha256
        (base32
         "17k3xvv0sk1ig7y8h7283ccdqhryr4a4mrmadnsjxnrffv6143cj"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (inputs `(("python-wheel" ,python-wheel)))
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
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "textX" version))
       (sha256
        (base32
         "1b7v9v3npp6m696bb307ky3wqi7dds6cbkf8jivilhmfsh9gqni6"))))
    (build-system python-build-system)
    (inputs `(("python-wheel" ,python-wheel)))
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
  (let ((commit "f7c2b9419f9de450be76a0e9cf681931295df65f"))
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
           "0cjs9wgmxa76xqmjhsw4fsb2mhgvd85jgs2mrjxqp6fwp8rlgnl1"))))
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
  (let ((commit "a35d870a994c2919116b2c06166dc127febb1512"))
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
           "1flbcyb2xz174dgqv2964qra80xj2vbzbqwjb27shvxm6knj9ikf"))))
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


(define-public python-setuptools44
  (package
    (inherit python-setuptools)
    (version "44.1.1")
    (source
     (origin
       (inherit (package-source python-setuptools))
       (uri (pypi-uri "setuptools" version ".zip"))
       (sha256
        (base32
         "0ss7b8mm88lqd4lr0kjl9p4crlybc6cvl81f9pfdm81jnmfsayn6"))))))

(define-public python-asttokens
  (package
    (name "python-asttokens")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asttokens" version))
       (sha256
        (base32
         "0a2ixiz04aw4p0aivxh47k3fa9ql804l3y5iv5gcih9aizi5fbm4"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-six" ,python-six)))
    (native-inputs
     `(("python-astroid" ,python-astroid)
       ("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools44)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-wheel" ,python-wheel)
       ("python-toml" ,python-toml)))
    (home-page
     "https://github.com/gristlabs/asttokens")
    (synopsis
     "Annotate AST trees with source code positions")
    (description
     "Annotate AST trees with source code positions")
    (license license:asl2.0)))

(define-public python-executing
  (package
    (name "python-executing")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "executing" version))
       (sha256
        (base32
         "1sv8jy3kij4mvrj2cmv6z9hxk7cfbxgdnr81z9wqb7iry3g1mdq7"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-wheel" ,python-wheel)
       ("python-toml" ,python-toml)
       ("python-asttokens" ,python-asttokens))) ; for tests
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
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "varname" version))
       (sha256
        (base32
         "16qzzdls1jdygdn7x2pnvrn6gkxpm0j7p69zy7ykc6hbd683v8wn"))))
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
  (let ((commit "605db98382156fa293e27cd987eea71fb549df0b"))
    (package
      (name "symbiyosys")
      (version (string-append "2021.02.13-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/SymbiYosys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "04ncicd2nvlr9l93yvmk7qjk97qqb60s0nrf57bhla7jhnx1lnfi"))))
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
    (version "1.36")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skyfield" version))
       (sha256
        (base32
         "1dm1327a4qv3klj9blrvddbhl72v1fqz52ym9km8qjj9vdkpywh6"))))
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
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "toposort" version))
       (sha256
        (base32
         "1b2hppzjg3p006qya3yfdnp76dwq8frl97lypdam0kw4xxb8yhm7"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pymodbus" version))
       (sha256
        (base32
         "1m9cq8vhcsfngfgl5hxy0w3h0pgklpnrjw4x5579zk5p8l161zr4"))))
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
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "solaredge_modbus" version))
       (sha256
        (base32
         "0fsn60vwns0m3nw3b2mhda232p5kyhchmv3w4m66gxws85japwgb"))))
    (build-system python-build-system)
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
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
  (let* ((commit "faa8987a1b8e3a085b584becaabdddb276b32a1b")
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
           "0hzr099g33p1pz1wclr4yaavl89g3bqy2plxgglkmk9mmrmiy3z7"))))
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
         (commit "f80ebca523353a074129efd05fdf9b28f35cf674"))
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
           "04fp1i9dnwniplq8579nc1gk6393241yz3wjy5pr3iblajg9q3h6"))))
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
                '("shapely/geos.py" "shapely/_buildcfg.py")))
               
             #t)))))))

(define-public python-flask-cors
  (package
    (name "python-flask-cors")
    (version "3.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-Cors" version))
       (sha256
        (base32
         "1pl16615fn1pc5n0vdrqlxm45mqsdjjxqv3gfkrs111v7wwkj25n"))))
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
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pybind11" version))
       (sha256
        (base32
         "1vg5cp1l3bpkxlfjghqhkvl53fg3nrz70fr404az4mln4zcsxq6h"))))
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
    (version "0.15.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xdoctest" version))
       (sha256
        (base32
         "1l85hpggiqk1bxvmm5467y973adc6bn2dh1mdz5x723r2k9967zg"))))
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
    (version "0.9.8.post2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pythran" version))
       (sha256
        (base32
         "1j10djs8jzw0acx1skbwjvdlln679bc9kf3jcm1mnwb1ivmnfc81"))))
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
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "recommonmark" version))
       (sha256
        (base32
         "0rvdd2ikdr0yg6cx6594fdzn53cmdc0g0i6qsbcdq8i2kxjdpd5x"))))
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
    (version "0.4.7.post0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transonic" version))
       (sha256
        (base32
         "0ajcnaifxvcaribhnaj0qsz2n2gzxja2cq3mcpdnnphk2q336fs9"))))
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
    (version "1.29.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pmbootstrap" version))
       (sha256
        (base32
         "1l6sghirnn5spvdkr9w1yp1v9m78028w9lkgxb9v9chpmb5lv2hm"))))
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


(define-public python-proto-plus
  (package
    (name "python-proto-plus")
    (version "1.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "proto-plus" version))
        (sha256
          (base32
            "1i5jjnwpd288378h37zads08h695iwmhxm0sxbr3ln6aax97rdb1"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-protobuf" ,python-protobuf)))
    (native-inputs
      `(("python-google-api-core"
         ,python-google-api-core)))
    (home-page
      "https://github.com/googleapis/proto-plus-python.git")
    (synopsis
      "Beautiful, Pythonic protocol buffers.")
    (description
      "Beautiful, Pythonic protocol buffers.")
    (license license:asl2.0)))

(define-public python-google-cloud-firestore
  (package
    (name "python-google-cloud-firestore")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-cloud-firestore" version))
        (sha256
          (base32
            "1q5s2gpkibnjxal9zrz02jfnazf7rxk0bi0ln5a3di6i47kjnga9"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-api-core"
         ,python-google-api-core)
        ("python-google-cloud-core"
         ,python-google-cloud-core)
        ("python-proto-plus" ,python-proto-plus)
        ("python-pytz" ,python-pytz)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/googleapis/python-firestore")
    (synopsis
      "Google Cloud Firestore API client library")
    (description
      "Google Cloud Firestore API client library")
    (license license:asl2.0)))

(define-public python-google-crc32c
  (package
    (name "python-google-crc32c")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-crc32c" version))
        (sha256
          (base32
            "09qbsvmbdhd8mlq8sy67pa758i41lzkmvllr16anczvk6q9bvxfz"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cffi" ,python-cffi)))
    (native-inputs
      `(("python-pytest" ,python-pytest)))
    (home-page
      "https://github.com/googleapis/python-crc32c")
    (synopsis
      "A python wrapper of the C library 'Google CRC32C'")
    (description
      "A python wrapper of the C library 'Google CRC32C'")
    (license license:asl2.0)))

(define-public python-google-resumable-media
  (package
    (name "python-google-resumable-media")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-resumable-media" version))
        (sha256
          (base32
            "0hwxdgsqh6933kp4jkv6hwwdcqs7bgjn9j08ga399njv3s9b367f"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-crcmod" ,python-crcmod)
        ("python-google-crc32c" ,python-google-crc32c)
        ("python-six" ,python-six)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/googleapis/google-resumable-media-python")
    (synopsis
      "Utilities for Google Media Downloads and Resumable Uploads")
    (description
      "Utilities for Google Media Downloads and Resumable Uploads")
    (license license:asl2.0)))

(define-public python-google-cloud-core
  (package
    (name "python-google-cloud-core")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-cloud-core" version))
        (sha256
          (base32
            "00mvgh4vm4z5mjnlbiigmp674dwsrrsxxi7ghby7jlsl4y2v3ay6"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-api-core"
         ,python-google-api-core)
        ("python-google-auth" ,python-google-auth)
        ("python-six" ,python-six)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/googleapis/python-cloud-core")
    (synopsis "Google Cloud API client core library")
    (description
      "Google Cloud API client core library")
    (license license:asl2.0)))

(define-public python-google-cloud-storage
  (package
    (name "python-google-cloud-storage")
    (version "1.36.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-cloud-storage" version))
        (sha256
          (base32
            "1g04szsbkwfj5887i02i5qd3awhy0nsd2g78276jzrdlmmnf45wm"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-auth" ,python-google-auth)
        ("python-google-cloud-core"
         ,python-google-cloud-core)
        ("python-google-resumable-media"
         ,python-google-resumable-media)
        ("python-requests" ,python-requests)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/googleapis/python-storage")
    (synopsis
      "Google Cloud Storage API client library")
    (description
      "Google Cloud Storage API client library")
    (license license:asl2.0)))

(define-public python-futures
  (package
    (name "python-futures")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "futures" version))
        (sha256
          (base32
            "154pvaybk9ncyb1wpcnzgd7ayvvhhzk92ynsas7gadaydbvkl0vy"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/agronholm/pythonfutures")
    (synopsis
      "Backport of the concurrent.futures package from Python 3")
    (description
      "Backport of the concurrent.futures package from Python 3")
    (license #f)))

(define-public python-googleapis-common-protos
  (package
    (name "python-googleapis-common-protos")
    (version "1.52.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "googleapis-common-protos" version))
        (sha256
          (base32
            "0lakcsd35qm5x4visvw6z5f1niasv9a0mjyf2bd98wqi0z41c1sn"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-protobuf" ,python-protobuf)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/googleapis/python-api-common-protos")
    (synopsis "Common protobufs used in Google APIs")
    (description
      "Common protobufs used in Google APIs")
    (license #f)))

(define-public python-grpcio
  (package
    (name "python-grpcio")
    (version "1.35.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "grpcio" version))
        (sha256
          (base32
            "0p34hjhliajpjwrjyblm8gly94rkkp9fyqhidbv8pryx2jxypl3v"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-six" ,python-six)))
    (home-page "https://grpc.io")
    (synopsis "HTTP/2-based RPC framework")
    (description "HTTP/2-based RPC framework")
    (license #f)))

(define-public python-google-api-core
  (package
    (name "python-google-api-core")
    (version "1.26.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-api-core" version))
        (sha256
          (base32
            "02vk1qsmbk9a4lzgk3gnfvhl9s1igqhwr1cvwr7r7jj89mvfqc22"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-grpcio" ,python-grpcio)
        ("python-google-auth" ,python-google-auth)
        ("python-googleapis-common-protos"
         ,python-googleapis-common-protos)
        ("python-packaging" ,python-packaging)
        ("python-protobuf" ,python-protobuf)
        ("python-pytz" ,python-pytz)
        ("python-requests" ,python-requests)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/googleapis/python-api-core")
    (synopsis "Google API client core library")
    (description "Google API client core library")
    (license license:asl2.0)))

(define-public python-google-auth-httplib2
  (package
    (name "python-google-auth-httplib2")
    (version "0.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-auth-httplib2" version))
        (sha256
          (base32
            "0fdwnx2yd65f5vhnmn39f4xnxac5j6x0pv2p42qifrdi1z32q2cd"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-auth" ,python-google-auth)
        ("python-httplib2" ,python-httplib2)
        ("python-six" ,python-six)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/GoogleCloudPlatform/google-auth-library-python-httplib2")
    (synopsis
      "Google Authentication Library: httplib2 transport")
    (description
      "Google Authentication Library: httplib2 transport")
    (license license:asl2.0)))

(define-public python-cachetools
  (package
    (name "python-cachetools")
    (version "4.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cachetools" version))
        (sha256
          (base32
            "1scjvk1w6r9zv7h6hv514l8yms3dwxfdkap4im6zdkx4gagf4sgl"))))
    (build-system python-build-system)
    (home-page "https://github.com/tkem/cachetools/")
    (synopsis
      "Extensible memoizing collections and decorators")
    (description
      "Extensible memoizing collections and decorators")
    (license license:expat)))

(define-public python-google-auth
  (package
    (name "python-google-auth")
    (version "1.26.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-auth" version))
        (sha256
          (base32
            "0ksm317n1hc9c78scfbfm7kskzrnqnalx0d7jbjfyl2nkc3isihv"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cachetools" ,python-cachetools)
        ("python-pyasn1-modules" ,python-pyasn1-modules)
        ("python-rsa" ,python-rsa)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)))
    (arguments `(#:phases (modify-phases %standard-phases
                           (delete 'check))))
    (home-page
      "https://github.com/googleapis/google-auth-library-python")
    (synopsis "Google Authentication Library")
    (description "Google Authentication Library")
    (license license:asl2.0)))

(define-public python-google-api-python-client
  (package
    (name "python-google-api-python-client")
    (version "1.12.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-api-python-client" version))
        (sha256
          (base32
            "1fq89wifa9ymby655is246w5d54ixybffj5vz7lwzhpf8926ifgk"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-api-core"
         ,python-google-api-core)
        ("python-google-auth" ,python-google-auth)
        ("python-google-auth-httplib2"
         ,python-google-auth-httplib2)
        ("python-httplib2" ,python-httplib2)
        ("python-six" ,python-six)
        ("python-uritemplate" ,python-uritemplate)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/googleapis/google-api-python-client/")
    (synopsis "Google API Client Library for Python")
    (description
      "Google API Client Library for Python")
    (license license:asl2.0)))

(define-public python-firebase-admin
  (package
    (name "python-firebase-admin")
    (version "4.5.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://files.pythonhosted.org/packages/3a/b0/e5996cc335d67463901be3481973517b81d34dce760cc5a619f61c025064/firebase_admin-4.5.1.tar.gz")
        ; (uri (pypi-uri "firebase-admin" version))
        (sha256
          (base32
            "1y1k5a98qjpi9lbdn47fl47mkcidav3nyi2dp75a00a5h28nw6jn"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cachecontrol" ,python-cachecontrol)
        ("python-google-api-core"
         ,python-google-api-core)
        ("python-google-api-python-client"
         ,python-google-api-python-client)
        ("python-google-cloud-firestore"
         ,python-google-cloud-firestore)
        ("python-google-cloud-storage"
         ,python-google-cloud-storage)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://firebase.google.com/docs/admin/setup/")
    (synopsis "Firebase Admin Python SDK")
    (description "Firebase Admin Python SDK")
    (license #f)))

(define-public python-demjson
  (package
    (name "python-demjson")
    (version "2.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "demjson" version))
        (sha256
          (base32
            "0ygbddpnvp5lby6mr5kz60la3hkvwwzv3wwb3z0w9ngxl0w21pii"))))
    (build-system python-build-system)
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "http://deron.meranda.us/python/demjson/")
    (synopsis
      "encoder, decoder, and lint/validator for JSON (JavaScript Object Notation) compliant with RFC 7159")
    (description
      "encoder, decoder, and lint/validator for JSON (JavaScript Object Notation) compliant with RFC 7159")
    (license #f)))

(define-public python-stdlib-list
  (package
    (name "python-stdlib-list")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "stdlib-list" version))
        (sha256
          (base32
            "17vdn4q0sdlndc2fr9svapxx6366hnrhkn0fswp1xmr0jxqh7rd1"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-sphinx" ,python-sphinx)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page
      "https://github.com/jackmaney/python-stdlib-list")
    (synopsis
      "A list of Python Standard Libraries (2.6-7, 3.2-9).")
    (description
      "A list of Python Standard Libraries (2.6-7, 3.2-9).")
    (license license:expat)))

(define-public python-pydeps
  (package
    (name "python-pydeps")
    (version "1.9.13")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pydeps" version))
        (sha256
          (base32
            "0hyi9011fczrkjmcs6adaycw8fjxhhynwsa1n0nq5sgg9a0i062g"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-stdlib-list" ,python-stdlib-list)))
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (home-page "https://github.com/thebjorn/pydeps")
    (synopsis "Display module dependencies")
    (description "Display module dependencies")
    (license license:bsd-3)))


(define-public python-openstep-parser
  (package
    (name "python-openstep-parser")
    (version "1.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "openstep-parser" version))
        (sha256
          (base32
            "0y3dk0l612r7w48jfnbz6iq7df406hbv7fkpq2gv6w0gg33qsmb4"))))
    (build-system python-build-system)
    (home-page
      "http://github.com/kronenthaler/openstep-parser")
    (synopsis
      "OpenStep plist reader into python objects")
    (description
      "OpenStep plist reader into python objects")
    (license license:bsd-3)))

(define-public python-pbxproj
  (package
    (name "python-pbxproj")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pbxproj" version))
        (sha256
          (base32
            "034dx46y8lj42b3v5vbcnb69i7c4z7sxdsl21g0dvpps5gc94sls"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-docopt" ,python-docopt)
        ("python-openstep-parser"
         ,python-openstep-parser)))
    (home-page
      "http://github.com/kronenthaler/mod-pbxproj")
    (synopsis
      "XCode Project manipulation library for Python")
    (description
      "XCode Project manipulation library for Python")
    (license license:expat)))


;; (define-public python-docx
;;   (package
;;     (name "python-docx")
;;     (version "0.2.4")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (pypi-uri "docx" version))
;;         (sha256
;;           (base32
;;             "0xw8vkjvpaq510qa6s53xbrw2ffh319rk8inf45xlv78qvm9axcx"))))
;;     (build-system python-build-system)
;;     (propagated-inputs
;;       `(("python-lxml" ,python-lxml)
;;         ("python-pillow" ,python-pillow)))
;;     (home-page
;;       "http://github.com/mikemaccana/python-docx")
;;     (synopsis
;;       "The docx module creates, reads and writes Microsoft Office Word 2007 docx files")
;;     (description
;;       "The docx module creates, reads and writes Microsoft Office Word 2007 docx files")
;;     (license #f)))


(define-public python-docx
  (package
    (name "python-docx")
    (version "0.8.10")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-docx" version))
        (sha256
          (base32
            "0900j8by7pvjfid41n1w55rcswawyfk077d689jcw01ddfnfqxmw"))))
    (build-system python-build-system)
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (propagated-inputs
      `(("python-lxml" ,python-lxml)))
    (home-page
      "https://github.com/python-openxml/python-docx")
    (synopsis
      "Create and update Microsoft Word .docx files.")
    (description
      "Create and update Microsoft Word .docx files.")
    (license #f)))


(define-public python-docx2python
  (package
    (name "python-docx2python")
    (version "1.27.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "docx2python" version))
        (sha256
          (base32
            "0s56ix45jyf1bvp5ii1x6n48ah4lx3nmqj6f1q30h1r2x4pak83c"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/ShayHill/docx2python")
    (synopsis "Extract content from docx files")
    (description "Extract content from docx files")
    (license #f)))

(define-public python-nose-cov
  (package
    (name "python-nose-cov")
    (version "1.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose-cov" version))
        (sha256
          (base32
            "04j4fw01bv648gimqqj4z88606lcczbm1k326agcc74gb4sh7v4b"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cov-core" ,python-cov-core)
        ("python-nose" ,python-nose)))
    (home-page
      "http://bitbucket.org/memedough/nose-cov/overview")
    (synopsis
      "nose plugin for coverage reporting, including subprocesses and multiprocessing")
    (description
      "nose plugin for coverage reporting, including subprocesses and multiprocessing")
    (license license:expat)))

(define-public python-influxdb
  (package
    (name "python-influxdb")
    (version "5.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "influxdb" version))
        (sha256
          (base32
            "0ymjv322mv6y424fmpd70f87152w55mbwwj6i7p3sjzf0ixmxy26"))))
    (build-system python-build-system)
    (arguments `(#:phases (modify-phases %standard-phases
                            (delete 'check))))
    (propagated-inputs
      `(("python-dateutil" ,python-dateutil)
        ("python-msgpack" ,python-msgpack)
        ("python-pytz" ,python-pytz)
        ("python-requests" ,python-requests)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-mock" ,python-mock)
        ("python-nose" ,python-nose)
        ("python-nose-cov" ,python-nose-cov)
        ("python-requests-mock" ,python-requests-mock)))
    (home-page
      "https://github.com/influxdb/influxdb-python")
    (synopsis "InfluxDB client")
    (description "InfluxDB client")
    (license license:expat)))
