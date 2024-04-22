(define-module (vup python-xyz)
  #:use-module (guix utils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix-science packages python)
  #:use-module (vup fpga)
  #:use-module (vup smt)
  #:use-module (vup linux)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
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

(define-public python-orthopy
  (package
    (name "python-orthopy")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://zenodo.org/records/5751934/files/nschloe/orthopy-v" version ".zip"))
       (sha256
        (base32
         "0ghcahjy7cjx95hdccbqvcds3vmcj3gpr852fqy92czfsnpc0lna"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-sympy" ,python-sympy)
       ("python-importlib-metadata" ,python-importlib-metadata)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-wheel" ,python-wheel)
       ("unzip" ,unzip)))
    (home-page "https://github.com/nschloe/orthopy")
    (synopsis
     "Tools for orthogonal polynomials, Gaussian quadrature")
    (description
     "Tools for orthogonal polynomials, Gaussian quadrature")
    (license #f)
    (arguments '(#:tests? #f))))

(define-public python-ndim
  (package
    (name "python-ndim")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sigma-py/ndim/archive/refs/tags/v" version ".tar.gz"))
       (sha256
        (base32
         "0z9pvj79n72x4ls9ndq3dm5znwflk38pkrd05afh5algakvh18wm"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-sympy))
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/nschloe/ndim")
    (synopsis
     "Compute multidimensional volumes and monomial integrals")
    (description
     "Compute multidimensional volumes and monomial integrals")
    (license #f)
    (arguments '(#:tests? #f))))

(define-public python-quadpy
  (package
    (name "python-quadpy")
    (version "0.16.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://zenodo.org/records/5541216/files/nschloe/quadpy-v" version ".zip"))
       (sha256
        (base32
         "1f989dipv7lqxvalfrvvlmhlxyl67a87lavyyqrr1mh88glhl592"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-orthopy" ,python-orthopy)
       ("python-scipy" ,python-scipy)
       ("python-sympy" ,python-sympy)
       ("python-sympy" ,python-ndim)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-wheel" ,python-wheel)
       ("unzip" ,unzip)))
    (home-page "https://github.com/nschloe/quadpy")
    (synopsis
     "Numerical integration, quadrature for various domains")
    (description
     "Numerical integration, quadrature for various domains")
    (license #f)
    (arguments '(#:tests? #f))))

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
    (arguments '(#:tests? #f))
    (synopsis
     "Python function signatures from PEP362 for Python 2.6, 2.7 and 3.2+")
    (description
     "Python function signatures from PEP362 for Python 2.6, 2.7 and 3.2+")
    (license #f)))

(define-public python-xarray
  (package
    (name "python-xarray")
    (version "2023.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xarray" version))
       (sha256
        (base32
         "1jic3g3b12hi10l939awb3d76qd4cwyczs9axwcnjbh71fv78p7h"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-packaging" ,python-packaging)))
    (arguments
     `(#:tests? #f)) ; no tests
    (home-page "https://github.com/pydata/xarray")
    (synopsis
     "N-D labeled arrays and datasets in Python")
    (description
     "N-D labeled arrays and datasets in Python")
    (license #f)))

(define-public python-sparse
  (package
    (name "python-sparse")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sparse" version))
       (sha256
        (base32
         "1600xad37mff46xg80cy6bi3l2n6jm69j7sl19rzdmkcgyijfn2z"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numba" ,python-numba)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-black" ,python-pytest-black)
       ("python-pytest-cov" ,python-pytest-cov)))
    ;; (arguments '(#:tests? #f))
    (home-page "https://github.com/pydata/sparse/")
    (synopsis "Sparse n-dimensional arrays")
    (description "Sparse n-dimensional arrays")
    (license #f)))

(define-public python-pint
  (package
    (name "python-pint")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pint" version))
       (sha256
        (base32
         "0rv0cbala7ibjbaf6kkcn0mdhqdbajnvlcw0f15gwzfwg10g0z1q"))))
    (build-system python-build-system)
    (native-inputs (list python-wheel python-setuptools-scm))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/hgrecco/pint")
    (synopsis "Physical quantities module")
    (description "Physical quantities module")
    (license license:bsd-3)))

(define-public python-colorhash
  (package
    (name "python-colorhash")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colorhash" version))
       (sha256
        (base32
         "0ywwhs47d5izwbi8nrf8fwx5zp9zjyd4y9rmkpb4jjgbvsd76g8l"))))
    (build-system python-build-system)
    (home-page
     "https://bitbucket.org/fk/python-color-hash")
    (synopsis "Generate a color based on a value")
    (description "Generate a color based on a value")
    (license license:expat)))

(define-public python-arpeggio
  (package
    (name "python-arpeggio")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Arpeggio" version))
       (sha256
        (base32
         "0ggdsck1wpladd5bh9drhkmm86bblgk2wagrhn3sdf4v04wkic6n"))))
    (build-system python-build-system)
    (native-inputs (list python-wheel python-pytest-runner python-pytest))
    (home-page "https://github.com/textX/Arpeggio")
    (synopsis "Packrat parser interpreter")
    (description "Packrat parser interpreter")
    (license license:expat)))

(define-public python-textx
  (package
    (name "python-textx")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "textX" version))
       (sha256
        (base32
         "1ymhz9lf7nzwmpvfgd2l73vsahjw4xq3fc7hya5iz9x41q4pvyz2"))))
    (build-system python-build-system)
    (native-inputs `(("python-wheel" ,python-wheel)))
    (propagated-inputs
     `(("python-arpeggio" ,python-arpeggio)
       ("python-click" ,python-click)))
    (home-page "https://github.com/textX/textX")
    (synopsis
     "Meta-language for DSL implementation inspired by Xtext")
    (description
     "Meta-language for DSL implementation inspired by Xtext")
    (license license:expat)))

(define (make-linux-module-builder linux)
  (package
    (inherit linux)
    (name (string-append (package-name linux) "-module-builder"))
    (inputs
     `(("linux" ,linux)))
    (arguments
     (substitute-keyword-arguments (package-arguments linux)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda _
                (invoke "make" "modules_prepare")))
            (delete 'strip)             ;faster
            (replace 'install
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((out-lib-build (string-append #$output "/lib/modules/build")))
                  ;; Delete some huge items that we probably don't need.
                  ;; TODO: Only preserve the minimum, i.e. [Kbuild], Kconfig,
                  ;; scripts, include, ".config".
                  (copy-recursively "." out-lib-build)
                  (for-each (lambda (name)
                              (when (file-exists? name)
                                (delete-file-recursively name)))
                            (map (lambda (name)
                                   (string-append out-lib-build "/" name))
                                 '("arch"          ; 137 MB
                                   ;;"tools"       ; 44 MB built by our 'build phase
                                   "tools/testing" ; 14 MB
                                   "tools/perf"    ; 17 MB
                                   "drivers"       ; 600 MB
                                   "Documentation" ; 52 MB
                                   "fs"            ; 43 MB
                                   "net"           ; 33 MB
                                   "samples"       ; 2 MB
                                   "sound")))      ; 40 MB
                  ;; Reinstate arch/**/dts since "scripts/dtc" depends on it.
                  ;; Reinstate arch/**/include directories.
                  ;; Reinstate arch/**/Makefile.
                  ;; Reinstate arch/**/module.lds.
                  (for-each
                   (lambda (name)
                     (mkdir-p (dirname (string-append out-lib-build "/" name)))
                     (copy-recursively name
                                       (string-append out-lib-build "/" name)))
                   (append (find-files "arch" "^(dts|include)$"
                                       #:directories? #t)
                       (find-files "arch" "^(Makefile|module.lds)$")))
                  (let* ((linux #$(this-package-input "linux")))
                    (install-file (string-append linux "/System.map")
                                  out-lib-build)
                    (let ((source (string-append linux "/Module.symvers")))
                      (when (file-exists? source)
                        (install-file source out-lib-build)))))))))))))

(define-public python-chipsec
  (package
    (name "python-chipsec")
    (version "1.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chipsec/chipsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yqf308g7sp893g94mdzm6yddazf4vppxcgw29r56xxjm81hjj5b"))))
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
               (setenv "KSRC" kernel-src-dir))))
         (delete 'check)))))) ; broken for some reason

(define (setuptools-scm-version-setter v)
  `(lambda* (#:key inputs #:allow-other-keys)
     (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,v)
     #t))

(define-public python-amaranth
  (let ((commit "120375dabeceaeeb1cf3fcaff2d9217b3e513cb3"))
    (package
      (name "python-amaranth")
      (version (string-append "0.3+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/amaranth-lang/amaranth")
               (commit commit)))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         ;; (snippet
         ;;  '(begin
         ;;     (substitute* "setup.py"
         ;;       (("if git.exact:")
         ;;        "if git is not None and git.exact:")) ; we have no git folder
         ;;     #t))
         (sha256
          (base32
           "03d8r21y2q1bb3qksg26v9920d17mkq3rdpvjqk4f9si8dix79gm"))))
      (build-system pyproject-build-system)
      (native-inputs (list python-pdm-backend python-pytest))
      (arguments
       `(#:phases
         ,#~(modify-phases %standard-phases
             (add-before 'build 'pretend-version
               (lambda _
                 (setenv "PDM_BUILD_SCM_VERSION" #$version))))))
         ;; #:tests? #f))
      (inputs `(("yosys" ,yosys-git)
                ("symbiyosys" ,symbiyosys)))
      (propagated-inputs
       `(("python-jinja2" ,python-jinja2)
         ("python-pyvcd" ,python-pyvcd)
         ("python-click" ,python-click)
         ("python-paramiko" ,python-paramiko)))
      (home-page "https://amaranth-lang.org/")
      (synopsis
       "Python toolbox for building complex digital hardware")
      (description
       "Python toolbox for building complex digital hardware")
      (license license:bsd-3))))


(define-public python-amaranth-boards
  (let ((commit "54000b09498080706152bbb8782f68b8efa0ad33"))
    (package
      (name "python-amaranth-boards")
      (version (string-append "0.0+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/amaranth-lang/amaranth-boards")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "047q9h5cphd98j41bmvnrprsymggc9z1rjbvs142jws2npcsvx28"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-amaranth" ,python-amaranth)))
      (arguments `(#:tests? #f))                  ; kind of super broken?
      (home-page "https://amaranth-lang.org/")
      (synopsis
       "Board and connector definitions for Amaranth")
      (description
       "Board and connector definitions for Amaranth")
      (license license:bsd-3))))

(define-public python-amaranth-stdio
  (let ((commit "21f44f5f712a149b97dd4f6b534417679c7dfc27"))
    (package
      (name "python-amaranth-stdio")
      (version (string-append "0.0+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/amaranth-lang/amaranth-stdio")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "09axmgv114jfk70ijyz0kb5rw02rawkhx1z5lb5fc774gkry9id7"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-amaranth" ,python-amaranth)))
      (arguments `(#:tests? #f))                  ; kind of super broken?
      (home-page "https://amaranth-lang.org/")
      (synopsis "Industry standard I/O for Amaranth")
      (description "Industry standard I/O for Amaranth")
      (license license:bsd-3))))

(define-public python-amaranth-soc
  (let ((commit "2b37115c7b996ad0befc73837ed16683dd07f06b"))
    (package
      (name "python-amaranth-soc")
      (version (string-append "0.0+g" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/amaranth-lang/amaranth-soc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0m1nk63ybfzmv99gmn9y8xf858gk1v1xl25r9113cvdcxndfxd93"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-amaranth" ,python-amaranth)))
      (arguments `(#:tests? #f))                  ; kind of super broken?
      (home-page "https://amaranth-lang.org/")
      (synopsis "System on Chip toolkit for Amaranth")
      (description "System on Chip toolkit for Amaranth")
      (license license:bsd-3))))


(define-public python-asttokens
  (package
    (name "python-asttokens")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asttokens" version))
       (sha256
        (base32
         "0b756m6xf1h473cy8kmfsd5vgw3jk9gwfvw2s7aazgxfbl8iyyz2"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-six" ,python-six)))
    (native-inputs
     `(("python-astroid" ,python-astroid)
       ("python-pytest" ,python-pytest)
       ("python-wheel" ,python-wheel)
       ("python-toml" ,python-toml)
       ("python-setuptools-scm" ,python-setuptools-scm)))
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
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "executing" version))
       (sha256
        (base32
         "05qhbvfk2bmkz6gvvgl9arc0b7h7mypdb028jh6lyslis6lyznlq"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-wheel" ,python-wheel)
       ("python-toml" ,python-toml)
       ("python-pytest" ,python-pytest)
       ("python-littleutils" ,python-littleutils)))
    (inputs `(("python-asttokens" ,python-asttokens)))
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
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "varname" version))
       (sha256
        (base32
         "1lizldli4g3jn3siacwn4fblgr6k34jdszhnpji37gjlca5gb9xk"))))
    (build-system pyproject-build-system)
    (native-inputs (list poetry))
    (arguments `(#:tests? #f))                  ; kind of super broken?
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
  (let ((commit "f0f140c83c766c71368a457d035ea06da34ecf2f"))
    (package
      (name "symbiyosys")
      (version (string-append "2023.12.07-" (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/SymbiYosys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rrcac8c9a342gb5zihz8041w7sffmhcw69rhjfqkhpnm20ckl9y"))))
      (inputs `(("python" ,python)
                ("yosys" ,yosys-git)))
      (propagated-inputs `(("yices" ,yices)))
      (arguments
       `(#:make-flags `(,(string-append "PREFIX=" %output))
         #:tests? #f
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
                                 (delete 'build))))     ; no compilation
                    
      (build-system gnu-build-system)
      (home-page "https://github.com/YosysHQ/SymbiYosys")
      (synopsis "SymbiYosys (sby) -- Front-end for Yosys-based formal verification flows")
      (description "SymbiYosys (sby) is a front-end driver program for Yosys-based formal hardware verification flows.")
      (license license:isc))))

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

(define-public python-prompt-toolkit
  (package
    (name "python-prompt-toolkit")
    (version "3.0.38")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "prompt_toolkit" version))
              (sha256
               (base32
                "16sv7mb9ixw6z237m1b20v6kwh0bgnsfqpz0pp43i6laad85vb13"))))
    (build-system python-build-system)
    (propagated-inputs (list python-wcwidth))
    (home-page "https://github.com/prompt-toolkit/python-prompt-toolkit")
    (synopsis
     "Library for building powerful interactive command lines in Python")
    (description
     "Library for building powerful interactive command lines in Python")
    (license #f)))

(define-public python-pymodbus
  (package
   (name "python-pymodbus")
   (version "3.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pymodbus" version))
     (sha256
      (base32
       "12ah6l30jcbgnxqzc0z324l76ymvyhagaq563g25lpl785dzaikg"))))
   (build-system python-build-system)
   (arguments '(#:tests? #f))
   (propagated-inputs
    (list python-pyserial python-six python-click python-typer python-prompt-toolkit python-pygments python-aiohttp python-pyserial-asyncio))
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
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "solaredge_modbus" version))
       (sha256
        (base32
         "0l12n69ry6s3fkk20l6fwwxgvp9pmj0cx9jar4x3xkvmp90ls61d"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pymodbus" ,python-pymodbus)))
    (home-page
     "https://github.com/nmakel/solaredge_modbus")
    (synopsis "SolarEdge Modbus parser library")
    (description "SolarEdge Modbus parser library")
    (license license:expat)))

(define-public python-openant
  (let* ((commit "8d2c6a0e91d90da38e1e48f32de62cf2aae37075")
         (version (string-append "1.2+" (string-take commit 9))))
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
           "1cm3wv8hsssqjnj2yammffhwcl965dl6jqvgd1pnmpknxay94q3c"))))
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
    (arguments '(#:tests? #f))))

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
       ("python-shapely" ,python-shapely)))
    (arguments `(#:tests? #f))
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
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "loky" version))
       (sha256
        (base32
         "0wqy5csmwxzbynk9nx0fdwbs9mhxj2mwvii90nl3s683jvc0rqfv"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cloudpickle" ,python-cloudpickle)))
    (native-inputs (list python-psutil python-pytest))
    (arguments `(#:tests? #f))
    (home-page "https://github.com/joblib/loky/")
    (synopsis
     "A robust implementation of concurrent.futures.ProcessPoolExecutor")
    (description
     "A robust implementation of concurrent.futures.ProcessPoolExecutor")
    (license license:bsd-3)))

(define-public python-line-profiler
  (package
    (name "python-line-profiler")
    (version "4.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "line_profiler" version))
              (sha256
               (base32
                "0zky9758cjmgdb4m7xz3r5qcv8k8xb4cxv93pvg13n8rj6gfpcny"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-cython python-coverage python-ipython python-pytest
                         python-pytest-cov python-ubelt))
    (arguments `(#:tests? #f))
    (home-page "https://github.com/pyutils/line_profiler")
    (synopsis "Line-by-line profiler")
    (description "Line-by-line profiler")
    (license license:bsd-3)))

(define-public python-snakeviz
  (package
    (name "python-snakeviz")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "snakeviz" version))
       (sha256
        (base32
         "1ncl9kp9bli7ph5h5faarakkbjk6p25xjzpvnfs5q2ag603c15hd"))))
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


(define-public pmbootstrap
  (package
    (name "pmbootstrap")
    (version "1.50.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/postmarketOS/pmbootstrap")
                    (commit version)))
              (sha256
               (base32
                "0n26zbpw7nhyh9gq0ydgij7m4pgv2n5wlmyc7g2sr7gqhqv04j2j"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f ; TODO: many test fail
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'fix-paths
                          (lambda* _
                            (let ((git (string-append #$git "/bin/"))
                                  (procps (string-append #$procps "/bin"))
                                  (openssl (string-append #$openssl "/bin"))
                                  (sudo "/run/setuid-programs"))
                              (wrap-program (string-append #$output
                                                           "/bin/pmbootstrap")
                                            `("PATH" ":" suffix
                                              ,(list git procps openssl sudo))))))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "pytest")))))))
    (native-inputs (list python-pytest python-pyopenssl))
    (inputs (list git procps openssl sudo))
    (home-page "https://postmarketos.org")
    (synopsis "Build and flash tool for postmarketOS")
    (description
     "Bootstrap program that abstracts everything in chroots and therefore
basically runs on top of any Linux distribution. Features:
@enumerate
@item chroot setup (distro-independent QEMU user emulation
@item clean chroot shutdown (umount) and zapping
@item build software as packages
@item cross-compile all armhf-packages
@item effective caching out of the box (survives chroot zaps)
@item installation targets
@item flasher abstractions
@item logging
@item security
@end enumerate")
    (license license:gpl3+)))

(define-public python-bitstring
  (package
    (name "python-bitstring")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bitstring" version))
       (sha256
        (base32
         "01mpvjdws36scqs07frdi7d1n7gcm3a81rd5af2f577qdn7z06bp"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))
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

(define-public python-camel-converter
  (package
    (name "python-camel-converter")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "camel_converter" version))
              (sha256
               (base32
                "1sz6h1r1hwb45xq7j9h1c2863s1xvi3wbxidmqwmy93720fhw83z"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pydantic))
    (home-page "https://github.com/sanders41/camel-converter")
    (synopsis
     "Converts a string from snake case to camel case or camel case to snake case")
    (description
     "Converts a string from snake case to camel case or camel case to snake case")
    (license license:expat)))

(define-public python-meilisearch
  (package
   (name "python-meilisearch")
   (version "0.26.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "meilisearch" version))
     (sha256
      (base32
       "03d90cnqkyvb1jmms8lkf04slycky5dg1v041s6449mpyvmcazjr"))))
   (build-system pyproject-build-system)
   (arguments `(#:tests? #f))
   (propagated-inputs
    (list python-requests python-camel-converter))
   (home-page
    "https://github.com/meilisearch/meilisearch-python")
   (synopsis
    "The python client for MeiliSearch API.")
   (description
    "The python client for MeiliSearch API.")
   (license #f)))

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

(define-public python-proto-plus-1.22
  (package
    (name "python-proto-plus")
    (version "1.22.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "proto-plus" version))
              (sha256
               (base32
                "0r9i4js4v8zxnmwf1c6yfp5qcr61aa9kqmswnyarhkb3b8yxm30f"))))
    (build-system python-build-system)
    (propagated-inputs (list python-protobuf))
    (native-inputs (list python-google-api-core))
    (home-page "https://github.com/googleapis/proto-plus-python.git")
    (synopsis "Beautiful, Pythonic protocol buffers.")
    (description "Beautiful, Pythonic protocol buffers.")
    (license license:asl2.0)))

(define-public python-protobuf-4.22.1
  (package
    (name "python-protobuf")
    (version "4.22.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "protobuf" version))
              (sha256
               (base32
                "19q7mdr4gmw4daad25g81fy13kv3yz1zdcmdi3vfqc8wa1fsbryw"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://developers.google.com/protocol-buffers/")
    (inputs (list protobuf))
    (synopsis "")
    (description "")
    (license #f)))

(define-public python-google-auth-2.17
  (package
    (inherit python-google-auth)
    (version "2.17.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "google-auth" version))
              (sha256
               (base32
                "0nnm2lfnm57q2f1389wzlz1d6a5d2c6gp7cq1cmav0fkp939ndwg"))))))

(define-public python-google-api-core-2.11
  (package
    (inherit python-google-api-core)
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "google-api-core" version))
              (sha256
               (base32
                "08jch08wbki3yixy6c0p5hk8k6la3djh5cvk0pxbx840lgavb6sb"))))
   (propagated-inputs
    (modify-inputs
     (package-propagated-inputs python-google-api-core)
     (replace "python-google-auth" python-google-auth-2.17)
     (replace "python-protobuf" python-protobuf-4.22.1)
     (replace "python-proto-plus" python-proto-plus-1.22)))))

(define-public python-google-api-python-client
  (package
   (name "python-google-api-python-client")
   (version "2.83.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "google-api-python-client" version))
            (sha256
             (base32
              "1ksivmh0a197hkrcmks839sy25bsdycxnm5lcdrl5cnjnbqhjxfh"))))
   (build-system python-build-system)
   (propagated-inputs (list python-google-api-core-2.11 python-google-auth-2.17
                            python-google-auth-httplib2 python-httplib2
                            python-uritemplate))
   (native-inputs (list python-parameterized))
   (arguments `(#:tests? #f))           ; require internet connection
   (home-page "https://github.com/googleapis/google-api-python-client/")
   (synopsis "Google API Client Library for Python")
   (description "Google API Client Library for Python")
   (license license:asl2.0)))

(define-public python-grpcio-status
  (package
    (name "python-grpcio-status")
    (version "1.47.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "grpcio-status" version))
              (sha256
               (base32
                "007s12c9ka1ylb0ad4qblkm56d3b9bma7b1i3j0dhvscx09k5kn9"))))
    (build-system python-build-system)
    (propagated-inputs (list python-googleapis-common-protos python-grpcio
                             python-protobuf))
    (home-page "https://grpc.io")
    (synopsis "Status proto mapping for gRPC")
    (description "Status proto mapping for gRPC")
    (license #f)))

(define-public python-google-cloud-firestore
  (package
   (name "python-google-cloud-firestore")
   (version "2.11.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "google-cloud-firestore" version))
            (sha256
             (base32
              "1bhdlikslpy8vk2ymfwa66bbvmccnnnr2ihklx05gvm0f1gwa3cq"))))
   (build-system python-build-system)
   (propagated-inputs (list python-google-api-core-2.11 python-google-cloud-core
                            python-proto-plus-1.22 python-protobuf-4.22.1 python-grpcio-status))
   (arguments `(#:tests? #f))           ; really fucked
   (home-page "https://github.com/googleapis/python-firestore")
   (synopsis "Google Cloud Firestore API client library")
   (description "Google Cloud Firestore API client library")
   (license license:asl2.0)))

(define-public python-firebase-admin
  (package
   (name "python-firebase-admin")
   (version "6.1.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "firebase_admin" version))
            (sha256
             (base32
              "17g1j5j5pwzviiz0np25pvpamkdaan76ndrcgwiwcgcx7hwkxqaj"))))
   (build-system python-build-system)
   (propagated-inputs (list python-cachecontrol
                            python-google-api-core-2.11
                            python-google-api-python-client
                            python-google-cloud-firestore
                            python-google-cloud-storage
                            python-pyjwt))
   (home-page "https://firebase.google.com/docs/admin/setup/")
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
   (propagated-inputs (list python-setuptools-57))
   (arguments `(#:tests? #f))
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
     (source (origin
               (method url-fetch)
               (uri (pypi-uri "stdlib-list" version))
               (sha256
                (base32
                 "17vdn4q0sdlndc2fr9svapxx6366hnrhkn0fswp1xmr0jxqh7rd1"))))
     (build-system python-build-system)
     (native-inputs (list python-sphinx))
     (home-page "https://github.com/jackmaney/python-stdlib-list")
     (arguments `(#:tests? #f))
     (synopsis "A list of Python Standard Libraries (2.6-7, 3.2-9).")
     (description
      "This package provides a list of Python Standard Libraries (2.6-7, 3.2-9).")
     (license license:expat)))

(define-public python-pydeps
  (package
   (name "python-pydeps")
   (version "1.11.2")
   (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pydeps" version))
      (sha256
       (base32
        "0h869792bllfvaq8r17xw9jc7wsdygxkh6a2w04h8g7rwy1rxih6"))))
   (build-system python-build-system)
   (propagated-inputs (list python-stdlib-list))
   (native-inputs (list python-pytest))
   (arguments `(#:tests? #f))
   (home-page "https://github.com/thebjorn/pydeps")
   (synopsis "Display module dependencies")
   (description "Display module dependencies")
   (license license:bsd-3)))


(define-public python-openstep-parser
  (package
    (name "python-openstep-parser")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "openstep_parser" version))
        (sha256
          (base32
            "0n0npqm30rw7xxzzf533g1c7zyh93xy6c3ybf321iiwaqzcpyg7h"))))
    (build-system python-build-system)
    (native-inputs (list python-coverage python-nose))
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
    (version "3.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pbxproj" version))
        (sha256
          (base32
            "1cnkhlyv7ipf1w8rpihcc5ipqmn26f75d4dfxb3jp0sjn0mxzscx"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-docopt" ,python-docopt)
        ("python-openstep-parser"
         ,python-openstep-parser)))
    (arguments `(#:tests? #f))
    (home-page
      "http://github.com/kronenthaler/mod-pbxproj")
    (synopsis
      "XCode Project manipulation library for Python")
    (description
      "XCode Project manipulation library for Python")
    (license license:expat)))

(define-public python-lxml-4.9
  (package
    (inherit python-lxml)
    (version "4.9.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lxml" version))
              (sha256
               (base32
                "0rsvhd03cv7fczd04xqf1idlnkvjy0hixx2p6a5k6w5cnypcym94"))))))

(define-public python-docx2python
  (package
    (name "python-docx2python")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "docx2python" version))
              (sha256
               (base32
                "07ms7i5r1zmxyxlifabw2ywawkaxiwf80nsq2f7ya3zng0rnvas6"))))
    (build-system python-build-system)
    (propagated-inputs (list python-lxml-4.9))
    (home-page "")
    (synopsis "Extract content from docx files")
    (description "Extract content from docx files")
    (license license:expat)))


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

(define-public python-randomize
  (package
    (name "python-randomize")
    (version "0.14")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "randomize" version))
        (sha256
          (base32
            "0srvk1rvlppvhpzj7p2dims1nx8hlycrqad0j748d5wgxlsp95zy"))))
    (build-system python-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                  (delete 'check))))
    (home-page
      "https://github.com/nloadholtes/nose-randomize")
    (synopsis
      "Randomize the order of the tests within a unittest.TestCase class")
    (description
      "Randomize the order of the tests within a unittest.TestCase class")
    (license license:lgpl2.0)))

(define-public python-reactivex
  (package
    (name "python-reactivex")
    (version "4.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "reactivex" version))
              (sha256
               (base32
                "1s041rjqmiyqr4qrjvzj06isg3wczr9scj43vxv93ar221cyc4p9"))))
    (build-system python-build-system)
    (propagated-inputs (list python-typing-extensions))
    (native-inputs (list python-wheel))
    (arguments `(#:tests? #f))
    (home-page "http://reactivex.io")
    (synopsis "ReactiveX (Rx) for Python")
    (description "ReactiveX (Rx) for Python")
    (license license:expat)))

(define-public python-jinja2-3.1.2
  (package
    (inherit python-jinja2)
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Jinja2" version))
              (sha256
               (base32
                "0lp86yadzf8dph67f6g3yxmvnhrzzi863z58jmsrx2j059q1ld9i"))))))

(define-public python-influxdb-client
  (package
   (name "python-influxdb-client")
   (version "1.36.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "influxdb_client" version))
            (sha256
             (base32
              "1z3vnlakd3w4jghiili5q55mqbpla1dr64b4j77hwgqkyzs4lrar"))))
   (build-system python-build-system)
   (propagated-inputs (list python-certifi python-dateutil python-reactivex
                          python-setuptools python-urllib3))
   (native-inputs (list python-aioresponses
                      python-coverage
                      python-flake8
                      python-httpretty
                      python-jinja2-3.1.2
                      python-nose
                      python-pluggy
                      python-psutil
                      python-py
                      python-pytest
                      python-pytest-cov
                      python-pytest-timeout
                      python-randomize
                      python-sphinx
                      python-sphinx-rtd-theme
                      python-wheel))
   (arguments `(#:tests? #f))           ; requires an ancient sphinx version (1.8.5) for some reason?
   (home-page "https://github.com/influxdata/influxdb-client-python")
   (synopsis "InfluxDB 2.0 Python client library")
   (description "InfluxDB 2.0 Python client library")
   (license #f)))

(define-public python-fpzip
  (package
    (name "python-fpzip")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fpzip" version))
        (sha256
          (base32 "032v7fcc53mcg3w1xvnfi05vali0xsi59i04dlimm8pch80amby8"))))
    (build-system python-build-system)
    (propagated-inputs (list python-numpy python-pbr))
    (home-page "https://github.com/seung-lab/fpzip/")
    (synopsis
      "Numpy wrapper for fpzip algorithm (P. Lindstrom & M. Isenburg, 2006)")
    (description
      "Numpy wrapper for fpzip algorithm (P.  Lindstrom & M.  Isenburg, 2006)")
    (license #f)))

(define-public python-find-libpython
  (package
    (name "python-find-libpython")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "find_libpython" version))
              (sha256
               (base32
                "1rg9fqrgid6v9j72zfxjzvqp22n9x6h1amfbcv06vbbzmzcyazvf"))))
    (build-system python-build-system)
    (home-page "https://github.com/ktbarrett/find_libpython")
    (synopsis
     "Finds the libpython associated with your environment, wherever it may be hiding")
    (description
     "Finds the libpython associated with your environment, wherever it may be hiding")
    (license license:expat)))

(define-public python-cocotb
  (package
   (name "python-cocotb")
   (version "1.7.2")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "cocotb" version))
            (sha256
             (base32
              "0l160106nxkz9yn2ngl5xdnl2dir7b9m3jhg1vs3y2xxjkmr564m"))))
   (build-system python-build-system)
   (propagated-inputs (list python-find-libpython))
   (arguments `(#:tests? #f))
   (home-page "https://docs.cocotb.org")
   (synopsis
    "cocotb is a coroutine based cosimulation library for writing VHDL and Verilog testbenches in Python.")
   (description
    "cocotb is a coroutine based cosimulation library for writing VHDL and Verilog
testbenches in Python.")
   (license license:bsd-3)))

(define-public python-cocotb-test
  (package
    (name "python-cocotb-test")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cocotb-test" version))
              (sha256
               (base32
                "01j05i3acdzxcza9nv72jcj1z1pf2ag2f3kvbdchwfc0lq7yhbg3"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cocotb python-pytest))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public python-cocotb-bus
  (package
    (name "python-cocotb-bus")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cocotb-bus" version))
              (sha256
               (base32
                "13dav1nwwy0mdlkvk4glm5nj1h1fzfrl2yw7r1lq9lha1r5sm5x1"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cocotb))
    (home-page "https://github.com/cocotb/cocotb-bus")
    (synopsis "")
    (description "")
    (license #f)))


(define-public python-cocotbext-axi
  (package
    (name "python-cocotbext-axi")
    (version "0.1.24")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cocotbext-axi" version))
              (sha256
               (base32
                "14q5jqnqcz6q17j339s690f46in3qmdpql16d0bk7224z752vmiy"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cocotb python-cocotb-bus))
    (native-inputs (list python-cocotb-test python-pytest))
    (home-page "https://github.com/alexforencich/cocotbext-axi")
    (synopsis "AXI, AXI lite, and AXI stream modules for cocotb")
    (description "AXI, AXI lite, and AXI stream modules for cocotb")
    (license license:expat)))

(define-public python-cocotbext-eth
  (package
   (name "python-cocotbext-eth")
   (version "0.1.20")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "cocotbext-eth" version))
            (sha256
             (base32
              "17f30qs6g3fx7ini81v5n32kcmzxn6rzswmqklfkjp59an4c4w0a"))))
   (build-system python-build-system)
   (propagated-inputs (list python-cocotb python-cocotbext-axi))
   (native-inputs (list python-cocotb-test python-pytest))
   (home-page "https://github.com/alexforencich/cocotbext-eth")
   (synopsis "Ethernet interface modules for cocotb")
   (description "Ethernet interface modules for cocotb")
   (license license:expat)))


(define-public python-pybtex-docutils
  (package
    (name "python-pybtex-docutils")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pybtex-docutils" version))
              (sha256
               (base32
                "05m09byzhq8p1pax66g69z9n3l1jwfc3l1zh62ndb3s9dlxkbaj3"))))
    (build-system python-build-system)
    (propagated-inputs (list python-docutils python-pybtex))
    (home-page "https://github.com/mcmtroffaes/pybtex-docutils")
    (synopsis "A docutils backend for pybtex.")
    (description "This package provides a docutils backend for pybtex.")
    (license license:expat)))
(define-public python-sphinxcontrib-bibtex
  (package
    (name "python-sphinxcontrib-bibtex")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-bibtex" version))
              (sha256
               (base32
                "1ahfn3szw9l80isps9w269v9maink6zjclw78gr89qp2n1fjxd3i"))))
    (build-system python-build-system)
    (propagated-inputs (list python-dataclasses
                             python-docutils
                             python-importlib-metadata
                             python-pybtex
                             python-pybtex-docutils
                             python-wheel
                             python-pytest-cov
                             python-sphinx))
    (home-page "https://github.com/mcmtroffaes/sphinxcontrib-bibtex")
    (synopsis "Sphinx extension for BibTeX style citations.")
    (description "Sphinx extension for BibTeX style citations.")
    (license license:bsd-3)))
(define-public python-tokenize-rt-5
  (package
    (name "python-tokenize-rt")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tokenize_rt" version))
              (sha256
               (base32
                "0h07s44585m1f9mpdxgm82i61h8zhvm1s5w50hnk34c47q6bqq1i"))))
    (build-system python-build-system)
    (home-page "https://github.com/asottile/tokenize-rt")
    (synopsis "A wrapper around the stdlib `tokenize` which roundtrips.")
    (description
     "This package provides a wrapper around the stdlib `tokenize` which roundtrips.")
    (license license:expat)))
(define-public python-pyupgrade
  (package
    (name "python-pyupgrade")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyupgrade" version))
              (sha256
               (base32
                "0kxiiangyliy8jfl8v735n63v124x6kjwjvilnaanz485mvf5hbf"))))
    (build-system python-build-system)
    (propagated-inputs (list python-tokenize-rt-5))
    (home-page "https://github.com/asottile/pyupgrade")
    (synopsis "A tool to automatically upgrade syntax for newer versions.")
    (description
     "This package provides a tool to automatically upgrade syntax for newer versions.")
    (license license:expat)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))))
(define-public python-flynt
  (package
    (name "python-flynt")
    (version "0.76")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flynt" version))
              (sha256
               (base32
                "0fhr2djg0z3pd622crc4pkbvm776iknrjs9z40hqr7paa2jwb6bs"))))
    (build-system python-build-system)
    (propagated-inputs (list python-astor python-tomli))
    (home-page "https://github.com/ikamensh/flynt")
    (synopsis
     "CLI tool to convert a python project's %-formatted strings to f-strings.")
    (description
     "CLI tool to convert a python project's %-formatted strings to f-strings.")
    (license license:expat)))
(define-public python-biblib-simple
  (package
    (name "python-biblib-simple")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "biblib-simple" version))
              (sha256
               (base32
                "1gqnr11vdn6f2z0dip209v9r5riawbgqp9xsykq3b2a4zx6mrjkd"))))
    (build-system python-build-system)
    (home-page "https://github.com/colour-science/biblib")
    (synopsis "Simple, correct BibTeX parser and algorithms")
    (description "Simple, correct BibTeX parser and algorithms")
    (license #f)))
(define-public python-colour-science
  (package
    (name "python-colour-science")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "colour_science" version))
              (sha256
               (base32
                "0rlvsr6v6jhnqj0slrd24xjkaplkw0qjz53wrpmzxnh4f73d9a19"))))
    (build-system python-build-system)
    (propagated-inputs (list python-biblib-simple
                             python-black
                             python-coverage
                             python-coveralls
                             python-flake8
                             python-flynt
                             python-imageio
                             python-invoke
                             jupyter
                             python-matplotlib
                             python-mypy
                             python-networkx
                             python-numpy
                             python-pandas
                             python-pre-commit
                             python-pydata-sphinx-theme
                             python-pydocstyle
                             python-pygraphviz
                             python-pytest
                             python-pytest-cov
                             python-pyupgrade
                             python-restructuredtext-lint
                             python-scikit-learn
                             python-scipy
                             python-sphinx
                             python-sphinxcontrib-bibtex
                             python-toml
                             python-tqdm
                             python-trimesh
                             python-twine
                             python-typing-extensions))
    (arguments `(#:tests? #f))
    (home-page "https://www.colour-science.org/")
    (synopsis "Colour Science for Python")
    (description "Colour Science for Python")
    (license #f)))

(define-public python-git-review-2.3
  (package
    (inherit python-git-review)
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git-review" version))
       (sha256
        (base32 "0kq16qvd57jwb19jhnlbpfsp0sr8981jnplbng5yddpcdq9kis94"))))))

(define-public python-waf-visionary
  (package
    (inherit python-waf)
    (name "0kq16qvd57jwb19jhnlbpfsp0sr8981jnplbng5yddpcdq9kis94python-waf-visionary")
    (version "2.0.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/electronicvisions/waf")
             (commit "c77b023482ac2a3a3147c8dcbe6be59fc0e8c3a2")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cx5rr0q0js0dkxj2bbbvmf48y7xnzcja8riv4vymfrzh2pl1vk0"))))
    (build-system python-build-system)))

(define-public python-socketio-client
  (package
    (name "python-socketio-client")
    (version "0.5.7.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "socketIO-client" version))
              (sha256
               (base32
                "0xg546g3dasgwk4zzz0ny1h1p8as2pxia0n4398xsml7ip64p84b"))))
    (build-system python-build-system)
    (propagated-inputs (list python-requests python-six
                             python-websocket-client python-wheel))
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check))))
    (home-page "https://github.com/invisibleroads/socketIO-client")
    (synopsis "A socket.io client library")
    (description "This package provides a socket.io client library")
    (license license:expat)))

(define-public overleaf-sync
  (package
    (name "overleaf-sync")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moritzgloeckl/overleaf-sync")
             (commit "aa62165eb9eba48f8b8bf3d93358f9feed0bf5a9")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0h0wspvhb5qr1n7b95pgdx6a1fwcqfsd6hbn2xwa30iwn5kahq00"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyside-6 python-yaspin python-socketio python-socketio-client))
    (native-inputs (list python-flit))
    (home-page "https://github.com/moritzgloeckl/overleaf-sync")
    (synopsis "Overleaf Two-Way Synchronization")
    (description "Overleaf Two-Way Synchronization")
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (delete 'check) (delete 'sanity-check))))
    (license license:expat)))

(define-public python-mypy-extensions-next
  (package
    (name "python-mypy-extensions")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mypy_extensions" version))
              (sha256
               (base32
                "10h7mwjjfbwxzq7jzaj1pnv9g6laa1k0ckgw72j44160bnazinvm"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/python/mypy_extensions")
    (synopsis
     "Type system extensions for programs checked with the mypy type checker.")
    (description
     "Type system extensions for programs checked with the mypy type checker.")
    (license license:expat)))

(define-public python-mypy-next
  (package
    (name "python-mypy")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mypy" version))
              (sha256
               (base32
                "1lbjdd1d3j492w5sz3llwx6s0199cp5bxysarczy4jkp1m0l02pp"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-pytest-xdist))
    (propagated-inputs (list python-mypy-extensions-next python-tomli
                             python-typed-ast python-typing-extensions))
    (home-page "http://www.mypy-lang.org/")
    (arguments '(#:tests? #f))
    (synopsis "Optional static typing for Python")
    (description "Optional static typing for Python")
    (license license:expat)))

(define-public python-jaxopt
  (package
    (name "python-jaxopt")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jaxopt" version))
       (sha256
        (base32 "0ih6k5i0v107qpa94p6w90d3fsb0mx2n4q4r2qlz790mz6kdy1jb"))))
    (build-system pyproject-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs (list python-jax python-jaxlib python-numpy
                             python-scipy))
    (home-page "https://github.com/google/jaxopt")
    (synopsis
     "Hardware accelerated, batchable and differentiable optimizers in JAX.")
    (description
     "Hardware accelerated, batchable and differentiable optimizers in JAX.")
    (license license:asl2.0)))
