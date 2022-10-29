(define-module (vup horizon)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages engineering)
  #:use-module ((guix licenses) #:prefix license:))

(define-public opencascade-occt-fixed
  ((package-input-rewriting/spec `(("tbb" . ,(const tbb-2020)))) opencascade-occt))


(define-public horizon
  (let ((commit "750111e829cd6f3563ef8040c224ce147808c27d"))
    (package
      (name "horizon")
      (version (string-append "2.4.0+" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/horizon-eda/horizon")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0vx3ba4m1knjq53dylj9n3l38cnqihgj6x4jvkr0zmzspic4n6j3"))))
      (build-system glib-or-gtk-build-system)
      (inputs `(("pkg-config" ,pkg-config) ("util-linux" ,util-linux) ("yaml-cpp" ,yaml-cpp)
                ("sqlite" ,sqlite) ("gtkmm" ,gtkmm-3) ("curl" ,curl) ("glib" ,glib)
                ("glib:bin" ,glib "bin") ("libzip" ,libzip) ("libgit2" ,libgit2)
                ("glm" ,glm) ("librsvg" ,librsvg) ("zeromq" ,zeromq) ("python3" ,python)
                ("boost" ,boost) ("opencascade" ,opencascade-occt-fixed) ("cppzmq" ,cppzmq)
                ("podofo" ,podofo) ("coreutils" ,coreutils) ("hicolor-icon-theme" ,hicolor-icon-theme)
                ("librsvg" ,librsvg) ("gcc" ,gcc-11) ("libsigc++" ,libsigc++) ("glibmm" ,glibmm)
                ("libarchive" ,libarchive) ("libspnav" ,libspnav)))
      (arguments
       `(#:make-flags (list "GOLD=" "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (delete 'check) ; no tests?
                    (add-after 'unpack 'patch-makefile
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "Makefile"
                          (("/usr/bin/install") (string-append (assoc-ref inputs "coreutils") "/bin/install")))
                        #t))
                    (add-before 'build 'set-casroot
                      (lambda* (#:key inputs #:allow-other-keys)
                        (setenv "CASROOT" (assoc-ref inputs "opencascade"))
                        #t)))))
      (synopsis "Horizon is a free EDA package")
      (description "Horizon EDA is an Electronic Design Automation package supporting an integrated end-to-end workflow for printed circuit board design including parts management and schematic entry.")
      (home-page "https://horizon-eda.org/")
      (license license:gpl3))))


horizon
