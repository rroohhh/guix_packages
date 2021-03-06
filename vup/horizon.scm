(define-module (vup horizon)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gtk)
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
  #:use-module ((guix licenses) #:prefix license:))

(define-public horizon
  (let ((commit "02c56ccb84cd4c2c0ad7e8c42041a5e646995953"))
    (package
      (name "horizon")
      (version (string-append "1.4.0+" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/horizon-eda/horizon")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "059bcqyfc02m19x277bvb4ddmf48p8ybd0x5rqi1xkgpggf88x4j"))))
      (build-system glib-or-gtk-build-system)
      (inputs `(("pkg-config" ,pkg-config) ("util-linux" ,util-linux) ("yaml-cpp" ,yaml-cpp)
                ("sqlite" ,sqlite) ("gtkmm" ,gtkmm) ("curl" ,curl) ("glib" ,glib)
                ("glib:bin" ,glib "bin") ("libzip" ,libzip) ("libgit2" ,libgit2)
                ("glm" ,glm) ("librsvg" ,librsvg) ("zeromq" ,zeromq) ("python3" ,python)
                ("boost" ,boost) ("opencascade" ,opencascade-oce) ("cppzmq" ,cppzmq)
                ("podofo" ,podofo) ("coreutils" ,coreutils) ("hicolor-icon-theme" ,hicolor-icon-theme)
                ("gdk-pixbuf" ,gdk-pixbuf+svg)))
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
