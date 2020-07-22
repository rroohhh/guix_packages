(define-module (vup solvespace)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages gettext)
  #:use-module ((guix licenses) #:prefix license:))

(define-public solvespace
  (let ((commit "188b2e26cec71fa7de86fd8202932c79eeecf295"))
    (package
      (name "solvespace")
      (version (string-append "solvespace+" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/solvespace/solvespace")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0v6k7k9057rjpbd82jr47lx16cx9wn9a4xv8ks6nlgwwrzy2w5jj"))))
      (inputs `(("pkg-config" ,pkg-config) ("zlib" ,zlib) ("libpng" ,libpng)
                ("freetype" ,freetype) ("cairo" ,cairo) ("opengl" ,mesa)
                ("josn-c" ,json-c) ("libspnav" ,libspnav) ("gtkmm" ,gtkmm)
                ("gettext" ,gnu-gettext)))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-cmakefile
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "CMakeLists.txt"
                          (("include\\(GetGitCommitHash\\)") "# include(GetGitCommitHash)")
                          (("# set\\(GIT_COMMIT_HASH 0000000000000000000000000000000000000000\\)") (string-append "set(GIT_COMMIT_HASH" ,commit ")")))
                        #t))
                    (add-after 'install 'wrap-gsettings-schema-dir
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (wrap-program (string-append (assoc-ref outputs "out")
                                                     "/bin/solvespace")
                          ;; For GtkFileChooserDialog.
                          `("GSETTINGS_SCHEMA_DIR" =
                            (,(string-append (assoc-ref inputs "gtk+")
                                             "/share/glib-2.0/schemas"))))
                        #t)))))
      (synopsis "Parametric 2d/3d CAD")
      (description "Parametric 2d/3d CAD")
      (home-page "https://solvespace.com/")
      (license license:gpl3))))
