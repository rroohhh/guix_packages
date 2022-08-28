(define-module (vup glscopeclient)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages opencl)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix licenses:))

(define-public ffts
  (let ((commit "fe86885ecafd0d16eb122f3212403d1d5a86e24e"))
    (package
     (name "ffts")
     (version (string-append "0.0-" (string-take commit 9)))
     (source
      (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/anthonix/ffts")
         (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wij7n93ak9dxw512xp7nyrqs28m29swlwksx4rds6fa8s85gc3a"))))
     (build-system cmake-build-system)
     (arguments
      `(#:configure-flags '("-DENABLE_SHARED=ON")
        #:phases (modify-phases %standard-phases
                   (delete 'check))))
     (home-page "https://anthonix.com/ffts")
     (synopsis "The Fastest Fourier Transform in the South")
     (description "The Fastest Fourier Transform in the South")
     (license #f)))) ; whatever it is

(define-public glscopeclient
  (let ((commit "3686a915f12ee02460d9f50a3fe7db15307720fd"))
    (package
     (name "glscopeclient")
     (version (string-append "0.0-" (string-take commit 9)))
     (source
      (origin
       (method git-fetch)
       (uri
        (git-reference
         (recursive? #t)
         (url "https://github.com/azonenberg/scopehal-apps")
         (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "037ykay3vj81z16lfygfbpdfh7acqhvm48hkwh8c36jz590k35hc"))))
     (build-system cmake-build-system)
     (inputs `(("ffts" ,ffts) ("yaml" ,yaml-cpp) ("glew" ,glew) ("gtkmm" ,gtkmm-3) ("catch2" ,catch-framework2)
               ("opencl" ,opencl-icd-loader)))
               
     (native-inputs `(("pkg-config" ,pkg-config) ("git" ,git) ("opencl-headers" ,opencl-headers)))
     (arguments
      `(#:configure-flags '("-DCMAKE_BUILD_TYPE=RELEASE")
        #:imported-modules ((guix build glib-or-gtk-build-system)
                            ,@%cmake-build-system-modules)
        #:modules
        ((guix build utils)
         (guix build cmake-build-system)
         ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
        #:phases (modify-phases %standard-phases
                  (add-after 'wrap-program 'glib-or-gtk-wrap
                   (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
                  (add-after 'glib-or-gtk-wrap 'glib-or-gtk-compile-schemas
                   (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas)))))
     (home-page "https://github.com/azonenberg/scopehal-apps")
     (synopsis "glscopeclient and other client applications for libscopehal")
     (description "glscopeclient and other client applications for libscopehal")
     (license licenses:bsd-3)))) ; whatever it is
