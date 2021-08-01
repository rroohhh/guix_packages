(define-module (vup qt-apps)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages compression))

(define-public qdirstat
  (package
   (name "qdirstat")
   (version "1.7+844ee2ff")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/shundhammer/qdirstat")
           (commit "844ee2ffab029c949de5511b7e60f9621ad89862")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0p78x77g8vzr9wl0rwi84wwf65vwkrvcpn1rs7y4sgpb2ycyqawf"))))
   (build-system qt-build-system)
   (arguments '(#:phases
                (modify-phases %standard-phases
                  (delete 'check)
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "qmake"
                              (string-append "INSTALL_PREFIX=" (assoc-ref outputs "out"))))))))
   (inputs
    `(("qtbase" ,qtbase-5)
      ("zlib" ,zlib)))
   (home-page "https://github.com/shundhammer/qdirstat")
   (synopsis "QDirStat - Qt-based directory statistics (KDirStat without any KDE - from the original KDirStat author)")
   (description "QDirStat - Qt-based directory statistics (KDirStat without any KDE - from the original KDirStat author)")
   (license license:gpl2)))


(define-public klayout
  (package
   (name "klayout")
   (version "0.27.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/KLayout/klayout")
           (commit "v0.27.3")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0kd0sspyhs29ql7a7zs6ynh5qi0hz892gy0iqqgibbks3nhx03za"))))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'check)
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (mkdir "build")
            (chdir "build")
            (invoke "qmake"
                    (string-append "PREFIX=" (assoc-ref outputs "out"))
                    "HAVE_QT5=1"
                    "HAVE_QT=0"
                    "HAVE_QT_NETWORK=1"
                    "HAVE_QT_UITOOLS=1"
                    "HAVE_QT_SVG=1"
                    "HAVE_QT_SQL=1"
                    "HAVE_QT_PRINTSUPPORT=1"
                    "HAVE_QT_XML=1"
                    "HAVE_QT_DESIGNER=1"
                    "HAVE_QT_MULTIMEDIA=1"
                    "HAVE_QTBINDINGS=1"
                    "HAVE_QT=1"
                    "HAVE_PYTHON=0"
                    "HAVE_RUBY=0"
                    "HAVE_64BIT_COORD=1"
                    "CONFIG+=release"
                    "-recursive"
                    "../src/klayout.pro")
            #t)))))
   (build-system qt-build-system)

   (inputs
    `(("qtbase" ,qtbase-5)
      ("qtsvg" ,qtsvg)
      ("qtxmlpatterns" ,qtxmlpatterns)
      ("qttools" ,qttools)
      ("qtmultimedia" ,qtmultimedia)
      ("python" ,python)
      ("zlib" ,zlib)))
   (home-page "https://github.com/KLayout/klayout")
   (synopsis "KLayout")
   (description "KLayout")
   (license #f)))
