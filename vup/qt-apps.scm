(define-module (vup qt-apps)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt)
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
