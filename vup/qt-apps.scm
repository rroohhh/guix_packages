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
   (version "1.7+c90dbb3a")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/shundhammer/qdirstat")
           (commit "c90dbb3aedcaa178b6c064616fba840e88fb8846")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1fa2dan5vg4za6v24hysnaiypa858s9m72lcqwbm7lzrfwx26rpd"))))
   (build-system qt-build-system)
   (arguments '(#:phases
                (modify-phases %standard-phases
                  (delete 'check)
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "qmake"
                              (string-append "INSTALL_PREFIX=" (assoc-ref outputs "out"))))))))
   (inputs
    `(("qtbase" ,qtbase)
      ("zlib" ,zlib)))
   (home-page "https://github.com/shundhammer/qdirstat")
   (synopsis "QDirStat - Qt-based directory statistics (KDirStat without any KDE - from the original KDirStat author)")
   (description "QDirStat - Qt-based directory statistics (KDirStat without any KDE - from the original KDirStat author)")
   (license license:gpl2)))
