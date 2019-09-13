(define-module (vup i3-gaps)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages autotools))

(define-public i3-gaps
  (package
   (inherit i3-wm)
   (name "i3-gaps")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/airblader/i3")
           (commit "474f57f0d8ae57fdf31cb5367008cfdacca1b91c")))
     (file-name (git-file-name name (package-version i3-wm)))
     (sha256
      (base32 "1wxn50mhmfy7z7b6l2m4wix797yrdzm238kzl23yql9x2ckg8wca"))))
   (native-inputs 
     (append `(("autoconf" ,autoconf) ("automake" ,automake)) (package-native-inputs i3-wm))))) ;; needed because they are not included in the
                                                                                                ;; git repo (but in the tarballs i3-proper offers
