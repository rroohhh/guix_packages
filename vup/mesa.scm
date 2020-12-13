(define-module (vup mesa)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages gl))

(define-public mesa-fixed
  (package
    (inherit mesa)
    (name "mesa-fixed")
    (source
     (origin (inherit (package-source mesa))
             (patches (append (origin-patches (package-source mesa)) (search-patches "mesa_drm_rdwr.patch")))))))
