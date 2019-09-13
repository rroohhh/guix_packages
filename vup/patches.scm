(define-module (vup patches)
  #:use-module (guix packages)
  #:use-module (gnu packages) ;; search-patches
  #:use-module (gnu packages xorg))

;; patch xkeyboard-config to include my vup layout
(let* 
  ((old-xkeyboard-config (module-ref (resolve-module '(gnu packages xorg)) 'xkeyboard-config))
   (old-source (package-source old-xkeyboard-config)))

  (module-define! (resolve-module '(gnu packages xorg)) 'xkeyboard-config
    (package (inherit old-xkeyboard-config)
      (source (origin 
                (inherit old-source)
                (patches (append (origin-patches old-source) (search-patches '("vup.patch")))))))))
