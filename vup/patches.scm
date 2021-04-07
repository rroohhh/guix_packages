(define-module (vup patches)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages) ;; search-patches
  #:use-module (gnu packages xorg))

;; patch xkeyboard-config to include my vup layout
(let* 
  ((old-xkeyboard-config (module-ref (resolve-module '(gnu packages xorg)) 'xkeyboard-config))
   (old-source (package-source old-xkeyboard-config))
   (patched-pkg (package (inherit old-xkeyboard-config)
	          (source (origin (inherit old-source)
				  (patches (append (origin-patches old-source) (origin (method url-fetch) (uri "https://raw.githubusercontent.com/rroohhh/guix_packages/825ffca/vup.patch") (sha256 "0ayvi8js754vwfq2zlf5bmv3fq0f5phdl5vrh0b8fws0vmni977w")))))))))

  (if (not (package-replacement old-xkeyboard-config))
    (module-define! (resolve-module '(gnu packages xorg)) 'xkeyboard-config
      (package (inherit old-xkeyboard-config)
        (replacement patched-pkg)))))
