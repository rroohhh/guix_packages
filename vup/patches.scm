(define-module (vup patches)
  #:use-module (guix packages)
  #:use-module (guix download))
  ;; #:use-module (gnu packages) ;; search-patches
  ;#:use-module (gnu packages xorg))

;; patch xkeyboard-config to include my vup layout
(let* 
  ((old-xkeyboard-config (module-ref (resolve-module '(gnu packages xorg)) 'xkeyboard-config))
   (old-source (package-source old-xkeyboard-config))
   (patched-pkg
    (package
     (inherit old-xkeyboard-config)
     (source
      (origin
       (inherit old-source)
       (patches (append (origin-patches old-source) (list (origin (method url-fetch) (uri "https://raw.githubusercontent.com/rroohhh/guix_packages/1e057f94dd/vup.patch") (sha256 "1n5r518qgfjylqc463rcv9a4dz4ar4m723h21mk0dh50l9fa0k8d"))))))))))
  (if (not (package-replacement old-xkeyboard-config))
    (module-define! (resolve-module '(gnu packages xorg)) 'xkeyboard-config
      (package (inherit old-xkeyboard-config)
        (replacement patched-pkg)))))

(let*
  ((old-libinput (module-ref (resolve-module '(gnu packages freedesktop)) 'libinput))
   (old-source (package-source old-libinput))
   (patched-pkg
    (package
     (inherit old-libinput)
     (source
      (origin
       (inherit old-source)
       (patches (append (origin-patches old-source) (list (origin (method url-fetch) (uri "https://raw.githubusercontent.com/rroohhh/guix_packages/ec60db1ffd03df360a236b0a856b0d02e26ce7fd/libinput-quirk.patch") (sha256 "0adjp37g0zrlkc5qh6v3i5lddqn2yjqz1kvkhsdf3psjs9alimfi"))))))))))
  (if (not (package-replacement old-libinput))
    (module-define! (resolve-module '(gnu packages freedesktop)) 'libinput
      (package (inherit old-libinput)
        (replacement patched-pkg)))))
