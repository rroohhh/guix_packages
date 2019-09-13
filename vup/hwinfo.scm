(define-module (vup hwinfo)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake))

(define-public hwinfo
  (package
   (name "hwinfo")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/rroohhh/hwinfo")
           (commit "174f511dabe053eaa62cc5c9259f099a2049b8a4")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0cb5i53kdrmvkjmlwzx6xkxp6lyhpx7j7y0qcrinldydnh37djx1"))))
   (build-system cmake-build-system)
   (arguments '(#:phases
                (modify-phases %standard-phases
                  (delete 'check)))) ;; there are no tests
;;   (arguments '(#:phases
;;                (modify-phases %standard-phases
;;                  (delete 'configure) 
;;                  (delete 'check) 
;;                  ;; The upstream makefile does not include an install phase.
;;                  (replace 'install
;;                    (lambda* (#:key outputs #:allow-other-keys)
;;                      (let* ((out (assoc-ref outputs "out"))
;;                             (bin (string-append out "/bin")))
;;                        (for-each (lambda (file)
;;                                    (install-file file bin)
;;                                    (delete-file file))
;;                                  '("caps2esc")))
;;                      #t)))))
   (home-page "https://github.com/rroohhh/hwinfo")
   (synopsis "Hardware information tool")
   (description "Hardware information tool with influxdb and zsh completion support.")
   (license agpl3)))

hwinfo
