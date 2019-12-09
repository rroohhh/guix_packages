(define-module (vup unrar))
(use-modules (guix packages))
(use-modules (guix build-system gnu))
(use-modules (guix download))
(use-modules ((guix licenses) #:prefix license:))

(define-public unrar
  (package
    (name "unrar")
    (version "5.3.11")
    (source (origin
              (method url-fetch)
              (uri "http://www.rarlab.com/rar/unrarsrc-5.3.11.tar.gz")
              (sha256
               (base32
                "0qw77gvr57azjbn76cjlm4sv1hf2hh90g7n7n33gfvlpnbs7mf3p"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "DESTDIR=" %output))
       #:phases (alist-delete 'configure (alist-delete 'check %standard-phases))))
    (inputs
     `())
    (home-page "http://www.rarlab.com/rar_add.htm/")
    (synopsis "unrar")
    (description
     "unrar, non free.")
    (license license:x11)))
