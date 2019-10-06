(define-module (vup lld))
(use-modules (gnu packages llvm))
(use-modules (guix packages))
(use-modules (guix download))
(use-modules (guix build-system cmake))
(use-modules ((guix licenses) #:prefix license:))

(define-public lld-8
  (package
    (name "lld")
    (version "8.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://llvm.org/releases/"
                          version "/lld-" version ".src.tar.xz"))
      (sha256
       (base32
        "0dvf38pbm7jy88g66mz7ikkdfhm2qpj0iyzh62hzycifjbnciblw"))))
    (build-system cmake-build-system)
	(inputs `(("llvm" ,llvm)))
	(arguments
	 `(#:phases (modify-phases %standard-phases
							   (delete 'check))))
    (home-page "https://www.llvm.org")
    (synopsis "LLD - The LLVM Linker")
    (description
     "LLD is a linker from the LLVM project that is a drop-in replacement for system linkers and runs much faster than them. It also provides features that are useful for toolchain developers.")
    (license license:ncsa)))
