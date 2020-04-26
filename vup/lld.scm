(define-module (vup lld))
(use-modules (gnu packages llvm))
(use-modules (guix packages))
(use-modules (guix download))
(use-modules (guix utils))
(use-modules (guix build-system cmake))
(use-modules ((guix licenses) #:prefix license:))

(define (llvm-download-uri component version)
  (if (version>=? version "9.0.1")
      (string-append "https://github.com/llvm/llvm-project/releases/download"
                     "/llvmorg-" version "/" component "-" version ".src.tar.xz")
      (string-append "https://releases.llvm.org/" version "/" component "-"
                     version ".src.tar.xz")))

(define-public lld-10
  (package
    (name "lld")
    (version "10.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-download-uri "lld" version))
      (sha256
       (base32
        "026pwcbczcg0j5c9h7hxxrn3ki81ia9m9sfn0sy0bvzffv2xg85r"))))
    (build-system cmake-build-system)
	(inputs `(("llvm-10" ,llvm-10)))
	(arguments
	 `(#:phases (modify-phases %standard-phases
							   (delete 'check))))
    (home-page "https://www.llvm.org")
    (synopsis "LLD - The LLVM Linker")
    (description
     "LLD is a linker from the LLVM project that is a drop-in replacement for system linkers and runs much faster than them. It also provides features that are useful for toolchain developers.")
    (license license:ncsa)))
