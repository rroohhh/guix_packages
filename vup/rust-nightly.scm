(define-module (vup rust-nightly))

(use-modules (gnu packages))
(use-modules (gnu packages rust))
(use-modules (guix packages))
(use-modules (guix build utils))
(use-modules (guix utils))
(use-modules (ice-9 match))


(use-modules (gnu packages llvm))
(use-modules (guix download))
(use-modules (guix build-system cmake))
(use-modules ((guix licenses) #:prefix license:))

(define* (rust-uri version #:key date (dist "static"))
  (string-append "https://" dist ".rust-lang.org/dist/"
                 (if date (string-append date "/") "")
                 "rustc-" version "-src.tar.gz"))

(define* (rust-bootstrapped-package base-rust version checksum #:key date)
  "Bootstrap rust VERSION with source checksum CHECKSUM using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
      (origin
        (inherit (package-source base-rust))
        (uri (rust-uri version #:date date))
        (sha256 (base32 checksum))))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))
(define-public rust-nightly
  (let ((base-rust
         (rust-bootstrapped-package rust-1.45 "1.45.0"
           "0z6dh0yd3fcm3qh960wi4s6fa6pxz9mh77psycsqfkkx5kqra15s")))
    (package
      (inherit base-rust)
      (name "rust-nightly")
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'configure 'switch-to-nightly
               (lambda _
                 (substitute* "config.toml"
                   (("channel = \"stable\"") "channel = \"nightly\"")))))))))))

rust-nightly
