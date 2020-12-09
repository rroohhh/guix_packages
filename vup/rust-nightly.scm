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

(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

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
         (rust-bootstrapped-package rust-1.46 "1.47.0"
           "07fqd2vp7cf1ka3hr207dnnz93ymxml4935vp74g4is79h3dz19i")))
    (package
      (inherit base-rust)
      (name "rust-nightly")
      (outputs '("out" "doc"))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'check) ;; TODO(robin): remove again, just delete for testing
             (add-after 'configure 'enable-extended
               (lambda* (#:key outputs #:allow-other-keys)
                 (substitute* "config.toml"
                   (("submodules = false")
                    "submodules = false
sanitizers = true
profiler = true
extended = true
tools = [\"cargo\", \"rls\", \"clippy\", \"miri\", \"llvm-tools\", \"rustfmt\", \"analysis\", \"src\"]"))
;; tools = [\"cargo\", \"rls\", \"clippy\", \"rust-analyzer\", \"miri\", \"llvm-tools\", \"rustfmt\", \"analysis\", \"src\"]"))
                 #t))
             ;; (add-after 'build 'build-more-tools
             ;;   (lambda _
             ;;     (invoke "./x.py" "build" "src/tools/clippy")
             ;;     (invoke "./x.py" "build" "src/tools/rustfmt")
             ;;     ;; (invoke "./x.py" "build" "src/tools/rust-analyzer") ; not yet
             ;;     ))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 ;; (invoke "./x.py" "install" "cargo")
                 ;; (invoke "./x.py" "install" "clippy")
                 ;; (invoke "./x.py" "install" "rustfmt")
                 ))
             (delete 'delete-install-logs)
             ;; (add-after 'install 'delete-install-logs
             ;;   (lambda* (#:key outputs #:allow-other-keys)
             ;;     (define (delete-manifest-file out-path file)
             ;;       (delete-file (string-append out-path "/lib/rustlib/" file)))
             ;;     (let ((out (assoc-ref outputs "out")))
             ;;       (for-each
             ;;         (lambda (file) (delete-manifest-file out file))
             ;;         '("install.log"
             ;;           "manifest-rust-docs"
             ;;           "manifest-rust-std-x86_64-unknown-linux-gnu"
             ;;           "manifest-rustc"))
             ;;       #t)))
             (delete 'patch-cargo-checksums)
             (add-before 'build 'patch-cargo-checksums
               ;; The Cargo.lock format changed.
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor")
                 ;; (generate-all-checksums "src")
                 #t))
             (replace 'patch-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (substitute* "library/std/src/process.rs"
                     ;; The newline is intentional.
                     ;; There's a line length "tidy" check in Rust which would
                     ;; fail otherwise.
                     (("\"/bin/sh\"") (string-append "\n\"" bash "/bin/sh\"")))
                   (substitute* "library/std/src/net/tcp.rs"
                     ;; There is no network in build environment
                     (("fn connect_timeout_unroutable")
                      "#[ignore]\nfn connect_timeout_unroutable"))
                   ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00222.html>
                   (substitute* "library/std/src/sys/unix/process/process_common.rs"
                     (("fn test_process_mask") "#[allow(unused_attributes)]
    #[ignore]
    fn test_process_mask"))
                   #t)))
             (replace 'mkdir-prefix-paths
               (lambda* (#:key outputs #:allow-other-keys)
                 ;; As result of https://github.com/rust-lang/rust/issues/36989
                 ;; `prefix' directory should exist before `install' call
                 (mkdir-p (assoc-ref outputs "out"))
                 #t))
             ;; TODO(robin): make this work
             ;; (add-after 'install 'install-tools
             ;;   (lambda* (#:key outputs #:allow-other-keys)
             ;;     ;; TODO(robin): move to own output
             ;;     ;; (substitute* "config.toml"
             ;;     ;;   ;; replace prefix to specific output
             ;;     ;;   (("prefix = \"[^\"]*\"")
             ;;     ;;    (string-append "prefix = \"" (assoc-ref outputs "tools") "\"")))
             ;;     ;; (mkdir-p (assoc-ref outputs "tools"))
             ;;     ;; (invoke "./x.py" "install" "rust-analyzer") ; not yet
             ;;     ))
             (add-after 'configure 'switch-to-nightly
               (lambda _
                 (substitute* "config.toml"
                   (("channel = \"stable\"") "channel = \"nightly\"")))))))))))
