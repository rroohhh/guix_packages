(define-module (vup rust-nightly))

(use-modules (gnu packages))
(use-modules (gnu packages rust))
(use-modules (gnu packages ninja))
(use-modules (guix packages))
(use-modules (guix build utils))
(use-modules (guix utils))
(use-modules (ice-9 match))


(use-modules (gnu packages llvm))
(use-modules (guix download))
(use-modules (guix build-system cmake))
(use-modules ((guix licenses) #:prefix license:))

(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    (_                (nix-system->gnu-triplet system))))

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
         (rust-bootstrapped-package rust "1.58.1"
           "1iq7kj16qfpkx8gvw50d8rf7glbm6s0pj2y1qkrz7mi56vfsyfd8")))
    (package
      (inherit base-rust)
      (name "rust-nightly")
      (outputs '("out" "doc" "cargo" "rustfmt"))
      (source
        (origin
          (inherit (package-source base-rust))
          (snippet #f)))
      (inputs (append (alist-replace "llvm" (list llvm-13) (package-inputs base-rust)) `(("ninja" ,ninja))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'check) ;; TODO(robin): remove again, just delete for testing
             (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gcc (assoc-ref inputs  "gcc")))
                   ;; Hide GCC's C++ headers so that they do not interfere with
                   ;; the ones we are attempting to build.
                   (setenv "CPLUS_INCLUDE_PATH"
                           (string-join
                            (cons (string-append
                                   (assoc-ref inputs "gcc") "/include/c++/x86_64-unknown-linux-gnu")
                                  (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                        #\:))
                            ":"))
                   (format #true
                           "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                           (getenv "CPLUS_INCLUDE_PATH")))))
             (add-after 'configure 'enable-extended
               (lambda* (#:key outputs #:allow-other-keys)
                 (substitute* "config.toml"
                   (("submodules = false")
                    "submodules = false
sanitizers = true
profiler = true
extended = true
tools = [\"cargo\",  \"rust-demangler\", \"rls\", \"clippy\", \"llvm-tools\", \"rustfmt\", \"analysis\", \"src\", \"rust-analyzer\", \"miri\"]"))
                 (substitute* "config.toml"
                   (("jemalloc=true")
                    "jemalloc=true
codegen-backends=[\"llvm\"]
lld=true
llvm-tools=true
"))
                 #t))
             (add-after 'configure 'fix-lld-compilation
                (lambda* _
                  (invoke "cp" "-r" "src/llvm-project/libunwind/include/mach-o/" "src/llvm-project/lld/MachO/")
                  #t))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (sanitizers-dir (string-append (assoc-ref outputs "out") "/lib/rustlib/x86_64-unknown-linux-gnu/lib/"))
                        (from-dir "build/x86_64-unknown-linux-gnu/native/sanitizers/build/lib/linux/"))
                     (mkdir-p sanitizers-dir)
                     (copy-file (string-append from-dir "libclang_rt.asan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.asan.a"))
                     (copy-file (string-append from-dir "libclang_rt.tsan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.tsan.a"))
                     (copy-file (string-append from-dir "libclang_rt.msan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.msan.a"))
                     (copy-file (string-append from-dir "libclang_rt.lsan-x86_64.a") (string-append sanitizers-dir "librustc-nightly_rt.lsan.a")))
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'rustfmt' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "rustfmt"))))
                 (invoke "./x.py" "install" "rustfmt")))
             (delete 'patch-cargo-checksums)
             (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
               ;; Generate checksums after patching generated files (in
               ;; particular, vendor/jemalloc/rep/Makefile).
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (substitute* "src/tools/rust-analyzer/Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor")
                 (generate-all-checksums "src/tools")
                 #t))
             (delete 'delete-install-logs)
            ;; (replace 'delete-install-logs
            ;;    (lambda* (#:key outputs #:allow-other-keys)
            ;;      (define (delete-manifest-file out-path file)
            ;;        (delete-file (string-append out-path "/lib/rustlib/" file)))

            ;;      (let ((out (assoc-ref outputs "out")))
            ;;        (for-each
            ;;          (lambda (file) (delete-manifest-file out file))
            ;;          '("install.log"
            ;;            "manifest-rust-docs"
            ;;            ,(string-append "manifest-rust-std-"
            ;;                            (nix-system->gnu-triplet-for-rust))
            ;;            "manifest-rustc"))
            ;;        #t)))
            (add-after 'configure 'switch-to-nightly
              (lambda _
                (substitute* "config.toml"
                             (("channel = \"stable\"") "channel = \"nightly\"")))))))))))

(define-public rust-nightly-1.59
 (rust-bootstrapped-package rust-nightly "1.59.0"
           "1yc5bwcbmbwyvpfq7zvra78l0r8y3lbv60kbr62fzz2vx2pfxj57"))

(define-public rust-nightly-1.60
  (let ((base-rust
         (rust-bootstrapped-package rust-nightly-1.59 "1.60.0"
           "1drqr0a26x1rb2w3kj0i6abhgbs3jx5qqkrcwbwdlx7n3inq5ji0")))
    (package
      (inherit base-rust)
      (inputs (alist-replace "llvm" (list llvm-14) (package-inputs base-rust))))))
