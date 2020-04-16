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

(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    (_                (nix-system->gnu-triplet system))))

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

(define (patch-command-exec-tests-phase test-path)
  "The command-exec.rs test moves around between releases.  We need to apply
a Guix-specific patch to it for each release.  This function generates the phase
that applies said patch, parametrized by the test-path.  This is done this way
because the phase is more complex than the equivalents for other tests that
move around."
 `(lambda* (#:key inputs #:allow-other-keys)
    (let ((coreutils (assoc-ref inputs "coreutils")))
      (substitute* ,test-path
        ;; This test suite includes some tests that the stdlib's
        ;; `Command` execution properly handles situations where
        ;; the environment or PATH variable are empty, but this
        ;; fails since we don't have `echo` available in the usual
        ;; Linux directories.
        ;; NB: the leading space is so we don't fail a tidy check
        ;; for trailing whitespace, and the newlines are to ensure
        ;; we don't exceed the 100 chars tidy check as well
        ((" Command::new\\(\"echo\"\\)")
         (string-append "\nCommand::new(\"" coreutils "/bin/echo\")\n")))
      #t)))


(define-public rust-1.40
  (let ((base-rust
         (rust-bootstrapped-package rust-1.39 "1.40.0"
            "1ba9llwhqm49w7sz3z0gqscj039m53ky9wxzhaj11z6yg1ah15yx")))
    (package
      (inherit base-rust)
      (source
        (origin
          (inherit (package-source base-rust))
          (snippet '(begin
                      (delete-file-recursively "src/llvm-project")
                      (delete-file-recursively "vendor/jemalloc-sys/jemalloc")
                      #t))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'remove-unsupported-tests)
             (add-after 'install 'fixup-install
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (target-system ,(or (%current-target-system)
                                            (nix-system->gnu-triplet
                                             (%current-system))))
                        (out-libs (string-append out "/lib/rustlib/"
                                                 target-system "/lib")))
                   (mkdir-p out-libs)

                   (for-each
                    (lambda (file)
                      (copy-file (string-append out "/lib/" file)
                                    (string-append out "/lib/rustlib/" target-system "/lib/" file)))
                     '("librustc_driver-ea714330082e255b.so" "librustc_macros-7bc422cb42f2abdc.so" "libstd-38842ad84adf0ce6.so" "libtest-37b181cebd6810e9.so"))
                   #t)))
             (replace 'configure
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (doc (assoc-ref outputs "doc"))
                        (gcc (assoc-ref inputs "gcc"))
                        (gdb (assoc-ref inputs "gdb"))
                        (binutils (assoc-ref inputs "binutils"))
                        (python (assoc-ref inputs "python-2"))
                        (rustc (assoc-ref inputs "rustc-bootstrap"))
                        (cargo (assoc-ref inputs "cargo-bootstrap"))
                        (llvm (assoc-ref inputs "llvm"))
                        (jemalloc (assoc-ref inputs "jemalloc")))
                   (call-with-output-file "config.toml"
                     (λ (port)
                       (display (string-append "
[llvm]
[build]
cargo = \"" cargo "/bin/cargo" "\"
rustc = \"" rustc "/bin/rustc" "\"
docs = true
python = \"" python "/bin/python2" "\"
gdb = \"" gdb "/bin/gdb" "\"
vendor = true
submodules = false
[install]
prefix = \"" out "\"
docdir = \"" doc "/share/doc/rust" "\"
sysconfdir = \"etc\"
[rust]
default-linker = \"" gcc "/bin/gcc" "\"
channel = \"nightly\"
rpath = true
" ;; There are 2 failed codegen tests:
;; codegen/mainsubprogram.rs and codegen/mainsubprogramstart.rs
;; These tests require a patched LLVM
"codegen-tests = false
[target." ,(nix-system->gnu-triplet-for-rust) "]
llvm-config = \"" llvm "/bin/llvm-config" "\"
cc = \"" gcc "/bin/gcc" "\"
cxx = \"" gcc "/bin/g++" "\"
ar = \"" binutils "/bin/ar" "\"
jemalloc = \"" jemalloc "/lib/libjemalloc_pic.a" "\"
[dist]
") port)))
                   #t))))))))))

(define-public rust-1.41.1
  (let ((base-rust
         (rust-bootstrapped-package rust-1.40 "1.41.1"
           "0ws5x0fxv57fyllsa6025h3q6j9v3m8nb3syl4x0hgkddq0kvj9q")))
    (package
      (inherit base-rust)
      (source
        (origin
          (inherit (package-source base-rust))
          (patches (append (origin-patches (package-source base-rust)) (search-patches "rust_line_length.patch")))
          (snippet '(begin
                      (delete-file-recursively "src/llvm-project")
                      (delete-file-recursively "vendor/jemalloc-sys/jemalloc")
                      #t))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-command-exec-tests
               ,(patch-command-exec-tests-phase
                  "src/test/ui/command/command-exec.rs"))
             (replace 'patch-command-uid-gid-test
               (lambda _
                 (substitute* "src/test/ui/command/command-uid-gid.rs"
                   (("/bin/sh") (which "sh"))
                   (("ignore-sgx") "ignore-sgx\n// ignore-tidy-linelength"))
                 #t))
             (delete 'disable-cargo-test-for-nightly-channel)
             (delete 'fixup-install))))))))


(define-public rust-nightly
  (let ((base-rust
         (rust-bootstrapped-package rust-1.41.1 "1.42.0"
           "0x9lxs82may6c0iln0b908cxyn1cv7h03n5cmbx3j1bas4qzks6j")))
    (package
      (inherit base-rust)
      (name "rust-nightly"))))
