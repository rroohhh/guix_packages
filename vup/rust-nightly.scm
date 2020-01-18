(define-module (vup rust-nightly))

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

(define-public rust-1.38
  (let ((base-rust
         (rust-bootstrapped-package rust "1.38.0"
           "101dlpsfkq67p0hbwx4acqq6n90dj4bbprndizpgh1kigk566hk4")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'configure 'configure-cargo-home
               (lambda _
                 (let ((cargo-home (string-append (getcwd) "/.cargo")))
                   (mkdir-p cargo-home)
                   (setenv "CARGO_HOME" cargo-home)
                   #t)))
			 (replace 'patch-command-exec-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((coreutils (assoc-ref inputs "coreutils")))
                   (substitute* "src/test/ui/command-exec.rs"
                     ((" Command::new\\(\"echo\"\\)")
                      (string-append "\nCommand::new(\"" coreutils "/bin/echo\")\n")))
                   #t)))
			 (add-after 'patch-tests 'patch-command-uid-gid-tests-ls
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((coreutils (assoc-ref inputs "coreutils")))
                   (substitute* "src/test/ui/command-uid-gid.rs"
                     ((" Command::new\\(\"/bin/ls\"\\)")
                      (string-append "\nCommand::new(\"" coreutils "/bin/ls\")\n")))
                   #t)))
			 (add-after 'patch-tests 'patch-command-uid-gid-tests-sh
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (substitute* "src/test/ui/command-uid-gid.rs"
                     ((" Command::new\\(\"/bin/sh\"\\)")
                      (string-append "\nCommand::new(\"" bash "/bin/sh\")\n")))
                   #t))))))))))


(define-public rust-1.39
  (let ((base-rust
         (rust-bootstrapped-package rust-1.38 "1.39.0"
           "0mwkc1bnil2cfyf6nglpvbn2y0zfbv44zfhsd5qg4c9rm6vgd8dl")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
			 ;; the checksums moved from a big blob at the end to the individual packages
             (replace 'patch-cargo-checksums
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* "Cargo.lock"
                   (("checksum = .*$")
                    (string-append "checksum = \"" ,%cargo-reference-hash "\"\n")))
                 (generate-all-checksums "vendor")
                 #t)))))))))

(define-public rust-1.40
  (rust-bootstrapped-package rust-1.39 "1.40.0"
    "1ba9llwhqm49w7sz3z0gqscj039m53ky9wxzhaj11z6yg1ah15yx"))

(define-public rust-nightly
  (let ((base-rust
         (rust-bootstrapped-package rust-1.39 "1.40.0"
           "1ba9llwhqm49w7sz3z0gqscj039m53ky9wxzhaj11z6yg1ah15yx")))
    (package
      (inherit base-rust)
	  (name "rust-nightly")
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
