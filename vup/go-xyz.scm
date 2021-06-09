(define-module (vup go-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public vault
  (package
    (name "vault")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.hashicorp.com/vault/" version "/vault_" version "_linux_amd64.zip"))
       (sha256
        (base32 "1g37pgj7hbi6vfpwq9rrh6is980lfwbq5jb4736jfp5m360vprjy"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (invoke (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip") (assoc-ref %build-inputs "source"))
         (install-file "vault" (string-append %output "/bin"))
         #t)))
    (home-page
      "https://www.vaultproject.io")
    (synopsis "A tool for managing secrets")
    (description
      "A tool for managing secrets")
    (license license:mpl2.0)))
