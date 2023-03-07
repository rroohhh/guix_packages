(define-module (vup concourse)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define concourse-version "7.9.0")

(define-public concourse
  (package
   (name "concourse")
   (version concourse-version)
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/concourse/concourse/releases/download/v" version "/concourse-" version "-linux-amd64.tgz"))
     (sha256
      (base32 "0mhcp86p8i8zpwvazfx4v2f5h5w2q7b9m554snb49lfsj1ch1mhj"))))
   (build-system copy-build-system)
   (synopsis "Concourse is an open-source continuous thing-doer.")
   (description "Concourse is an open-source continuous thing-doer.")
   (home-page "https://concourse-ci.org/")
   (license license:asl2.0)))

(define-public fly
  (package
    (name "fly")
    (version concourse-version)
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/concourse/concourse/releases/download/v" version "/fly-" version "-linux-amd64.tgz"))
       (sha256
        (base32 "1hrkfqsv0hmgil3jix0fci42swj920xiaijdv9c14791dgnpf9i6"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("fly" "bin/fly"))))
    (synopsis "A command line interface that runs a build in a container with ATC.")
    (description "A command line interface that runs a build in a container with ATC.")
    (home-page "https://concourse-ci.org/")
    (license license:asl2.0)))
