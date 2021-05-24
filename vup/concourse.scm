(define-module (vup concourse)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public concourse
  (package
    (name "concourse")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/concourse/concourse/releases/download/v" version "/concourse-" version "-linux-amd64.tgz"))
       (sha256
        (base32 "0638r9cb8drjh5gbw1l6m5pls33hbs17ykljqh9f2llg0822w5xa"))))
    (build-system copy-build-system)
    (synopsis "Concourse is an open-source continuous thing-doer.")
    (description "Concourse is an open-source continuous thing-doer.")
    (home-page "https://concourse-ci.org/")
    (license license:asl2.0)))

(define-public fly
  (package
    (name "fly")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/concourse/concourse/releases/download/v" version "/fly-" version "-linux-amd64.tgz"))
       (sha256
        (base32 "1agsinxfsvklnrmpgdm54kx2mz0rcrycq951vnhfkc8jryby84gm"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("fly" "bin/fly"))))
    (synopsis "A command line interface that runs a build in a container with ATC.")
    (description "A command line interface that runs a build in a container with ATC.")
    (home-page "https://concourse-ci.org/")
    (license license:asl2.0)))
