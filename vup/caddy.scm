(define-module (vup caddy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module ((guix licenses) #:prefix license:))

(define-public caddy
  (package
    (name "caddy")
    (version "2.6.2")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/caddyserver/caddy/releases/download/v" version "/caddy_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0am6rkmim7q2xbx5mv3zbxnm5nmwi90v08k3p6vhh092l1jyxw2s"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("caddy" "bin/caddy"))))
    (home-page
      "https://caddyserver.com/")
    (synopsis "Caddy 2 is a powerful, enterprise-ready, open source web server with automatic HTTPS written in Go")
    (description
      "Caddy 2 is a powerful, enterprise-ready, open source web server with automatic HTTPS written in Go")
    (license license:asl2.0)))
