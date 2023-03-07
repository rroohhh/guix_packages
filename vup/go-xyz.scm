(define-module (vup go-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public vault
  (package
    (name "vault")
    (version "386")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.hashicorp.com/vault/" version "/vault_" version "_linux_amd64.zip"))
       (sha256
        (base32 "0jfbpbbvvzxbpnbr31fgvn27b16n38w7ihgqnfrzw4zva57mdic4"))))
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

(define-public rtsp-simple-server
  (package
    (name "rtsp-simple-server")
    (version "0.16.4")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/aler9/rtsp-simple-server/releases/download/v" version "/rtsp-simple-server_v" version "_linux_amd64.tar.gz"""))
       (sha256
        (base32 "1nabx2zmkx758cw7szg118x61hl2y8hp13a5pai4f3q391vag7h6"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("rtsp-simple-server" "bin/rtsp-simple-server")
         ("rtsp-simple-server.yml" "etc/rtsp-simple-server.yml"))))
    (home-page
      "https://github.com/aler9/rtsp-simple-server")
    (synopsis "ready-to-use RTSP / RTMP / HLS server and proxy that allows to read, publish and proxy video and audio streams")
    (description
      "ready-to-use RTSP / RTMP / HLS server and proxy that allows to read, publish and proxy video and audio streams")
    (license #f))) ;; mit
