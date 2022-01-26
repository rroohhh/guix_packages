(define-module (vup influxdb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module ((guix licenses) #:prefix license:))

(define-public influxd
  (package
    (name "influxd")
    (version "2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.influxdata.com/influxdb/releases/influxdb2-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32 "0ridj48inkzqhsrs5aqxi49rcy010fnrd6ga4p9pjm45bd4ydims"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("influxd" "bin/influxd"))))
    (home-page
      "https://www.influxdata.com/")
    (synopsis "InfluxDB is a time series platform")
    (description
      "InfluxDB is a time series platform")
    (license #f)))                      ; MIT

(define-public influx
  (package
   (inherit influxd)
   (name "influx")
   (arguments
    '(#:install-plan
      '(("influx" "bin/influx"))))))

(define-public telegraf
  (package
    (name "telegraf")
    (version "1.19.0-rc1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.influxdata.com/telegraf/releases/telegraf-" (string-replace-substring version "-" "~") "_linux_amd64.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10vf1jh9ay5zxfbpn5c78h6g7bf1356hn5d2xd824pcbnjz19pmk"))))
    (build-system copy-build-system)
    (inputs `(("libc" ,glibc)
              ("patchelf" ,patchelf)))
    (arguments
     `(#:install-plan
       '(("usr/bin/telegraf" "bin/telegraf"))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'patch
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((ld-so (string-append (assoc-ref inputs "libc")
                                                   ,(glibc-dynamic-linker)))
                             (out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin/telegraf")))
                        (invoke "patchelf" "--set-interpreter" ld-so bin)))))))
    (home-page
      "https://www.influxdata.com/")
    (synopsis "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (description
      "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (license #f)))                      ; MIT

telegraf
