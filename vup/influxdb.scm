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
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.influxdata.com/influxdb/releases/influxdb2-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32 "07ll8q9vfcvbs8l54c3rjahbclk5yr08xmnaqwy60lwnrjx0hf80"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.influxdata.com/telegraf/releases/telegraf-" (string-replace-substring version "-" "~") "_linux_amd64.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rm2n4zl3h1a2s915cihk125kjqwai30rps6l4n0wia68fcwwscq"))))
    (build-system copy-build-system)
    (inputs `(("libc" ,glibc)
              ("patchelf" ,patchelf)))
    (arguments
     `(#:install-plan
       '(("usr/bin/telegraf" "bin/telegraf"))))
    (home-page
      "https://www.influxdata.com/")
    (synopsis "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (description
      "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (license #f)))                      ; MIT
