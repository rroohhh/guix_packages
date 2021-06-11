(define-module (vup influxdb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
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
       (file-name (git-file-name name version))
       (sha256
        (base32 "10vf1jh9ay5zxfbpn5c78h6g7bf1356hn5d2xd824pcbnjz19pmk"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("usr/bin/telegraf" "bin/telegraf"))))
    (home-page
      "https://www.influxdata.com/")
    (synopsis "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (description
      "Telegraf is a plugin-driven server agent for collecting and sending metrics and events from databases, systems, and IoT sensors.")
    (license #f)))                      ; MIT
