(define-module (vup influxdb)
  #:use-module (guix packages)
  #:use-module (guix download)
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
