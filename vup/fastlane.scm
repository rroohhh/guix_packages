(define-module (vup fastlane)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages ruby))

(define-public ruby-xcpretty-travis-formatter
  (package
    (name "ruby-xcpretty-travis-formatter")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri
             "xcpretty-travis-formatter"
             version))
       (sha256
        (base32
         "15b5c0lxz2blmichfdlabzlbyw5nlh1ci898pxwb661m9bahz3ml"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-xcpretty" ,ruby-xcpretty)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "
  Formatter for xcpretty customized to provide pretty output on TravisCI
  ")
    (description
     "
  Formatter for xcpretty customized to provide pretty output on TravisCI
  ")
    (home-page
     "https://github.com/kattrali/xcpretty-travis-formatter")
    (license license:expat)))

(define-public ruby-rouge-2.0.7
  (package
    (inherit ruby-rouge-2)
    (version "2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rouge" version))
       (sha256
        (base32
         "0sfikq1q8xyqqx690iiz7ybhzx87am4w50w8f2nq36l3asw4x89d"))))))

(define-public ruby-xcpretty
  (package
    (name "ruby-xcpretty")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "xcpretty" version))
       (sha256
        (base32
         "1xq47q2h5llj7b54rws4796904vnnjz7qqnacdv7wlp3gdbwrivm"))))
    (build-system ruby-build-system)
    (propagated-inputs `(("ruby-rouge" ,ruby-rouge-2.0.7)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "
    Xcodebuild formatter designed to be piped with `xcodebuild`,
    and thus keeping 100% compatibility.

    It has modes for CI, running tests (RSpec dot-style),
    and it can also mine Bitcoins.
    ")
    (description
     "
    Xcodebuild formatter designed to be piped with `xcodebuild`,
    and thus keeping 100% compatibility.

    It has modes for CI, running tests (RSpec dot-style),
    and it can also mine Bitcoins.
    ")
    (home-page
     "https://github.com/supermarin/xcpretty")
    (license license:expat)))

(define-public ruby-nanaimo
  (package
    (name "ruby-nanaimo")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "nanaimo" version))
       (sha256
        (base32
         "0xi36h3f7nm8bc2k0b6svpda1lyank2gf872lxjbhw3h95hdrbma"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "A library for (de)serialization of ASCII Plists.")
    (description
     "This package provides a library for (de)serialization of ASCII Plists.")
    (home-page
     "https://github.com/CocoaPods/Nanaimo")
    (license license:expat)))

(define-public ruby-colored2
  (package
    (name "ruby-colored2")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "colored2" version))
       (sha256
        (base32
         "0jlbqa9q4mvrm73aw9mxh23ygzbjiqwisl32d8szfb5fxvbjng5i"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "This is a heavily modified fork of http://github.com/defunkt/colored gem, with many
sensible pull requests combined. Since the authors of the original gem no longer support it,
this might, perhaps, be considered a good alternative.

Simple gem that adds various color methods to String class, and can be used as follows:

  require 'colored2'

  puts 'this is red'.red
  puts 'this is red with a yellow background'.red.on.yellow
  puts 'this is red with and italic'.red.italic
  puts 'this is green bold'.green.bold &lt;&lt; ' and regular'.green
  puts 'this is really bold blue on white but reversed'.bold.blue.on.white.reversed
  puts 'this is regular, but '.red! &lt;&lt; 'this is red '.yellow! &lt;&lt; ' and yellow.'.no_color!
  puts ('this is regular, but '.red! do
    'this is red '.yellow! do
      ' and yellow.'.no_color!
    end
  end)

")
    (description
     "This is a heavily modified fork of http://github.com/defunkt/colored gem, with many
sensible pull requests combined.  Since the authors of the original gem no longer support it,
this might, perhaps, be considered a good alternative.

Simple gem that adds various color methods to String class, and can be used as follows:

  require 'colored2'

  puts 'this is red'.red
  puts 'this is red with a yellow background'.red.on.yellow
  puts 'this is red with and italic'.red.italic
  puts 'this is green bold'.green.bold &lt;&lt; ' and regular'.green
  puts 'this is really bold blue on white but reversed'.bold.blue.on.white.reversed
  puts 'this is regular, but '.red! &lt;&lt; 'this is red '.yellow! &lt;&lt; ' and yellow.'.no_color!
  puts ('this is regular, but '.red! do
    'this is red '.yellow! do
      ' and yellow.'.no_color!
    end
  end)

")
    (home-page "http://github.com/kigster/colored2")
    (license license:expat)))

(define-public ruby-claide
  (package
    (name "ruby-claide")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "claide" version))
       (sha256
        (base32
         "0kasxsms24fgcdsq680nz99d5lazl9rmz1qkil2y5gbbssx89g0z"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "A small command-line interface framework.")
    (description
     "This package provides a small command-line interface framework.")
    (home-page "https://github.com/CocoaPods/CLAide")
    (license license:expat)))

(define-public ruby-atomos
  (package
    (name "ruby-atomos")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "atomos" version))
       (sha256
        (base32
         "17vq6sjyswr5jfzwdccw748kgph6bdw30bakwnn6p8sl4hpv4hvx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "A simple gem to atomically write files")
    (description
     "This package provides a simple gem to atomically write files")
    (home-page "https://github.com/segiddins/atomos")
    (license license:expat)))

(define-public ruby-xcodeproj
  (package
    (name "ruby-xcodeproj")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "xcodeproj" version))
       (sha256
        (base32
         "18idiqfbvyrcyflccwy4qw125psckrnqy7ggci33m8f3zs8h7hnm"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-atomos" ,ruby-atomos)
       ("ruby-cfpropertylist" ,ruby-cfpropertylist)
       ("ruby-claide" ,ruby-claide)
       ("ruby-colored2" ,ruby-colored2)
       ("ruby-nanaimo" ,ruby-nanaimo)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Xcodeproj lets you create and modify Xcode projects from Ruby. Script boring management tasks or build Xcode-friendly libraries. Also includes support for Xcode workspaces (.xcworkspace) and configuration files (.xcconfig).")
    (description
     "Xcodeproj lets you create and modify Xcode projects from Ruby.  Script boring management tasks or build Xcode-friendly libraries.  Also includes support for Xcode workspaces (.xcworkspace) and configuration files (.xcconfig).")
    (home-page
     "https://github.com/cocoapods/xcodeproj")
    (license license:expat)))

(define-public ruby-word-wrap
  (package
    (name "ruby-word-wrap")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "word_wrap" version))
       (sha256
        (base32
         "1iyc5bc7dbgsd8j3yk1i99ral39f23l6wapi0083fbl19hid8mpm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "As simple as it gets CLI tool for word-wrapping
                          plain-text. You can also use the library in your
                          Ruby scripts. Check out the sources for details.")
    (description
     "As simple as it gets CLI tool for word-wrapping
                          plain-text.  You can also use the library in your
                          Ruby scripts.  Check out the sources for details.")
    (home-page
     "https://github.com/pazdera/word_wrap")
    (license license:expat)))

(define-public ruby-tty-cursor
  (package
    (name "ruby-tty-cursor")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tty-cursor" version))
       (sha256
        (base32
         "0j5zw041jgkmn605ya1zc151bxgxl6v192v2i26qhxx7ws2l2lvr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "The purpose of this library is to help move the terminal cursor around and manipulate text by using intuitive method calls.")
    (description
     "The purpose of this library is to help move the terminal cursor around and manipulate text by using intuitive method calls.")
    (home-page "https://ttytoolkit.org")
    (license license:expat)))

(define-public ruby-tty-spinner
  (package
    (name "ruby-tty-spinner")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tty-spinner" version))
       (sha256
        (base32
         "0hh5awmijnzw9flmh5ak610x1d00xiqagxa5mbr63ysggc26y0qf"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-tty-cursor" ,ruby-tty-cursor)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "A terminal spinner for tasks that have non-deterministic time frame.")
    (description
     "This package provides a terminal spinner for tasks that have non-deterministic time frame.")
    (home-page "https://ttytoolkit.org")
    (license license:expat)))

(define-public ruby-tty-screen
  (package
    (name "ruby-tty-screen")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tty-screen" version))
       (sha256
        (base32
         "18jr6s1cg8yb26wzkqa6874q0z93rq0y5aw092kdqazk71y6a235"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Terminal screen size detection which works on Linux, OS X and Windows/Cygwin platforms and supports MRI, JRuby, TruffleRuby and Rubinius interpreters.")
    (description
     "Terminal screen size detection which works on Linux, OS X and Windows/Cygwin platforms and supports MRI, JRuby, TruffleRuby and Rubinius interpreters.")
    (home-page "https://ttytoolkit.org")
    (license license:expat)))

(define-public ruby-terminal-table
  (package
    (name "ruby-terminal-table")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "terminal-table" version))
       (sha256
        (base32
         "1512cngw35hsmhvw4c05rscihc59mnj09m249sm9p3pik831ydqk"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-unicode-display-width"
        ,ruby-unicode-display-width)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Simple, feature rich ascii table generation library")
    (description
     "Simple, feature rich ascii table generation library")
    (home-page
     "https://github.com/tj/terminal-table")
    (license license:expat)))

(define-public ruby-terminal-notifier
  (package
    (name "ruby-terminal-notifier")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "terminal-notifier" version))
       (sha256
        (base32
         "1slc0y8pjpw30hy21v8ypafi8r7z9jlj4bjbgz03b65b28i2n3bs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'extract-gemspec)
                  (replace 'install ,ruby-install)
                  (delete 'check))))
    (synopsis
     "Send User Notifications on macOS 10.10 or higher.")
    (description
     "Send User Notifications on macOS 10.10 or higher.")
    (home-page
     "https://github.com/julienXX/terminal-notifier")
    (license license:expat)))

(define ruby-install
  `(lambda*
       (#:key inputs outputs (gem-flags '())
        #:allow-other-keys)
     (use-modules (guix build utils))
     (use-modules (ice-9 ftw))
     (use-modules (ice-9 match))
     (use-modules (ice-9 popen))
     (use-modules (ice-9 rdelim))
     (use-modules (ice-9 regex))
     (use-modules (srfi srfi-1))
     (use-modules (srfi srfi-26))
     (define (log-file-deletion file)
       (display (string-append "deleting '" file "' for reproducibility\n")))
     (let* ((ruby-version
             (match:substring (string-match "ruby-(.*)\\.[0-9]$"
                                            (assoc-ref inputs "ruby"))
                              1))
            (out (assoc-ref outputs "out"))
            (vendor-dir (string-append out "/lib/ruby/vendor_ruby"))
            ;; (gem-file (first-matching-file "\\.gem$"))
            (gem-file "vendor/terminal-notifier/Ruby/terminal-notifier-2.0.0.gem")
            (gem-file-basename (basename gem-file))
            (gem-name (substring gem-file-basename
                                 0
                                 (- (string-length gem-file-basename) 4)))
            (gem-dir (string-append vendor-dir "/gems/" gem-name)))
       (setenv "GEM_VENDOR" vendor-dir)

       (or (zero?
            ;; 'zero? system*' allows the custom error handling to function as
            ;; expected, while 'invoke' raises its own exception.
            (apply system* "gem" "install" gem-file
                   "--verbose"
                   "--local" "--ignore-dependencies" "--vendor"
                   ;; Executables should go into /bin, not
                   ;; /lib/ruby/gems.
                   "--bindir" (string-append out "/bin")
                   gem-flags))
           (begin
             (let ((failed-output-dir (string-append (getcwd) "/out")))
               (mkdir failed-output-dir)
               (copy-recursively out failed-output-dir))
             (error "installation failed")))

       ;; Remove the cached gem file as this is unnecessary and contains
       ;; timestamped files rendering builds not reproducible.
       (let ((cached-gem (string-append vendor-dir "/cache/" gem-file-basename)))
         (log-file-deletion cached-gem)
         (delete-file cached-gem))

       ;; For gems with native extensions, several Makefile-related files
       ;; are created that contain timestamps or other elements making
       ;; them not reproducible.  They are unnecessary so we remove them.
       (when (file-exists? (string-append gem-dir "/ext"))
         (for-each (lambda (file)
                     (log-file-deletion file)
                     (delete-file file))
                   (append
                    (find-files (string-append vendor-dir "/doc")
                                "page-Makefile.ri")
                    (find-files (string-append vendor-dir "/extensions")
                                "gem_make.out")
                    (find-files (string-append gem-dir "/ext")
                                "Makefile"))))

       #t)))


(define-public ruby-slack-notifier
  (package
    (name "ruby-slack-notifier")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "slack-notifier" version))
       (sha256
        (base32
         "1pkfn99dhy5s526r6k8d87fwwb6j287ga9s7lxqmh60z28xqh3bv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     " A slim ruby wrapper for posting to slack webhooks ")
    (description
     " A slim ruby wrapper for posting to slack webhooks ")
    (home-page
     "http://github.com/stevenosloan/slack-notifier")
    (license license:expat)))

(define-public ruby-naturally
  (package
    (name "ruby-naturally")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "naturally" version))
       (sha256
        (base32
         "0dzqdawqr4agx7zr1fr5zxdwl8vb5rhpz57l1lk7d2y46ha6l4l7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Natural Sorting with support for legal numbering, course numbers, and other number/letter mixes.")
    (description
     "Natural Sorting with support for legal numbering, course numbers, and other number/letter mixes.")
    (home-page
     "http://github.com/dogweather/naturally")
    (license license:expat)))

(define-public ruby-simctl
  (package
    (name "ruby-simctl")
    (version "1.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "simctl" version))
       (sha256
        (base32
         "1v9rsdmg5c5kkf8ps47xnrfbvjnq11sbaifr186jwkh4npawz00x"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-cfpropertylist" ,ruby-cfpropertylist)
       ("ruby-naturally" ,ruby-naturally)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'extract-gemspec 'remove-fastlane-plugin
                    (lambda _
                      (begin
                        (delete-file-recursively "fastlane-plugin-simctl")
                        (substitute* "simctl.gemspec"
                          (("`git ls-files`") "`find . -type f |sort`")
                          (("`git ls-files -z`") "`find . -type f -print0 |sort -z`"))
                        #t)))
                  (delete 'check))))
    (synopsis "Ruby interface to xcrun simctl")
    (description "Ruby interface to xcrun simctl")
    (home-page "https://github.com/plu/simctl")
    (license license:expat)))

(define-public ruby-security
  (package
    (name "ruby-security")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "security" version))
       (sha256
        (base32
         "1ryjxs0j66wrbky2c08yf0mllwalvpg12rpxzbdx2rdhj3cbrlxa"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Library for interacting with the Mac OS X Keychain")
    (description
     "Library for interacting with the Mac OS X Keychain")
    (home-page "http://mattt.me")
    (license #f)))

(define-public ruby-plist
  (package
    (name "ruby-plist")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "plist" version))
       (sha256
        (base32
         "0ra0910xxbhfsmdi0ig36pr3q0khdqzwb5da3wg7y3n8d1sh9ffp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Plist is a library to manipulate Property List files, also known as plists. It can parse plist files into native Ruby data structures as well as generating new plist files from your Ruby objects.")
    (description
     "Plist is a library to manipulate Property List files, also known as plists.  It can parse plist files into native Ruby data structures as well as generating new plist files from your Ruby objects.")
    (home-page "https://github.com/patsplat/plist")
    (license license:expat)))

(define-public ruby-mini-magick
  (package
    (name "ruby-mini-magick")
    (version "4.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mini_magick" version))
       (sha256
        (base32
         "0lpq12z70n10c1qshcddd5nib2pkcbkwzvmiqqzj60l01k3x4fg9"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Manipulate images with minimal use of memory via ImageMagick / GraphicsMagick")
    (description
     "Manipulate images with minimal use of memory via ImageMagick / GraphicsMagick")
    (home-page
     "https://github.com/minimagick/minimagick")
    (license license:expat)))

(define-public ruby-google-cloud-errors
  (package
    (name "ruby-google-cloud-errors")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "google-cloud-errors" version))
       (sha256
        (base32
         "1hvs1x39g77hbdqjxmzcl6gq8160pv3kskvzbbch0ww1np6qwm67"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "google-cloud-errors defines error classes for google-cloud-ruby.")
    (description
     "google-cloud-errors defines error classes for google-cloud-ruby.")
    (home-page
     "https://github.com/googleapis/google-cloud-ruby/tree/master/google-cloud-errors")
    (license #f)))

(define-public ruby-google-cloud-env
  (package
    (name "ruby-google-cloud-env")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "google-cloud-env" version))
       (sha256
        (base32
         "0frifczbhs5k699avaxnaypijfmjd2zb1rbdmcb30vdx8sskypa9"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-faraday" ,ruby-faraday-1)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "google-cloud-env provides information on the Google Cloud Platform hosting environment. Applications can use this library to determine hosting context information such as the project ID, whether App Engine is running, what tags are set on the VM instance, and much more.")
    (description
     "google-cloud-env provides information on the Google Cloud Platform hosting environment.  Applications can use this library to determine hosting context information such as the project ID, whether App Engine is running, what tags are set on the VM instance, and much more.")
    (home-page
     "https://github.com/googleapis/google-cloud-ruby/tree/master/google-cloud-env")
    (license #f)))

(define-public ruby-google-cloud-core
  (package
    (name "ruby-google-cloud-core")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "google-cloud-core" version))
       (sha256
        (base32
         "1qjn7vs8f85vxi1nkikbjfja6bv9snrj26vzscjii0cm8n4dy0i1"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-google-cloud-env" ,ruby-google-cloud-env)
       ("ruby-google-cloud-errors"
        ,ruby-google-cloud-errors)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "google-cloud-core is the internal shared library for google-cloud-ruby.")
    (description
     "google-cloud-core is the internal shared library for google-cloud-ruby.")
    (home-page
     "https://github.com/googleapis/google-cloud-ruby/tree/master/google-cloud-core")
    (license #f)))

(define-public ruby-digest-crc
  (package
    (name "ruby-digest-crc")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "digest-crc" version))
       (sha256
        (base32
         "1nygbn680kmya9vnz0pr9681z8lszr2whln0rxk168vaxbcnga8d"))))
    (build-system ruby-build-system)
    (propagated-inputs `(("ruby-rake" ,ruby-rake)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Adds support for calculating Cyclic Redundancy Check (CRC) to the Digest module.")
    (description
     "Adds support for calculating Cyclic Redundancy Check (CRC) to the Digest module.")
    (home-page
     "https://github.com/postmodern/digest-crc#readme")
    (license license:expat)))

(define-public ruby-google-cloud-storage
  (package
    (name "ruby-google-cloud-storage")
    (version "1.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "google-cloud-storage" version))
       (sha256
        (base32
         "1ph5wj3kpxc4iczwf72ww2h3ljsa5dx0c47g9ixj7y1clb0g3sac"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-digest-crc" ,ruby-digest-crc)
       ("ruby-google-api-client"
        ,ruby-google-api-client)
       ("ruby-googleauth" ,ruby-googleauth)
       ("ruby-google-cloud-core"
        ,ruby-google-cloud-core)
       ("ruby-mini-mime" ,ruby-mini-mime)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "google-cloud-storage is the official library for Google Cloud Storage.")
    (description
     "google-cloud-storage is the official library for Google Cloud Storage.")
    (home-page
     "https://github.com/googleapis/google-cloud-ruby/tree/master/google-cloud-storage")
    (license #f)))

(define-public ruby-retriable
  (package
    (name "ruby-retriable")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "retriable" version))
       (sha256
        (base32
         "1q48hqws2dy1vws9schc0kmina40gy7sn5qsndpsfqdslh65snha"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Retriable is a simple DSL to retry failed code blocks with randomized exponential backoff. This is especially useful when interacting external api/services or file system calls.")
    (description
     "Retriable is a simple DSL to retry failed code blocks with randomized exponential backoff.  This is especially useful when interacting external api/services or file system calls.")
    (home-page "http://github.com/kamui/retriable")
    (license license:expat)))

(define-public ruby-uber
  (package
    (name "ruby-uber")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "uber" version))
       (sha256
        (base32
         "1p1mm7mngg40x05z52md3mbamkng0zpajbzqjjwmsyw0zw3v9vjv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis "A gem-authoring framework.")
    (description
     "This package provides a gem-authoring framework.")
    (home-page "https://github.com/apotonick/uber")
    (license license:expat)))

(define-public ruby-declarative-option
  (package
    (name "ruby-declarative-option")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "declarative-option" version))
       (sha256
        (base32
         "1g4ibxq566f1frnhdymzi9hxxcm4g2gw4n21mpjk2mhwym4q6l0p"))))
    (build-system ruby-build-system)
    (synopsis "Dynamic options.")
    (description "Dynamic options.")
    (home-page
     "https://github.com/apotonick/declarative-option")
    (license license:expat)))

(define-public ruby-declarative
  (package
    (name "ruby-declarative")
    (version "0.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "declarative" version))
       (sha256
        (base32
         "1yczgnqrbls7shrg63y88g7wand2yp9h6sf56c9bdcksn5nds8c0"))))
    (build-system ruby-build-system)
    (synopsis
     "DSL for nested generic schemas with inheritance and refining.")
    (description
     "DSL for nested generic schemas with inheritance and refining.")
    (home-page
     "https://github.com/apotonick/declarative")
    (license license:expat)))

(define-public ruby-representable
  (package
    (name "ruby-representable")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "representable" version))
       (sha256
        (base32
         "0qm9rgi1j5a6nv726ka4mmixivlxfsg91h8rpp72wwd4vqbkkm07"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-declarative" ,ruby-declarative)
       ("ruby-declarative-option"
        ,ruby-declarative-option)
       ("ruby-uber" ,ruby-uber)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Renders and parses JSON/XML/YAML documents from and to Ruby objects. Includes plain properties, collections, nesting, coercion and more.")
    (description
     "Renders and parses JSON/XML/YAML documents from and to Ruby objects.  Includes plain properties, collections, nesting, coercion and more.")
    (home-page
     "https://github.com/trailblazer/representable/")
    (license license:expat)))

(define-public ruby-mini-mime
  (package
    (name "ruby-mini-mime")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mini_mime" version))
       (sha256
        (base32
         "1axm0rxyx3ss93wbmfkm78a6x03l8y4qy60rhkkiq0aza0vwq3ha"))))
    (build-system ruby-build-system)
    (synopsis "A lightweight mime type lookup toy")
    (description
     "This package provides a lightweight mime type lookup toy")
    (home-page
     "https://github.com/discourse/mini_mime")
    (license license:expat)))

(define-public ruby-signet
  (package
    (name "ruby-signet")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "signet" version))
       (sha256
        (base32
         "10g2667fvxnc50hcd1aywgsbf8j7nrckg3n7zjvywmyz82pwmpqp"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-faraday" ,ruby-faraday-1)
       ("ruby-jwt" ,ruby-jwt)
       ("ruby-multi-json" ,ruby-multi-json)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Signet is an OAuth 1.0 / OAuth 2.0 implementation.
")
    (description
     "Signet is an OAuth 1.0 / OAuth 2.0 implementation.
")
    (home-page
     "https://github.com/googleapis/signet")
    (license #f)))

(define-public ruby-os
  (package
    (name "ruby-os")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "os" version))
       (sha256
        (base32
         "12fli64wz5j9868gpzv5wqsingk1jk457qyqksv9ksmq9b0zpc9x"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "The OS gem allows for some useful and easy functions, like OS.windows? (=> true or false) OS.bits ( => 32 or 64) etc\"")
    (description
     "The OS gem allows for some useful and easy functions, like OS.windows? (=> true or false) OS.bits ( => 32 or 64) etc\"")
    (home-page "http://github.com/rdp/os")
    (license license:expat)))

(define-public ruby-memoist
  (package
    (name "ruby-memoist")
    (version "0.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "memoist" version))
       (sha256
        (base32
         "0i9wpzix3sjhf6d9zw60dm4371iq8kyz7ckh2qapan2vyaim6b55"))))
    (build-system ruby-build-system)
    (synopsis "memoize methods invocation")
    (description "memoize methods invocation")
    (home-page
     "https://github.com/matthewrudy/memoist")
    (license license:expat)))

(define-public ruby-googleauth
  (package
    (name "ruby-googleauth")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "googleauth" version))
       (sha256
        (base32
         "0ldhllhv8mlbalp8a3zb1hsj5p2i8f0vf88r71vm083gq7820iwn"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-faraday" ,ruby-faraday-1)
       ("ruby-jwt" ,ruby-jwt)
       ("ruby-memoist" ,ruby-memoist)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-os" ,ruby-os)
       ("ruby-signet" ,ruby-signet)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "   Allows simple authorization for accessing Google APIs.
   Provide support for Application Default Credentials, as described at
   https://developers.google.com/accounts/docs/application-default-credentials
")
    (description
     "   Allows simple authorization for accessing Google APIs.
   Provide support for Application Default Credentials, as described at
   https://developers.google.com/accounts/docs/application-default-credentials
")
    (home-page
     "https://github.com/googleapis/google-auth-library-ruby")
    (license #f)))

(define-public ruby-google-api-client
  (package
    (name "ruby-google-api-client")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "google-api-client" version))
       (sha256
        (base32
         "1jybks8i00rxrxx9mkx90dbdk6pczh2w757wchlavmrkrk0dp9s1"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-googleauth" ,ruby-googleauth)
       ("ruby-httpclient" ,ruby-httpclient)
       ("ruby-mini-mime" ,ruby-mini-mime)
       ("ruby-representable" ,ruby-representable)
       ("ruby-retriable" ,ruby-retriable)
       ("ruby-rexml" ,ruby-rexml)
       ("ruby-signet" ,ruby-signet)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis "Client for accessing Google APIs")
    (description "Client for accessing Google APIs")
    (home-page
     "https://github.com/google/google-api-ruby-client")
    (license #f)))

(define-public ruby-gh-inspector
  (package
    (name "ruby-gh-inspector")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "gh_inspector" version))
       (sha256
        (base32
         "0f8r9byajj3bi2c7c5sqrc7m0zrv3nblfcd4782lw5l73cbsgk04"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Search through GitHub issues for your project for existing issues about a Ruby Error.")
    (description
     "Search through GitHub issues for your project for existing issues about a Ruby Error.")
    (home-page
     "https://github.com/orta/gh_inspector")
    (license license:expat)))

(define-public ruby-fastimage
  (package
    (name "ruby-fastimage")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "fastimage" version))
       (sha256
        (base32
         "11ny2pj0j6pljszrf1w3iqdv2pcl2iwwghjbgcjlizy424zbh0hb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "FastImage finds the size or type of an image given its uri by fetching as little as needed.")
    (description
     "FastImage finds the size or type of an image given its uri by fetching as little as needed.")
    (home-page "http://github.com/sdsykes/fastimage")
    (license license:expat)))

(define-public ruby-faraday-middleware
  (package
    (name "ruby-faraday-middleware")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday_middleware" version))
       (sha256
        (base32
         "0jik2kgfinwnfi6fpp512vlvs0mlggign3gkbpkg5fw1jr9his0r"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-faraday" ,ruby-faraday-1)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis "Various middleware for Faraday")
    (description "Various middleware for Faraday")
    (home-page
     "https://github.com/lostisland/faraday_middleware")
    (license license:expat)))

(define-public ruby-faraday-cookie-jar
  (package
    (name "ruby-faraday-cookie-jar")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday-cookie_jar" version))
       (sha256
        (base32
         "00hligx26w9wdnpgsrf0qdnqld4rdccy8ym6027h5m735mpvxjzk"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-faraday" ,ruby-faraday-1)
       ("ruby-http-cookie" ,ruby-http-cookie)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis "Cookie jar middleware for Faraday")
    (description "Cookie jar middleware for Faraday")
    (home-page
     "https://github.com/miyagawa/faraday-cookie_jar")
    (license license:expat)))

(define-public ruby-excon
  (package
    (name "ruby-excon")
    (version "0.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "excon" version))
       (sha256
        (base32
         "05is0kb650j8wrdi4rgkdls662chnhdg2p64pgq3zkd3d7l2a9zw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis "EXtended http(s) CONnections")
    (description "EXtended http(s) CONnections")
    (home-page "https://github.com/excon/excon")
    (license license:expat)))

(define-public ruby-emoji-regex
  (package
    (name "ruby-emoji-regex")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "emoji_regex" version))
       (sha256
        (base32
         "0mjl5g6fv8iyf3nnbfyzd64bhl8l1qxyh452797km1aj4c682ia3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "A pair of Ruby regular expressions for matching Unicode Emoji symbols.")
    (description
     "This package provides a pair of Ruby regular expressions for matching Unicode Emoji symbols.")
    (home-page
     "https://github.com/ticky/ruby-emoji-regex")
    (license license:expat)))

(define-public ruby-dotenv
  (package
    (name "ruby-dotenv")
    (version "2.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "dotenv" version))
       (sha256
        (base32
         "0iym172c5337sm1x2ykc2i3f961vj3wdclbyg1x6sxs3irgfsl94"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Loads environment variables from `.env`.")
    (description
     "Loads environment variables from `.env`.")
    (home-page "https://github.com/bkeepers/dotenv")
    (license license:expat)))

(define-public ruby-commander-fastlane
  (package
    (name "ruby-commander-fastlane")
    (version "4.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "commander-fastlane" version))
       (sha256
        (base32
         "0y8d3ac9qwm1cg6rnpf8rcdsy1yxacrd2g2kl809xsp2vi973g65"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-highline" ,ruby-highline-1.7.10)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "The complete solution for Ruby command-line executables. Commander bridges the gap between other terminal related libraries you know and love (OptionParser, HighLine), while providing many new features, and an elegant API.")
    (description
     "The complete solution for Ruby command-line executables.  Commander bridges the gap between other terminal related libraries you know and love (OptionParser, HighLine), while providing many new features, and an elegant API.")
    (home-page
     "https://github.com/fastlane/commander")
    (license license:expat)))

(define-public ruby-colored
  (package
    (name "ruby-colored")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "colored" version))
       (sha256
        (base32
         "0b0x5jmsyi0z69bm6sij1k89z7h0laag3cb4mdn7zkl9qmxb90lx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "  >> puts \"this is red\".red
 
  >> puts \"this is red with a blue background (read: ugly)\".red_on_blue

  >> puts \"this is red with an underline\".red.underline

  >> puts \"this is really bold and really blue\".bold.blue

  >> logger.debug \"hey this is broken!\".red_on_yellow     # in rails

  >> puts Color.red \"This is red\" # but this part is mostly untested
")
    (description
     "  >> puts \"this is red\".red
 
  >> puts \"this is red with a blue background (read: ugly)\".red_on_blue

  >> puts \"this is red with an underline\".red.underline

  >> puts \"this is really bold and really blue\".bold.blue

  >> logger.debug \"hey this is broken!\".red_on_yellow     # in rails

  >> puts Color.red \"This is red\" # but this part is mostly untested
")
    (home-page "http://github.com/defunkt/colored")
    (license #f)))

(define-public ruby-cfpropertylist
  (package
    (name "ruby-cfpropertylist")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "CFPropertyList" version))
       (sha256
        (base32
         "1825ll26p28swjiw8n3x2pnh5ygsmg83spf82fnzcjn2p87vc5lf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "This is a module to read, write and manipulate both binary and XML property lists as defined by apple.")
    (description
     "This is a module to read, write and manipulate both binary and XML property lists as defined by apple.")
    (home-page
     "http://github.com/ckruse/CFPropertyList")
    (license license:expat)))

(define-public ruby-bundler
  (package
    (name "ruby-bundler")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bundler" version))
       (sha256
        (base32
         "12glbb1357x91fvd004jgkw7ihlkpc9dwr349pd7j83isqhls0ah"))))
    (build-system ruby-build-system)
    (synopsis
     "Bundler manages an application's dependencies through its entire life, across many machines, systematically and repeatably")
    (description
     "Bundler manages an application's dependencies through its entire life, across many machines, systematically and repeatably")
    (home-page "https://bundler.io/")
    (license license:expat)))

(define-public ruby-babosa
  (package
    (name "ruby-babosa")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "babosa" version))
       (sha256
        (base32
         "10nn9bw63i4awpzn5vrx6kmpx1sg7z8r3fhw9r8bvg9pz2wh489g"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "    A library for creating slugs. Babosa an extraction and improvement of the
    string code from FriendlyId, intended to help developers create similar
    libraries or plugins.
")
    (description
     "    A library for creating slugs.  Babosa an extraction and improvement of the
    string code from FriendlyId, intended to help developers create similar
    libraries or plugins.
")
    (home-page "http://github.com/norman/babosa")
    (license #f)))

(define-public ruby-aws-sdk-kms
  (package
    (name "ruby-aws-sdk-kms")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-kms" version))
       (sha256
        (base32
         "1lri9hx8fh852yq81w3x457pvswihq69dh8s3m9gd50b1lhybzfc"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-aws-sdk-core" ,ruby-aws-sdk-core)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Official AWS Ruby gem for AWS Key Management Service (KMS). This gem is part of the AWS SDK for Ruby.")
    (description
     "Official AWS Ruby gem for AWS Key Management Service (KMS).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license #f)))

(define-public ruby-jmespath
  (package
    (name "ruby-jmespath")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jmespath" version))
       (sha256
        (base32
         "1d4wac0dcd1jf6kc57891glih9w57552zgqswgy74d1xhgnk0ngf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis "Implements JMESPath for Ruby")
    (description "Implements JMESPath for Ruby")
    (home-page
     "http://github.com/trevorrowe/jmespath.rb")
    (license #f)))

(define-public ruby-aws-sigv4
  (package
    (name "ruby-aws-sigv4")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sigv4" version))
       (sha256
        (base32
         "1ll9382c1x2hp750cilh01h1cycgyhdr4cmmgx23k94hyyb8chv5"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-aws-eventstream" ,ruby-aws-eventstream)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Amazon Web Services Signature Version 4 signing library. Generates sigv4 signature for HTTP requests.")
    (description
     "Amazon Web Services Signature Version 4 signing library.  Generates sigv4 signature for HTTP requests.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license #f)))

(define-public ruby-aws-partitions
  (package
    (name "ruby-aws-partitions")
    (version "1.375.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-partitions" version))
       (sha256
        (base32
         "0fyh0f3qr1kx4dcrn3bp03zxrcjsvnbsm8rjvn0qn4wji07h2axq"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Provides interfaces to enumerate AWS partitions, regions, and services.")
    (description
     "This package provides interfaces to enumerate AWS partitions, regions, and services.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license #f)))

(define-public ruby-aws-eventstream
  (package
    (name "ruby-aws-eventstream")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-eventstream" version))
       (sha256
        (base32
         "0r0pn66yqrdkrfdin7qdim0yj2x75miyg4wp6mijckhzhrjb7cv5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Amazon Web Services event stream library. Decodes and encodes binary stream under `vnd.amazon.event-stream` content-type")
    (description
     "Amazon Web Services event stream library.  Decodes and encodes binary stream under `vnd.amazon.event-stream` content-type")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license #f)))

(define-public ruby-aws-sdk-core
  (package
    (name "ruby-aws-sdk-core")
    (version "3.107.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-core" version))
       (sha256
        (base32
         "0sqa5p4cczc2r1iivf3cl1s851i2vbizwjzq8jawii78lbamfwnp"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-aws-eventstream" ,ruby-aws-eventstream)
       ("ruby-aws-partitions" ,ruby-aws-partitions)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)
       ("ruby-jmespath" ,ruby-jmespath)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Provides API clients for AWS. This gem is part of the official AWS SDK for Ruby.")
    (description
     "This package provides API clients for AWS.  This gem is part of the official AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license #f)))

(define-public ruby-aws-sdk-s3
  (package
    (name "ruby-aws-sdk-s3")
    (version "1.81.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-s3" version))
       (sha256
        (base32
         "1hmjnp20x7g5arim6w23fa18pywnf94424r6vq184qw9blmba8vx"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-aws-sdk-core" ,ruby-aws-sdk-core)
       ("ruby-aws-sdk-kms" ,ruby-aws-sdk-kms)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "Official AWS Ruby gem for Amazon Simple Storage Service (Amazon S3). This gem is part of the AWS SDK for Ruby.")
    (description
     "Official AWS Ruby gem for Amazon Simple Storage Service (Amazon S3).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license #f)))


(define-public ruby-highline-1.7.10
  (package
    (inherit ruby-highline)
    (version "1.7.10")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "highline" version))
       (sha256
        (base32
         "01ib7jp85xjc4gh4jg0wyzllm46hwv8p0w1m4c75pbgi41fps50y"))))))

(define-public ruby-faraday-1
  (package
    (inherit ruby-faraday)
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday" version))
       (sha256
        (base32
         "0wwks9652xwgjm7yszcq5xr960pjypc07ivwzbjzpvy9zh2fw6iq"))))))

(define-public ruby-rubyzip-2.3.0
  (package
    (inherit ruby-rubyzip)
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rubyzip" version))
       (sha256
        (base32
         "0590m2pr9i209pp5z4mx0nb1961ishdiqb28995hw1nln1d1b5ji"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))))

(define-public ruby-fastlane
  (package
    (name "ruby-fastlane")
    (version "2.160.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "fastlane" version))
       (sha256
        (base32
         "1397bc2w282v3fyasiyln266mq4zar29f2nlx946hmpx8wzzkqv6"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-aws-sdk-s3" ,ruby-aws-sdk-s3)
       ("ruby-babosa" ,ruby-babosa)
       ("bundler" ,bundler)
       ("ruby-cfpropertylist" ,ruby-cfpropertylist)
       ("ruby-colored" ,ruby-colored)
       ("ruby-commander-fastlane"
        ,ruby-commander-fastlane)
       ("ruby-dotenv" ,ruby-dotenv)
       ("ruby-emoji-regex" ,ruby-emoji-regex)
       ("ruby-excon" ,ruby-excon)
       ("ruby-faraday" ,ruby-faraday-1)
       ("ruby-faraday-cookie-jar"
        ,ruby-faraday-cookie-jar)
       ("ruby-faraday-middleware"
        ,ruby-faraday-middleware)
       ("ruby-fastimage" ,ruby-fastimage)
       ("ruby-gh-inspector" ,ruby-gh-inspector)
       ("ruby-google-api-client"
        ,ruby-google-api-client)
       ("ruby-google-cloud-storage"
        ,ruby-google-cloud-storage)
       ("ruby-highline" ,ruby-highline-1.7.10)
       ("ruby-json" ,ruby-json)
       ("ruby-jwt" ,ruby-jwt)
       ("ruby-mini-magick" ,ruby-mini-magick)
       ("ruby-multipart-post" ,ruby-multipart-post)
       ("ruby-plist" ,ruby-plist)
       ("ruby-rubyzip" ,ruby-rubyzip-2.3.0)
       ("ruby-security" ,ruby-security)
       ("ruby-simctl" ,ruby-simctl)
       ("ruby-slack-notifier" ,ruby-slack-notifier)
       ("ruby-terminal-notifier"
        ,ruby-terminal-notifier)
       ("ruby-terminal-table" ,ruby-terminal-table)
       ("ruby-tty-screen" ,ruby-tty-screen)
       ("ruby-tty-spinner" ,ruby-tty-spinner)
       ("ruby-word-wrap" ,ruby-word-wrap)
       ("ruby-xcodeproj" ,ruby-xcodeproj)
       ("ruby-xcpretty" ,ruby-xcpretty)
       ("ruby-xcpretty-travis-formatter"
        ,ruby-xcpretty-travis-formatter)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check))))
    (synopsis
     "The easiest way to automate beta deployments and releases for your iOS and Android apps")
    (description
     "The easiest way to automate beta deployments and releases for your iOS and Android apps")
    (home-page "https://fastlane.tools")
    (license license:expat)))
