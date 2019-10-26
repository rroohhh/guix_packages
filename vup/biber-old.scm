(define-module (vup biber-old)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (gnu packages tex))

(define-public biber-2.11
  (package
    (name "biber")
    (version "2.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plk/biber/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              ;; TODO: Patch awaiting inclusion upstream (see:
              ;; https://github.com/plk/biber/issues/239).
              (patches (search-patches "biber-fix-encoding-write.patch"))
              (sha256
               (base32
                "0qgkc1k9n36yfmndwz879pak6mjphld0p85lzn9g2ng0vhxsifzz"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
		 (delete 'check)
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perl5lib (getenv "PERL5LIB")))
               (wrap-program (string-append out "/bin/biber")
                 `("PERL5LIB" ":" prefix
                   (,(string-append perl5lib ":" out
                                    "/lib/perl5/site_perl")))))
             #t)))))
    (inputs
     `(("perl-autovivification" ,perl-autovivification)
       ("perl-class-accessor" ,perl-class-accessor)
       ("perl-data-dump" ,perl-data-dump)
       ("perl-data-compare" ,perl-data-compare)
       ("perl-data-uniqid" ,perl-data-uniqid)
       ("perl-datetime-format-builder" ,perl-datetime-format-builder)
       ("perl-datetime-calendar-julian" ,perl-datetime-calendar-julian)
       ("perl-file-slurper" ,perl-file-slurper)
       ("perl-ipc-cmd" ,perl-ipc-cmd)
       ("perl-ipc-run3" ,perl-ipc-run3)
       ("perl-list-allutils" ,perl-list-allutils)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-mozilla-ca" ,perl-mozilla-ca)
       ("perl-regexp-common" ,perl-regexp-common)
       ("perl-log-log4perl" ,perl-log-log4perl)
       ;; We cannot use perl-unicode-collate here, because otherwise the
       ;; hardcoded hashes in the tests would differ.  See
       ;; https://mail-archive.com/debian-bugs-dist@lists.debian.org/msg1469249.html
       ;;("perl-unicode-collate" ,perl-unicode-collate)
       ("perl-unicode-normalize" ,perl-unicode-normalize)
       ("perl-unicode-linebreak" ,perl-unicode-linebreak)
       ("perl-encode-eucjpascii" ,perl-encode-eucjpascii)
       ("perl-encode-jis2k" ,perl-encode-jis2k)
       ("perl-encode-hanextra" ,perl-encode-hanextra)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-libxml-simple" ,perl-xml-libxml-simple)
       ("perl-xml-libxslt" ,perl-xml-libxslt)
       ("perl-xml-writer" ,perl-xml-writer)
       ("perl-sort-key" ,perl-sort-key)
       ("perl-text-csv" ,perl-text-csv)
       ("perl-text-csv-xs" ,perl-text-csv-xs)
       ("perl-text-roman" ,perl-text-roman)
       ("perl-uri" ,perl-uri)
       ("perl-text-bibtex" ,perl-text-bibtex)
       ("perl-libwww" ,perl-libwww)
       ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
       ("perl-business-isbn" ,perl-business-isbn)
       ("perl-business-issn" ,perl-business-issn)
       ("perl-business-ismn" ,perl-business-ismn)
       ("perl-lingua-translit" ,perl-lingua-translit)))
    (native-inputs
     `(("perl-config-autoconf" ,perl-config-autoconf)
       ("perl-extutils-libbuilder" ,perl-extutils-libbuilder)
       ("perl-module-build" ,perl-module-build)
       ;; for tests
       ("perl-file-which" ,perl-file-which)
       ("perl-test-more" ,perl-test-most) ; FIXME: "more" would be sufficient
       ("perl-test-differences" ,perl-test-differences)))
    (home-page "http://biblatex-biber.sourceforge.net/")
    (synopsis "Backend for the BibLaTeX citation management tool")
    (description "Biber is a BibTeX replacement for users of biblatex.  Among
other things it comes with full Unicode support.")
    (license license:artistic2.0)))
