(define-module (vup pidgin-libnotify)
  #:use-module (gnu packages)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public pidgin-libnotify
  (package
	(name "pidgin-libnotify")
	(version "0.14")
	(source
	 (origin
	   (method url-fetch)
	   (uri "https://downloads.sourceforge.net/gaim-libnotify/pidgin-libnotify-0.14.tar.gz")
	   (sha256
		(base32 "1gisxj0a5bg473abq4lm31v62y404cgqy5vlk7rksj0a1vrakx3l"))
	   (patches (search-patches "pidgin-libnotify-0.14-libnotify-0.7.patch"
								"pidgin-libnotify-getfocus.patch"
								"pidgin-libnotify-language_fixes.patch"
								"pidgin-libnotify-showbutton.patch"))
	   (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "src/Makefile.in"
             (("^gddir =.*")
              (string-append
               "gddir := @libdir@/purple-2\n")))
           #t))
	   ))

	(build-system gnu-build-system)
	(arguments '(#:configure-flags '("--disable-deprecated")))
	(native-inputs `(("intltool" ,intltool)
					 ("pkg-config" ,pkg-config)))
	(inputs `(("pidgin" ,pidgin)
			  ("libnotify" ,libnotify)
			  ("gtk+2" ,gtk+-2)))
	(home-page "http://gaim-libnotify.sourceforge.net/")
	(synopsis "Pidgin plugin that enables popups when someone logs in or messages you")
	(description "Pidgin plugin that enables popups when someone logs in or messages you")
	(license gpl2)))
