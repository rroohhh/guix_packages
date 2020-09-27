;;; most code taken from https://github.com/flatwhatson/guix-channel/blob/df39c83/flat/packages/emacs.scm
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Andrew Whatson <whatson@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define-module (vup emacs)
  #:use-module (guix packages)
  #:use-module (guix memoization)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (flat packages gcc))

(define emacs-with-native-comp
  (mlambda (emacs gcc)
    (let ((libgccjit (libgccjit-for-gcc gcc)))
      (package
        (inherit emacs)
        (arguments
         (substitute-keyword-arguments (package-arguments emacs)
           ((#:configure-flags flags)
            `(cons* "--with-nativecomp" ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               ;; Add build-time library paths for libgccjit.
               (add-before 'configure 'set-libgccjit-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((libgccjit-libdir
                          (string-append (assoc-ref inputs "libgccjit")
                                         "/lib/gcc/" %host-type "/"
                                         ,(package-version libgccjit) "/")))
                     (setenv "LIBRARY_PATH"
                             (string-append libgccjit-libdir ":"
                                            (getenv "LIBRARY_PATH"))))
                   #t))
               ;; Add runtime library paths for libgccjit.
               (add-before 'restore-emacs-pdmp 'wrap-library-path
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((gcc-libdir
                           (string-append (assoc-ref inputs "gcc:lib")
                                          "/lib/"))
                          (glibc-libdir
                           (string-append (assoc-ref inputs "glibc")
                                          "/lib/"))
                          (libgccjit-libdir
                           (string-append (assoc-ref inputs "libgccjit")
                                          "/lib/gcc/" %host-type "/"
                                          ,(package-version libgccjit) "/"))
                          (library-path (list gcc-libdir
                                              glibc-libdir
                                              libgccjit-libdir))
                          (output   (assoc-ref outputs "out"))
                          (bindir   (string-append output "/bin"))
                          (libexec  (string-append output "/libexec"))
                          (bin-list (append (find-files bindir ".*")
                                            (find-files libexec ".*"))))
                     (for-each (lambda (program)
                                 (unless (wrapper? program)
                                   (wrap-program
                                       program
                                     `("LIBRARY_PATH" prefix ,library-path))))
                               bin-list))
                   #t))
               ;; Remove wrappers around .eln files in libexec.
               (add-after 'restore-emacs-pdmp 'unwrap-eln-files
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((output   (assoc-ref outputs "out"))
                          (libexec  (string-append output "/libexec"))
                          (eln-list (find-files libexec "\\.eln$")))
                     (for-each (lambda (wrapper)
                                 (let ((real (string-append
                                              (dirname wrapper) "/."
                                              (basename wrapper) "-real")))
                                   (delete-file wrapper)
                                   (rename-file real wrapper)))
                               eln-list)
                     #t)))))))
        (native-inputs
         `(("gcc" ,gcc)
           ,@(package-native-inputs emacs)))
        (inputs
         `(("gcc:lib" ,gcc "lib")
           ("glibc" ,glibc)
           ("libgccjit" ,libgccjit)
           ,@(package-inputs emacs)))))))

(define emacs-with-xwidgets
  ;; FIXME xwidgets-webkit is missing TLS/SSL support
  ;; FIXME memory leak? needs more testing
  (mlambda (emacs)
    (package
      (inherit emacs)
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:configure-flags flags)
          `(cons* "--with-xwidgets" ,flags))))
      (inputs
       `(("glib-networking" ,glib-networking)
         ("webkitgtk" ,webkitgtk)
         ,@(package-inputs emacs))))))

(define emacs-with-pgtk
  (mlambda (emacs)
    (package
      (inherit emacs)
      (inputs
       `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
         ,@(package-inputs emacs)))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:configure-flags flags)
          `(cons* "--with-pgtk" ,flags))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'wrap-for-gnome-settings
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((output   (assoc-ref outputs "out"))
                        (bindir   (string-append output "/bin"))
                        (bin-list (append (find-files bindir ".*"))))
                   (for-each (lambda (program)
                               (unless (wrapper? program)
                                 (wrap-program
                                     program
                                   `("XDG_DATA_DIRS" ":" suffix (,(string-append (assoc-ref inputs "gsettings-desktop-schemas") "/share"))))))
                             bin-list)
                 #t)))))
         )))))

(define emacs-from-git
  (lambda* (emacs #:key pkg-name pkg-version pkg-revision git-repo git-commit checksum)
    (package
      (inherit emacs)
      (name pkg-name)
      (version (git-version pkg-version pkg-revision git-commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url git-repo)
               (commit git-commit)))
         (sha256 (base32 checksum))
         (file-name (git-file-name pkg-name pkg-version))
         (patches (origin-patches (package-source emacs)))
         (modules (origin-modules (package-source emacs)))
         (snippet (origin-snippet (package-source emacs)))))
      (native-search-paths
       (list (search-path-specification
              (variable "EMACSLOADPATH")
              (files
               (list "share/emacs/site-lisp"
                     (string-append "share/emacs/" pkg-version "/lisp"))))
             (search-path-specification
              (variable "INFOPATH")
              (files '("share/info"))))))))

(define-public emacs-pgtk-native-comp-no-xwidgets
  (emacs-from-git
   (emacs-with-pgtk
     (emacs-with-native-comp emacs-next gcc-10))
   #:pkg-name "emacs-pgtk-native-comp-no-xwidgets"
   #:pkg-version "28.0.50"
   #:pkg-revision "0"
   #:git-repo "https://github.com/flatwhatson/emacs.git"
   #:git-commit "890a5bf910d5d0c4df44706976c360103cdbe8aa"
   #:checksum "1v177137yhgmz8d0i5h97isi1isym4a22wkhb0fsjp5g3bi18v3p"))
