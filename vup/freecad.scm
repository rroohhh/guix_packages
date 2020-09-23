(define-module (vup freecad)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages engineering))

(define-public freecad-fixed
  (package
    (inherit freecad)
    (name "freecad-fixed")
    (arguments
     (substitute-keyword-arguments (package-arguments freecad)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-pyside2-tool-detection
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "cMake/FindPySide2Tools.cmake"
                 (("pyside2-uic") "pyside2-uic uic")
                 (("pyside2-rcc") "pyside2-uic rcc"))
               #t))))))))
