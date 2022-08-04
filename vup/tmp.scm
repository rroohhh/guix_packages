(define-module (vup tmp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages language)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages telegram)
  #:use-module ((guix licenses) #:prefix license:))


(define-public hime-fixed
  (package
   (inherit hime)
   (inputs
    (modify-inputs
     (package-inputs hime)
     (append qtbase-5)))
   (arguments
    (substitute-keyword-arguments (package-arguments hime)
      ((#:phases phases)
       `(modify-phases ,phases
          (replace 'qt-wrap
            (lambda* (#:key outputs inputs #:allow-other-keys)
              ((assoc-ref qt:%standard-phases 'qt-wrap) #:inputs inputs #:outputs outputs #:qtbase (assoc-ref inputs "qtbase"))
              #t))))))))

(define-public nimf-fixed
  (package
   (inherit nimf)
   (inputs
    (modify-inputs
     (package-inputs nimf)
     (append qtbase-5)))
   (arguments
    (substitute-keyword-arguments (package-arguments nimf)
      ((#:phases phases)
       `(modify-phases ,phases
          (replace 'qt-wrap
            (lambda* (#:key outputs inputs #:allow-other-keys)
              ((assoc-ref qt:%standard-phases 'qt-wrap) #:inputs inputs #:outputs outputs #:qtbase (assoc-ref inputs "qtbase"))
              #t))))))))

(define-public telegram-desktop-fixed
  (package
   (inherit telegram-desktop)
   (inputs
    (modify-inputs
     (package-inputs telegram-desktop)
     (replace "hime" hime-fixed)
     (replace "nimf" nimf-fixed)))))
