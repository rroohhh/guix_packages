(define-module (vup home i3)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (i3-configuration
    i3-bar-configuration
    i3-bar-colors-configuration
    i3-bar-color
    make-i3-bar-color
    i3-font-configuration
    i3-color-configuration
    i3-color-spec
    make-i3-color-spec
    ))

(define-record-type* <i3-configuration> 
    i3-configuration make-i3-configuration i3-configuration?
    (extra-config i3-configuration-extra-config (default '()))
    (colors i3-configuration-colors)
    (font i3-configuration-font)
    (bar i3-configuration-bar)
    (inner-gaps i3-configuration-inner-gaps (default #f))
    (outer-gaps i3-configuration-inner-gaps (default #f))
    (smart-gaps i3-configuration-smart-gaps (default #f))
    (smart-borders i3-configuration-smart-gaps (default #f))
    (floating-modifier i3-configuration-floating-modifier)
    (key-bindings i3-configuration-key-bindings (default '())))

(define-record-type* <i3-bar-configuration>
    i3-bar-configuration make-i3-bar-configuration i3-bar-configuration?
    (position i3-bar-configuration-position)
    (workspace-buttons i3-bar-configuration-workspace-buttons)
    (status-command i3-bar-configuration-status-command)
    (font i3-bar-configuration-font)
    (strip-workspace-numbers i3-bar-configuration-strip-workspace-numbers)
    (colors i3-bar-configuration-colors))

(define-record-type* <i3-bar-colors-configuration>
    i3-bar-colors-configuration make-i3-bar-colors-configuration i3-bar-colors-configuration?
    (statusline i3-bar-colors-configuration-statusline)
    (background i3-bar-colors-configuration-background)
    (separator i3-bar-colors-configuration-separator)
    (focused-workspace i3-bar-colors-configuration-focused-workspace)
    (inactive-workspace i3-bar-colors-configuration-inactive-workspace)
    (active-workspace i3-bar-colors-configuration-active-workspace)
    (urgent-workspace i3-bar-colors-configuration-urgent-workspace)
    (binding-mode i3-bar-colors-configuration-binding-mode))

(define-record-type* <i3-bar-color>
    i3-bar-color make-i3-bar-color i3-bar-color?
    (border i3-bar-color-border)
    (background i3-bar-color-background)
    (text i3-bar-color-text))

(define-record-type* <i3-font-configuration> 
    i3-font-configuration make-i3-font-configuration i3-font-configuration?
    (families i3-font-configuration-name)
    (size i3-size-configuration-size))

(define-record-type* <i3-color-configuration>
    i3-color-configuration make-i3-color-configuration i3-color-configuration?
    (focused i3-color-configuration-focused)
    (focused-inactive i3-color-configuration-focused-inactive)
    (unfocused i3-color-configuration-unfocused)
    (urgent i3-color-configuration-urgent)
    (placeholder i3-color-configuration-placeholder)
    (background i3-color-configuration-background))


(define (generate-i3-color-config config)
  (match config
    (($ <i3-color-configuration> focused focused-inactive
        unfocused urgent placeholder background)
     (list
       (format #f "client.focused ~a" (generate-i3-color focused))
       (format #f "client.focused_inactive ~a" (generate-i3-color focused-inactive))
       (format #f "client.unfocused ~a" (generate-i3-color unfocused))
       (format #f "client.urgent ~a" (generate-i3-color urgent))
       (format #f "client.placeholder ~a" (generate-i3-color placeholder))
       (format #f "client.background ~a" background)))))


(define-record-type* <i3-color-spec>
    i3-color-spec make-i3-color-spec i3-color-spec?
    (border i3-color-spec-border)
    (background i3-color-spec-border)
    (text i3-color-spec-border)
    (indicator i3-color-spec-border)
    (child-border i3-color-spec-border))

(define (generate-i3-color config)
  (match config
    (($ <i3-color-spec> border background text
        indicator child-border)
     (format #f "~a ~a ~a ~a ~a" border background text indicator child-border))))

(define (generate-i3-font-config config)
  (match config
         (($ <i3-font-configuration> families size)
          (list (format #f "font pango:~a ~a"
            (string-join families ", ")
            size)))))

(define (generate-i3-key-bindings config)
  (map 
    (lambda (binding) 
      (format #f "bindsym ~a ~a"
        (string-join (car binding) "+")
        (cadr binding))) 
    config))

(define (i3-yes-no bool)
  (match bool
         (#t "yes")
         (#f "no")))

(define (generate-i3-bar-color config)
  (match config
    (($ <i3-bar-color> border background text)
     (format #f "~a ~a ~a" border background text))))

(define (generate-i3-bar-color-config config)
  (match config
    (($ <i3-bar-colors-configuration> statusline background 
        separator focused-workspace inactive-workspace 
        active-workspace urgent-workspace binding-mode)
     (list
       "colors {"
       (format #f "statusline ~a" statusline)
       (format #f "background ~a" background)
       (format #f "separator ~a" separator)
       (format #f "focused_workspace ~a" (generate-i3-bar-color focused-workspace))
       (format #f "inactive_workspace ~a" (generate-i3-bar-color inactive-workspace))
       (format #f "active_workspace ~a" (generate-i3-bar-color active-workspace))
       (format #f "urgent_workspace ~a" (generate-i3-bar-color urgent-workspace))
       (format #f "binding_mode ~a" (generate-i3-bar-color binding-mode))
       "}"))))

(define (generate-i3-bar-config config)
  (match config
    (($ <i3-bar-configuration> position workspace-buttons 
        status-command font strip-workspace-numbers colors)
      `("bar {"
        ,(format #f "workspace_buttons ~a" (i3-yes-no workspace-buttons))
        ,(format #f "strip_workspace_numbers ~a" (i3-yes-no strip-workspace-numbers))
        ,(format #f "status_command ~a" status-command)
        ,@(generate-i3-font-config font)
        ,(format #f "position ~a" position)
        ,@(generate-i3-bar-color-config colors)
        "}")
     )))


(define (generate-i3-config config)
  (match config
         (($ <i3-configuration> extra-config colors font bar 
             inner-gaps outer-gaps smart-gaps 
             smart-borders floating-modifier 
             key-bindings)
          (string-join (append 
                         (list 
                           (if inner-gaps
                               (format #f "gaps inner ~d" inner-gaps))
                           (if outer-gaps
                               (format #f "gaps outer ~d" outer-gaps))
                           (if smart-gaps
                               (format #f "smart_gaps on"))
                           (if smart-borders
                               (format #f "smart_borders on"))
                           (string-append "floating_modifier " floating-modifier))
                         extra-config
                         (generate-i3-font-config font)
                         (generate-i3-bar-config bar)
                         (generate-i3-color-config colors)
                         (generate-i3-key-bindings key-bindings)
                         )
                       "\n"))))

(define (i3-home config)
  (computed-file "i3-home"
    #~(let ((config #$(plain-file "config" (generate-i3-config config)))
	    (i3-dir (string-append #$output "/.config/i3")))
	(use-modules (guix build utils))
	(mkdir-p i3-dir)
	(copy-file config (string-append i3-dir "/config")))
    #:options
    '(#:local-build? #t
      #:modules ((guix build utils)))))


(let*
  ((mod "Mod4")
   (left "s")
   (down "n")
   (up "t")
   (right "l")
   (bg-color "#282828")
   (bg-hl-color "#383838")
   (fg-color "#ebdbb2")
   (border-color "#ebdbb2")
   (fonts '("Hack" "FontAwesome"))
   (config 
     (i3-configuration
       (extra-config 
         '("for_window [ title=\"^pdfpc - present\" ] border none floating enable"
           "for_window [ title=\"^org.anbox.*\" ] border none floating enable"
           "default_border pixel 3"))
       (bar (i3-bar-configuration
                (position "top") 
                (workspace-buttons #t) 
                (status-command "i3blocks") 
                (font (i3-font-configuration
                        (families fonts)
                        (size 11))) 
                (strip-workspace-numbers #t)
                (colors (i3-bar-colors-configuration
                          (statusline fg-color)
                          (background bg-color)
                          (separator bg-color)
                          (focused-workspace 
                            (make-i3-bar-color bg-color bg-color "#98971a"))
                          (inactive-workspace 
                            (make-i3-bar-color bg-color bg-color "#928374"))
                          (active-workspace 
                            (make-i3-bar-color bg-color bg-color "#98971a"))
                          (urgent-workspace 
                            (make-i3-bar-color bg-color bg-color "#cc241d"))
                          (binding-mode
                            (make-i3-bar-color bg-color bg-color "#cc241d"))))))
       (colors (i3-color-configuration 
                 (focused 
                   (make-i3-color-spec bg-color bg-color fg-color "#666666" "#5f676a"))
                 (focused-inactive 
                   (make-i3-color-spec bg-color bg-color fg-color bg-hl-color bg-hl-color))
                 (unfocused 
                   (make-i3-color-spec bg-hl-color bg-hl-color fg-color bg-hl-color "#222222"))
                 (urgent 
                   (make-i3-color-spec "#2f343a" "#900000" fg-color bg-hl-color "#900000"))
                 (placeholder 
                   (make-i3-color-spec "#000000" "#0c0c0c" fg-color bg-hl-color "#ffffff"))
                 (background bg-color)))
       (font (i3-font-configuration
               (families fonts)
               (size 8)))
       (floating-modifier mod)
       (inner-gaps 10)
       (outer-gaps 7)
       (smart-gaps #t)
       (smart-borders #t)
       (key-bindings
         `(((,mod "f") "fullscreen toggle")
           ((,mod "Return") "exec alacritty")
           ((,mod "Shift" "q") "kill")
           ((,mod "d") "exec rofi -show run")
           ((,mod ,left) "focus left")
           ((,mod ,down) "focus down")
           ((,mod ,up) "focus up")
           ((,mod ,right) "focus right")
           ((,mod "a") "focus parent")
           ((,mod "Shift" ,left) "move left")
           ((,mod "Shift" ,down) "move down")
           ((,mod "Shift" ,up) "move up")
           ((,mod "Shift" ,right) "move right")
           ((,mod "Shift" "v") "split h")
           ((,mod "v") "split v")
           ((,mod "w") "layout tabbed")
           ((,mod "e") "layout toggle split")
           ((,mod "space") "floating toggle")
           ((,mod "Shift" "c") "reload")
           ((,mod "Shift" "r") "restart")
           ((,mod "Shift" "e") "exit")
           ,@(fold-right append '() (map (lambda (x)
                   (let*
                     ((ws-num (+ x 1))
                      (ws (format #f "~a" ws-num))
                      (ws-key (modulo ws-num 10))
                      (ws-key (format #f "~a" ws-key))
                      (ws-pretty #("¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹" "⁰"))
                      (ws-name (format #f "~a:~a" ws (array-ref ws-pretty x))))
                     `(((,mod ,ws-key) 
                        ,(string-join (list "workspace" ws-name)))
                       ((,mod "Shift" ,ws-key) 
                        ,(string-join (list "move container to workspace" ws-name)))))) 
                 (iota 10))))))))
  (display (generate-i3-config config)))

