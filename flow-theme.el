;;; flow-theme.el --- Quiet monochrome theme

(deftheme flow "Designed to encourage quiet focus")

(let ((bg "#0c0c0c")           ; background
      (fg "#e4e4e4")           ; foreground (white)
      (dim "#a8a8a8")          ; bright black (invisible gray)
      (dimmer "#262626")       ; normal black
      (red "#d70000")          ; normal red
      (bright-red "#ff005f")   ; bright red (error red)
      (blue "#0087af")         ; normal blue
      (bright-blue "#00afff")) ; bright blue (UI blue)

  (custom-theme-set-faces
   'flow
   
   ;; Basic faces
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,fg))))
   `(region ((t (:background ,dimmer))))
   `(mode-line ((t (:background ,bg :foreground ,fg))))
   `(mode-line-inactive ((t (:background ,bg :foreground ,fg))))
   `(fringe ((t (:background ,bg))))
   `(minibuffer-prompt ((t (:foreground ,fg))))
   `(hl-line ((t (:background ,dimmer))))
   
   ;; Syntax highlighting - mostly monochrome
   `(font-lock-comment-face ((t (:foreground ,dim))))
   `(font-lock-string-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   
   ;; Errors and warnings
   `(error ((t (:foreground ,bright-red))))
   `(warning ((t (:foreground ,red))))
   
   ;; Line numbers
   `(line-number ((t (:foreground ,dimmer))))
   `(line-number-current-line ((t (:foreground ,dim))))

   `(markdown-header-face-1 ((t (:height 1.3 :weight bold))))
   `(markdown-header-face-2 ((t (:height 1.2 :weight bold))))
   `(markdown-header-face-3 ((t (:height 1.1 :weight bold))))
   `(markdown-header-face-4 ((t (:height 1.0 :weight bold))))

   `(org-level-1 ((t (:height 1.3 :weight bold :foreground ,bright-red))))
   `(org-level-2 ((t (:height 1.2 :weight bold :foreground ,red))))
   `(org-level-3 ((t (:height 1.1 :weight bold :foreground ,fg))))
   `(org-level-4 ((t (:height 1.0 :weight bold :foreground ,dim))))
   `(org-level-5 ((t (:height 1.0 :weight bold :foreground ,dimmer))))
   `(org-level-6 ((t (:height 1.0 :weight bold :foreground ,dimmer))))
   `(org-level-7 ((t (:height 1.0 :weight bold :foreground ,dimmer))))
   `(org-level-8 ((t (:height 1.0 :weight bold :foreground ,dimmer))))
   
   ))

(provide-theme 'flow)
