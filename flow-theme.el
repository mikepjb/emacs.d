;;; flow-theme.el, Quiet theme + synthwave accents  -*- lexical-binding: t; -*-
(deftheme flow "Designed to encourage quiet focus")
(let (;; Primary colors
      (bg "#0c0c0c")			; background
      (fg "#e4e4e4")			; foreground
      
					;; Normal colors - darkened synthwave palette
      (black "#262626")			; quiet black
      (red "#8b0040")			; dark magenta-red
      (green "#004d26")			; dark teal-green
      (yellow "#664d00")		; dark amber
      (blue "#003366")			; dark electric blue
      (magenta "#4d0066")		; dark purple
      (cyan "#004d4d")			; dark cyan
      (white "#e0e6ff")			; cool white (same as fg)

      (mid-black "#464646")             ; used for UI mainly

					;; Bright colors - full neon synthwave
      (bright-black "#a8a8a8")		; quiet bright black
      (bright-red "#ff0080")		; hot pink/neon magenta
      (bright-green "#00ff80")		; electric green
      (bright-yellow "#ffb300")		; neon amber
      (bright-blue "#00ccff")		; electric cyan-blue
      (bright-magenta "#ff00ff")	; pure neon magenta
      (bright-cyan "#00ffff")		; pure cyan
      (bright-white "#ffffff"))		; pure white

  (custom-theme-set-faces
   'flow
   
   ;; Basic faces
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,bright-magenta :foreground ,bg))))
   `(region ((t (:background ,black))))
   `(fringe ((t (:background ,bg))))
   `(minibuffer-prompt ((t (:foreground ,fg))))
   `(hl-line ((t (:background ,black))))
   
   `(mode-line ((t (:background ,bg 
                    :foreground ,fg
                    :box (:line-width 1 :color ,black :style nil)))))
   `(mode-line-inactive ((t (:background ,bg
                             :foreground ,bright-black
                             :box (:line-width 1 :color ,black :style nil)))))
   
   `(mode-line-buffer-id ((t (:foreground ,bright-blue :weight bold))))
   
   ;; Additional modeline refinements
   `(mode-line-emphasis ((t (:foreground ,bright-red))))
   `(mode-line-highlight ((t (:foreground ,bright-blue))))

   `(vertical-border ((t (:foreground ,black))))
   
   ;; Success, info, and search faces using full color palette
   `(success ((t (:foreground ,bright-green))))
   `(info ((t (:foreground ,bright-cyan))))
   `(match ((t (:background ,bg :foreground ,bright-yellow))))
   `(isearch ((t (:background ,bright-magenta :foreground ,bg))))
   `(lazy-highlight ((t (:background ,magenta :foreground ,bg))))
   `(query-replace ((t (:background ,bright-yellow :foreground ,bg))))
   
   ;; Dired colors
   `(dired-directory ((t (:foreground ,bright-blue))))
   `(dired-executable ((t (:foreground ,bright-green))))
   `(dired-symlink ((t (:foreground ,bright-cyan))))
   `(dired-marked ((t (:foreground ,bright-yellow :weight bold))))
   
   ;; Compilation and grep
   `(compilation-info ((t (:foreground ,bright-green))))
   `(compilation-warning ((t (:foreground ,bright-yellow))))
   `(compilation-error ((t (:foreground ,bright-red))))
   `(grep-hit-face ((t (:foreground ,bright-magenta))))
   `(grep-match-face ((t (:background ,magenta :foreground ,bg))))
   
   ;; Version control (magit, vc, etc.)
   `(diff-added ((t (:foreground ,bright-green))))
   `(diff-removed ((t (:foreground ,bright-red))))
   `(diff-changed ((t (:foreground ,bright-yellow))))
   `(diff-header ((t (:foreground ,bright-blue))))
   `(diff-file-header ((t (:foreground ,bright-cyan :weight bold))))
   
   ;; Completions and minibuffer
   `(completions-highlight ((t (:background ,mid-black))))
   `(icomplete-selected-match ((t (:background ,mid-black))))
   `(completions-annotations ((t (:foreground ,bright-black))))
   `(completions-common-part ((t (:foreground ,bright-blue))))
   `(completions-first-difference ((t (:foreground ,bright-yellow))))
   
   ;; Syntax highlighting - mostly monochrome
   `(font-lock-comment-face ((t (:foreground ,bright-black))))
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
   `(line-number ((t (:foreground ,mid-black))))
   `(line-number-current-line ((t (:foreground ,bright-black :background ,black))))

   `(fill-column-indicator ((t (:foreground ,black))))
   
   ;; Headers
   `(markdown-header-face-1 ((t (:height 1.3 :weight bold :foreground ,bright-blue))))
   `(markdown-header-face-2 ((t (:height 1.2 :weight bold :foreground ,blue))))
   `(markdown-header-face-3 ((t (:height 1.1 :weight bold :foreground ,fg))))
   `(markdown-header-face-4 ((t (:height 1.0 :weight bold :foreground ,bright-black))))
   `(org-level-1 ((t (:height 1.3 :weight bold :foreground ,bright-blue))))
   `(org-level-2 ((t (:height 1.2 :weight bold :foreground ,bright-green))))
   `(org-level-3 ((t (:height 1.1 :weight bold :foreground ,fg))))
   `(org-level-4 ((t (:height 1.0 :weight bold :foreground ,bright-black))))
   `(org-level-5 ((t (:height 1.0 :weight bold :foreground ,black))))
   `(org-level-6 ((t (:height 1.0 :weight bold :foreground ,black))))
   `(org-level-7 ((t (:height 1.0 :weight bold :foreground ,black))))
   `(org-level-8 ((t (:height 1.0 :weight bold :foreground ,black))))
   `(org-ellipsis ((t (:foreground ,mid-black :underline nil))))

   ;; Language Specific
   `(sh-quoted-exec ((t (:foreground ,fg))))

   )

  (defun flow/truncate-buffer-name ()
    (let* ((name (if buffer-file-name 
                     (abbreviate-file-name buffer-file-name) 
                   (buffer-name)))
           (max-len (/ (window-width) 3)))
      (if (<= (length name) max-len)
          name
	(concat "<" (substring name (- (length name) (- max-len 1)))))))

  (let ((flow/mode-line
	 `(" "
	   (:eval (propertize
		   (flow/truncate-buffer-name)
		   'face
		   '(:foreground ,bright-blue :weight bold)))
	   " [%*]"
	   mode-line-format-right-align
	   (:eval (when (and (boundp 'vc-mode) vc-mode) ;; git branch + space
		    (concat (propertize " | " 'face '(:foreground ,mid-black))
			    (replace-regexp-in-string "^ Git[-:]" "" vc-mode))))
	   (:eval (propertize " | " 'face '(:foreground ,mid-black)))
	   "%l:%c" ;; line/col count
	   (:eval (propertize " | " 'face '(:foreground ,mid-black)))
	   (:eval (when-let ((proc (get-buffer-process (current-buffer))))
		    (propertize (format "[%s] " (process-name proc))
				'face '(:foreground ,bright-magenta :weight bold))))
	   (:eval (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
	   " ")))
    (setq mode-line-format flow/mode-line)
    (setq-default mode-line-format flow/mode-line)))

(provide-theme 'flow)

;; (progn (disable-theme 'flow) (load-theme 'flow t))

