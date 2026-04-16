;;; flow-theme.el, Quiet theme + synthwave accents  -*- lexical-binding: t; -*-
(deftheme flow "Designed to encourage quiet focus")

(defcustom flow-mode 'dark
  "Theme variant. Either 'dark or 'light"
  :type '(choice (const :tag "Dark" 'dark)
                 (const :tag "Light" 'light))
  :group 'flow)

(defun flow-toggle-theme ()
  (interactive)
  (setq flow-mode
        (if (eq flow-mode 'dark) 'light 'dark))
  (load-theme 'flow t))

(let* ((dark-p (eq flow-mode 'dark))
       ;; Color organised as a pyramid for attention, with small areas
       ;; of interest down to the main bulk of the view. This should
       ;; also dictate color choice in UI i.e you don't want to draw
       ;; the eye with a yellow for truncation symbols in the fringes.

       ;; Top layer, high interest - buffer names, errors/warnings,
       ;; prompts, cursor. Do we need all 4? Especially as magenta is
       ;; close to purple/lavender.
       (yellow         (if dark-p "#f9e2af" "#df8e1d"))       ; catppucin mocha yellow
       (magenta        (if dark-p "#cba6f7" "#8839ef"))       ; purple
       (cyan           (if dark-p "#89dceb" "#04a5e5"))       ; muted cyan
       (bright-magenta "#ff00ff")       ; pure neon magenta - cursor

       ;; Middle layer, low but significant interest, function names
       ;; etc that appear frequently.
       (lavender       (if dark-p "#b7bdf8" "#7287fd"))
       (teal           (if dark-p "#94e2d5" "#179299"))
       (sapphire       (if dark-p "#74c7ec" "#209fb5"))

       ;; Main foreground/background used for the rest of the view.
       (fg             (if dark-p "#cdd6f4" "#4c4f69"))
       (fg+            (if dark-p "#bac2de" "#5c5f77"))
       (fg++           (if dark-p "#a6adc8" "#6c6f85"))
       (fg+++          (if dark-p "#6c7086" "#9ca0b0"))

       (bg             (if dark-p "#0a0a10" "#eff1f5"))
       ;; +colors are darker/lighter depending on theme for consistent depth effect.
       (bg+            (if dark-p "#11111b" "#eff1f5"))
       (bg++            (if dark-p "#181825" "#e6e9ef"))
       (bg+++          (if dark-p "#1e1e2e" "#dce0e8"))
       (bg+green       (if dark-p "#0c1412" "#d1fde6")) ;; for git diffs
       (bg+green+       (if dark-p "#0e211c" "#bffbdc"))
       (bg+red         (if dark-p "#1f0809" "#f4d3d5"))
       (bg+red+         (if dark-p "#350f11" "#ecb9bc"))

       ;; rest/unsorted - to be deprecated most likely.
       ;; not sure black etc really works here? for dual theme
       (red            (if dark-p "#eba0ac" "#e64553"))       ; catppucin mocha maroon
       (green          (if dark-p "#a6e3a1" "#40a02b"))       ; muted green
       ;; may want a more faded yellow
       (blue           (if dark-p "#89b4fa" "#1e66f5"))       ; catppucin mocha sapphire


       ;; Bright/neon colors (from alacritty bright palette)
       (bright-black   "#787878")       ; invisible gray
       (bright-green   "#00ff80")       ; electric green
       (bright-yellow  "#ffb300")       ; neon amber
       (bright-blue    "#00ccff")       ; electric cyan-blue
       (bright-magenta "#ff00ff")       ; pure neon magenta - cursor
       (bright-cyan    "#00ffff")       ; pure cyan
       (bright-white   "#ffffff")

       ;; Semantic colors set from the above
       (match yellow)

       )

  (custom-theme-set-faces
   'flow

   ;; Basic faces
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,bright-magenta :foreground ,bg))))
   `(region ((t (:background ,bg++)))) ;; highlight
   `(fringe ((t (:background ,bg :foreground ,bg+++))))
   `(minibuffer-prompt ((t (:foreground ,green))))
   `(hl-line ((t (:background ,bg+))))

   `(mode-line ((t (:background ,bg
                                :foreground ,fg
                                :box (:line-width 1 :color ,bg++ :style nil)))))
   `(mode-line-inactive ((t (:background ,bg
                                         :foreground ,fg
                                         :box (:line-width 1 :color ,bg++ :style nil)))))

   ;; Doesn't work for custom modeline
   `(mode-line-buffer-id ((t (:foreground ,cyan :weight bold))))

   ;; Additional modeline refinements
   `(mode-line-emphasis ((t (:foreground ,yellow))))
   `(mode-line-highlight ((t (:foreground ,teal))))

   ;; Misc. UI
   `(vertical-border ((t (:foreground ,bg+))))
   `(link ((t (:foreground ,lavender :underline t))))
   `(escape-glyph ((t (:foreground ,bg+))))
   `(icon ((t (:foreground ,yellow))))
   `(xref-line-number ((t (:foreground ,yellow))))

   ;; Success, info, and search faces using full color palette
   `(success ((t (:foreground ,green))))
   `(info ((t (:foreground ,lavender))))
   `(match ((t (:background ,bg :foreground ,match))))
   `(isearch ((t (:background ,match :foreground ,bg))))
   `(lazy-highlight ((t (:background ,match :foreground ,bg))))
   `(query-replace ((t (:background ,green :foreground ,bg))))

   ;; Dired colors
   `(dired-directory ((t (:foreground ,teal))))
   `(dired-executable ((t (:foreground ,sapphire))))
   `(dired-symlink ((t (:foreground ,lavender))))
   `(dired-marked ((t (:foreground ,yellow :weight bold))))

   ;; Compilation and grep
   `(compilation-info ((t (:foreground ,green))))
   `(compilation-warning ((t (:foreground ,yellow))))
   `(compilation-error ((t (:foreground ,magenta))))
   `(grep-hit-face ((t (:foreground ,magenta))))
   `(grep-match-face ((t (:background ,match :foreground ,bg))))

   ;; Version control (magit, vc, etc.)
   `(diff-added ((t (:background ,bg+green))))
   `(diff-refine-added ((t (:background ,bg+green+))))
   `(diff-removed ((t (:background ,bg+red))))
   `(diff-refine-removed ((t (:background ,bg+red+))))
   `(diff-changed ((t (:foreground ,yellow))))
   `(diff-header ((t (:foreground ,sapphire))))
   `(diff-file-header ((t (:foreground ,cyan :weight bold))))

   ;; Completions and minibuffer
   `(completions-highlight ((t (:foreground ,fg :background ,bg+++))))
   `(icomplete-selected-match ((t (:foreground ,fg :background ,bg+++))))
   `(completions-annotations ((t (:foreground ,teal))))
   `(completions-common-part ((t (:foreground ,fg++))))
   `(completions-first-difference ((t (:foreground ,fg+++))))

   ;; Syntax highlighting - mostly monochrome
   `(font-lock-comment-face ((t (:foreground ,fg+++))))
   `(font-lock-string-face ((t (:foreground ,teal))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-function-name-face ((t (:foreground ,sapphire))))
   `(font-lock-variable-name-face ((t (:foreground ,lavender))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-constant-face ((t (:foreground ,sapphire))))
   `(font-lock-builtin-face ((t (:foreground ,fg++))))

   ;; Errors and warnings
   `(error ((t (:foreground ,red))))
   `(warning ((t (:foreground ,yellow))))

   ;; Line numbers
   `(line-number ((t (:foreground ,bg+++))))
   `(line-number-current-line ((t (:foreground ,fg+++ :background ,bg+++))))

   `(fill-column-indicator ((t (:foreground ,bg))))

   ;; Headers
   `(markdown-header-face-1 ((t (:height 1.3 :weight bold :foreground ,sapphire))))
   `(markdown-header-face-2 ((t (:height 1.2 :weight bold :foreground ,teal))))
   `(markdown-header-face-3 ((t (:height 1.1 :weight bold :foreground ,lavender))))
   `(markdown-header-face-4 ((t (:height 1.0 :weight bold :foreground ,fg))))
   `(org-level-1 ((t (:height 1.6 :weight bold :foreground ,fg
                              :box (:line-width (-1 . 32) :color ,bg)))))
   `(org-level-2 ((t (:height 1.3 :slant italic :foreground ,fg
                              :box (:line-width (-1 . 20) :color ,bg)))))
   `(org-level-3 ((t (:height 1.0 :weight bold :slant italic :foreground ,fg
                              :box (:line-width (-1 . 12) :color ,bg)))))
   `(org-level-4 ((t (:height 1.0 :weight bold :foreground ,fg))))
   `(org-level-5 ((t (:height 1.0 :foreground ,fg))))
   `(org-level-6 ((t (:height 1.0 :foreground ,fg))))
   `(org-ellipsis ((t (:foreground ,bg++ :underline nil))))
   `(org-headline-done ((t (:foreground ,fg+++ :underline nil))))
   `(org-done ((t (:foreground ,green :underline nil))))
   `(org-todo ((t (:foreground ,sapphire :underline nil))))
   `(org-tag ((t (:foreground ,fg+++ :underline nil))))
   `(org-hide ((t (:foreground ,bg :inherit fixed-pitch))))

   ;; Monospace faces
   `(org-block            ((t (:inherit fixed-pitch))))
   `(org-block-begin-line ((t (:inherit fixed-pitch))))
   `(org-block-end-line   ((t (:inherit fixed-pitch))))
   `(org-code             ((t (:inherit fixed-pitch))))
   `(org-verbatim         ((t (:inherit fixed-pitch))))
   `(org-table            ((t (:inherit fixed-pitch))))
   `(org-formula          ((t (:inherit fixed-pitch))))
   `(org-checkbox         ((t (:inherit fixed-pitch))))
   `(org-meta-line        ((t (:inherit fixed-pitch))))  ; #+title, #+begin_src etc
   `(org-drawer           ((t (:inherit fixed-pitch))))  ; :PROPERTIES: etc
   ;; `(org-special-keyword  ((t (:inherit fixed-pitch))))  ; SCHEDULED, DEADLINE etc
   `(org-todo             ((t (:inherit fixed-pitch :weight bold :foreground ,sapphire))))
   `(org-done             ((t (:inherit fixed-pitch :weight bold :foreground ,green))))

   ;; Eshell
   `(eshell-prompt ((t (:foreground ,yellow))))

   ;; Language Specific
   `(sh-quoted-exec ((t (:foreground ,fg))))

   ;; Terminal
   `(ansi-color-black ((t (:foreground ,bg :background ,bg))))
   `(ansi-color-red ((t (:foreground ,red :background ,bg))))
   `(ansi-color-green ((t (:foreground ,green :background ,bg))))
   `(ansi-color-yellow ((t (:foreground ,yellow :background ,bg))))
   `(ansi-color-magenta ((t (:foreground ,magenta :background ,bg))))

   ;; Parentheses and brackets
   `(font-lock-paren-face ((t (:foreground ,lavender))))
   `(font-lock-bracket-face ((t (:foreground ,sapphire))))
   `(font-lock-brace-face ((t (:foreground ,teal))))
   )

  ;; When fringe-mode is 0, I am not sure the truncation chars are
  ;; fontified so we set this manually and update the glyph while
  ;; we're at it.
  (set-display-table-slot
   standard-display-table 0
   (make-glyph-code ?… 'escape-glyph))

  (defface font-lock-paren-face nil "Face for parentheses." :group 'font-lock-faces)
  (defface font-lock-bracket-face nil "Face for brackets." :group 'font-lock-faces)
  (defface font-lock-brace-face nil "Face for braces." :group 'font-lock-faces)

  (defun flow-delimiters-enable ()
    (font-lock-add-keywords
     nil
     '(("(\\|)" 0 'font-lock-paren-face t)
       ("\\[\\|\\]" 0 'font-lock-bracket-face t)
       ("{\\|}" 0 'font-lock-brace-face t))
     'append)
    (font-lock-flush))

  (add-hook 'prog-mode-hook #'flow-delimiters-enable)

  (defun flow-org-style-bullets ()
    ;; Hide levels 1-3 stars completely (invisible)
    (font-lock-add-keywords
     nil
     '(("^\\*\\{1,3\\} "
        (0 (put-text-property (match-beginning 0)
                              (match-end 0)
                              'invisible t)
           nil)))
     'append)

    ;; Replace level 4+ stars with a bullet •
    (font-lock-add-keywords
     nil
     '(("^\\(\\*\\{4,\\}\\) "
        (1 (prog1 () (compose-region (match-beginning 1)
                                     (match-end 1)
                                     "	•")))))
     'append)

    ;; Replace - list markers with bullet •
    (font-lock-add-keywords
     nil
     '(("^ *\\(-\\) "
        (1 (prog1 () (compose-region (match-beginning 1)
                                     (match-end 1)
                                     "	•")))))
     'append)

    (font-lock-fontify-buffer))

  (add-hook 'org-mode-hook #'flow-org-style-bullets)

  (defvar flow-org-indent-min-level 4
    "Suppress org-indent indentation for levels below this.")

  (defun flow-org-indent--clear-shallow-prefixes (&rest _)
    "Remap indent prefixes so indentation starts fresh at `flow-org-indent-min-level'."
    (let ((empty (org-add-props "" nil 'face 'org-indent))
          (min flow-org-indent-min-level)
          (depth (length org-indent--text-line-prefixes)))
      ;; Remap FIRST while low indices still have their original values
      (cl-loop for n from min below depth do
               (let ((remapped (- n min -1)))
                 ;; Text body gets +1 so it sits inside its heading, not level with it
                 (aset org-indent--text-line-prefixes n
                       (aref org-indent--text-line-prefixes
                             (min (1+ remapped) (1- depth))))
                 (aset org-indent--heading-line-prefixes n
                       (aref org-indent--heading-line-prefixes remapped))
                 (aset org-indent--inlinetask-line-prefixes n
                       (aref org-indent--inlinetask-line-prefixes remapped))))
      ;; THEN zero out the shallow levels
      (dotimes (n min)
        (aset org-indent--heading-line-prefixes n empty)
        (aset org-indent--inlinetask-line-prefixes n empty)
        (aset org-indent--text-line-prefixes n empty))))

  (advice-add 'org-indent--compute-prefixes :after
              #'flow-org-indent--clear-shallow-prefixes)

  ;; Useful if we want bullets for all headings
  ;; (defun flow-org-bullets ()
  ;;   (font-lock-add-keywords
  ;;    nil
  ;;    '(("^\\(\\*+\\)"
  ;;       (1 (prog1 nil
  ;;            (let* ((beg (match-beginning 1))
  ;;                   (end (match-end 1))
  ;;                   (n   (- end beg)))
  ;;              (compose-region (1- end) end "◇")
  ;;              (when (> n 1)
  ;;                (put-text-property beg (1- end)
  ;;                                   'face 'org-hide)))))))
  ;;    'append))

  ;; (add-hook 'org-mode-hook #'flow-org-bullets)

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
                   '(:foreground ,cyan :weight bold)))
           " [%*]"
           mode-line-format-right-align
           (:eval (when (and (boundp 'vc-mode) vc-mode) ;; git branch + space
                    (concat (propertize " | " 'face '(:foreground ,bg++))
                            (replace-regexp-in-string "^ Git[-:]" "" vc-mode))))
           (:eval (propertize " | " 'face '(:foreground ,bg++)))
           "%l:%c" ;; line/col count
           (:eval (propertize " | " 'face '(:foreground ,bg++)))
           (:eval (when-let ((proc (get-buffer-process (current-buffer))))
                    (propertize (format "[%s] " (process-name proc))
                                'face '(:foreground ,bright-magenta :weight bold))))
           (:eval (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
           " ")))

    ;; Avoid resetting local hidden modelines in org when switching light themes.
    ;; (setq mode-line-format flow/mode-line)

    (setq-default mode-line-format flow/mode-line)))

(provide-theme 'flow)

;; (font-family-list)
;; (face-at-point)
;; (list-faces-display)
;; (progn (disable-theme 'flow) (load-theme 'flow t))
