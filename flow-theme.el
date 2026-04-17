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

(defvar flow--face-attr-symbols
  '(bold italic oblique normal ultra-bold semi-bold semi-light ultra-light
    thin unspecified inherit fixed-pitch)
  "Symbols used as face attribute values that need quoting.")

(defun flow--val->form (val)
  (cond
   ((and (consp val) (not (listp (cdr val)))) ; cons cell e.g. (-1 . 32)
    `(cons ,(car val) ,(cdr val)))
   ((and (listp val) (listp (car val)))        ; nested list e.g. :inherit (bold italic)
    `',val)
   ((and (listp val) (keywordp (car val)))     ; nested plist e.g. :box (...)
    `(list ,@(flow--plist->form val)))
   ((memq val flow--face-attr-symbols)         ; known attr symbol e.g. bold
    `',val)
   (t val)))                                   ; variable e.g. bg, fg, cyan

(defun flow--plist->form (attrs)
  "Convert a face attr plist into a (list ...) form,
recursively handling nested plists like :box and quoted symbols like bold."
  (let (result)
    (while attrs
      (push (pop attrs) result)
      (push (flow--val->form (pop attrs)) result))
    (nreverse result)))

(defmacro flow-set-faces (theme &rest specs)
  `(custom-theme-set-faces
    ',theme
    ,@(mapcar (lambda (spec)
                `(list ',(car spec)
                       (list (list t (list ,@(flow--plist->form (cdr spec)))))))
              specs)))

(let* ((dark-p (eq flow-mode 'dark))
       ;; Color organised as a pyramid for attention, with small areas
       ;; of interest down to the main bulk of the view. This should
       ;; also dictate color choice in UI i.e you don't want to draw
       ;; the eye with a yellow for truncation symbols in the fringes.

       ;; Top layer, high interest - buffer names, errors/warnings,
       ;; prompts, cursor. Do we need all 4? Especially as magenta is
       ;; close to purple/lavender.
       (yellow         (if dark-p "#f9e2af" "#df8e1d")) ; catppucin mocha yellow
       (magenta        (if dark-p "#cba6f7" "#8839ef")) ; purple
       (cyan           (if dark-p "#89dceb" "#04a5e5")) ; muted cyan
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
       (fg++++         (if dark-p "#585b70" "#8c8fa1"))

       (bg             (if dark-p "#0a0a10" "#eff1f5"))
       ;; +colors are darker/lighter depending on theme for consistent depth effect.
       (bg+            (if dark-p "#11111b" "#dce0e8"))
       (bg++            (if dark-p "#181825" "#e6e9ef"))
       (bg+++          (if dark-p "#1e1e2e" "#eff1f5"))
       (bg++++         (if dark-p "#313244" "#ccd0da"))
       (bg+green       (if dark-p "#0c1412" "#d1fde6")) ;; for git diffs
       (bg+green+       (if dark-p "#0e211c" "#bffbdc"))
       (bg+red         (if dark-p "#1f0809" "#f4d3d5"))
       (bg+red+         (if dark-p "#350f11" "#ecb9bc"))

       ;; rest/unsorted - to be deprecated most likely.
       ;; not sure black etc really works here? for dual theme
       (red            (if dark-p "#eba0ac" "#e64553")) ; catppucin mocha maroon
       (green          (if dark-p "#a6e3a1" "#40a02b")) ; muted green
       ;; may want a more faded yellow
       (blue           (if dark-p "#89b4fa" "#1e66f5")) ; catppucin mocha sapphire


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

  (flow-set-faces
   flow
   (default :background bg :foreground fg)
   (cursor :background bright-magenta :foreground bg)
   (region :background bg++)
   (fringe :background bg :foreground bg+++)
   (minibuffer-prompt :foreground teal)
   (hl-line :background bg)

   ;; Mode Line
   (mode-line :background bg :foreground fg
              :box (:line-width 1 :color bg++++ :style nil))
   (mode-line-inactive :background bg :foreground fg
                       :box (:line-width 1 :color bg++++ :style nil))
   (mode-line-buffer-id :foreground cyan :weight bold)
   (mode-line-emphasis :foreground teal)
   (mode-line-highlight :foreground teal)

   ;; Misc. UI
   (vertical-border :foreground bg++++)
   (link :foreground lavender :underline t)
   (escape-glyph :foreground bg+)
   (icon :foreground yellow)
   (xref-line-number)

   ;; ?? status section?
   (success :foreground green)
   (info :foreground lavender)
   (match :background bg :foreground match)
   (isearch :background match :foreground bg)
   (lazy-highlight :background match :foreground bg)
   (query-replace :background green :foreground bg)

   ;; Dired
   (dired-directory :foreground teal)
   (dired-executable :foreground sapphire)
   (dired-symlink :foreground lavender)
   (dired-marked :foreground yellow :weight bold)

   ;; Compilation
   (compilation-info :foreground teal)
   (compilation-warning :foreground yellow)
   (compilation-error :foreground magenta)

   ;; Grep
   (grep-hit-face :foreground magenta)
   (grep-match-face :background match :foreground bg)

   ;; Git / Version Control
   (diff-added :foreground fg :background bg+green)
   (diff-refine-added :background bg+green+)
   (diff-removed :background bg+red)
   (diff-refine-removed :background bg+red+)
   (diff-changed :foreground yellow)
   (diff-header :foreground sapphire)
   (diff-file-header :foreground cyan :weight bold)

   ;; Completions
   (completions-highlight :foreground fg :background bg+++)
   (icomplete-selected-match :foreground fg :background bg+++)
   (completions-annotations :foreground teal)
   (completions-common-part :foreground fg++)
   (completions-first-difference :foreground fg+++)

   ;; Syntax highlighting
   (font-lock-comment-face :foreground fg+++)
   (font-lock-string-face :foreground teal)
   (font-lock-keyword-face :foreground fg)
   (font-lock-function-name-face :foreground sapphire)
   (font-lock-variable-name-face :foreground lavender)
   (font-lock-type-face :foreground fg)
   (font-lock-constant-face :foreground sapphire)
   (font-lock-builtin-face :foreground fg++)

   ;; Errors and Warnings
   (error :foreground magenta)
   (warning :foreground yellow)

   ;; Line Numbers
   (line-number :foreground fg+++)
   (line-number-current-line :foreground fg++ :background bg+++)

   (fill-column-indicator :foreground bg) ;; what does this do?

   ;; Document Headings
   (markdown-header-face-1 :height 1.6 :weight bold :foreground fg
                           :box (:line-width (-1 . 32) :color bg))
   (markdown-header-face-2 :height 1.3 :slant italic :foreground fg
                           :box (:line-width (-1 . 20) :color bg))
   (markdown-header-face-3 :height 1.0 :weight bold :foreground fg
                           :box (:line-width (-1 . 12) :color bg))
   (markdown-header-face-4 :height 1.0 :weight bold :foreground fg)
   (org-level-1 :height 1.6 :weight bold :foreground fg
                :box (:line-width (-1 . 32) :color bg))
   (org-level-2 :height 1.3 :slant italic :foreground fg
                :box (:line-width (-1 . 20) :color bg))
   (org-level-3 :height 1.0 :weight bold :foreground fg
                :box (:line-width (-1 . 12) :color bg))
   ;; bullets style after this point
   (org-level-4 :height 1.0 :foreground fg)
   (org-level-5 :height 1.0 :foreground fg)
   (org-level-6 :height 1.0 :foreground fg)

   ;; Org Styling
   (org-ellipsis :foreground fg+++ :underline nil)
   (org-headline-done :foreground fg+++ :underline nil :strike-through t)
   (org-done :foreground green :underline nil)
   (org-todo :foreground sapphire :underline nil)
   (org-tag :foreground fg+++ :underline nil)
   (org-agenda-structure :foreground lavender)
   (org-scheduled-previously :foreground yellow)
   (org-hide :foreground bg :inherit fixed-pitch)
   (org-block :inherit fixed-pitch)
   (org-block-begin-line :inherit fixed-pitch)
   (org-block-end-line :inherit fixed-pitch)
   (org-code :inherit fixed-pitch)
   (org-verbatim :inherit fixed-pitch)
   (org-checkbox :inherit fixed-pitch)
   (org-meta-line :inherit fixed-pitch)
   (org-drawer :inherit fixed-pitch)

   (eshell-prompt :inherit yellow)

   (sh-quoted-exec :foreground fg)

   (ansi-color-black :foreground bg :background fg)
   (ansi-color-red :foreground red :background bg)
   (ansi-color-green :foreground green :background bg)
   (ansi-color-yellow :foreground yellow :background bg)
   (ansi-color-blue :foreground blue :background bg)
   (ansi-color-magenta :foreground magenta :background bg)

   (font-lock-paren-face :foreground lavender)
   (font-lock-bracket-face :foreground sapphire)
   (font-lock-brace-face :foreground teal))

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

  (defvar flow/vc-commit-age-cache (make-hash-table :test 'equal))

  (defun flow/vc-commit-age-refresh ()
    (when-let* ((root (and buffer-file-name (vc-root-dir)))
                (root (expand-file-name root)))
      (unless (gethash root flow/vc-commit-age-cache)
        (puthash root 'pending flow/vc-commit-age-cache)
        (make-process
         :name "flow/vc-commit-age"
         :command (list "git" "--no-pager" "-C" root "log" "-1" "--format=%ct")
         :noquery t
         :filter (lambda (_ output)
                   (let* ((ts (string-to-number (string-trim output)))
                          (secs (- (float-time) ts))
                          (age (when (> ts 0)
                                 (cond ((< secs 3600)  (format "%dm" (/ secs 60)))
                                       ((< secs 86400) (format "%dh" (/ secs 3600)))
                                       (t              (format "%dd" (/ secs 86400)))))))
                     (puthash root (or age 'error) flow/vc-commit-age-cache)
                     (force-mode-line-update t)))))))

  (defun flow/vc-commit-age ()
    (when-let* ((root (and buffer-file-name (vc-root-dir)))
                (val (gethash (expand-file-name root) flow/vc-commit-age-cache)))
      (unless (memq val '(pending error)) val)))

  (run-with-idle-timer 0.5 t #'flow/vc-commit-age-refresh)
  (run-with-timer 0 60 (lambda () (clrhash flow/vc-commit-age-cache)))

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
           (:eval (concat (if (and (boundp 'org-clocking-p) (org-clocking-p))
                              (propertize (format " ● %s"
                                                  (org-duration-from-minutes (org-clock-get-clocked-time)))
                                          'face '(:foreground ,cyan :weight bold))
                            (propertize " ○" 'face '(:foreground ,cyan :weight bold)))
                          (propertize " | " 'face '(:foreground ,bg++++))))
           (:eval (when (and (boundp 'vc-mode) vc-mode)
                  (concat (replace-regexp-in-string "^ Git[-:]" "" vc-mode)
                          (when-let ((age (flow/vc-commit-age)))
                            (propertize (concat " (" age ")")
                                        'face '(:foreground ,fg++++)))
                          (propertize " | " 'face '(:foreground ,bg++++)))))
           "%l:%c"
           (:eval (propertize " | " 'face '(:foreground ,bg++++)))
           (:eval (when-let ((proc (get-buffer-process (current-buffer))))
                    (propertize (format "[%s] " (process-name proc))
                                'face '(:foreground ,bright-magenta :weight bold))))
           (:eval (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
           " ")))
    (setq-default mode-line-format flow/mode-line)))

(provide-theme 'flow)

;; (font-family-list)
;; (face-at-point)
;; (list-faces-display)
;; (progn (disable-theme 'flow) (load-theme 'flow t))
