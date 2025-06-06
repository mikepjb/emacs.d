(menu-bar-mode -1)
(tool-bar-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (fringe-mode -1))

;; still want to open vc-dir in another (new) window if there is only one.
;; considering citre for managing tags and setting up xref for M-./M-, jumping.

(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux
   (setenv "SSH_ASKPASS" "/usr/lib/seahorse/ssh-askpass") ;; for vc
   (setq x-super-keysym 'meta)))

(setq inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore
      custom-file (concat user-emacs-directory "custom.el")
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      isearch-wrap-pause 'no-ding
      use-short-answers t
      truncate-lines t ;; no word wrap thanks
      vc-follow-symlinks t
      find-file-visit-truename t
      split-height-threshold 80
      split-width-threshold 160
      log-edit-show-diff t
      log-edit-show-files nil
      org-src-preserve-indentation t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-window-setup 'current-window
      org-export-with-section-numbers nil)

(setq-default compilation-scroll-output 'first-error
	      compilation-window-height 15
	      display-fill-column-indicator-column 80
	      truncate-lines t)

(when (file-exists-p custom-file) (load custom-file))
(let ((local-config (concat user-emacs-directory "local.el")))
  (when (file-exists-p local-config) (load local-config t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (base-mode '(fido-vertical-mode
		     show-paren-mode
		     save-place-mode
		     electric-pair-mode
		     savehist-mode))
  (funcall base-mode 1))

(when (executable-find "rg")
  (setq grep-program "rg"
	grep-use-null-device nil
	grep-command
	"rg --color=never --no-heading --line-number --max-filesize=300K "))

(defun kill-region-or-backward-word ()
  "Kill region if active, otherwise kill backward word."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (cond ((and (bound-and-true-p paredit-mode) (fboundp 'paredit-backward-kill-word))
           (paredit-backward-kill-word))
          (t (backward-kill-word 1)))))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w") 'icomplete-fido-backward-updir))

(setq inferior-lisp-program "scheme")
(setq inferior-lisp-prompt "^[0-9]* *\\]=> *")

(defun repl ()
  (interactive)
  (other-window-prefix)
  (pcase major-mode
    ('clojure-mode (inferior-lisp "clojure"))
    ('emacs-lisp-mode (ielm))
    ('scheme-mode (inferior-lisp "scheme"))
    ('sql-mode (sql-connect))
    (_ (message "No REPL defined for %s" major-mode))))

(defmacro ff (path)
  `(lambda ()
    (interactive)
    (find-file ,path)))

(dolist (binding `(("M-o" other-window)
		   ("M-O" delete-other-window)
		   ("C-w" kill-region-or-backward-word) ("M-K" kill-whole-line)
		   ("M-D" duplicate-line)
		   ("C-;" hippie-expand)
		   ("M-j" (lambda () (interactive) (join-line -1)))
		   ("M-F" toggle-frame-fullscreen)
		   ("M-R" repl)
		   ("C-j" newline) ;; because electric-indent overrides this
		   ("M-P" project-find-file)
		   ("M-C" org-agenda) ;; a.k.a checklist
		   ("C-c d" sql-connect)
		   ("C-c p" project-find-file)
		   ("C-c g" vc-dir-root)
		   ("C-c h" vc-region-history) ;; + file history without region
		   ("C-c a" vc-annotate) ;; a.k.a git blame
		   ("C-h" delete-backward-char)
		   ("M-s" save-buffer)
		   ("M-/" comment-line)
		   ("C-c i" ,(ff user-init-file))
		   ("C-c n" ,(ff (concat user-emacs-directory "notes/index.org")))
		   ("C-c l" ,(ff (concat user-emacs-directory "local.el")))
		   ("C-c P" ,(ff "~/src"))
		   ("C-c m" recompile) ("C-c M" project-compile)))
  (global-set-key (kbd (car binding)) (cadr binding)))

(global-set-key (kbd "M-H") help-map)
(global-set-key (kbd "M-S") search-map)

(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
		   (display-line-numbers-mode 1)
		   (display-fill-column-indicator-mode 1)
		   (column-number-mode 1)
		   (hl-line-mode 1))))

(defun should-center-buffer-p ()
  (memq major-mode '(org-mode markdown-mode)))

(defun center-prose-buffer-margins ()
  "Apply smart margins based on window width and buffer type."
  (set-window-margins nil 0 0)
  
  (when (should-center-buffer-p)
    (let* ((char-width-pix (frame-char-width))
           (window-width-pix (window-body-width nil t))  ; t = pixels!
           (target-width-chars 80)
           (target-width-pix (* target-width-chars char-width-pix))
           (margin-total-pix (max 0 (- window-width-pix target-width-pix)))
           (margin-each-pix (/ margin-total-pix 2.0))
           (margin-chars (max 0 (round (/ margin-each-pix char-width-pix)))))
      (set-window-margins nil margin-chars margin-chars))))

(add-hook 'buffer-list-update-hook 'center-prose-buffer-margins)

(defun prose-config ()
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (center-prose-buffer-margins)
  (add-hook 'window-size-change-functions
            (lambda (frame) (center-prose-buffer-margins)) nil t))

(add-hook 'org-mode-hook (lambda ()
			   (prose-config)
			   (org-indent-mode)))

(custom-set-faces
 '(org-level-1 ((t (:height 1.3 :weight bold))))
 '(org-level-2 ((t (:height 1.2 :weight bold))))
 '(org-level-3 ((t (:height 1.1 :weight bold))))
 '(org-level-4 ((t (:height 1.0 :weight bold)))))

(cl-flet ((find-font (names) (seq-find #'x-list-fonts names)))
  (let ((font (find-font '("Rec Mono Linear" "Monaco" "Monospace")))
        (writing-font (find-font '("Rec Mono Casual" "Sans Serif"))))
    (when font
      (set-face-attribute 'default nil :font font :height 160)
      (set-face-attribute 'org-block nil :font font)
      (set-face-attribute 'org-code nil :font font)
      (set-face-attribute 'org-verbatim nil :font font)
      (set-face-attribute 'org-table nil :font font))
    (when writing-font
      (set-face-attribute 'variable-pitch nil :font writing-font :height 160))))

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "R") 'vc-revert))

(add-hook 'vc-dir-mode-hook 
          (lambda () 
            (vc-dir-hide-up-to-date)))

(defun my/format-buffer-on-save ()
  "Format current buffer based on file type."
  (when buffer-file-name
    (cond
     ;; Go files with goimports
     ((and (string-suffix-p ".go" buffer-file-name)
           (executable-find "goimports"))
      (let ((original-point (point)))
        (shell-command-on-region 
         (point-min) (point-max) 
         "goimports" 
         nil t)  ; replace buffer contents
        (goto-char original-point)))
     
     ;; Templ files with templ fmt
     ((and (string-suffix-p ".templ" buffer-file-name)
           (executable-find "templ"))
      (save-buffer)  ; templ fmt works on files, not stdin
      (shell-command (format "templ fmt %s" (shell-quote-argument buffer-file-name)))
      (revert-buffer nil t nil)))))

(add-hook 'before-save-hook #'my/format-buffer-on-save)

(setq-default modus-themes-common-palette-overrides
              '((comment fg-dim)
                (doc-markup fg-alt)
                (border-mode-line-inactive bg-inactive)
                (bg-line-number-inactive bg-main)
                (fg-line-number-inactive fg-dim)
                (fringe bg-main)
                (red red-faint)
                (err blue)
                (string fg-alt)           ; Strings less prominent
                (keyword fg-main)         ; Keywords same as normal text
                (builtin fg-main)         ; Built-ins quiet
                (constant fg-main)        ; Constants quiet  
                (type fg-main)            ; Types quiet
                (variable fg-main)        ; Variables quiet
                (function fg-main)        ; Functions quiet
                (bg-mode-line-active bg-dim) ; Subtle mode line
                (fg-mode-line-active fg-main)
                (bg-region bg-dim)        ; Subtle selection
                (fg-region unspecified)   ; No special selection color
                (yellow yellow-faint)     ; Warnings less bright
                (green green-faint)       ; Success less bright
                (blue blue-faint)         ; Info less bright
                (magenta magenta-faint))) ; Less bright overall

(load-theme 'modus-vivendi-tinted t)
  
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package clojure-mode :hook (clojure-mode . subword-mode))
(use-package ruby-mode)
(use-package go-mode)
(use-package json-mode)
(use-package yaml-mode)
(use-package markdown-mode
  :hook (markdown-mode . prose-config)
  :custom-face
  (markdown-header-face-1 ((t (:height 1.3 :weight bold))))
  (markdown-header-face-2 ((t (:height 1.2 :weight bold))))
  (markdown-header-face-3 ((t (:height 1.1 :weight bold))))
  (markdown-header-face-4 ((t (:height 1.0 :weight bold)))))
(use-package js2-mode)
(use-package paredit
  :hook ((clojure-mode
	  emacs-lisp-mode
	  cider-repl-mode
	  lisp-data-mode)
	 . paredit-mode)
  :config
  (with-eval-after-load 'paredit
    (define-key paredit-mode-map (kbd "M-s") nil)
    (define-key paredit-mode-map (kbd "M-k") 'paredit-forward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-l") 'paredit-forward-slurp-sexp)))
