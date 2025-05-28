;;; init.el -- Development Configuration -*- lexical-binding: t -*-

(let ((minver "30.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Appearance
(defconst *default-frame-width* 140)
(defconst *default-frame-height* 50)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when window-system (scroll-bar-mode -1)
      (set-frame-size (selected-frame) *default-frame-width* *default-frame-height*))
(setq inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

(let ((font (seq-find #'x-list-fonts '("Rec Mono Linear" "Monaco" "Monospace")))
      (writing-font (seq-find #'x-list-fonts '("Rec Mono Casual" "Sans Serif"))))
  (when font  ; Only set if we found a font
    (set-face-attribute 'default nil :font font :height 160))
  (when writing-font
    (set-face-attribute 'variable-pitch nil :font writing-font :height 160)))

;;; Essential behavior
(setq custom-file (concat user-emacs-directory "custom.el")
      make-backup-files nil
      create-lockfiles nil
      isearch-wrap-pause 'no-ding
      split-width-threshold 180)
(defalias 'yes-or-no-p 'y-or-n-p)
(fido-vertical-mode 1)
(add-to-list 'completion-ignored-extensions ".git")
(winner-mode 1)
(show-paren-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(setq-default compilation-scroll-output 'first-error
	      compilation-window-height 15)

(cond
 ((eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'meta
        mac-control-modifier 'control))
 ((eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta)))

(when (executable-find "rg")
  (setq grep-program "rg")
  (setq grep-command "rg --color=never --no-heading --line-number --max-filesize=300K ")
  (setq grep-use-null-device nil)
  (setq grep-find-command "rg --color=never --no-heading --line-number --max-filesize=300K "))

(global-set-key (kbd "M-H") help-map)
(global-set-key (kbd "M-S") search-map)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'delete-other-windows)
(global-set-key (kbd "M-;") 'winner-undo)
(global-set-key (kbd "M-'") 'winner-redo)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c n") (lambda () (interactive) (find-file (concat user-emacs-directory "/notes/index.org"))))
(global-set-key (kbd "C-c P") (lambda () (interactive) (find-file "~/src")))
(global-set-key (kbd "C-c p") 'project-find-file)
(global-set-key (kbd "M-F") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m") 'recompile)
(global-set-key (kbd "C-c M") 'project-compile)

(defun kill-region-or-backward-word ()
  "Kill region if active, otherwise kill backward word."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (cond
     ((and (bound-and-true-p paredit-mode)
           (fboundp 'paredit-backward-kill-word))
      (paredit-backward-kill-word))
     (t (backward-kill-word 1)))))

(global-set-key (kbd "C-w") #'kill-region-or-backward-word)

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w") 'icomplete-fido-backward-updir))

(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
		   (display-line-numbers-mode 1)
		   (display-fill-column-indicator-mode 1)
		   (column-number-mode 1)
		   (hl-line-mode 1))))

(setq-default modus-themes-common-palette-overrides
	      '((comment fg-dim)
		(doc-markup fg-alt)
		(border-mode-line-inactive bg-inactive)
		(bg-line-number-inactive bg-main)
		(fg-line-number-inactive fg-dim)
		(fringe bg-main)
		(red red-faint)
		(err blue)))

(load-theme 'modus-vivendi-tinted t)

;;; Package setup
(defun reset-packages ()
  "When upgrading your Emacs, it's useful to kill 3rd party deps and start again."
  (interactive)
  (delete-directory package-user-dir t))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package which-key :diminish :config (which-key-mode 1))
(use-package magit :bind ("C-c g" . magit-status))
(use-package olivetti)
(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  (diminish 'abbrev-mode))

(use-package popper
  :bind (("C-z"   . popper-toggle)
	 ("M-`"   . popper-cycle))
  :config
  (setq-default popper-reference-buffers
		'("\\*eshell\\*"
		  "\\*shell\\*"
		  "\\*compilation\\*"
		  "\\*Warnings\\*")
		popper-window-height 25
		popper-group-function 'popper-group-by-project
		popper-display-control 'display-buffer-in-direction		)
  (popper-mode +1))

(use-package flymake
  :ensure nil  ; Built-in
  :hook (prog-mode . flymake-mode)
  :bind (("C-c l" . flymake-show-buffer-diagnostics)
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 2))

;; major-modes/filetype packages
(use-package emacs-lisp-mode
  :ensure nil ;; included in emacs, not available externally
  :bind (:map emacs-lisp-mode-map
	      ("C-c b" . eval-buffer)))
(use-package go-mode
  :bind (("C-c t" . go-test-current-file)
	 ("C-c T" . go-test-current-project)))
(use-package ruby-mode)
(use-package gotest
  :config (with-eval-after-load 'compile ;; also applies to `make test` in compile mode.
	    (add-to-list 'compilation-error-regexp-alist-alist
			 '(go-test-trace
			   "Error Trace:[[:space:]]*\\([^:[:space:]]+\\.go\\):\\([0-9]+\\)" 1 2))
	    (add-to-list 'compilation-error-regexp-alist 'go-test-trace)))
(use-package clojure-mode
  :hook (clojure-mode . subword-mode)
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)))
(use-package cider :hook ((clojure-mode . cider-mode)
			  (cider-repl-mode . subword-mode))
  :config (setq clojure-indent-style 'align-arguments
		cider-repl-use-pretty-printing t
		cider-repl-display-help-banner nil
		nrepl-log-messages t))
(use-package groovy-mode :mode "\\.gradle\\'")
(use-package gradle-mode :hook ((java-mode . gradle-mode)
				(groovy-mode . gradle-mode)))
(use-package json-mode :mode ("\\.json\\'" . json-mode))
(use-package yaml-mode)
(use-package markdown-mode :mode ("\\.md\\'" . markdown-mode))
(use-package org
  :hook (org-mode . (lambda ()
		      (variable-pitch-mode 1)
		      (olivetti-mode 1)
		      (visual-line-mode 1)))
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-agenda-files `(,(concat user-emacs-directory "org"))
	olivetti-body-width 80)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit fixed-pitch))))
   '(org-table ((t (:inherit fixed-pitch)))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t))))
(use-package treesit-auto :config
  (let ((langs '(go gomod javascript html)))
    (dolist (lang langs)
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang))))
  (global-treesit-auto-mode))
(use-package templ-ts-mode)
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.tmpl\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2))
(use-package js2-mode :mode "\\.js\\'")
(use-package typescript-mode :mode "\\.ts\\'")

;; major-mode dependent packages
(use-package eglot
  :ensure nil
  :hook ((go-mode . eglot-ensure)
	 (clojure-mode . eglot-ensure)
	 (java-mode . eglot-ensure)
	 (templ-ts-mode . eglot-ensure))
  :bind ("C-c f" . (lambda ()
                     (interactive)
		     (ignore-errors
                       (eglot-code-action-organize-imports (point-min) (point-max)))
                     (eglot-format-buffer)
                     (save-buffer)))
  :config
  (add-to-list 'eglot-server-programs '(java-mode . ("jdtls")))
  (setq-default eglot-autoshutdown t
                eglot-confirm-server-initiated-edits nil))

(use-package paredit
  :diminish
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)
	 (cider-repl-mode . paredit-mode)
	 (lisp-data-mode . paredit-mode))
  :bind (:map paredit-mode-map
	      ("M-s" . nil)
	      ("M-k" . paredit-forward-barf-sexp)
	      ("M-l" . paredit-forward-slurp-sexp)))

(use-package aggressive-indent
  :diminish
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'org-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'shell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'go-mode))

(use-package deadgrep)

(provide 'init)
