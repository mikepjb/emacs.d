;;; init.el -- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Designed to work as my main programming environment.

;;; Code:

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Consts
(defconst *mac-host* (eq system-type 'darwin))
(defconst *linux-host* (eq system-type 'gnu/linux))
(defconst *android-host* (string-equal system-type "android"))
(defconst *fixed-font* (seq-find #'x-list-fonts '("Rec Mono Linear" "Monaco" "Monospace")))
(defconst *variable-font* (seq-find #'x-list-fonts '("Rec Mono Casual" "Novaletra Serif CF" "Sans Serif")))
(defconst *font-size* 160) ;; 16px is a reasonable minimum.
(defconst *frame-width* 140)
(defconst *frame-height* (if (>= (display-pixel-width) 3000) 80 50))

;; Functions:
(eval-when-compile (require 'subr-x)) ;; string-join comes from here

(defmacro ifn (fn) "Execute interactive FN." `(lambda () (interactive) ,fn))
(defmacro ifn-from (from-dir fn)
  "Execute interactive FN in FROM-DIR."
  `(lambda () (interactive)
     (let ((default-directory ,from-dir)) (call-interactively ,fn))))

(defun format-buffer ()
  "Indent and clean up the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(defun is-fullscreen-p ()
  "Return non-nil if the current frame is in fullscreen state."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun disable-modes-in-large-files ()
  "Avoid slowing down Emacs by disabling major-mode work in large files."
  (when (> (buffer-size) (* 1024 1024))
    (fundamental-mode)
    (display-line-numbers-mode -1)
    (font-lock-mode -1)))

(defun cycle-frame-size ()
  "Cycles between frame sizes: fullscreen -> default size -> fullscreen."
  (interactive)
  (cond
   ;; If currently fullscreen, return to default size
   ((is-fullscreen-p)
    (toggle-frame-fullscreen))

   ;; If not at default size, set to default size
   ((not (and (= (frame-width) *frame-width*)
              (= (frame-height) *frame-height*)))
    (set-frame-size (selected-frame) *frame-width* *frame-height*))

   ;; Otherwise (already at default size), go to fullscreen
   (t
    (toggle-frame-fullscreen))))

(defun toggle-frame-translucency ()
  "Toggle current frame translucency between fully opaque and slightly see-through."
  (interactive)
  (let ((level (if (eq (car (frame-parameter nil 'alpha)) 93) 100 93)))
    (set-frame-parameter nil 'alpha `(,level . ,level))))

;; C-w should act as kill backward work in fido-vertical-mode
(define-key minibuffer-local-map (kbd "C-w")
	    (lambda ()
	      (interactive)
	      (let ((input (minibuffer-contents)))
		(when (string-match-p "/" input)
		  (delete-minibuffer-contents)
		  (insert (file-name-directory (directory-file-name input)))))))

;; Environment
(when *mac-host*
  (setq mac-command-modifier 'meta)

  (setenv
   "LIBRARY_PATH"
   (string-join
    '("/opt/homebrew/opt/gcc/lib/gcc/12"
      "/opt/homebrew/opt/libgccjit/lib/gcc/12"
      "/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin22/12")
    ":"))

  (setenv
   "PATH"
   (string-join
    '("/opt/homebrew/bin" "/usr/local/bin" "/usr/local/sbin" "/bin" "/usr/bin"
      "/usr/sbin" "/opt/homebrew/opt/node@16/bin")
    ":"))

  (when *android-host*
    ;; Add Termux binaries to PATH environment
    (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
      (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
      (setq exec-path (append exec-path (list termuxpath))))

    ;; Fix SSL connection for F-Droid version of Emacs.
    (setq-default
     tls-program '("gnutls-cli -p %p %h"
		   "gnutls-cli -p %p %h --protocols ssl3")))

  (setq package-native-compile t)
  (setq native-comp-deferred-compilation t))

;; Appearance
(pixel-scroll-precision-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'fixed-pitch nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'variable-pitch nil :font *variable-font* :height *font-size* :weight 'regular)
(set-face-attribute 'tooltip nil :font *fixed-font* :height *font-size* :weight 'regular)
(setq use-system-tooltips nil)

(when window-system
  (scroll-bar-mode -1)
  (set-frame-size (selected-frame) *frame-width* *frame-height*))

(setq custom-file (concat user-emacs-directory "custom.el")
      inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore)

(let ((frame-dimensions '((width . 120) (height . 40))))
  (setq initial-frame-alist frame-dimensions)
  (setq default-frame-alist frame-dimensions))

(add-to-list 'default-frame-alist '(cursor-type . box))

;; Default Keybindings
(dolist
    (binding
     `(
       ;; vc-dir (show current changes in repo)
       ("M-o" . other-window)
       ("M-O" . delete-other-windows) ;; more ergonomic C-c 1
       ("M-;" . winner-undo)
       ("M-'" . winner-redo)
       ("C-c 0" . mx/toggle-frame-size)
       ("C-c a" . org-agenda-list)
       ("C-c A" . org-agenda)
       ("C-c i" . ,(ifn (find-file user-init-file)))
       ("C-c n" . ,(ifn (find-file (concat user-emacs-directory "org/notes.org"))))
       ("C-c o" . ,(ifn (find-file (concat user-emacs-directory "org/ops.org"))))
       ("C-c O" . ,(ifn-from "~/.emacs.d/org/" 'find-file))
       ("C-c m" . recompile)
       ;; ("C-c M" . ,(ifn-from (vc-git-root buffer-file-name) 'compile)) ;; compile from git root
       ("C-c p" . project-find-file)
       ;; ("C-c g" . ,(ifn (vc-dir (vc-git-root buffer-file-name))))
       ("C-c L" . vc-print-root-log) ;; git log
       ("C-c l" . flycheck-list-errors)
       ("C-c s" . vc-git-grep) ;; grep over current project
       ("C-c '" . modus-themes-toggle) ;; toggle light/dark
       ;; ("s-d" . duplicate-line) ;; think this is a recent function, Emacs 29.1
       ("C-;" . completion-at-point) ;; works just fine? company is nicer..
       ("M-D" . ,(ifn (progn (end-of-line 1)
			     (open-line 1)
			     (forward-line 1)
			     (copy-from-above-command))))
       ("s-x" . execute-extended-command)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("C-h" . delete-backward-char)
       ("C-z" . mikepjb/toggle-repl)
       ("C-c r" . recentf-open-files)
       ("M-j" . ,(ifn (join-line -1)))
       ("M-H" . ,help-map)
       ("M-s" . save-buffer) ;; C-x C-s is too awkward for a common keybind.
       ("M-S" . ,search-map)
       ("M-/" . comment-or-uncomment-region)
       ("s-k" . kill-buffer) ;; actually originally matched to kill-current-buffer, maybe try that out too.
       ("C-c P" . ,(ifn-from "~/src/" 'find-file))
       ("M-J" . mx/toggle-frame-translucency)
       ("M-F" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))


(global-set-key (kbd "C-c f") 'format-buffer)

;; Behaviour:
(setq create-lockfiles nil)
(setq split-width-threshold 180)
(setq split-height-threshold 160)
(setq make-backup-files nil)
(setq isearch-wrap-pause 'no-ding)
(defalias 'yes-or-no-p 'y-or-n-p)
(winner-mode 1)
(fido-vertical-mode 1)
(add-to-list 'completion-ignored-extensions ".git")
(add-hook 'find-file-hook #'disable-modes-in-large-files)


(dolist (mode '(;; electric-pair-mode ;; conflicts in web-mode.. and others?
		show-paren-mode
		save-place-mode
		column-number-mode
		savehist-mode))
  (funcall mode 1))

(dolist (mode '(menu-bar-mode
		tool-bar-mode
		electric-indent-mode)) ;; enabling this, disables indent for C-j
  (funcall mode -1))

(defun code-config ()
  "Default configuration for code editing buffers."
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1)
  (hl-line-mode 1)
  ;; (flycheck-mode 1)
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook css-mode-hook)) (add-hook hook 'code-config))

(defun prose-config ()
  "Default configuration for writing buffers."
  (variable-pitch-mode 1)
  (writegood-mode 1)
  (olivetti-mode 1)
  (visual-line-mode 1)
  (flyspell-mode 1))

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

;; Packages:
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package emacs-lisp-mode
  :ensure nil ;; included in emacs, not available externally
  :bind (:map emacs-lisp-mode-map
	      ("C-c b" . eval-buffer)))

(use-package diminish)

;; LSP mode for language server protocol support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
	 (clojure-mode . lsp-deferred))
  :config
  (setq-default lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting t)
  (setq-default lsp-prefer-flymake nil))

;; Clojure and CIDER configuration using use-package
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :config
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :defer t
  :diminish cider-mode
  :hook ((clojure-mode . cider-mode)
         (cider-repl-mode . subword-mode)
         (cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-show-error-buffer t)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq nrepl-log-messages t)
  ;; Indentation for Clojure
  (setq clojure-indent-style 'align-arguments))

;; Optional - some useful supporting packages for Clojure development
(use-package paredit
  :ensure t
  :diminish paredit-mode)

(use-package rainbow-delimiters
  :ensure t)

;; Flycheck for linting
(use-package flycheck
  :config (global-flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("C-c l" . flycheck-list-errors)))

;; LSP UI enhancements
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t))

;; Company for autocompletion
(use-package company
  :hook (prog-mode . company-mode)
  :after diminish
  :config
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-c C-l") 'company-show-location)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (diminish 'company-mode))

(use-package go-mode
  :after lsp-mode
  :hook (go-mode . lsp-deferred)
  :config
  (if (fboundp 'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-format-buffer nil t))
  (if (fboundp 'lsp-organize-imports)
      (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

;; For html/template files
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.tmpl\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '(("go" . "\\.\\(html\\|tmpl\\)\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package popper
  :bind (("C-z"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*vterm\\*"
	  "\\*eshell\\*"
	  "\\*shell\\*"
	  "\\*compilation\\*"))
  (popper-mode +1))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package olivetti
  :custom
  (olivetti-body-width 80)
  (olivetti-minimum-body-width 60)
  (olivetti-style 'fancy))

;; For writing
(use-package org
  :after olivetti
  :hook
  (org-mode . prose-config)
  (org-mode . org-indent-mode)
  :custom
  ;; Set default font faces for Org mode
  (org-hide-emphasis-markers t)
  :config
  (setq-default
   org-agenda-files `(,(concat user-emacs-directory "org"))
   org-archive-location (concat user-emacs-directory "org/archive.org::")
   org-agenda-start-on-weekday nil ;; show the next 7 days
   org-agenda-start-day "0d"
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)" "COMPLETE(c)" "CANCELLED(x)")))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Keep some elements fixed-width for better alignment
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-checkbox ((t (:inherit fixed-pitch)))))
  ;; Set nicer bullets for headlines
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; Focus - dims text outside current paragraph
(use-package focus
  :bind (("C-c f" . focus-mode)))

(use-package writegood-mode)

(use-package flyspell
  :hook (org-mode . flyspell-mode)
  :init
  (if *mac-host*
      (setq ispell-program-name "hunspell"
	    ispell-local-dictionary "en_GB"
            ispell-local-dictionary-alist '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra"))))

(use-package magit
  :bind (("C-c g" . magit)))

(use-package js2-mode)
(use-package json-mode)

(use-package markdown-mode
  :hook (markdown-mode . prose-config)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-indent-code t)
  (setq markdown-enable-math t))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'org-mode)
  (setq aggressive-indent-sit-on-edit-timer 0.5)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'aggressive-indent-dont-indent-if
                 '(and (lsp-feature? "textDocument/formatting")
                       (eq (symbol-function 'indent-region-function)
                           'lsp-format-region)))))

(use-package templ-ts-mode)

;;requires cmake to build an external module
(use-package vterm
  :defer t)

(provide 'init)
;;; init.el ends here
