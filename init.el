;;; init.el -- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Designed to work as my main programming environment.

;;; Code:

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Consts
(defconst *fixed-font* (seq-find #'x-list-fonts '("Rec Mono Linear" "Monaco" "Monospace")))
(defconst *variable-font* (seq-find #'x-list-fonts '("Rec Mono Casual" "Novaletra Serif CF" "Sans Serif")))
(defconst *font-size* 130)
(defconst *frame-width* 140)
(defconst *frame-height* 45)

;; Functions
(defmacro ifn (fn) "Execute FN interactively." `(lambda () (interactive) ,fn))

(defun format-buffer ()
  "Indent and clean up the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(defun is-fullscreen-p ()
  "Return non-nil if the current frame is in fullscreen state."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

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

;; Appearance
(when (fboundp 'pixel-scroll-precision-mode) ;; expect in Emacs 29.1
  (pixel-scroll-precision-mode 1))

(menu-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'fixed-pitch nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'variable-pitch nil :font *variable-font* :height *font-size* :weight 'regular)

(when window-system
    (scroll-bar-mode -1)
    (set-frame-size (selected-frame) 140 45))

(setq custom-file (concat user-emacs-directory "custom.el")
      inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore)

(let ((frame-dimensions '((width . 120) (height . 40))))
  (setq initial-frame-alist frame-dimensions)
  (setq default-frame-alist frame-dimensions))

(add-to-list 'default-frame-alist '(cursor-type . box))

;; Default Keybindings
(bind-key (kbd "M-o") 'other-window)
(bind-key (kbd "C-c i") (ifn (find-file user-init-file)))
(bind-key (kbd "C-c n") (ifn (find-file (concat user-emacs-directory "org/ops.org"))))
(bind-key (kbd "M-F") 'cycle-frame-size)

(global-set-key (kbd "C-c f") 'format-buffer)

;; Behaviour
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(winner-mode 1)
(fido-vertical-mode 1)
(add-to-list 'completion-ignored-extensions ".git")

(dolist (mode '(electric-pair-mode
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
  (flycheck-mode 1)
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook css-mode-hook)) (add-hook hook 'code-config))


(defun unix-werase-advice (orig-fun &rest args)
  "When ORIG-FUN/ARGS has no active region, delete a single word backwards instead."
  (if (and (called-interactively-p 'any) (not mark-active))
      (delete-region (save-excursion (backward-word 1) (point)) (point))
    (apply orig-fun args)))

;; Add the advice to kill-region
(advice-add 'kill-region :around #'unix-werase-advice)

;; Packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package emacs-lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
	      ("C-c b" . eval-buffer)))

;; LSP mode for language server protocol support
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
	 (clojure-mode . lsp-deferred))
  :config
  (setq-default lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting t)
  (setq-default lsp-prefer-flymake nil))

;; LSP UI enhancements
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t))

;; Company for autocompletion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;; Flycheck for linting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("C-c l" . flycheck-list-errors)))

(use-package go-mode
  :ensure t
  :after lsp-mode
  :hook (go-mode . lsp-deferred)
  :config
  (if (fboundp 'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-format-buffer nil t))
  (if (fboundp 'lsp-organize-imports)
      (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

;; For html/template files
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.tmpl\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '(("go" . "\\.\\(html\\|tmpl\\)\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package popper
  :ensure t
  :bind (("C-z"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*vterm\\*"
	  "\\*eshell\\*"
	  "\\*shell\\*"
	  "\\*compilation\\*"))
  (popper-mode +1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; For writing
(use-package org
  :ensure t
  :hook
  ;; Enable variable-pitch-mode for Org files (variable width font)
  (org-mode . variable-pitch-mode)
  :custom
  ;; Set default font faces for Org mode
  (org-hide-emphasis-markers t)
  :config
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

(use-package olivetti
  :ensure t
  :hook
  (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 80)
  (olivetti-minimum-body-width 60)
  (olivetti-style 'fancy))

;; Writeroom - distraction-free fullscreen writing
(use-package writeroom-mode
  :ensure t
  :bind (("C-c w" . writeroom-mode))
  :custom
  (writeroom-width 80)
  (writeroom-mode-line t)
  (writeroom-global-effects nil)) ; Disable some of the more aggressive effects

;; Focus - dims text outside current paragraph
(use-package focus
  :ensure t
  :bind (("C-c f" . focus-mode)))

(use-package writegood-mode
  :ensure t
  ;;:bind (("C-c g" . writegood-mode))
  )

;; Flyspell - spell checking
(use-package flyspell
  :ensure t
  :hook (org-mode . flyspell-mode)
  :init
  (setq ispell-program-name "aspell" ; You might need to adjust this based on your system
        ispell-extra-args '("--sug-mode=ultra")))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit)))

(provide 'init)
;;; init.el ends here
