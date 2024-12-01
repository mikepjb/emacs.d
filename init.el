;; -*- lexical-binding: t -*-

;; check for emacs 28+
;; do not early-init disable package loading, assuming this will always happen since it's been introduced since emacs 27 and even Debian has 28 LOL.

(let ((minver "28.2"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; only supported in Emacs 29
;;  (setq pixel-scroll-precision-large-scroll-height 40.0)

(eval-when-compile (require 'subr-x)) ;; string-join comes from here

(defconst *is-a-mac* (eq system-type 'darwin))

(when *is-a-mac* ;; for native compilation on mac os
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
      "/opt/homebrew/opt/node@16/bin")
    ":")))

(defconst *fixed-font*
  (cond ((x-list-fonts "Recursive Mono Linear Static") "Recursive Mono Linear Static")
	((x-list-fonts "Monaco") "Monaco") ;; defaul mac os
	((x-list-fonts "Menlo") "Menlo") ;; mac os with vertical line glyph
	((x-list-fonts "Monospace") "Monospace") ;; linux deja vu sans mono default
	(t nil)))

(defconst *variable-font*
  (cond ((x-list-fonts "Recursive") "Recursive")
	((x-list-fonts "Recursive Mono Casual Static") "Recursive Mono Casual Static")
	((x-list-fonts "Novaletra Serif CF") "Novaletra Serif CF")
	((x-list-fonts "Georgia") "Georgia")
	((x-list-fonts "Sans Serif") "Sans Serif")
	(t nil)))

(defconst *font-size*
  (if *is-a-mac* 120 110))

(set-face-attribute 'default nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'fixed-pitch nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'variable-pitch nil :font *variable-font* :height *font-size* :weight 'regular)

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(when window-system
    (scroll-bar-mode -1)
    (set-frame-size (selected-frame) 140 80))

(defalias 'yes-or-no-p 'y-or-n-p)

(defmacro set-mode (mode value)
  `(funcall ,mode (if (eq ,value :enable) 1 -1)))

(dolist (mode '(electric-pair-mode
		fido-mode
		show-paren-mode
		column-number-mode
		global-auto-revert-mode
		savehist-mode))
  (set-mode mode :enable))

(dolist (mode '(menu-bar-mode
		tool-bar-mode
		electric-indent-mode)) ;; enabling this, disables indent for C-j
  (set-mode mode :disable))


(defun code-config ()
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1)
  (hl-line-mode 1)
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook css-mode-hook)) (add-hook hook 'code-config))

;; (load-theme 'tango-dark t) ;; how to load this IF other theme can't be found?

(defmacro ifn (fn)
  `(lambda () (interactive) ,fn))

(defmacro ifn-from (from-dir fn)
  `(lambda () (interactive)
     (let ((default-directory ,from-dir)) (call-interactively ,fn))))

(dolist
    (binding
     `(
       ("M-o" . other-window)
       ("C-c i" . ,(ifn (find-file user-init-file)))
       ("C-c n" . ,(ifn (find-file (concat user-emacs-directory "org/notes.org"))))
       ("C-c o" . ,(ifn (find-file (concat user-emacs-directory "org/ops.org"))))
       ("C-c O" . ,(ifn-from "~/.emacs.d/org/" 'find-file))
       ("M-F" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(add-hook 'org-mode-hook (lambda ()
			   (variable-pitch-mode)
			   (org-indent-mode)
			   (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
(add-hook 'org-agenda-mode-hook 'variable-pitch-mode)

;; (use-package gruvbox :ensure t)
;; doom themes!
;; tomorrow

;; also checkout purcell config
;; lsp-java?

(require 'local nil t) ;; optionally load a local.el

;; 3rd party territory, everything up to this point should not fail without an internet connection.

(when (not (member "melpa" (mapcar 'car package-archives))) ;; check there isn't a local override
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/"))))

(when (not (require 'use-package nil t))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gruvbox-theme :ensure t
  :config (load-theme 'gruvbox-theme t))


;; TODO do not use custom in this file.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3" "0517759e6b71f4ad76d8d38b69c51a5c2f7196675d202e3c2507124980c3c2a3" "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378" default))
 '(package-selected-packages
   '(gruvbox-theme gruvbox use-package "use-package" "use-package" "use-package" "use-package")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
