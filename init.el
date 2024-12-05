;;; init.el -- Summary
;;; Commentary:
;;;
;;; This setup prefers using Recursive as both a sans-serif and mono font, you can get it here https://recursive.design.
;;; In the top right you can download a zip containing a lot of fonts! Install the static linear + casual versions.
;;; Specifically under ~Recursive_Code -> RecMono{Casual,Linear} -> RecMono{Casual,Linear}-Regular-1.085.ttf
;;; Also on Mac OS, you should disable the keyboard shortcut under "Input Sources" labelled "Select the previous input source" as it conflicts with C-SPC (set-mark-command)
;;; Under Gnome, disable "Show the overview" shortcut under "System" (originally bound to s-s)

;; -*- lexical-binding: t -*-

;; check for Emacs 28+
;; do not early-init disable package loading, assuming this will always happen since it's been introduced since Emacs 27 and even Debian has 28 LOL.

;; TODO company-mode always enabled please
;; ielm/open relevant repl pls! (what is that for java?)
;; emacs jdtls should not create tons of rubbish files when running.

;;; Code:

(let ((minver "28.2"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; TODO if 29.1
;; (pixel-scroll-precision-mode 1)

;; only supported in Emacs 29
;;  (setq pixel-scroll-precision-large-scroll-height 40.0)

(setq custom-file (concat user-emacs-directory "custom.el")
      bidi-paragraph-direction 'left-to-right ;; performance
      bidi-inhibit-bpa t
      ring-bell-function nil
      enable-local-variables :safe
      inhibit-startup-screen t
      )
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(add-to-list 'completion-ignored-extensions ".git") ;; will still match if there are no other candidates.

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

(setq modus-themes-mode-line '(borderless)
      modus-themes-vivendi-color-overrides '((bg-main . "#111111")
					     (bg-dim . "#111111"))
      module-themes-org-blocks 'tinted-background
      modus-themes-italic-constructs t ;; TODO doesn't work?
      ;; TODO only seems to change color, 1.8/number should affect the size I think?
      ;; modus-themes-headings '((1 . (rainbow overline background 1.8)))
      )
(load-theme 'modus-vivendi t)


(defconst *fixed-font*
  (cond ((x-list-fonts "Rec Mono Linear") "Rec Mono Linear")
	((x-list-fonts "Recursive Mono Linear Static") "Recursive Mono Linear Static")
	((x-list-fonts "Monaco") "Monaco") ;; defaul mac os
	((x-list-fonts "Menlo") "Menlo") ;; mac os with vertical line glyph
	((x-list-fonts "Monospace") "Monospace") ;; linux deja vu sans mono default
	(t nil)))

(defconst *variable-font*
  (cond ((x-list-fonts "Rec Mono Casual") "Rec Mono Casual")
	((x-list-fonts "Recursive") "Recursive")
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
  "When called interactively with no active region, delete a single word backwards instead."
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

;; for light modes
;; (set-face-attribute 'hl-line nil :inherit nil :background "#eeeeee")
;; choices:
;; tango.. dirty white but nice
;; leuven, awesome org stuff (e.g background table)
;; modus-operandi pretty cool

;; WHAT leuven-dark is sick!!!! modeline is a bit weird but that's fine.
;; misterioso is pretty cool too.
;; modus-vivendi is very nice, but a bit high constrast, maybe we can lighten the background very slightly.

(defun code-config ()
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1)
  (hl-line-mode 1)
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook css-mode-hook)) (add-hook hook 'code-config))

(defun prose-config ()
  (variable-pitch-mode))

(dolist (hook '(markdown-mode)) (add-hook hook 'prose-config))

(defmacro ifn (fn)
  `(lambda () (interactive) ,fn))

(defmacro ifn-from (from-dir fn)
  `(lambda () (interactive)
     (let ((default-directory ,from-dir)) (call-interactively ,fn))))

;; TODO C-z crack open eshell in GUI mode
;; TODO super-z is undo even outside mac os.
;; TODO s-r to open repl.
(defun mikepjb/open-repl ()
  ;; TODO if already in  a "repl buffer", then close it
  (interactive)
  (progn
    (if (= (count-windows) 1)
	(split-window-below))
    (other-window 1)
    (cond ((eq major-mode 'emacs-lisp-mode) (ielm))
	  ((eq major-mode 'python-mode) (run-python))
	  (eshell))))

;; TODO does it matter if we include methods that may not exist in the keybindings?
(dolist
    (binding
     `(
       ("M-o" . other-window)
       ;; ("C-c p" . project-find-file)
       ("C-c a" . ,(ifn (org-agenda nil "d")))
       ("C-c A" . org-agenda)
       ("C-c i" . ,(ifn (find-file user-init-file)))
       ("C-c n" . ,(ifn (find-file (concat user-emacs-directory "org/notes.org"))))
       ("C-c o" . ,(ifn (find-file (concat user-emacs-directory "org/ops.org"))))
       ("C-c O" . ,(ifn-from "~/.emacs.d/org/" 'find-file))
       ("C-c g" . magit)
       ("C-c l" . flycheck-list-errors)
       ("C-c '" . modus-themes-toggle)
       ;; ("s-d" . duplicate-line) ;; think this is a recent function, Emacs 29.1
       ("C-;" . company-capf)
       ("M-D" . ,(ifn (progn (end-of-line 1)
			     (open-line 1)
			     (next-line 1)
			     (copy-from-above-command))))
       ("s-s" . save-buffer)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("C-h" . delete-backward-char)
       ("C-z" . mikepjb/open-repl)
       ("M-j" . ,(ifn (join-line -1)))
       ("M-H" . ,help-map)
       ("s-s" . save-buffer)
       ("s-o" . switch-to-buffer)
       ("s-k" . kill-buffer) ;; actually originally matched to kill-current-buffer, maybe try that out too.
       ("s-f" . find-file)
       ("C-c P" . ,(ifn-from "~/src/" 'find-file))
       ("M-F" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(add-hook 'org-mode-hook (lambda ()
			   (variable-pitch-mode)
			   (org-indent-mode)
			   (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
(add-hook 'org-agenda-mode-hook 'variable-pitch-mode)

;; also checkout purcell config
;; lsp-java?

;; TODO hl-line setting fails until after emacs fully loaded? it fails at startup either way, whatever the cause.

(require 'local nil t) ;; optionally load a local.el

;; 3rd party territory, everything up to this point should not fail without an internet connection.

(require 'package) ;; TODO built-in, should use as conditional for setting archives
(when (not (member "melpa" (mapcar 'car package-archives))) ;; check there isn't a local override
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/"))))

(require 'package)
(package-initialize)

(when (not (require 'use-package nil t))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package company
  :ensure t
  :init (global-company-mode t)
  )

(use-package eglot :ensure t
  :hook (prog-mode . eglot-ensure))
;; TODO not sure eglot is actually started in coding buffers.

(use-package flycheck :ensure t
  :init (global-flycheck-mode))

(use-package markdown-mode :ensure t)

;; (use-package magit :ensure t)

;; all use-packages fail if you don't have internet. "failed to install"
(use-package paredit :ensure t)
(use-package clojure-mode :ensure t)
(use-package cider :ensure t)

(provide 'init)

;;; init.el ends here
