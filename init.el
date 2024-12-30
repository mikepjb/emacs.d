;;; init.el -- Summary
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Designed to work as my main programming environment.

;;; Code:

(let ((minver "28.2"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Performance Tuning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq-default
   native-comp-async-report-warnings-errors nil
   native-comp-deferred-compilation t
   native-comp-async-jobs-number 4)) ;; assumes at least 4 cpus

(setq gc-cons-threshold 100000000 ; 100mb
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024) ;; Increase read process output max to 1mb
      create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      inhibit-compacting-font-caches t
      bidi-inhibit-bpa t
      frame-resize-pixelwise t
      inhibit-redisplay nil
      x-wait-for-event-timeout nil)

(setq-default bidi-paragraph-direction 'left-to-right)
(run-with-idle-timer 5 t #'garbage-collect) ;; Only garbage collect when idle for 5 seconds

(defun mx/check-large-file ()
  "Avoid slowing down Emacs by disabling major-mode work in large files."
  (when (> (buffer-size) (* 1024 1024))
    (fundamental-mode)
    (display-line-numbers-mode -1)
    (font-lock-mode -1)))
(add-hook 'find-file-hook #'mx/check-large-file)

;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar modus-themes-mode-line '(borderless))
(defvar modus-themes-vivendi-color-overrides '((bg-main . "#111111") (bg-dim . "#111111")))
(defvar modus-themes-org-blocks 'tinted-background "Style configuration for org blocks.")
(defvar modus-themes-italic-constructs t)
(load-theme 'modus-vivendi t)

(defconst *fixed-font* (seq-find #'x-list-fonts '("Rec Mono Linear" "Monaco" "Monospace")))
(defconst *variable-font* (seq-find #'x-list-fonts '("Rec Mono Casual" "Novaletra Serif CF" "Sans Serif")))
(defconst *font-size* 120)

(set-face-attribute 'default nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'fixed-pitch nil :font *fixed-font* :height *font-size*)
(set-face-attribute 'variable-pitch nil :font *variable-font* :height *font-size* :weight 'regular)

(when window-system
    (scroll-bar-mode -1)
    (set-frame-size (selected-frame) 140 80))

;; Emacs Behaviour ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el")
      inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore
      enable-local-variables :safe ;; dir-locals.el files can load most vars without asking.
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      tab-always-indent 'complete)

(setq-default recentf-max-menu-items 25
	      recentf-max-saved-items 25)

;; instead of company mode?
;; TODO this doesn't exist?
;; (add-hook 'after-init-hook #'global-completion-at-point-mode)

(recentf-mode 1)
(global-so-long-mode 1)
(global-auto-revert-mode 1)

(add-to-list 'completion-ignored-extensions ".git") ;; will still match if there are no other candidates.

(when (fboundp 'pixel-scroll-precision-mode) ;; expect in Emacs 29.1
  (pixel-scroll-precision-mode 1))

(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (mode '(electric-pair-mode
		fido-mode
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

(defun prose-config ()
  "Default configuration for writing buffers."
  (variable-pitch-mode))
(dolist (hook '(markdown-mode)) (add-hook hook 'prose-config))

;; TODO watching-video-mode (set-frame-parameter nil 'alpha '(90 . 75))
;; TODO prose "focus" mode

(defmacro ifn (fn) "Execute FN interactively." `(lambda () (interactive) ,fn))
(defmacro ifn-from (from-dir fn)
  "Execute FN in FROM-DIR."
  `(lambda () (interactive)
     (let ((default-directory ,from-dir)) (call-interactively ,fn))))

(defun mx/toggle-frame-size ()
  "Toggle current frame size between the default and a larger working space."
  (interactive) ;; width height
  (if (eq (frame-width) 140)
      (set-frame-size (selected-frame) 280 100)
      (set-frame-size (selected-frame) 140 80)))

(defun mx/toggle-frame-translucency ()
  "Toggle current frame translucency between fully opaque and slightly see-through."
  (interactive)
  (let ((level (if (eq (car (frame-parameter nil 'alpha)) 90) 100 90)))
    (set-frame-parameter nil 'alpha `(,level . ,level))))

;;TODO repl pls.

;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'vc)
(require 'vc-git)

(dolist
    (binding
     `(
       ;; vc-dir (show current changes in repo)
       ("M-o" . other-window)
       ("C-c 0" . mx/toggle-frame-size)
       ("C-c a" . org-agenda-list)
       ("C-c A" . org-agenda)
       ("C-c i" . ,(ifn (find-file user-init-file)))
       ("C-c n" . ,(ifn (find-file (concat user-emacs-directory "org/notes.org"))))
       ("C-c o" . ,(ifn (find-file (concat user-emacs-directory "org/ops.org"))))
       ("C-c O" . ,(ifn-from "~/.emacs.d/org/" 'find-file))
       ("C-c m" . recompile)
       ("C-c M" . ,(ifn-from (vc-git-root buffer-file-name) 'compile)) ;; compile from git root
       ("C-c p" . project-find-file)
       ("C-c g" . ,(ifn (vc-dir (vc-git-root buffer-file-name))))
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
       ("M-/" . comment-or-uncomment-region)
       ("s-s" . save-buffer)
       ("s-o" . other-window)
       ("s-k" . kill-buffer) ;; actually originally matched to kill-current-buffer, maybe try that out too.
       ("s-f" . find-file)
       ("C-c P" . ,(ifn-from "~/src/" 'find-file))
       ("M-J" . mx/toggle-frame-translucency)
       ("M-F" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))


(require 'local nil t) ;; optionally load a local.el

;; Packages & Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 org-agenda-files `(,(concat user-emacs-directory "org"))
 org-archive-location (concat user-emacs-directory "org/archive.org::")
 org-agenda-start-on-weekday nil ;; show the next 7 days
 org-agenda-start-day "0d"
 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "DOING(d)" "|" "DONE(d!)" "COMPLETE(c)" "CANCELLED(x)")))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(require 'org-indent)

(add-hook 'org-mode-hook (lambda ()
			   (variable-pitch-mode)
			   (org-indent-mode)
			   (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
(add-hook 'org-agenda-mode-hook 'variable-pitch-mode)

(setq package-quickstart t
      package-native-compile t)
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(condition-case err
    (package-initialize)
  (error (message "Failed to initialize packages: %s" err)))

(defmacro maybe-require (package &rest body)
  "Safely load PACKAGE and configure with BODY."
  `(if (require ',package nil 'noerror)
       (progn ,@body)
     (message "Package '%s' not available" ',package)))

;; TODO not sure these configs are firing, the packages are loading but the @body is maybe not being executed.. e.g no eglot on-save in go-mode.
;; TODO also consider ansi-color with compilation filter for compile.
(defvar mx-packages
  '((project . '((setq project-vc-extra-root-markers '(".git"))))
    (eglot . '((add-hook 'prog-mode-hook 'eglot-ensure)))
    (flycheck)
    (paredit) ;; TODO is this enabled for all lisp langs?
    (company-mode)
    (rust-mode)
    (yaml-mode)
    (typescript-mode)
    (cider)
    (clojure-mode)
    (go-mode . '((add-hook 'before-save-hook
			   (lambda ()
			     (gofmt-before-save)
			     (eglot-code-action-organize-imports 1)))))
    (flycheck-golangci-lint)
    (markdown-mode)))

(dolist (config mx-packages)
  (let ((package (car config))
	(setup-forms (cdr config)))
    (when (maybe-require package)
      (eval `(progn ,@setup-forms)))))

(provide 'init)
;;; init.el ends here
