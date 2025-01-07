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
      inhibit-redisplay nil)

(setq-default
 bidi-paragraph-direction 'left-to-right
 x-wait-for-event-timeout nil) ;; TODO does this work as setq-default? What's the difference vs. setq?
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
      "/opt/homebrew/opt/node@16/bin")
    ":")))

(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))

;; Fix SSL connection for F-Droid version of Emacs.
(when (string-equal system-type "android")
  (setq-default ;; originally used setq
   tls-program '("gnutls-cli -p %p %h"
		 "gnutls-cli -p %p %h --protocols ssl3")))

;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar modus-themes-mode-line '(borderless))
(defvar modus-themes-vivendi-color-overrides '((bg-main . "#111111") (bg-dim . "#111111")))
(defvar modus-themes-org-blocks 'tinted-background "Style configuration for org blocks.")
(defvar modus-themes-italic-constructs t)
(load-theme 'modus-vivendi t)

(defconst *fixed-font* (seq-find #'x-list-fonts '("Rec Mono Linear" "Monaco" "Monospace")))
(defconst *variable-font* (seq-find #'x-list-fonts '("Rec Mono Casual" "Novaletra Serif CF" "Sans Serif")))
(defconst *font-size* 130)

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

(recentf-mode 1)
(global-so-long-mode 1)
(global-auto-revert-mode 1)
(winner-mode 1) ;; record and revert/replay window/split configurations
(fido-vertical-mode 1)

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

;; TODO highlight TODOs
;; TODO update bindings so that we aren't using super as Mac OS will have opt+cmd both as super.
;; this will require remapping M-s probably to make saving easier (or consider another binding?)
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
       ("M-s" . save-buffer) ;; C-x C-s is too awkward for a common keybind.
       ("M-S" . ,search-map)
       ("M-/" . comment-or-uncomment-region)
       ("s-k" . kill-buffer) ;; actually originally matched to kill-current-buffer, maybe try that out too.
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

;; TODO one of these functions isn't defined initially, this works if you list-packages first.. would be nice to fix it.
(defun require-package (package)
  "Require PACKAGE, installing it too if not available."
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package))
  (package-installed-p package))


(defun maybe-require-package (package)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message."
  (condition-case err
      (require-package package)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(when (maybe-require-package 'project)
  ;; TODO consider making it so all ~/src projects are project.el projects.
  ;; This extra-root-markers setting just adds projects as they are manually visited.. which is still good but maybe we can do better.
  (setq-default project-vc-ignores '("node_modules"))
  (setq-default project-vc-extra-root-markers '(".git")))

(when (maybe-require-package 'eglot)
  (add-hook 'prog-mode-hook 'eglot-ensure))

(maybe-require-package 'flycheck)

(maybe-require-package 'paredit) ;; TODO do I need to add this for elisp/clojure & other lisps? probs derived lisp-mode?

(maybe-require-package 'company)
;; TODO tab auto complete?

(maybe-require-package 'rust-mode)
(maybe-require-package 'yaml-mode)
(maybe-require-package 'markdown-mode)
(maybe-require-package 'typescript-mode)

(maybe-require-package 'clojure-mode)
(maybe-require-package 'cider)

(when (maybe-require-package 'go-mode)
  (add-hook 'before-save-hook
	    (gofmt-before-save)
	    ;; no current JSON-RPC connection?
	    ;; (eglot-code-action-organize-imports 1)
	    ))

(maybe-require-package 'flycheck-golangci-lint)

(maybe-require-package 'restclient)

(provide 'init)
;;; init.el ends here
