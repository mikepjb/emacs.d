;;; Package --- init -------- -*- lexical-binding: t; -*-
;;
;;  Package-Requires: ((emacs "30.1"))

;;; Commentary:
;;  A Spartan Emacs configuration.
;;
;;  This is designed to be a minimal environment that is stable, to
;;  help the user get things done!

;;; Development Notes:
;;
;;  Consider using outline-minor-mode
;;  Consider using fewer/no packages
;;  Consider using flow-theme.el BUT refine colors etc before embarking on this mission.
;;  also ^^ modeus vivendi is awesome either way.
;;  Consider checking for external deps.. ripgrep/clojure/curl etc.
;;  Consider newsticker
;;  Consider your own olivetti again.
;;  Consider popper-mode.

;;; Code:

;;; Configuration:

;; possibly
;;(project-vc-extra-root-markers '("Cargo.toml" "package.json" "go.mod")) ; Excelent for mono repos with multiple langs, makes Eglot happy
(defconst *project-identifiers*
  '("Makefile" "gradlew" "pom.xml" "go.mod" "package.json" "deps.edn" ".git"))

(defconst *repl-table*
  '((python-mode      run-python)
    (ruby-mode        comint-run "irb")
    (emacs-lisp-mode  ielm)
    (edn-mode         inferior-lisp "clojure -A:dev")
    (clojure-mode     inferior-lisp "clojure -A:dev")))

(dolist (meta-key '(mac-command-modifier x-super-keysym))
  (when (boundp meta-key)
    (set meta-key 'meta)))

(setq isearch-wrap-pause 'no
      inhibit-startup-screen t
      split-width-threshold 170
      split-height-threshold nil
      ring-bell-function 'ignore
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      frame-resize-pixelwise t
      completion-ignore-case t
      global-goto-address-mode t
      create-lockfiles nil
      make-backup-files nil
      pixel-scroll-precision-mode t
      scroll-conservatively 8
      scroll-margin 5
      backup-directory-alist `(("." . ,(concat user-emacs-directory "saves")))
      large-file-warning-threshold (* 512 1024 1024) ; 500MB
      custom-file (concat user-emacs-directory "local.el")
      auth-sources (list (concat user-emacs-directory ".authinfo.gpg"))
      package-archives (if-let ((mirror (getenv "EMACS_PACKAGE_MIRROR")))
                           `(("local" . ,mirror))
                         '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/"))))

(modify-coding-system-alist 'file "" 'utf-8)

(add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

(load custom-file t)

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0) ;; not actually a minor mode!

  (set-face-attribute
   'default nil
   :font (seq-find #'x-list-fonts '("Rec Mono Casual" "Monospace"))
   :height 160))

(setq-default mode-line-format
  '(" "
    (:eval (propertize (+flow-truncate-buffer-name)
           'face '(:foreground "#00ccff" :weight bold)))
    " [%*]"
    mode-line-format-right-align
    (:eval (when (and (boundp 'vc-mode) vc-mode)
       (concat (propertize " | " 'face '(:foreground "#787878"))
         (replace-regexp-in-string " Git[-:]" "" vc-mode))))
    (:eval (propertize " | " 'face '(:foreground "#787878")))
    "%l:%c"
    (:eval (propertize " | " 'face '(:foreground "#787878")))
    (:eval (when-let ((proc (get-buffer-process (current-buffer))))
       (propertize (format "[%s] " (process-name proc))
       'face '(:foreground "#ff00ff" :weight bold))))
    (:eval (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
    " "))

(dolist (mode ;; global minor modes
         '(fido-vertical-mode
           global-auto-revert-mode
           save-place-mode
           savehist-mode))
  (funcall mode 1))


;;; Editor Settings:

(setq-default
 truncate-lines t
 indent-tabs-mode nil tab-width 2 standard-indent 2
 display-fill-column-indicator-column 80
 whitespace-style '(face trailing tabs empty indentation::space))

(dolist (mode '(show-paren-mode electric-pair-mode)) (funcall mode 1))

;;; Functions:

(defmacro +with-context (&rest body)
  `(let ((default-directory
          (or (seq-some (lambda (f) (locate-dominating-file default-directory f))
                       *project-identifiers*)
              default-directory)))
     ,@body))

(defun +compile ()
  (interactive)
  (+with-context (call-interactively 'compile)))

(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

(defun +repl-cmd   (entry) (cadr entry))
(defun +repl-args  (entry) (cddr entry))
(defun +repl-label (entry)
  (if (+repl-args entry)
      (car (last (+repl-args entry)))
    (symbol-name (+repl-cmd entry))))

(defun +repl-run (entry)
  (if (+repl-args entry)
      (apply (+repl-cmd entry) (+repl-args entry))
    (call-interactively (+repl-cmd entry))))

(defun +repl (arg)
  (interactive "P")
  (other-window-prefix)
  (if arg
      (let* ((unique (seq-uniq *repl-table*
                               (lambda (a b) (equal (+repl-label a) (+repl-label b)))))
             (choice (completing-read "REPL: " (mapcar #'+repl-label unique) nil t))
             (entry  (seq-find (lambda (e) (equal (+repl-label e) choice)) *repl-table*)))
        (+repl-run entry))
    (let ((entry (assq major-mode *repl-table*)))
      (if entry
          (+repl-run entry)
        (user-error "No REPL configured for %s" major-mode)))))

(defun +kill-region-or-backward-word ()
  (interactive)
  (cond ((region-active-p) (call-interactively #'kill-region))
        ((bound-and-true-p paredit-mode) (paredit-backward-kill-word))
        (t (backward-kill-word 1))))

(defun +minibuffer-C-w ()
  (interactive)
  (if (string-match-p "/" (minibuffer-contents))
      (icomplete-fido-backward-updir) (backward-kill-word 1)))

(defun +project-tags-generate ()
  (interactive)
  (+with-context
   (let ((default-directory default-directory))
     (make-process
      :name "ctags"
      :buffer nil
      :command '("ctags" "-eR" "-f" ".tags" "--exclude=node_modules" ".")
      :sentinel (lambda (_ e) (message "ctags: %s" (string-trim e)))))))

(defun +project-tags-load ()
  (when-let ((f (locate-dominating-file default-directory ".tags")))
    (visit-tags-table (expand-file-name ".tags" f) t)))

(defun +flow-truncate-buffer-name ()
  "Show right 1/3 of buffer name, prepend '<' if truncated."
  (let* ((name (if buffer-file-name
                   (abbreviate-file-name buffer-file-name)
                 (buffer-name)))
         (max-len (/ (window-width) 3)))
    (if (<= (length name) max-len)
        name
      (concat "<" (substring name (- (length name) (- max-len 1)))))))

(defun +paredit-RET ()
  (interactive)
  (call-interactively
   (pcase major-mode
     ('inferior-lisp-mode 'comint-send-input)
     ('minibuffer-mode 'read--expression-try-read)
     (_ 'paredit-RET))))

(defun +paredit-M-r ()
  (interactive)
  (call-interactively
   (pcase major-mode
     ('inferior-lisp-mode 'comint-history-isearch-backward-regexp)
     ('minibuffer-mode 'previous-matching-history-element)
     (_ 'paredit-raise-sexp))))

(defun +org-clock-todo-change ()
  (if (string= org-state "CURRENT")
      (org-clock-in)
    (org-clock-out-if-current)))

(defun +read-stacktrace ()
  "Open clipboard contents in a compilation buffer for navigation."
  (interactive)
  (let ((buf (get-buffer-create "*stacktrace*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (current-kill 0))
      (compilation-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;; Input/Keybinds:

(dolist (bind
         `(
           ("C-c C-l" flycheck-list-errors)
           ("C-c i" ,(ff user-emacs-directory "init.el"))
           ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
           ("C-c p" project-find-file)
           ("C-c P" ,(ff "~/src"))
           ("M-RET" toggle-frame-fullscreen)
           ("M-H" ,help-map)
           ("C-c a" ,(il (org-agenda nil "a")))
           ("C-z" nil)
           ("M-_" gptel)

           ;; Split management
           ("C-c k" ,(il (select-window (split-window-below))))
           ("C-c l" ,(il (select-window (split-window-right))))
           ("C-c o" delete-other-windows)
           ("M-o" ,(il (other-window 1)))
           ("M-O" ,(il (other-window -1)))

           ;; Code Tools
           ("M-E" flycheck-list-errors)
           ("M-i" rgrep)
           ("C-c g" vc-dir-root)
           ("C-c C-g" vc-print-root-log)
           ("M-T" eshell)
           ("M-R" +repl)

           ;; Editing
           ("M-/" replace-string)
           ("M-n" forward-paragraph) ;; more ergo than M-{} but not that easy
           ("M-p" backward-paragraph)
           ("C-w" +kill-region-or-backward-word)
           ("M-K" kill-whole-line)
           ("C-h" delete-backward-char)
           ("C-j" newline) ;; autoindents
           ("M-j" ,(il (join-line -1)))
           ("M-s" save-buffer)

           ("M-D" duplicate-line)

           ("C-c m" recompile)  ("C-c M" +compile)
           ("C-;" completion-at-point)
           ("C-c C-s" ,search-map)))
  (global-set-key (kbd (car bind)) (cadr bind)))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w") #'+minibuffer-C-w)
  (define-key icomplete-minibuffer-map (kbd "C-e") #'icomplete-ret))

;;; Packages:

(use-package clojure-mode :ensure t)
(use-package json-mode :ensure t)
(use-package project :ensure nil)
(use-package olivetti :ensure t
  :custom
  (olivetti-style nil)
  (olivetti-body-width 80)
  :bind (:map org-mode-map
              ("M-RET" . nil)
              ;; ("M-RET" . org-meta-return) ;; TODO needs new binding
              )
  :hook ((org-mode-hook
          markdown-mode-hook)
         . olivetti-mode))

(use-package org :ensure nil
  :custom
  (org-ellipsis " ▼")
  (org-hide-emphasis-markers t)
  (org-export-with-section-numbers nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "CURRENT(c)" "|" "DONE(d!)" "CANCELLED(x@)")))
  (org-todo-keyword-faces
   '(("TODO"      . (:foreground "#ff6c6b" :weight bold))
     ("NEXT"      . (:foreground "#51afef" :weight bold))
     ("CURRENT"   . (:foreground "#ef51af" :weight bold))
     ("DONE"      . (:foreground "#98be65"))
     ("CANCELLED" . (:foreground "#5B6268" :strike-through t))))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  (org-directory "~/.emacs.d/notes")
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(clock closed))
  (org-agenda-files '("~/.emacs.d/notes"))
  (org-agenda-custom-commands
   '(("a" "Agenda + Unscheduled TODOs"
      ((agenda "")
       (todo "TODO|NEXT|CURRENT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
              (org-agenda-overriding-header "Unscheduled TODOs")))))))
  :hook ((org-mode . org-indent-mode)
         (org-after-todo-state-change . +org-clock-todo-change))
  :config (org-clock-persistence-insinuate))

;; (use-package which-key
;;   :defer t
;;   :ensure nil
;;   :hook
;;   (after-init-hook . which-key-mode)
;;   :config
;;   (setq which-key-separator " ")
;;   (setq which-key-prefix-prefix "… ")
;;   (setq which-key-max-display-columns 3)
;;   (setq which-key-idle-delay 0.5)
;;   (setq which-key-idle-secondary-delay 0.25)
;;   (setq which-key-add-column-padding 1)
;;   (setq which-key-max-description-length 40))

(use-package paredit :ensure t
  :hook ((clojure-mode-hook
          emacs-lisp-mode-hook
          inferior-lisp-mode-hook lisp-data-mode-hook
          eval-expression-minibuffer-setup-hook)
         . #'enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("C-j" . +paredit-RET)
              ("M-r" . +paredit-M-r)
              ("M-k" . paredit-forward-barf-sexp)
              ("M-l" . paredit-forward-slurp-sexp)
              ("C-z" . paredit-splice-sexp)
              ("M-s" . nil)))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))

(use-package compile :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t))

(use-package js :ensure nil
  :custom
  (js-indent-level 2))

(use-package vc :ensure nil
  :custom
  (vc-handled-backends '(Git))
  (vc-auto-revert-mode t)
  (vc-git-diff-switches '("--patch-with-stat" "histogram"))
  (vc-git-log-switches '("--stat"))
  (vc-git-log-edit-summary-target-len 50)
  (vc-git-print-log-follow t)
  (vc-git-show-stash 0)
  (vc-dir-hide-up-to-date-on-revert t)
  (vc-make-backup-files nil)
  (vc-git-diff-switches '("--stat" "-p"))
  :config
  (remove-hook 'log-edit-hook #'log-edit-show-files))

(use-package diff-mode :ensure nil
  :defer t
  :bind (:map diff-mode-map ("M-o" . nil)))

(use-package grep
  :ensure nil
  :config
  (defvar +rg-available (executable-find "rg"))

  (when +rg-available
    (grep-apply-setting
     'grep-command "rg --no-heading --color=never -nH -e ")
    (grep-apply-setting
     'grep-find-command
     '("rg --no-heading --color=never -nH -r -e ''" . 46)))

  (when +rg-available
    (setq xref-search-program 'ripgrep))

  (defun +grep-project (regexp)
    "Search REGEXP recursively from the project root."
    (interactive "sSearch: ")
    (+with-context
     (grep (concat (if +rg-available
                       "rg --no-heading --color=never -nH -r -e "
                     "grep -rn --color=never -e ")
                   (shell-quote-argument regexp)
                   " ."))))
  :bind
  ("M-i" . +grep-project))

(use-package dired
  :ensure nil
  :custom (dired-kill-when-opening-new-dired-buffer t))

(use-package eshell
  :ensure nil
  :custom
  (eshell-visual-commands '("vi" "vim" "nvim" "top" "htop" "less" "more" "psql" "sqlite3" "w3m"))
  :config
  (setopt eshell-banner-message ""))

(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user))

(use-package gptel
  :config
  (gptel-make-openai "llama.cpp"
    :host "127.0.0.1:7777"
    :protocol "http"
    :endpoint "/inference/v1/chat/completions"
    :stream t
    :models '(gemma4-26b-a4b))

  (gptel-make-openai "fireworks"
    :host "api.fireworks.ai"
    :endpoint "/inference/v1/chat/completions"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "api.fireworks.ai"))
    :models '(accounts/fireworks/models/kimi-k2p5
              accounts/fireworks/models/glm-4p7))

  (gptel-make-openai "gemini"
    :host "generativelanguage.googleapis.com"
    :endpoint "/v1beta/openai/chat/completions"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "generativelanguage.googleapis.com"))
    :models '(gemini-3-flash-preview))

  (setq gptel-backend (gptel-get-backend "fireworks")
        gptel-model  'accounts/fireworks/models/kimi-k2p5))

(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
                   (display-line-numbers-mode 1) (column-number-mode 1)
                   (completion-preview-mode 1)
                   (display-fill-column-indicator-mode 1) (hl-line-mode 1))))

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'find-file-hook #'+project-tags-load)

(load-theme 'modus-vivendi t)

(provide 'init)
;;; init.el ends here
