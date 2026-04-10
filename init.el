;;; Spartan Emacs -binding: t; -*-

;;  Consider newsticker
;;  Consider your own olivetti again.
;;  Consider popper-mode.
;;  Consider grouping C-x C-b buffer list.
;;  Consider using eglot+flymake instead of flycheck+ctags

;;; Configuration:

;; possibly
;;(project-vc-extra-root-markers '("Cargo.toml" "package.json" "go.mod")) ; Excelent for mono repos with multiple langs, makes Eglot happy

(setq gc-cons-threshold (* 64 1024 1024) ; 64MB
      gc-cons-percentage 0.2)

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
      create-lockfiles nil
      make-backup-files nil
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

(dolist (mode ;; global minor modes
         '(fido-vertical-mode
           global-auto-revert-mode
           save-place-mode
           global-goto-address-mode
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

(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

(defun +kill-region-or-backward-word ()
  (interactive)
  (cond ((region-active-p) (call-interactively #'kill-region))
        ((bound-and-true-p paredit-mode) (paredit-backward-kill-word))
        (t (backward-kill-word 1))))

(defun +minibuffer-C-w ()
  (interactive)
  (if (string-match-p "/" (minibuffer-contents))
      (icomplete-fido-backward-updir) (backward-kill-word 1)))

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
           ;; Useful defaults
           ;; "C-x C-;" => comment-line (also does uncomment + region)

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
           ("M-i" rgrep)
           ("C-c g" vc-dir-root)
           ;; ("C-c C-g" vc-print-root-log) ;; C-g should always exit lol
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

(use-package js :ensure nil
  :custom
  (js-indent-level 2))

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-hide-markup t))

(use-package vc :ensure nil
  :custom
  (vc-handled-backends '(Git))
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
     '("rg --no-heading --color=never -nH -e ''" . 46)))

  (when +rg-available
    (setq xref-search-program 'ripgrep))

  ;; TODO needs to move to context package
  (defun +grep-project (regexp)
    "Search REGEXP recursively from the project root."
    (interactive "sSearch: ")
    (+with-context
     (grep (concat (if +rg-available
                       "rg --no-heading --color=never -nH -e "
                     "grep -rn --color=never -e ")
                   (shell-quote-argument regexp)
                   " ."))))
  :bind
  ("M-i" . +grep-project))

(use-package project :ensure nil)
(use-package olivetti :ensure t
  :custom
  (olivetti-style nil)
  (olivetti-body-width 80)
  :hook ((org-mode
          markdown-mode)
         . olivetti-mode))

(use-package paredit :ensure t
  :hook ((clojure-mode
          emacs-lisp-mode
          inferior-lisp-mode lisp-data-mode
          eval-expression-minibuffer-setup)
         . #'enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("C-j" . +paredit-RET)
              ("M-r" . +paredit-M-r)
              ("M-k" . paredit-forward-barf-sexp)
              ("M-l" . paredit-forward-slurp-sexp)
              ("C-z" . paredit-splice-sexp)
              ("M-s" . nil)))

(use-package compile :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t))

(use-package dired
  :ensure nil
  :custom (dired-kill-when-opening-new-dired-buffer t))

(use-package eshell
  :ensure nil
  :custom
  (eshell-visual-commands '("vi" "vim" "nvim" "top" "htop" "less" "more" "psql" "sqlite3" "w3m"))
  (eshell-banner-message ""))

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

;; only useful if you figure out how to integration with clj-kondo/eslint etc
;; (use-package flymake :ensure nil
;;   :hook (prog-mode . flymake-mode)
;;   :custom
;;   (flymake-no-changes-timeout nil) ; check on save only, not while typing
;;   (flymake-fringe-indicator-position 'left-fringe)
;;   (flymake-show-diagnostics-at-end-of-line nil)
;;   (flymake-wrap-around nil)
;;   :bind (:map flymake-mode-map
;;               ;; ("C-c e n" . flymake-goto-next-error)
;;               ;; ("C-c e p" . flymake-goto-prev-error)
;;               ("C-c C-l" . flymake-show-buffer-diagnostics)
;;               ))

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1) (column-number-mode 1)
            (completion-preview-mode 1)
            (display-fill-column-indicator-mode 1) (hl-line-mode 1)))

(add-hook 'before-save-hook 'whitespace-cleanup)
;; (add-hook 'find-file-hook #'+project-tags-load) ;; slow AF to run on every file change.

(load-theme 'flow t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'spartan-ai)
(require 'spartan-repl)
(require 'spartan-org)
(require 'spartan-environment)
(require 'spartan-context)

(provide 'init)
;;; init.el ends here
