;;; package --- init -------- -*- lexical-binding: t; -*-

;;; Commentary:
;;; A Spartan Emacs configuration.
;;;
;;; This is designed to be a minimal environment that is stable, to
;;; help the user get things done!

;;; Code:

;;; Configuration:

(defconst *project-identifiers*
  '("Makefile" "gradlew" "pom.xml" "go.mod" "package.json" "deps.edn" ".git"))

(defconst *repl-table*
  '((python run-python)
    (ruby irb))) ;; how to handle SQL? all .sql

(dolist (meta-key '(mac-command-modifier x-super-keysym))
  (when (boundp meta-key)
    (set meta-key 'meta)))

(setq isearch-wrap-pause 'no
      inhibit-startup-screen t
      ring-bell-function 'ignore
      create-lockfiles nil
      use-short-answers t
      frame-resize-pixelwise t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "saves")))
      large-file-warning-threshold (* 512 1024 1024) ; 500MB
      custom-file (concat user-emacs-directory "local.el")
      auth-sources (concat user-emacs-directory ".authinfo.gpg")
      package-archives (if-let ((mirror (getenv "EMACS_PACKAGE_MIRROR")))
                           `(("local" . ,mirror))
                         '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/"))))

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
         (replace-regexp-in-string "^ Git[-:]" "" vc-mode))))
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

(defconst *editing-modes*
  '(show-paren-mode
    electric-pair-mode))

(dolist (mode *editing-modes*) (funcall mode 1))

;;; Functions:

(defmacro +with-context (&rest body)
  `(let ((default-directory
          (or (cl-some (lambda (f) (locate-dominating-file default-directory f))
                       *project-identifiers*)
              default-directory)))
     ,@body))

(defun +compile ()
  (interactive)
  (+with-context (call-interactively 'compile)))

(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

;; (defun center-prose (buf)
;;   (when (buffer-live-p buf)
;;     (dolist (win (get-buffer-window-list buf nil 'visible))
;;       (let ((margin (max 0 (round (/ (- (window-body-width win t)
;;           (* 80 (frame-char-width)))
;;              2.0 (frame-char-width))))))
;;   (set-window-margins win margin margin)))))

;; (defun prose-config ()
;;   (visual-line-mode 1)
;;   (let ((buf (current-buffer)))
;;     (add-hook 'window-size-change-functions
;;         (lambda (&rest _) (center-prose buf))
;;         nil t)))

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
  "Manually generate tags for current project."
  (interactive)
  (when-let ((project-root (cl-some (lambda (f) (locate-dominating-file default-directory f))
            *project-identifiers*)))
    (let ((tags-file (expand-file-name (format "~/.emacs.d/.tag-store/%s.tags"
                 (file-name-nondirectory (directory-file-name project-root))))))
      (ignore-errors (make-directory (file-name-directory tags-file) t))
      (make-process
       :name "ctags"
       :buffer (get-buffer-create "*ctags*")
       :command `("ctags" "--output-format=etags" "-f" ,tags-file "-R"
      "--languages=all" "--exclude=node_modules" ,project-root)
       :sentinel (lambda (_ event)
       (when (string-match-p "finished" event)
         (message "Tags generated at %s" tags-file))))
      (when (file-exists-p tags-file) ;; assign newly generated tags file
  (setq-local tags-table-list (list tags-file))
  (visit-tags-table tags-file t)))))

(defun +project-tags-load ()
  "Load tags file for current project if it exists."
  (when-let ((project-root (cl-some (lambda (f) (locate-dominating-file default-directory f))
            *project-identifiers*)))
    (let ((tags-file (expand-file-name (format "~/.emacs.d/.tag-store/%s.tags"
                 (file-name-nondirectory (directory-file-name project-root))))))
      (when (file-exists-p tags-file)
  (setq-local tags-table-list (list tags-file))
  (visit-tags-table tags-file t)))))

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

(defun +time-current-tasks ()
  "Clock in when entering CURRENT, clock out when leaving."
  (when (and (bound-and-true-p org-state)
             )
    (if (string= org-state "CURRENT")
        (org-clock-in)
      (when (string= org-from-state "CURRENT")
        (org-clock-out nil t)))))

(defun +org-clock-todo-change ()
  (if (string= org-state "CURRENT")
      (org-clock-in)
    (org-clock-out-if-current)))

;;; Input/Keybinds:

(dolist (bind
         `(
           ("M-C" ,(il (comint-run "navi")))
           ("C-c C-l" flycheck-list-errors)
           ("C-c i" ,(ff user-emacs-directory "init.el"))
           ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
           ("C-c p" project-find-files)
           ("C-c P" ,(ff "~/src"))
           ("M-RET" toggle-frame-fullscreen)
           ("M-H" ,help-map)
           ("C-c a" org-agenda-list)

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
           ("C-;" execute-extended-command)
           ;; ("C-;" dabbrev-expand)
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
  :hook (text-mode-hook . olivetti-mode))

(use-package org :ensure nil
  :custom
  (org-ellipsis " ▼")
  (org-hide-emphasis-markers t)
  (org-export-with-section-numbers nil)
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
  ;; Log DONE time into LOGBOOK drawer
  (org-log-done 'time)
  (org-log-into-drawer t)
  ;; Clock tracking
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  ;; Agenda files — point at your org dir
  (org-agenda-files '("~/.emacs.d/notes/"))
  :hook ((org-mode . org-indent-mode)
         (org-after-todo-state-change . +org-clock-todo-change))
  :config (org-clock-persistence-insinuate))

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
              ("C-;" . paredit-splice-sexp)
              ("M-s" . nil)))

(use-package flycheck
  :ensure nil
  :config (global-flycheck-mode 1))

(use-package ansi-color :ensure nil
  :hook (compilation-filter-hook ansi-color-compilation-filter))

(use-package vc :ensure nil
  :custom
  (vc-handled-backends '(Git))
  (vc-git-diff-switches '("--stat" "-p"))
  :config
  (remove-hook 'log-edit-hook #'log-edit-show-files))

(use-package diff-mode :ensure nil
  :bind (:map diff-mode-map ("M-o" . nil)))

(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
                   (display-line-numbers-mode 1) (column-number-mode 1)
                   (auto-revert-mode 1)
                   (display-fill-column-indicator-mode 1) (hl-line-mode 1))))

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'find-file-hook #'+project-tags-load)

(load-theme 'modus-vivendi t)

(provide 'init)
;;; init.el ends here
