;;; 🔱 Spartan Emacs -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 64 1024 1024))
(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

(dolist (k '(mac-command-modifier x-super-keysym))
  (when (boundp k) (set k 'meta)))

(load-theme 'flow t)

(modify-coding-system-alist 'file "" 'utf-8)

(setq
 ;; Emacs/System settings
 inhibit-startup-screen t
 ring-bell-function 'ignore
 use-dialog-box nil
 use-file-dialog nil
 use-short-answers t
 split-width-threshold 170
 split-height-threshold nil
 frame-resize-pixelwise t
 completion-ignore-case t
 load-prefer-newer t ;; init.el > init.elc if newer
 dired-listing-switches "-lah" ;; human readable sizes.. again, does this work when set this way?
 package-archives '(("melpa" . "https://melpa.org/packages/")
                    ("gnu" . "https://elpa.gnu.org/packages/"))

 ;; Editing
 isearch-wrap-pause 'no

 ;; Save files
 create-lockfiles nil
 make-backup-files nil
 custom-file (concat user-emacs-directory "local.el")

 ;; Code tools
 large-file-warning-threshold (* 512 1024 1024) ;; for ctags
 compilation-always-kill t
 compilation-scroll-output t
 ansi-color-for-compilation-mode t ;; does this work when set this way? check.
 vc-handled-backends '(Git)
 vc-make-backup-files nil
 eshell-banner-message ""
 eshell-visual-commands '("vi" "htop" "less" "more"))

(setq-default
 ;; Editing
 truncate-lines t
 indent-tabs-mode nil
 tab-width 2
 standard-indent 2
 whitespace-style '(face trailing tabs empty indentation::space)
 )

(load custom-file t)

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0)
  (set-face-attribute 'default nil
    :font (seq-find #'x-list-fonts '("Rec Mono Casual" "Monospace")) :height 160))

(dolist (m '(fido-vertical-mode
             global-auto-revert-mode
             save-place-mode
             global-goto-address-mode
             savehist-mode
             show-paren-mode
             electric-pair-mode))
  (funcall m 1))

(dolist (b `(("C-c i" ,(ff user-emacs-directory "init.el"))
             ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
             ("C-c a" ,(il (org-agenda nil "a")))
             ("C-c g" vc-dir-root)
             ("C-c p" project-find-file)
             ("C-c P" ,(ff "~/src"))

             ;; Split management
             ("C-c k" ,(il (select-window (split-window-below))))
             ("C-c l" ,(il (select-window (split-window-right))))
             ("C-c o" delete-other-windows)
             ("M-o" ,(il (other-window 1)))
             ("M-O" ,(il (other-window -1)))

             ("C-c f" imenu)
             ("M-RET" toggle-frame-fullscreen)
             ("M-H" ,help-map)
             ("C-c t" +ctags)
             ("M-i" ,(il (+with-context (call-interactively 'rgrep))))

             ("M-T" eshell)
             ("M-R" ,(il (other-window-prefix) (inferior-lisp "clojure -A:dev")))

             ("C-h" delete-backward-char)
             ("C-j" newline) ;; autoindents
             ("M-j" ,(il (join-line -1)))
             ("M-s" save-buffer)
             ("C-w" +kill-region-or-backward-word)
             ("M-K" kill-whole-line)
             ("M-D" duplicate-line)
             ("M-n" forward-paragraph)
             ("M-p" backward-paragraph)
             ("M-/" replace-string) ;; maybe there's better fn that supports regions too?
             ("C-c m" recompile) ("C-c M" +compile)
             ))
  (global-set-key (kbd (car b)) (cadr b)))

;; this whole minibuffer re-mapping.. just seems like it should be a
;; configuration rather than ad-hoc code, I am surprised C-w doesn't
;; go back a directory out of the box or at least be a customize option.
(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w")
    (lambda () (interactive)
      (if (string-match-p "/" (minibuffer-contents))
          (icomplete-fido-backward-updir) (backward-kill-word 1))))
  (define-key icomplete-minibuffer-map (kbd "C-e") #'icomplete-ret))

(use-package proced :ensure nil :defer t :custom (proced-filter 'user))
(use-package which-key :ensure nil :init (which-key-mode 1))

(with-eval-after-load 'diff-mode
  (define-key diff-mode-map (kbd "M-o") nil))

;; Functions
(defun +kill-region-or-backward-word ()
  (interactive)
  (cond ((region-active-p) (call-interactively #'kill-region))
        ((bound-and-true-p paredit-mode) (paredit-backward-kill-word))
        (t (backward-kill-word 1))))

(defconst *context-markers*
  '("Makefile" "gradlew" "pom.xml" "go.mod" "package.json" "deps.edn" ".git"))

(defmacro +with-context (&rest body)
  `(let ((default-directory
          (or (seq-some (lambda (f) (locate-dominating-file default-directory f))
                        *context-markers*)
              default-directory)))
     ,@body))

(defun +compile () (interactive) (+with-context (call-interactively 'compile)))

(defun +ctags ()
  (interactive)
  (+with-context
   (make-process :name "ctags" :buffer nil
     :command '("ctags" "-eR" "-f" ".tags" "--exclude=node_modules" ".")
     :sentinel (lambda (_ e) (message "ctags: %s" (string-trim e))))))

(defun +ctags-link ()
  (when-let ((dir (locate-dominating-file default-directory ".tags")))
    (let ((tags-file (expand-file-name ".tags" dir)))
      (unless (member tags-file tags-table-list)
        (visit-tags-table tags-file t)))))

(add-hook 'find-file-hook #'+ctags-link)

(use-package paredit :ensure t
  :hook ((clojure-mode emacs-lisp-mode inferior-lisp-mode
                       lisp-data-mode eval-expression-minibuffer-setup)
         . enable-paredit-mode)
  :config
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
  :bind (:map paredit-mode-map
              ("C-j" . +paredit-RET)
              ("M-r" . +paredit-M-r)
              ("M-k" . paredit-forward-barf-sexp)
              ("M-l" . paredit-forward-slurp-sexp)
              ("C-z" . paredit-splice-sexp)
              ("M-s" . nil)))

;; (add-hook 'prog-mode) ;; not sure what the syntax is here

(use-package clojure-mode :ensure t)
(use-package typescript-mode :ensure t :custom (typescript-indent-level 2))
(use-package json-mode :ensure t)
(use-package js :ensure nil :custom (js-indent-level 2))
(use-package markdown-mode :ensure t :custom (markdown-hide-markup t))

(add-hook 'prog-mode-hook
  (lambda () (display-line-numbers-mode 1) (hl-line-mode 1)))
(add-hook 'before-save-hook #'whitespace-cleanup)

(use-package olivetti :ensure t ;; to replace (eventually)
  :custom
  (olivetti-style nil)
  (olivetti-body-width 80)
  :hook ((org-mode
          markdown-mode)
         . olivetti-mode))

(use-package org :ensure nil
  :custom
  (org-modules nil)
  (org-ellipsis " ▼")
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "CURRENT(c)" "|" "DONE(d!)" "CANCELLED(x@)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-span 14)
  (org-agenda-start-day "today")
  (org-agenda-start-on-weekday nil)
  (org-archive-location "~/.emacs.d/notes/archive.org::* From %s")
  (org-directory "~/.emacs.d/notes")
  (org-agenda-files '("~/.emacs.d/notes"))
  (org-agenda-custom-commands
   '(("a" "Agenda + Unscheduled TODOs"
      ((agenda "")
       (todo "TODO|NEXT|CURRENT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
              (org-agenda-overriding-header "Unscheduled TODOs")))))))
  (org-refile-targets
   '((nil :maxlevel . 3)
     ("~/.emacs.d/notes/archive.org" :maxlevel . 3)))
  :bind (:map org-mode-map
              ("M-RET" . nil)
              ("C-c r" . org-archive-subtree))
  :hook ((org-mode . org-indent-mode)
         (org-after-todo-state-change . +org-clock-todo-change))
  :config
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

(defun +org-clock-todo-change ()
  (if (string= org-state "CURRENT")
      (org-clock-in)))

(provide 'init)
