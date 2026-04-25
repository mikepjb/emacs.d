;;; 🔱 Spartan Emacs -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 64 1024 1024))
(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))
(defun +setm (param modes) (dolist (m modes) (funcall m param)))

(+setm -1 '(menu-bar-mode
            tool-bar-mode
            scroll-bar-mode
            blink-cursor-mode))

(+setm 1 '(fido-vertical-mode
           global-auto-revert-mode
           save-place-mode
           global-goto-address-mode
           savehist-mode
           show-paren-mode
           electric-pair-mode))

(dolist (k '(mac-command-modifier x-super-keysym))
  (when (boundp k) (set k 'meta)))

(setq ;; Emacs/System settings
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
 dired-listing-switches "-lah" ;; human readable sizes
 create-lockfiles nil
 make-backup-files nil
 auto-save-default nil
 isearch-wrap-pause 'no
 compilation-always-kill t
 compilation-scroll-output t
 vc-handled-backends '(Git)
 eshell-banner-message ""
 custom-file (concat user-emacs-directory "local.el")
 package-archives '(("melpa" . "https://melpa.org/packages/")
                    ("gnu" . "https://elpa.gnu.org/packages/")))

(setq-default
 truncate-lines t
 indent-tabs-mode nil
 tab-width 2
 standard-indent 2
 whitespace-style '(face trailing tabs empty indentation::space)
 cursor-in-non-selected-windows nil)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(require 'external)
(load custom-file t)
(load-theme 'flow t)

(defun +font (&rest names)
  (seq-find (lambda (f) (member f (font-family-list))) names))

(when (display-graphic-p)
  (fringe-mode 0)
  (when-let ((mono (+font "Rec Mono Linear")))
    (set-face-attribute 'default     nil :font mono :height 160)
    (set-face-attribute 'fixed-pitch nil :font mono :height 140))
  (when-let ((vari (+font "Recursive Sans Casual Static")))
    (set-face-attribute 'variable-pitch nil :font vari :height 160))
  (when-let ((fci (+font "Menlo" "Noto Sans Mono")))
    (set-face-attribute 'fill-column-indicator nil :family fci))
  (setq-default display-fill-column-indicator-character ?│))

(dolist (b `(;; Navigation
             ("C-c i" ,(ff user-emacs-directory "init.el"))
             ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
             ("C-c a" ,(il (org-agenda nil "a")))
             ("C-c g" vc-dir-root)
             ("C-c p" project-find-file)
             ("C-c P" ,(ff "~/src"))

             ;; Information
             ("M-N" newsticker-show-news)

             ;; Display management
             ("C-c k" ,(il (select-window (split-window-below))))
             ("C-c l" ,(il (select-window (split-window-right))))
             ("C-c o" delete-other-windows)
             ("M-o" ,(il (other-window 1)))
             ("M-O" ,(il (other-window -1)))
             ("M--" ,(il (set-frame-size nil 160 50)))
             ("M-L" flow-toggle-theme)
             ("M-V" ,(il (load-theme 'flow t)))
             ("M-RET" toggle-frame-fullscreen)

             ;; Code Tools + Editing
             ("C-c f" imenu)
             ("C-;" completion-at-point) ;; anything more important?
             ("M-H" ,help-map)
             ("C-c t" +ctags)
             ("M-i" ,(il (+with-context (call-interactively 'rgrep))))
             ("M-I" ,(il (+with-context (call-interactively 'occur))))
             ("M-T" eshell)
             ("M-R" +repl)
             ("C-h" delete-backward-char)
             ("C-j" newline) ;; autoindents
             ("M-j" ,(il (join-line -1)))
             ("M-s" save-buffer)
             ("C-w" +kill-region-or-backward-word)
             ("M-K" kill-whole-line)
             ("M-D" duplicate-line)
             ("M-n" forward-paragraph)
             ("M-p" backward-paragraph)
             ("M-/" replace-string)
             ("C-c m" recompile)
             ("C-c M" ,(il (+with-context (call-interactively 'compile))))
             ("C-c ." +test)))
  (global-set-key (kbd (car b)) (cadr b)))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w")
    (lambda () (interactive)
      (if (string-match-p "/" (minibuffer-contents))
          (icomplete-fido-backward-updir) (backward-kill-word 1))))
  (define-key icomplete-minibuffer-map (kbd "C-e") #'icomplete-ret))

(with-eval-after-load 'diff-mode
  (define-key diff-mode-map (kbd "M-o") nil))

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

(defun +lisp-load-current-file ()
  (interactive)
  (lisp-load-file (buffer-file-name)))

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

(use-package clojure-mode :ensure t
  :bind (:map clojure-mode-map
              ("C-x C-e" . lisp-eval-last-sexp)
              ("C-M-x"   . lisp-eval-defun)
              ("C-c C-r" . lisp-eval-region)
              ("C-c C-k" . +lisp-load-current-file)
              ("C-c C-l" . lisp-load-file))
  :hook (clojure-mode . (lambda ()
                          (setq-local inferior-lisp-load-command
                                      "(load-file \"%s\")\n"))))

(use-package go-mode :ensure t)
(use-package typescript-mode :ensure t :custom (typescript-indent-level 2))
(use-package json-mode :ensure t)
(use-package js :ensure nil :custom (js-indent-level 2))
(use-package markdown-mode :ensure t :custom (markdown-hide-markup t))

(add-hook 'prog-mode-hook
  (lambda () (display-line-numbers-mode 1) (hl-line-mode 1)
    (display-fill-column-indicator-mode 1)))
(add-hook 'before-save-hook #'whitespace-cleanup)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtlsw"))))

(use-package olivetti :ensure t
  :custom
  (olivetti-style nil)
  (olivetti-body-width 80)
  :hook ((org-mode markdown-mode) . olivetti-mode))

(use-package org :ensure nil
  :custom
  (org-modules nil)
  (org-ellipsis " ▼")
  (org-startup-folded 'show3levels) ;; 'content also works
  (org-startup-with-inline-images t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "CURRENT(c)" "|" "DONE(d!)" "CANCELLED(x@)")))
  (org-todo-keyword-faces
   '(("CURRENT" . org-current) ("NEXT"    . org-next)))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-clock-idle-time nil)
  (org-agenda-span 14)
  (org-agenda-start-day "today")
  (org-agenda-start-on-weekday nil)
  (org-agenda-restore-windows-after-quit t)
  (org-archive-location "~/.emacs.d/notes/archive.org::* From %s")
  (org-archive-subtree-add-inherited-tags t)
  (org-directory "~/.emacs.d/notes")
  (org-agenda-show-inherited-tags t)
  (org-agenda-sorting-strategy '(todo-state-down priority-up))
  (org-agenda-files '("~/.emacs.d/notes"))
  (org-agenda-prefix-format
   '((agenda . " %i %?-12t% s")
     (todo . " %i ")
     (tags . " %i ")
     (search . " %i ")))
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
              ("C-c RET" . org-insert-todo-heading)
              ("C-c r" . org-archive-subtree))
  :hook ((org-mode . org-indent-mode)
         (org-mode . variable-pitch-mode)
         (org-after-todo-state-change . +org-clock-todo-change))
  :config
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

(defun +org-clock-todo-change ()
  (cond ((string= org-state "CURRENT") (org-clock-in))
        ((org-clocking-p) (org-clock-out))))

(use-package newsticker
  :ensure nil
  :custom
  (newsticker-retrieval-interval 1800)
  (newsticker-wget-name "curl")
  (newsticker-wget-arguments '("--silent" "--location"))
  (newsticker-obsolete-item-max-age (* 3 86400))
  (newsticker-frontend 'newsticker-treeview)
  (newsticker-start-news-ticker-on-start nil)
  :config
  (setq newsticker-url-list
        '(("Hacker News" "https://hnrss.org/frontpage" nil nil nil)
          ("Guardian World" "https://www.theguardian.com/world/rss" nil nil nil))))
