;;; Spartan Emacs -binding: t; -*-

;;  Consider newsticker
;;  Consider your own olivetti again.
;;  Consider popper-mode.
;;  Consider grouping C-x C-b buffer list.
;;  Consider using eglot+flymake instead of flycheck+ctags
;;  Consider.. nerd tree kinda thing
;;  Also.. consider emacs wayland lol - ye it's good, now try pixel-scroll-precision-mode
;;  Refine keybindings.. C-c f for imenu is.. fine but could be really cool to jump around.
;;  Consider moving temp emacs files into .cache or similar inside emacs

;;; Configuration:

;; possibly
;;(project-vc-extra-root-markers '("Cargo.toml" "package.json" "go.mod")) ; Excelent for mono repos with multiple langs, makes Eglot happy

;;; 🔱 System (Emacs defaults, UI, system level configuration)

(setq gc-cons-threshold (* 64 1024 1024) ; 64MB
      gc-cons-percentage 0.2)

(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

(dolist (meta-key '(mac-command-modifier x-super-keysym))
  (when (boundp meta-key)
    (set meta-key 'meta)))

(use-package emacs
  :custom
  (inhibit-startup-screen t)
  (split-width-threshold 170)
  (split-height-threshold nil)
  (ring-bell-function 'ignore)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (frame-resize-pixelwise t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (scroll-margin 5)
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "saves"))))
  (large-file-warning-threshold (* 512 1024 1024)) ; 500MB
  (custom-file (concat user-emacs-directory "local.el"))
  (auth-sources (list (concat user-emacs-directory ".authinfo.gpg")))
  (package-archives (if-let ((mirror (getenv "EMACS_PACKAGE_MIRROR")))
                        `(("local" . ,mirror))
                      '(("melpa" . "https://melpa.org/packages/")
                        ("gnu" . "https://elpa.gnu.org/packages/"))))
  :config
  (modify-coding-system-alist 'file "" 'utf-8)
  (load custom-file t)

  (menu-bar-mode -1)
  (when window-system
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (fringe-mode 0) ;; not actually a minor mode!
    (pixel-scroll-precision-mode 1)

    (set-face-attribute
     'default nil
     :font (seq-find #'x-list-fonts '("Rec Mono Casual" "Monospace"))
     :height 160))
  (add-hook 'before-save-hook #'whitespace-cleanup)

  (dolist (mode ;; global minor modes
         '(fido-vertical-mode
           global-auto-revert-mode
           save-place-mode
           global-goto-address-mode
           savehist-mode
           show-paren-mode
           electric-pair-mode))
  (funcall mode 1))
  :bind
  ;; Useful defaults
  ;; "C-x C-;" => comment-line (also does uncomment + region)

  ("C-c i" . ,(ff user-emacs-directory "init.el"))
  ("C-c n" . ,(ff user-emacs-directory "notes/index.org"))
  ("C-c p" . project-find-file)
  ("C-c P" . ,(ff "~/src"))
  ("C-c f" . imenu)
  ("M-RET" . toggle-frame-fullscreen)
  ("M-H" . ,help-map)
  ("C-c a" . ,(il (org-agenda nil "a")))
  ("C-z" . nil)
  ("M-_" . gptel)

  ;; Split management
  ("C-c k" . ,(il (select-window (split-window-below))))
  ("C-c l" . ,(il (select-window (split-window-right))))
  ("C-c o" . delete-other-windows)
  ("M-o" . ,(il (other-window 1)))
  ("M-O" . ,(il (other-window -1)))

  ;; Code Tools
  ("M-i" . rgrep)
  ("C-c g" . vc-dir-root)
  ;; ("C-c C-g" vc-print-root-log) ;; C-g should always exit lol
  ("M-T" . eshell)
  ("M-R" . +repl)

  ;; Editing
  ("M-/" . replace-string)
  ("M-n" . forward-paragraph) ;; more ergo than M-{} but not that easy
  ("M-p" . backward-paragraph)
  ("C-w" . +kill-region-or-backward-word)
  ("M-K" . kill-whole-line)
  ("C-h" . delete-backward-char)
  ("C-j" . newline) ;; autoindents
  ("M-j" . ,(il (join-line -1)))
  ("M-s" . save-buffer)
  ("M-D" . duplicate-line)

  ("C-c m" . recompile)  ("C-c M" +compile)
  ("C-;" . completion-at-point)
  ("C-c C-s" . ,search-map))

(defun +minibuffer-C-w ()
    (interactive)
    (if (string-match-p "/" (minibuffer-contents))
        (icomplete-fido-backward-updir) (backward-kill-word 1)))

(use-package icomplete
  :ensure nil
  :bind (:map icomplete-minibuffer-map
              ("C-w" . '+minibuffer-C-w)
              ("C-e" . #'icomplete-ret)))

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

(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.1))


;;; 🖋️ Editing (specific to writing text)

(use-package prog-mode
  :hook
  ((prog-mode . (lambda ()
                  (display-line-numbers-mode 1) (column-number-mode 1)
                  (completion-preview-mode 1)
                  (display-fill-column-indicator-mode 1) (hl-line-mode 1)))))

(setq isearch-wrap-pause 'no
      completion-ignore-case t)

(setq-default ;; TODO do these need to be setq-defaults?
 truncate-lines t
 indent-tabs-mode nil
 tab-width 2
 standard-indent 2
 display-fill-column-indicator-column 80
 whitespace-style '(face trailing tabs empty indentation::space))

(defun +kill-region-or-backward-word ()
  (interactive)
  (cond ((region-active-p) (call-interactively #'kill-region))
        ((bound-and-true-p paredit-mode) (paredit-backward-kill-word))
        (t (backward-kill-word 1))))

(use-package paredit :ensure t
  :hook ((clojure-mode
          emacs-lisp-mode
          inferior-lisp-mode lisp-data-mode
          eval-expression-minibuffer-setup)
         . #'enable-paredit-mode)
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

(use-package clojure-mode :ensure t)
(use-package typescript-mode :ensure t :custom (typescript-indent-level 2))
(use-package json-mode :ensure t)
(use-package js :ensure nil :custom (js-indent-level 2))
(use-package markdown-mode :ensure t :custom (markdown-hide-markup t))

;;; 🛠️ Tools (augmenting code/editing

(use-package outline
  :bind (:map outline-minor-mode-map
              ("M-[" . outline-cycle)
              ("M-]" . outline-cycle-buffer))
  :hook ((emacs-lisp-mode . outline-minor-mode)) ;; TODO prog-mode? with other regexps?
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (when (equal (buffer-file-name) user-init-file)
                (outline-hide-body)))))

(use-package compile :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t))

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
  (remove-hook 'log-edit-hook 'log-edit-show-files))

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

;;; UNSORTED -----------------------------------

;;; Packages:

(use-package project :ensure nil) ;; to remove (eventually)
(use-package olivetti :ensure t ;; to replace (eventually)
  :custom
  (olivetti-style nil)
  (olivetti-body-width 80)
  :hook ((org-mode
          markdown-mode)
         . olivetti-mode))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah")
  (dired-kill-when-opening-new-dired-buffer t))

(use-package eshell
  :ensure nil
  :custom
  (eshell-visual-commands '("vi" "vim" "nvim" "top" "htop" "less" "more" "psql" "sqlite3" "w3m"))
  (eshell-banner-message ""))

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

;; (add-hook 'find-file-hook #'+project-tags-load) ;; slow AF to run on every file change.

(load-theme 'flow t)

(defun +local-llm ()
  (interactive)
  (make-process
   :name "local-llm"
   :buffer (get-buffer-create "*local-llm*")
   :command `("llama-server"
              "-m" ,(expand-file-name "~/models/gemma-4-E2B-it-UD-Q4_K_XL.gguf")
              "--host" "127.0.0.1"
              "--port" "7777"
              "-c" "32768"
              "--metrics"
              "-ngl" "1"
              "-np" "1"
              "--temp" "0.7"
              "--top-p" "0.95"
              "--top-k" "64")))

(use-package gptel
  :ensure t
  :custom
  gptel-directives
  `((default . ,(lambda ()
                  (with-temp-buffer
                    (insert-file-contents
                     (concat user-emacs-directory "assistant.md"))
                    (buffer-string)))))
  :config
  (gptel-make-openai "llama.cpp"
    :host "127.0.0.1:7777"
    :protocol "http"
    :endpoint "/v1/chat/completions"
    :stream t
    :models '(gemma4-e2b))

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

  (setq
   gptel-backend (gptel-get-backend "llama.cpp")
   gptel-model  'gemma4-e2b)

  (add-hook 'gptel-mode-hook #'visual-line-mode))

(defconst *repl-table*
  '((python-mode      run-python)
    (ruby-mode        comint-run "irb")
    (emacs-lisp-mode  ielm)
    (edn-mode         inferior-lisp "clojure -A:dev")
    (clojure-mode     inferior-lisp "clojure -A:dev")))

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

(use-package org :ensure nil
  :custom
  (org-modules nil) ;; don't load the world
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
  (org-refile-targets
   '((nil :maxlevel . 3)
     ("~/.emacs.d/notes/archive.org" :maxlevel . 3)))
  ;; Show full path in completion (e.g. "Projects/Work/Task" not just "Task")
  (org-refile-use-outline-path t)
  ;; Required when using outline-path — lets completing-read handle the full path
  (org-outline-path-complete-in-steps nil)
  ;; Allow creating new parent nodes on refile
  (org-refile-allow-creating-parent-nodes 'confirm)
  :bind (:map org-mode-map
              ("M-RET" . nil)
              ("C-c r" . +archive-task-at-point)
              ;; ("M-RET" . org-meta-return) ;; TODO needs new binding
              )
  :hook ((org-mode . org-indent-mode)
         (org-after-todo-state-change . +org-clock-todo-change))
  :config
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

(defvar +archive-file "~/.emacs.d/notes/archive.org")

(defun +archive-task-at-point ()
  "Archive the heading at point to archive.org under the same project."
  (interactive)
  (let* ((project (save-excursion
                    (while (and (> (org-current-level) 3) (org-up-heading-safe)))
                    (when (= (org-current-level) 3)
                      (org-get-heading t t t t))))
         (archive-file (expand-file-name +archive-file))
         (pos (when project
                (with-current-buffer (find-file-noselect archive-file)
                  (org-find-exact-headline-in-buffer project)))))
    (if (not project)
        (user-error "No level-3 project ancestor found")
      (org-refile nil nil (list project archive-file nil pos))
      (message "Archived under: %s" project))))

(defun +org-clock-todo-change ()
  (if (string= org-state "CURRENT")
      (org-clock-in)))


(defconst *context-identifiers*
  '("Makefile" "gradlew" "pom.xml" "go.mod" "package.json" "deps.edn" ".git"))


(defmacro +with-context (&rest body)
  `(let ((default-directory
          (or (seq-some (lambda (f) (locate-dominating-file default-directory f))
                       *context-identifiers*)
              default-directory)))
     ,@body))

(defun +compile ()
  (interactive)
  (+with-context (call-interactively 'compile)))

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
  (when-let ((f (locate-dominating-file default-directory ".tags")))))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'spartan-ai)

(provide 'init)
;;; init.el ends here
