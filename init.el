;;; 🔱 Spartan Emacs -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 64 1024 1024))
(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

(dolist (k '(mac-command-modifier x-super-keysym))
  (when (boundp k) (set k 'meta)))

(modify-coding-system-alist 'file "" 'utf-8)

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
 create-lockfiles nil ;; Save files
 make-backup-files nil
 isearch-wrap-pause 'no ;; Editing
 large-file-warning-threshold (* 512 1024 1024) ;; for ctags
 compilation-always-kill t
 compilation-scroll-output t
 ansi-color-for-compilation-mode t ;; does this work when set this way? check.
 vc-handled-backends '(Git)
 vc-make-backup-files nil
 eshell-banner-message ""
 eshell-visual-commands '("vi" "htop" "less" "more")
 custom-file (concat user-emacs-directory "local.el")
 initial-frame-alist (append initial-frame-alist '((width . 160) (height . 60)))
 package-archives '(("melpa" . "https://melpa.org/packages/")
                    ("gnu" . "https://elpa.gnu.org/packages/")))

(setq-default ;; Editing
 truncate-lines t
 indent-tabs-mode nil
 tab-width 2
 standard-indent 2
 whitespace-style '(face trailing tabs empty indentation::space))

(load custom-file t)
(load-theme 'flow t)

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0)

  (defmacro with-font (var font &rest body)
    `(if (member ,font (font-family-list))
       (let ((,var ,font)) ,@body)
       (message "%s font not found, assuming defaults" ,font)))

  (with-font
   mono "Rec Mono Casual"
   (set-face-attribute 'default nil :font mono :height 160)
   (set-face-attribute 'fixed-pitch nil :font mono :height 140))

  (with-font
   variable "Recursive Sans Casual Static"
   (set-face-attribute 'variable-pitch nil :font variable) :height 160))

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
             ("C-c G" vc-print-root-log)
             ("C-c p" project-find-file)
             ("C-c P" ,(ff "~/src"))
             ("M-_" ,(il (if (org-clocking-p) (org-clock-out) (org-clock-in-last))))

             ;; Information
             ("M-N" newsticker-show-news)

             ;; Split management
             ("C-c k" ,(il (select-window (split-window-below))))
             ("C-c l" ,(il (select-window (split-window-right))))
             ("C-c o" delete-other-windows)
             ("M-o" ,(il (other-window 1)))
             ("M-O" ,(il (other-window -1)))

             ("M-L" flow-toggle-theme)
             ("M-V" ,(il (load-theme 'flow t))) ;; temporary for theme work

             ("C-c f" imenu)
             ("M-RET" toggle-frame-fullscreen)
             ("M-H" ,help-map)
             ("C-c t" +ctags)
             ("M-i" ,(il (+with-context (call-interactively 'rgrep))))
             ("M-I" ,(il (+with-context (call-interactively 'occur))))

             ("M-T" eshell)
             ("M-R" ,(il (+launch-repl "clojure")))
             ("M-Q" ,(il (+launch-repl)))

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
             ("C-c m" recompile)
             ("C-c M" ,(il (+with-context (call-interactively 'compile))))))
  (global-set-key (kbd (car b)) (cadr b)))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w")
    (lambda () (interactive)
      (if (string-match-p "/" (minibuffer-contents))
          (icomplete-fido-backward-updir) (backward-kill-word 1))))
  (define-key icomplete-minibuffer-map (kbd "C-e") #'icomplete-ret))

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

(defvar +repls
  '(("python"        run-python)
    ("ruby"          comint-run "irb")
    ("node"          comint-run "node")
    ("racket"        comint-run "racket")
    ("sqlite"        sql-sqlite)
    ("elisp"         ielm)
    ("clojure"       inferior-lisp "clojure -A:dev")
    ("cljs"          inferior-lisp "clojure -M:cljs")
    ("bb"            inferior-lisp "bb"))
  "REPL specs for `+launch-repl'. Each entry is (NAME FN &optional ARGS...).")

(defun +launch-repl (&optional repl)
  "Launch a REPL. REPL is a key from `+repls'; if nil, prompt via completing-read."
  (interactive)
  (let* ((choice (or repl (completing-read "REPL: " (mapcar #'car +repls) nil t)))
         (spec   (cdr (assoc choice +repls)))
         (fn     (car spec))
         (args   (cdr spec)))
    (other-window-prefix)
    (apply fn args)))

(defun +lisp-load-current-file ()
  (interactive)
  (lisp-load-file (buffer-file-name)))

(defun +toggle-transparency ()
  (interactive)
  (let* ((current (frame-parameter nil 'alpha-background))
         (new-alpha (if (= current 100) 60 100)))
    (set-frame-parameter (selected-frame) 'alpha-background new-alpha)
    (setf (alist-get 'alpha-background default-frame-alist) new-alpha)))

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

(use-package olivetti :ensure t ;; to replace (eventually)
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
  (org-clock-idle-time 10)
  (org-agenda-span 14)
  (org-agenda-start-day "today")
  (org-agenda-start-on-weekday nil)
  (org-agenda-restore-windows-after-quit t)
  (org-archive-location "~/.emacs.d/notes/archive.org::* From %s")
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
  :hook (                               ; (org-mode . org-indent-mode)
         (org-mode . variable-pitch-mode)
         ;; (org-mode . (lambda () (setq-local mode-line-format nil)))
         (org-after-todo-state-change . +org-clock-todo-change))
  :config
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

(defun +org-clock-todo-change ()
  (if (string= org-state "CURRENT")
      (org-clock-in)))

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
