;; -- Spartan Emacs configuration ------------------ -*- lexical-binding: t; -*-

(defun +pkg-init ()
  "Initialize package.el for installing packages (idempotent via package--initialized)"
  (unless (boundp 'package--initialized)
    (require 'package)
    (setq package-archives
          (if-let ((mirror (getenv "EMACS_PACKAGE_MIRROR")))
              `(("local" . ,mirror))
            '(("melpa" . "https://melpa.org/packages/")
              ("gnu" . "https://elpa.gnu.org/packages/"))))
    (package-initialize)))

(defmacro +pkg (name &rest rest)
  "Install NAME from package.el with optional :modes and :config.
:modes is a list of (mode-function . file-pattern) pairs
:config contains forms to run after package loads"
  (let (modes config)
    (while rest
      (cond
        ((eq (car rest) :modes)
         (setq modes (cadr rest))
         (setq rest (cddr rest)))
        ((eq (car rest) :config)
         (setq config (cdr rest))
         (setq rest nil))
        (t
         (setq rest (cdr rest)))))

    `(progn
       (unless (package-installed-p ',name)
         (+pkg-init)
         (unless package-archive-contents
           (package-refresh-contents))
         (package-install ',name))
       (require ',name)
       ,@(when modes
           (mapcar (lambda (pair)
                     `(autoload ',(car pair) ,(symbol-name name) nil t))
                   modes))
       ,@(when modes
           (mapcar (lambda (pair)
                     `(add-to-list 'auto-mode-alist '(,(cdr pair) . ,(car pair))))
                   modes))
       ,@(when config
           `((with-eval-after-load ',name ,@config))))))

(defconst +project-definitions
  '("Makefile" "gradlew" "pom.xml" "go.mod" "package.json" "deps.edn" ".git"))

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      auto-save-default nil
      create-lockfiles nil
      use-short-answers t
      search-wrap-around t
      frame-resize-pixelwise t ;; do not maximise after leaving fullscreen
      split-height-threshold 80 split-width-threshold 160 ;; needed fs laptop
      custom-file (concat user-emacs-directory "custom.el")
      whitespace-style '(face trailing tabs empty indentation::space)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "saves")))
      org-hide-emphasis-markers t
      org-export-with-section-numbers nil ;; essential for exporting
      display-buffer-alist '(("\\*vc-dir\\*" display-buffer-pop-up-window))
      vc-dir-hide-up-to-date t)

(setq-default cursor-in-non-selected-windows nil
              display-fill-column-indicator-column 80
              indent-tabs-mode nil tab-width 2 standard-indent 2
              truncate-lines t) ;; no word wrap thanks

(dolist (mode '(fido-vertical-mode
                global-auto-revert-mode show-paren-mode
                save-place-mode electric-pair-mode savehist-mode))
  (funcall mode 1)) ;; enable these

(load custom-file t)
(load (concat user-emacs-directory "local.el") t)

(defun +add-to-list (dst src)
  (set dst (cl-union (eval dst) src :test 'equal)))

(with-eval-after-load 'grep
  (+add-to-list 'grep-find-ignored-directories '("node_modules" ".git"))
  (+add-to-list 'grep-find-ignored-files '("*.min.js" "*.bundle.js" "tags")))

(when (require 'ansi-color nil)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;; -- Bindings -----------------------------------------------------------------
(defun +kill-region-or-backward-word ()
  (interactive)
  (cond ((region-active-p) (call-interactively #'kill-region))
        ((bound-and-true-p paredit-mode) (paredit-backward-kill-word))
        (t (backward-kill-word 1))))

(defun +minibuffer-C-w ()
  (interactive)
  (if (string-match-p "/" (minibuffer-contents))
      (icomplete-fido-backward-updir) (backward-kill-word 1)))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w") #'+minibuffer-C-w)
  (define-key icomplete-minibuffer-map (kbd "C-e") #'icomplete-ret))

(defmacro +with-context (&rest body)
  `(let ((default-directory
          (or (cl-some (lambda (f) (locate-dominating-file default-directory f))
                       +project-definitions)
              default-directory)))
     ,@body))

(defun +repl ()
  (interactive)
  (other-window-prefix)
  (+with-context (pcase major-mode
           ((or 'clojure-mode 'edn-mode) (inferior-lisp "clojure -A:dev"))
           ('emacs-lisp-mode (ielm))
           ('scheme-mode (inferior-lisp "scheme"))
           ('sh-mode (shell))
           (_ (message "No REPL defined for %s" major-mode)))))

(advice-add 'icomplete--fido-mode-setup :after ;; fido match whole then partial
            (lambda ()
              (setq-local completion-styles '(substring flex))))

(defun +find-file ()
  (interactive)
  (+with-context
   (let* ((use-git (= 0 (call-process "git" nil nil nil "rev-parse" "--git-dir")))
          (cmd (if use-git "git ls-files" "find . -type f"))
          (files (split-string (shell-command-to-string cmd) "\n" t)))
     (find-file (completing-read
                 (format "Find file in %s: " default-directory)
                 files)))))

(defun +compile () (interactive) (+with-context (call-interactively 'compile)))
(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux (setq x-super-keysym 'meta)))

(dolist (binding `(("C-c g" vc-dir-root) ("C-c h" vc-region-history)
                   ("C-c l" vc-print-root-log)
                   ("C-c t" +generate-tags)
                   ("C-c i" ,(ff user-init-file))
                   ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
                   ("C-c p" +find-file) ("C-c P" ,(ff "~/src"))
                   ("C-." repeat) ("M-z" zap-up-to-char)
                   ("C-c ;" comment-region)
                   ("C-c C-l" flycheck-list-errors)
                   ("C-h" delete-backward-char) ("C-j" newline) ;; autoindents
                   ("C-w" +kill-region-or-backward-word) ("C-;" dabbrev-expand)
                   ("M-e" ,(il (select-window (or (split-window-sensibly)
                                                  (split-window)))))
                   ("M-RET" toggle-frame-fullscreen)
                   ("M-I" (lambda (pattern) (interactive "sSearch: ")
                            (+with-context (rgrep pattern "*" "."))))
                   ("M-M" delete-other-windows)
                   ("M-L" split-window-below)
                   ("M-P" split-window-right)
                   ("M-D" duplicate-line) ("M-K" kill-whole-line)
                   ("M-R" +repl) ("M-Q" sql-connect)
                   ("M-B" ,(il (other-window-prefix) (shell)))
                   ("M-j" ,(il (join-line -1)))
                   ("M-s" save-buffer) ("M-/" replace-string)
                   ("M-o" other-window) ("M-O" delete-other-windows)
                   ("C-c m" recompile)  ("C-c M" +compile)
                   ("M-n" forward-paragraph) ("M-p" backward-paragraph)
                   ("M-H" ,help-map) ("C-c C-s" ,search-map)))
  (global-set-key (kbd (car binding)) (cadr binding)))

;; -- Editing setup ------------------------------------------------------------
(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
           (display-line-numbers-mode 1) (column-number-mode 1)
           (auto-revert-mode 1)
           (display-fill-column-indicator-mode 1) (hl-line-mode 1))))

(defun center-prose ()
  (set-window-margins nil 0 0)
  (let ((margin (max 0 (round (/ (- (window-body-width nil t)
                                    (* 80 (frame-char-width)))
                                 2.0 (frame-char-width))))))
    (set-window-margins nil margin margin)))

(defun prose-config ()
  (visual-line-mode 1)
  (add-hook 'window-size-change-functions #'center-prose nil t)
  (add-hook 'buffer-list-update-hook #'center-prose nil t))

(add-hook 'org-mode-hook (lambda () (prose-config) (org-indent-mode)))
(add-hook 'before-save-hook 'whitespace-cleanup)

;; -- Appearance ---------------------------------------------------------------
(dolist (ui-mode
         '(menu-bar-mode tool-bar-mode blink-cursor-mode))
  (funcall ui-mode -1)) ;; disable these

(when window-system
  (scroll-bar-mode -1) (fringe-mode -1)
  (defconst *font* (seq-find #'x-list-fonts '("Rec Mono Casual" "Monospace")))
  (set-face-attribute 'default nil :font *font* :height 160))

(dolist (attr `((alpha (95 . 95)) (width 100) (height 60)))
  (set-frame-parameter (selected-frame) (car attr) (cadr attr)))

(ignore-errors (load-theme 'flow t))

;; -- Languages ----------------------------------------------------------------

(+pkg clojure-mode
  :modes ((clojure-mode . "\\.clj\\'")
          (clojurescript-mode . "\\.cljs\\'")
          (clojurec-mode . "\\.cljc\\'")
          (edn-mode . "\\.edn\\'"))
  :config
  (add-hook 'clojure-mode-hook 'subword-mode)
  (define-key clojure-mode-map (kbd "C-x C-e")
    (lambda () (interactive)
      (if (region-active-p) (lisp-eval-region (region-beginning) (region-end))
        (lisp-eval-last-sexp))))
  (define-key clojure-mode-map (kbd "C-c C-k")
    (lambda () (interactive) (lisp-eval-region (point-min) (point-max))))

  (defun +clojure-tag ()
    "Strip namespace alias and hiccup selectors from symbol at point for tag lookup."
    (let ((sym (find-tag-default)))
      (when sym
        (setq sym (replace-regexp-in-string "^[^/]+/" "" sym))  ;; namespace (foo/bar → bar)
        (setq sym (replace-regexp-in-string "^:[^.]*\\." "" sym))  ;; hiccup (:div.class → class)
        sym)))

  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local find-tag-default-function #'+clojure-tag))))

(+pkg go-mode
  :modes ((go-mode . "\\.go\\'"))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default gofmt-command "goimports" gofmt-show-errors 'echo))

(+pkg markdown-mode
  :modes ((markdown-mode . "\\.md\\'"))
  :config
  (add-hook 'markdown-mode-hook 'prose-config))

(+pkg csv-mode
  :modes ((csv-mode . "\\.csv\\'"))
  :config
  (add-hook 'csv-mode-hook (lambda ()
                             (csv-align-mode 1)
                             (csv-header-line 1))))

(+pkg rust-mode
  :modes ((rust-mode . "\\.rs\\'"))
  :config
  (add-hook 'rust-mode-hook 'subword-mode)
  (add-hook 'before-save-hook
            (lambda () (when (eq major-mode 'rust-mode)
                         (ignore-errors (rust-format-buffer))))))

(+pkg flycheck
  :config
  (global-flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; (+pkg citre)

;; (+pkg typescript-mode
;;   :modes ((typescript-mode . "\\.tsx?\\'"))
;;   :config
;;   (add-hook 'typescript-mode-hook 'subword-mode))

;; (+pkg java-mode
;;   :modes ((java-mode . "\\.java\\'"))
;;   :config
;;   (add-hook 'java-mode-hook 'subword-mode))

;; Built-in modes
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

;; -- Paredit ------------------------------------------------------------------
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

(+pkg paredit
  :config
  (dolist (hook '(clojure-mode-hook
                  emacs-lisp-mode-hook
                  inferior-lisp-mode-hook lisp-data-mode-hook
                  eval-expression-minibuffer-setup-hook))
    (add-hook hook #'enable-paredit-mode))

  (dolist (binding '(("C-j" +paredit-RET) ("M-r" +paredit-M-r)
                     ("M-k" paredit-forward-barf-sexp) ("M-s" nil)
                     ("M-l" paredit-forward-slurp-sexp)))
    (define-key paredit-mode-map (kbd (car binding)) (cadr binding))))

;; -- External Configuration ---------------------------------------------------

(defconst +external-packages
    '((git :mac git :arch git)
      (jq :mac jq :arch jq)
      (yq :mac yq :arch go-yq)
      (shellcheck :mac shellcheck :arch shellcheck)
      (python :mac python :arch python)
      (kubectl :mac kubectl :arch kubectl)
      (cwebp :mac libwebp-utils :arch libwebp-utils)
      (rg :mac ripgrep :arch ripgrep)
      (ctags :mac universal-ctags :arch ctags)))

(defun +external-package-sync ()
  (interactive)
  (let ((packages '()))
    (dolist (binding +external-packages)
      (let ((cmd (car binding))
            (pkg (pcase system-type
                   ('darwin (plist-get (cdr binding) :mac))
                   ('gnu/linux (plist-get (cdr binding) :arch)))))
        (when (and pkg (not (executable-find (symbol-name cmd))))
          (push pkg packages))))

    (pcase system-type
      ('darwin (async-shell-command (format "brew install %s"
                                            (mapconcat
                                             #'symbol-name packages " "))))
      ('gnu/linux (async-shell-command (format "sudo pacman -Syu %s"
                                               (mapconcat
                                                #'symbol-name packages " ")))))))

;; -- Tags ---------------------------------------------------------------------

(defun +generate-tags ()
    "Generate ctags for your current project."
    (interactive)
    (+with-context
     (let ((project-root (expand-file-name default-directory)))
       (if (file-exists-p "deps.edn")
           (+generate-tags-clojure project-root)
         (+run-ctags-project project-root)))))


(defun +generate-tags-clojure (project-root)
  "Generate tags for a Clojure project with deps.edn."
  (+generate-tags-clojure-deps
   (lambda (jars)
     (let* ((project-name (file-name-nondirectory (directory-file-name project-root)))
            (target-dir (expand-file-name (format "~/.local/src/%s" project-name))))
       (unless (file-exists-p target-dir)
         (make-directory target-dir t))
       (+extract-jars jars target-dir
                      (lambda ()
                        (+run-ctags-clojure project-root target-dir)))))))

  (defun +run-ctags-clojure (project-root extracted-dir)
    "Run ctags on Clojure sources (project src + extracted jars)."
    (let ((tags-file (expand-file-name (format "~/.emacs.d/.tag-store/%s.tags"
                                                (file-name-nondirectory (directory-file-name project-root))))))
      (unless (file-exists-p (file-name-directory tags-file))
        (make-directory (file-name-directory tags-file) t))
      (make-process
       :name "clojure-ctags"
       :buffer (get-buffer-create "*clojure-ctags*")
       :command `("ctags" "--output-format=etags"
                  "-f" ,tags-file
                  "-R"
                  "--languages=clojure,css"
                  "--regex-css=/\\.([A-Za-z0-9_-]+) *[,{]/\\1/c,class/"
                  "--regex-css=/^[ \t]+\\.([A-Za-z0-9_-]+) *[,{]/\\1/c,class/"
                  "--regex-css=/,[[:space:]]*\\.([A-Za-z0-9_-]+)/\\1/c,class/"
                  "--regex-clojure=/\\(def[^n][[:space:]]+([^[:space:]]+)/\\1/d,def/"
                  "--regex-clojure=/\\(defn-?[[:space:]]+([^[:space:]]+)/\\1/f,defn/"
                  "--regex-clojure=/\\(defmacro[[:space:]]+([^[:space:]]+)/\\1/m,macro/"
                  "--regex-clojure=/\\(defmulti[[:space:]]+([^[:space:]]+)/\\1/M,multi/"
                  "--regex-clojure=/\\(defrecord[[:space:]]+([^[:space:]]+)/\\1/r,record/"
                  "--regex-clojure=/\\(defprotocol[[:space:]]+([^[:space:]]+)/\\1/p,protocol/"
                  "--regex-clojure=/\\(ns[[:space:]]+([^[:space:]]+)/\\1/n,namespace/"
                  ,project-root
                  ,extracted-dir)
       :sentinel (lambda (_ event)
                   (when (string-match-p "finished" event)
                     (message "Clojure tags generated at %s" tags-file))))))

(defun +generate-tags-clojure-deps (on-jars)
  "Get classpath, filter .jars, pass to ON-JARS callback."
  (make-process
   :name "clj-spath"
   :buffer (get-buffer-create "*clj-spath*")
   :command '("clojure" "-Spath")
   :sentinel (lambda (proc event)
               (when (string= event "finished\n")
                 (with-current-buffer (process-buffer proc)
                   (let* ((paths (split-string (string-trim (buffer-string)) ":"))
                          (jars (delete-dups
                                 (seq-filter (lambda (p) (string-suffix-p ".jar" p)) paths))))
                     (funcall on-jars jars)))))))

(defun +extract-jars (jars target-dir on-complete)
  "Extract JARS to TARGET-DIR in parallel, call ON-COMPLETE when all done."
  (let ((remaining (length jars)))
    (dolist (jar jars)
      (make-process
       :name (format "jar-extract-%s" (file-name-nondirectory jar))
       :command `("sh" "-c" ,(format "cd %s && jar xf %s" target-dir jar))
       :sentinel (lambda (_ event)
                   (when (string-match-p "finished" event)
                     (message "Extracted: %s" jar)
                     (setq remaining (1- remaining))
                     (when (= remaining 0)
                       (funcall on-complete))))))))

(defun +run-ctags-project (project-root)
  "Run ctags on project sources (all languages)."
  (let ((tags-file (expand-file-name (format "~/.emacs.d/.tag-store/%s.tags"
                                              (file-name-nondirectory (directory-file-name project-root))))))
    (ignore-errors (make-directory (file-name-directory tags-file) t))
    (make-process
     :name "ctags"
     :buffer (get-buffer-create "*ctags*")
     :command `("ctags" "--output-format=etags"
                "-f" ,tags-file
                "-R"
                "--languages=all"
                ,project-root)
     :sentinel (lambda (_ event)
                 (when (string-match-p "finished" event)
                   (message "Tags generated at %s" tags-file))))))

(defun +setup-project-tags ()
  "Load tags file for current project if it exists."
  (when-let ((project-root (cl-some (lambda (f) (locate-dominating-file default-directory f))
                                    +project-definitions)))
    (let ((tags-file (expand-file-name
                      (format "~/.emacs.d/.tag-store/%s.tags"
                              (file-name-nondirectory (directory-file-name project-root))))))
      (when (file-exists-p tags-file)
        (setq-local tags-table-list (list tags-file))
        (visit-tags-table tags-file t)))))

  (add-hook 'find-file-hook #'+setup-project-tags)

(provide 'init)
;;; init.el ends here
