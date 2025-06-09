;; Emacs Configuration ---------------------------------------------------------

(setq inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore
      custom-file (concat user-emacs-directory "custom.el")
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      isearch-wrap-pause 'no-ding
      use-short-answers t
      vc-follow-symlinks t
      find-file-visit-truename t
      split-height-threshold 80
      split-width-threshold 160
      sql-input-ring-file-name (concat user-emacs-directory "sql-history")
      org-src-preserve-indentation t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-window-setup 'current-window
      org-export-with-section-numbers nil
      org-startup-with-inline-images t
      display-buffer-alist
      '(("\\*vc-dir\\*" display-buffer-pop-up-window)))

(setq-default compilation-scroll-output 'first-error
	      compilation-window-height 15
	      display-fill-column-indicator-column 80
	      default-frame-width 160
	      default-frame-height 60
	      truncate-lines t) ;; no word wrap thanks

(dolist (base-mode
	 '(fido-vertical-mode
	   global-auto-revert-mode
	   show-paren-mode
	   save-place-mode
	   electric-pair-mode
	   savehist-mode))
  (funcall base-mode 1))

(when (executable-find "rg")
  (setq grep-program "rg"
	grep-use-null-device nil
	grep-command
	"rg --color=never --no-heading --line-number --max-filesize=300K "))

;; Bindings --------------------------------------------------------------------
(defun +rg (pattern)
  (interactive "sSearch: ")
  (let* ((git-root (or (vc-git-root default-directory)
                       default-directory))
         (default-directory git-root)
         (command (format "rg --color=always --smart-case --no-heading --line-number --column %s ."
                         (shell-quote-argument pattern))))
    (grep command)))

(defun +kill-region-or-backward-word ()
  "Kill region if active, otherwise kill backward word."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (cond ((and (bound-and-true-p paredit-mode)
		(fboundp 'paredit-backward-kill-word))
           (paredit-backward-kill-word))
          (t (backward-kill-word 1)))))

(defun +minibuffer-kill-backward ()
  "Kill backward word, or go up directory if in file completion."
  (interactive)
  (let* ((metadata (completion-metadata 
                    (buffer-substring-no-properties 
                     (minibuffer-prompt-end) (point))
                    minibuffer-completion-table
                    minibuffer-completion-predicate))
         (category (completion-metadata-get metadata 'category)))
    (if (eq category 'file)
        (icomplete-fido-backward-updir)
      (backward-kill-word 1))))

(defun +scratch (mode) ;; TODO experimental
  "Create a scratch buffer with specified major mode."
  (interactive "aMode: ")
  (let ((buffer (generate-new-buffer "*scratch*")))
    (switch-to-buffer buffer)
    (funcall mode)))

(with-eval-after-load 'icomplete
  (define-key
   icomplete-minibuffer-map (kbd "C-w") #'+minibuffer-kill-backward))

(defun repl ()
  (interactive)
  (other-window-prefix)
  (pcase major-mode
    ('clojure-mode (inferior-lisp "clojure"))
    ('emacs-lisp-mode (ielm))
    ('scheme-mode
     (setq inferior-lisp-prompt "^[0-9]* *\\]=> *")
     (inferior-lisp "scheme"))
    ('sql-mode (sql-connect))
    (_ (message "No REPL defined for %s" major-mode))))

(defun +find-git-tags ()
  "Set tags table to include .git/tags/tags, .git/tags/tags-lib, and .git/tags/tags-std if they exist."
  (let ((git-dir (locate-dominating-file default-directory ".git")))
    (when git-dir
      (let ((tag-files '(".git/tags/tags" ".git/tags/tags-lib" ".git/tags/tags-std"))
            (existing-tags '()))
        ;; Check which tag files actually exist
        (dolist (tag-file tag-files)
          (let ((full-path (expand-file-name tag-file git-dir)))
            (when (file-exists-p full-path)
              (push full-path existing-tags))))
        ;; Set the tags table list if any tag files exist
        (when existing-tags
          (setq-local tags-table-list (reverse existing-tags))
          ;; Set the primary tags file to the first one
          (setq-local tags-file-name (car existing-tags)))))))

(add-hook 'find-file-hook #'+find-git-tags)

(defun +compile ()
  "Compile from directory with build file, before resorting to git root."
  (interactive)
  (let ((build-dir
	 (locate-dominating-file
	  default-directory
          (lambda (dir)
            (or (file-exists-p (expand-file-name "Makefile" dir))
                (file-exists-p (expand-file-name "go.mod" dir))
                (file-exists-p (expand-file-name "package.json" dir)))))))
    (if build-dir
        (let ((default-directory build-dir))
          (call-interactively 'compile))
      (call-interactively 'project-compile))))

(defmacro ff (&rest path)
  `(lambda ()
     (interactive)
     (find-file (concat ,@path))))

(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux (setq x-super-keysym 'meta)))

(dolist (binding `(("M-o" other-window)
		   ("M-O" delete-other-windows)
		   ("C-w" +kill-region-or-backward-word) ("M-K" kill-whole-line)
		   ("M-D" duplicate-line)
		   ("C-;" hippie-expand)
		   ("M-j" (lambda () (interactive) (join-line -1)))
		   ("M-F" toggle-frame-fullscreen)
		   ("M-E" emoji-search) ;; express yourself!
		   ("M-Q" sql-connect) ;; a.k.a query
		   ("M-I" repl)
		   ("M-R" +rg)
		   ("C-j" newline) ;; because electric-indent overrides this
		   ("C-x F" find-file-other-window)
		   ("M-C" org-agenda) ;; a.k.a checklist
		   ("C-c d" sql-connect)
		   ("C-c p" project-find-file)
		   ("M-[" backward-paragraph)
		   ("M-]" forward-paragraph)
		   ("M-P" project-find-file)
		   ("C-c g" vc-dir-root)
		   ("C-c h" vc-region-history) ;; + file history without region
		   ("C-c a" vc-annotate)       ;; a.k.a git blame
		   ("C-h" delete-backward-char)
		   ("M-s" save-buffer)
		   ("M-/" comment-line)
		   ("C-c i" ,(ff user-init-file))
		   ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
		   ("C-c l" ,(ff user-emacs-directory "local.el"))
		   ("C-c P" ,(ff "~/src"))
		   ("C-c m" recompile) ("C-c M" +compile)))
  (global-set-key (kbd (car binding)) (cadr binding)))

(global-set-key (kbd "M-H") help-map)
(global-set-key (kbd "M-S") search-map)

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "R") 'vc-revert))

;; Editing setup ---------------------------------------------------------------
(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
		   (display-line-numbers-mode 1)
		   (display-fill-column-indicator-mode 1)
		   (column-number-mode 1)
		   (hl-line-mode 1)
		   (local-set-key (kbd "M-n") 'forward-paragraph)
		   (local-set-key (kbd "M-p") 'backward-paragraph))))

;; TODO regex to align SQL by keywords (uppercase but not DESC/ASC etc)
;; TODO regex to align SQL entities e.g SELECT this, that, other onto new lines
;; TODO combine these

(define-derived-mode templ-mode prog-mode "Templ"
  "Major mode for editing templ files."
  (add-hook 'after-save-hook 'format-buffer-templ nil t))

(add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-mode))

(defun format-buffer-templ ()
  (interactive)
  (when (buffer-file-name)
    (shell-command (concat "templ fmt " (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(define-key isearch-mode-map (kbd "C-j") #'+isearch-exit-other-end)
(defun +isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit) (goto-char isearch-other-end))


(defun should-center-buffer-p ()
  (memq major-mode '(org-mode markdown-mode)))

(defun center-prose-buffer-margins ()
  (set-window-margins nil 0 0)
  (when (should-center-buffer-p)
    (let* ((char-width-pix (frame-char-width))
           (window-width-pix (window-body-width nil t))  ; t = pixels!
           (target-width-chars 80)
           (target-width-pix (* target-width-chars char-width-pix))
           (margin-total-pix (max 0 (- window-width-pix target-width-pix)))
           (margin-each-pix (/ margin-total-pix 2.0))
           (margin-chars (max 0 (round (/ margin-each-pix char-width-pix)))))
      (set-window-margins nil margin-chars margin-chars))))

(add-hook 'buffer-list-update-hook 'center-prose-buffer-margins)

(defun prose-config ()
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (center-prose-buffer-margins)
  (add-hook 'window-size-change-functions
            (lambda (frame) (center-prose-buffer-margins)) nil t))

(add-hook 'org-mode-hook (lambda ()
			   (prose-config)
			   (org-indent-mode)))

;; Appearance ------------------------------------------------------------------
(defun find-font (names) (seq-find #'x-list-fonts names))
(defconst *default-font* (find-font '("Rec Mono Linear" "Monaco" "Monospace")))
(defconst *writing-font* (find-font '("Rec Mono Casual" "Sans Serif")))

(when *default-font*
  (set-face-attribute 'default nil :font *default-font* :height 160)
  (with-eval-after-load 'org
    (set-face-attribute 'org-block nil :font *default-font*)
    (set-face-attribute 'org-code nil :font *default-font*)
    (set-face-attribute 'org-verbatim nil :font *default-font*)
    (set-face-attribute 'org-table nil :font *default-font*)))
(when *writing-font*
  (set-face-attribute 'variable-pitch nil :font *writing-font* :height 160))

(menu-bar-mode -1)
(tool-bar-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (fringe-mode -1))

(dolist (attr `((alpha (95 . 95))
		(width ,default-frame-width)
		(height ,default-frame-height)))
  (set-frame-parameter (selected-frame) (car attr) (cadr attr)))

(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(ignore-errors (load-theme 'flow t))

;; Major mode (experiment to reduce 3rd party dependency)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
;; For YAML, use text-mode with whitespace visualization
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . text-mode))
(add-hook 'text-mode-hook 
          (lambda () 
            (when (string-match "\\.ya?ml\\'" (buffer-name))
              (whitespace-mode 1))))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-mode))

;; Packages (mostly just language major-modes) ---------------------------------
(add-to-list 'load-path "~/.emacs.d/external-modes/")

;; Autoload the modes (only loads when actually used)
(autoload 'clojure-mode "clojure-mode" "Major mode for Clojure" t)
(autoload 'go-mode "go-mode" "Major mode for Go" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown" t)
(autoload 'paredit-mode "paredit" "Minor mode for balanced parentheses" t)

;; Auto-mode associations (triggers autoload when opening files)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Configure only when modes are actually loaded
(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'subword-mode))

(with-eval-after-load 'go-mode
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default gofmt-command "goimports"
                gofmt-show-errors 'echo))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'prose-config))

(with-eval-after-load 'paredit
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'lisp-data-mode-hook 'paredit-mode)
  
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-RET)
  (define-key paredit-mode-map (kbd "RET") 'paredit-C-j)
  (define-key paredit-mode-map (kbd "M-k") 'paredit-forward-barf-sexp))

;; Local files -----------------------------------------------------------------
(load custom-file t)
(load (concat user-emacs-directory "local.el") t)
