;; -- Spartan Emacs Configuration, never more than 300 lines, sometimes less. --

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      custom-file (concat user-emacs-directory "custom.el")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "saves")))
      create-lockfiles nil
      use-short-answers t
      vc-follow-symlinks t
      split-height-threshold 80
      split-width-threshold 160
      sql-input-ring-file-name (concat user-emacs-directory "sql-history")
      org-export-with-section-numbers nil ;; essential for exporting
      org-ellipsis " ▼"
      org-hide-emphasis-markers t
      org-agenda-files `(,(concat user-emacs-directory "notes"))
      display-buffer-alist '(("\\*vc-dir\\*" display-buffer-pop-up-window)))

(setq-default display-fill-column-indicator-column 80
	      cursor-in-non-selected-windows nil
	      truncate-lines t) ;; no word wrap thanks

(dolist (mode '(fido-vertical-mode
		global-auto-revert-mode show-paren-mode
		save-place-mode electric-pair-mode savehist-mode))
  (funcall mode 1)) ;; enable these

(load custom-file t)
(load (concat user-emacs-directory "local.el") t)

;; -- Bindings -----------------------------------------------------------------

(defun +kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p) (call-interactively #'kill-region)
    (if (bound-and-true-p paredit-mode) (paredit-backward-kill-word)
      (backward-kill-word 1))))

(defun +minibuffer-C-w ()
  (interactive)
  (if (and (fboundp 'icomplete-fido-backward-updir)
           (string-match-p "/" (minibuffer-contents)))
      (icomplete-fido-backward-updir)
    (backward-kill-word 1)))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w") #'+minibuffer-C-w))

(when (require 'vc-git nil)
  (defmacro +maybe-git-root (&rest body)
    `(let ((default-directory (or (vc-git-root default-directory) 
                                  default-directory)))
       ,@body))

  (defun +rgrep (pattern)
    (interactive "sSearch: ")
    (+maybe-git-root (rgrep pattern "*" ".")))

  (defun +repl ()
    (interactive)
    (other-window-prefix)
    (+maybe-git-root (pcase major-mode
		       ('clojure-mode
			(setq-local inferior-lisp-prompt "^[^=> \n]*[=>] *")
			(inferior-lisp "clojure -A:dev"))
		       ('emacs-lisp-mode (ielm))
		       ('scheme-mode
			(setq-local inferior-lisp-prompt "^[0-9]* *\\]=> *")
			(inferior-lisp "scheme"))
		       ('sh-mode (shell))
		       (_ (message "No REPL defined for %s" major-mode)))))

  (add-hook ;; link tags
   'find-file-hook 
   (lambda ()
     (+maybe-git-root
      (setq-local
       tags-table-list 
       (mapcar (lambda (f) (expand-file-name f default-directory))
	       '(".git/tags/project" ".git/tags/deps" ".git/tags/lang")))))))

(defun +compile ()
  "Compile from directory with build file."
  (interactive)
  (let* ((+proj (lambda (dir)
	    (seq-some (lambda (f) (file-exists-p (expand-file-name f dir)))
		      '("Makefile" "go.mod" "package.json"))))
	 (default-directory (locate-dominating-file default-directory +proj)))
    (call-interactively 'compile)))

(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux (setq x-super-keysym 'meta)))

(defmacro ff (&rest path)
  `(lambda () (interactive)
     (find-file (concat ,@path))))

(dolist (binding `(("C-c d" vc-diff-mergebase) ;; diff two branches
		   ("C-c g" vc-dir-root)
		   ("C-c h" vc-region-history) ;; + file history without region
		   ("C-c i" ,(ff user-init-file))
		   ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
		   ("C-c p" project-find-file)
		   ("C-c P" ,(ff "~/src"))
		   ("C-h" delete-backward-char)
		   ("C-j" newline) ;; C-j indents like RET in non-lisp modes
		   ("C-w" +kill-region-or-backward-word)
		   ("C-;" completion-at-point)
		   ("M-E" emoji-search)	;; express yourself!
		   ("M-F" toggle-frame-fullscreen)
		   ("M-I" +rgrep) ;; I for investigate
		   ("M-K" kill-whole-line)
		   ("M-Q" sql-connect) ;; a.k.a query
		   ("M-R" +repl)
		   ("M-j" (lambda () (interactive) (join-line -1)))
		   ("M-s" save-buffer)
		   ("M-o" other-window) ("M-O" delete-other-windows)
		   ("C-c m" recompile)  ("C-c M" +compile)
		   ("M-n" forward-paragraph) ("M-p" backward-paragraph)
		   ("M-H" ,help-map) ("M-S" ,search-map)))
  (global-set-key (kbd (car binding)) (cadr binding)))

;; -- Editing setup ------------------------------------------------------------
(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
		   (display-line-numbers-mode 1)
		   (display-fill-column-indicator-mode 1)
		   (column-number-mode 1)
		   (hl-line-mode 1))))

(define-key isearch-mode-map (kbd "C-j")
	    (lambda () (interactive)
	      (isearch-exit) (goto-char isearch-other-end)))

(defun should-center-buffer-p ()
  (memq major-mode '(org-mode markdown-mode)))

(defun center-prose-buffer-margins ()
  (set-window-margins nil 0 0)
  (when (memq major-mode '(org-mode markdown-mode))
    (let* ((char-width-pix (frame-char-width))
           (window-width-pix (window-body-width nil t))	; t = pixels!
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

(add-hook 'org-mode-hook (lambda () (prose-config) (org-indent-mode)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c f") 
    (lambda () (interactive)
      (shell-command-on-region (point-min) (point-max) "pg_format -" nil t))))

;; -- Appearance ---------------------------------------------------------------
(dolist (ui-mode
	 '(menu-bar-mode tool-bar-mode blink-cursor-mode))
  (funcall ui-mode -1)) ;; disable these

(when window-system
  (scroll-bar-mode -1)
  (fringe-mode -1)
  (defun +font (names) (seq-find #'x-list-fonts names))
  (defconst *default-font* (+font '("Rec Mono Linear" "Monaco" "Monospace")))
  (defconst *writing-font* (+font '("Rec Mono Casual" "Sans Serif")))

  (set-face-attribute 'default nil :font *default-font* :height 160)
  (set-face-attribute 'variable-pitch nil :font *writing-font* :height 160)

  (with-eval-after-load 'org
    (dolist (group '(org-block org-code org-verbatim org-table))
      (set-face-attribute group nil :font *default-font*))))

(setq-default cursor-type 'box)
(setq cursor-in-non-selected-windows t)

(dolist (attr `((alpha (95 . 95)) (width 100) (height 60)))
  (set-frame-parameter (selected-frame) (car attr) (cadr attr)))

(ignore-errors (load-theme 'flow t))

;; -- Languages ----------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/external-modes/")

(autoload 'clojure-mode "clojure-mode" "Major mode for Clojure" t)
(autoload 'go-mode "go-mode" "Major mode for Go" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown" t)

(define-derived-mode templ-mode prog-mode "Templ"
  (add-hook 'after-save-hook 'format-buffer-templ nil t))

(defun format-buffer-templ ()
  (interactive)
  (when (buffer-file-name)
    (shell-command (concat "templ fmt " (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook
	    '(lambda ()
	       (define-key clojure-mode-map (kbd "C-x C-e")
			   (lambda ()
			     (interactive)
			     (if (region-active-p)
				 (call-interactively #'lisp-eval-region)
			       (lisp-eval-last-sexp))))
	       (define-key clojure-mode-map (kbd "C-c C-k")
			   (lambda ()
			     (interactive)
			     (save-excursion
			       (mark-whole-buffer)
			       (call-interactively #'lisp-eval-region)))))))

(with-eval-after-load 'go-mode
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default gofmt-command "goimports"
                gofmt-show-errors 'echo))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'prose-config))

(dolist (assoc '(("\\.\\(clj\\|cljs\\|cljc\\|edn\\)\\'" . clojure-mode)
		 ("\\.go\\'" . go-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.ya?ml\\'" . conf-mode)
		 ("\\.templ\\'" . templ-mode)
		 ("\\.\\(json\\|ts\\|tsx\\)\\'" . js-mode)))
  (add-to-list 'auto-mode-alist assoc))

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

(when (require 'paredit nil)
  (dolist (lisp-mode-hook
	   '(clojure-mode-hook emacs-lisp-mode-hook
	     inferior-lisp-mode-hook lisp-data-mode-hook
	     eval-expression-minibuffer-setup-hook))
    (add-hook lisp-mode-hook #'enable-paredit-mode))

  (dolist (binding '(("M-s" nil) ("C-j" +paredit-RET) ("M-r" +paredit-M-r)
		     ("M-k" paredit-forward-barf-sexp)
		     ("M-l" paredit-forward-slurp-sexp)))
    (define-key paredit-mode-map (kbd (car binding)) (cadr binding))))
