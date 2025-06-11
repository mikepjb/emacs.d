;; Spartan Emacs Configuration, never more than 150 lines, sometimes less. -----

;; Dynamic prose centering (~15 lines) - fixed margins would do
;; Half the key bindings (~20 lines) - keep only daily-use ones

;; Your core insight about flow state vs. tooling assistance is sound - but your implementation has accumulated too much surface area to be truly minimal.

;; still need a good way to go up/down paragraph on ZSA voyager, M-{ & M-} are not accessible

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      custom-file (concat user-emacs-directory "custom.el")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "saves")))
      create-lockfiles nil
      use-short-answers t
      vc-follow-symlinks t
      find-file-visit-truename t
      split-height-threshold 80
      split-width-threshold 160
      sql-input-ring-file-name (concat user-emacs-directory "sql-history")
      org-export-with-section-numbers nil ;; essential for exporting
      org-ellipsis " ▼"
      org-hide-emphasis-markers t
      org-agenda-files `(,(concat user-emacs-directory "notes"))
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)"))
      display-buffer-alist '(("\\*vc-dir\\*" display-buffer-pop-up-window)))

(setq-default display-fill-column-indicator-column 80
	      default-frame-width 160
	      default-frame-height 60
	      truncate-lines t) ;; no word wrap thanks

(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(add-hook 'inferior-lisp-mode-hook (lambda () (font-lock-mode -1)))

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
  "Kill backward word, or go up directory if looks like file path."
  (interactive)
  (if (and (fboundp 'icomplete-fido-backward-updir)
           (string-match-p "/" (minibuffer-contents)))
      (icomplete-fido-backward-updir)
    (backward-kill-word 1)))

(with-eval-after-load 'icomplete
  (define-key
   icomplete-minibuffer-map (kbd "C-w") #'+minibuffer-kill-backward))

(defun +repl ()
  (interactive)
  (other-window-prefix)
  (let ((default-directory (or (vc-git-root default-directory)
			       default-directory)))
    (pcase major-mode
      ('clojure-mode
       (setq-local inferior-lisp-prompt "^[^=> \n]*[=>] *")
       (inferior-lisp "clojure -A:dev"))
      ('emacs-lisp-mode (ielm))
      ('scheme-mode
       (setq-local inferior-lisp-prompt "^[0-9]* *\\]=> *")
       (inferior-lisp "scheme"))
      ('sh-mode (shell))
      (_ (message "No REPL defined for %s" major-mode)))))

(defun +link-tags ()
  (when-let ((git-dir (locate-dominating-file default-directory ".git")))
    (setq-local tags-table-list 
                (mapcar (lambda (f) (expand-file-name f git-dir))
                        '(".git/tags/project" ".git/tags/deps" ".git/tags/external")))))

(add-hook 'find-file-hook #'+link-tags)

(defun +compile ()
  "Compile from directory with build file."
  (interactive)
  (let ((build-dir
	 (locate-dominating-file
	  default-directory
	  (lambda (dir)
	    (seq-some (lambda (f) (file-exists-p (expand-file-name f dir)))
		      '("Makefile" "go.mod" "package.json"))))))
    (let ((default-directory (or build-dir default-directory)))
      (call-interactively 'compile))))

(defmacro ff (&rest path)
  `(lambda ()
     (interactive)
     (find-file (concat ,@path))))

(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux (setq x-super-keysym 'meta)))

(dolist (binding `(("C-c d" vc-diff-mergebase) ;; diff two branches
		   ("C-c g" vc-dir-root)
		   ("C-c h" vc-region-history) ;; + file history without region
		   ("C-c i" ,(ff user-init-file))
		   ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
		   ("C-c p" project-find-file)
		   ("C-c P" ,(ff "~/src"))
		   ("C-h" delete-backward-char)
		   ("C-j" newline) ;; because electric-indent overrides this
		   ("C-w" +kill-region-or-backward-word)
		   ("M-C" org-agenda-list) ;; a.k.a checklist
		   ("M-E" emoji-search)	   ;; express yourself!
		   ("M-F" toggle-frame-fullscreen)
		   ("M-I" +rg) ;; I for investigate
		   ("M-K" kill-whole-line)
		   ("M-Q" sql-connect) ;; a.k.a query
		   ("M-R" +repl)
		   ("M-j" (lambda () (interactive) (join-line -1)))
		   ("M-s" save-buffer)
		   ("M-o" other-window) ("M-O" delete-other-windows)
		   ("C-c m" recompile)  ("C-c M" +compile)))
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

(add-hook 'org-mode-hook (lambda () (prose-config) (org-indent-mode)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-i") 'org-todo))

(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c f") 
    (lambda () (interactive)
      (shell-command-on-region (point-min) (point-max) "pg_format -" nil t))))

;; Appearance ------------------------------------------------------------------
(dolist (ui-mode ;; disable these
	 '(menu-bar-mode tool-bar-mode blink-cursor-mode))
  (funcall ui-mode -1))

(when window-system
  (scroll-bar-mode -1)
  (fringe-mode -1)
  (defun +font (names) (seq-find #'x-list-fonts names))
  (defconst *default-font* (+font '("Rec Mono Linear" "Monaco" "Monospace")))
  (defconst *writing-font* (+font '("Rec Mono Casual" "Sans Serif")))

  (set-face-attribute 'default nil :font *default-font* :height 160)
  (set-face-attribute 'variable-pitch nil :font *writing-font* :height 160)

  ;; breaks when loading org-mode.. find after loading
  (with-eval-after-load 'org
    (dolist (group '(org-block org-code org-verbatim org-table))
      (set-face-attribute group nil :font *default-font*)))
  )

(setq-default cursor-type 'box)

(dolist (attr `((alpha (95 . 95))
		(width 160)
		(height 60)))
  (set-frame-parameter (selected-frame) (car attr) (cadr attr)))

(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(ignore-errors (load-theme 'flow t))

;; Built-in Language Assignment ------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(json\\|ts\\|tsx\\)\\'" . js-mode))

;; Packages (mostly just language major-modes) ---------------------------------
(add-to-list 'load-path "~/.emacs.d/external-modes/")

;; Autoload the modes (only loads when actually used)
(autoload 'clojure-mode "clojure-mode" "Major mode for Clojure" t)
(autoload 'go-mode "go-mode" "Major mode for Go" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown" t)
(autoload 'paredit-mode "paredit" "Minor mode for balanced parentheses" t)

(when (require 'paredit nil)
  (defun +newline-or-repl-eval ()
    (interactive)
    (call-interactively (if (eq major-mode 'inferior-lisp-mode)
			    'comint-send-input
			  'paredit-RET)))

  (defun +raise-sexp-or-search-repl ()
    (interactive)
    (call-interactively (if (eq major-mode 'inferior-lisp-mode)
			    'comint-history-isearch-backward-regexp
			  'paredit-raise-sexp)))

  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'inferior-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-data-mode-hook 'paredit-mode)


  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "C-j") #'+newline-or-repl-eval)
  (define-key paredit-mode-map (kbd "RET") 'paredit-C-j)
  (define-key paredit-mode-map (kbd "M-r") #'+raise-sexp-or-search-repl)
  (define-key paredit-mode-map (kbd "M-k") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-l") 'paredit-forward-slurp-sexp))

;; Auto-mode associations (triggers autoload when opening files)
(add-to-list 'auto-mode-alist '("\\.\\(clj\\|cljs\\|cljc\\|edn\\)\\'"
				. clojure-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Configure only when modes are actually loaded
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

;; Local files -----------------------------------------------------------------
(load custom-file t)
(load (concat user-emacs-directory "local.el") t)
