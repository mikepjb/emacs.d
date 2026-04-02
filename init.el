;;; repl + repl lookup (rebind man bind?)

(defconst +project-definitions
  '("Makefile" "gradlew" "pom.xml" "go.mod" "package.json" "deps.edn" ".git"))

(defmacro il (&rest body) `(lambda () (interactive) ,@body))
(defmacro ff (&rest path) `(il (find-file (concat ,@path))))

;;; Input
(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux (setq x-super-keysym 'meta)))

;; TODO can we/should be have all bindings (incl. ones from other languages) in one place? How does this work if the package isn't available?
(defconst *bindings*
  `(("M-o" other-window) ("M-O" delete-other-windows)
    ("M-s" save-buffer)
    ("C-c C-l" flycheck-list-errors)
    ("C-c i" ,(ff user-emacs-directory "init.el"))
    ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
    ("C-c p" project-find-files)
    ("C-c P" ,(ff "~/src"))
    ("C-c g" vc-dir-root)
    ("C-w" +kill-region-or-backward-word)
    ("M-k" +lisp-forward-barf)
    ("M-l" +lisp-forward-slurp)
    ("C-c s" +lisp-splice-sexp)
    ("C-h" delete-backward-char) ("C-j" newline) ;; autoindents
    ("M-j" ,(il (join-line -1)))
    ("M-RET" toggle-frame-fullscreen)
    ("M-H" ,help-map)
    ("M-D" duplicate-line)
    ("C-;" dabbrev-expand)
    ("C-c C-s" ,search-map)))

(dolist (bind *bindings*)
  (global-set-key (kbd (car bind)) (cadr bind)))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w") #'+minibuffer-C-w)
  (define-key icomplete-minibuffer-map (kbd "C-e") #'icomplete-ret))

;;; General Settings
(setq
 search-wrap-around t
 ring-bell-function 'ignore
 create-lockfiles nil
 frame-resize-pixelwise t)

(defconst *general-modes*
  '(fido-vertical-mode
    global-auto-revert-mode
    save-place-mode
    savehist-mode))

(dolist (mode *general-modes*) (funcall mode 1))

;;; Editor Settings
(setq-default truncate-lines t)

(defconst *editing-modes*
  '(show-paren-mode
    electric-pair-mode))

(dolist (mode *editing-modes*) (funcall mode 1))

;;; unorganised

(setq custom-file (concat user-emacs-directory "local.el"))
(load custom-file t)

;; Appearance
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode -1)

  (defconst *font* (seq-find #'x-list-fonts '("Rec Mono Casual" "Monospace")))
  (set-face-attribute 'default nil :font *font* :height 160))

(defun +lisp-forward-slurp ()
  (interactive)
  (save-excursion
    (up-list)
    (let ((close (char-before)))
      (delete-char -1)
      (forward-sexp)
      (insert close))))

(defun +lisp-forward-barf ()
  (interactive)
  (save-excursion
    (up-list)
    (let ((close (char-before)))
      (delete-char -1)
      (backward-sexp)
      (delete-horizontal-space)
      (insert (string close ?\s)))))

(defun +lisp-splice-sexp ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (delete-char 1)
    (forward-sexp)
    (delete-char -1)))

(use-package clojure-mode :ensure t)
(use-package json-mode :ensure t)
(use-package project :ensure nil)
(use-package org :ensure nil
  :custom
  (org-hide-emphasis-markers t)
  (org-export-with-section-numbers nil))

(use-package flycheck
  :ensure nil
  :config (global-flycheck-mode 1))

(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
	   (display-line-numbers-mode 1) (column-number-mode 1)
	   (auto-revert-mode 1)
	   (display-fill-column-indicator-mode 1) (hl-line-mode 1))))

(defun center-prose (buf)
  (when (buffer-live-p buf)
    (dolist (win (get-buffer-window-list buf nil 'visible))
      (let ((margin (max 0 (round (/ (- (window-body-width win t)
					(* 80 (frame-char-width)))
				     2.0 (frame-char-width))))))
	(set-window-margins win margin margin)))))

(defun prose-config ()
  (visual-line-mode 1)
  (let ((buf (current-buffer)))
    (add-hook 'window-size-change-functions
	      (lambda (&rest _) (center-prose buf))
	      nil t)))

(add-hook 'org-mode-hook (lambda () (prose-config) (org-indent-mode)))

(add-hook 'before-save-hook 'whitespace-cleanup)

(defun +kill-region-or-backward-word ()
  (interactive)
  (cond ((region-active-p) (call-interactively #'kill-region))
	(t (backward-kill-word 1))))

(defun +minibuffer-C-w ()
  (interactive)
  (if (string-match-p "/" (minibuffer-contents))
      (icomplete-fido-backward-updir) (backward-kill-word 1)))

(defun +local-ai ()
  (interactive)
  (start-process "llama-server" "*llama-server*"
		 "llama-server" "--port" "7777"
		 "--model" "~/models/Qwen3.5-9B-UD-Q3_K_XL.gguf"))

(load-theme 'modus-vivendi t)

;;; Modeline
(defun +flow-truncate-buffer-name ()
  "Show right 1/3 of buffer name, prepend '<' if truncated."
  (let* ((name (if buffer-file-name
		   (abbreviate-file-name buffer-file-name)
		 (buffer-name)))
	 (max-len (/ (window-width) 3)))
    (if (<= (length name) max-len)
	name
      (concat "<" (substring name (- (length name) (- max-len 1)))))))

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

(defmacro +with-context (&rest body)
  `(let ((default-directory
	  (or (cl-some (lambda (f) (locate-dominating-file default-directory f))
		       +project-definitions)
	      default-directory)))
     ,@body))

(defun +project-tags-generate ()
  "Manually generate tags for current project."
  (interactive)
  (when-let ((project-root (cl-some (lambda (f) (locate-dominating-file default-directory f))
				    +project-definitions)))
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
		     (message "Tags generated at %s" tags-file)))))))

(defun +project-tags-load ()
  "Load tags file for current project if it exists."
  (when-let ((project-root (cl-some (lambda (f) (locate-dominating-file default-directory f))
				    +project-definitions)))
    (let ((tags-file (expand-file-name (format "~/.emacs.d/.tag-store/%s.tags"
					       (file-name-nondirectory (directory-file-name project-root))))))
      (when (file-exists-p tags-file)
	(setq-local tags-table-list (list tags-file))
	(visit-tags-table tags-file t)))))

(add-hook 'find-file-hook #'+project-tags-load)
