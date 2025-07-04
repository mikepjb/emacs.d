;; -- Spartan Emacs configuration ------------------ -*- lexical-binding: t; -*-
;; html-div, skeleton-insert, completing-read alterations for full match then partial?
;; generate-new-buffer?
;; setup split on char to be easy like your vim muscle memory. with s/,/,\r
;; M-d kills word, how to delete *whole* word like dW?
;; paredit for HTML/tags? we already extend for []{} in Clojure?
;; good to avoid killing/unbalancing tags but also copy by tag (vim vat/vit)
;; how useful is transpose day to day? e.g transpose-sexp/word/line etc.
;; Select rectangular region: C-x SPC (or C-x r SPC)
;; Insert text in rectangle: C-x r t then type your text
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
      display-buffer-alist '(("\\*vc-dir\\*" display-buffer-pop-up-window)))

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
                       '("Makefile" "go.mod" "package.json" "deps.edn" ".git"))
              default-directory)))
     ,@body))

(defun +repl ()
  (interactive)
  (other-window-prefix)
  (+with-context (pcase major-mode
           ('clojure-mode (inferior-lisp "clojure -A:dev"))
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
   (let ((files (split-string (shell-command-to-string "find . -type f") "\n" t)))
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
                   ("C-c i" ,(ff user-init-file))
                   ("C-c n" ,(ff user-emacs-directory "notes/index.org"))
                   ("C-c p" +find-file) ("C-c P" ,(ff "~/src"))
                   ("C-." repeat) ("M-z" zap-up-to-char)
                   ("C-h" delete-backward-char) ("C-j" newline) ;; autoindents
                   ("C-w" +kill-region-or-backward-word) ("C-;" dabbrev-expand)
                   ("M-e" ,(il (select-window (or (split-window-sensibly)
                                                  (split-window)))))
                   ("M-F" toggle-frame-fullscreen)
                   ("M-I" (lambda (pattern) (interactive "sSearch: ")
                            (+with-context (rgrep pattern "*" "."))))
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
(add-to-list 'load-path "~/.emacs.d/external-modes/")

(autoload 'clojure-mode "clojure-mode" "Major mode for Clojure" t)
(autoload 'go-mode "go-mode" "Major mode for Go" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown" t)
(autoload 'csv-mode "csv-mode" "Major mode for CSV" t)

(with-eval-after-load 'csv-mode
  (add-hook 'csv-mode-hook (lambda ()
                             (csv-align-mode 1)
                             (csv-header-line 1))))

(define-derived-mode templ-mode prog-mode "Templ"
  (add-hook 'after-save-hook 'format-buffer-templ nil t)
  (require 'sgml-mode)
  (setq-local sgml-tag-alist html-tag-alist
              sgml-tag-help html-tag-help
              sgml-xml-mode t)
  (define-key templ-mode-map (kbd "C-c t") #'sgml-tag))

(defun format-buffer-templ ()
  (when (buffer-file-name)
    (shell-command (concat "templ fmt " (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'subword-mode)
  (define-key clojure-mode-map (kbd "C-x C-e")
    (lambda () (interactive)
      (if (region-active-p) (lisp-eval-region (region-beginning) (region-end))
        (lisp-eval-last-sexp))))
  (define-key clojure-mode-map (kbd "C-c C-k")
    (lambda () (interactive) (lisp-eval-region (point-min) (point-max)))))

(with-eval-after-load 'go-mode
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default gofmt-command "goimports" gofmt-show-errors 'echo))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'prose-config))

(+add-to-list
 'auto-mode-alist
 '(("\\.\\(clj[sc]?\\|bb\\(?:\\.edn\\)?\\|edn\\)\\'" . clojure-mode)
   ("\\.go\\'" . go-mode) ("\\.md\\'" . markdown-mode) ("\\.csv\\'" . csv-mode)
   ("\\.ya?ml\\'" . conf-mode) ("\\.templ\\'" . templ-mode)
   ("\\.\\(json\\|ts\\|tsx\\)\\'" . js-mode)))

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
  (dolist (hook '(clojure-mode-hook
          emacs-lisp-mode-hook
          inferior-lisp-mode-hook lisp-data-mode-hook
          eval-expression-minibuffer-setup-hook))
    (add-hook hook #'enable-paredit-mode))

  (dolist (binding '(("C-j" +paredit-RET) ("M-r" +paredit-M-r)
             ("M-k" paredit-forward-barf-sexp) ("M-s" nil)
             ("M-l" paredit-forward-slurp-sexp)))
    (define-key paredit-mode-map (kbd (car binding)) (cadr binding))))
