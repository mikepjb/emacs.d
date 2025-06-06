(menu-bar-mode -1)
(tool-bar-mode -1)
(when window-system (scroll-bar-mode -1))

(setq inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore)

(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux (setq x-super-keysym 'meta)))

(setq inhibit-startup-screen t
      visible-bell t
      ring-bell-function 'ignore
      custom-file (concat user-emacs-directory "custom.el")
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      isearch-wrap-pause 'no-ding
      use-short-answers t
      truncate-lines t ;; no word wrap thanks
      vc-follow-symlinks t
      find-file-visit-truename t
      split-height-threshold 80
      split-width-threshold 160)

;; TODO #file# files still being created, what are these?

(setq-default compilation-scroll-output 'first-error
	      compilation-window-height 15
	      display-fill-column-indicator-column 80
	      truncate-lines t)

(when (file-exists-p custom-file)
  (load custom-file))

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (base-mode '(fido-vertical-mode
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

(defun kill-region-or-backward-word ()
  "Kill region if active, otherwise kill backward word."
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (cond ((and (bound-and-true-p paredit-mode) (fboundp 'paredit-backward-kill-word))
           (paredit-backward-kill-word))
          (t (backward-kill-word 1)))))


(setq inferior-lisp-program "scheme")
(setq inferior-lisp-prompt "^[0-9]* *\\]=> *")

(defun repl ()
  (interactive)
  (other-window-prefix)
  (pcase major-mode
    ('emacs-lisp-mode (ielm))
    ('scheme-mode (inferior-lisp "scheme"))
    ('sql-mode (sql-postgres))
    (_ (message "No REPL defined for %s" major-mode))))

(defmacro ff (path)
  `(lambda ()
    (interactive)
    (find-file ,path)))

(dolist (binding `(("M-o" other-window)
		   ("M-O" delete-other-window)
		   ("C-w" kill-region-or-backward-word) ("M-K" kill-whole-line)
		   ("M-D" duplicate-line)
		   ("C-;" hippie-expand)
		   ("M-j" (lambda () (interactive) (join-line -1)))
		   ("M-F" toggle-frame-fullscreen)
		   ("M-R" repl)
		   ("C-j" newline) ;; because electric-indent overrides this
		   ("M-P" project-find-file)
		   ("C-c d" sql-connect)
		   ("C-c p" project-find-file)
		   ("C-c g" vc-dir-root)
		   ("C-c h" vc-region-history) ;; + file history without region
		   ("C-c a" vc-annotate) ;; a.k.a git blame
		   ("C-h" delete-backward-char)
		   ("M-s" save-buffer)
		   ("M-/" comment-line)
		   ("C-c i" ,(ff user-init-file))
		   ("C-c n" ,(ff (concat user-emacs-directory "notes.org")))
		   ("C-c P" ,(ff "~/src"))
		   ("C-c m" recompile) ("C-c M" project-compile)))
  (global-set-key (kbd (car binding)) (cadr binding)))

(global-set-key (kbd "M-H") help-map)
(global-set-key (kbd "M-S") search-map)

(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
		   (display-line-numbers-mode 1)
		   (display-fill-column-indicator-mode 1)
		   (column-number-mode 1)
		   (hl-line-mode 1))))

;; TODO think yaml is text-mode based LOL
;; can we center the context of a buffer without olivetti?
;; also writegood?
(add-hook 'text-mode-hook (lambda ()
			    (variable-pitch-mode 1)
			    (visual-line-mode 1)))

(cl-flet ((find-font (names) (seq-find #'x-list-fonts names)))
  (let ((font (find-font '("Rec Mono Linear" "Monaco" "Monospace")))
        (writing-font (find-font '("Rec Mono Casual" "Sans Serif"))))
    (when font
      (set-face-attribute 'default nil :font font :height 160))
    (when writing-font
      (set-face-attribute 'variable-pitch nil :font writing-font :height 160))))

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "R") 'vc-revert))

(add-hook 'vc-dir-mode-hook 
          (lambda () 
            (vc-dir-hide-up-to-date)))

;; window-toggle-side-windows? what is this?

(setq log-edit-show-diff t)
(setq log-edit-show-files nil)

(setq-default modus-themes-common-palette-overrides
              '((comment fg-dim)
                (doc-markup fg-alt)
                (border-mode-line-inactive bg-inactive)
                (bg-line-number-inactive bg-main)
                (fg-line-number-inactive fg-dim)
                (fringe bg-main)
                (red red-faint)
                (err blue)
                ;; Make more things subtle/quiet
                (string fg-alt)           ; Strings less prominent
                (keyword fg-main)         ; Keywords same as normal text
                (builtin fg-main)         ; Built-ins quiet
                (constant fg-main)        ; Constants quiet  
                (type fg-main)            ; Types quiet
                (variable fg-main)        ; Variables quiet
                (function fg-main)        ; Functions quiet
                (bg-mode-line-active bg-dim) ; Subtle mode line
                (fg-mode-line-active fg-main)
                (bg-region bg-dim)        ; Subtle selection
                (fg-region unspecified)   ; No special selection color
                (yellow yellow-faint)     ; Warnings less bright
                (green green-faint)       ; Success less bright
                (blue blue-faint)         ; Info less bright
                (magenta magenta-faint))) ; Less bright overall

(load-theme 'modus-vivendi-tinted t)
  
;; 3rd party packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

;; emacs can do jumping to it's own source code, can it do this for Java.. and clojure?

(use-package magit :bind ("C-c g" . magit-status))
(use-package ruby-mode)
(use-package go-mode)
(use-package json-mode)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package js2-mode)
(use-package typescript-mode)


;; ssh-add --apple-use-keychain ~/.ssh/id_rsa

;; # Add to ~/.ssh/config
;; Host *
;;   UseKeychain yes
;;   AddKeysToAgent yes
;;   IdentityFile ~/.ssh/id_rsa

;; export SSH_ASKPASS=/usr/lib/seahorse/seahorse-ssh-askpass

(setenv "SSH_ASKPASS" "/usr/lib/seahorse/ssh-askpass")
