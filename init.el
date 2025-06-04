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
      make-backup-files nil
      create-lockfiles nil
      isearch-wrap-pause 'no-ding
      split-height-threshold 80
      split-width-threshold 160)

(setq-default compilation-scroll-output 'first-error
	      compilation-window-height 15)

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
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word 1)))

(defun copy-current-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save (point) (line-end-position))))

(defun repl ()
  (interactive)
  (other-window-prefix)
  (pcase major-mode
    ('emacs-lisp-mode (ielm))
    ('sql-mode (sql-postgres))
    (_ (message "No REPL defined for %s" major-mode))))

(defmacro ff (path)
  `(lambda ()
    (interactive)
    (find-file ,path)))

(dolist (binding `(("M-o" other-window)
		   ("M-O" delete-other-window)
		   ("C-w" kill-region-or-backward-word)
		   ("M-K" kill-whole-line)
		   ("M-D" copy-current-line)
		   ("C-;" hippie-expand)
		   ("M-j" (lambda () (interactive) (join-line -1)))
		   ("M-R" repl)
		   ("C-j" newline) ;; because electric-indent overrides this
		   ("C-h" delete-backward-char)
		   ("M-s" save-buffer)
		   ("M-/" comment-line)
		   ("C-c i" ,(ff user-init-file))
		   ("C-c n" ,(ff (concat user-emacs-directory "notes.org")))
		   ;; copy current line? M-D?
		   ))
  (global-set-key (kbd (car binding)) (cadr binding)))

;; map help/search maps separately
(global-set-key (kbd "M-H") help-map)
(global-set-key (kbd "M-S") search-map)
(global-set-key (kbd "C-w") #'kill-region-or-backward-word)

(dolist (hook '(prog-mode-hook css-mode-hook))
  (add-hook hook (lambda ()
		   (display-line-numbers-mode 1)
		   (display-fill-column-indicator-mode 1)
		   (column-number-mode 1)
		   (hl-line-mode 1))))

(let ((font (seq-find #'x-list-fonts '("Rec Mono Linear" "Monaco" "Monospace")))
      (writing-font (seq-find #'x-list-fonts '("Rec Mono Casual" "Sans Serif"))))
  (when font  ; Only set if we found a font
    (set-face-attribute 'default nil :font font :height 160))
  (when writing-font
    (set-face-attribute 'variable-pitch nil :font writing-font :height 160)))

(setq-default modus-themes-common-palette-overrides
	      '((comment fg-dim)
		(doc-markup fg-alt)
		(border-mode-line-inactive bg-inactive)
		(bg-line-number-inactive bg-main)
		(fg-line-number-inactive fg-dim)
		(fringe bg-main)
		(red red-faint)
		(err blue)))

(load-theme 'modus-vivendi-tinted t)
