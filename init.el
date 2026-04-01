;; Behaviour
(setq search-wrap-around t)
(setq ring-bell-function 'ignore)
(setq truncate-lines t)
(setq create-lockfiles nil)
(setq frame-resize-pixelwise t) ;; do not maximise after leaving fullscreen

(fido-vertical-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(save-place-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)

(setq custom-file (concat user-emacs-directory "local.el"))
(load custom-file t)

(pcase system-type
  ('darwin (setq mac-command-modifier 'meta))
  ('gnu/linux (setq x-super-keysym 'meta)))

;; Appearance
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode -1)

  (defconst *font* (seq-find #'x-list-fonts '("Rec Mono Casual" "Monospace")))
  (set-face-attribute 'default nil :font *font* :height 160))

(defun forward-slurp ()
  (interactive)
  (save-excursion
    (up-list)
    (let ((close (char-before)))
      (delete-char -1)
      (forward-sexp)
      (insert close))))

(defun forward-barf ()
  (interactive)
  (save-excursion
    (up-list)
    (let ((close (char-before)))
      (delete-char -1)
      (backward-sexp)
      (delete-horizontal-space)
      (insert (string close ?\s)))))

(use-package flycheck
  :ensure nil)

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

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-w") #'+minibuffer-C-w)
  (define-key icomplete-minibuffer-map (kbd "C-e") #'icomplete-ret))

(defmacro il (&rest body) `(lambda () (interactive) ,@body))

;; Bindings
(dolist (binding `(("M-o" other-window) ("M-O" delete-other-windows)
		   ("M-s" save-buffer)
		   ("C-c g" vc-dir-root)
		   ("C-w" +kill-region-or-backward-word)
		   ("M-k" forward-barf) ("M-l" forward-slurp)
		   ("C-h" delete-backward-char) ("C-j" newline) ;; autoindents
		   ("M-j" ,(il (join-line -1)))
		   ("M-RET" toggle-frame-fullscreen)
		   ("M-H" ,help-map)
		   ("C-c C-s" ,search-map) ;; rarely used.. what is good to use in search-map?
		   ))
  (global-set-key (kbd (car binding)) (cadr binding)))

(load-theme 'modus-vivendi t)
