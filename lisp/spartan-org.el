(use-package org :ensure nil
  :custom
  (org-modules nil) ;; don't load the world
  (org-ellipsis " ▼")
  (org-hide-emphasis-markers t)
  (org-export-with-section-numbers nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "CURRENT(c)" "|" "DONE(d!)" "CANCELLED(x@)")))
  (org-todo-keyword-faces
   '(("TODO"      . (:foreground "#ff6c6b" :weight bold))
     ("NEXT"      . (:foreground "#51afef" :weight bold))
     ("CURRENT"   . (:foreground "#ef51af" :weight bold))
     ("DONE"      . (:foreground "#98be65"))
     ("CANCELLED" . (:foreground "#5B6268" :strike-through t))))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-directory "~/.emacs.d/notes")
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(clock closed))
  (org-agenda-files '("~/.emacs.d/notes"))
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
  ;; Show full path in completion (e.g. "Projects/Work/Task" not just "Task")
  (org-refile-use-outline-path t)
  ;; Required when using outline-path — lets completing-read handle the full path
  (org-outline-path-complete-in-steps nil)
  ;; Allow creating new parent nodes on refile
  (org-refile-allow-creating-parent-nodes 'confirm)
  :bind (:map org-mode-map
              ("M-RET" . nil)
              ("C-c r" . +archive-task-at-point)
              ;; ("M-RET" . org-meta-return) ;; TODO needs new binding
              )
  :hook ((org-mode . org-indent-mode)
         (org-after-todo-state-change . +org-clock-todo-change))
  :config
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))))

(defvar +archive-file "~/.emacs.d/notes/archive.org")

(defun +archive-task-at-point ()
  "Archive the heading at point to archive.org under the same project."
  (interactive)
  (let* ((project (save-excursion
                    (while (and (> (org-current-level) 3) (org-up-heading-safe)))
                    (when (= (org-current-level) 3)
                      (org-get-heading t t t t))))
         (archive-file (expand-file-name +archive-file))
         (pos (when project
                (with-current-buffer (find-file-noselect archive-file)
                  (org-find-exact-headline-in-buffer project)))))
    (if (not project)
        (user-error "No level-3 project ancestor found")
      (org-refile nil nil (list project archive-file nil pos))
      (message "Archived under: %s" project))))

(defun +org-clock-todo-change ()
  (if (string= org-state "CURRENT")
      (org-clock-in)
    (org-clock-out-if-current)))


(provide 'spartan-org)
