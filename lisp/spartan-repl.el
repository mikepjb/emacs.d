(defconst *repl-table*
  '((python-mode      run-python)
    (ruby-mode        comint-run "irb")
    (emacs-lisp-mode  ielm)
    (edn-mode         inferior-lisp "clojure -A:dev")
    (clojure-mode     inferior-lisp "clojure -A:dev")))

(defun +repl-cmd   (entry) (cadr entry))
(defun +repl-args  (entry) (cddr entry))
(defun +repl-label (entry)
  (if (+repl-args entry)
      (car (last (+repl-args entry)))
    (symbol-name (+repl-cmd entry))))

(defun +repl-run (entry)
  (if (+repl-args entry)
      (apply (+repl-cmd entry) (+repl-args entry))
    (call-interactively (+repl-cmd entry))))

(defun +repl (arg)
  (interactive "P")
  (other-window-prefix)
  (if arg
      (let* ((unique (seq-uniq *repl-table*
                               (lambda (a b) (equal (+repl-label a) (+repl-label b)))))
             (choice (completing-read "REPL: " (mapcar #'+repl-label unique) nil t))
             (entry  (seq-find (lambda (e) (equal (+repl-label e) choice)) *repl-table*)))
        (+repl-run entry))
    (let ((entry (assq major-mode *repl-table*)))
      (if entry
          (+repl-run entry)
        (user-error "No REPL configured for %s" major-mode)))))


(provide 'spartan-repl)
