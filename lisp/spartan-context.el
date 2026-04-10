;; spartan-context is a collection of tools used to work within a project/context.
;; similar to project.el. We don't use project.el


(defconst *context-identifiers*
  '("Makefile" "gradlew" "pom.xml" "go.mod" "package.json" "deps.edn" ".git"))


(defmacro +with-context (&rest body)
  `(let ((default-directory
          (or (seq-some (lambda (f) (locate-dominating-file default-directory f))
                       *context-identifiers*)
              default-directory)))
     ,@body))

(defun +compile ()
  (interactive)
  (+with-context (call-interactively 'compile)))

(defun +project-tags-generate ()
  (interactive)
  (+with-context
   (let ((default-directory default-directory))
     (make-process
      :name "ctags"
      :buffer nil
      :command '("ctags" "-eR" "-f" ".tags" "--exclude=node_modules" ".")
      :sentinel (lambda (_ e) (message "ctags: %s" (string-trim e)))))))

(defun +project-tags-load ()
  (when-let ((f (locate-dominating-file default-directory ".tags")))
    (visit-tags-table (expand-file-name ".tags" f) t)))

(provide 'spartan-context)
