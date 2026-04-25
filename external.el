;;; 🔱 Spartan Emacs External -*- lexical-binding: t; -*-

;; eshell & bash env
(defconst *path*
  '("$HOME/.sdkman/candidates/java/current/bin"
    "/usr/local/go/bin"
    "$HOME/go/bin"
    "/sbin"
    "/opt/homebrew/bin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/usr/local/sbin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
    "$HOME/.emacs.d/scripts"))

(setenv "PATH" (mapconcat #'substitute-in-file-name *path* ":"))
(setq exec-path
      (append (mapcar #'substitute-in-file-name *path*)
              (list exec-directory)))

(defun eshell/gr ()
  (if-let ((root (vc-root-dir)))
      (eshell/cd root)
    (error "Not in a VC repository")))

(defun eshell/e (&rest args)
  (find-file (pop args)))

;; external dependencies
(defconst *external-deps*
  '((jq)
    (gh) ;; github cli tool
    (java :linux jre25-openjdk)
    (yq :linux go-yq)))

(defun +install-external-deps ()
  (interactive)
  (dolist (dep *external-deps*)
    (message "%s" (car dep))))

;; repls
(defun +repl (&optional arg)
  (interactive "P")
  (let* ((repls '(("clojure" inferior-lisp "clojure -A:dev")
                  ("python" run-python)
                  ("sqlite" sql-sqlite)
                  ("node" comint-run "node")
                  ("ruby" comint-run "irb")))
         (spec (cdr (assoc (if arg (completing-read "REPL: " repls nil t)
                             "clojure")
                           repls))))
    (other-window-prefix)
    (apply (car spec) (cdr spec))))

;; ctags
(defun +ctags ()
  (interactive)
  (+with-context
   (make-process
    :name "ctags" :buffer nil
    :command '("ctags" "-eR" "-f" ".tags"
               "--exclude=node_modules" "--exclude=dist" ".")
    :sentinel (lambda (_ e) (message "ctags: %s" (string-trim e))))))

(defun +ctags-link ()
  (when-let ((dir (locate-dominating-file default-directory ".tags")))
    (let ((tags-file (expand-file-name ".tags" dir)))
      (when (and (file-regular-p tags-file)
                 (not (member tags-file tags-table-list)))
        (visit-tags-table tags-file t)))))

(add-hook 'find-file-hook #'+ctags-link)

;; testing
(defun +test (&optional arg)
  "Run current test file, or lint/test the whole project with C-u."
  (interactive "P")
  (+with-context
   (pcase major-mode
     ('java-mode
 (let* ((root (locate-dominating-file
               default-directory
               (lambda (d)
                 (seq-some (lambda (f) (file-exists-p (expand-file-name f d)))
                           '("build.gradle" "build.gradle.kts" "pom.xml")))))
        (gradle? (or (file-exists-p (expand-file-name "build.gradle" root))
                     (file-exists-p (expand-file-name "build.gradle.kts" root))))
        (cmd (cond ((and gradle? (file-exists-p (expand-file-name "gradlew" root))) "./gradlew")
                   (gradle? "gradle")
                   ((file-exists-p (expand-file-name "mvnw" root)) "./mvnw")
                   (t "mvn")))
        (class (file-name-base (buffer-file-name)))
        (default-directory root))
   (compile
    (cond (arg     (concat cmd " test"))
          (gradle? (format "%s test --tests %s" cmd class))
          (t       (format "%s test -Dtest=%s" cmd class))))))
     ('js-mode (compile
                (concat "fnm use && FORCE_COLOR=1 node --test " (buffer-file-name))))
     (_ (message "no test configuration for %s" major-mode)))))

;; llm


(provide 'external)
