;;; 🔱 Spartan Emacs External -*- lexical-binding: t; -*-

;; eshell & bash env
(defconst *path*
  '("~/.sdkman/candidates/java/current/bin"
    "~/.local/bin"
    "/usr/local/go/bin"
    "~/go/bin"
    "/sbin"
    "/opt/homebrew/bin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/usr/local/sbin"
    "~/.cargo/bin"))

(setenv "PATH" (mapconcat #'expand-file-name *path* ":"))
(setq exec-path
      (append (mapcar #'expand-file-name *path*)
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
    ;; (bb)
    (gh) ;; github cli tool
    (java :linux jre25-openjdk)
    (sdk :message "install via https://sdkman.io/")
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
          (gradle? (format "%s test --tests %s --console=plain" cmd class))
          (t       (format "%s test -Dtest=%s -B -Dsurefire.useFile=false -Dtrimstacktrace=false" cmd class))))))
     ('js-mode (compile
                (concat "fnm use && FORCE_COLOR=1 node --test " (buffer-file-name))))
     (_ (message "no test configuration for %s" major-mode)))))

;; auditing
(defun +audit-most-changed ()
  (let* ((files (split-string
                 (shell-command-to-string
                  "git log --name-only --pretty=format: --since='1 year ago'")
                 "\n" t))
         (freq (seq-group-by #'identity files))
         (grouped (seq-group-by #'identity files))
         (sorted (seq-sort (lambda (a b) (> (length (cdr a))
                                            (length (cdr b))))
                           grouped)))
    (mapcar (lambda (x) (cons (car x) (length (cdr x))))
            (seq-take sorted 20))))

(defun +audit-authors ()
  "git shortlog -sn --no-merges")

(defun +audit ()
  (interactive)
  (let ((buf (get-buffer-create "*Project Audit*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Project Audit\n\n" 'face 'bold))

        (dolist (item (+audit-most-changed))
          (let ((file (car item)) (count (cdr item)))
            (insert (format "  %5d  %s\n" count file))))

        ;; authors insert here
        )
      (pop-to-buffer buf))))

;;; navi: Fire-and-forget LLM queries
(require 'auth-source)
(require 'json)

(defvar navi-backends
  '((local
     :endpoint "http://localhost:7777/v1/chat/completions"
     :model "gemma-4-e2b"
     :auth-host nil)
    (gemini
     :endpoint "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions"
     :model "gemini-3-flash-preview"
     :auth-host "generativelanguage.googleapis.com")
    (fireworks
     :endpoint "https://api.fireworks.ai/inference/v1/chat/completions"
     :model "accounts/fireworks/models/kimi-k2p5"
     :auth-host "api.fireworks.ai")))

(defvar navi-backend 'local)
(defvar navi-system-prompt
  "You are an expert senior developer specializing in Clojure, but
proficient in Rust, Go, JavaScript, TypeScript, and Java.

Your role is to provide clear, concise, and constructive code reviews
or detailed Stack Overflow-style answers.

Always default to Clojure unless a specific language is
requested.

Focus on idiomatic solutions and best practices. Respond directly to
the code or question presented.
")
(defvar navi-buffer-name "*navi*")

(defun navi--api-key (host)
  (when host
    (let* ((e (car (auth-source-search :host host :require '(:secret) :max 1)))
           (s (and e (plist-get e :secret))))
      (if (functionp s) (funcall s) s))))

(defun navi--sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (let* ((curl-buf (process-buffer proc))
           (output (with-current-buffer curl-buf (buffer-string)))
           (question (process-get proc :question))
           (content
            (or (ignore-errors
                  (let* ((obj (json-parse-string output :object-type 'alist))
                         (choices (alist-get 'choices obj)))
                    (alist-get 'content
                               (alist-get 'message (aref choices 0)))))
                (format "[no content]\n%s" output)))
           (buf (get-buffer-create navi-buffer-name)))
      (with-current-buffer buf
        (unless (eq major-mode 'markdown-mode) (markdown-mode))
        (goto-char (point-max))
        (insert (if (bobp) "" "\n\n---\n\n")
                "````user\n" question "\n````\n\n"
                content "\n"))
      (kill-buffer curl-buf)
      (message "navi: response ready (M-x navi-view)"))))

;;;###autoload
(defun navi (input &optional context context-lang)
  (interactive
   (let* ((region (and (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))))
          (lang (and region
                     (replace-regexp-in-string
                      "-\\(ts-\\)?mode\\'" "" (symbol-name major-mode)))))
     (list (read-string "navi> ") region lang)))
  (let* ((cfg (cdr (assq navi-backend navi-backends)))
         (endpoint (plist-get cfg :endpoint))
         (model (plist-get cfg :model))
         (key (navi--api-key (plist-get cfg :auth-host)))
         (full (if context
                   (format "```%s\n%s\n```\n\n%s"
                           (or context-lang "") context input)
                 input))
         (payload
          (json-encode
           `((model . ,model)
             (messages . [((role . "system") (content . ,navi-system-prompt))
                          ((role . "user") (content . ,full))]))))
         (proc (make-process
                :name "navi"
                :buffer (generate-new-buffer " *navi-curl*")
                :noquery t :coding 'utf-8 :connection-type 'pipe
                :command `("curl" "-sS" "-X" "POST"
                           "-H" "Content-Type: application/json"
                           ,@(when key (list "-H" (concat "Authorization: Bearer " key)))
                           "--data-binary" "@-" ,endpoint)
                :sentinel #'navi--sentinel)))
    (process-put proc :question full)
    (process-send-string proc payload)
    (process-send-eof proc)
    (message "navi: query sent (%s)..." navi-backend)))

(defun navi-view ()
  (interactive)
  (let ((buf (get-buffer navi-buffer-name)))
    (if buf (pop-to-buffer buf)
      (message "navi: no responses yet"))))

(defun navi-set-backend (backend)
  (interactive
   (list (intern (completing-read "Backend: "
                                  (mapcar #'car navi-backends) nil t))))
  (setq navi-backend backend)
  (message "navi: %s" backend))

(defvar navi-server-command
  `("llama-server"
    "-m" ,(expand-file-name "~/models/gemma-4-E2B-it-UD-Q4_K_XL.gguf")
    "--jinja"
    "--host" "127.0.0.1"
    "--port" "7777"
    "-c" "65536"
    "--metrics"
    "-ngl" "-1"
    "-np" "1"
    "--temp" "1"
    "--top-p" "0.95"
    "--top-k" "64"))

(defun navi-server ()
  "Start llama-server in the background."
  (interactive)
  (make-process :name "navi-server"
                :buffer (get-buffer-create "*navi-server*")
                :command navi-server-command)
  (message "navi: server started"))

(provide 'external)
