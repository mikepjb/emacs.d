;;; Package --- navi -------- -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; A lightweight client for working with OpenAI compatible LLM
;;; servers like llama.cpp.

;;; Code:

(defvar navi-buffer-name "*navi*"
  "Default name for a navi buffer.")

(defvar navi-setup-hook nil
  "Hook to run commands after navi buffer is created.")

(defvar-local navi-output-marker nil
  "Where streaming output gets inserted.")

(defvar-local navi-message-log nil
  "Conversation history for this buffer, as a list of plists.")

(defvar navi-llm-config
  (list :model "gemma4"
        :host "127.0.0.1"
        :port 7777))

(defun navi-buffer ()
  "Get or create & prepare a new buffer for navi."
  (or (get-buffer navi-buffer-name)
      (let ((buf (get-buffer-create navi-buffer-name)))
        (with-current-buffer buf
          (setq navi-output-marker (make-marker))
          (set-marker navi-output-marker (point-max))
          (set-marker-insertion-type navi-output-marker t)
          (setq navi-message-log nil))
        buf)))

(defun navi-send ()
  (with-current-buffer (navi-buffer)
    (setq navi-message-log
          (append navi-message-log
                  (list (list :role "user" :content input))))
    (goto-char (point-max))
    (insert (format "\n>>> %s\n\n"))
    (set-marker navi-output-marker (point-max)))

  (let ((body (json-serialize
               `(:model "gemma4" ;; take from navi-llm-config
                 :stream t
                 :messages ,(vconcat (buffer-local-value 'navi-message-log (navi-buffer))))))
        (display-buffer (navi-buffer))
        (make-process
         :name "navi-llm-request"
         :buffer nil
         :command
         `("curl" "-sS" "-N"
           "-H" "Content-Type: application/json"
           "-d" ,body
           (format "http://%s:%s/v1/chat/completions"
                   (plist-get navi-llm-config :host)
                   (plist-get navi-llm-config :port)))
         :filter (lambda (proc chunk) (navi--handle-chunk (navi-buffer) chunk))
         :sentinel (lambda (proc event)
                     (when (string-match-p "finished" event)))))))

(defun navi--llm-response-finished (buf)
  ;; TODO also append assistant/llm message to history
  (insert "\n\n>>>"))

(defun navi--handle-chunk (buf chunk)
  "Decode SSE events sent back from the LLM."
  (message "ok"))

;; (defun navi ()
;;   (interactive)
;;   ;; if buffer doesn't exist, create it
;;   ;; either way, open it using (other-window-prefix) so it doesn't open in the current buffer.
;;   ;; ..guess that's it for now
;;   )

(provide 'navi)
;;; navi.el ends here
