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

(defun navi ()
  (interactive)
  ;; if buffer doesn't exist, create it
  ;; either way, open it using (other-window-prefix) so it doesn't open in the current buffer.
  ;; ..guess that's it for now
  )

(provide 'navi)
;;; navi.el ends here
