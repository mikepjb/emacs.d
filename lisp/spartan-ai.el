(defun +local-llm ()
  (interactive)
  (make-process
   :name "local-llm"
   :buffer (get-buffer-create "*local-llm*")
   :command `("llama-server"
              "-m" ,(expand-file-name "~/models/gemma-4-E2B-it-UD-Q4_K_XL.gguf")
              "--host" "127.0.0.1"
              "--port" "7777"
              "-c" "32768"
              "--metrics"
              "-ngl" "1"
              "-np" "1"
              "--temp" "0.7"
              "--top-p" "0.95"
              "--top-k" "64")))

(use-package gptel
  :ensure t
  :custom
  gptel-directives
  `((default . ,(lambda ()
                  (with-temp-buffer
                    (insert-file-contents
                     (concat user-emacs-directory "assistant.md"))
                    (buffer-string)))))
  :config
  (gptel-make-openai "llama.cpp"
    :host "127.0.0.1:7777"
    :protocol "http"
    :endpoint "/v1/chat/completions"
    :stream t
    :models '(gemma4-e2b))

  (gptel-make-openai "fireworks"
    :host "api.fireworks.ai"
    :endpoint "/inference/v1/chat/completions"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "api.fireworks.ai"))
    :models '(accounts/fireworks/models/kimi-k2p5
              accounts/fireworks/models/glm-4p7))

  (gptel-make-openai "gemini"
    :host "generativelanguage.googleapis.com"
    :endpoint "/v1beta/openai/chat/completions"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "generativelanguage.googleapis.com"))
    :models '(gemini-3-flash-preview))

  (setq
   gptel-backend (gptel-get-backend "llama.cpp")
   gptel-model  'gemma4-e2b)

  (add-hook 'gptel-mode-hook #'visual-line-mode))

(provide 'spartan-ai)
;;; spartan-ai.el ends here.
