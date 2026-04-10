;; 2 purposes

;; 1. spartan-environment is about managing your environment
;; - setting up bash
;; - PATH
;; - mac/linux diffs/normalisation

;; 2. also a temp store for config organisation.

;; - vars i.e global minor modes
;; - ui configuration
;; - editor/editing configuration
;; - coding tools - i.e trace fn

(dolist (meta-key '(mac-command-modifier x-super-keysym))
  (when (boundp meta-key)
    (set meta-key 'meta)))

(provide 'spartan-environment)
