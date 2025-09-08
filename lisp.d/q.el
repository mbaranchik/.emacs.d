;;; -*- lexical-binding: t -*-

(when (string= (config-wrap "lsp/package") "eglot")
  (use-package amz-q-macs
    :straight '(amz-q-macs
                 :type git
                 :host nil
                 :branch "main"
                 :repo "ssh://git.amazon.com:2222/pkg/Amz-q-macs"
                 ;; :local-repo ,(format "%s/Amz-q-macs" user-emacs-directory)
                 ;; :type nil
                 ;;   :build t
                   )
    :after (eglot lsp-multi)
    :demand t
    :bind
    ("C-c <tab>" . amz-q-macs-complete-here)
    ("C-c q c" . amz-q-toggle-chat)
    :custom
    (amz-q-lsp-client 'eglot)
    (amz-q-lsp-include-derived-modes nil)
    (amz-q-lsp-auth-method 'identity)
    (amz-q-lsp-debug-level 'info)
    (amz-q-lsp-server-debug-level 'info)
    :config
    (amz-q-lsp-setup)))

(when (string= (config-wrap "lsp/package") "lsp-mode")
  (use-package amz-q-macs
    :straight '(amz-q-macs
                 :type git
                 :host nil
                 :branch "main"
                 :repo "ssh://git.amazon.com:2222/pkg/Amz-q-macs"
                 ;; :local-repo ,(format "%s/Amz-q-macs" user-emacs-directory)
                 ;; :type nil
                 ;; :build t
                   )
    :after lsp-mode
    :demand t
    :bind
    ("C-c <tab>" . amz-q-macs-complete-here)
    ("C-c q c" . amz-q-toggle-chat)
    :custom
    (amz-q-lsp-client 'lsp-mode)
    (amz-q-lsp-supported-modes '(prog-mode))
    (amz-q-lsp-include-derived-modes t)
    (amz-q-lsp-auth-method 'identity)
    (amz-q-lsp-debug-level 'info)
    (amz-q-lsp-server-debug-level 'info)
    (lsp-response-timeout nil)
    :config
    (amz-q-lsp-setup)
))

(provide 'my/q)
