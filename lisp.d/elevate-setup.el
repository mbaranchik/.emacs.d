;;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package elevate
  :straight nil  ;; Don't try to fetch from a remote repo
  :after prog-mode
  :commands (elevate-mode
             elevate-chat
             elevate-explain-code-with-context
             elevate-complete-here
             elevate-create-context
             elevate-switch-context
             elevate-list-contexts
             elevate-remove-context)
  :hook ((prog-mode . elevate-mode))
  :bind (:map elevate-mode-map
         ("C-c l a" . elevate-chat)
         ("C-c l e" . elevate-explain-code-with-context)
         ("C-c l c" . elevate-complete-here)
         ("C-c <tab>" . elevate-complete-here)
         ("C-c l n" . elevate-create-context)
         ("C-c l s" . elevate-switch-context)
         ("C-c l l" . elevate-list-contexts)
         ("C-c l r" . elevate-remove-context)
         ("C-c l m" . elevate-switch-chat-model)
         ("C-c l M" . elevate-switch-completion-model))
  :custom
  ;; Define modes to skip (non-programming modes where we don't want elevate)
  (elevate-ignored-modes
   '(fundamental-mode
     special-mode
     dired-mode
     help-mode
     Info-mode
     compilation-mode
     debugger-mode
     shell-mode
     term-mode
     vterm-mode
     eshell-mode
     magit-mode
     diff-mode
     log-view-mode))
  ;; Set enabled-modes to nil to use the ignored-modes logic
  (elevate-enabled-modes nil)
  :config
  ;; Ensure contexts directory exists
  (elevate-ensure-contexts-directory)

  ;; Disable global mode since we're using hook-based activation
  (global-elevate-mode -1))

(provide 'my/elevate-setup)
