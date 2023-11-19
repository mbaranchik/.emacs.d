;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(when (config-wrap "use-eglot")
  (defun my/eglot-enable (type)
    (when (member type (config-wrap "lsp/enable-modes"))
      (hack-local-variables)
      (when (config-wrap "use-which-function")
        (which-function-mode))
      (add-hook 'eglot-managed-mode-hook (lambda () (when (not (member type (config-wrap "lsp/autoformat-enable-modes")))
                                                      (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentFormattingProvider)
                                                      (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentRangeFormattingProvider))))
      (when (member type (config-wrap "lsp/autoformat-enable-modes"))
        (add-hook 'eglot-managed-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))
      (eglot-ensure)))
  (use-package markdown-mode)
  (unless (fboundp 'project-root)
    (defun project-root (project)
      (car (project-roots project)))
    )
  (use-package eglot
    :straight nil
    :commands eglot-ensure
    :hook
    ((my/c-mode) . (lambda () (my/eglot-enable 'c)))
    ((my/c++-mode) . (lambda () (my/eglot-enable 'cpp)))
    ((my/python-mode) . (lambda () (my/eglot-enable 'python)))
    ((my/sh-mode) . (lambda () (my/eglot-enable 'bash)))
    :config
    (pcase (config-wrap "lsp/cpp-backend")
      ("ccls" (push '((my/c-mode my/c++-mode) . ("ccls")) eglot-server-programs))
      ("clangd" (push '((my/c-mode my/c++-mode) . ("clangd")) eglot-server-programs))
      (_ (warn "Unknown option for 'lsp/cpp-backend': %s" (config-wrap "lsp/cpp-backend"))))
    (pcase (config-wrap "lsp/py-backend")
      ("pylsp" (push '((my/python-mode) . ("pylsp")) eglot-server-programs))
      ("pyright" (push '((my/python-mode) . ("pyright")) eglot-server-programs))
      (_ (warn "Unknown option for 'lsp/py-backend': %s" (config-wrap "lsp/py-backend")))))
  )

(when (config-wrap "use-lsp-bridge")
  (defun my/lsp-bridge-enable (type)
    (when (member type (config-wrap "lsp/enable-modes"))
      (hack-local-variables)
      (lsp-bridge-mode)
      (when (config-wrap "use-which-function")
        (which-function-mode))
      (when (config-wrap "use-flycheck")
        (flycheck-mode nil))
      (when (config-wrap "use-flymake")
        (flymake-mode nil))))
  (use-package markdown-mode)
  (use-package lsp-bridge
    :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))

    :after (markdown-mode yasnippet)
    :hook
    ((c-mode c-ts-mode) . (lambda () (my/lsp-bridge-enable 'c)))
    ((c++-mode c++-ts-mode) . (lambda () (my/lsp-bridge-enable 'cpp)))
    ((python-mode python-ts-mode) . (lambda () (my/lsp-bridge-enable 'python)))
    ((sh-mode bash-ts-mode) . (lambda () (my/lsp-bridge-enable 'bash)))
    :init
    (setq tab-always-indent t)
    (defun lsp-bridge-indent-for-tab-command (&optional arg)
      (interactive "P")
      (lsp-bridge-popup-complete-menu)
      (indent-for-tab-command arg))
    :config
    (push "find-file" lsp-bridge-completion-stop-commands)
    :custom
    (lsp-bridge-c-lsp-server (config-wrap "lsp/cpp-backend"))
    (lsp-bridge-enable-hover-diagnostic t)
    (acm-enable-yas t)
    (acm-enable-tabnine nil)
    :bind (
           ("M-i"   . 'lsp-bridge-find-impl)
           ("M-I"   . 'lsp-bridge-find-impl-other-window)
           ("M-t"   . 'lsp-bridge-find-def)
           ("M-,"   . 'lsp-bridge-find-def-return)
           ("M-T"   . 'lsp-bridge-find-def-other-window)
           ("M-r"   . 'lsp-bridge-find-references)
           ("<tab>" . 'lsp-bridge-indent-for-tab-command)
           )
    )
  (daemon-wrap my/load-acm-terminal
               (unless (display-graphic-p)
                 (use-package popon
                   :straight (:host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
                 (use-package acm-terminal
                   :straight (:host github :repo "twlz0ne/acm-terminal")
                   :after (lsp-bridge popon))
                 ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my/lsp)
