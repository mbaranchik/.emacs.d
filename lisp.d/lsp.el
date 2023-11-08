;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(when (config-wrap "use-eglot")
  (defun my/eglot-enable ()
    (eglot-ensure))
  (use-package markdown-mode)
  (unless (fboundp 'project-root)
    (defun project-root (project)
      (car (project-roots project)))
    )
  (use-package eglot
    :straight nil
    :commands eglot-ensure
    :hook
    ((c-mode
      c-ts-mode
      c++-mode
      c++-ts-mode
      python-mode
      python-ts-mode
      sh-mode
      bash-ts-mode) . (lambda ()
                               (hack-local-variables)
                               (my/eglot-enable)
                               (if (config-wrap "use-which-function") (which-function-mode))
                               (when (or (string= major-mode "python-mode") (string= major-mode "python-ts-mode"))
                                 (setq-local eglot-ignored-server-capabilities '(:documentFormattingProvider :documentRangeFormattingProvider)))))
    :config
    (push '(python-mode . ("pyright")) eglot-server-programs)
    (when (string= (config-wrap "lsp/cpp-backend") "ccls")
      (push '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("ccls")) eglot-server-programs))
    )
    (add-hook 'eglot-managed-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  )

(when (config-wrap "use-lsp-bridge")
  (use-package markdown-mode)
  (use-package lsp-bridge
    :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))

    :after (markdown-mode yasnippet)
    :hook
    ((prog-mode text-mode) . (lambda ()
                               (hack-local-variables)
                               (lsp-bridge-mode)
                               (if (config-wrap "use-which-function") (which-function-mode))
                               (when (config-wrap "use-flycheck") (flycheck-mode nil))
                               (when (config-wrap "use-flymake") (flymake-mode nil))))
    :init
    (setq tab-always-indent t)
    (defun lsp-bridge-indent-for-tab-command (&optional arg)
      (interactive "P")
      (lsp-bridge-popup-complete-menu)
      (indent-for-tab-command arg))
    :config
    (push "find-file" lsp-bridge-completion-stop-commands)
    (push "projectile-find-file" lsp-bridge-completion-stop-commands)
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
