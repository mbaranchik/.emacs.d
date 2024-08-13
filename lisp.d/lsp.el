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
      (when (not (member type (config-wrap "lsp/diagnostics-enable-modes")))
        (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t)
        (flymake-mode 0)
        (add-to-list (make-local-variable 'eglot-stay-out-of) 'flymake))
      (when (not (member type (config-wrap "lsp/autoformat-enable-modes")))
        (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentOnTypeFormattingProvider)
        (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentFormattingProvider)
        (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentRangeFormattingProvider))
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
    :demand t
    :hook
    ((my/c-mode) . (lambda () (my/eglot-enable 'c)))
    ((my/c++-mode) . (lambda () (my/eglot-enable 'cpp)))
    ((my/python-mode) . (lambda () (my/eglot-enable 'python)))
    ((my/sh-mode) . (lambda () (my/eglot-enable 'bash)))
    :config
    ;;(push `((,(my/get-mode "c") ,(my/get-mode "c++") ,(my/get-mode "python")) . ("cwls")) eglot-server-programs))
    (pcase (config-wrap "lsp/cpp-backend")
      ("ccls" (push `((,(my/get-mode "c") ,(my/get-mode "c++")) . ("ccls")) eglot-server-programs))
      ("clangd" (push `((,(my/get-mode "c") ,(my/get-mode "c++")) . ("clangd")) eglot-server-programs))
      (_ (warn "Unknown option for 'lsp/cpp-backend': %s" (config-wrap "lsp/cpp-backend"))))
    (pcase (config-wrap "lsp/py-backend")
      ("pylsp" (push `((,(my/get-mode "python")) . ("pylsp")) eglot-server-programs))
      ("pyright" (push `((,(my/get-mode "python")) . ("pyright-langserver" "--stdio")) eglot-server-programs))
      (_ (warn "Unknown option for 'lsp/py-backend': %s" (config-wrap "lsp/py-backend")))))
  (use-package eglot-booster
    :straight (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))
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
    ((my/c-mode) . (lambda () (my/lsp-bridge-enable 'c)))
    ((my/c++-mode) . (lambda () (my/lsp-bridge-enable 'cpp)))
    ((my/python-mode) . (lambda () (my/lsp-bridge-enable 'python)))
    ((my/sh-mode) . (lambda () (my/lsp-bridge-enable 'bash)))
    :init
    (setq tab-always-indent t)
    (defun lsp-bridge-indent-for-tab-command (&optional arg)
      (interactive "P")
      (lsp-bridge-popup-complete-menu)
      (indent-for-tab-command arg))
    :config
    (push "find-file" lsp-bridge-completion-stop-commands)
    (setq lsp-bridge-python-command (string-trim
                                     (shell-command-to-string "pyenv which python3")))
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

(when (config-wrap "use-lsp-mode")
  (use-package lsp-mode
    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
           ((my/c-mode) . lsp)
           ((my/c++-mode) . lsp)
           ((my/python-mode) . lsp)
           ((my/sh-mode) . lsp)
           ;; if you want which-key integration
           (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp
    :config
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "cwls")
                      :major-modes `(,(my/get-mode "python")
                                     ,(my/get-mode "sh")
                                     ,(my/get-mode "c")
                                     ,(my/get-mode "c++")
                                     )
                      ;; Allow other servers to run in parallel
                      :add-on? t
                      :server-id 'cwls))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my/lsp)
