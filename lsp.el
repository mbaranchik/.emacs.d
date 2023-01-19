;;; -*- lexical-binding: t -*-

;; Enable LSP
(when use-lsp
  (use-package lsp-mode
    :commands lsp
    :hook
    ((c-mode c++-mode python-mode sh-mode) . (lambda () (hack-local-variables) (lsp) (which-function-mode)))
    (lsp-mode . lsp-enable-which-key-integration)
    :config
    (setq lsp-headerline-breadcrumb-segments '(symbols))
    (set-config-var 'lsp-headerline-breadcrumb-enable nil "EMACS_LSP_BREADCRUMB")
    (setq lsp-enable-semantic-tokens nil)
    (setq lsp-enable-file-watchers nil)
    (setq lsp-lens-enable nil)
    (setq lsp-lens-auto-enable nil)
    (push "[/\\\\]\\.cquery_cached_index\\'" lsp-file-watch-ignored-directories)
    (push "[/\\\\][^/\\\\]*\\.\\(so\\|d\\|o\\)$" lsp-file-watch-ignored-files)
    (when (string= my-lsp-c++-backend "ccls")
      (use-package ccls))
    ;; TODO: pyright hangs
    ;;(use-package lsp-pyright
    ;;  :config
    ;;  (add-to-list 'lsp-disabled-clients 'pyls)
    ;;  (add-to-list 'lsp-disabled-clients 'jedi)
    ;;  (add-to-list 'lsp-disabled-clients 'mspyls)
    ;;  (add-to-list 'lsp-enabled-clients 'pyright)
    ;;  (setq lsp-pyright-log-level "info")
    ;;  (setq lsp-pyright-diagnostic-mode "openFilesOnly")
    ;;  (setq lsp-pyright-python-executable-cmd "/usr/local/bin/python3.9")
    ;;  )
    (add-to-list 'lsp-enabled-clients 'bash-ls)
    (add-to-list 'lsp-enabled-clients 'ccls)
    (add-to-list 'lsp-enabled-clients 'pyls)
    )
  )

(when use-eglot
  ;;(use-package project
  ;;  :straight (:type built-in)
  ;;  )
  (use-package markdown-mode)
  (unless (fboundp 'project-root)
    (defun project-root (project)
      (car (project-roots project)))
    )
  (use-package eglot
;;    :straight (eglot
;;               :type git
;;               :flavor melpa
;;               :host github
;;               :repo "joaotavora/eglot")
    :commands eglot-ensure
    :hook
    ((c-mode c++-mode python-mode sh-mode) . (lambda () (hack-local-variables) (eglot-ensure) (which-function-mode)))
    :config
    (when (string= my-lsp-c++-backend "ccls")
      (use-package ccls))
    )
  )

(when use-lsp-bridge
  (use-package posframe)
  (use-package markdown-mode)
  (use-package lsp-bridge
    :straight (:host github
                     :repo "manateelazycat/lsp-bridge"
                     :files ("*"))
    :hook
    ;;((c-mode c++-mode python-mode sh-mode lisp-mode) . (lambda () (hack-local-variables) (lsp-bridge-mode) (which-function-mode)))
    ((prog-mode) . (lambda () (hack-local-variables) (lsp-bridge-mode) (which-function-mode)))
    :init
    (straight-use-package '(acm
                            :local-repo "lsp-bridge/acm" :type nil
			                :files ("*")))
    (setq acm-frame-background-dark-color t)
    (setq acm-frame-background-light-color nil)

    (setq tab-always-indent t)
    (defun lsp-bridge-indent-for-tab-command (&optional arg)
      (interactive "P")
      (lsp-bridge-popup-complete-menu)
      (indent-for-tab-command arg))
    :custom
    (lsp-bridge-c-lsp-server my-lsp-c++-backend)
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;
