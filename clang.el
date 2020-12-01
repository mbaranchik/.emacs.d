;;; -*- lexical-binding: t -*-

;;(load "/usr/local/Cellar/clang-format/2018-01-11/share/clang/clang-format.el")

;;(straight-use-package `(clang-format-on-save
;;  :local-repo ,(concat user-emacs-directory "lisp")))

(use-package clang-format+)

;; Enable LSP
(when use-lsp
  (use-package lsp-mode)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'sh-mode-hook #'lsp)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 65536))

(when (and use-lsp (equal my-lsp-c++-backend "cquery"))
  (use-package cquery))

(when (and use-lsp (equal my-lsp-c++-backend "ccls"))
  (use-package ccls))

;;;;;;;;;;;;;;;;;;;;;;;;

