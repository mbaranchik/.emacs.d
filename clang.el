;;; -*- lexical-binding: t -*-

;;(load "/usr/local/Cellar/clang-format/2018-01-11/share/clang/clang-format.el")

;;(straight-use-package `(clang-format-on-save
;;  :local-repo ,(concat user-emacs-directory "lisp")))

(use-package clang-format+)

;; Enable LSP
(when use-lsp
  (use-package lsp-mode
    :commands lsp
    :hook
    ((c-mode c++-mode python-mode js-mode sh-mode) . (lambda () (hack-local-variables) (lsp)))
    :config
    (setq lsp-enable-file-watchers nil)
    (push "[/\\\\]\\.cquery_cached_index\\'" lsp-file-watch-ignored-directories)
    (push "[/\\\\][^/\\\\]*\\.\\(so\\|d\\|o\\)$" lsp-file-watch-ignored-files)
    (when (and use-lsp (equal my-lsp-c++-backend "cquery"))
      (use-package cquery))
    (when (and use-lsp (equal my-lsp-c++-backend "ccls"))
      (use-package ccls))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;

