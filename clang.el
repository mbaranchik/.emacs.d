;;; -*- lexical-binding: t -*-

;;(load "/usr/local/Cellar/clang-format/2018-01-11/share/clang/clang-format.el")

(use-package clang-format)

(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the project root."
  (when (and (locate-dominating-file default-directory ".clang-format") (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)))
    (clang-format-buffer)))

(define-minor-mode clang-format-on-save-mode
  "automatically execute git-sync when editing file's path contains .sync"
  :lighter " ClangFormatHook"
  :global t
  (cond (clang-format-on-save-mode
         (add-hook 'before-save-hook 'clang-format-buffer-smart))
        (t
         (remove-hook 'before-save-hook 'clang-format-buffer-smart))))

;; Enable LSP
(when use-lsp
  (use-package lsp-mode)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp))

(when (and use-lsp (equal my-lsp-c++-backend "cquery"))
  (use-package cquery))

(when (and use-lsp (equal my-lsp-c++-backend "ccls"))
  (use-package ccls))

(provide 'clang-format-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;

