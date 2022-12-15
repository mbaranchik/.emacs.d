;;; -*- lexical-binding: t -*-

;;(load "/usr/local/Cellar/clang-format/2018-01-11/share/clang/clang-format.el")

;;(straight-use-package `(clang-format-on-save
;;  :local-repo ,(concat user-emacs-directory "lisp")))

(use-package clang-format+)

(if use-flycheck
  (use-package flycheck
    :demand
    :hook (prog-mode . flycheck-mode)))

(if use-flymake
  (use-package flymake
    :demand
    :hook (prog-mode . flymake-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;
