;;; -*- lexical-binding: t -*-

;;(load "/usr/local/Cellar/clang-format/2018-01-11/share/clang/clang-format.el")

;;(straight-use-package `(clang-format-on-save
;;  :local-repo ,(concat user-emacs-directory "lisp")))

(use-package clang-format+)

(if use-flycheck
  (use-package flycheck
    :hook (prog-mode . flycheck-mode)
    :config
    (use-package flycheck-clang-tidy
    	:hook
    	(flycheck-mode . flycheck-clang-tidy-setup)
    	:config
    	(setq flycheck-clang-tidy-executable my/flycheck-clang-tidy-executable)
    	(setq flycheck-clang-tidy-build-path "."))
    )
  )

(if use-flymake
  (use-package flymake
    :hook (prog-mode . flymake-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;
