;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

;;(load "/usr/local/Cellar/clang-format/2018-01-11/share/clang/clang-format.el")

;;(straight-use-package `(clang-format-on-save
;;  :local-repo ,(concat user-emacs-directory "lisp")))

;; (use-package clang-format+)

(when (config-wrap "use-flycheck")
  (use-package flycheck
    :hook (prog-mode . flycheck-mode)
    :config
    (use-package flycheck-clang-tidy
    	:hook
    	(flycheck-mode . flycheck-clang-tidy-setup)
    	:config
    	(setq flycheck-clang-tidy-executable (config-wrap "flycheck-clang-tidy-executable"))
    	(setq flycheck-clang-tidy-build-path "."))
    )
  )

(when (config-wrap "use-flymake")
  (use-package flymake
    :hook (prog-mode . flymake-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my/clang)
