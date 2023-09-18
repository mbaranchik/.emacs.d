;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;;(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)

(use-package whitespace-mode
  :hook (prog-mode . whitespace-mode))

;;(add-hook 'vterm-mode-hook (lambda ()
;;                             (whitespace-mode nil)
;;                             )
;;          )

(use-package indent-guide
  :if (config-wrap "use-indent-guide")
  :hook (prog-mode . indent-guide-mode))

(provide 'my/whitespace)
