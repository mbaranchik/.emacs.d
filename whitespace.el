;;; -*- lexical-binding: t -*-

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(global-whitespace-mode 1)

(use-package indent-guide)
(when use-indent-guide
  (indent-guide-global-mode))

