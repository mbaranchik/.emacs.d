;;; -*- lexical-binding: t -*-

(use-package git-gutter-fringe+
  :demand
  :hook (prog-mode . git-gutter+-mode)
  :config
  (setq git-gutter-fr+-side 'right-fringe)
  )

(use-package magit
  :commands (magit magit-status))



