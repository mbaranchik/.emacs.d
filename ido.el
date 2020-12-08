;;; -*- lexical-binding: t -*-

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-create-new-buffer 'always)
  (ido-mode 1)
  (ido-vertical-mode))

