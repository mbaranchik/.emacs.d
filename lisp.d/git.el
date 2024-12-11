;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(use-package magit
  :mode ("/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode)
  :commands (magit magit-status vc-git-log-edit-mode)
  )

(when (config-wrap "ui/diff-hl")
  (use-package diff-hl
    :custom
    (diff-hl-disable-on-remote t)
    :config
    (global-diff-hl-mode)
    (global-diff-hl-show-hunk-mouse-mode)
    ;;(add-hook 'find-file-hook 'diff-hl-flydiff-mode) ;; Use flydiff for unsaved changes diff
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    )
  )

(provide 'my/git)
