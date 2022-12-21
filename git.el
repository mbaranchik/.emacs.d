;;; -*- lexical-binding: t -*-

;; TODO - revisit git gutter when stable

(use-package magit
  :mode ("/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode)
  :commands (magit magit-status vc-git-log-edit-mode)
  :config
  (use-package magit-gerrit
    :ensure t
    )
  )

;; (add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))

