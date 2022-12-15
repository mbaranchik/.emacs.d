;;; -*- lexical-binding: t -*-

;; TODO - revisit git gutter when stable

(use-package magit
  :commands (magit magit-status vc-git-log-edit-mode))

(use-package gerrit
  :commands (magit magit-status gerrit-dashboard gerrit-upload gerrit-download)
  )

(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))

