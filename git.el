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

(defun my/load-git-gutter ()
  "loads git-gutter-mode if file has a vc-backend"
  (when (string= "Git" (vc-backend (buffer-file-name)))
    (git-gutter-mode)))

(use-package git-gutter
  :hook (prog-mode . my/load-git-gutter)
  :config
  (setq git-gutter:update-interval 0))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; (add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))

