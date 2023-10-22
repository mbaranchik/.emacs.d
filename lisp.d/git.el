;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(use-package magit
  :mode ("/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode)
  :commands (magit magit-status vc-git-log-edit-mode)
  )

(when (config-wrap "use-git-gutter")
  (defun my/load-git-gutter ()
    "loads git-gutter-mode if file has a vc-backend"
    (when (string= "Git" (vc-backend (buffer-file-name)))
      (git-gutter-mode)))

  (use-package git-gutter
    :config
    (setq git-gutter:update-interval 2)
    (add-hook 'find-file-hook 'my/load-git-gutter))

  (use-package git-gutter-fringe
    :after git-gutter
    :config
    (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
  )

(when (config-wrap "use-diff-hl")
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

;;(use-package blamer
;;  :straight (:host github :repo "artawower/blamer.el")
;;  :bind (("s-i" . blamer-show-commit-info))
;;  :custom
;;  (blamer-idle-time 0.3)
;;  (blamer-min-offset 70)
;;  (blamer-max-lines 2)
;;  :custom-face
;;  (blamer-face ((t :foreground "#7a88cf"
;;                    :background nil
;;                    :height 140
;;                    :italic t)))
;;  :config
;;  (global-blamer-mode 1))

;; (add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))

(provide 'my/git)
