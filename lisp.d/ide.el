;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

;; vterm on bottom
(use-package vterm
  :bind
  (:map vterm-mode-map
        ;; Copy-Paste
        ("C-v" . vterm-yank)
        ("<mouse-2>" . vterm-yank-primary)
        ;; Delete
        ("<deletechar>" . vterm-send-delete)
        ;; PgUp & PgDn
        ("<next>" . vterm-send-next)
        ("<prior>" . vterm-send-prior))
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0)
                 )
               )
  )

(use-package vterm-toggle
  :bind (("<f2>" . vterm-toggle)
         ("<C-f2>" . vterm-toggle-cd))
    )

;; project - built-in, doesn't do anything else now
(use-package project
    :straight nil
    :config
    (setq project-vc-extra-root-markers '(".root")))

;; treemacs


;;(setq display-buffer-alist
;;      `(
;;    ("\\*eldoc\\*"
;;     (display-buffer-in-side-window)
;;     (window-height 0.25)
;;     (side . bottom)
;;     (slot . 2))
;;    ;;("\\*compilation\\*"
;;    ;; (display-buffer-in-side-window)
;;    ;; (window-height 0.3)
;;    ;; (side . bottom)
;;    ;; (display-buffer-reuse-window display-buffer-at-bottom)
;;    ;; (slot . 1))
;;    ))

;; code formatting
(when (not (config-wrap "use-eglot"))
  (use-package apheleia
    :config
    (apheleia-global-mode +1)))

;; GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

(provide 'my/ide)
