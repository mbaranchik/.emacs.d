;;; -*- lexical-binding: t -*-

;; vterm on bottom
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

;; treemacs

;; 

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

