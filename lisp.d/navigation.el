;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(use-package multiple-cursors
  :commands multiple-cursors-mode)

(use-package symbol-overlay
  :bind (("<C-f3>" . symbol-overlay-put)
         ("<f3>" . symbol-overlay-jump-next)
         ("<S-f3>" . symbol-overlay-jump-prev)
         ("<M-f3>" . symbol-overlay-query-replace))
  :config
  (setq symbol-overlay-inhibit-map t)
  :custom-face
  ;; (symbol-overlay-default-face ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-1 ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-2 ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-3 ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-4 ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-5 ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-6 ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-7 ((t (:foreground "#808080"))))
  ;; (symbol-overlay-face-8 ((t (:foreground "#808080"))))
  )

;; (transient-define-prefix symbol-overlay-transient ()
;;   "Symbol Overlay transient"
;;   ["Symbol Overlay"
;;    ["Overlays"
;;     ("." "Add/Remove at point" symbol-overlay-put)
;;     ("k" "Remove All" symbol-overlay-remove-all)
;;     ]
;;    ["Move to Symbol"
;;     ("n" "Next" symbol-overlay-jump-next)
;;     ("p" "Previous" symbol-overlay-jump-prev)
;;     ]
;;    ["Other"
;;     ("m" "Highlight symbol-at-point" symbol-overlay-mode)
;;     ]
;;    ]
;;   )
;; (global-set-key (kbd "s-.") 'symbol-overlay-transient)

;;(use-package back-button
;;  :config
;;  (back-button-mode 1))

(message "Display: %s" (display-graphic-p))
(message "Window-System: %s" window-system)

;; Treemacs
(use-package treemacs
  :bind
  ("M-s M-s" . treemacs)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil)
  )

(use-package treemacs-magit
    :after (treemacs magit))

(use-package ultra-scroll
    :straight (:host github :repo "jdtsmith/ultra-scroll")
    :init
    (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
    :config
    (ultra-scroll-mode 1))

(provide 'my/navigation)
