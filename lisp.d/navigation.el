;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

;;(unless (boundp 'aquamacs-version)
;;  (use-package tabbar)
;;  (tabbar-mode 1)
;;  ;;(setq tabbar-ruler-global-tabbar t)    ; get tabbar
;;  ;;(setq tabbar-ruler-global-ruler t)     ; get global ruler
;;  ;;(setq tabbar-ruler-popup-menu t)       ; get popup menu.
;;  ;;(setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;;  ;;(setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
;;  ;;(use-package tabbar-ruler)
;;  )

;; Enable Tab-Line Mode (Emacs >= 27)
(daemon-wrap my/load-tab-line
             (progn
               (defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
                 (concat (nerd-icons-powerline "nf-ple-forwardslash_separator")
                         " "
                         (nerd-icons-icon-for-file (buffer-name buffer))
                         (format " %s " (buffer-name buffer))
                         (nerd-icons-powerline "nf-ple-backslash_separator")
                         )
                 )
               (use-package tab-line
                 :straight nil
                 :demand
                 :custom
                 (tab-line-switch-cycling t)
                 :custom-face
                 :bind
                 ([M-left] . tab-line-switch-to-prev-tab)
                 ([M-right] . tab-line-switch-to-next-tab)
                 :config
                 (global-tab-line-mode t)
                 (setq
                  tab-line-new-button-show nil  ;; do not show add-new button
                  tab-line-close-button-show nil  ;; do not show close button
                  tab-line-separator "|"  ;; delimitation between tabs
                  tab-line-tab-name-function #'my/tab-line-tab-name-buffer
                  ))
               )
             )

(use-package multiple-cursors
  :commands multiple-cursors-mode)

(use-package symbol-overlay
  :bind (("<C-f3>" . symbol-overlay-put)
         ("<f3>" . symbol-overlay-jump-next)
         ("<S-f3>" . symbol-overlay-jump-prev)
         ("<M-f3>" . symbol-overlay-rename))
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

;; Centaur-Tabs
;;(daemon-wrap my/load-centaur
;;             (progn
;;               (use-package centaur-tabs
;;                 :demand
;;                 :bind
;;                 ([M-left] . centaur-tabs-backward)
;;                 ([M-right] . centaur-tabs-forward))
;;               (centaur-tabs-mode nil)
;;               (setq centaur-tabs-style "bar"
;;	                 centaur-tabs-height 32
;;	                 centaur-tabs-set-icons t
;;	                 centaur-tabs-set-bar 'left
;;	                 x-underline-at-descent-line t)
;;               (centaur-tabs-mode t)
;;               (centaur-tabs-headline-match)
;;               )
;;             )

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

(provide 'my/navigation)
