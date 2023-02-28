;;; -*- lexical-binding: t -*-

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
;;(global-tab-line-mode t)

(xterm-mouse-mode 1)

(use-package multiple-cursors
  :commands multiple-cursors-mode)

(use-package highlight-symbol
  :bind (("<C-f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<M-f3>" . highlight-symbol-query-replace)))

(use-package back-button
  :config
  (back-button-mode 1))

(message "Display: %s" (display-graphic-p))
(message "Window-System: %s" window-system)

;; Centaur-Tabs
(daemon-wrap my/load-centaur
             (progn
               (use-package centaur-tabs
                 :demand
                 :bind
                 ([M-left] . centaur-tabs-backward)
                 ([M-right] . centaur-tabs-forward)
                 :init
                 (centaur-tabs-mode nil)
                 (setq centaur-tabs-style "bar"
	                   centaur-tabs-height 32
	                   centaur-tabs-set-icons t
	                   centaur-tabs-set-bar 'left
	                   x-underline-at-descent-line t)
                 (centaur-tabs-mode t)
                 (centaur-tabs-headline-match)
                 (centaur-tabs-group-by-projectile-project)
                 )
               )
             )

;; Treemacs
(use-package treemacs
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

