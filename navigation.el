;;; -*- lexical-binding: t -*-
(unless (boundp 'aquamacs-version)
  (use-package tabbar)
  (tabbar-mode 1)
  ;;(setq tabbar-ruler-global-tabbar t)    ; get tabbar
  ;;(setq tabbar-ruler-global-ruler t)     ; get global ruler
  ;;(setq tabbar-ruler-popup-menu t)       ; get popup menu.
  ;;(setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
  ;;(setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
  ;;(use-package tabbar-ruler)
  )

(use-package multiple-cursors)

(use-package highlight-symbol)

