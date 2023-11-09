;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(pcase (config-wrap "theme-name")
  ((pred (string-match "zenburn")) (use-package zenburn-theme))
  ((pred (string-match "atom")) (use-package atom-dark-theme))
  ((pred (string-match "spacemacs")) (use-package spacemacs-theme))
  ((pred (string-match "vscode")) (use-package vscode-dark-plus-theme))
  ((pred (string-match "doom")) (use-package doom-themes))
  )

(when (not (string= (config-wrap "theme-name") "default"))
  (daemon-wrap "my/load-theme"
               (bench-wrap "Enable main theme"
                           (load-theme (config-wrap "theme-sym") t t)
                           (enable-theme (config-wrap "theme-sym")))))

(bench-wrap "Load nerd-icons"
       (use-package nerd-icons
         :custom
         ;; The Nerd Font you want to use in GUI
         ;; "Symbols Nerd Font Mono" is the default and is recommended
         ;; but you can use any other Nerd Font if you want
         (nerd-icons-font-family "Symbols Nerd Font Mono")
         )
       (use-package treemacs-nerd-icons
         :after treemacs)
       (use-package nerd-icons-completion)
       (use-package nerd-icons-dired)
       (use-package nerd-icons-ibuffer))

(when (config-wrap "use-doom-modeline")
  (bench-wrap "Doom-Modeline"
         (use-package doom-modeline
           :demand
           :config
           ;; Fix for symlink expanding
           (setq doom-modeline-project-detection 'project)
           ;; Don’t compact font caches during GC.
           (setq inhibit-compacting-font-caches t)
           (doom-modeline-mode)
           )
         )
  )
(when (config-wrap "use-mood-modeline")
  (bench-wrap "Mood-Modeline"
              (use-package mood-line
                :config
                (mood-line-mode)
                )
              )
  )

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t)
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t))

(when (fboundp 'scroll-bar-mode)
(bench-wrap "Scroll-bar-mode"
       (scroll-bar-mode -1))
  )


;; Set Title Bar
(setq frame-title-format "%b")

(provide 'my/theme)
