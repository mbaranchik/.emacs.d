;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(defun my/load-theme ()
  (when (not (string= (config-wrap "theme-name") "default"))
    (bench-wrap "Enable main theme"
                (load-theme (config-wrap "theme-sym") t)
                (when (memq window-system '(ns))
                  (customize-set-variable 'ns-antialias-text t)))))

(pcase (config-wrap "theme-name")
  ((pred (string-match "zenburn")) (use-package zenburn-theme
                                     :config
                                     (my/load-theme)))
  ((pred (string-match "atom")) (use-package atom-dark-theme
                                  :config
                                  (my/load-theme)))
  ((pred (string-match "spacemacs")) (use-package spacemacs-theme
                                       :config
                                       (my/load-theme)))
  ((pred (string-match "vscode")) (use-package vscode-dark-plus-theme
                                    :config
                                    (my/load-theme)))
  ((pred (string-match "doom")) (use-package doom-themes
                                  :config
                                  (my/load-theme)))
  ((pred (string-match "modus")) (use-package modus-themes
                                   :config
                                   (my/load-theme)))
  ((pred (string-match "ef")) (use-package ef-themes
                                   :config
                                   (my/load-theme)))
  (_ (my/load-theme)))

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


;; Enable Tab-Line Mode (Emacs >= 27)
(use-package s)
(use-package powerline)

(defun my/tab-line-buffer-group (buffer)
  "Use the project.el name for the buffer group"
  (with-current-buffer buffer
    (if (project-current)
        (s-chop-suffix "/" (project-root (project-current)))
      "+++")))

(defun my/buffer-sort (a b) (string< (buffer-name a) (buffer-name b)))

(defvar my/tab-height 25)
(defvar my/tab-left (powerline-wave-right 'tab-line nil my/tab-height))
(defvar my/tab-right (powerline-wave-left nil 'tab-line my/tab-height))

(defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
  (powerline-render (list my/tab-left
                          (nerd-icons-icon-for-file (buffer-name buffer))
                          (format " %s" (buffer-name buffer))
                          my/tab-right)))
;;(defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
;;  (concat ;;(nerd-icons-powerline "nf-ple-ice_waveform_mirrored")
;;   ;;"  "
;;   "["
;;   (nerd-icons-icon-for-file (buffer-name buffer))
;;   " ]"
;;   (format " %s " (buffer-name buffer))
;;   ;;(nerd-icons-powerline "nf-ple-ice_waveform")
;;   ))

(defun my/tab-line-tabs-buffer-list ()
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (/= (aref (buffer-name b) 0) ?\s)
                               (with-current-buffer b
                                 (not (or (minibufferp)
                                          (string-match-p "\\` " (buffer-name))
                                          (string-match-p "\\*" (buffer-name))
                                          (memq major-mode tab-line-exclude-modes)
                                          (get major-mode 'tab-line-exclude)
                                          (buffer-local-value 'tab-line-exclude (current-buffer)))))))
              (seq-uniq (append (list (current-buffer))
                                (mapcar #'car (window-prev-buffers))
                                (buffer-list)))))

(use-package tab-line
  :straight nil
  :demand
  :custom
  (tab-line-switch-cycling t)
  :custom-face
  (tab-line ((t (:inherit modus-themes-ui-variable-pitch :background "#2c3045" :height 1.0))))
  (tab-line-highlight ((t (:background "#45605e" :foreground "#FFFFFF" :box nil))))
  (tab-line-tab ((t (:background "#0d0e1c"))))
  (tab-line-tab-current ((t (:inherit bold :background "#0d0e1c" :foreground "gray93" :box nil))))
  (tab-line-tab-group ((t (:inherit tab-line :box nil))))
  (tab-line-tab-inactive ((t (:background "#4a4f6a" :foreground "gray93" :box nil))))
  :custom
  (tab-line-new-button-show nil)  ;; do not show add-new button
  (tab-line-close-button-show nil)  ;; do not show close button
  (tab-line-tabs-function #'tab-line-tabs-buffer-groups)
  (tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
  (tab-line-exclude-modes '(completion-list-mode
                            special-mode
                            lisp-interaction-mode
                            messages-buffer-mode))
  :bind
  ("M-<left>" . tab-line-switch-to-prev-tab)
  ("M-<right>" . tab-line-switch-to-next-tab)
  :config
  (global-tab-line-mode 1)
  (setq
   tab-line-tabs-buffer-group-sort-function #'my/buffer-sort
   tab-line-tabs-buffer-group-function #'my/tab-line-buffer-group
   tab-line-tabs-function #'tab-line-tabs-buffer-groups
   tab-line-tabs-buffer-list-function #'my/tab-line-tabs-buffer-list)
  (add-to-list 'tab-line-tab-face-functions 'tab-line-tab-face-group))

(when (and (window-system) (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode 1)
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)
  (when (vertico-mode) ;; TODO: Fix properly
    (keymap-set vertico-map "<remap> <pixel-scroll-interpolate-down>" 'vertico-scroll-up)
    (keymap-set vertico-map "<remap> <pixel-scroll-interpolate-up>" 'vertico-scroll-down)))

(show-paren-mode 1)
(display-time-mode 1)
(global-display-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(menu-bar-mode 1)

;; Set Title Bar
(setq frame-title-format "%b")

(provide 'my/theme)
