;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(use-package which-key
  :config
  (which-key-mode))

;;;; Tabs
;;(global-set-key [M-left] 'tab-line-switch-to-prev-tab)
;;(global-set-key [M-right] 'tab-line-switch-to-prev-tab)

;; Highlight Symbol
(global-set-key [(super f3)] 'highlight-frame-toggle)

;; In-Directory
(global-set-key (kbd "M-X") 'in-directory)

;; Shell Toggle
(global-set-key [M-f1] 'shell-toggle)
(global-set-key [C-f1] 'shell-toggle-cd)

;; Transparency
;; sample keybinding for transparency manipulation
(global-set-key (kbd "C-?") 'transparency-set-value)
;; the two below let for smooth transparency control
(global-set-key (kbd "C->") 'transparency-increase)
(global-set-key (kbd "C-<") 'transparency-decrease)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)

(when (not (config-wrap "use-lsp-bridge"))
  (global-set-key (kbd "M-,") 'xref-pop-marker-stack)
  (global-set-key (kbd "M-t") 'xref-find-definitions)
  (global-set-key (kbd "M-T") 'xref-find-definitions-other-window)
  (if (config-wrap "use-lsp-mode")
      (global-set-key (kbd "M-r") 'lsp-find-references)
    (global-set-key (kbd "M-r") 'xref-find-references))
  )

;; Multiple Cursors
(global-set-key (kbd "C-x c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; Clang Format
;; (global-set-key (kbd "C-s-<tab>") 'clang-format-region)

;; Auto Insert
(global-set-key (kbd "<f12>") 'auto-insert)

(global-set-key (kbd "C-<f12>") 'insert-sc-module)

;; Compile
(global-set-key (kbd "C-x m") 'compile)

;; Ace-Window
(global-set-key (kbd "M-o") 'ace-window)

;; Header/Source toggle
(global-set-key (kbd "C-x t") 'ff-find-other-file)

;; Window Split
(global-set-key (kbd "C-x \\") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)

;; Set Mark
(global-set-key (kbd "C-S-m") 'point-to-register)
;; Jump to Mark
(global-set-key (kbd "C-S-j") 'jump-to-register)

;; Zoom-Window
(global-set-key (kbd "M-z") 'zoom-window-zoom)

(daemon-wrap my/terminal-key-map
             (unless (display-graphic-p)
               (global-set-key (kbd "M-[ P") (kbd "<f1>"))
               (global-set-key (kbd "M-[ Q") (kbd "<f2>"))
               (global-set-key (kbd "M-[ R") (kbd "<f3>"))
               (global-set-key (kbd "M-[ S") (kbd "<f4>"))
               (global-set-key (kbd "M-[ 1 ; 5 r") (kbd "C-<f3>"))
               (global-set-key (kbd "<select>") (kbd "<end>"))))
(provide 'my/keybindings)
