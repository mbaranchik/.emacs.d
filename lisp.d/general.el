;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time

(global-display-line-numbers-mode t)
(put 'downcase-region 'disabled nil)
(display-time-mode t)
(if (config-wrap "use-visual-line-mode")
    (global-visual-line-mode 1)
  (global-visual-line-mode 0)
  )
(when (not (config-wrap "use-visual-line-mode"))
  (defun disable-visual-line-mode()
    (setq-local line-move-visual nil))
  (add-hook 'prog-mode-hook 'disable-visual-line-mode)
  )

(use-package desktop+
  :commands (desktop+-load desktop+-create)
  )

(use-package jenkinsfile-mode)

(use-package smex)

(use-package ace-window
  :config
  (setq aw-scope 'frame))

(when (or (config-wrap "use-company") (config-wrap "use-corfu"))
;; A few more useful configurations...
  (use-package emacs
    :straight nil
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete))
)

(when (config-wrap "use-company")
  (use-package company
    :hook (prog-mode . company-mode)
    :config
    (setq company-backends '((company-capf :with company-yasnippet)))
    (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
    )

  (use-package company-box
    :after company
    :hook (company-mode . company-box-mode))

  ;;(when (config-wrap "use-lsp")
  ;;  (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))

  ;;  ;;(add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '((company-capf ;; I think this must come first?
  ;;  ;;          :with
  ;;  ;;          company-yasnippet)))))
  ;;  ;;(setq lsp-completion-provider :none)
  ;;  )
  )

(when (config-wrap "use-corfu")
  (use-package corfu-terminal
  :straight (:type git
                   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (daemon-wrap my/load-corfu-terminal
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))))

  (use-package corfu
    ;; Optional customizations
    :custom
    ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `corfu-excluded-modes'.
    :init
    (global-corfu-mode))

  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (face-spec-set 'rainbow-delimiters-depth-2-face '((t (:foreground "green"))) 'face-defface-spec)
  (face-spec-set 'rainbow-delimiters-depth-3-face '((t (:foreground "dark orange"))) 'face-defface-spec)
  (face-spec-set 'rainbow-delimiters-depth-4-face '((t (:foreground "light blue"))) 'face-defface-spec)
  (face-spec-set 'rainbow-delimiters-depth-5-face '((t (:foreground "magenta"))) 'face-defface-spec)
  (face-spec-set 'rainbow-delimiters-depth-6-face '((t (:foreground "cyan"))) 'face-defface-spec)
  (face-spec-set 'rainbow-delimiters-unmatched-face '((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000" :bold t))) 'face-defface-spec))

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

(use-package cua-base
  :config
  (setq cua-remap-control-z nil)
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ;; No region when it is not highlighted
  (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
  )

;; Undo
(use-package undo-fu
  :config
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))
(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))
(use-package vundo
  :commands (vundo)
  :bind (
         ("C-x u" . vundo)
         :map vundo-mode-map
         ("<right>" . vundo-forward)
         ("<left>" . vundo-backward)
         ("<down>" . vundo-next)
         ("<up>" . vundo-previous)
         ("<home>" . vundo-stem-root)
         ("<end>" . vundo-stem-end)
         ("q" . vundo-quit)
         ("C-g" . vundo-quit)
         ("RET" . vundo-confirm)
   )

  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Better contrasting highlight.
  (custom-set-faces
    '(vundo-node ((t (:foreground "#808080"))))
    '(vundo-stem ((t (:foreground "#808080"))))
    '(vundo-highlight ((t (:foreground "#FFFF00")))))
  )

;; Transpose
(use-package transpose-frame
  :commands transpose-frame)

;; Verilog-Mode
(use-package verilog-mode
  :mode ("\\.v\\'" "\\.sv\\'" "\\.svh\\'")
  :hook (verilog-mode-hook . (lambda ()
                               (set (make-local-variable 'verilog-linter) (symbol-value 'compile-command))
                               ))
  :config
  (setq verilog-tool 'verilog-linter)
  (setq verilog-auto-delete-trailing-whitespace t)
  (setq verilog-auto-newline nil)
  (setq verilog-case-indent 4)
  (setq verilog-cexp-indent 4)
  (setq verilog-compiler "make all")
  (setq verilog-highlight-modules t)
  (setq verilog-indent-begin-after-if nil)
  (setq verilog-indent-level 4)
  (setq verilog-indent-level-behavioral 4)
  (setq verilog-indent-level-declaration 4)
  (setq verilog-indent-level-directive 0)
  (setq verilog-indent-level-module 0)
  (setq verilog-simulator "make run"))

;; Idle-Highlight-Mode
(use-package idle-highlight-mode
  :if (config-wrap "use-idle-highlight")
  :hook (prog-mode . idle-highlight-mode))

;; Highlight TODO
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . (lambda () (yas-reload-all) (yas-minor-mode-on)))
  )

;; TODO: Not supported with acm/lsp-bridge
;; (setq tab-always-indent 'complete)

;;;; Common Snippets
;;(use-package yasnippet-snippets)

;; Browse-Kill-Ring
(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;; Zoom-Window
(use-package zoom-window
  :bind ("M-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "DarkGreen"))

;; Expand-Region
(use-package expand-region
  :bind ("C-=" . expand-region))

;; Enable mouse in Xterm
(daemon-wrap my/set-xterm-mouse-mode
             (unless (display-graphic-p)
               (xterm-mouse-mode 1)
               ))

;; Enable Clipetty
(use-package clipetty
  :config
  (daemon-wrap my/set-clipetty
             (unless (display-graphic-p)
               (global-clipetty-mode)
               )))

(setq echo-keystrokes 0.01)

;; (use-package emacs-everywhere)

(provide 'my/general)
