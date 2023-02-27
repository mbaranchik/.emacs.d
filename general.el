;;; -*- lexical-binding: t -*-

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time

(global-display-line-numbers-mode t)
(put 'downcase-region 'disabled nil)
(display-time-mode t)
(if use-visual-line-mode
    (global-visual-line-mode 1)
  (global-visual-line-mode 0)
  )
(when (not use-visual-line-mode)
  (defun disable-visual-line-mode()
    (setq-local line-move-visual nil))
  (add-hook 'prog-mode-hook 'disable-visual-line-mode)
  )

(when use-autocomplete
  (use-package auto-complete-config
    :config
    (ac-config-default))
  )

(use-package desktop+
  :commands (desktop+-load desktop+-create)
  )

(use-package jenkinsfile-mode)

(use-package smex)

(use-package ace-window
  :config
  (setq aw-scope 'frame))

(use-package tree-sitter
  :hook
  ((c-mode python-mode c++-mode sh-mode json-mode js-mode html-mode verilog-mode yaml-mode) . tree-sitter-mode)
  ((c-mode python-mode c++-mode sh-mode json-mode js-mode html-mode verilog-mode yaml-mode) . tree-sitter-hl-mode)
  )
(use-package tree-sitter-langs)

(use-package vterm-toggle
  :bind (([f2] . vterm-toggle)
         ([C-f2] . vterm-toggle-cd)))

(add-to-list 'auto-mode-alist '("\\.cshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.csh\\'" . sh-mode))

(when use-company
  (use-package company-c-headers)
  (use-package company
    :hook (prog-mode . company-mode)
    :config
    (setq company-backends (delete 'company-semantic company-backends))
    (add-to-list 'company-backends 'company-c-headers)
    )

  (use-package company-box
    :after company
    :hook (company-mode . company-box-mode))

  (when use-lsp
    (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))

    ;;(add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '((company-capf ;; I think this must come first?
    ;;          :with
    ;;          company-yasnippet)))))
    ;;(setq lsp-completion-provider :none)
    )
  )

(when use-corfu
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

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete)))

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

;; Undo-Tree mode
(use-package undo-tree
  :hook (prog-mode . undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize))

;; ;; SLN mode
;; (load "~/.emacs.d/sln-mode")
;; (setq auto-mode-alist (cons  '("\\.sln\\'" . sln-mode) auto-mode-alist))(
;;  custom-set-variables
;;  '(sln-auto-newline nil)      ;; t for true, nil for false/disabled (do a new line after all semicolons, I rather disable this)
;;  '(sln-semi-is-electric nil)  ;; t for true, nil for false/disabled (do new-line or goto begin of line, add "};" end comment, I rather disable this)
;;  '(sln-basic-offset 4)        ;; set spaces-per-TAB size. 4 is my prefered optimum, to be aligend with 8 space tabs (some people like 2, 3, ... )
;;  )

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
(when use-idlehightlist
  (use-package idle-highlight-mode
    :hook (prog-mode . idle-highlight-mode)))

;; Highlight TODO
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode-on)
  :config
  (yas-reload-all))

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
(xterm-mouse-mode 1)

(setq echo-keystrokes 0.01)

