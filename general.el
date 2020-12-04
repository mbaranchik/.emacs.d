;;; -*- lexical-binding: t -*-
;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time

(global-linum-mode 1)
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
  (use-package auto-complete-config)
  (ac-config-default))

(when use-company
  (use-package company)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-c-headers)
  (use-package company-box
      :hook (company-mode . company-box-mode))
  (advice-add 'company-clang--handle-error :after
              (lambda (&rest _args) (message nil))
              '((name . "Silence errors"))))

(when (and use-company use-lsp)
  (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))
  ;;(add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '((company-capf ;; I think this must come first?
  ;;          :with
  ;;          company-yasnippet)))))
  ;;(setq lsp-completion-provider :none)
  )

(when (and use-company use-python-jedi)
  (defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package desktop+)

(use-package figlet)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; Undo-Tree mode
(global-undo-tree-mode)

;; SLN mode
(load "~/.emacs.d/sln-mode")
(setq auto-mode-alist (cons  '("\\.sln\\'" . sln-mode) auto-mode-alist))(
 custom-set-variables
 '(sln-auto-newline nil)      ;; t for true, nil for false/disabled (do a new line after all semicolons, I rather disable this)
 '(sln-semi-is-electric nil)  ;; t for true, nil for false/disabled (do new-line or goto begin of line, add "};" end comment, I rather disable this)
 '(sln-basic-offset 4)        ;; set spaces-per-TAB size. 4 is my prefered optimum, to be aligend with 8 space tabs (some people like 2, 3, ... )
 )

;; Transpose
;;(use-package transpose-frame)
(use-package transpose-frame)

;; Verilog-Mode
;;(unload-feature 'verilog-mode)
(load "~/.emacs.d/verilog-mode")
(use-package verilog-mode)

;; Idle-Highlight-Mode
(use-package idle-highlight-mode)
(when use-idlehightlist
  (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t))))

;; TODO highlight mode
(use-package hl-todo)
(global-hl-todo-mode)

;; Yasnippet
(use-package yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Browse-Kill-Ring
(use-package browse-kill-ring)

;; Zoom-Window
(use-package zoom-window)
(custom-set-variables
 '(zoom-window-mode-line-color "DarkGreen"))

;; Expand-Region
(use-package expand-region)

