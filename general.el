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
  (use-package auto-complete-config
    :config
    (ac-config-default))
  )

(use-package smex)

(use-package ace-window)

(use-package vterm-toggle
  :bind (([f2] . vterm-toggle)
         ([C-f2] . vterm-toggle-cd)))

(when use-company
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

  ;;(use-package company-posframe
  ;;  :after company
  ;;  :hook (company-mode . company-posframe-mode))

  ;;(advice-add 'company-clang--handle-error :after
  ;;            (lambda (&rest _args) (message nil))
  ;;            '((name . "Silence errors")))

  )

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode))

;;(use-package ivy-posframe
;;  :hook (ivy-mode . ivy-posframe-mode))

;;(when (and use-company use-python-jedi)
;;  (defun my/python-mode-hook ()
;;  (add-to-list 'company-backends 'company-jedi))
;;  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; (use-package desktop+)

;; (use-package figlet)

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
                               (set (make-local-variable 'compile-command)
                                    (let ((verif_path (car (split-string (cadr (split-string (cadr (split-string (file-truename (expand-file-name (file-name-directory buffer-file-name))) "/Volumes/ANPA/")) "sync/")) "/"))))
                                      (format "ssh dub-gw015 \"echo 'rm /project/users/michaelba/toprock/%s/err.log \\n touch /project/users/michaelba/toprock/%s/.check \\n while (! -f /project/users/michaelba/toprock/%s/err.log) \\n end \\n sleep 2 && tail -n +1 /project/users/michaelba/toprock/%s/err.log' | csh -f\"" verif_path verif_path verif_path verif_path)))
                               (set (make-local-variable 'verilog-linter) (symbol-value 'compile-command))
                               ))
  :config
  (setq compilation-read-command t)
  (setq verilog-tool 'verilog-linter))

;; Idle-Highlight-Mode
(when use-idlehightlist
  (use-package idle-highlight-mode
    :hook (prog-mode . idle-highlight-mode)))

;; Highlight TODO
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

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

