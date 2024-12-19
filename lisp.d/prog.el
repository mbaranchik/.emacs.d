;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

;; Set modes aliasing
(if (config-wrap "treesit")
    (progn
      (defalias 'my/c-mode 'c-ts-mode)
      (defvaralias 'my/c-mode-hook 'c-ts-mode-hook)
      (defvaralias 'my/c-mode-map 'c-ts-mode-map)
      (defalias 'my/c++-mode 'c++-ts-mode)
      (defvaralias 'my/c++-mode-hook 'c++-ts-mode-hook)
      (defvaralias 'my/c++-mode-map 'c++-ts-mode-map)
      (defalias 'my/python-mode 'python-ts-mode)
      (defvaralias 'my/python-mode-hook 'python-ts-mode-hook)
      (defvaralias 'my/python-mode-map 'python-ts-mode-map)
      (defalias 'my/sh-mode 'bash-ts-mode)
      (defvaralias 'my/sh-mode-hook 'bash-ts-mode-hook)
      (defvaralias 'my/sh-mode-map 'bash-ts-mode-map))
  (progn
    (defalias 'my/c-mode 'c-mode)
    (defvaralias 'my/c-mode-hook 'c-mode-hook)
    (defvaralias 'my/c-mode-map 'c-mode-map)
    (defalias 'my/c++-mode 'c++-mode)
    (defvaralias 'my/c++-mode-hook 'c++-mode-hook)
    (defvaralias 'my/c++-mode-map 'c++-mode-map)
    (defalias 'my/python-mode 'python-mode)
    (defvaralias 'my/python-mode-hook 'python-mode-hook)
    (defvaralias 'my/python-mode-map 'python-mode-map)
    (defalias 'my/sh-mode 'sh-mode)
    (defvaralias 'my/sh-mode-hook 'sh-mode-hook)
    (defvaralias 'my/sh-mode-map 'sh-mode-map)))

(defun my/get-mode (mode)
  (car (function-alias-p (intern (concat "my/" mode "-mode")))))

(defun my/get-mode-str (mode)
  (concat "my/" mode "-mode"))

;;(dolist (item (mapcar (lambda (sym) (cons sym sym))
;;                       '(c-basic-offset
;;                         c-ts-mode-indent-offset
;;                         lisp-indent-offset
;;                         groovy-indent-offset
;;                         js-indent-level
;;                         json-ts-mode-indent-offset
;;                         python-indent-offset
;;                         rust-ts-mode-indent-offset
;;                         sh-basic-offset
;;                         verilog-indent-level
;;                         yaml-indent-offset
;;                         cmake-ts-mode-indent-offset)))
;;  (when (boundp (car item))
;;    (set item (config-wrap "basic-indent-offset"))))

(if (config-wrap "indent/indent-tabs")
    (setq-default indent-tabs-mode t)
    (setq-default indent-tabs-mode nil))
(setq tab-width (config-wrap "indent/basic-offset"))

;; Treesit
(when (config-wrap "treesit")
  (require 'my/treesit-sources (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "treesit-sources"))
  (use-package treesit
    :straight nil
    :config
    ;; Fallback for local compilation of tree-sitter language support
    (setq treesit-language-source-alist my/treesit-sources)
    (setq treesit-font-lock-level 4)
    (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))
    (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
    (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
    )
  (use-package json-ts-mode
    :straight nil)
  (use-package dockerfile-ts-mode
    :straight nil)
  (use-package c-ts-mode
    :straight nil)
  (use-package c++-ts-mode
    :straight nil)
  (use-package python
    :straight nil
    :config
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))
  (use-package cmake-ts-mode
    :straight nil)
  (use-package rust-ts-mode
    :straight nil)
  (use-package yaml-ts-mode
    :straight nil))

(use-package sh-script
  :straight nil
  :config
  (when (config-wrap "treesit")
    (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))
  (add-to-list 'auto-mode-alist '("\\.cshrc\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.csh\\'" . sh-mode)))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :config
  (use-package edit-indirect)
  (setopt markdown-fontify-code-blocks-natively t))

(when (config-wrap "indent/basic-offset")
    (dolist (item '(c-basic-offset
                       c-ts-mode-indent-offset
                       lisp-indent-offset
                       groovy-indent-offset
                       js-indent-level
                       json-ts-mode-indent-offset
                       python-indent-offset
                       rust-ts-mode-indent-offset
                       sh-basic-offset
                       verilog-indent-level
                       yaml-indent-offset
                       cmake-ts-mode-indent-offset))
        (when (boundp `,item)
            (set item (config-wrap "indent/basic-offset")))))

(use-package emacs
  :straight nil
  :config
  (add-hook 'emacs-startup-hook (lambda ()
                                  (define-key my/sh-mode-map (kbd "C-c C-c") nil)
                                  (define-key my/c-mode-map (kbd "C-c C-c") nil)
                                  (define-key my/c++-mode-map (kbd "C-c C-c") nil)
                                  (define-key my/python-mode-map (kbd "C-c C-c") nil)
                                  (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line))))

(provide 'my/prog)
