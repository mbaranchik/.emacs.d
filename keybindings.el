;;; -*- lexical-binding: t -*-

(use-package which-key)

;;;; Tabs
;;(global-set-key [M-left] 'tab-line-switch-to-prev-tab)
;;(global-set-key [M-right] 'tab-line-switch-to-prev-tab)

;; Highlight Symbol
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
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

;; Helm
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(when use-helm
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  (global-set-key (kbd "C-x b")   'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings))

;; Ivy
(when use-ivy
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-x k") 'ibuffer)
  (global-set-key (kbd "C-c r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c C-g") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "M-y") 'browse-kill-ring)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
(eval-after-load "verilog-mode"
  '(progn
     (define-key verilog-mode-map (kbd "C-c C-c") nil)
     (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)))
(eval-after-load "sh-mode"
  '(progn
     (define-key sh-mode-map (kbd "C-c C-c") nil)
     (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)))
(eval-after-load "c-mode"
  '(progn
     (define-key c-mode-map (kbd "C-c C-c") nil)
     (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)))
(eval-after-load "c++-mode"
  '(progn
     (define-key c++-mode-map (kbd "C-c C-c") nil)
     (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)))

;;;; Helm-Gtags
;;(when (and use-helm use-tags)
;;  (eval-after-load "helm-gtags"
;;    '(progn
;;       (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;       (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;       (define-key helm-gtags-mode-map (kbd "M-f") 'helm-gtags-find-symbol)
;;       (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;       (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;       (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;       (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))))
;; 
;;;; Ivy-Gtags
;;(when (and use-ivy use-tags)
;;  (with-eval-after-load 'counsel-gtags
;;  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
;;  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
;;  (define-key counsel-gtags-mode-map (kbd "M-f") 'counsel-gtags-find-symbol)
;;  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)))

(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(global-set-key (kbd "M-t") 'xref-find-definitions)
(global-set-key (kbd "M-T") 'xref-find-definitions-other-window)
(if use-lsp
    (global-set-key (kbd "M-r") 'lsp-find-references)
  (global-set-key (kbd "M-r") 'xref-find-references))

;; Projectile
(when (and use-helm use-projectile)
  (eval-after-load "helm-projectile"
    '(progn
       (global-set-key (kbd "C-x p f")     'helm-projectile))))
(when (and use-ivy use-projectile)
  (eval-after-load "counsel-projectile"
    '(progn
       (global-set-key (kbd "C-x p f")     'counsel-projectile)
       (global-set-key (kbd "C-x p p")     'counsel-projectile-switch-project))))

;; Treemacs
(global-set-key (kbd "M-s M-s") 'treemacs)

;; Multiple Cursors
(global-set-key (kbd "C-x c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Vimish-Fold
(eval-after-load "vimish-fold"
  '(progn
     (global-set-key (kbd "C-`") #'vimish-fold)
     (global-set-key (kbd "C-~") #'vimish-fold-delete)))


(fset 'byteify
   [right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right ?  right right])


(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; Clang Format
(global-set-key (kbd "C-s-<tab>") 'clang-format-region)

;; Company Mode
(global-set-key (kbd "<backtab>") 'company-complete)
;; Company - Yasnippet
(global-set-key (kbd "C-<tab>") 'company-yasnippet)

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

;; Expand-Region
(global-set-key (kbd "C-=") 'er/expand-region)
