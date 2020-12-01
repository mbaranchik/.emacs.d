;;; -*- lexical-binding: t -*-
; list the packages you want
(setq package-list '(compile
             flycheck
             flycheck-cstyle
             flycheck-clangcheck
             compile
             package
             ;;auto-complete-config
             company
             company-c-headers
             company-box
             company-shell
             company-jedi
             vterm
             vterm-toggle
             multi-vterm
             desktop+
             smex
             ido
             ido-vertical-mode
             ivy
             counsel
             gcmh
             counsel-gtags
             counsel-projectile
             counsel-tramp
             swiper
             lsp-mode
             lsp-ui
             ivy-xref
             helm-xref
             cquery
             ccls
             company-lsp
             magit
             ace-window
             yasnippet
             yasnippet-snippets
             figlet
             transpose-frame
             ;;phonetool
             verilog-mode
             idle-highlight-mode
             git-gutter-fringe
             vimish-fold
             git-blamed
             mo-git-blame
             monokai-theme
             horizon-theme
             hl-todo
             ;;sr-speedbar
             auto-compile
             scratch-pop
             ;;git-sync
             helm
             ;;helm-config
             projectile
             helm-projectile
             ;;tree-mode
             ;;dirtree
             tabbar
             tabbar-ruler
             multiple-cursors
             highlight-symbol
             ldap
             ;;auto-rsync
             ggtags
             helm-gtags
             powerline
             moe-theme
             airline-themes
             indent-guide
             undo-tree
             rainbow-delimiters
             browse-kill-ring
             clang-format
             treemacs
             treemacs-magit
             treemacs-projectile
             treemacs-icons-dired
             all-the-icons-dired
             all-the-icons
             all-the-icons-ivy
             spaceline-all-the-icons
             spaceline
             spacemacs-theme
             zoom-window
             back-button
             expand-region
             exec-path-from-shell
             benchmark-init
             which-key
             lsp-ivy
             helm-lsp
             lsp-treemacs
             use-package))

; list the repositories containing them
;(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
;                         ("gnu" . "http://elpa.gnu.org/packages/")
;                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; (add-to-list 'package-archives
;;         '(("melpa" . "http://melpa.org/packages/")
;;           ("elpa" . "http://tromey.com/elpa/")
;;           ("gnu" . "http://elpa.gnu.org/packages/")
;;           ("marmalade" . "http://marmalade-repo.org/packages/"))
;; )

;; Avoid loading older byte-compiled
(setq load-prefer-newer t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(defun install-my-packages ()
  "Install new packages"
  (interactive)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )

(defun update-my-packages ()
  "Refresh and update all new packages"
  (interactive)
  (package-refresh-contents)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (sleep-for 5)
  (package-menu-execute)
  )

;; This is only needed once, near the top of the file
(require 'use-package)

;; Auto compile lisp
(use-package auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

