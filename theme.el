;;; -*- lexical-binding: t -*-

(bench "Load extra themes"
       (use-package zenburn-theme)
       (use-package atom-dark-theme)
       (use-package spacemacs-common
         :defer t
         :straight spacemacs-theme)
       )

(bench "Load main theme"
       (load-theme use-theme t))

(bench "Load powerline"
       (use-package powerline))

;; ;;(load-theme 'zenburn t)
;;(use-package moe-theme)

;; ;; Choose a color for mode-line.(Default: blue)
;; (moe-theme-set-color 'blue)

;;(powerline-center-theme)
;;(moe-dark)

;; (use-package airline-themes)
;; (load-theme 'airline-cool)

;; (load-theme 'tango-dark)

;; (eval-after-load "moe-theme"
;;   '(progn
;;      (powerline-center-theme)
;;      (powerline-moe-theme)))

;; (eval-after-load "moe-theme"
;;   '(progn
;;      (powerline-vim-theme)))

;;(powerline-default-theme)

(bench "Load all-the-icons"
       (use-package all-the-icons)
       (use-package all-the-icons-ivy
         :after all-the-icons
         :config
         (all-the-icons-ivy-setup))
       (use-package treemacs-all-the-icons
         :after treemacs))

(bench "Spaceline"
       (use-package spaceline
         :config
         (spaceline-emacs-theme)))

;;(bench "Spaceline-all-the-icons"
;;       (setq inhibit-compacting-font-caches t)
;;       (use-package spaceline-all-the-icons
;;         :after (spaceline all-the-icons)
;;         :config
;;         (spaceline-all-the-icons-theme)))

(bench "Scroll-bar-mode"
       (scroll-bar-mode -1))


;; Set Title Bar
(setq frame-title-format "%b")

;; Customs - Faces TODO
;; (face-spec-set 'default '((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))) 'face-defface-spec)

;;(face-spec-set 'tabbar-button '((t (:inherit tabbar-default :box (:line-width 3 :color "white" :style released-button)))) 'face-defface-spec)
;;(face-spec-set 'tabbar-default '((t (:inherit variable-pitch :background "gray80" :foreground "black" :height 140))) 'face-defface-spec)

;; (face-spec-set 'region '((t (:background "light cyan" :foreground "black"))) 'face-defface-spec)

;; Centaur-Tabs
;;(load-file "~/.emacs.d/centaur-tabs-master/centaur-tabs.el")
;;(use-package centaur-tabs)
;;(setq centaur-tabs-style "wave")
;;(setq centaur-tabs-set-icons t)
;;(setq centaur-tabs-height 32)
;;(centaur-tabs-mode t)

;;;; Tabbar tweaks
;;(set-face-attribute
;; 'tabbar-default nil
;; :background "gray20"
;; :foreground "gray20"
;; :box '(:line-width 1 :color "gray20" :style nil))
;;(set-face-attribute
;; 'tabbar-unselected nil
;; :background "gray30"
;; :foreground "white"
;; :box '(:line-width 5 :color "gray30" :style nil))
;;(set-face-attribute
;; 'tabbar-selected nil
;; :background "gray75"
;; :foreground "black"
;; :box '(:line-width 5 :color "gray75" :style nil))
;;(set-face-attribute
;; 'tabbar-highlight nil
;; :background "white"
;; :foreground "black"
;; :underline nil
;; :box '(:line-width 5 :color "white" :style nil))
;;(set-face-attribute
;; 'tabbar-button nil
;; :box '(:line-width 1 :color "gray20" :style nil))
;;(set-face-attribute
;; 'tabbar-separator nil
;; :background "gray20"
;; :height 0.6)
