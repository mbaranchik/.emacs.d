;;; -*- lexical-binding: t -*-

(bench "Load extra themes"
       (use-package zenburn-theme)
       (use-package atom-dark-theme)
       ;;(use-package spacemacs-common
       ;;  ;;:defer t
       ;;  :straight spacemacs-theme)
       (use-package spacemacs-theme)
       (use-package vscode-dark-plus-theme)
       (use-package doom-themes)
       )

(bench "Load main theme"
       (load-theme use-theme t t)
       )
(daemon-wrap my/load-theme
             (bench "Enable main theme"
                    (enable-theme use-theme)
                    (when (fboundp 'powerline-reset)
                      (powerline-reset)))
             )

;;(daemon-wrap my/load-powerline
(bench "Load powerline"
       (use-package powerline)
       )
;;)

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

;;(daemon-wrap my/load-all-the-icons
(bench "Load all-the-icons"
       (use-package all-the-icons)
       (use-package all-the-icons-completion)
       (use-package treemacs-all-the-icons
         :after treemacs))
;; )

(when use-doom-modeline
  (bench "Doom-Modeline"
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

(when use-spaceline
(bench "Spaceline-Load"
       (use-package spaceline
         :demand
         :config
         (spaceline-emacs-theme)
         (defvar my/spaceline-flycheck-flymake ())
         (when use-flymake
           ;; HACK - Allow flymake + click
           (defmacro my/spaceline-flymake-lighter (severity)
             `(let ((count
                     (length
                      (seq-filter
                       (lambda (diag)
                         (= ,severity
                            (flymake--severity (flymake-diagnostic-type diag))))
                       (flymake-diagnostics)))))
                (if (not (zerop count)) (format spaceline-flycheck-bullet count))))
           (dolist (state '(error warning note))
             (let ((segment-name (intern (format "flymake-%S" state)))
                   (face (intern (if (eq state "note")
                                     (format "spaceline-flycheck-info")
                                   (format "spaceline-flycheck-%S" state) )))
                   (severity (flymake--severity (intern (format ":%S" state)))))
               (eval
                `(spaceline-define-segment ,segment-name
                   (when (bound-and-true-p flymake-mode)
                     (let ((lighter (my/spaceline-flymake-lighter ,severity)))
                       (when lighter
                         (propertize
                          (powerline-raw (s-trim lighter) ',face)
                          'local-map (make-mode-line-mouse-map
                                      'mouse-1 (lambda()
                                                 (interactive)
                                                 (call-interactively 'flymake-show-diagnostics-buffer))))
                         )))))))
           )
         (when use-flycheck
           ;; HACK - Allow flycheck click
           (dolist (state '(error warning info))
             (let ((segment-name (intern (format "flycheckclick-%S" state)))
                   (face (intern (format "spaceline-flycheck-%S" state)) ))
               (eval
                `(spaceline-define-segment ,segment-name
                   ,(format "Information about flycheck %Ss. Requires `flycheck-mode' to be enabled" state)
                   (when (and (bound-and-true-p flycheck-mode)
                              (or flycheck-current-errors
                                  (eq 'running flycheck-last-status-change)))
                     (let ((lighter (spaceline--flycheck-lighter ,state)))
                       (when lighter
                         (propertize
                          (powerline-raw (s-trim lighter) ',face)
                          'local-map (make-mode-line-mouse-map
                                      'mouse-1 (lambda()
                                                 (interactive)
                                                 (call-interactively 'flycheck-list-errors))))
                         )))))))
           )
         (defun my/spaceline-theme (flycheck-flymake &rest additional-segments)
           "Convenience function for the spacemacs and emacs themes."
           (spaceline-compile
                                        ; left side
             `(((persp-name
                 workspace-number
                 window-number)
                :fallback evil-state
                :face highlight-face
                :priority 100)
               (anzu :priority 95)
               auto-compile
               ((buffer-modified buffer-size buffer-id remote-host)
                :priority 98)
               (major-mode :priority 79)
               (process :when active)
               (,flycheck-flymake
                :when active
                :priority 89)
               (minor-modes :when active
                            :priority 9)
               (mu4e-alert-segment :when active)
               (erc-track :when active)
               (version-control :when active
                                :priority 78)
               (org-pomodoro :when active)
               (org-clock :when active)
               nyan-cat)
                                        ; right side
             `((which-function
                :priority 99
                :face highlight-face)
               (python-pyvenv :fallback python-pyenv)
               (purpose :priority 94)
               (battery :when active)
               (selection-info :priority 95)
               input-method
               ((buffer-encoding-abbrev
                 point-position
                 line-column)
                :separator " | "
                :priority 96)
               (global :when active)
               ,@additional-segments
               (buffer-position :priority 99)
               (hud :priority 99)))
           )
         (if use-flymake
             ;; flymake
             (my/spaceline-theme '(flymake-error flymake-warning flymake-info flymake-note))
           ;; flycheck
           (my/spaceline-theme '(flycheckclick-error flycheckclick-warning flycheckclick-info))
           )
         )
       )
)
;;(daemon-wrap my/load-spaceline
;;             (bench "Spaceline-Enable"
;;             )
;;             )

;;(bench "Spaceline-all-the-icons"
;;       (setq inhibit-compacting-font-caches t)
;;       (use-package spaceline-all-the-icons
;;         :after (spaceline all-the-icons)
;;         :config
;;         (spaceline-all-the-icons-theme)))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

(when (fboundp 'scroll-bar-mode)
(bench "Scroll-bar-mode"
       (scroll-bar-mode -1))
  )


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
