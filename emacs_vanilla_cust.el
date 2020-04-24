;;; -*- lexical-binding: t -*-
;; (when (boundp 'aquamacs-version)
;;   (custom-set-variables
;;    '(aquamacs-additional-fontsets nil t)
;;    '(aquamacs-customization-version-id 307 t)
;;    '(aquamacs-tool-bar-user-customization nil t)
;;    '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
;;    )
;;   )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(c-basic-offset 4)
;; '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "5436e5df71047d1fdd1079afa8341a442b1e26dd68b35b7d3c5ef8bd222057d1" "3c98d13ae2fc7aa59f05c494e8a15664ff5fe5db5256663a907272869c4130dd" "71182be392aa922f3c05e70087a40805ef2d969b4f8f965dfc0fc3c2f5df6168" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "c9ca319da1db5fc5064f9c44c6b134c10d721681ed44ab505394ed8ccf5b928c" "8fd96d5434100d13aef7f3a72088981ca1374fee3f764f8827c5ce697790f23c" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "6af55f6f26c0c6f113427d8ce72dea34aa1972b70e650486e6c725abd18bbe91" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "c9ca319da1db5fc5064f9c44c6b134c10d721681ed44ab505394ed8ccf5b928c" "8fd96d5434100d13aef7f3a72088981ca1374fee3f764f8827c5ce697790f23c" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "6af55f6f26c0c6f113427d8ce72dea34aa1972b70e650486e6c725abd18bbe91" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" default)))
 '(default-frame-alist
    (quote
     ((cursor-type . box)
      (vertical-scroll-bars . right)
      (internal-border-width . 0)
      (modeline . t)
      (fringe)
      (cursor-color . "Red")
      (background-mode . light)
      (tool-bar-lines . 1)
      (menu-bar-lines . 1)
      ;;(font . "-*-Menlo-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
      (fontsize . 0)
      (font-backend mac-ct ns))))
 '(display-time-mode t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(ggtags-highlight-tag 0.1)
 '(ggtags-sort-by-nearness t)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(ns-antialias-text t)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(package-selected-packages
   (quote
    (magit magit-filenotify visible-mark nameframe-projectile helm-projectile airline-themes ample-theme color-theme-sanityinc-tomorrow gruvbox-theme material-theme moe-theme monokai-theme solarized-theme helm helm-core helm-google helm-gtags zenburn-theme unison sr-speedbar rainbow-identifiers rainbow-delimiters micgoline indent-guide highlight-symbol hc-zenburn-theme git-gutter-fringe ggtags figlet dirtree darkroom auto-complete anti-zenburn-theme)))
 '(semantic-mode t)
 '(send-mail-function nil)
 '(show-paren-mode t)
 '(speedbar-show-unknown-files t)
;; '(tabbar-mode t nil (tabbar))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-lineup (quote all))
 '(verilog-auto-newline nil)
 '(verilog-case-indent 4)
 '(verilog-cexp-indent 4)
 '(verilog-compiler "make all")
 '(verilog-highlight-modules t)
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level 4)
 '(verilog-indent-level-behavioral 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-directive 0)
 '(verilog-indent-level-module 0)
 '(verilog-simulator "make run")
 '(visual-line-mode nil t)
 '(whitespace-style (quote (face trailing spaces tabs))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "DejaVu Sans Mono"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "light blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-unmatched-face ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000" :bold t))))
 '(whitespace-space nil)
 '(whitespace-tab nil)
 )

;; '(whitespace-space ((t (:background "Green"))))
;; '(whitespace-tab ((t (:background "Magenta")))))

