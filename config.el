;;; -*- lexical-binding: t -*-
(defun set-config-var (varname default env)
  (cond ((string= "y" (getenv env)) (set varname t))
	    ((string= "n" (getenv env)) (set varname nil))
	    ((getenv env) (set varname (getenv env)))
        (t (set varname default))
        )
  )

(defun set-config-quote-var (varname default env)
  (cond ((getenv env) (set varname (intern (getenv env))))
	    (t (set varname default))
        )
  )

;;(setq ASymbol 10)  => 10
;;(intern "ASymbol")  => 'ASymbol (e.g. the unevaluated symbol with name "ASymbol")
;;(symbol-value (intern "ASymbol")) => 10

;; Example
;; (set-config-var 'my-new-shiny-var 3277 "EMACS_NEW_SHINY_VAR")

;; Enables
(setq START_ME (getenv "EMACS_START_SERVER"))
(message "Start Server? %s" START_ME)
(set-config-var 'start-server t "EMACS_START_SERVER")
(set-config-var 'use-helm nil "EMACS_USE_HELM")
(set-config-var 'use-ido nil "EMACS_USE_IDO")
(set-config-var 'use-ivy t "EMACS_USE_IVY")
(set-config-var 'use-projectile t "EMACS_USE_PROJECTILE")
(set-config-var 'use-git nil "EMACS_USE_GIT")
(set-config-var 'use-gitsync nil "EMACS_USE_GITSYNC")
(set-config-var 'use-gitblame nil "EMACS_USE_GITBLAME")
(set-config-var 'use-clang t "EMACS_USE_CLANG")
(set-config-var 'use-cua t "EMACS_USE_CUA")
(set-config-var 'use-tags t "EMACS_USE_TAGS")
(set-config-var 'use-flycheck t "EMACS_USE_FLYCHECK")
(set-config-var 'use-whitespace t "EMACS_USE_WHITESPACE")
(set-config-var 'use-navigation t "EMACS_USE_NAVIGATION")
(set-config-var 'use-idlehightlist nil "EMACS_USE_IDLEHIGHTLIST")
(set-config-var 'use-company t "EMACS_USE_COMPANY")
(set-config-var 'use-python-jedi t "EMACS_USE_PYTHON_JEDI")
(set-config-var 'use-autocomplete nil "EMACS_USE_AUTOCOMPLETE")
(set-config-var 'use-lsp t "EMACS_USE_LSP")
(set-config-var 'use-visual-line-mode t "EMACS_USE_VISUAL_LINE_MODE")
(set-config-var 'use-indent-guide t "EMACS_USE_INDENT_GUIDE")

;; Specifics
(set-config-var 'flycheck-gcc-language-standard "c++11" "EMACS_FLYCHECK_GCC_LANGUAGE_STANDARD")
(set-config-var 'flycheck-clang-language-standard "c++11" "EMACS_FLYCHECK_CLANG_LANGUAGE_STANDARD")
(set-config-var 'gitsync-basefrom "/Volumes/ANPA" "EMACS_GITSYNC_BASEFROM")
(set-config-var 'gitsync-basefrom2 "sync/" "EMACS_GITSYNC_BASEFROM2")
(set-config-var 'gitsync-script "/Volumes/ANPA/sync/rsync_script.py" "EMACS_GITSYNC_SCRIPT")
(set-config-quote-var 'use-theme 'horizon "EMACS_USE_THEME")
(set-config-quote-var 'aw-scope 'frame "EMACS_AW_SCOPE")
(set-config-var 'cquery-executable "cquery" "EMACS_CQUERY_PATH")
(set-config-var 'my-lsp-c++-backend "cquery" "EMACS_LSP_BACKEND")
(set-config-var 'lsp-clients-clangd-executable "clangd" "EMACS_CLANGD_PATH")
(set-config-var 'lsp-ui-flycheck-enable t "EMACS_LSP_UI_FLYCHECK_ENABLE")
(set-config-var 'lsp-prefer-flymake nil "EMACS_LSP_PREFER_FLYMAKE")
(set-config-var 'lsp-ui-doc-delay 1 "EMACS_LSP_UI_DOC_DELAY")
(set-config-var 'lsp-ui-sideline-enable nil "EMACS_LSP_UI_SIDELINE_ENABLE")
(set-config-var 'auto-insert-copyright "REPLACE_WITH_COPYRIGHT" "EMACS_COPYRIGHT_COMPANY")
(set-config-var 'auto-insert-name "REPLACE_WITH_NAME" "EMACS_COPYRIGHT_NAME")

(set-config-var 'read-process-output-max (* 1024 1024) "EMACS_READ_PROCESS_MAX")
(set-config-var 'lsp-headerline-breadcrumb-enable t "EMACS_LSP_BREADCRUMB")
(set-config-var 'lsp-idle-delay 0.1 "EMACS_LSP_IDLE_DELAY")
(set-config-var 'company-idle-delay 0.1 "EMACS_COMPANY_IDLE_DELAY")

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1 ;; clangd is fast
;;       ;; be more ide-ish
;;       lsp-headerline-breadcrumb-enable t)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(setq-default server-socket-dir "~/.emacs.d/server-sock")

;; Customs - Vars
(setq-default c-basic-offset 4)
(setq-default default-frame-alist
  (quote
   ((cursor-type . (bar . 3))
    (internal-border-width . 0)
    (modeline . t)
    (fringe)
    (cursor-color . "Red")
    (background-mode . light)
    (tool-bar-lines . 1)
    (fontsize . 0)
    (font-backend mac-ct ns))))
;;(setq-default fringe-mode (quote (nil . 0)) nil (fringe))
(setq-default ggtags-highlight-tag 0.1)
(setq-default ggtags-sort-by-nearness t)
(setq-default inhibit-startup-screen t)
(setq-default ns-antialias-text t)
;;(setq-default package-archives
;; (quote
;;  (("melpa" . "http://melpa.org/packages/")
;;   ("gnu" . "http://elpa.gnu.org/packages/")
;;   )))
(setq-default semantic-mode nil)
(setq-default send-mail-function nil)
(setq-default show-paren-mode t)
(setq-default sln-auto-newline nil)
(setq-default sln-basic-offset 4)
(setq-default sln-semi-is-electric nil)
(setq-default speedbar-show-unknown-files t)
(setq-default verilog-auto-delete-trailing-whitespace t)
(setq-default verilog-auto-newline nil)
(setq-default verilog-case-indent 4)
(setq-default verilog-cexp-indent 4)
(setq-default verilog-compiler "make all")
(setq-default verilog-highlight-modules t)
(setq-default verilog-indent-begin-after-if nil)
(setq-default verilog-indent-level 4)
(setq-default verilog-indent-level-behavioral 4)
(setq-default verilog-indent-level-declaration 4)
(setq-default verilog-indent-level-directive 0)
(setq-default verilog-indent-level-module 0)
(setq-default verilog-simulator "make run")
(setq-default whitespace-style (quote (face trailing spaces tabs)))

