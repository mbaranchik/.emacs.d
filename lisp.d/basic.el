;;; -*- lexical-binding: t -*-

;; Benchmark macro
(defmacro bench-wrap (name &rest code)
  `(cond ((string= "y" (getenv "EMACS_BENCHMARK"))
          (progn
             (message "Benchmark for '%s':" ,name)
             (benchmark-progn
               ,@code
               )
             ))
         (t (progn
               ,@code
               ))
         )
  )

;; Macro for graphics dependant code
(defmacro daemon-wrap (name &rest code)
  `(if (daemonp)
       (cl-labels ((,name (frame)
                          (with-selected-frame frame
                            ,@code)
                          (remove-hook 'after-make-frame-functions #',name)))
         (add-hook 'after-make-frame-functions #',name))
     ,@code
     )
  )

;; Macro for getting config value
(defmacro config-wrap (name)
  `(symbol-value (intern (concat "my/" ,name)))
  )

;; Function for setting a configuration variable with default value and override
(defun set-config-var (varname default &optional env)
  (let ((varsym (intern (concat "my/" varname)))
        (envvar (getenv (or env ""))))
      (cond ((or (string= "1" envvar) (string= "y" envvar)) (set varsym t))
	        ((or (string= "0" envvar) (string= "n" envvar)) (set varsym nil))
	        (envvar (set varsym (getenv env)))
            (t (set varsym default))
            )
    )
  )

;; Same as above, but verifies boolean
(defun set-config-bool-var (varname default &optional env)
  (let ((varsym (intern (concat "my/" varname)))
        (envvar (getenv (or env ""))))
      (cond ((or (string= "1" envvar) (string= "y" envvar)) (set varsym t))
	        ((or (string= "0" envvar) (string= "n" envvar)) (set varsym nil))
	        (envvar (warn "%s is boolean, can recieve {y, n, 1, 0}. received '%s'" env envvar))
            (t (set varsym default))
            )
    )
  )

;; Same as above, only creates a symbol out of the value
(defun set-config-quote-var (varname default &optional env)
  (let ((varsym (intern (concat "my/" varname)))
        (envvar (getenv (or env ""))))
    (cond ((getenv env) (set varsym (intern envvar)))
	      (t (set varsym default))
          )
    )
  )

;; Server Enable
(set-config-bool-var "start-server" t "EMACS_START_SERVER")

;; LSP
;; ["lsp", "eglot", "lsp-bridge"]
(set-config-bool-var "use-lsp" nil)
(set-config-bool-var "use-eglot" nil)
(set-config-bool-var "use-lsp-bridge" nil)
(set-config-var "lsp" "lsp-bridge" "EMACS_LSP")
(cond ((string= "lsp" (config-wrap "lsp")) (set-config-bool-var "use-lsp" t))
      ((string= "eglot" (config-wrap "lsp")) (set-config-bool-var "use-eglot" t))
      ((string= "lsp-bridge" (config-wrap "lsp")) (set-config-bool-var "use-lsp-bridge" t))
      (t (if (config-wrap "lsp")
             (warn "EMACS_LSP env var can receive one of lsp|eglot|lsp-bridge, received %s instead" (config-wrap "lsp"))))
      )
(set-config-var "lsp/cpp-backend" "clangd" "EMACS_LSP_CPP_BACKEND")
(cond ((string= "clangd" (config-wrap "lsp/cpp-backend")) (set-config-bool-var "use-lsp-clangd" t))
      ((string= "ccls" (config-wrap "lsp/cpp-backend")) (set-config-bool-var "use-lsp-ccls" t))
      (t (if (config-wrap "lsp/cpp-backend")
             (warn "EMACS_LSP_CPP_BACKEND env var can receive one of ccls|clangd, received %s instead" (config-wrap "lsp/cpp-backend"))))
      )

;; Completion - Code
;; ["company", "corfu", "acm"]
(set-config-bool-var "use-company" nil)
(set-config-bool-var "use-corfu" nil)
(set-config-var "autocomplete" nil "EMACS_AUTOCOMPLETE")
(cond ((string= "company" (config-wrap "autocomplete")) (set-config-bool-var "use-company" t))
      ((string= "corfu" (config-wrap "autocomplete")) (set-config-bool-var "use-corfu" t))
      (t (if (config-wrap "autocomplete")
             (warn "EMACS_AUTOCOMPLETE env var can receive one of company|ac, received %s instead" (config-wrap "autocomplete"))))
      )

;; Diagnostics - Code
;; ["flycheck", "flymake"]
(set-config-bool-var "use-flycheck" nil)
(set-config-bool-var "use-flymake" nil)
(set-config-var "code-diag" "" "EMACS_DIAGNOSTICS")
(cond ((string= "flycheck" (config-wrap "code-diag")) (set-config-bool-var "use-flycheck" t))
      ((string= "flymake" (config-wrap "code-diag")) (set-config-bool-var "use-flymake" t))
      ((string= "" (config-wrap "code-diag")) (message "skipping code-diag"))
      (t (if (config-wrap "code-diag")
             (warn "EMACS_DIAGNOSTICS env var can receive one of flycheck|flymake, received %s instead" (config-wrap "code-diag"))))
      )

;; UI - General
(set-config-var "theme-name" "doom-vibrant" "EMACS_USE_THEME")
(set-config-quote-var "theme-sym" 'doom-vibrant "EMACS_USE_THEME")
(set-config-bool-var "use-idle-highlight" nil "EMACS_USE_IDLE_HIGHLIGHT")
(set-config-bool-var "use-visual-line-mode" nil "EMACS_USE_VISUAL_LINE_MODE")
(set-config-bool-var "use-indent-guide" nil "EMACS_USE_INDENT_GUIDE")
(set-config-bool-var "use-which-function" nil "EMACS_USE_WHICH_FUNCTION")
(set-config-bool-var "use-diff-hl" t)
(set-config-bool-var "use-git-gutter" nil)
(set-config-var "vc-gutter" "diff-hl" "EMACS_VC_GUTTER")
(cond ((string= "diff-hl" (config-wrap "vc-gutter")) (set-config-bool-var "use-diff-hl" t))
      ((string= "git-gutter" (config-wrap "vc-gutter")) (set-config-bool-var "use-git-gutter" t))
      (t (if (config-wrap "vc-gutter")
             (warn "EMACS_VC_GUTTER env var can receive one of diff-hl|git-gutter, received %s instead" (config-wrap "vc-gutter"))))
      )
;; UI - Modeline
(set-config-bool-var "use-doom-modeline" nil)
(set-config-bool-var "use-mood-modeline" nil)
(set-config-var "modeline" "doom" "EMACS_MODELINE")
(cond ((string= "doom" (config-wrap "modeline")) (set-config-bool-var "use-doom-modeline" t))
      ((string= "mood" (config-wrap "modeline")) (set-config-bool-var "use-mood-modeline" t))
      (t (if (config-wrap "modeline")
             (warn "EMACS_MODELINE env var can receive one of doom|mood, received %s instead" (config-wrap "modeline"))))
      )

;; Code - Misc
(set-config-var "flycheck-clang-tidy-executable" "clang-tidy" "EMACS_FLYCHECK_TIDY_EXEC")

;; Specifics
(set-config-var "auto-insert-copyright" "REPLACE_WITH_COPYRIGHT" "EMACS_COPYRIGHT_COMPANY")
(set-config-var "auto-insert-name" "REPLACE_WITH_NAME" "EMACS_COPYRIGHT_NAME")


;; Customs - Vars
(setq-default server-socket-dir "~/.emacs.d/server-sock")
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
(setq-default inhibit-startup-screen t)
(setq-default ns-antialias-text t)
(setq-default semantic-mode nil)
(setq-default send-mail-function nil)
(setq-default show-paren-mode t)
(setq-default speedbar-show-unknown-files t)
(setq-default whitespace-style (quote (face trailing spaces tabs)))
(setq-default display-time-mode t)
(setq-default global-display-line-numbers-mode t)
(setq-default line-numbers-mode t)
(setq-default column-numbers-mode t)
(defalias 'yes-or-no #'y-or-n-p)
(setopt use-short-answers t)
(setq confirm-kill-emacs #'yes-or-no-p)

(provide 'my/basic)
