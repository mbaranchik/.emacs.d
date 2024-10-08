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
    (cond (envvar (set varsym (intern envvar)))
	      (t (set varsym (intern default)))
          )
    )
  )

;; Set defaults
(set-config-bool-var "start-server" nil)
(set-config-var "lsp" "")
(set-config-var "lsp/cpp-backend" "")
(set-config-var "lsp/py-backend" "")
(set-config-var "lsp/enable-modes" '())
(set-config-var "ide/autoformat-enable-modes" '())
(set-config-var "ide/diagnostics-enable-modes" '())
(set-config-var "autocomplete" "")
(set-config-var "code-diag" "")
(set-config-var "modeline" "")
(set-config-var "theme-name" "default")
(set-config-bool-var "use-idle-highlight" nil)
(set-config-bool-var "use-visual-line-mode" nil)
(set-config-bool-var "use-indent-guide" nil)
(set-config-bool-var "use-which-function" nil)
(set-config-bool-var "use-diff-hl" t)
(set-config-var "basic-indent-offset" 4)
(set-config-bool-var "use-indent-tabs" nil)
(set-config-var "auto-insert-copyright" "")
(set-config-var "auto-insert-name" "")

;; Load user configuration
(defconst user-config (expand-file-name "config.el" user-emacs-directory))
(when (file-exists-p user-config)
  (load user-config))

(set-config-quote-var "theme-sym" (config-wrap "theme-name"))

;; LSP
;; ["lsp", "eglot", "lsp-bridge"]
(set-config-bool-var "use-lsp-mode" nil)
(set-config-bool-var "use-eglot" nil)
(set-config-bool-var "use-lsp-bridge" nil)
(cond ((string= "lsp-mode" (config-wrap "lsp")) (set-config-bool-var "use-lsp-mode" t))
      ((string= "eglot" (config-wrap "lsp")) (set-config-bool-var "use-eglot" t))
      ((string= "lsp-bridge" (config-wrap "lsp")) (set-config-bool-var "use-lsp-bridge" t))
      ((string= "" (config-wrap "lsp")) (message "skipping LSP"))
      (t (if (config-wrap "lsp")
             (warn "EMACS_LSP env var can receive one of lsp-mode|eglot|lsp-bridge, received %s instead" (config-wrap "lsp"))))
      )
(cond ((string= "clangd" (config-wrap "lsp/cpp-backend")) (set-config-bool-var "use-lsp-clangd" t))
      ((string= "ccls" (config-wrap "lsp/cpp-backend")) (set-config-bool-var "use-lsp-ccls" t))
      ((string= "" (config-wrap "lsp/cpp-backend")) (message "skipping cpp-backend"))
      (t (if (config-wrap "lsp/cpp-backend")
             (warn "EMACS_LSP_CPP_BACKEND env var can receive one of ccls|clangd, received %s instead" (config-wrap "lsp/cpp-backend"))))
      )

;; Completion - Code
;; ["company", "corfu", "acm"]
(set-config-bool-var "use-company" nil)
(set-config-bool-var "use-corfu" nil)
(cond ((string= "company" (config-wrap "autocomplete")) (set-config-bool-var "use-company" t))
      ((string= "corfu" (config-wrap "autocomplete")) (set-config-bool-var "use-corfu" t))
      ((string= "" (config-wrap "autocomplete")) (message "skipping autocomplete"))
      (t (if (config-wrap "autocomplete")
             (warn "EMACS_AUTOCOMPLETE env var can receive one of company|ac, received %s instead" (config-wrap "autocomplete"))))
      )

;; Diagnostics - Code
;; ["flycheck", "flymake"]
(set-config-bool-var "use-flycheck" nil)
(set-config-bool-var "use-flymake" nil)
(cond ((string= "flycheck" (config-wrap "code-diag")) (set-config-bool-var "use-flycheck" t))
      ((string= "flymake" (config-wrap "code-diag")) (set-config-bool-var "use-flymake" t))
      ((string= "" (config-wrap "code-diag")) (message "skipping code-diag"))
      (t (if (config-wrap "code-diag")
             (warn "EMACS_DIAGNOSTICS env var can receive one of flycheck|flymake, received %s instead" (config-wrap "code-diag"))))
      )

;; UI - Modeline
(set-config-bool-var "use-doom-modeline" nil)
(set-config-bool-var "use-mood-modeline" nil)
(cond ((string= "doom" (config-wrap "modeline")) (set-config-bool-var "use-doom-modeline" t))
      ((string= "mood" (config-wrap "modeline")) (set-config-bool-var "use-mood-modeline" t))
      ((string= "" (config-wrap "modeline")) (message "skipping modeline"))
      (t (if (config-wrap "modeline")
             (warn "EMACS_MODELINE env var can receive one of doom|mood, received %s instead" (config-wrap "modeline"))))
      )

;; Customs - Vars
(setq-default server-socket-dir (concat user-emacs-directory "server-sock"))
(setq-default inhibit-startup-screen t)
(setq-default semantic-mode nil)
(setq-default send-mail-function nil)


(setq-default default-frame-alist
              (quote
               ((cursor-type . (bar . 3))
                (internal-border-width . 0)
                (modeline . t)
                (fringe)
                (cursor-color . "Red")
                (tool-bar-lines . 1)
                (fontsize . 0)
                (font-backend mac-ct ns))))
(setq-default whitespace-style (quote (face trailing spaces tabs)))

(defalias 'yes-or-no #'y-or-n-p)
(setopt use-short-answers t)
(setq confirm-kill-emacs #'yes-or-no-p)

(daemon-wrap my/confirm-client-exit
             (define-advice save-buffers-kill-terminal (:around (oldfun &rest args) my/delete-client)
               "Confirm deleting the client."
               (interactive)
               (when (y-or-n-p "Confirm exit ? ")
                 (apply oldfun args))))

(provide 'my/basic)
