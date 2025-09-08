;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

;; Enable debug prints for troubleshooting
(setq my/enable-debug-prints nil)

;; Define server command mapping
(defvar my/lsp-server-commands
  `(("clangd" . ("clangd"))
    ("ccls" . ("ccls"))
    ("pyright" . ("pyright-langserver" "--stdio"))
    ("pylsp" . ("pylsp"))
    ("pylyzer" . ("pylyzer" "--server" "--verbose" "2"))
    ("bash-language-server" . ("bash-language-server" "start")))
  "Mapping of server names to their command strings.")

(defun my/get-server-command (server)
  "Get the command for SERVER from the server commands map."
  (or (cdr (assoc server my/lsp-server-commands))
      server))

(defun my/get-lsp-servers-for-mode (mode)
  "Get the list of LSP server configurations for MODE."
  (let ((mode-config (config-wrap (concat "lsp/" mode))))
    (my/debug-print "Mode config for %s: %S (type: %s)" mode mode-config (type-of mode-config))
    (cond
     ;; New format: vector of server configs from JSON
     ((and mode-config (vectorp mode-config))
      (my/debug-print "Converting vector to list for %s" mode)
      (append mode-config nil)) ;; Convert vector to list

     ;; Old format: single server config as alist
     ((and mode-config (listp mode-config) (alist-get 'server mode-config))
      (my/debug-print "Using old format config for %s" mode)
      (let ((enabled (my/convert-json-bool (alist-get 'enable mode-config)))
            (server (alist-get 'server mode-config)))
        (when (and enabled server)
          (list mode-config))))

     ;; No config
     (t
      (my/debug-print "No config found for %s" mode)
      nil))))

(defun my/should-enable-lsp-for-mode (mode)
  "Check if LSP should be enabled for MODE."
  (let ((servers (my/get-lsp-servers-for-mode mode)))
    (my/debug-print "Checking if LSP should be enabled for %s, servers: %S" mode servers)
    (cl-some (lambda (server-config)
               (my/convert-json-bool (alist-get 'enable server-config)))
             servers)))

(when (string= (config-wrap "lsp/package") "eglot")
  (defun my/eglot-enable (type)
    (let ((mode-name (symbol-name type)))
      (my/debug-print "Checking if eglot should be enabled for %s" mode-name)
      (when (my/should-enable-lsp-for-mode mode-name)
        (my/debug-print "Enabling eglot for %s" mode-name)
        (hack-local-variables)
        (when (config-wrap "ui/which-function")
          (which-function-mode))
        (when (or (not (member type (get-enabled-modes "code-diag"))) (not (string= (config-wrap "code-diag/package") "flymake")))
          (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t)
          (flymake-mode 0)
          (add-to-list (make-local-variable 'eglot-stay-out-of) 'flymake))
        ;; Disable formatting capabilities if not needed
        (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentOnTypeFormattingProvider)
        (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentFormattingProvider)
        (add-to-list (make-local-variable 'eglot-ignored-server-capabilities) :documentRangeFormattingProvider)
        (eglot-ensure))))

  (use-package markdown-mode)
  (unless (fboundp 'project-root)
    (defun project-root (project)
      (car (project-roots project))))

  (defun my/map-mode-to-eglot-mode (mode)
    (pcase mode
      ("bash"
        `(,(my/get-mode mode) :language-id "shell"))
      (_
        (my/get-mode mode))))

  (use-package eglot
    :straight nil
    :demand t
    :hook
    ((my/c-mode) . (lambda () (my/eglot-enable 'c)))
    ((my/c++-mode) . (lambda () (my/eglot-enable 'cpp)))
    ((my/python-mode) . (lambda () (my/eglot-enable 'python)))
    ((my/sh-mode) . (lambda () (my/eglot-enable 'bash)))
    :custom
    ;; Don't block UI until LSP connection is accomplished
    (eglot-sync-connect nil)
    :config
    (setq-default amz-q-lsp-supported-modes '())
    (dolist (mode '("c" "cpp" "python" "bash"))
      (let ((servers (my/get-lsp-servers-for-mode mode)))
        (dolist (server-config servers)
          (when (my/convert-json-bool (alist-get 'enable server-config))
            (let* ((server-name (alist-get 'server server-config))
                    (custom-command (alist-get 'command server-config))
                    (server-command (if custom-command
                                      custom-command
                                      (my/get-server-command server-name))))
              (my/debug-print "Server %s enabled with command: %s" server-name server-command)
              (if server-command
                (if (string= server-name "amz-q")
                  (push (my/map-mode-to-eglot-mode mode) amz-q-lsp-supported-modes)
                 (push `((,(my/map-mode-to-eglot-mode mode)) . ,server-command) eglot-server-programs))
                (error "No matching command found for server %s and no custom command supplied" server-name)))))))
      ;; ;; Enable EGLOT debug logging
      ;; (eglot-events-buffer-config
      ;;   '(:size 2000000 :format lisp))
      )

  (use-package lsp-multi
      :straight '(lsp-multi
                  :type git
                  :host github
                  :repo "mbaranchik/lsp-multi"
                  :files ("*"))
      :after eglot
      :custom
      (lsp-multi-executable nil)     ; Auto-detect (default)
      (lsp-multi-debug nil)            ; Enable debugging
      (lsp-multi-error-timeout 0)    ; Wait indefinitely
      :config
      ;; Install if needed
      (unless (executable-find "lsp-multi")
        (lsp-multi-install))
      ;; ;; Enable integration
      ;; (lsp-multi-enable)
    )

  (use-package eglot-booster
    :straight (:host github :repo "jdtsmith/eglot-booster")
    :after eglot
    :config (eglot-booster-mode)))

;; lsp-bridge support
(when (string= (config-wrap "lsp/package") "lsp-bridge")
  (message "Warning: lsp-bridge is not supported with lsp-multi multiplexer")
  (defun my/lsp-bridge-enable (type)
    (when (my/should-enable-lsp-for-mode (symbol-name type))
      (hack-local-variables)
      (lsp-bridge-mode)
      (when (config-wrap "ui/which-function")
        (which-function-mode))
      (when (string= (config-wrap "code-diag/package") "flycheck")
        (flycheck-mode 0))
      (when (string= (config-wrap "code-diag/package") "flymake")
        (flymake-mode 0))))
  (use-package markdown-mode)
  (use-package lsp-bridge
    :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                           :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                           :build (:not compile))

    :after (markdown-mode yasnippet)
    :hook
    ((my/c-mode) . (lambda () (my/lsp-bridge-enable 'c)))
    ((my/c++-mode) . (lambda () (my/lsp-bridge-enable 'cpp)))
    ((my/python-mode) . (lambda () (my/lsp-bridge-enable 'python)))
    ((my/sh-mode) . (lambda () (my/lsp-bridge-enable 'bash)))
    :init
    (setq tab-always-indent t)
    (defun lsp-bridge-indent-for-tab-command (&optional arg)
      (interactive "P")
      (lsp-bridge-popup-complete-menu)
      (indent-for-tab-command arg))
    :config
    (push "find-file" lsp-bridge-completion-stop-commands)
    (setq lsp-bridge-python-command (string-trim
                                     (shell-command-to-string "pyenv which python3")))
    :custom
    (lsp-bridge-c-lsp-server (config-wrap "lsp/cpp/server"))
    (lsp-bridge-enable-hover-diagnostic t)
    (lsp-bridge-remote-start-automatically t)
    (acm-enable-yas t)
    (acm-enable-tabnine nil)
    :bind (
           ("M-i"   . 'lsp-bridge-find-impl)
           ("M-I"   . 'lsp-bridge-find-impl-other-window)
           ("M-t"   . 'lsp-bridge-find-def)
           ("M-,"   . 'lsp-bridge-find-def-return)
           ("M-T"   . 'lsp-bridge-find-def-other-window)
           ("M-r"   . 'lsp-bridge-find-references)
           ("<tab>" . 'lsp-bridge-indent-for-tab-command)
           )
    )
  (daemon-wrap my/load-acm-terminal
               (unless (display-graphic-p)
                 (use-package popon
                   :straight (:host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
                 (use-package acm-terminal
                   :straight (:host github :repo "twlz0ne/acm-terminal")
                   :after (lsp-bridge popon))
                 ))
  )

;; lsp-mode support
(when (string= (config-wrap "lsp/package") "lsp-mode")
  (message "Warning: lsp-mode is not supported with lsp-multi multiplexer")
  (use-package lsp-mode
    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
           ((my/c-mode) . lsp)
           ((my/c++-mode) . lsp)
           ((my/python-mode) . lsp)
           ((my/sh-mode) . lsp)
           ;; if you want which-key integration
           (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp
    :config
    ;; ;; Enable LSP-mode debug logging
    ;; (setq lsp-log-io t)                    ; Log all JSON-RPC communication
    ;; (setq lsp-print-performance t)         ; Log performance metrics
    ;; (setq lsp-trace t)                     ; Enable tracing
    ;; (setq lsp-print-io t)                  ; Print IO to messages buffer
    ;; (setq lsp-log-max 10000)              ; Increase log buffer size
    ;; (setq lsp-enable-file-watchers nil)    ; Disable file watchers to reduce noise
    )
  )

;; Turn off debug prints after initialization
(add-hook 'after-init-hook (lambda () (setq my/enable-debug-prints nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my/lsp)
