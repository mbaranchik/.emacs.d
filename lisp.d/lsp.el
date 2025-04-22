;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

;; Enable debug prints for troubleshooting
(setq my/enable-debug-prints nil)

;; Define server command mapping
(defvar my/lsp-server-commands
  '(("clangd" . "clangd")
    ("ccls" . "ccls")
    ("pyright" . "pyright-langserver --stdio")
    ("pylsp" . "pylsp")
    ("pylyzer" . "pylyzer --server --verbose 2")
    ("bash-language-server" . "bash-language-server start")
    ("amz-q" . "node /Users/mbaranch/AmazonQNVim/language-server/build/aws-lsp-codewhisperer-token-binary.js --stdio")
    ("typescript-language-server" . "typescript-language-server --stdio")
    ("tailwindcss-language-server" . "tailwindcss-language-server --stdio")
    ("eslint-lsp" . "eslint-lsp --stdio"))
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

(defun my/build-lsp-multi-command (mode)
  "Build the lsp-multi command for MODE with all enabled servers."
  (let ((servers (my/get-lsp-servers-for-mode mode))
        (lsp-multi-args '()))
    (my/debug-print "Building lsp-multi command for %s with servers: %S" mode servers)
    (dolist (server-config servers)
      (my/debug-print "Processing server config: %S" server-config)
      (when (my/convert-json-bool (alist-get 'enable server-config))
        (let* ((server-name (alist-get 'server server-config))
               (custom-command (alist-get 'command server-config))
               (server-command (if custom-command
                                  custom-command
                                (my/get-server-command server-name))))
          (my/debug-print "Server %s enabled with command: %s" server-name server-command)
          (when server-command
            (push (list "--lsp" server-command) lsp-multi-args)))))

    (when lsp-multi-args
      (let ((cmd (apply #'append (list "lsp-multi") (nreverse lsp-multi-args))))
        (my/debug-print "Final lsp-multi command list: %S" cmd)
        cmd))))

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

  (use-package eglot
    :straight nil
    :demand t
    :hook
    ((my/c-mode) . (lambda () (my/eglot-enable 'c)))
    ((my/c++-mode) . (lambda () (my/eglot-enable 'cpp)))
    ((my/python-mode) . (lambda () (my/eglot-enable 'python)))
    ((my/sh-mode) . (lambda () (my/eglot-enable 'bash)))
    :config
    ;; Configure lsp-multi for C/C++
    (let ((c-lsp-multi-cmd (my/build-lsp-multi-command "c")))
      (when c-lsp-multi-cmd
        (my/debug-print "Setting up eglot for C with command: %S" c-lsp-multi-cmd)
        (push `((,(my/get-mode "c")) . ,c-lsp-multi-cmd) eglot-server-programs)))

    ;; Configure lsp-multi for C++
    (let ((cpp-lsp-multi-cmd (my/build-lsp-multi-command "cpp")))
      (when cpp-lsp-multi-cmd
        (my/debug-print "Setting up eglot for C++ with command: %S" cpp-lsp-multi-cmd)
        (push `((,(my/get-mode "c++")) . ,cpp-lsp-multi-cmd) eglot-server-programs)))

    ;; Configure lsp-multi for Python
    (let ((python-lsp-multi-cmd (my/build-lsp-multi-command "python")))
      (when python-lsp-multi-cmd
        (my/debug-print "Setting up eglot for Python with command: %S" python-lsp-multi-cmd)
        (push `((,(my/get-mode "python")) . ,python-lsp-multi-cmd) eglot-server-programs)))

    ;; Configure lsp-multi for Bash
    (let ((bash-lsp-multi-cmd (my/build-lsp-multi-command "bash")))
      (when bash-lsp-multi-cmd
        (my/debug-print "Setting up eglot for Bash with command: %S" bash-lsp-multi-cmd)
        (push `((,(my/get-mode "sh")) . ,bash-lsp-multi-cmd) eglot-server-programs))))

  (use-package eglot-booster
    :straight (:host github :repo "jdtsmith/eglot-booster")
    :after eglot
    :config (eglot-booster-mode))
  )

;; Keep lsp-bridge and lsp-mode configurations for backward compatibility
;; but mark them as legacy with comments

;; Legacy lsp-bridge support
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

;; Legacy lsp-mode support
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
    :commands lsp)
  )

;; Turn off debug prints after initialization
(add-hook 'after-init-hook (lambda () (setq my/enable-debug-prints nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my/lsp)
