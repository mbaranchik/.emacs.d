;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025 Michael Baranchik
;; Author: Michael Baranchik <mbaranch@amazon.com>

;;; Commentary:
;; Modular Emacs configuration with straight.el package management.

;;; Code:

;;;; Constants and Paths
(defconst user-init-dir (concat user-emacs-directory "lisp.d/"))

;;;; Core Functions
(defun load-user-file (file)
  "Load FILE from `user-init-dir' and require its feature."
  (interactive "f")
  (load (expand-file-name file user-init-dir))
  (require (intern (concat "my/" file))))

;;;; Bootstrap
(load-user-file "basic")
(load-user-file "straight-bootstrap")

;;;; Performance Settings
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 200 1024 1024)
                  read-process-output-max (* 1024 1024))))

(setq byte-compile-warnings '(cl-functions)
      load-prefer-newer t)

;;;; Core Packages
(use-package benchmark-init
  :hook (after-init . benchmark-init/deactivate))

(use-package bind-key)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package gcmh
  :commands gcmh-mode)

(require 'compat)

;; Compat-31 skips `set-local' on Emacs >= 31.0.50, but the function
;; isn't actually in the emacs-31 branch yet. Provide it ourselves.
(unless (fboundp 'set-local)
  (defun set-local (variable value)
    "Make VARIABLE buffer local and set it to VALUE."
    (set (make-local-variable variable) value)))

(use-package filelock)

;;;; Module Loading
(dolist (module '(("Prog" . "prog")
                  ("General" . "general")
                  ("Whitespace" . "whitespace")
                  ("Highlight-Global" . "highlight-global")
                  ("Git" . "git")
                  ("Completion-Next-Gen" . "completion-next-gen")
                  ("Custom" . "custom")
                  ("LSP" . "lsp")
                  ("Keybindings" . "keybindings")
                  ("IDE" . "ide")
                  ("Navigation" . "navigation")
                  ("TMUX" . "tmux")
                  ("Org" . "org")
                  ("Theme" . "theme")))
  (bench-wrap (car module) (load-user-file (cdr module))))

;;;; Additional Packages
(bench-wrap "Compile" (use-package compile))

(bench-wrap "Exec-Path"
  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :config
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode '("!=" "==" "->" "<-" ">=" "<=" ">>" "<<" ">>=" "<<="))
  :custom
  (global-ligature-mode t))


;;;; Custom Hooks
(bench-wrap "Custom Hooks"
  (add-hook 'prog-mode-hook #'unshow-ws))

;;;; Customization File
(filelock-with-lock
    (setq custom-file (expand-file-name "local_customized.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

;;;; Miscellaneous Settings
(put 'upcase-region 'disabled nil)

(when (boundp 'server-name)
  (setq recentf-save-file (format "/tmp/recentf.%s" server-name)))

;;;; TRAMP Configuration
(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;;; Utility Functions
(defun touch-buffer-file ()
  "Update mtime on the file for the current buffer."
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (expand-file-name (buffer-file-name)))))
  (clear-visited-file-modtime))

;;;; Server
(message "Sockets Dir: %s" server-socket-dir)
(when (config-wrap "server")
  (server-start))

;;;; Dotfiles Overlay Fragments
;; Keep .emacs.d generic while allowing dotfiles layers to add numbered overlay
;; fragments in ~/.emacs.d-overlays/*.el.
(let ((overlay-dir (expand-file-name "~/.emacs.d-overlays/")))
  (when (file-directory-p overlay-dir)
    (dolist (file (directory-files overlay-dir t "^[^.].*\\.el$" t))
      (load file))))

(provide 'init)
;;; init.el ends here
