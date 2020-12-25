;;; -*- lexical-binding: t -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; This is now done with GCMH - Testing
(add-hook 'after-init-hook (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

(use-package benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;;(straight-use-package `(bench
;;                        :local-repo ,(concat user-emacs-directory "lisp")))

;; TODO: understand why straight f***s this up...
(defmacro bench (name &rest code)
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

(use-package bind-key)

;; Avoid loading older byte-compiled
(setq load-prefer-newer t)

;; Auto compile lisp
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Load GCMH
(use-package gcmh
  :commands gcmh-mode)

(defconst user-init-dir "~/emacs.init/")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load (expand-file-name file user-init-dir)))

;;(load-user-file "packages.el")

(bench "Config"
       (load-user-file "config"))

;;(load-user-file "filelock.el")
(use-package filelock)

(bench "General"
       (load-user-file "general"))

(bench "CUA"
       (when use-cua
         (load-user-file "cua")))

(bench "Whitespace"
       (when use-whitespace
         (load-user-file "whitespace")))

(bench "Higlight-Global"
       (load-user-file "highlight-global"))

(bench "Navigation"
       (when use-navigation
         (load-user-file "navigation")))

(bench "Git"
       (when use-git
         (load-user-file "git")))

(bench "Helm"
       (when use-helm
         (load-user-file "helm")))

(bench "Ido"
       (when use-ido
         (load-user-file "ido")))

(bench "Ivy"
       (when use-ivy
         (load-user-file "ivy")))

(bench "Projectile"
       (when use-projectile
         (use-package projectile)
         (projectile-mode t))
       (when (and use-helm use-projectile)
         (use-package helm-projectile))
       (when (and use-ivy use-projectile)
         (use-package counsel-projectile)))

(bench "Tags"
       (when use-tags
         (load-user-file "tags")))

(bench "Custom"
       (load-user-file "custom"))

(bench "Clang"
       (when use-clang
         (load-user-file "clang")
         (add-hook 'c-mode-hook 'clang-format+-mode)
         (add-hook 'c++-mode-hook 'clang-format+-mode)
         ))

(bench "Git-Sync"
       (load-user-file "git_sync")
       (when use-gitsync
         (use-package git-sync
           :hook (prog-mode . git-sync-mode))))

(bench "Keybindings"
       (load-user-file "keybindings"))

(bench "Compile"
       (use-package compile))

(bench "Exec-Path"
       (setq exec-path-from-shell-arguments nil)
       (exec-path-from-shell-initialize))

(put 'upcase-region 'disabled nil)

;; LOAD THEME ;;
(bench "Theme"
       (load-user-file "theme"))

(bench "Custom Hooks"
       (add-hook 'prog-mode-hook #'unshow-ws)
       (add-hook 'sln-mode-hook #'unshow-ws)
       ;; HACK
       (add-hook 'c-mode-hook (lambda() (define-key c-mode-map (kbd "C-c C-c") nil)))
       (add-hook 'c++-mode-hook (lambda() (define-key c++-mode-map (kbd "C-c C-c") nil))))

(filelock-with-lock
 (setq custom-file (expand-file-name "local_customized.el" user-emacs-directory))
 (load custom-file)
 )

;; Avoid locks on recentf
(when (boundp 'server-name)
  (setq recentf-save-file (format "/tmp/recentf.%s" server-name)))

(byte-recompile-directory user-init-dir 0)

;; Server Start
(message "Sockets Dir: %s" server-socket-dir)
(when start-server
  (server-start))

;;(gcmh-mode 1)

