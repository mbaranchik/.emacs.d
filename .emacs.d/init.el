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

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Avoid loading older byte-compiled
(setq load-prefer-newer t)

;; Auto compile lisp
(use-package auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(defconst user-init-dir "~/emacs.init/")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load (expand-file-name file user-init-dir)))

;;(load-user-file "packages.el")

(use-package benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(load-user-file "config")

;;(load-user-file "filelock.el")
(use-package filelock)

(load-user-file "general")

(when use-cua
  (load-user-file "cua"))

(when use-whitespace
  (load-user-file "whitespace"))

(load-user-file "highlight-global")

(when use-navigation
  (load-user-file "navigation"))

(when use-gitblame
  (load-user-file "git-blame"))

(when use-git
  (load-user-file "git"))

(when use-helm
  (load-user-file "helm"))

(when use-ido
  (load-user-file "ido"))

(when use-ivy
  (load-user-file "ivy"))

(when use-projectile
  (use-package projectile)
  (projectile-mode t))
(when (and use-helm use-projectile)
  (use-package helm-projectile))
(when (and use-ivy use-projectile)
  (use-package counsel-projectile))

(when use-tags
  (load-user-file "tags"))

(load-user-file "custom")

(when use-clang
  (load-user-file "clang")
  (add-hook 'c-mode-hook 'clang-format+-mode)
  (add-hook 'c++-mode-hook 'clang-format+-mode)
  )

(load-user-file "git_sync")
(when use-gitsync
  (use-package git-sync)
  (git-sync-mode t))

(load-user-file "keybindings")

(use-package compile)

(eval-after-load "verilog-mode"
  '(progn
     (setq compilation-read-command t)
     (setq verilog-tool 'verilog-linter)
     (add-hook 'verilog-mode-hook
               (lambda ()
                 (set (make-local-variable 'compile-command)
                      (let ((verif_path (car (split-string (cadr (split-string (cadr (split-string (file-truename (expand-file-name (file-name-directory buffer-file-name))) "/Volumes/ANPA/")) "sync/")) "/"))))
                        (format "ssh dub-gw015 \"echo 'rm /project/users/michaelba/toprock/%s/err.log \\n touch /project/users/michaelba/toprock/%s/.check \\n while (! -f /project/users/michaelba/toprock/%s/err.log) \\n end \\n sleep 2 && tail -n +1 /project/users/michaelba/toprock/%s/err.log' | csh -f\"" verif_path verif_path verif_path verif_path)))
                 (set (make-local-variable 'verilog-linter) (symbol-value 'compile-command))
                 ))
  )
)

(setq exec-path-from-shell-arguments nil)
(exec-path-from-shell-initialize)


(put 'upcase-region 'disabled nil)

;; LOAD THEME ;;
(load-user-file "theme")

(add-hook 'prog-mode-hook #'unshow-ws)
(add-hook 'sln-mode-hook #'unshow-ws)

;; HACK
(add-hook 'c-mode-hook (lambda() (define-key c-mode-map (kbd "C-c C-c") nil)))
(add-hook 'c++-mode-hook (lambda() (define-key c++-mode-map (kbd "C-c C-c") nil)))

(filelock-with-lock
 (setq custom-file (expand-file-name "local_customized.el" user-emacs-directory))
 (load custom-file)
 )

;; Avoid locks on recentf
(when (boundp 'server-name)
  (setq recentf-save-file (format "/tmp/recentf.%s" server-name)))

(byte-recompile-directory user-init-dir 0)

;; Server Start
(when start-server
  (server-start))

(gcmh-mode 1)

