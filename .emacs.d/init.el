;;; -*- lexical-binding: t -*-

(defconst user-init-dir "~/emacs.init/")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load (expand-file-name file user-init-dir)))

(setq gc-cons-threshold 1073741824
      gc-cons-percentage 0.6)

(load-user-file "packages.el")

(load-user-file "config")

(load-user-file "filelock.el")
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

(load-user-file "clang")
(when use-clang
  (use-package clang-format-on-save)
  (add-hook 'c-mode-hook 'clang-format-on-save-mode)
  (add-hook 'c++-mode-hook 'clang-format-on-save-mode)
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

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))


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
(setq recentf-save-file (format "/tmp/recentf.%s" server-name))

(byte-recompile-directory user-init-dir 0)

;; Server Start
(when start-server
  (server-start))

