
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(unless (boundp 'aquamacs-version)
  (setq use-spacemacs nil)   ; or nil 

  (when use-spacemacs
    (setq user-emacs-directory "~/.spacemacs.d/"))   ; defaults to ~/.emacs.d/

  (load (expand-file-name "init" user-emacs-directory))
  ;;(sleep-for 4)
  )

(put 'upcase-region 'disabled nil)

;; LOAD THEME ;;
(load-user-file "theme")

(add-hook 'prog-mode-hook #'unshow-ws)
(add-hook 'sln-mode-hook #'unshow-ws)

;; HACK
(add-hook 'c-mode-hook (lambda() (define-key c-mode-map (kbd "C-c C-c") nil)))
(add-hook 'c++-mode-hook (lambda() (define-key c++-mode-map (kbd "C-c C-c") nil)))
;;(add-hook 'sh-mode '(show-paren-mode nil))

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

