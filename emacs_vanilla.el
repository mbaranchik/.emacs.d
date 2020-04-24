;;; -*- lexical-binding: t -*-
;; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

;; (defconst user-init-dir
;;   (cond ((boundp 'user-emacs-directory)
;;          user-emacs-directory)
;;         ((boundp 'user-init-directory)
;;          user-init-directory)
;;         (t "~/emacs.init/")))


;; enabled by default in emacs >= 24
(require 'package)

;; Any add to list for package-archives (to add marmalade or melpa) goes here
;; (add-to-list 'package-archives 
;;     '("marmalade" .
;;       "http://marmalade-repo.org/packages/"))

(unless (boundp 'aquamacs-version)
  (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/"))
  )

;; TODO
;;(package-initialize)

(load-user-file "filelock.el")
(require 'filelock)

(load-user-file "packages.el")

(load-user-file "config")

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

;;(load-user-file "rsync")

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

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "\*E" msg)
    (progn
      (delete-windows-on buffer)
      (tooltip-show "\n Compilation Successful :-( \n ")
      (save-window-excursion
        (shell-command "say -v Yuri Compilation Successful &" nil)))
    (progn
      (tooltip-show "\n Compilation Failed :-( \n ")
      (save-window-excursion
        (shell-command "say -v Yuri Compilation Failed &" nil))))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  )

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)



