(defconst user-init-dir "~/emacs.init/")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load (expand-file-name file user-init-dir)))

(setq gc-cons-threshold 1073741824
      gc-cons-percentage 0.6)

;;(use-package tabbar-mode)
(let ((file-name-handler-alist nil))
  (load-user-file "emacs_vanilla")
  )

(exec-path-from-shell-initialize)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;;(load-user-file "emacs_vanilla_cust.el")

