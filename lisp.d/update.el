;;(load-file "~/emacs.init/config.el")
;;(load-file "~/emacs.init/packages.el")
;;
;;;; fetch the list of packages available
;;(package-refresh-contents)
;;
;;;; install the missing packages
;;(dolist (package package-list)
;;  (unless (package-installed-p package)
;;    (package-install package)))
;;
;;(package-list-packages)
;;(package-menu-mark-upgrades)
;;(sleep-for 5)
;;(package-menu-execute)

(defconst user-init-dir (concat user-emacs-directory "lisp.d/"))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (require (intern (concat "my/" file)) (expand-file-name file user-init-dir)))

;; Basic stuff
(load-user-file "basic")

(load-user-file "straight-bootstrap")
(straight-pull-recipe-repositories)
(straight-pull-all)
(provide 'my/update)
