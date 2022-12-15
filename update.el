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

(load-file "~/emacs.init/straight-bootstrap.el")
(straight-pull-recipe-repositories)
(straight-pull-all)
