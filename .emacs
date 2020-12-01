
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(unless (boundp 'aquamacs-version)
  (setq use-spacemacs nil)   ; or nil 

  (when use-spacemacs
    (setq user-emacs-directory "~/.spacemacs.d/"))   ; defaults to ~/.emacs.d/

  (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
  (load (expand-file-name "init" user-emacs-directory))
  ;;(sleep-for 4)
  )

