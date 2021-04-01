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

;;;;; TODO ;;;;;
;; Only for EMACS 28, which changed definitions, until packages comply ;;
(when (>= emacs-major-version 28)

(defun make-obsolete (obsolete-name current-name &optional when)
  "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
OBSOLETE-NAME should be a function name or macro name (a symbol).

The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when) "23.1"))
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias (obsolete-name current-name
                                                        &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional access-type) "23.1"))
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)

(defmacro define-obsolete-variable-alias (obsolete-name current-name
						        &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
This uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))
)

;;;;; END TODO ;;;;;

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

;; Macro for graphics dependant code
(defmacro daemon-wrap (name &rest code)
  `(if (daemonp)
       (cl-labels ((,name (frame)
                          (with-selected-frame frame
                            ,@code)
                          (remove-hook 'after-make-frame-functions #',name)))
         (add-hook 'after-make-frame-functions #',name))
     ,@code
     )
  )

;; Based on:
;; 1.
;;(if (daemonp)
;;    (cl-labels ((load-nord (frame)
;;                           (with-selected-frame frame
;;                             (load-theme 'nord t))
;;                           (remove-hook 'after-make-frame-functions #'load-nord)))
;;      (add-hook 'after-make-frame-functions #'load-nord))
;;  (load-theme 'nord t))
;; 2.
;;(if (daemonp)
;;        (add-hook 'after-make-frame-functions 
;;    	      (lambda (frame) 
;;    		(with-selected-frame frame (load-theme 'nord t)))) 
;;      (load-theme 'nord t))

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
         (use-package projectile
           :config
           (setq projectile-indexing-method 'alien))
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
       (when use-exec-path
         (use-package exec-path-from-shell
           :config
           (setq exec-path-from-shell-arguments nil)
           (exec-path-from-shell-initialize))))

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
;;(when (fboundp 'native-compile-async)
;;  (native-compile-async user-init-dir))

;; Server Start
(message "Sockets Dir: %s" server-socket-dir)
(when start-server
  (server-start))

;;(gcmh-mode 1)

