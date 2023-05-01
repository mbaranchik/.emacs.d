;;; -*- lexical-binding: t -*-

(defconst user-init-dir (concat user-emacs-directory "lisp.d/"))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load (expand-file-name file user-init-dir)))

;; Bootstrap Straight
(load-user-file "straight-bootstrap.el")

;;(add-to-list 'load-path (concat user-emacs-directory "lisp.d/"))

;; This is now done with GCMH - Testing
(add-hook 'after-init-hook (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Suppress cl warning
(setq byte-compile-warnings '(cl-functions))

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

(bench "Completion-Next-Gen"
       (load-user-file "completion-next-gen"))

(bench "Projectile"
       (when use-projectile
         (use-package projectile
           :config
           (setq projectile-indexing-method 'alien))
         (projectile-mode t)))

(bench "Tags"
       (when use-tags
         (load-user-file "tags")))

(bench "Custom"
       (load-user-file "custom"))

(bench "Clang"
       (load-user-file "clang")
       (when use-clang
         (add-hook 'c-mode-hook 'clang-format+-mode)
         (add-hook 'c++-mode-hook 'clang-format+-mode)
         ))

(bench "LSP"
       (load-user-file "lsp"))

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
         (use-package exec-path-from-shell
           :if (memq window-system '(mac ns))
           :config
           (setq exec-path-from-shell-arguments nil)
           (exec-path-from-shell-initialize)))

(put 'upcase-region 'disabled nil)

(load-user-file "ide")

;; LOAD THEME ;;
(bench "Theme"
       (load-user-file "theme"))

(bench "Custom Hooks"
       (add-hook 'prog-mode-hook #'unshow-ws)
       ;; HACK
       (add-hook 'c-mode-hook (lambda() (define-key c-mode-map (kbd "C-c C-c") nil)))
       (add-hook 'c++-mode-hook (lambda() (define-key c++-mode-map (kbd "C-c C-c") nil))))

;; Load Ligatures ;;
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
   ;; Enable traditional ligature support in eww-mode, if the
   ;; `variable-pitch' face supports it
   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
   ;; Enable all Cascadia Code ligatures in programming modes
   ;; (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
   ;;                                      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
   ;;                                      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
   ;;                                      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
   ;;                                      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
   ;;                                      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
   ;;                                      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
   ;;                                      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
   ;;                                      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
   ;;                                      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
   ;;                                      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
   ;;                                      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
   ;;                                      "\\\\" "://"))
   (ligature-set-ligatures 'prog-mode '("<!--" "||="
                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "#_(" "..<"
                                        "..." "+++" "/==" "///" "_|_" "&&" "^=" "~~" "~@" "~="
                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                        "://"))
   ;; Enables ligature checks globally in all buffers. You can also do it
   ;; per mode with `ligature-mode'.
   :custom
   (global-ligature-mode t)
   )

(filelock-with-lock
 (setq custom-file (expand-file-name "local_customized.el" user-emacs-directory))
 (load custom-file)
 )

;; Avoid locks on recentf
(when (boundp 'server-name)
  (setq recentf-save-file (format "/tmp/recentf.%s" server-name)))

;;(byte-recompile-directory user-init-dir 0)
;;(when (fboundp 'native-compile-async)
;;  (native-compile-async user-init-dir))

;; Disable TRAMP ssh options
(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
;; Disable VC when in TRAMP buffer
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Touch hook after save - Trigger unison better
(defun touch-buffer-file ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))
(add-hook 'after-save-hook 'touch-buffer-file)

;; Server Start
(message "Sockets Dir: %s" server-socket-dir)
(when start-server
  (server-start))

;;(gcmh-mode 1)

