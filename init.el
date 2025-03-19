;;; -*- lexical-binding: t -*-

(defconst user-init-dir (concat user-emacs-directory "lisp.d/"))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load (expand-file-name file user-init-dir))
  (require (intern (concat "my/" file))))

;; Basic stuff
(load-user-file "basic")

;; Bootstrap Straight
(load-user-file "straight-bootstrap")

;;(add-to-list 'load-path (concat user-emacs-directory "lisp.d/"))

;; This is now done with GCMH - Testing
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 200 1024 1024)) ;; 100mb
                             (setq read-process-output-max (* 1024 1024)) ;; 1mb
                             ))

;; Suppress cl warning
(setq byte-compile-warnings '(cl-functions))

(use-package benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;;(straight-use-package `(bench
;;                        :local-repo ,(concat user-emacs-directory "lisp")))

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

;;(load-user-file "filelock.el")
(use-package filelock)

(bench-wrap "Prog"
            (load-user-file "prog"))

(bench-wrap "General"
            (load-user-file "general"))

(bench-wrap "Whitespace"
            (load-user-file "whitespace"))

(bench-wrap "Higlight-Global"
            (load-user-file "highlight-global"))

(bench-wrap "Git"
            (load-user-file "git"))

(bench-wrap "Completion-Next-Gen"
            (load-user-file "completion-next-gen"))

(bench-wrap "Custom"
            (load-user-file "custom"))

(bench-wrap "LSP"
            (load-user-file "lsp"))

(bench-wrap "Keybindings"
            (load-user-file "keybindings"))

(bench-wrap "Compile"
            (use-package compile))

(bench-wrap "Exec-Path"
            (use-package exec-path-from-shell
              :if (memq window-system '(mac ns))
              :config
              (setq exec-path-from-shell-arguments nil)
              (exec-path-from-shell-initialize)))

(put 'upcase-region 'disabled nil)

(bench-wrap "IDE"
            (load-user-file "ide"))

(bench-wrap "Navigation"
    (load-user-file "navigation"))

(bench-wrap "TMUX"
    (load-user-file "tmux"))

(bench-wrap "Elevate"
    (load-user-file "elevate")
    (load-user-file "elevate-setup"))

(bench-wrap "Custom Hooks"
            (add-hook 'prog-mode-hook #'unshow-ws)
            ;; HACK
            ;; (add-hook 'c-mode-hook (lambda() (define-key c-mode-map (kbd "C-c C-c") nil)))
            ;; (add-hook 'c++-mode-hook (lambda() (define-key c++-mode-map (kbd "C-c C-c") nil)))
    )

(bench-wrap "Org"
    (load-user-file "org"))

;; Load Ligatures ;;
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
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
  (ligature-set-ligatures 'prog-mode '("!=" "==" "->" "<-" ">=" "<=" ">>" "<<" ">>=" "<<="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  :custom
  (global-ligature-mode t)
  )

;; LOAD THEME ;;
(bench-wrap "Theme"
            (load-user-file "theme"))

(filelock-with-lock
    (setq custom-file (expand-file-name "local_customized.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))
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
  (shell-command (concat "touch " (shell-quote-argument (expand-file-name (buffer-file-name)))))
  (clear-visited-file-modtime))
;;(add-hook 'after-save-hook 'touch-buffer-file)

;; Server Start
(message "Sockets Dir: %s" server-socket-dir)
(when (config-wrap "server")
  (server-start))

;;(gcmh-mode 1)

