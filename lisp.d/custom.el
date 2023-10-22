;;; -*- lexical-binding: t -*-

;; Include basic macros and functions
(defvar my/this nil) (setq my/this (symbol-file 'my/this))
(require 'my/basic (concat (expand-file-name (file-name-directory (or load-file-name (buffer-file-name) my/this))) "basic"))

(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given
directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'execute-extended-command)))

(defun reverse-words (beg end)
    "Reverse the order of words in region."
    (interactive "*r")
    (apply
     'insert
      (reverse
       (split-string
        (delete-and-extract-region beg end) "\\b"))))

(autoload 'shell-toggle "shell-toggle"
  "Toggles between the shell buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)

(defvar hexcolour-keywords
  '(("#[0-9a-fA-F]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background
                                       (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords))

(setq tramp-ssh-controlmaster-options "-o ControlPath=%%C -o ControlMaster=auto -o ControlPersist=no")

;;; Whitespaces BEGIN ;;;

(defun use-tabs ()
  (interactive)
  (set (make-local-variable 'indent-tabs-mode) 1))

(defun use-spaces ()
  (interactive)
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun unshow-ws ()
  (interactive)
  (set (make-local-variable 'whitespace-space) nil)
  (set (make-local-variable 'whitespace-tab) nil)
  (whitespace-mode 0)
  (whitespace-mode 1)
  ;;(set (make-local-variable 'whitespace-style) (quote (face trailing)))
  )

(defun show-ws ()
  (interactive)
  (set (make-local-variable 'whitespace-space) (quote (t (:background "Green"))))
  (set (make-local-variable 'whitespace-tab) (quote (t (:background "Magenta"))))
  (whitespace-mode 0)
  (whitespace-mode 1)
  ;;(set (make-local-variable 'whitespace-style) (quote (face trailing spaces tabs)))
  )

;;; Transparency BEGIN ;;;

(defun transparency-set-initial-value ()
  "Set initial value of alpha parameter for the current frame"
  (interactive)
  (if (equal (frame-parameter nil 'alpha) nil)
      (set-frame-parameter nil 'alpha 100)))

(defun transparency-set-value (numb)
  "Set level of transparency for the current frame"
  (interactive "nEnter transparency level in range 0-100: ")
  (if (> numb 100)
      (message "Error! The maximum value for transparency is 100!")
    (if (< numb 0)
        (message "Error! The minimum value for transparency is 0!")
      (set-frame-parameter nil 'alpha numb))))

(defun transparency-increase ()
  "Increase level of transparency for the current frame"
  (interactive)
  (transparency-set-initial-value)
  (if (> (frame-parameter nil 'alpha) 0)
       (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) -2))
     (message "This is a minimum value of transparency!")))

(defun transparency-decrease ()
  "Decrease level of transparency for the current frame"
  (interactive)
  (transparency-set-initial-value)
  (if (< (frame-parameter nil 'alpha) 100)
      (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) +2))
    (message "This is a minimum value of transparency!")))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; (use-package compile)
;; (add-hook 'verilog-mode-hook
;;           (lambda ()
;;             (unless (file-exists-p "Makefile")
;;               (set (make-local-variable 'compile-command)
;;                    ;; emulate make's .c.o implicit pattern rule, but with
;;                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
;;                    ;; variables:
;;                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
;;                    (let ((file (file-name-nondirectory buffer-file-name)))
;;                      (format "%s -c -o %s.o %s %s %s"
;;                              (or (getenv "CC") "gcc")
;;                              (file-name-sans-extension file)
;;                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
;;                              (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
;;                              file))))))

;;(when (config-wrap "use-flycheck")
;;  (add-hook 'after-init-hook #'global-flycheck-mode)
;;  (eval-after-load "flycheck"
;;    '(progn
;;       (use-package flycheck-cstyle)
;;       (flycheck-cstyle-setup)
;;       ;; chain after cppcheck since this is the last checker in the upstream
;;       ;; configuration
;;       (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle)))))

(when (config-wrap "use-flycheck")
  (setq-default flycheck-disabled-checkers '(verilog-verilator)))

(use-package scratch-pop)

;; Vimish-Fold
(use-package vimish-fold
  :hook
  (prog-mode . vimish-fold-mode)
  :bind
  (
   ("C-M-`" . vimish-fold)
   ("C-M-~" . vimish-fold-delete)
   )
  )

;; TODO: When it supports built-in treesit
;;;; Tree-sitter fold
;;(use-package ts-fold
;;  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
;;  )
(use-package hs
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (
   ("C-`" . hs-toggle-hiding)
   ))

;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
    "//\n"
    "// " (config-wrap "auto-insert-copyright") "\n"
    "//\n\n"
    "//\n"
    "// Author: " (config-wrap "auto-insert-name") "\n"
    "//\n"
    "// Date: " (format-time-string "%Y-%m-%d")"\n"
    "//\n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat (upcase nopath) "_H")))
      (concat "#ifndef " ident "\n"
              "#define " ident  "\n\n"
              "\n\n#endif // " ident "\n"))
    (make-string 70 ?/) "\n"
    "//\n"
    ))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
    "//\n"
    "// " (config-wrap "auto-insert-copyright") "\n"
    "//\n\n"
    "//\n"
    "// Author: " (config-wrap "auto-insert-name") "\n"
    "//\n"
    "// Date: " (format-time-string "%Y-%m-%d")"\n"
    "//\n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (file-name-sans-extension buffer-file-name))
           (nopath (file-name-nondirectory noext))
           (ident-hdr (concat nopath ".h"))
           (ident nopath))
      ;;(if (file-exists-p ident)
      (concat "#include \"" ident-hdr "\"\n\n"
              ident "::" ident "(const sc_core::sc_module_name& name) : sc_core::sc_module(name), m_log(" (upcase ident) "_TAG)\n"
              "{}\n\n"
              ident "::~" ident "()\n"
              "{}\n"
              "\n\n"));;)
    (make-string 70 ?/) "\n"
    "//\n"
    ))

(defun insert-sc-module()
  (interactive)
  (let* ((noext (file-name-sans-extension buffer-file-name))
         (ident (file-name-nondirectory noext)))
    (insert (concat "#include \"al_report.h\"\n"
                    "#include \"systemc.h\"\n"
                    "#include \"tlm.h\"\n"
                    "#include \"tlm_utils/simple_initiator_socket.h\"\n"
                    "#include \"tlm_utils/simple_target_socket.h\"\n"
                    "\n"
                    "#define " (upcase ident) "_TAG std::string(this->name())\n"
                    "\n"
                    "typedef tlm_utils::simple_target_socket<class " ident "> " ident "_t;\n"
                    "typedef tlm_utils::simple_initiator_socket<class " ident "> " ident "_i;\n"
                    "\n"
                    "class " ident " : public sc_core::sc_module\n"
                    "{\n"
                    " public:\n"
                    "    " ident "(const sc_core::sc_module_name& name);\n"
                    "    virtual ~" ident "();\n"
                    " protected:\n"
                    "    al_report m_log;\n"
                    "};"))
    )
  )

;; (interactive "nEnter transparency level in range 0-100: ")


(defun clear-ws()
  (interactive)
  (picture-mode)
  (picture-mode-exit)
  (save-buffer)
  )

(setq cc-other-file-alist
      '(("\\.c"   (".h"))
       ("\\.cpp"   (".h"))
       ("\\.h"   (".c"".cpp"))))

(setq cc-search-directories
      '("." "../src" "../include" "../source"))

(defun gen-dir-locals ()
  (interactive)
  (let* ((my_dir (projectile-project-root)))
    (when (not my_dir)
      (setq my_dir (locate-dominating-file default-directory ".git")))
    (when (not my_dir)
      (setq my_dir (read-directory-name "Base Directory: ")))
    (let ((choice '("Auto" "C" "C++")))
      (setq c_mode (ido-completing-read "Choose Language: " choice))
      )
    (let ((choice '("Tabs" "Spaces")))
      (setq tab_mode (ido-completing-read "Choose Whitespaces: " choice))
      )
    (message "Running %s/%s-dir-locals.sh in %s" user-emacs-directory "cpp" my_dir)
    (save-window-excursion
      (shell-command (format "cd %s; %s/gen-dir-locals.sh .dir-locals.el %s %s" my_dir user-emacs-directory tab_mode c_mode) nil)
      )
    (message "Done!")
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
(provide 'my/custom)
