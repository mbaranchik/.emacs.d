;;; -*- lexical-binding: t -*-

(require 'json)

;; Benchmark macro
(defmacro bench-wrap (name &rest code)
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

;; Function to load and cache configuration
(defvar my/config-cache nil)
(defvar my/config-file (expand-file-name "config.json" user-emacs-directory))

(defvar my/enable-debug-prints nil)

(defun my/debug-print (format-string &rest args)
    (when my/enable-debug-prints
        (apply #'message (concat "[CONFIG-DEBUG] " format-string) args)))

(defun my/convert-json-bool (value)
  "Convert JSON boolean values to proper Lisp boolean values"
  (cond ((eq value t) t)
        ((eq value :false) nil)
        (t value)))

(defun my/load-config ()
    "Load configuration from JSON file"
    (my/debug-print "Loading config from: %s" my/config-file)
    (unless my/config-cache
        (if (file-exists-p my/config-file)
            (with-temp-buffer
                (insert-file-contents my/config-file)
                (let ((json-contents (buffer-string)))
                  (my/debug-print "Config file contents: %s" json-contents)
                  ;; Validate JSON structure
                  (unless (string-match-p "\"ui\"\\s-*:" json-contents)
                    (error "Config file missing required 'ui' section"))
                  )
                (setq my/config-cache (json-parse-buffer :object-type 'alist)))
            (error "Configuration file %s not found" my/config-file)))
    my/config-cache)

(defun my/get-config-value (path &optional default)
    "Get configuration value at PATH with optional DEFAULT.
PATH is a list of keys to traverse the JSON structure."
    (let* ((config (my/load-config))
              (value nil)
              (path-str (mapconcat #'identity path "/")))
        (while (and path config)
            (let ((key (if (stringp (car path))
                          (intern (car path))
                        (car path))))
              (setq value (alist-get key config nil nil #'equal))
              (setq config value))
            (setq path (cdr path)))
        (my/debug-print "Getting config value for path: %s -> %S" path-str value)
        (my/convert-json-bool (or value default))))

;; Macro for getting config value using dot notation
(defmacro config-wrap (name)
    `(let* ((path (split-string ,name "/"))
               (result (my/get-config-value path)))
         (my/debug-print "config-wrap: %s -> %S" ,name result)
         result)
    )

;; Helper function to get enabled modes from a feature section
(defun get-enabled-modes (feature)
    "Get list of enabled modes for a given feature (lsp, code-diag, autoformat)"
    (let ((modes '())
             (config (my/get-config-value (list feature)))
             (feature-name feature))
        (my/debug-print "Checking enabled modes for feature '%s'" feature-name)
        (my/debug-print "Feature '%s' config = %S" feature-name config)
        (dolist (mode '("c" "cpp" "python" "bash"))
            (let* ((mode-config (alist-get (intern mode) config))
                      (enabled (and mode-config (my/convert-json-bool (alist-get 'enable mode-config)))))
                (my/debug-print "Feature '%s' mode '%s': config=%S, enabled=%S" feature-name mode mode-config enabled)
                (when (eq enabled t)  ; Only consider explicitly set to t as enabled
                    (push mode modes))))
        (my/debug-print "Feature '%s' enabled modes = %S" feature-name modes)
        modes))


;; Customs - Vars
(setq-default server-socket-dir (concat user-emacs-directory "server-sock"))
(setq-default inhibit-startup-screen t)
(setq-default semantic-mode nil)
(setq-default send-mail-function nil)


(setq-default default-frame-alist
    (quote
        ((cursor-type . (bar . 3))
            (internal-border-width . 0)
            (modeline . t)
            (fringe)
            (cursor-color . "Red")
            (tool-bar-lines . 1)
            (fontsize . 0)
            (font-backend mac-ct ns))))
(setq-default whitespace-style (quote (face trailing spaces tabs)))

(defalias 'yes-or-no #'y-or-n-p)
(setopt use-short-answers t)
(setq confirm-kill-emacs #'yes-or-no-p)

(daemon-wrap my/confirm-client-exit
    (define-advice save-buffers-kill-terminal (:around (oldfun &rest args) my/delete-client)
        "Confirm deleting the client."
        (interactive)
        (when (y-or-n-p "Confirm exit ? ")
            (apply oldfun args))))

(provide 'my/basic)
