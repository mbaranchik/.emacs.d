;;; -*- lexical-binding: t -*-

(require 'json)
(require 'shell)
(require 'markdown-mode)

;;;;;;;;;;;;;;;;;;
;; Custom Group ;;
;;;;;;;;;;;;;;;;;;
(defgroup elevate nil
  "Customization group for elevate."
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;
;; Model Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defcustom elevate-models
  '(("AmazonNovaPro" . "us.amazon.nova-pro-v1:0")
    ("Sonnet3.5v2" . "us.anthropic.claude-3-5-sonnet-20241022-v2:0")
    ("Haiku3.5" . "us.anthropic.claude-3-5-haiku-20241022-v1:0")
    ("Sonnet3.7Think" . "us.anthropic.claude-3-7-sonnet-20250219-v1:0"))
  "Available LLM models and their IDs."
  :type '(alist :key-type string :value-type string)
  :group 'elevate)

(defcustom elevate-chat-default-model "Sonnet3.5v2"
  "Default model for chat and explanation."
  :type `(choice ,@(mapcar (lambda (m) `(const ,(car m))) elevate-models))
  :group 'elevate)

(defcustom elevate-completion-default-model "Haiku3.5"
  "Default model for code completion."
  :type `(choice ,@(mapcar (lambda (m) `(const ,(car m))) elevate-models))
  :group 'elevate)

;; For code completion
(defcustom elevate-completion-temperature 0.2
  "Temperature for code completion.
Lower values make output more focused and deterministic."
  :type 'number
  :group 'elevate)

(defcustom elevate-completion-top-p 0.9
  "Top-P (nucleus sampling) for code completion."
  :type 'number
  :group 'elevate)

(defcustom elevate-completion-top-k 10
  "Top-K for code completion.
Lower value means more focused on most likely tokens."
  :type 'integer
  :group 'elevate)

;; For chat/explanation
(defcustom elevate-chat-temperature 0.7
  "Temperature for chat and explanation.
Higher values allow more creativity."
  :type 'number
  :group 'elevate)

(defcustom elevate-chat-top-p 0.95
  "Top-P (nucleus sampling) for chat and explanation."
  :type 'number
  :group 'elevate)

(defcustom elevate-chat-top-k 250
  "Top-K for chat and explanation."
  :type 'integer
  :group 'elevate)

(defvar-local elevate-current-chat-model nil
  "Currently selected model for chat/explanation in this buffer.")

(defvar-local elevate-current-completion-model nil
  "Currently selected model for completion in this buffer.")

(defun elevate-get-model-id (model-name)
  "Get the model ID for MODEL-NAME."
  (cdr (assoc model-name elevate-models)))

(defun elevate-switch-chat-model ()
  "Switch the model used for chat and explanation."
  (interactive)
  (let ((chosen (completing-read "Choose model for chat/explanation: "
                               (mapcar #'car elevate-models)
                               nil t)))
    (setq elevate-current-chat-model chosen)
    (message "Chat model switched to %s" chosen)))

(defun elevate-switch-completion-model ()
  "Switch the model used for code completion."
  (interactive)
  (let ((chosen (completing-read "Choose model for completion: "
                               (mapcar #'car elevate-models)
                               nil t)))
    (setq elevate-current-completion-model chosen)
    (message "Completion model switched to %s" chosen)))

(defun elevate-get-inference-params (model-name &optional is-completion)
  "Get the model inference parameters as a JSON object, formatted for MODEL-NAME.
If IS-COMPLETION is non-nil, use completion-specific parameters."
  (let ((temp (if is-completion
                  elevate-completion-temperature
                elevate-chat-temperature))
        (top-p (if is-completion
                   elevate-completion-top-p
                 elevate-chat-top-p))
        (top-k (if is-completion
                   elevate-completion-top-k
                 elevate-chat-top-k)))
      (if (string= model-name "AmazonNovaPro")
          ;; Nova Pro format
          `(("temperature" . ,temp)
               ("topP" . ,top-p)
               ("topK" . ,top-k))
          ;; Claude format
          `(("temperature" . ,temp)
               ("top_p" . ,top-p)
               ("top_k" . ,top-k)))))

;;;;;;;;;;;;;;;;;;;;;
;; Context Support ;;
;;;;;;;;;;;;;;;;;;;;;
(defvar elevate-chat-contexts (make-hash-table :test 'equal)
  "Hash table storing chat contexts by their names.")

(defvar elevate-current-context nil
  "Current chat context name being used.")

(defun elevate-get-context-buffer-name (context-name)
  "Get the buffer name for CONTEXT-NAME."
  (format "*LLM Chat: %s*" context-name))

(defun elevate-switch-to-context-buffer (context-name)
  "Switch to the buffer for CONTEXT-NAME."
  (let ((buf-name (elevate-get-context-buffer-name context-name)))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p 'markdown-mode)
        (markdown-mode))
        (display-buffer (current-buffer)))))

(defcustom elevate-contexts-directory
  (expand-file-name ".llm_contexts" user-emacs-directory)
  "Directory where LLM conversation contexts are stored."
  :type 'directory
  :group 'elevate)

(defun elevate-ensure-contexts-directory ()
  "Ensure the contexts directory exists."
  (unless (file-directory-p elevate-contexts-directory)
    (make-directory elevate-contexts-directory t)))

(defun elevate-context-file (context-name)
  "Get the file path for a given CONTEXT-NAME."
  (expand-file-name (concat (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" context-name) ".json")
                    elevate-contexts-directory))

(defun elevate-save-context (context-name)
  "Save the specified context to disk."
  (when-let ((history (gethash context-name elevate-chat-contexts)))
    (elevate-ensure-contexts-directory)
    (with-temp-file (elevate-context-file context-name)
      (insert (json-encode history)))))

(defun elevate-load-context (context-name)
  "Load a context from disk if it exists and isn't already loaded."
  (unless (gethash context-name elevate-chat-contexts)
    (let ((file (elevate-context-file context-name)))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (condition-case nil
              (let ((history (json-parse-string
                            (buffer-string)
                            :object-type 'alist
                            :array-type 'list)))
                (puthash context-name history elevate-chat-contexts)
                (message "Loaded context %s with %d messages"
                        context-name (length history)))
            (error
             (message "Error loading context %s" context-name)
             nil)))))))

(defun elevate-list-saved-contexts ()
  "Get a list of all saved context names."
  (elevate-ensure-contexts-directory)
  (mapcar (lambda (file)
            (string-remove-suffix ".json"
                                (file-name-nondirectory file)))
          (directory-files elevate-contexts-directory t "\\.json$")))

(defun elevate-create-context (context-name)
  "Create a new chat context with CONTEXT-NAME."
  (interactive "sEnter new context name: ")
  (if (gethash context-name elevate-chat-contexts)
      (message "Context '%s' already exists" context-name)
    (unless elevate-chat-contexts
      (setq elevate-chat-contexts (make-hash-table :test 'equal)))
    (puthash context-name '() elevate-chat-contexts)
    (setq elevate-current-context context-name)
    (elevate-switch-to-context-buffer context-name)))

(defun elevate-display-context-history (context-name)
  "Display the history of CONTEXT-NAME in its buffer."
  (when-let ((history (gethash context-name elevate-chat-contexts)))
    (with-current-buffer (get-buffer-create (elevate-get-context-buffer-name context-name))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (markdown-mode)
        (dolist (entry (seq-partition history 2))
          (let ((human-msg (alist-get 'text (car entry)))
                (assistant-msg (alist-get 'text (cadr entry))))
            (insert (format "### Question\n\n%s\n\n### Answer\n\n%s\n\n---\n\n"
                          human-msg assistant-msg))))))))

(defun elevate-switch-to-context-buffer (context-name)
  "Switch to the buffer for CONTEXT-NAME."
  (let ((buf-name (elevate-get-context-buffer-name context-name)))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p 'markdown-mode)
        (markdown-mode))
      ;; Display existing history
      (elevate-display-context-history context-name)
      (display-buffer (current-buffer)))))

(defun elevate-debug-context (context-name)
  "Print debug information about a context's history."
  (interactive "sContext name: ")
  (let ((history (gethash context-name elevate-chat-contexts)))
    (message "Context %s history:\n%S" context-name history)))

(defun elevate-switch-context ()
  "Switch to a different chat context."
  (interactive)
  (unless elevate-chat-contexts
    (setq elevate-chat-contexts (make-hash-table :test 'equal)))
  (let* ((available-contexts (delete-dups
                             (append (hash-table-keys elevate-chat-contexts)
                                   (elevate-list-saved-contexts))))
         (chosen (completing-read "Choose context: " available-contexts nil t))
         (current-context-buffer (and elevate-current-context
                                     (get-buffer (elevate-get-context-buffer-name elevate-current-context)))))

    ;; If switching to the same context, just display the buffer without reloading
    (if (and elevate-current-context (string= chosen elevate-current-context))
        (when current-context-buffer
          (display-buffer current-context-buffer)
          (message "Already in context '%s'" chosen))

      ;; Otherwise, load the new context
      (unless (gethash chosen elevate-chat-contexts)
        (elevate-load-context chosen))

      (setq elevate-current-context chosen)
      (elevate-switch-to-context-buffer chosen)
      (message "Switched to context '%s'" chosen))))

(defun elevate-remove-context ()
  "Remove a chat context."
  (interactive)
  (unless elevate-chat-contexts
    (setq elevate-chat-contexts (make-hash-table :test 'equal)))
  (let* ((available-contexts (delete-dups
                             (append (hash-table-keys elevate-chat-contexts)
                                   (elevate-list-saved-contexts))))
         (chosen (completing-read "Remove context: " available-contexts nil t)))
    (when (equal chosen elevate-current-context)
      (setq elevate-current-context nil))
    (remhash chosen elevate-chat-contexts)
    (let ((context-file (elevate-context-file chosen)))
      (when (file-exists-p context-file)
        (delete-file context-file)))
    (let ((buf (get-buffer (elevate-get-context-buffer-name chosen))))
      (when buf
        (kill-buffer buf)))
    (message "Context '%s' removed" chosen)))

(defun elevate-list-contexts ()
  "List all available chat contexts."
  (interactive)
  (unless elevate-chat-contexts
    (setq elevate-chat-contexts (make-hash-table :test 'equal)))
  (let ((contexts (delete-dups
                   (append (hash-table-keys elevate-chat-contexts)
                          (elevate-list-saved-contexts)))))
    (if contexts
        (with-current-buffer (get-buffer-create "*LLM Contexts*")
          (erase-buffer)
          (markdown-mode)
          (insert "# Available Chat Contexts\n\n")
          (dolist (ctx contexts)
            (insert (format "- %s%s%s\n"
                          ctx
                          (if (equal ctx elevate-current-context)
                              " (current)"
                            "")
                          (if (gethash ctx elevate-chat-contexts)
                              " (loaded)"
                            " (on disk)"))))
          (display-buffer (current-buffer)))
      (message "No chat contexts available"))))

;;;;;;;;;;;;;;;;;;;;;
;; Connection Info ;;
;;;;;;;;;;;;;;;;;;;;;
(defcustom elevate-cookie-file (expand-file-name "~/.midway/cookie")
  "Path to the cookie file for LLM requests."
  :type 'string
  :group 'elevate)

(defcustom elevate-api-endpoint "https://api.chat.com.amazon.dev"
  "API endpoint for the LLM service."
  :type 'string
  :group 'elevate)


;;;;;;;;;;;;;;;;;;;;;
;; Query Functions ;;
;;;;;;;;;;;;;;;;;;;;;
(defun elevate-decode-response (response)
  "Decode special characters in the LLM response string."
  (when response
    (condition-case nil
        (with-temp-buffer
          (insert response)
          (goto-char (point-min))
          ;; Handle newlines
          (while (search-forward "\\n" nil t)
            (replace-match "\n" nil t))
          ;; Handle tabs
          (goto-char (point-min))
          (while (search-forward "\\t" nil t)
            (replace-match "\t" nil t))
          ;; Handle quotes
          (goto-char (point-min))
          (while (search-forward "\\\"" nil t)
            (replace-match "\"" nil t))
          ;; Handle backslashes
          (goto-char (point-min))
          (while (search-forward "\\\\" nil t)
            (replace-match "\\" nil t))
          ;; Handle carriage returns
          (goto-char (point-min))
          (while (search-forward "\\r" nil t)
            (replace-match "" nil t))
          (buffer-string))
      (error response))))

(defun elevate-format-message (sender text)
  "Format a chat message according to the expected format."
  `(("sender" . ,sender)
       ("text" . ,text)))

(defun elevate-with-temp-file (prefix content)
  "Create a temporary file with PREFIX and CONTENT, return path."
  (let ((temp-file (make-temp-file prefix)))
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun elevate-chat-query-async (prompt context-name callback)
    "Send a query with chat history from CONTEXT-NAME to the LLM."
    ;; Ensure context is loaded
    (unless (gethash context-name elevate-chat-contexts)
        (elevate-load-context context-name))
    (let* ((chat-history (or (gethash context-name elevate-chat-contexts) '()))
              (model-name (or elevate-current-chat-model
                              elevate-chat-default-model))
              (json-data (json-encode
                             `(("prompt" . ,prompt)
                               ("chatHistory" . ,(vconcat chat-history))
                               ("model" . ,(elevate-get-model-id model-name))
                               ("modelInferenceParams" . ,(elevate-get-inference-params model-name nil)))))
              (request-file (elevate-with-temp-file "llm-request-" json-data))
              (response-file (make-temp-file "llm-response-"))
              (curl-command (format "curl -L --cookie %s --cookie-jar %s '%s' --data-binary @%s -o %s"
                                elevate-cookie-file
                                elevate-cookie-file
                                elevate-api-endpoint
                                request-file
                                response-file))
              (buf-name (generate-new-buffer-name " *elevate-curl*"))
              (buf (get-buffer-create buf-name))
              (proc (get-buffer-process buf)))
        (message "Sending request to LLM...")
        (when proc
            (delete-process proc))
        (with-current-buffer buf
            (erase-buffer))
        (make-process
            :name (concat "elevate-curl-" (number-to-string (random)))
            :buffer buf
            :command (list "sh" "-c" curl-command)
            :sentinel (lambda (proc event)
                          (when (string= event "finished\n")
                                      (let ((json-response (with-temp-buffer
                                                               (insert-file-contents response-file)
                                                               (buffer-string))))
                                          (let* ((json-parsed-response
                                                     (json-parse-string json-response :object-type 'alist))
                                                    (response (alist-get 'completion
                                                                  json-parsed-response nil nil #'equal)))
                                      (progn
                                          (when response
                                              (setq chat-history
                                                  (append chat-history
                                                      (list (elevate-format-message "Human" prompt)
                                                          (elevate-format-message "Assistant" response))))
                                              (puthash context-name chat-history elevate-chat-contexts)
                                              ;; Save context after updating
                                              (elevate-save-context context-name))
                                          (funcall callback (elevate-decode-response response)))))
                              ;; Clean up
                              (ignore-errors
                                  (delete-file request-file)
                                  (delete-file response-file)
                                  )
                              (kill-buffer (process-buffer proc)))))))

(defun elevate-query-async (prompt callback complete &optional system)
    "Send a context-less query to the LLM."
    (let* ((model-name (if complete
                           (or elevate-current-completion-model
                               elevate-completion-default-model)
                           (or elevate-current-chat-model
                               elevate-completion-default-model)))
              (chat-history (list (elevate-format-message "Human" (or system ""))))
              (json-data (json-encode
                             `(("prompt" . ,prompt)
                               ("chatHistory" . ,chat-history)
                               ("model" . ,(elevate-get-model-id model-name))
                               ("modelInferenceParams" . ,(elevate-get-inference-params model-name t)))))
              (request-file (elevate-with-temp-file "llm-request-" json-data))
              (response-file (make-temp-file "llm-response-"))
              (curl-command (format "curl -L --cookie %s --cookie-jar %s '%s' --data-binary @%s -o %s"
                                elevate-cookie-file
                                elevate-cookie-file
                                elevate-api-endpoint
                                request-file
                                response-file))
              (buf-name (generate-new-buffer-name " *elevate-curl*"))
              (buf (get-buffer-create buf-name))
              (proc (get-buffer-process buf)))
        (message "Sending request to LLM...")
        ;; (message "CMD: '%S'" json-data)
        (when proc
            (delete-process proc))
        (with-current-buffer buf
            (erase-buffer))
        (make-process
            :name (concat "elevate-curl-" (number-to-string (random)))
            :buffer buf
            :command (list "sh" "-c" curl-command)
            :sentinel (lambda (proc event)
                          (when (string= event "finished\n")
                                  (let ((json-response (with-temp-buffer
                                                           (insert-file-contents response-file)
                                                           (buffer-string))))
                                      (let* ((json-parsed-response
                                                 (json-parse-string json-response :object-type 'alist))
                                                (response (alist-get 'completion
                                                              json-parsed-response nil nil #'equal)))
                                          (funcall callback (elevate-decode-response response))))
                              ;; Clean up
                              (ignore-errors
                                  (delete-file request-file)
                                  (delete-file response-file))
                              (kill-buffer (process-buffer proc)))))))

;;;;;;;;;;;;;;;;;;;;;
;; UX/UI Utilities ;;
;;;;;;;;;;;;;;;;;;;;;
(defvar elevate-language-map
  '((emacs-lisp-mode . "elisp")
    (lisp-interaction-mode . "elisp")
    (python-mode . "python")
    (python-ts-mode . "python")
    (java-mode . "java")
    (java-ts-mode . "java")
    (js-mode . "javascript")
    (js2-mode . "javascript")
    (javascript-mode . "javascript")
    (typescript-mode . "typescript")
    (typescript-ts-mode . "typescript")
    (ruby-mode . "ruby")
    (ruby-ts-mode . "ruby")
    (c-mode . "c")
    (c++-mode . "cpp")
    (c++-ts-mode . "cpp")
    (rust-mode . "rust")
    (rust-ts-mode . "rust")
    (go-mode . "go")
    (go-ts-mode . "go")
    (php-mode . "php")
    (swift-mode . "swift")
    (kotlin-mode . "kotlin")
    (scala-mode . "scala")
    (clojure-mode . "clojure")
    (csharp-mode . "csharp")
    (css-mode . "css")
    (scss-mode . "scss")
    (less-css-mode . "less")
    (html-mode . "html")
    (web-mode . "html")
    (sh-mode . "bash")
    (shell-mode . "bash")
    (markdown-mode . "markdown")
    (sql-mode . "sql")
    (yaml-mode . "yaml")
    (json-mode . "json")
    (terraform-mode . "hcl")
    (dockerfile-mode . "dockerfile"))
  "Map Emacs major modes to standard language names.")

(defun elevate-detect-language ()
  "Detect the programming language of the current buffer."
  (or (alist-get major-mode elevate-language-map)
      (replace-regexp-in-string
       "-mode$" ""
       (replace-regexp-in-string "-ts-mode$" "" (symbol-name major-mode)))))

(defun elevate-create-markdown-buffer (name)
  "Create or get a markdown buffer with NAME."
  (with-current-buffer (get-buffer-create name)
    (unless (derived-mode-p 'markdown-mode)
      (markdown-mode))
    (current-buffer)))

(defun elevate-chat ()
  "Start or continue a chat with the LLM in the current context."
  (interactive)
  (unless elevate-current-context
    (call-interactively #'elevate-create-context))
  (let ((prompt (read-string "Ask LLM: ")))
    (elevate-chat-query-async
     prompt
     elevate-current-context
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create
                                (elevate-get-context-buffer-name
                                 elevate-current-context))
             (goto-char (point-max))
             (insert (format "### Question\n\n%s\n\n### Answer\n\n%s\n\n---\n\n"
                           prompt response))
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun elevate-chat-no-context ()
  "Send single LLM query without context"
  (interactive)
  (let ((prompt (read-string "Ask LLM: ")))
    (elevate-query-async
     prompt
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create
                                (elevate-get-context-buffer-name "Elevate"))
             (goto-char (point-max))
             (insert (format "### Question\n\n%s\n\n### Answer\n\n%s\n\n---\n\n"
                           prompt response))
             (display-buffer (current-buffer)))
           (message "Error: No valid response received from the LLM.")))
        nil)))

(defun elevate-explain-code-with-context ()
  "Explain code with context awareness."
  (interactive)
  (unless elevate-current-context
    (call-interactively #'elevate-create-context))
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
         (lang-name (elevate-detect-language))
         (prompt (format "Explain the following code:\n\n%s" code)))
    (elevate-chat-query-async
     prompt
     elevate-current-context
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create
                                (elevate-get-context-buffer-name
                                 elevate-current-context))
             (goto-char (point-max))
             (insert "# Code Explanation\n\n")
             ;; (insert "## Original Code\n\n```")
             ;; (insert lang-name)
             ;; (insert "\n")
             ;; (insert code)
             ;; (insert "\n```\n\n")
             ;; (insert "## Explanation\n\n")
             (insert response)
             (insert "\n\n---\n\n")
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun elevate-query-code-with-context ()
  "Explain code with context awareness."
  (interactive)
  (unless elevate-current-context
    (call-interactively #'elevate-create-context))
  (let* ((user-prompt (read-string "Ask LLM: "))
         (code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
         (lang-name (elevate-detect-language))
         (prompt (format "Answer the following while using content in CODE XML tag as context:\n\n%s\n\n<CODE>%s</CODE>" user-prompt code)))
    (elevate-chat-query-async
     prompt
     elevate-current-context
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create
                                (elevate-get-context-buffer-name
                                 elevate-current-context))
             (goto-char (point-max))
             (insert "# Code Explanation\n\n")
             ;; (insert "## Original Code\n\n```")
             ;; (insert lang-name)
             ;; (insert "\n")
             ;; (insert code)
             ;; (insert "\n```\n\n")
             ;; (insert "## Explanation\n\n")
             (insert response)
             (insert "\n\n---\n\n")
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun elevate-get-code-context ()
  "Get the context for code completion."
  (let* ((line-start (line-beginning-position))
         (context-start (max (- line-start 2048) (point-min)))
         (context (buffer-substring-no-properties context-start (point))))
    context))

(defvar-local elevate-current-overlay nil
  "Overlay for the current completion suggestion.")

(defun elevate-show-inline-completion (completion)
  "Show COMPLETION inline at point as a greyed-out suggestion."
  (when elevate-current-overlay
    (delete-overlay elevate-current-overlay))
  (when completion
    (let ((ov (make-overlay (point) (point))))
      (setq elevate-current-overlay ov)
      (overlay-put ov 'after-string
                  (propertize completion
                             'face '(:foreground "gray50")
                             'completion-text completion))
      ;; Store the point position
      (overlay-put ov 'completion-point (point)))))

(defun elevate-accept-completion ()
  "Accept the current completion suggestion."
  (interactive)
  (when elevate-current-overlay
    (let* ((completion (get-text-property
                       0 'completion-text
                       (overlay-get elevate-current-overlay 'after-string)))
           (pos (overlay-get elevate-current-overlay 'completion-point)))
      ;; Remove the overlay first
      (delete-overlay elevate-current-overlay)
      (setq elevate-current-overlay nil)
      ;; Then insert the completion at the stored position
      (save-excursion
        (goto-char pos)
        (insert completion))
      ;; Move point to end of inserted completion
      (goto-char (+ pos (length completion))))
    t))

(defun elevate-cancel-completion ()
  "Cancel the current completion suggestion."
  (interactive)
  (when elevate-current-overlay
    (delete-overlay elevate-current-overlay)
    (setq elevate-current-overlay nil)))

(defun elevate-smart-tab ()
  "Accept completion if available, otherwise do normal TAB action."
  (interactive)
  (unless (elevate-accept-completion)
    (indent-for-tab-command)))

(defun elevate-maybe-cancel-completion ()
  "Cancel completion if the next command isn't a completion-related command."
  (unless (memq this-command '(elevate-smart-tab
                              elevate-accept-completion))
    (setq elevate-completion-cancelled t)
    (elevate-cancel-completion)
    (elevate-stop-loading-animation)))

(defvar-local elevate-loading-overlay nil
  "Overlay for the loading animation.")

(defvar-local elevate-loading-timer nil
  "Timer for the loading animation.")

(defvar-local elevate-completion-cancelled nil
  "Flag to indicate if completion was cancelled while waiting.")

(defvar elevate-loading-frames '("." ".." "...")
  "Frames for the loading animation.")

(defun elevate-update-loading-animation ()
  "Update the loading animation frame."
  (when (and elevate-loading-overlay
             (overlay-buffer elevate-loading-overlay))
    (let* ((frames elevate-loading-frames)
           (current (overlay-get elevate-loading-overlay 'frame-index))
           (next (if (>= current (1- (length frames))) 0 (1+ current)))
           (frame (nth next frames)))
      (overlay-put elevate-loading-overlay 'frame-index next)
      (overlay-put elevate-loading-overlay 'after-string
                   (propertize frame 'face '(:foreground "gray50"))))))

(defun elevate-start-loading-animation ()
  "Start the loading animation at point."
  (elevate-stop-loading-animation) ; Clean up any existing animation
  (setq elevate-completion-cancelled nil)
  (let ((ov (make-overlay (point) (point))))
    (setq elevate-loading-overlay ov)
    (overlay-put ov 'frame-index 0)
    (overlay-put ov 'after-string
                 (propertize "." 'face '(:foreground "gray50")))
    (setq elevate-loading-timer
          (run-with-timer 0 0.3 #'elevate-update-loading-animation))))

(defun elevate-stop-loading-animation ()
  "Stop the loading animation and clean up."
  (when elevate-loading-timer
    (cancel-timer elevate-loading-timer)
    (setq elevate-loading-timer nil))
  (when elevate-loading-overlay
    (delete-overlay elevate-loading-overlay)
    (setq elevate-loading-overlay nil)))

(defcustom elevate-complete-system-prompt "You are an advanced code completion assistant embedded in an IDE.
Your task is to provide the best possible code completion based on the given context.

Input Format

The input will be enclosed in <input> tags with the following structure:

    First line: Name of the programming language
    All subsequent lines: Existing code up to the cursor position where completion is needed

Your Task

Generate ONLY the code that should come next at the cursor position. Your completion should:

    Be valid, syntactically correct code in the specified language
    Logically continue from the exact cursor position
    Complete the current statement, block, or function appropriately
    If the last line is a comment, generate code that implements what the comment describes
    Match the existing code's style, indentation, and naming conventions
    Consider the full context including variables, functions, classes, and imports
    Be production-ready and follow best practices for the specified language

Output Requirements

    Provide ONLY the code to be inserted - no explanations, notes, or markdown
    Do NOT repeat any code that was already in the input
    Do NOT provide multiple alternatives or suggestions
    Output only code that would be valid at the cursor position

Example

Input:

<input>
Python
def calculate_total(items):
    total = 0
    # Sum all item prices and apply 15% discount
</input>

Output:

for item in items:
    total += item.price
return total * 0.85"
  "System prompt for code completion"
  :type 'string
  :group 'elevate)

(defcustom elevate-complete-input-pattern "<input>%s\n%s</input>"
  "System prompt for code completion"
  :type 'string
  :group 'elevate)

(defun elevate-complete-here ()
  "Request and insert completions at point."
  (interactive)
  (let* ((context (elevate-get-code-context))
         (lang-prefix (elevate-detect-language))
         (orig-buffer (current-buffer))
         (orig-point (point)))
    (elevate-start-loading-animation)
    (elevate-query-async
        (format elevate-complete-input-pattern lang-prefix context)
        (lambda (response)
            (with-current-buffer orig-buffer
                (elevate-stop-loading-animation)
                (unless elevate-completion-cancelled
                    (when response
                        (save-excursion
                            (goto-char orig-point)
                            (elevate-show-inline-completion response))))))
        t
        elevate-complete-system-prompt)))

;;;;;;;;;;;;;;;;;;;;;
;; Mode Definition ;;
;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(define-minor-mode elevate-mode
  "Minor mode for LLM-powered assistance and code completion."
  :lighter " LLM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l a") #'elevate-chat)
            (define-key map (kbd "C-c l e") #'elevate-explain-code-with-context)
            (define-key map (kbd "C-c l c") #'elevate-complete-here)
            (define-key map (kbd "C-c <tab>") #'elevate-complete-here)
            (define-key map (kbd "C-c l n") #'elevate-create-context)
            (define-key map (kbd "C-c l s") #'elevate-switch-context)
            (define-key map (kbd "C-c l l") #'elevate-list-contexts)
            (define-key map (kbd "C-c l r") #'elevate-remove-context)
            (define-key map (kbd "C-c l m") #'elevate-switch-chat-model)
            (define-key map (kbd "C-c l M") #'elevate-switch-completion-model)
            (define-key map [tab] #'elevate-smart-tab)
            (define-key map (kbd "TAB") #'elevate-smart-tab)
            map)
  :global nil  ; Make it buffer-local by default
  (if elevate-mode
      (progn
        (unless elevate-chat-contexts
            (setq elevate-chat-contexts (make-hash-table :test 'equal)))
        ;; Set up completion cancellation for any command except TAB
        (add-hook 'pre-command-hook #'elevate-maybe-cancel-completion nil t)
        (message "LLM mode enabled. Use C-c l c for completion, C-c l n for new context."))
    (remove-hook 'pre-command-hook #'elevate-maybe-cancel-completion t)
    (elevate-cancel-completion)
    (elevate-stop-loading-animation)
    (when elevate-current-overlay
      (delete-overlay elevate-current-overlay))
      (message "LLM mode disabled.")))

(defcustom elevate-ignored-modes
  '(fundamental-mode special-mode dired-mode)
  "Major modes where elevate should not be enabled automatically."
  :type '(repeat symbol)
  :group 'elevate)

(defcustom elevate-enabled-modes nil
  "If non-nil, only enable elevate in these major modes.
If nil, enable in all modes except those in `elevate-ignored-modes'."
  :type '(choice (const :tag "All modes except ignored" nil)
                (repeat symbol))
  :group 'elevate)

(defun elevate-maybe-enable ()
  "Enable elevate-mode if appropriate for current buffer."
  (when (and (not (minibufferp))
             (not (derived-mode-p 'special-mode))
             (not (string-prefix-p " " (buffer-name)))
             (not (member major-mode elevate-ignored-modes))
             (or (null elevate-enabled-modes)
                 (member major-mode elevate-enabled-modes)))
    (elevate-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-elevate-mode
  elevate-mode
  (lambda ()
    (elevate-maybe-enable))
  :group 'elevate)

(provide 'my/elevate)
