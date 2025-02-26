;;; -*- lexical-binding: t -*-

(require 'json)
(require 'shell)
(require 'markdown-mode)

;;;;;;;;;;;;;;;;;;
;; Custom Group ;;
;;;;;;;;;;;;;;;;;;
(defgroup emacs-llm nil
  "Customization group for emacs-llm."
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;
;; Model Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defcustom emacs-llm-models
  '(("AmazonNovaPro" . "amazon.nova-pro-v1:0")
    ("Sonnet3.5v2" . "anthropic.claude-3-5-sonnet-20241022-v2:0")
    ("Haiku3.5" . "anthropic.claude-3-5-haiku-20241022-v1:0")
    ("Sonnet3.7Think" . "anthropic.claude-3-7-sonnet-20250219-v1:0"))
  "Available LLM models and their IDs."
  :type '(alist :key-type string :value-type string)
  :group 'emacs-llm)

(defcustom emacs-llm-chat-default-model "Sonnet3.5v2"
  "Default model for chat and explanation."
  :type `(choice ,@(mapcar (lambda (m) `(const ,(car m))) emacs-llm-models))
  :group 'emacs-llm)

(defcustom emacs-llm-completion-default-model "Haiku3.5"
  "Default model for code completion."
  :type `(choice ,@(mapcar (lambda (m) `(const ,(car m))) emacs-llm-models))
  :group 'emacs-llm)

(defcustom emacs-llm-temperature 1.0
  "Temperature for model inference."
  :type 'number
  :group 'emacs-llm)

(defcustom emacs-llm-top-p 1.0
  "Top-P value for model inference."
  :type 'number
  :group 'emacs-llm)

(defcustom emacs-llm-top-k 250
  "Top-K value for model inference."
  :type 'integer
  :group 'emacs-llm)

(defvar-local emacs-llm-current-chat-model nil
  "Currently selected model for chat/explanation in this buffer.")

(defvar-local emacs-llm-current-completion-model nil
  "Currently selected model for completion in this buffer.")

(defun emacs-llm-get-model-id (model-name)
  "Get the model ID for MODEL-NAME."
  (cdr (assoc model-name emacs-llm-models)))

(defun emacs-llm-switch-chat-model ()
  "Switch the model used for chat and explanation."
  (interactive)
  (let ((chosen (completing-read "Choose model for chat/explanation: "
                               (mapcar #'car emacs-llm-models)
                               nil t)))
    (setq emacs-llm-current-chat-model chosen)
    (message "Chat model switched to %s" chosen)))

(defun emacs-llm-switch-completion-model ()
  "Switch the model used for code completion."
  (interactive)
  (let ((chosen (completing-read "Choose model for completion: "
                               (mapcar #'car emacs-llm-models)
                               nil t)))
    (setq emacs-llm-current-completion-model chosen)
    (message "Completion model switched to %s" chosen)))

(defun emacs-llm-get-inference-params (model-name)
  "Get the model inference parameters as a JSON object, formatted for MODEL-NAME."
  (if (string= model-name "AmazonNovaPro")
      ;; Nova Pro format
      `(("temperature" . ,emacs-llm-temperature)
        ("topP" . ,emacs-llm-top-p)
        ("topK" . ,emacs-llm-top-k))
      ;; Claude format
      `(("temperature" . ,emacs-llm-temperature)
         ("top_p" . ,emacs-llm-top-p)
         ("top_k" . ,emacs-llm-top-k))))

;;;;;;;;;;;;;;;;;;;;;
;; Context Support ;;
;;;;;;;;;;;;;;;;;;;;;
(defvar emacs-llm-chat-contexts (make-hash-table :test 'equal)
  "Hash table storing chat contexts by their names.")

(defvar emacs-llm-current-context nil
  "Current chat context name being used.")

(defun emacs-llm-get-context-buffer-name (context-name)
  "Get the buffer name for CONTEXT-NAME."
  (format "*LLM Chat: %s*" context-name))

(defun emacs-llm-switch-to-context-buffer (context-name)
  "Switch to the buffer for CONTEXT-NAME."
  (let ((buf-name (emacs-llm-get-context-buffer-name context-name)))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p 'markdown-mode)
        (markdown-mode))
        (display-buffer (current-buffer)))))

(defcustom emacs-llm-contexts-directory
  (expand-file-name ".llm_contexts" user-emacs-directory)
  "Directory where LLM conversation contexts are stored."
  :type 'directory
  :group 'emacs-llm)

(defun emacs-llm-ensure-contexts-directory ()
  "Ensure the contexts directory exists."
  (unless (file-directory-p emacs-llm-contexts-directory)
    (make-directory emacs-llm-contexts-directory t)))

(defun emacs-llm-context-file (context-name)
  "Get the file path for a given CONTEXT-NAME."
  (expand-file-name (concat (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" context-name) ".json")
                    emacs-llm-contexts-directory))

(defun emacs-llm-save-context (context-name)
  "Save the specified context to disk."
  (when-let ((history (gethash context-name emacs-llm-chat-contexts)))
    (emacs-llm-ensure-contexts-directory)
    (with-temp-file (emacs-llm-context-file context-name)
      (insert (json-encode history)))))

;; (defun emacs-llm-load-context (context-name)
;;   "Load a context from disk if it exists and isn't already loaded."
;;   (unless (gethash context-name emacs-llm-chat-contexts)
;;     (let ((file (emacs-llm-context-file context-name)))
;;       (when (file-exists-p file)
;;         (with-temp-buffer
;;           (insert-file-contents file)
;;           (condition-case nil
;;               (let ((history (json-parse-string
;;                             (buffer-string)
;;                             :object-type 'alist
;;                             :array-type 'list)))
;;                 (puthash context-name history emacs-llm-chat-contexts))
;;             (error
;;              (message "Error loading context %s" context-name)
;;                 nil)))))))
(defun emacs-llm-load-context (context-name)
  "Load a context from disk if it exists and isn't already loaded."
  (unless (gethash context-name emacs-llm-chat-contexts)
    (let ((file (emacs-llm-context-file context-name)))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (condition-case nil
              (let ((history (json-parse-string
                            (buffer-string)
                            :object-type 'alist
                            :array-type 'list)))
                (puthash context-name history emacs-llm-chat-contexts)
                (message "Loaded context %s with %d messages"
                        context-name (length history)))
            (error
             (message "Error loading context %s" context-name)
             nil)))))))

(defun emacs-llm-list-saved-contexts ()
  "Get a list of all saved context names."
  (emacs-llm-ensure-contexts-directory)
  (mapcar (lambda (file)
            (string-remove-suffix ".json"
                                (file-name-nondirectory file)))
          (directory-files emacs-llm-contexts-directory t "\\.json$")))

(defun emacs-llm-create-context (context-name)
  "Create a new chat context with CONTEXT-NAME."
  (interactive "sEnter new context name: ")
  (if (gethash context-name emacs-llm-chat-contexts)
      (message "Context '%s' already exists" context-name)
    (unless emacs-llm-chat-contexts
      (setq emacs-llm-chat-contexts (make-hash-table :test 'equal)))
    (puthash context-name '() emacs-llm-chat-contexts)
    (setq emacs-llm-current-context context-name)
    (emacs-llm-switch-to-context-buffer context-name)))

;; (defun emacs-llm-display-context-history (context-name)
;;   "Display the history of CONTEXT-NAME in its buffer."
;;   (when-let ((history (gethash context-name emacs-llm-chat-contexts)))
;;     (with-current-buffer (get-buffer-create (emacs-llm-get-context-buffer-name context-name))
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (markdown-mode)
;;         (dolist (entry (seq-partition history 2))
;;           (let ((human-msg (cadr (alist-get 'text
;;                                           (aref (alist-get 'content (car entry)) 0))))
;;                 (assistant-msg (cadr (alist-get 'text
;;                                              (aref (alist-get 'content (cadr entry)) 0)))))
;;             (insert (format "### Question\n\n%s\n\n### Answer\n\n%s\n\n---\n\n"
;;                         human-msg assistant-msg))))))))
(defun emacs-llm-display-context-history (context-name)
  "Display the history of CONTEXT-NAME in its buffer."
  (when-let ((history (gethash context-name emacs-llm-chat-contexts)))
    (with-current-buffer (get-buffer-create (emacs-llm-get-context-buffer-name context-name))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (markdown-mode)
        (dolist (entry (seq-partition history 2))
          (let ((human-msg (alist-get 'text (car entry)))
                (assistant-msg (alist-get 'text (cadr entry))))
            (insert (format "### Question\n\n%s\n\n### Answer\n\n%s\n\n---\n\n"
                          human-msg assistant-msg))))))))

(defun emacs-llm-switch-to-context-buffer (context-name)
  "Switch to the buffer for CONTEXT-NAME."
  (let ((buf-name (emacs-llm-get-context-buffer-name context-name)))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p 'markdown-mode)
        (markdown-mode))
      ;; Display existing history
      (emacs-llm-display-context-history context-name)
      (display-buffer (current-buffer)))))

;; (defun emacs-llm-switch-context ()
;;   "Switch to a different chat context."
;;   (interactive)
;;   (unless emacs-llm-chat-contexts
;;     (setq emacs-llm-chat-contexts (make-hash-table :test 'equal)))
;;   (let* ((available-contexts (delete-dups
;;                              (append (hash-table-keys emacs-llm-chat-contexts)
;;                                    (emacs-llm-list-saved-contexts))))
;;          (chosen (completing-read "Choose context: " available-contexts nil t)))
;;     ;; Load the context if needed
;;     (unless (gethash chosen emacs-llm-chat-contexts)
;;       (emacs-llm-load-context chosen))
;;     (setq emacs-llm-current-context chosen)
;;       (emacs-llm-switch-to-context-buffer chosen)))

(defun emacs-llm-debug-context (context-name)
  "Print debug information about a context's history."
  (interactive "sContext name: ")
  (let ((history (gethash context-name emacs-llm-chat-contexts)))
    (message "Context %s history:\n%S" context-name history)))

(defun emacs-llm-switch-context ()
  "Switch to a different chat context."
  (interactive)
  (unless emacs-llm-chat-contexts
    (setq emacs-llm-chat-contexts (make-hash-table :test 'equal)))
  (let* ((available-contexts (delete-dups
                             (append (hash-table-keys emacs-llm-chat-contexts)
                                   (emacs-llm-list-saved-contexts))))
         (chosen (completing-read "Choose context: " available-contexts nil t)))
    ;; Load the context if needed
    (unless (gethash chosen emacs-llm-chat-contexts)
      (emacs-llm-load-context chosen))
    ;; Debug output
    (message "Context loaded")
    (setq emacs-llm-current-context chosen)
    (emacs-llm-switch-to-context-buffer chosen)))

(defun emacs-llm-remove-context ()
  "Remove a chat context."
  (interactive)
  (unless emacs-llm-chat-contexts
    (setq emacs-llm-chat-contexts (make-hash-table :test 'equal)))
  (let* ((available-contexts (delete-dups
                             (append (hash-table-keys emacs-llm-chat-contexts)
                                   (emacs-llm-list-saved-contexts))))
         (chosen (completing-read "Remove context: " available-contexts nil t)))
    (when (equal chosen emacs-llm-current-context)
      (setq emacs-llm-current-context nil))
    (remhash chosen emacs-llm-chat-contexts)
    (let ((context-file (emacs-llm-context-file chosen)))
      (when (file-exists-p context-file)
        (delete-file context-file)))
    (let ((buf (get-buffer (emacs-llm-get-context-buffer-name chosen))))
      (when buf
        (kill-buffer buf)))
    (message "Context '%s' removed" chosen)))

(defun emacs-llm-list-contexts ()
  "List all available chat contexts."
  (interactive)
  (unless emacs-llm-chat-contexts
    (setq emacs-llm-chat-contexts (make-hash-table :test 'equal)))
  (let ((contexts (delete-dups
                   (append (hash-table-keys emacs-llm-chat-contexts)
                          (emacs-llm-list-saved-contexts)))))
    (if contexts
        (with-current-buffer (get-buffer-create "*LLM Contexts*")
          (erase-buffer)
          (markdown-mode)
          (insert "# Available Chat Contexts\n\n")
          (dolist (ctx contexts)
            (insert (format "- %s%s%s\n"
                          ctx
                          (if (equal ctx emacs-llm-current-context)
                              " (current)"
                            "")
                          (if (gethash ctx emacs-llm-chat-contexts)
                              " (loaded)"
                            " (on disk)"))))
          (display-buffer (current-buffer)))
      (message "No chat contexts available"))))

;;;;;;;;;;;;;;;;;;;;;
;; Connection Info ;;
;;;;;;;;;;;;;;;;;;;;;
(defcustom emacs-llm-cookie-file (expand-file-name "~/.midway/cookie")
  "Path to the cookie file for LLM requests."
  :type 'string
  :group 'emacs-llm)

(defcustom emacs-llm-api-endpoint "https://api.chat.com.amazon.dev"
  "API endpoint for the LLM service."
  :type 'string
  :group 'emacs-llm)


;;;;;;;;;;;;;;;;;;;;;
;; Query Functions ;;
;;;;;;;;;;;;;;;;;;;;;
(defun emacs-llm-decode-response (response)
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

;;(defun hash-table-create (alist)
;;  "Create a hash table from ALIST."
;;  (let ((table (make-hash-table :test 'equal)))
;;    (dolist (pair alist)
;;      (puthash (car pair) (cdr pair) table))
;;    table))
;;(defun emacs-llm-format-message (role content)
;;  "Format a chat message according to Bedrock's expected format."
;;  `(("role" . ,role)
;;    ("content" . [,(hash-table-create
;;                    (list (cons "type" "text")
;;                        (cons "text" content)))])))
(defun emacs-llm-format-message (sender text)
  "Format a chat message according to the expected format."
  `(("sender" . ,sender)
       ("text" . ,text)))

(defun emacs-llm-with-temp-file (prefix content)
  "Create a temporary file with PREFIX and CONTENT, return path."
  (let ((temp-file (make-temp-file prefix)))
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun emacs-llm-chat-query-async (prompt context-name callback)
    "Send a query with chat history from CONTEXT-NAME to the LLM."
    ;; Ensure context is loaded
    (unless (gethash context-name emacs-llm-chat-contexts)
        (emacs-llm-load-context context-name))
    (let* ((chat-history (or (gethash context-name emacs-llm-chat-contexts) '()))
              (model-name (or emacs-llm-current-chat-model
                              emacs-llm-chat-default-model))
              (json-data (json-encode
                             `(("prompt" . ,prompt)
                               ("chatHistory" . ,(vconcat chat-history))
                               ("model" . ,(emacs-llm-get-model-id model-name))
                               ("modelInferenceParams" . ,(emacs-llm-get-inference-params model-name)))))
              (request-file (emacs-llm-with-temp-file "llm-request-" json-data))
              (response-file (make-temp-file "llm-response-"))
              (curl-command (format "curl -L --cookie %s --cookie-jar %s '%s' --data-binary @%s -o %s"
                                emacs-llm-cookie-file
                                emacs-llm-cookie-file
                                emacs-llm-api-endpoint
                                request-file
                                response-file))
              (buf-name (generate-new-buffer-name " *emacs-llm-curl*"))
              (buf (get-buffer-create buf-name))
              (proc (get-buffer-process buf)))
        (message "Sending request to LLM...")
        ;; (message (format "CMD: '%S'\n\nJSON: '%S'" curl-command json-data))
        (when proc
            (delete-process proc))
        (with-current-buffer buf
            (erase-buffer))
        (make-process
            :name (concat "emacs-llm-curl-" (number-to-string (random)))
            :buffer buf
            :command (list "sh" "-c" curl-command)
            :sentinel (lambda (proc event)
                          (when (string= event "finished\n")
                              ;; (with-current-buffer (process-buffer proc)
                              ;;     (let* ((json-response (buffer-string))
                              ;;               (json-parsed-response
                              ;;                   (json-parse-string json-response :object-type 'alist))
                              ;;               (response (alist-get 'completion
                              ;;                             json-parsed-response nil nil #'equal)))
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
                                                      (list (emacs-llm-format-message "Human" prompt)
                                                          (emacs-llm-format-message "Assistant" response))))
                                              (puthash context-name chat-history emacs-llm-chat-contexts)
                                              ;; Save context after updating
                                              (emacs-llm-save-context context-name))
                                          (funcall callback (emacs-llm-decode-response response)))))
                              ;; Clean up
                              (ignore-errors
                                  (delete-file request-file)
                                  (delete-file response-file)
                                  )
                              (kill-buffer (process-buffer proc)))))))

(defun emacs-llm-query-async (system prompt callback)
    "Send a context-less query to the LLM."
    (let* ((model-name (or emacs-llm-current-completion-model
                           emacs-llm-completion-default-model))
              (chat-history (list (emacs-llm-format-message "Human" system)))
              (json-data (json-encode
                             `(("prompt" . ,prompt)
                               ("chatHistory" . ,chat-history)
                               ("model" . ,(emacs-llm-get-model-id model-name))
                               ("modelInferenceParams" . ,(emacs-llm-get-inference-params model-name)))))
              (request-file (emacs-llm-with-temp-file "llm-request-" json-data))
              (response-file (make-temp-file "llm-response-"))
              (curl-command (format "curl -L --cookie %s --cookie-jar %s '%s' --data-binary @%s -o %s"
                                emacs-llm-cookie-file
                                emacs-llm-cookie-file
                                emacs-llm-api-endpoint
                                request-file
                                response-file))
              (buf-name (generate-new-buffer-name " *emacs-llm-curl*"))
              (buf (get-buffer-create buf-name))
              (proc (get-buffer-process buf)))
        (message "Sending request to LLM...")
        ;; (message (format "CMD:'%S'" prompt))
        (when proc
            (delete-process proc))
        (with-current-buffer buf
            (erase-buffer))
        (make-process
            :name (concat "emacs-llm-curl-" (number-to-string (random)))
            :buffer buf
            :command (list "sh" "-c" curl-command)
            :sentinel (lambda (proc event)
                          (when (string= event "finished\n")
                              ;; (with-current-buffer (process-buffer proc)
                              ;;     (let* ((json-response (buffer-string))
                              ;;                  (json-parsed-response
                              ;;                      (json-parse-string json-response :object-type 'alist))
                              ;;                  (response (alist-get 'completion
                              ;;                                json-parsed-response nil nil #'equal)))
                              ;;         (funcall callback (emacs-llm-decode-response response))))
                                  (let ((json-response (with-temp-buffer
                                                           (insert-file-contents response-file)
                                                           (buffer-string))))
                                      (let* ((json-parsed-response
                                                 (json-parse-string json-response :object-type 'alist))
                                                (response (alist-get 'completion
                                                              json-parsed-response nil nil #'equal)))
                                          (funcall callback (emacs-llm-decode-response response))))
                              ;; Clean up
                              (ignore-errors
                                  (delete-file request-file)
                                  (delete-file response-file))
                              (kill-buffer (process-buffer proc)))))))

;;;;;;;;;;;;;;;;;;;;;
;; UX/UI Utilities ;;
;;;;;;;;;;;;;;;;;;;;;
(defvar emacs-llm-language-map
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

(defun emacs-llm-detect-language ()
  "Detect the programming language of the current buffer."
  (or (alist-get major-mode emacs-llm-language-map)
      (replace-regexp-in-string
       "-mode$" ""
       (replace-regexp-in-string "-ts-mode$" "" (symbol-name major-mode)))))

(defun emacs-llm-create-markdown-buffer (name)
  "Create or get a markdown buffer with NAME."
  (with-current-buffer (get-buffer-create name)
    (unless (derived-mode-p 'markdown-mode)
      (markdown-mode))
    (current-buffer)))

(defun emacs-llm-chat ()
  "Start or continue a chat with the LLM in the current context."
  (interactive)
  (unless emacs-llm-current-context
    (call-interactively #'emacs-llm-create-context))
  (let ((prompt (read-string "Ask LLM: ")))
    (emacs-llm-chat-query-async
     prompt
     emacs-llm-current-context
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create
                                (emacs-llm-get-context-buffer-name
                                 emacs-llm-current-context))
             (goto-char (point-max))
             (insert (format "### Question\n\n%s\n\n### Answer\n\n%s\n\n---\n\n"
                           prompt response))
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun emacs-llm-explain-code-with-context ()
  "Explain code with context awareness."
  (interactive)
  (unless emacs-llm-current-context
    (call-interactively #'emacs-llm-create-context))
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
         (lang-name (emacs-llm-detect-language))
         (prompt (format "Explain the following code:\n\n%s" code)))
    (emacs-llm-chat-query-async
     prompt
     emacs-llm-current-context
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create
                                (emacs-llm-get-context-buffer-name
                                 emacs-llm-current-context))
             (goto-char (point-max))
             (insert "# Code Explanation\n\n")
             (insert "## Original Code\n\n```")
             (insert lang-name)
             (insert "\n")
             (insert code)
             (insert "\n```\n\n")
             (insert "## Explanation\n\n")
             (insert response)
             (insert "\n\n---\n\n")
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun emacs-llm-query-code-with-context ()
  "Explain code with context awareness."
  (interactive)
  (unless emacs-llm-current-context
    (call-interactively #'emacs-llm-create-context))
  (let* ((user-prompt (read-string "Ask LLM: "))
         (code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
         (lang-name (emacs-llm-detect-language))
         (prompt (format "Answer the following while using content in CODE XML tag as context:\n\n%s\n\n<CODE>%s</CODE>" user-prompt code)))
    (emacs-llm-chat-query-async
     prompt
     emacs-llm-current-context
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create
                                (emacs-llm-get-context-buffer-name
                                 emacs-llm-current-context))
             (goto-char (point-max))
             (insert "# Code Explanation\n\n")
             (insert "## Original Code\n\n```")
             (insert lang-name)
             (insert "\n")
             (insert code)
             (insert "\n```\n\n")
             (insert "## Explanation\n\n")
             (insert response)
             (insert "\n\n---\n\n")
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun emacs-llm-get-code-context ()
  "Get the context for code completion."
  (let* ((line-start (line-beginning-position))
         (context-start (max (- line-start 2048) (point-min)))
         (context (buffer-substring-no-properties context-start (point))))
    context))

(defvar-local emacs-llm-current-overlay nil
  "Overlay for the current completion suggestion.")

(defun emacs-llm-show-inline-completion (completion)
  "Show COMPLETION inline at point as a greyed-out suggestion."
  (when emacs-llm-current-overlay
    (delete-overlay emacs-llm-current-overlay))
  (when completion
    (let ((ov (make-overlay (point) (point))))
      (setq emacs-llm-current-overlay ov)
      (overlay-put ov 'after-string
                  (propertize completion
                             'face '(:foreground "gray50")
                             'completion-text completion))
      ;; Store the point position
      (overlay-put ov 'completion-point (point)))))

(defun emacs-llm-accept-completion ()
  "Accept the current completion suggestion."
  (interactive)
  (when emacs-llm-current-overlay
    (let* ((completion (get-text-property 
                       0 'completion-text
                       (overlay-get emacs-llm-current-overlay 'after-string)))
           (pos (overlay-get emacs-llm-current-overlay 'completion-point)))
      ;; Remove the overlay first
      (delete-overlay emacs-llm-current-overlay)
      (setq emacs-llm-current-overlay nil)
      ;; Then insert the completion at the stored position
      (save-excursion
        (goto-char pos)
        (insert completion))
      ;; Move point to end of inserted completion
      (goto-char (+ pos (length completion))))
    t))

(defun emacs-llm-cancel-completion ()
  "Cancel the current completion suggestion."
  (interactive)
  (when emacs-llm-current-overlay
    (delete-overlay emacs-llm-current-overlay)
    (setq emacs-llm-current-overlay nil)))

(defun emacs-llm-smart-tab ()
  "Accept completion if available, otherwise do normal TAB action."
  (interactive)
  (unless (emacs-llm-accept-completion)
    (indent-for-tab-command)))

(defun emacs-llm-maybe-cancel-completion ()
  "Cancel completion if the next command isn't a completion-related command."
  (unless (memq this-command '(emacs-llm-smart-tab
                              emacs-llm-accept-completion))
    (setq emacs-llm-completion-cancelled t)
    (emacs-llm-cancel-completion)
    (emacs-llm-stop-loading-animation)))

(defvar-local emacs-llm-loading-overlay nil
  "Overlay for the loading animation.")

(defvar-local emacs-llm-loading-timer nil
  "Timer for the loading animation.")

(defvar-local emacs-llm-completion-cancelled nil
  "Flag to indicate if completion was cancelled while waiting.")

(defvar emacs-llm-loading-frames '("." ".." "...")
  "Frames for the loading animation.")

(defun emacs-llm-update-loading-animation ()
  "Update the loading animation frame."
  (when (and emacs-llm-loading-overlay
             (overlay-buffer emacs-llm-loading-overlay))
    (let* ((frames emacs-llm-loading-frames)
           (current (overlay-get emacs-llm-loading-overlay 'frame-index))
           (next (if (>= current (1- (length frames))) 0 (1+ current)))
           (frame (nth next frames)))
      (overlay-put emacs-llm-loading-overlay 'frame-index next)
      (overlay-put emacs-llm-loading-overlay 'after-string
                   (propertize frame 'face '(:foreground "gray50"))))))

(defun emacs-llm-start-loading-animation ()
  "Start the loading animation at point."
  (emacs-llm-stop-loading-animation) ; Clean up any existing animation
  (setq emacs-llm-completion-cancelled nil)
  (let ((ov (make-overlay (point) (point))))
    (setq emacs-llm-loading-overlay ov)
    (overlay-put ov 'frame-index 0)
    (overlay-put ov 'after-string
                 (propertize "." 'face '(:foreground "gray50")))
    (setq emacs-llm-loading-timer
          (run-with-timer 0 0.3 #'emacs-llm-update-loading-animation))))

(defun emacs-llm-stop-loading-animation ()
  "Stop the loading animation and clean up."
  (when emacs-llm-loading-timer
    (cancel-timer emacs-llm-loading-timer)
    (setq emacs-llm-loading-timer nil))
  (when emacs-llm-loading-overlay
    (delete-overlay emacs-llm-loading-overlay)
    (setq emacs-llm-loading-overlay nil)))

(defun emacs-llm-complete-here ()
  "Request and insert completions at point."
  (interactive)
  (let* ((context (emacs-llm-get-code-context))
         (lang-prefix (emacs-llm-detect-language))
         (orig-buffer (current-buffer))
         (orig-point (point)))
    (emacs-llm-start-loading-animation)
    (emacs-llm-query-async
;;      (format "You are an automatic expert code completer.
;; You are fluent in any coding language, including C, C++, BASH, python and more.

;; You will serve as a machine that gets input, the input is located at the bottom of this prompt between XML <input> tags:
;; 1. Programming Language name: first line of the input.
;; 2. Code Context: rest of the input

;; The code context will be a varying number of lines before current user point of writing.

;; You task:
;; 1. You will suggest the 3 (or less) best suggestions for completion at that point.
;; 2. You will mark each suggestion end with OPTIONEND string
;; 3. You will not output any text which is not the direct completion options for the input
;; 4. If there is not completion option, output empty string
;; 5. For any unexpected input from the user, you will only output MALFORMED
;; 6. If the last entry in input contains a comment (in the relevant language) with special directive, such as \"# cc: <text>\" (for bash case, similarly for other languages and their comment type), you will offer the single best completion while taking <text> as directives for that, for example, if a C++ code last entry has:
;; // cc: sum of array into sum var
;; you will suggest something appropriate, in this case it can be:
;; auto sum = 0;
;; for (auto const& v : array)
;;     sum += v;

;; <input>%s
        ;; %s</input>" lang-prefix context)
        "
You are an advanced code completion assistant embedded in an IDE. Your task is to provide the best possible code completion based on the given context.
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
        (format "<input>%s
%s</input>" lang-prefix context)
     (lambda (response)
       (with-current-buffer orig-buffer
         (emacs-llm-stop-loading-animation)
         (unless emacs-llm-completion-cancelled
           (when response
             (save-excursion
               (goto-char orig-point)
               (emacs-llm-show-inline-completion response)))))))))

;;;;;;;;;;;;;;;;;;;;;
;; Mode Definition ;;
;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(define-minor-mode emacs-llm-mode
  "Minor mode for LLM-powered assistance and code completion."
  :lighter " LLM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l a") #'emacs-llm-chat)
            (define-key map (kbd "C-c l e") #'emacs-llm-explain-code-with-context)
            (define-key map (kbd "C-c l c") #'emacs-llm-complete-here)
            (define-key map (kbd "C-c <tab>") #'emacs-llm-complete-here)
            (define-key map (kbd "C-c l n") #'emacs-llm-create-context)
            (define-key map (kbd "C-c l s") #'emacs-llm-switch-context)
            (define-key map (kbd "C-c l l") #'emacs-llm-list-contexts)
            (define-key map (kbd "C-c l r") #'emacs-llm-remove-context)
            (define-key map (kbd "C-c l m") #'emacs-llm-switch-chat-model)
            (define-key map (kbd "C-c l M") #'emacs-llm-switch-completion-model)
            (define-key map [tab] #'emacs-llm-smart-tab)
            (define-key map (kbd "TAB") #'emacs-llm-smart-tab)
            map)
  :global nil  ; Make it buffer-local by default
  (if emacs-llm-mode
      (progn
        (unless emacs-llm-chat-contexts
            (setq emacs-llm-chat-contexts (make-hash-table :test 'equal)))
        ;; Set up completion cancellation for any command except TAB
        (add-hook 'pre-command-hook #'emacs-llm-maybe-cancel-completion nil t)
        (message "LLM mode enabled. Use C-c l c for completion, C-c l n for new context."))
    (remove-hook 'pre-command-hook #'emacs-llm-maybe-cancel-completion t)
    (emacs-llm-cancel-completion)
    (emacs-llm-stop-loading-animation)
    (when emacs-llm-current-overlay
      (delete-overlay emacs-llm-current-overlay))
      (message "LLM mode disabled.")))

(defcustom emacs-llm-ignored-modes
  '(fundamental-mode special-mode dired-mode)
  "Major modes where emacs-llm should not be enabled automatically."
  :type '(repeat symbol)
  :group 'emacs-llm)

(defcustom emacs-llm-enabled-modes nil
  "If non-nil, only enable emacs-llm in these major modes.
If nil, enable in all modes except those in `emacs-llm-ignored-modes'."
  :type '(choice (const :tag "All modes except ignored" nil)
                (repeat symbol))
  :group 'emacs-llm)

(defun emacs-llm-maybe-enable ()
  "Enable emacs-llm-mode if appropriate for current buffer."
  (when (and (not (minibufferp))
             (not (derived-mode-p 'special-mode))
             (not (string-prefix-p " " (buffer-name)))
             (not (member major-mode emacs-llm-ignored-modes))
             (or (null emacs-llm-enabled-modes)
                 (member major-mode emacs-llm-enabled-modes)))
    (emacs-llm-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-emacs-llm-mode
  emacs-llm-mode
  (lambda ()
    (emacs-llm-maybe-enable))
  :group 'emacs-llm)

(provide 'my/emacs-llm)
