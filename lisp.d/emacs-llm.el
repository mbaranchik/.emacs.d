(require 'json)
(require 'shell)

(defcustom emacs-llm-cookie-file (expand-file-name "~/.midway/cookie")
  "Path to the cookie file for LLM requests."
  :type 'string
  :group 'emacs-llm)

(defcustom emacs-llm-api-endpoint "https://api.chat.com.amazon.dev"
  "API endpoint for the LLM service."
  :type 'string
  :group 'emacs-llm)

(defun emacs-llm-query-async (prompt callback)
  "Send an asynchronous query to the LLM and call CALLBACK with the result."
  (let* ((json-data (json-encode `(("prompt" . ,prompt)
                                   ("chatHistory" . []))))
         (escaped-json (shell-quote-argument json-data))
         (curl-command (format "curl -L --cookie %s --cookie-jar %s '%s' --data-raw %s"
                               emacs-llm-cookie-file
                               emacs-llm-cookie-file
                               emacs-llm-api-endpoint
                               escaped-json)))
    (message "Sending request to LLM...")
    (make-process
     :name "emacs-llm-curl"
     :buffer " *emacs-llm-curl*"
     :command (list "sh" "-c" curl-command)
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (let ((response (buffer-string)))
                       (if (string-match "\"completion\":\"\\(.*?\\)\"" response)
                           (funcall callback (replace-regexp-in-string "\\\\" "" (match-string 1 response)))
                         (funcall callback nil)))))))))

(defun emacs-llm-assist ()
  "Interactively ask the LLM for assistance."
  (interactive)
  (let ((prompt (read-string "Ask LLM: ")))
    (emacs-llm-query-async
     prompt
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create "*LLM Conversation*")
             (goto-char (point-max))
             (insert (format "You: %s\n\nLLM: %s\n\n" prompt response))
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun emacs-llm-explain-code ()
  "Ask the LLM to explain the code in the current region or buffer."
  (interactive)
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (prompt (format "Explain the following code:\n\n%s" code)))
    (emacs-llm-query-async
     prompt
     (lambda (response)
       (if response
           (with-current-buffer (get-buffer-create "*Code Explanation*")
             (erase-buffer)
             (insert response)
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun emacs-llm-get-context ()
  "Get the context for code completion."
  (let* ((line-start (line-beginning-position))
         (context-start (max (- line-start 500) (point-min)))
         (context (buffer-substring-no-properties context-start (point))))
    context))

(defun emacs-llm-completion-at-point ()
  "Function to be added to `completion-at-point-functions'."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (list (or (car bounds) (point))
          (or (cdr bounds) (point))
          (completion-table-dynamic
           (lambda (_)
             (let* ((context (emacs-llm-get-context))
                    (prompt (format "Complete the following code:\n\n%s" context))
                    (completions '()))
               (emacs-llm-query-async
                prompt
                (lambda (response)
                  (when response
                    (setq completions (split-string response "\n" t "[ \t\n\r]+")))))
               (while (not completions)
                 (sleep-for 0.1))
               completions)))
          :exclusive 'no)))

(define-minor-mode emacs-llm-mode
  "Minor mode for LLM-powered assistance and code completion."
  :lighter " LLM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l a") #'emacs-llm-assist)
            (define-key map (kbd "C-c l e") #'emacs-llm-explain-code)
            map)
  (if emacs-llm-mode
      (add-hook 'completion-at-point-functions #'emacs-llm-completion-at-point nil t)
    (remove-hook 'completion-at-point-functions #'emacs-llm-completion-at-point t)))

(provide 'emacs-llm)
