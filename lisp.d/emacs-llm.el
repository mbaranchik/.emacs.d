;;; -*- lexical-binding: t -*-

(require 'json)
(require 'shell)
(require 'markdown-mode)

(defcustom emacs-llm-cookie-file (expand-file-name "~/.midway/cookie")
  "Path to the cookie file for LLM requests."
  :type 'string
  :group 'emacs-llm)

(defcustom emacs-llm-api-endpoint "https://api.chat.com.amazon.dev"
  "API endpoint for the LLM service."
  :type 'string
  :group 'emacs-llm)

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

(defun emacs-llm-decode-response (response)
    "Decode special characters in the LLM response string."
    (when response
        (thread-last response
            (replace-regexp-in-string "\\\\n" "\n")
            (replace-regexp-in-string "\\\\t" "\t")
            (replace-regexp-in-string "\\\\\"" "\"")
            (replace-regexp-in-string "\\\\\\\\" "\\")
            (replace-regexp-in-string "\\\\r" ""))))

(defun emacs-llm-query-async (prompt callback)
    "Send an asynchronous query to the LLM and call CALLBACK with the result."
    (let* ((json-data (json-encode `(("prompt" . ,prompt)
                                        ("chatHistory" . []))))
              (escaped-json (shell-quote-argument json-data))
              (curl-command (format "curl -L --cookie %s --cookie-jar %s '%s' --data-raw %s"
                                emacs-llm-cookie-file
                                emacs-llm-cookie-file
                                emacs-llm-api-endpoint
                                escaped-json))
              (buf-name (generate-new-buffer-name " *emacs-llm-curl*"))
              (buf (get-buffer-create buf-name))
              (proc (get-buffer-process buf)))
        (message "Sending request to LLM...")
        ;; Kill any existing process in the buffer
        (when proc
            (delete-process proc))
        ;; Clear the buffer content
        (with-current-buffer buf
            (erase-buffer))
        (make-process
            :name (concat "emacs-llm-curl-" (number-to-string (random)))
            :buffer buf
            :command (list "sh" "-c" curl-command)
            :sentinel (lambda (proc event)
                          (when (string= event "finished\n")
                              (with-current-buffer (process-buffer proc)
                                  (let* ((json-response (buffer-string))
                                            (json-parsed-response (json-parse-string json-response :object-type 'alist))
                                            (response (alist-get 'completion json-parsed-response nil nil #'equal)))
                                      (progn
                                          (message (format "JSON-RESPONSE: '%s'" json-response))
                                          (message (format "JSON-PARSED-RESPONSE: '%s'" json-parsed-response))
                                          (message (format "RESPONSE: '%s'" response))
                                          (funcall callback
                                              (emacs-llm-decode-response response)))))
                              (kill-buffer (process-buffer proc)))))))


(defun emacs-llm-create-markdown-buffer (name)
  "Create or get a markdown buffer with NAME."
  (with-current-buffer (get-buffer-create name)
    (unless (derived-mode-p 'markdown-mode)
      (markdown-mode))
    (current-buffer)))

(defun emacs-llm-assist ()
  "Interactively ask the LLM for assistance."
  (interactive)
  (let ((prompt (read-string "Ask LLM: ")))
    (emacs-llm-query-async
     prompt
     (lambda (response)
       (if response
           (with-current-buffer (emacs-llm-create-markdown-buffer "*LLM Conversation*")
             (goto-char (point-max))
             (insert (format "### Question\n\n%s\n\n### Answer\n\n%s\n\n---\n\n" 
                           prompt response))
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun emacs-llm-explain-code ()
  "Ask the LLM to explain the code in the current region or buffer."
  (interactive)
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max))))
         (lang-name (emacs-llm-detect-language))
         (prompt (format "Explain the following code:\n\n%s" code)))
    (emacs-llm-query-async
     prompt
     (lambda (response)
       (if response
           (with-current-buffer (emacs-llm-create-markdown-buffer "*Code Explanation*")
             (erase-buffer)
             (insert "# Code Explanation\n\n")
             (insert "## Original Code\n\n```")
             (insert lang-name)
             (insert "\n")
             (insert code)
             (insert "\n```\n\n")
             (insert "## Explanation\n\n")
             (insert response)
             (display-buffer (current-buffer)))
         (message "Error: No valid response received from the LLM."))))))

(defun emacs-llm-get-context ()
  "Get the context for code completion."
  (let* ((line-start (line-beginning-position))
         (context-start (max (- line-start 100) (point-min)))
         (context (buffer-substring-no-properties context-start (point))))
    context))

(defvar-local emacs-llm-completion-cache nil
  "Cache for completion candidates.")

(defun emacs-llm-complete-here ()
  "Request and insert completions at point."
  (interactive)
  (let* ((context (emacs-llm-get-context))
         (lang-prefix (emacs-llm-detect-language))
         (orig-buffer (current-buffer))
         (orig-point (point)))
    (emacs-llm-query-async
     (format "You are an automatic expert code completer.
You are fluent in any coding language, including C, C++, BASH, python and more.

You will serve as a machine that gets input, the input is located at the bottom of this prompt between XML <input> tags:
1. Programming Language name: first line of the input.
2. Code Context: rest of the input

The code context will be a varying number of lines before current user point of writing.

You task:
1. You will suggest the 3 (or less) best suggestions for completion at that point.
2. You will mark each suggestion end with OPTIONEND string
3. You will not output any text which is not the direct completion options for the input
4. If there is not completion option, output empty string
5. For any unexpected input from the user, you will only output MALFORMED
6. If the last entry in input contains a comment (in the relevant language) with special directive, such as \"# cc: <text>\" (for bash case, similarly for other languages and their comment type), you will offer the single best completion while taking <text> as directives for that, for example, if a C++ code last entry has:
// cc: sum of array into sum var
you will suggest something appropriate, in this case it can be:
auto sum = 0;
for (auto const& v : array)
    sum += v;

<input>%s
%s</input>" lang-prefix context)
     (lambda (response)
       (when response
         (let ((completions (split-string response "OPTIONEND" t "[ \t\n\r]+")))
           (if completions
               (with-current-buffer orig-buffer
                 (save-excursion
                   (goto-char orig-point)
                   (if (= (length completions) 1)
                       (insert (car completions))
                     (let ((chosen (completing-read "Choose completion: " completions nil t)))
                       (insert chosen)))))
             (message "No completions available"))))))))

(define-minor-mode emacs-llm-mode
  "Minor mode for LLM-powered assistance and code completion."
  :lighter " LLM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l a") #'emacs-llm-assist)
            (define-key map (kbd "C-c l e") #'emacs-llm-explain-code)
            (define-key map (kbd "C-c l c") #'emacs-llm-complete-here)
            map)
  (if emacs-llm-mode
      (message "LLM mode enabled. Use C-c l c for completion.")
    (message "LLM mode disabled.")))

(provide 'emacs-llm)
