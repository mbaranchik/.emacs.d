;;; -*- lexical-binding: t -*-

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

(defun emacs-llm-query-async (prompt callback)
  "Send an asynchronous query to the LLM and call CALLBACK with the result.
If INCLUDE-LANGUAGE is non-nil, prepend the current language to the prompt."
  (let* ((json-data (json-encode `(("prompt" . ,prompt)
                                  ("chatHistory" . []))))
         (escaped-json (shell-quote-argument json-data))
         (curl-command (format "curl -L --cookie %s --cookie-jar %s '%s' --data-raw %s"
                             emacs-llm-cookie-file
                             emacs-llm-cookie-file
                             emacs-llm-api-endpoint
                             escaped-json))
         (buf (generate-new-buffer " *emacs-llm-curl*")))
    (message "Sending request to LLM...")
    (make-process
     :name "emacs-llm-curl"
     :buffer buf
     :command (list "sh" "-c" curl-command)
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (with-current-buffer (process-buffer proc)
                     (let ((response (buffer-string)))
                       (if (string-match "\"completion\":\"\\(.*?\\)\"" response)
                           (funcall callback 
                                  (replace-regexp-in-string "\\\\" "" 
                                                          (match-string 1 response)))
                         (funcall callback nil))))
                   (kill-buffer (process-buffer proc)))))))

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

(defvar-local emacs-llm-completion-cache nil
  "Cache for completion candidates.")

(defun emacs-llm-completion-at-point ()
  "Function to be added to `completion-at-point-functions'."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (list (or (car bounds) (point))
          (or (cdr bounds) (point))
          (completion-table-dynamic
           (lambda (_)
               (let* ((context (emacs-llm-get-context))
                      (lang-prefix (emacs-llm-detect-language)))
               (setq emacs-llm-completion-cache nil)
               (emacs-llm-query-async
                (format "You are an automatic expert code completer.
You are fluent in any coding language, including C, C++, BASH, python and more.

You will serve as a machine that gets input, the input is located at the bottom of this prompt between XML <input> tags:
1. Programming Language name: first line of the input.
2. Code Context: rest of the input

The code context will be a varying number of lines before current user point of writing.

You task:
1. You will suggest the 3 best suggestions for completion at that point.
2. You will mark each suggestion end with OPTIONEND string
3. You will not output any text which is not the direct completion options for the input
4. If there is not completion option, output empty string
5. For any unexpected input from the user, you will only output MALFORMED
6. If the last entry in input contains a comment (in the relevant language) with special directive, such as 
# cc: <text>
(for bash case, similarly for other languages and their comment type), you will offer a completion while taking <text> as directives for that, for example, if a C++ code last entry has:
// cc: sum of array into sum var
you will suggest something appropriate, in this case it can be:
auto sum = 0;
for (auto const& v : array)
    sum += v;

<input>%s\n%s</input>" lang-prefix context)
                (lambda (response)
                  (when response
                    (setq emacs-llm-completion-cache
                          (split-string response "OPTIONEND" t "[ \t\n\r]+")))))
               (or emacs-llm-completion-cache '()))))
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
