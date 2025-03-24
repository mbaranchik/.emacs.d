;;; -*- lexical-binding: t -*-

(use-package org
    :straight (:type built-in)
    :demand t
    :bind
    (:map org-mode-map
        ("C-c $" . org-archive-subtree-default-with-confirmation)
        )
    :config
    (setq org-directory "~/org-roam")
    (setq org-modules '(org-tempo
                       org-protocol
                       org-habit
                       org-agenda))

    ;; Agenda configuration
    (setq org-agenda-files '("~/org-roam"))
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    ;; Refile configuration
    (setq org-refile-targets '((nil :maxlevel . 3)
                              (org-agenda-files :maxlevel . 3)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; Visual enhancements
    (setq org-hide-emphasis-markers t)           ; Hide formatting markers
    (setq org-pretty-entities t)                 ; Show entities as UTF8
    (setq org-ellipsis "⤵")                     ; Custom ellipsis
    (setq org-catch-invisible-edits 'show)      ; Show invisible edits
    (setq org-list-allow-alphabetical t)        ; Enable alphabetical lists

    ;; Only interpret subscripts when using curly brackets
    (setq org-use-sub-superscripts '{})
    
    ;; Improve faces
    (custom-set-faces
        '(org-document-title ((t (:height 1.5 :weight bold))))
        '(org-level-1 ((t (:height 1.3 :weight bold))))
        '(org-level-2 ((t (:height 1.2 :weight bold))))
        '(org-level-3 ((t (:height 1.1 :weight bold)))))

    ;; Source block configuration
    (setq org-src-fontify-natively t            ; Native code highlighting
        org-src-tab-acts-natively t           ; Tab works as in native mode
        org-src-preserve-indentation t)       ; Preserve source block indentation

    ;; Archive configuration
    (setq org-archive-reversed-order t)
    (setq org-archive-location "::* Archived Items")
    (setq org-archive-sibling-heading "Archived Items")
    (setq org-archive-mark-done t)
    (setq org-archive-default-command 'org-archive-to-archive-sibling)

    ;; Make sure ID is included in archived entries
    (setq org-archive-save-context-info '(time file category todo itags olpath))

    ;; Ensure org-id is loaded
    (require 'org-id)
    (setq org-id-link-to-org-use-id t)
    (setq org-id-method 'uuid)

    ;; General org settings
    (setq org-startup-folded 'content)           ; Start with content visible
    (setq org-startup-indented t)                ; Enable org-indent-mode by default
    (setq org-use-fast-todo-selection 'expert)   ; Fast todo selection
    (setq org-enforce-todo-dependencies t)       ; Block parent TODO if children not done
    (setq org-log-repeat 'time)                  ; Log time when repeating tasks

    ;; Custom agenda views
    (setq org-agenda-custom-commands
        '(("d" "Dashboard"
              ((agenda "" ((org-deadline-warning-days 7)))
                  (todo "NEXT"
                      ((org-agenda-overriding-header "Next Tasks")))
                  (tags-todo "inbox"
                      ((org-agenda-overriding-header "Inbox")
                          (org-agenda-files '("~/org-roam/inbox.org"))))
                  (tags-todo "PROJECT"
                      ((org-agenda-overriding-header "Active Projects")))))
             ("n" "Next Tasks"
                 ((todo "NEXT"
                      ((org-agenda-overriding-header "Next Tasks")))))
             ("w" "Waiting Tasks"
                 ((todo "WAITING"
                      ((org-agenda-overriding-header "Waiting Tasks")))))))

    ;; Tag hierarchy
    (setq org-tag-alist '((:startgroup)
                         ("@work" . ?w)
                         ("@home" . ?h)
                         (:endgroup)
                         ("project" . ?p)
                         ("idea" . ?i)
                         ("meeting" . ?m)
                         ("reading" . ?r)
                         ("research" . ?s)))

    ;; Default TODO keywords
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

    (setq org-capture-templates `(
	                             ("p" "Protocol" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
                                         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	                             ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
                                         "* %? [[%:link][%:description]] \nCaptured On: %U")
                                     ("i" "Inbox" entry (file+headline ,(concat org-directory "/inbox.org") "Inbox")
                                         "* %^{Title}\n:PROPERTIES:\n:FROM: %^{From}\n:END:\n\n%^{Message}\n\nCaptured: %U\n")
                                     ))
    )

(use-package ob-mermaid
    :config
    (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")
    :after org)

(use-package org-modern
    :after org
    :hook (org-mode . org-modern-mode)
    :config
    ;; (global-org-modern-mode)
    ;; Customize org-modern style
    (setq org-modern-star '("◉" "○" "●" "○" "●" "○" "●")
        org-modern-hide-stars nil              ; Don't hide the leading stars
        org-modern-timestamp t                 ; Pretty timestamp
        org-modern-table nil))                 ; Keep default table style

(use-package org-appear
    :after org
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-autolinks t)               ; Show hidden link parts
    (setq org-appear-autosubmarkers t)          ; Show subscript/superscript markers
    (setq org-appear-autoentities t))           ; Show entity markers

(use-package org-roam
    :after org
    :demand t
    :custom
    (org-roam-directory (file-truename "~/org-roam"))
    (org-roam-dailies-directory "journals/")  ; Store daily notes in journals folder
    (org-roam-completion-everywhere t)
    :config
    (require 'org-roam-dailies)
    (org-roam-db-autosync-mode +1)
    (setq org-roam-file-extensions '("org" "org_archive"))

    ;; Configure org-roam to work with agenda
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry
             "* %<%I:%M %p> %?"
             :if-new (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: :journal:\n\n* Tasks\n\n* Notes\n"))
            ("t" "Task" entry
             "* TODO %?\n  %U\n  %a\n  %i"
             :if-new (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: :journal:\n\n* Tasks\n\n* Notes\n")
             :empty-lines 1
             :heading "Tasks")
            ("j" "Journal" entry
             "* %<%I:%M %p> %?\n%i\n"
             :if-new (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: :journal:\n\n* Tasks\n\n* Notes\n")
             :empty-lines 1
             :heading "Notes")))

    ;; Add tags to easily identify tasks in agenda
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: ")
             :unnarrowed t)))
    (push (file-truename "~/org-roam") safe-local-variable-directories)

    :bind
    (("C-c n d" . org-roam-dailies-map)
     ("C-c n j" . org-roam-dailies-capture-today)
     :map org-roam-dailies-map
     ("d" . org-roam-dailies-goto-today)
     ("y" . org-roam-dailies-goto-yesterday)
     ("t" . org-roam-dailies-goto-tomorrow)
     ("f" . org-roam-dailies-goto-date)
     ("b" . org-roam-dailies-goto-previous-note)
     ("n" . org-roam-dailies-goto-next-note))
    )

(use-package org-roam-ui
    :after org-roam
    :commands org-roam-ui-mode)

(use-package git-auto-commit-mode)

;; Key bindings for agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(provide 'my/org)
