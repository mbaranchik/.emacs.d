;;; -*- lexical-binding: t -*-

(use-package org
    :straight (:type built-in)
    :demand t
    :config
    (setq org-modules '(org-tempo
                       org-protocol
                       org-habit
                       org-agenda))

    ;; Agenda configuration
    (setq org-agenda-files '("~/org-roam"))
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    ;; Default TODO keywords
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

    (setq org-capture-templates `(
	                                 ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	                                 ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                         "* %? [[%:link][%:description]] \nCaptured On: %U")
                                     ))
    )
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
    :after org-roam)

(use-package git-auto-commit-mode)

;; Key bindings for agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(provide 'my/org)
