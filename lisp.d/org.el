;;; -*- lexical-binding: t -*-

(use-package org
    :straight (:type built-in)
    :config
    (push 'org-tempo org-modules)
    (push 'org-protocol org-modules)
    (setq org-capture-templates `(
	                                 ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	                                 ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                         "* %? [[%:link][%:description]] \nCaptured On: %U")
                                     ))
    )
(use-package org-roam
    :after org
    :custom
    (org-roam-directory (file-truename "~/org-roam"))
    :config
    (org-roam-db-autosync-mode +1)
    (push (file-truename "~/org-roam") safe-local-variable-directories)
    )

(use-package org-roam-ui
    :after org-roam)

(use-package git-auto-commit-mode)

(provide 'my/org)
