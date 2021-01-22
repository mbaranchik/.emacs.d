;;; -*- lexical-binding: t -*-

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :demand
  :bind (("C-c c" . counsel-projectile-org-capture)
         ("C-c a" . counsel-projectile-org-agenda))
  )

(use-package ivy-xref
  :after ivy
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
