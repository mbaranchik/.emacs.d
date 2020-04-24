;;; -*- lexical-binding: t -*-
(ivy-mode 1)
(use-package counsel)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
(setq ivy-initial-inputs-alist nil)
(use-package ivy-xref)
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
