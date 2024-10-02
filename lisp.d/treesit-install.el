;;; -*- lexical-binding: t -*-

(setq lang-list '(bash c cpp cmake dockerfile elisp json lua make python rust yaml verilog))

(setq treesit-language-source-alist my/treesit-sources)

(dolist (item lang-list)
  (treesit-install-language-grammar item))
