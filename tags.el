;;; -*- lexical-binding: t -*-

;; Don't ask to reread TAGS
(setq tags-revert-without-query t)

;; Disable size warning
(setq large-file-warning-threshold nil)

;; Do case sensitive search
(setq tags-case-fold-search nil)

;; GTAGS - BEGIN
(use-package ggtags
  :hook (((verilog-mode c-mode c++-mode asm-mode python-mode) . ggtags-mode)))

(defun my/gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun my/gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (my/gtags-root-dir) " ; gtags --single-update " filename )))

(defun my/gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (my/gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (my/gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when 'ggtags-mode
    (when (my/gtags-root-dir)
      (my/gtags-update-current-file))))

(add-hook 'after-save-hook 'gtags-update-hook)
;; GTAGS - END


