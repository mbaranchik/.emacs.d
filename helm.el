;;; -*- lexical-binding: t -*-

;;; HELM BEGIN ;;;

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :after helm
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)
  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe)
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 10)
  (helm-autoresize-mode 1)
  (when (executable-find "ack")
    (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))
  (helm-mode 1))

(use-package helm-xref
  :after helm-config
  :config
  (when helm-mode
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)))

;;; HELM END  ;;;

