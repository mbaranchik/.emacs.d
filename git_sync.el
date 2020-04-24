;;; -*- lexical-binding: t -*-

(defun find-git-root-from (local-path)
  "Given a local path, try to locate the root of a Git repository
clone by searching the directory hierarchy."
  ;;(interactive)
  (if (equal local-path "/")
      nil
    (let ((curPath (concat local-path "/.git")))
      (if (file-directory-p curPath)
          (progn (message local-path) local-path)
        (find-git-root-from (expand-file-name (concat local-path "../")))
        )
      )
    )
  )

(defun find-git-root ()
  "Using the current buffer's file path as a starting point, try
to locate the root of a Git repository clone by searching the
directory hierarchy."
  ;;(interactive)
  (find-git-root-from
   (file-name-directory
    (or load-file-name buffer-file-name)))
  )

;;;;;;;; Git-sync ;;;;;;;;;

(defgroup git-sync nil "Git sync")

(defun git-sync-exec-sync ()
  "execute sync if editing file path contains .sync file"
  (interactive)
  (let* ((git_sync_dir (locate-dominating-file default-directory ".git"))
         (no_sync (locate-dominating-file default-directory ".nosync"))
         (sync_dir (locate-dominating-file default-directory ".sync")))
    (when (and git_sync_dir sync_dir (not no_sync))
      (save-window-excursion
        ;; avoid annoying shell comannd window
        (let* ((git_sync_dir (cadr (split-string (cadr (split-string (file-truename (expand-file-name git_sync_dir)) gitsync-basefrom)) gitsync-basefrom2))))
          (shell-command (format "cd %s/%s/%s; %s --ltr -d %s -h dub-gw015 &" gitsync-basefrom gitsync-basefrom2 gitsync-script git_sync_dir git_sync_dir) nil)
          )))
    (when sync_dir
        (save-window-excursion
          ;; avoid annoying shell comannd window
          (shell-command (format "cd %s; ./.sync &" sync_dir) nil)
          ))
    ))

(defun git-sync-pull ()
  "execute git pull if editing file path contains .pull file"
  (interactive)
  (let* ((sync_dir (locate-dominating-file default-directory ".sync")))
    (when sync_dir
      (save-window-excursion
        ;; avoid annoying shell comannd window
        (shell-command (format "cd %s; ./.pull &" sync_dir sync_dir) nil)
        ))))

(define-minor-mode git-sync-mode
  "automatically execute git-sync when editing file's path contains .sync"
  :lighter " Git-sync"
  :global t
  (cond (git-sync-mode
         (add-hook 'after-save-hook 'git-sync-exec-sync))
        (t
         (remove-hook 'after-save-hook 'git-sync-exec-sync))))

(provide 'git-sync)

;;;;;;;;;;;;;;;;;;;;;;;;



