;;; -*- lexical-binding: t -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el for straight.el
(setq package-enable-at-startup nil)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
;; (setq package-enable-at-startup nil) TODO
;; (fset #'package--ensure-init-file #'ignore)  ; DEPRECATED Removed in 28 TODO

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; macOS: libgccjit links against libSystem from the SDK, which is not on the
;; default linker path ("ld: library 'System' not found"). Put the active SDK's
;; usr/lib on LIBRARY_PATH so native compilation (packages, trampolines) works
;; however Emacs is launched (Dock, daemon, terminal).
(when (and (eq system-type 'darwin)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (let* ((sdk (car (ignore-errors (process-lines "xcrun" "--show-sdk-path"))))
         (lib (and sdk (expand-file-name "usr/lib" sdk)))
         (cur (getenv "LIBRARY_PATH")))
    (when (and lib
               (file-exists-p (expand-file-name "libSystem.tbd" lib))
               (not (member lib (and cur (split-string cur ":" t)))))
      (setenv "LIBRARY_PATH" (if (and cur (> (length cur) 0))
                                 (concat lib ":" cur)
                               lib)))))

(modify-all-frames-parameters '((background-mode . "dark")))
