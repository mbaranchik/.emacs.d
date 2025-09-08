;;; enriched-utils.el --- Enhanced utilities for enriched-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: mbaranch
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, text, faces
;; URL: https://github.com/mbaranch/.emacs.d

;;; Commentary:

;; This package provides enhanced utilities for enriched-mode, including:
;; - Highlighting regions with specific faces
;; - Automatically highlighting all occurrences of text
;; - Persistence of highlighting across sessions
;; - Custom face management
;;
;; This implementation uses text properties instead of overlays to ensure
;; compatibility with enriched-mode.

;;; Code:

(require 'enriched)
(require 'thingatpt)

;;;;;;;;;;;;;;;;;;
;; Custom Group ;;
;;;;;;;;;;;;;;;;;;
(defgroup enriched-utils nil
  "Enhanced utilities for enriched-mode."
  :group 'enriched
  :prefix "enriched-utils-")

;;;;;;;;;;;;;;;;;;;
;; Customization ;;
;;;;;;;;;;;;;;;;;;;
(defcustom enriched-utils-highlight-faces
  '((highlight-1 . (:background "yellow" :foreground "black"))
    (highlight-2 . (:background "lightgreen" :foreground "black"))
    (highlight-3 . (:background "lightblue" :foreground "black"))
    (highlight-4 . (:background "pink" :foreground "black"))
    (highlight-5 . (:background "lavender" :foreground "black")))
  "Alist of faces for highlighting.
Each entry is a cons cell (NAME . PROPERTIES) where NAME is a symbol
and PROPERTIES is a plist of face properties."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type sexp))
  :group 'enriched-utils)

(defcustom enriched-utils-case-sensitive nil
  "Whether highlighting should be case-sensitive."
  :type 'boolean
  :group 'enriched-utils)

(defcustom enriched-utils-whole-words-only nil
  "Whether highlighting should match only whole words."
  :type 'boolean
  :group 'enriched-utils)

(defcustom enriched-utils-auto-save-highlights t
  "Whether to automatically save highlights when the buffer is saved."
  :type 'boolean
  :group 'enriched-utils)

(defcustom enriched-utils-highlight-key-prefix (kbd "C-c h")
  "Key prefix for enriched-utils highlighting commands."
  :type 'key-sequence
  :group 'enriched-utils)

;;;;;;;;;;;;;;;;;;;;;
;; Internal State ;;
;;;;;;;;;;;;;;;;;;;;;
(defvar-local enriched-utils-highlights nil
  "List of highlights in the current buffer.
Each element is a plist with properties:
- :text - The highlighted text
- :face - The face used for highlighting
- :positions - List of (BEG . END) positions for this highlight")

(defvar-local enriched-utils-highlight-id-counter 0
  "Counter for generating unique highlight IDs.")

(defvar enriched-utils-highlight-history nil
  "History of selected highlight faces.")

;;;;;;;;;;;;;;;;;;;;;;;
;; Face Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun enriched-utils-define-faces ()
  "Define faces for highlighting based on `enriched-utils-highlight-faces'."
  (dolist (face-def enriched-utils-highlight-faces)
    (let ((face-name (car face-def))
          (face-props (cdr face-def)))
      (eval `(defface ,face-name
               '((t ,face-props))
               ,(format "Face for %s highlighting." face-name)
               :group 'enriched-utils)))))

(enriched-utils-define-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enriched-utils-apply-text-property (beg end face)
  "Apply text property from BEG to END with FACE."
  (let ((inhibit-read-only t))
    ;; Only use the standard 'face property which is already supported by enriched-mode
    ;; Store our highlight ID in a text property that won't be saved by enriched-mode
    (put-text-property beg end 'face face)
    (put-text-property beg end 'enriched-utils-highlight-id (plist-get (car enriched-utils-highlights) :id))))

(defun enriched-utils-highlight-region (beg end face)
  "Highlight region from BEG to END with FACE."
  (enriched-utils-apply-text-property beg end face)
  (cons beg end))

(defun enriched-utils-find-occurrences (text)
  "Find all occurrences of TEXT in the current buffer.
Returns a list of (BEG . END) positions."
  (let ((case-fold-search (not enriched-utils-case-sensitive))
        (positions nil)
        (search-start (point-min)))
    (save-excursion
      (goto-char search-start)
      (while (search-forward text nil t)
        (let ((match-beg (match-beginning 0))
              (match-end (match-end 0)))
          (when (or (not enriched-utils-whole-words-only)
                    (and (or (= match-beg (point-min))
                             (not (eq (char-syntax (char-before match-beg)) ?w)))
                         (or (= match-end (point-max))
                             (not (eq (char-syntax (char-after match-end)) ?w)))))
            (push (cons match-beg match-end) positions)))))
    (nreverse positions)))

(defun enriched-utils-highlight-occurrences (text face)
  "Highlight all occurrences of TEXT with FACE."
  (let ((positions (enriched-utils-find-occurrences text)))
    (dolist (pos positions)
      (enriched-utils-apply-text-property (car pos) (cdr pos) face))
    positions))

(defun enriched-utils-add-highlight (text face)
  "Add TEXT with FACE to the list of highlights."
  (let ((highlight (list :text text
                         :face face
                         :positions nil
                         :id (cl-incf enriched-utils-highlight-id-counter))))
    (push highlight enriched-utils-highlights)
    highlight))

(defun enriched-utils-remove-highlight (highlight)
  "Remove HIGHLIGHT from the current buffer."
  (let ((inhibit-read-only t))
    (dolist (pos (plist-get highlight :positions))
      (let ((beg (car pos))
            (end (cdr pos)))
        (when (and (>= beg (point-min))
                   (<= end (point-max)))
          ;; Only remove the face property, which is what enriched-mode uses
          (remove-text-properties beg end '(face nil))))))
  (setq enriched-utils-highlights
        (delq highlight enriched-utils-highlights)))

(defun enriched-utils-clear-all-highlights ()
  "Remove all highlights from the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    ;; Simplest approach: just remove all face properties from the entire buffer
    (remove-text-properties (point-min) (point-max) '(face nil)))
  (setq enriched-utils-highlights nil))

(defun enriched-utils-get-highlight-at-point ()
  "Get the highlight at point, if any."
  (let ((pos (point))
        (result nil)
        (face (get-text-property pos 'face)))
    (when face
      ;; Try to find a highlight with this face
      (dolist (highlight enriched-utils-highlights)
        (when (eq (plist-get highlight :face) face)
          (setq result highlight)
          (cl-return))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Commands ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun enriched-utils-highlight-region-and-occurrences (beg end)
  "Highlight the region from BEG to END and all its occurrences."
  (interactive "r")
  (when (use-region-p)
    (let* ((text (buffer-substring-no-properties beg end))
           (face-name (completing-read "Highlight face: "
                                      (mapcar #'car enriched-utils-highlight-faces)
                                      nil t nil 'enriched-utils-highlight-history))
           (face (intern face-name))
           (highlight (enriched-utils-add-highlight text face))
           (positions (enriched-utils-highlight-occurrences text face)))
      (setf (plist-get highlight :positions) positions)
      (message "Highlighted %d occurrences of \"%s\""
               (length positions)
               (if (> (length text) 20)
                   (concat (substring text 0 20) "...")
                 text)))))

(defun enriched-utils-remove-highlight-at-point ()
  "Remove highlight at point."
  (interactive)
  (let ((highlight (enriched-utils-get-highlight-at-point)))
    (if highlight
        (progn
          (enriched-utils-remove-highlight highlight)
          (message "Highlight removed"))
      (message "No highlight at point"))))

(defun enriched-utils-modify-highlight-at-point ()
  "Modify the face of the highlight at point."
  (interactive)
  (let ((highlight (enriched-utils-get-highlight-at-point)))
    (if highlight
        (let* ((face-name (completing-read "New highlight face: "
                                          (mapcar #'car enriched-utils-highlight-faces)
                                          nil t nil 'enriched-utils-highlight-history))
               (face (intern face-name))
               (text (plist-get highlight :text))
               (positions (plist-get highlight :positions)))
          ;; Remove old highlight
          (enriched-utils-remove-highlight highlight)
          ;; Add new highlight
          (let ((new-highlight (enriched-utils-add-highlight text face)))
            (setf (plist-get new-highlight :positions)
                  (enriched-utils-highlight-occurrences text face)))
          (message "Highlight modified"))
      (message "No highlight at point"))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun enriched-utils-save-highlights ()
  "Save highlights to buffer-local variables."
  (when enriched-utils-highlights
    (let ((data (mapcar (lambda (highlight)
                         (list (plist-get highlight :text)
                               (plist-get highlight :face)))
                       enriched-utils-highlights)))
      ;; Save as file-local variables
      (add-file-local-variable 'enriched-utils-saved-highlights data))))

(defun enriched-utils-restore-highlights ()
  "Restore highlights from buffer-local variables."
  (when (boundp 'enriched-utils-saved-highlights)
    (enriched-utils-clear-all-highlights)
    (dolist (item enriched-utils-saved-highlights)
      (let* ((text (nth 0 item))
             (face (nth 1 item))
             (highlight (enriched-utils-add-highlight text face))
             (positions (enriched-utils-highlight-occurrences text face)))
        (setf (plist-get highlight :positions) positions)))))

;; Ensure enriched-mode knows about our text properties
(defun enriched-utils-register-with-enriched-mode ()
  "Register our text properties with enriched-mode."
  ;; We don't need to register our custom properties with enriched-mode
  ;; Instead, we'll rely on the standard 'face property which is already supported
  ;; This avoids issues with the enriched-mode text property translation system
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Definition ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defvar enriched-utils-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h o") #'enriched-utils-highlight-region-and-occurrences)
    (define-key map (kbd "C-c h r") #'enriched-utils-remove-highlight-at-point)
    (define-key map (kbd "C-c h m") #'enriched-utils-modify-highlight-at-point)
    (define-key map (kbd "C-c h c") #'enriched-utils-clear-all-highlights)
    map)
  "Keymap for `enriched-utils-mode'.")

;;;###autoload
(define-minor-mode enriched-utils-mode
  "Minor mode for enhanced enriched-mode utilities."
  :lighter " EnrU"
  :keymap enriched-utils-mode-map
  (if enriched-utils-mode
      (progn
        (enriched-utils-restore-highlights)
        (add-hook 'before-save-hook #'enriched-utils-save-highlights nil t))
    (remove-hook 'before-save-hook #'enriched-utils-save-highlights t)))

;;;###autoload
(define-globalized-minor-mode global-enriched-utils-mode
  enriched-utils-mode
  (lambda ()
    (when (derived-mode-p 'text-mode)
      (enriched-utils-mode 1))))

(provide 'enriched-utils)
;;; enriched-utils.el ends here
