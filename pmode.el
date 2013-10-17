;;; pmode.el

;; Copyright (C) 2013
;;   Brad Jensen

;; Author: Brad Jensen <bjensen@cs.hmc.edu>

;;; Commentary:

;; This is a major mode for taking purity tests.

(defvar p-mode-hook nil
  "Hook for functions to run after loading p mode.")

(defun p-mode-mark (label)
  "In p-mode, replace the status of the current question.

LABEL is a string to replace the status with.
"
  ;; If we can't find a question, we must be at the start.
  (search-backward "\n=" nil t)
  (re-search-forward "\n\\[\\([DdTtWw ]\\)\\]" nil t)
  ;; Replace the current status with a new one
  (replace-match label nil nil nil 1)
  ;; If we can't find a question, we must be at the end.
  (search-forward "\n[" nil t)
  (recenter)
  )

(defun p-mode-mark-done ()
  "In p-mode, mark the current question as done."
  (interactive)
  (p-mode-mark "D")
  )

(defun p-mode-mark-technicality ()
  "In p-mode, mark the current question as technically complete."
  (interactive)
  (p-mode-mark "T")
  )

(defun p-mode-mark-would-do ()
  "In p-mode, mark the current question as would do."
  (interactive)
  (p-mode-mark "W")
  )

(defun p-mode-mark-incomplete ()
  "In p-mode, mark the current question as incomplete."
  (interactive)
  (p-mode-mark " ")
  )

(defun p-mode-forward-section ()
  "In p-mode, go forward one section."
  (interactive)
  (search-forward "\n## ")
  (recenter)
  )

(defun p-mode-backward-section ()
  "In p-mode, go backward one section."
  (interactive)
  (search-backward "\n## ")
  (recenter)
  )

(defun p-mode-score-test ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let*
        ((inhibit-point-motion-hooks t) ;; Intangible does strange things
         (done-count (float (how-many "^\\[[Dd]\\]")))
         (tech-count (float (how-many "^\\[[Tt]\\]")))
         (would-count (float (how-many "^\\[[Ww]\\]")))
         (all-count (float (how-many "^\\[[DdTtWw ]\\]")))
         (done-score (* 100 (/ (- all-count done-count) all-count)))
         (tech-score (* 100 (/ (- all-count (+ done-count tech-count))
                               all-count)))
         (would-score (* 100 (/ (- all-count (+ done-count tech-count
                                                would-count))
                                all-count)))
         )
      (message (format "%3.1f / %3.1f / %3.1f // %d"
                       done-score tech-score would-score all-count))
      )
    )
  )


(defvar p-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-d") 'p-mode-mark-done)
    (define-key map (kbd "C-t") 'p-mode-mark-technicality)
    (define-key map (kbd "C-q") 'p-mode-mark-would-do)
    (define-key map (kbd "C-p") 'p-mode-mark-incomplete)
    (define-key map (kbd "C-c C-c") 'p-mode-score-test)
    map)
  "Key map for p mode.")

(defconst p-mode-section-face 'font-lock-builtin-face)
(defconst p-mode-done-face 'font-lock-comment-face)
(defconst p-mode-technicality-face 'font-lock-variable-name-face)
(defconst p-mode-would-do-face 'font-lock-reference-face)
(defconst p-mode-incomplete-face 'font-lock-warning-face)

(defconst p-font-lock-patterns
  (list
   '("##[^\n]*" . p-mode-section-face)
   '("\\[[Dd]\\][^\n]*" . p-mode-done-face)
   '("\\[[Tt]\\][^\n]*" . p-mode-technicality-face)
   '("\\[[Ww]\\][^\n]*" . p-mode-would-do-face)
   '("\\[ \\][^\n]*" . p-mode-incomplete-face)
   )
  "Font lock definitions for p-mode.")

(defvar p-mode-syntax-table
  (make-syntax-table text-mode-syntax-table)
  "Syntax table for p-mode")

(defun p-mode-protect-questions ()
  "In p-mode, protect question text and separators from being modified."
  (interactive)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t) ;; Doesn't seem to work?
        (inhibit-point-motion-hooks t)
        (buffer-undo-list t))
    (save-excursion
      ;; Protect questions (but not status). Insertion after the [
      ;; should be allowed, so make it rear-nonsticky. Insertion
      ;; before the ] should be allowed. Default is fine.
      (goto-char (point-min))
      (while (re-search-forward "\\(\n\\[\\)[DdTtWw ]\\(\\][^\n]*\n\\)" nil t)
        (add-text-properties (match-beginning 1) (match-end 1)
                             '(read-only t
                               intangible 2
                               front-sticky (read-only intangible)
                               rear-nonsticky (read-only intangible)))
        (add-text-properties (match-beginning 2) (match-end 2)
                             '(read-only t
                               intangible 1))
        )
      ;; Protect sections. Insertion before/after shouldn't be
      ;; allowed, so we make it front sticky and leave back sticky.
      (goto-char (point-min))
      (while (re-search-forward "^##[^\n]*\n" nil t)
        ;; If it doesn't occur on the first line, we want to protect
        ;; the \n, but we don't want to make it front-sticky.
        (when (not (eq (match-beginning 0) (point-min)))
          (add-text-properties (- (match-beginning 0) 1) (match-beginning 0)
                               '(read-only t
                                 intangible 2)))
        ;; The rest of the section text should be protected.
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(read-only t
                               intangible 2
                               front-sticky (read-only)))
        )
      ;; Protect question separators. Insertion after shouldn't be
      ;; allowed, so the default is fine. Insertion before should be
      ;; allowed, so the default is fine.
      (goto-char (point-min))
      (while (re-search-forward "\n===*\n" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(read-only t
                               intangible 2))
        )
      ;; Protect comment separators. Insertion after should be okay,
      ;; so nonsticky on rear. Insertion before shouldn't be, so
      ;; front-sticky there.
      (goto-char (point-min))
      (while (re-search-forward "\n---*\n" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(read-only t
                               intangible 1
                               front-sticky (read-only)
                               rear-nonsticky (read-only intangible)))
        )
      )
    )
  )

;;;###autoload
(define-derived-mode p-mode fundamental-mode "Purity"
  "Major mode for editing purity test files"
  (set-syntax-table p-mode-syntax-table)
  (use-local-map p-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(p-font-lock-patterns))
  (setq major-mode 'p-mode)
  (setq mode-name "Purity")
  (visual-line-mode)
  (p-mode-protect-questions)
  (run-hooks 'p-mode-hook)
  )

(provide 'p-mode)
