;;; my-functions --- my personal custom functions
;;; Commentary:

;;; Code:
(require 'evil)
(require 'org)

(defun my-beginning-of-line ()
  "Go to the first non blank char of line unless already at or in front of it.
In which case go to hard bol."
  (interactive)
  (let ((save-point (point)))
    (evil-first-non-blank-of-visual-line)
    (when (<= save-point (point))
        (evil-beginning-of-visual-line))))

(evil-declare-not-repeat #'my-beginning-of-line)

; https://stackoverflow.com/a/9597612
(defun my-end-of-line ()
  "Move to the last non-whitespace character in the current line.
If point is at or ahead of it move to last character."
  (interactive)
  (let ((save-point (point)))
    (move-end-of-line nil)
    (re-search-backward "^\\|[^[:space:]]")
    (when (>= save-point (point))
        (evil-end-of-line))))

(evil-declare-not-repeat #'my-end-of-line)

(defun my-split-line ()
  "Split line at current cursor position."
  (interactive)
  (let ((save-point (point)))
    (newline-and-indent)
    (goto-char (- save-point 1))))

; https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
(defun my-backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (let ((movement (% (current-column) tab-width))
        (p (point)))
    (when (= movement 0) (setq movement tab-width))
    ;; Account for edge case near beginning of buffer
    (setq movement (min (- p 1) movement))
    (save-match-data
      (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties
                                                (- p movement) p))
          (backward-delete-char (- (match-end 1) (match-beginning 1)))
        (call-interactively 'backward-delete-char)))))

(defun my-org-up-heading ()
  "Go up to the parent heading.
If already at top heading go to the next heading above."
  (interactive)
  (if (not (outline-on-heading-p))
      (outline-back-to-heading)
    (let ((start-level (outline-level)) target-level)
      (if (> start-level 2)
          (setq target-level (- start-level 1))
        (setq target-level start-level)
        (outline-previous-visible-heading 1))
      (let ((level (outline-level)))
        (while (and (> level target-level) (not (bobp)))
          (outline-previous-visible-heading 1)
          (setq level (outline-level))))
    )))

(defun my-org-down-heading ()
  "Go down to heading of higher level.
If already at top heading go to the next heading below.
Goto end if no lower higher level headings."
  (interactive)
  (if (not (outline-on-heading-p))
      (progn (outline-back-to-heading) (outline-next-visible-heading 1))
    (let ((start-level (outline-level)) target-level)
      (if (> start-level 2)
          (setq target-level (- start-level 1))
        (setq target-level start-level)
        (outline-next-visible-heading 1))
      (let ((level (outline-level)))
        (while (and (> level target-level) (not (eobp)))
          (outline-next-visible-heading 1)
          (setq level (outline-level))))
    )))

(defun my-advice-preserve-timestamps (args)
  "Change preserve-timestamps in ARGS to t.
Filters arguments for undo-tree-undo-1 and undo-tree-redo-1."
  (list (nth 0 args) (nth 1 args) t))

(provide 'my-functions)
;;; my-functions.el ends here
