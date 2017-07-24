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
    (if (<= save-point (point))
        (evil-beginning-of-visual-line))))

(evil-declare-not-repeat #'my-beginning-of-line)

(defun my-split-line ()
  "Split line at current cursor position."
  (interactive)
  (let ((save-point (point)))
    (newline-and-indent)
    (goto-char (- save-point 1))))

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

(defun my-visualize-undo (&optional arg)
  "Undo like undo-tree-visualize-undo but preserve time stamps.
A numeric ARG serves as the repeat count."
  (interactive "p")
  (let ((old (undo-tree-current buffer-undo-tree))
    current)
    ;; unhighlight old current node
    (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
      (inhibit-read-only t))
      (undo-tree-draw-node old))
    ;; undo in parent buffer
    (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
        ; Preserve timestamps
    (let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-undo-1 arg nil t))
      (setq current (undo-tree-current buffer-undo-tree))
      (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
      ;; when using lazy drawing, extend tree upwards as required
      (when undo-tree-visualizer-lazy-drawing
    (undo-tree-expand-up old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (undo-tree-draw-node current 'current))
      ;; update diff display, if any
      (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))))

(defun my-visualize-redo (&optional arg)
  "Redo like undo-tree-visualize-redo but preserve time stamps.
A numeric ARG serves as a repeat count."
  (interactive "p")
  (let ((old (undo-tree-current buffer-undo-tree))
    current)
    ;; unhighlight old current node
    (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
      (inhibit-read-only t))
      (undo-tree-draw-node (undo-tree-current buffer-undo-tree)))
    ;; redo in parent buffer
    (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
    (let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-redo-1 arg nil t))
      (setq current (undo-tree-current buffer-undo-tree))
      (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
      ;; when using lazy drawing, extend tree downwards as required
      (when undo-tree-visualizer-lazy-drawing
    (undo-tree-expand-down old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (undo-tree-draw-node current 'current))
      ;; update diff display, if any
      (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))))

(defun my-visualize-undo-to-x (&optional x)
  "Undo to last branch point, register, or saved state.
If X is the symbol `branch', undo to last branch point. If X is
the symbol `register', undo to last register. If X is the sumbol
`saved', undo to last saved state. If X is null, undo to first of
these that's encountered.

Interactively, a single \\[universal-argument] specifies
`branch', a double \\[universal-argument] \\[universal-argument]
specifies `saved', and a negative prefix argument specifies
`register'."
  (interactive "P")
  (when (and (called-interactively-p 'any) x)
    (setq x (prefix-numeric-value x)
	  x (cond
	     ((< x 0)  'register)
	     ((<= x 4) 'branch)
	     (t        'saved))))
  (let ((current (if undo-tree-visualizer-selection-mode
		     undo-tree-visualizer-selected-node
		   (undo-tree-current buffer-undo-tree)))
	(diff undo-tree-visualizer-diff)
	r)
    (undo-tree-visualizer-hide-diff)
    (while (and (undo-tree-node-previous current)
		(or (if undo-tree-visualizer-selection-mode
			(progn
			  (undo-tree-visualizer-select-previous)
			  (setq current undo-tree-visualizer-selected-node))
		      (my-visualize-undo)
		      (setq current (undo-tree-current buffer-undo-tree)))
		    t)
		         ;; branch point
		(not (or (and (or (null x) (eq x 'branch))
			      (> (undo-tree-num-branches) 1))
			 ;; register
			 (and (or (null x) (eq x 'register))
			      (setq r (undo-tree-node-register current))
			      (undo-tree-register-data-p
			       (setq r (registerv-data (get-register r))))
			      (eq current (undo-tree-register-data-node r)))
			 ;; saved state
			 (and (or (null x) (eq x 'saved))
			      (undo-tree-node-unmodified-p current))
			 ))))
    ;; update diff display, if any
    (when diff
      (undo-tree-visualizer-show-diff
       (when undo-tree-visualizer-selection-mode
	 undo-tree-visualizer-selected-node)))))

(defun my-visualize-redo-to-x (&optional x)
  "Redo to last branch point, register, or saved state.
If X is the symbol `branch', redo to last branch point. If X is
the symbol `register', redo to last register. If X is the sumbol
`saved', redo to last saved state. If X is null, redo to first of
these that's encountered.

Interactively, a single \\[universal-argument] specifies
`branch', a double \\[universal-argument] \\[universal-argument]
specifies `saved', and a negative prefix argument specifies
`register'."
  (interactive "P")
  (when (and (called-interactively-p 'any) x)
    (setq x (prefix-numeric-value x)
	  x (cond
	     ((< x 0)  'register)
	     ((<= x 4) 'branch)
	     (t        'saved))))
  (let ((current (if undo-tree-visualizer-selection-mode
		     undo-tree-visualizer-selected-node
		   (undo-tree-current buffer-undo-tree)))
	(diff undo-tree-visualizer-diff)
	r)
    (undo-tree-visualizer-hide-diff)
    (while (and (undo-tree-node-next current)
		(or (if undo-tree-visualizer-selection-mode
			(progn
			  (undo-tree-visualizer-select-next)
			  (setq current undo-tree-visualizer-selected-node))
		      (my-visualize-redo)
		      (setq current (undo-tree-current buffer-undo-tree)))
		    t)
		         ;; branch point
		(not (or (and (or (null x) (eq x 'branch))
			      (> (undo-tree-num-branches) 1))
			 ;; register
			 (and (or (null x) (eq x 'register))
			      (setq r (undo-tree-node-register current))
			      (undo-tree-register-data-p
			       (setq r (registerv-data (get-register r))))
			      (eq current (undo-tree-register-data-node r)))
			 ;; saved state
			 (and (or (null x) (eq x 'saved))
			      (undo-tree-node-unmodified-p current))
			 ))))
    ;; update diff display, if any
    (when diff
      (undo-tree-visualizer-show-diff
       (when undo-tree-visualizer-selection-mode
	 undo-tree-visualizer-selected-node)))))


(provide 'my-functions)
;;; my-functions.el ends here
