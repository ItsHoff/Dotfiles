;;; my-functions --- my personal custom functions
;;; Commentary:

;;; Code:
(require 'evil)
(require 'org)

(defun my/test-function ()
  "Function for testing things."
  (interactive)
  (message "test")
  (message (auto-revert-active-p)))

(defun my/set-tab-width (width)
  "Set 'tab-width' to WIDTH."
  (setq tab-width width)
  (setq evil-shift-width width))

(defun my/beginning-of-line ()
  "First go to beginning of visual line.
Then to the beginning of line and finally
to the hard beginning of line."
  (interactive)
  (let ((save-point (point)))
    (evil-first-non-blank-of-visual-line)
    (when (<= save-point (point))
        (evil-first-non-blank))
    (when (<= save-point (point))
        (evil-beginning-of-line))))

(evil-declare-not-repeat #'my/beginning-of-line)

; https://stackoverflow.com/a/9597612
(defun my/end-of-line ()
  "Move to the last non-whitespace character in the current line.
If point is at or ahead of it move to last character."
  (interactive)
  (let ((save-point (point)))
    (if truncate-lines
        (evil-end-of-line)
      (evil-end-of-visual-line))
    (re-search-backward "^\\|[^[:space:]]")
    (when (>= save-point (point))
        (evil-end-of-line))))

(evil-declare-not-repeat #'my/end-of-line)

(defun my/split-line ()
  "Split line at current cursor position."
  (interactive)
  (let ((save-point (point)))
    (newline-and-indent)
    (goto-char (- save-point 1))))

; https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
(defun my/backspace-whitespace-to-tab-stop ()
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

; Use define and set so the possible changes can be applied with eval-buffer
; and not require a restart
(defvar my/extra-window-names nil "Names of buffers that `my/quit-extra-windows' should quit.")
(setq my/extra-window-names '(;; Ubiquitous help buffers
                              "*Help*"
                              "*Apropos*"
                              "*Messages*"
                              "*Completions*"
                              "*Warnings*"
                              ;; Other general buffers
                              "*Command History*"
                              "*Compile-Log*"
                              "*TeX Help*"
                              "*grep*"
                              "*compilation*"
                              "*disabled command*"
                              "*Cargo Clippy*"
                              "*xref*"
                              "*ripgrep-search*"
                              ))

(defvar my/extra-window-regexps nil "Regexps for buffers that 'my/quit-extra-windows' should also quit.")
(setq my/extra-window-regexps '("^magit.+"))

(defun my/quit-extra-windows (&optional kill frame)
  "Quit all windows with help-like buffers.

Call `quit-windows-on' for every buffer named in
`my/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (walk-windows (lambda (window)
                  (let* ((buffer (window-buffer window))
                         (name (buffer-name buffer)))
                    (dolist (test my/extra-window-names)
                      (when (string= test name)
                        (quit-windows-on buffer)))
                    (dolist (regexp my/extra-window-regexps)
                      (when (string-match-p regexp name)
                        (quit-windows-on buffer)))))))


(evil-define-command my/paste-and-repeat-pop (count &optional save-point)
  "Select paste or repeat pop depending on last command and do COUNT times."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not evil-repeat-move-cursor)))
  (cond ((and (memq last-command '(evil-paste-after
                              evil-paste-before
                              evil-visual-paste))
             evil-last-paste)
         (evil-paste-pop count))
        ((and (eq last-command #'evil-repeat)
              evil-last-repeat)
         (evil-repeat-pop count save-point))
        (t (message "Last command was not paste or repeat"))
        ))

(evil-define-command my/paste-and-repeat-pop-next (count &optional save-point)
  "Select paste or repeat pop next depending on last command and do COUNT times."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not evil-repeat-move-cursor)))
  (cond ((and (memq last-command '(evil-paste-after
                              evil-paste-before
                              evil-visual-paste))
             evil-last-paste)
         (evil-paste-pop-next count))
        ((and (eq last-command #'evil-repeat)
              evil-last-repeat)
         (evil-repeat-pop-next count save-point))
        (t (message "Last command was not paste or repeat"))
        ))

; https://stackoverflow.com/questions/23659909/reverse-evaluation-order-of-split-height-threshold-and-split-width-threshold-in
(defun my/split-window-sensibly (&optional window)
  "Reverse the WINDOW splitting order from default.
First try vertical split and only then horizontal split"
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(defun my/split-only-root (&optional window)
  "Split WINDOW only if there are no existing splits.
Perform the split along the longest axis."
  (when (one-window-p t)
    (if (> (window-pixel-height) (window-pixel-width))
        (with-selected-window window
          (split-window-below))
      (with-selected-window window
        (split-window-right)))))


(evil-define-command my/paste-clipboard-before (count)
  "Pastes the clipboard before point."
  :suppress-operator t
  (interactive "P")
  (evil-paste-before count ?+))

(evil-define-command my/paste-clipboard-after (count)
  "Pastes the clipboard behind point."
  :suppress-operator t
  (interactive "P")
  (evil-paste-after count ?+))

(evil-define-operator my/yank-clipboard (beg end type)
  "Yank to the clipboard."
  :move-point nil
  :repeat nil
  (interactive "<R>")
  (evil-yank beg end type ?+ nil))

(evil-define-operator my/yank-line-clipboard (beg end type)
  "Yank line to the clipboard."
  :motion evil-line
  :move-point nil
  :repeat nil
  (interactive "<R>")
  (evil-yank-line beg end type ?+))

(defun my/org-up-heading ()
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

(evil-declare-not-repeat #'my/org-up-heading)

(defun my/org-down-heading ()
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

(evil-declare-not-repeat #'my/org-down-heading)

(defun my/org-indent-advice ()
  "Align indent with 'tab-width'.
Advice type: after."
  (backward-delete-char (% (current-column) tab-width)))

(defun my/advice-preserve-timestamps (args)
  "Change preserve-timestamps in ARGS to t.
Filters arguments for undo-tree-undo-1 and undo-tree-redo-1.
Advice type: filter-args."
  (list (nth 0 args) (nth 1 args) t))

(provide 'my-functions)
;;; my-functions.el ends here
