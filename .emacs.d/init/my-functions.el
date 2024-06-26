;;; my-functions --- my personal custom functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/test-function ()
  "Function for testing things."
  (interactive)
  (prin1 (outline-level) t))

(defun my/lsp-test ()
  (lsp-request-async "textDocument/documentSymbol"
                     `(:textDocument ,(lsp--text-document-identifier))
                     (lambda (document-symbols)
                       (mapcar (lambda (item)
                                 (when (not (or (= (gethash "kind" item) 13)    ; variable
                                                (= (gethash "kind" item) 8)     ; field
                                                (= (gethash "kind" item) 22)    ; enum member
                                                (= (gethash "kind" item) 26)))  ; type parameter
                                   (prin1 "\n name:")
                                   (prin1 (gethash "name" item))
                                   (prin1 "\n kind:")
                                   (prin1 (gethash "kind" item))
                                   (prin1 "\n children:")
                                   (prin1 (gethash "children" item))
                                   (prin1 "\n containerName:")
                                   (prin1 (gethash "containerName" item))
                                   (prin1 "\n range:")
                                   (ignore-errors
                                     (let* ((location (gethash "location" item))
                                            (range (gethash "range" location)))
                                       (prin1 (gethash "line" (gethash "start" range)))
                                       (prin1 "-")
                                       (prin1 (gethash "line" (gethash "end" range)))))
                                   (print "")))
                               document-symbols))
                     :mode 'alive))

(defun my/beginning-of-line ()
  "First go to beginning of visual line.
Then to the beginning of line and finally
to the hard beginning of line."
  (interactive)
  (let ((save-point (point)))
    (evil-first-non-blank-of-visual-line)
    (when (<= save-point (point))
      (evil-beginning-of-visual-line))
    (when (<= save-point (point))
      (evil-beginning-of-line))))

(evil-declare-motion #'my/beginning-of-line)

;; https://stackoverflow.com/a/9597612
(defun my/end-of-line ()
  "Move to the last non-whitespace character in the current line.
If point is at or ahead of it move to last character."
  (interactive)
  (let ((save-point (point)))
    (if truncate-lines
        (evil-end-of-line)
      (evil-end-of-visual-line))
    (re-search-backward "^\\|[^[:space:]]")
    ;; Visual mode seems to handle point differently
    ;; so move forward one character to keep things consistent.
    (when (eq evil-state 'visual)
      (forward-char))
    (when (>= save-point (point))
      (evil-end-of-line))))

(evil-declare-motion #'my/end-of-line)

(defun my/split-line ()
  "Split line at current cursor position."
  (interactive)
  (let ((save-point (point)))
    (newline-and-indent)
    (goto-char (- save-point 1))))

;; https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
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

;; This uses defvar + setq so that possible changes do not require a restart.
(defvar my/extra-buffer-regexps nil "Regexps for buffers that `my/close-extra-buffers' should also quit.")
(setq my/extra-buffer-regexps '("^magit.+"
                                "^\*.+\*$"))

;; TODO: Add white list for *x* buffers that should not be closed (if there ever comes a need)
(defun my/close-extra-buffers ()
  "Close all non-selected buffers whose name match `my/extra-buffer-regexps'."
  (interactive)
  (walk-windows (lambda (window)
                  (when (not (eq window (selected-window)))
                    (let (done)
                      (while (not done) ; Loop until there is nothing to close
                        (setq done t)
                        (let* ((buffer (window-buffer window))
                               (name (buffer-name buffer)))
                          (dolist (regexp my/extra-buffer-regexps)
                            (when (string-match-p regexp name)
                              (quit-windows-on buffer)
                              ;; Closing a buffer might reveal another buffer we want to close,
                              ;; so we need to check again.
                              (setq done nil))))))))))

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
        (t (message "Last command was not paste or repeat"))))

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
        (t (message "Last command was not paste or repeat"))))

;; https://stackoverflow.com/questions/23659909/reverse-evaluation-order-of-split-height-threshold-and-split-width-threshold-in
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

;; https://github.com/bbatsov/solarized-emacs/issues/390#issuecomment-576626439
(defun my/disable-all-themes()
  "Disable all currently active themes."
  (interactive)
  (dolist (v custom-enabled-themes)
    (disable-theme v)))

(provide 'my-functions)
;;; my-functions.el ends here
