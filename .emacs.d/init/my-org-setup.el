;;; org-mode.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; My org-mode setup

;;; Code:
(defun my/org-insert-heading-above ()
  "Insert heading above the current one."
  (interactive)
  (outline-back-to-heading)
  (org-insert-heading)
  (evil-insert 1))

(defun my/org-insert-heading-below ()
  "Insert heading below the current section."
  (interactive)
  (end-of-line)
  (org-insert-heading '(4))
  (evil-insert 1))

(defun my/org-insert-item-above (&optional checkbox)
  "Insert item above the current item.
If CHECKBOX is non-nil, add a checkbox next to the bullet."
  (interactive "P")
  (beginning-of-line)
  (org-insert-item checkbox)
  (evil-insert 1))

(defun my/org-insert-item-below (&optional checkbox)
  "Insert item below the current item.
If CHECKBOX is non-nil, add a checkbox next to the bullet."
  (interactive "P")
  (end-of-line)
  (org-insert-item checkbox)
  (evil-insert 1))

(defun my/org-insert-checkbox-above ()
  "Insert checkbox above the current item."
  (interactive)
  (my/org-insert-item-above t)
  (evil-insert 1))

(defun my/org-insert-checkbox-below ()
  "Insert checkbox below the current item."
  (interactive)
  (my/org-insert-item-below t)
  (evil-insert 1))

(defun my/go-up-to-heading-level (target-level)
  "Go up headings until heading of at least TARGET-LEVEL is found."
  (outline-previous-heading)
  (let ((level (outline-level)))
    (while (and (> level target-level) (not (bobp)))
      (outline-previous-heading)
      (setq level (outline-level)))))

(defun my/go-down-to-heading-level (target-level)
  "Go up headings until heading of at least TARGET-LEVEL is found."
  (outline-next-heading)
  (while (and (> (outline-level) target-level) (not (eobp)))
    (outline-next-heading)))

(defun my/outline-up-heading ()
  "Go up to the parent heading.
If already at top heading go to the next heading above."
  (interactive)
  (if (not (outline-on-heading-p))
      (outline-back-to-heading)
    (let ((start-level (outline-level)) target-level)
      (if (> start-level 2)
          (setq target-level (- start-level 1))
        (setq target-level start-level))
      (my/go-up-to-heading-level target-level))))

(evil-declare-motion #'my/outline-up-heading)

(defun my/outline-down-heading ()
  "Go down to heading of higher level.
If already at top heading go to the next heading below.
Goto end if no lower higher level headings."
  (interactive)
  (if (not (outline-on-heading-p))
      (progn (outline-back-to-heading) (outline-next-visible-heading 1))
    (let ((start-level (outline-level)) target-level)
      (if (> start-level 2)
          (setq target-level (- start-level 1))
        (setq target-level start-level))
      (my/go-down-to-heading-level target-level))))

(evil-declare-motion #'my/outline-down-heading)

(defun my/org-backward-heading-same-level ()
  "Move backward to the previous subheading at same level as this one.
One hop over heading that is one level higher is allowed."
  (interactive)
  (if (not (outline-on-heading-p))
      (outline-back-to-heading)
    (let ((target-level (outline-level))
          (save-point (point)))
      (my/go-up-to-heading-level target-level)
      ;; Hop once more if we ended up on a heading that is one level higher.
      (when (= (outline-level) (1- target-level))
        (my/go-up-to-heading-level target-level))
      ;; If we didn't find appropriate heading stay at the original position.
      (if (/= (outline-level) target-level)
          (goto-char save-point)
        (org-reveal '(4))))))

(evil-declare-motion #'my/org-backward-heading-same-level)

(defun my/org-forward-heading-same-level ()
  "Move forward to the next subheading at same level as this one.
One hop over heading that is one level higher is allowed."
  (interactive)
  (let ((save-point (point)))
    (when (not (outline-on-heading-p))
      (outline-back-to-heading))
    (let ((target-level (outline-level)))
      (my/go-down-to-heading-level target-level)
      ;; Hop once more if we ended up on a heading that is one level higher.
      (when (= (outline-level) (1- target-level))
        (my/go-down-to-heading-level target-level))
      ;; If we didn't find appropriate heading stay at the original position.
      (if (/= (outline-level) target-level)
          (goto-char save-point)
        (org-reveal '(4))))))

(evil-declare-motion #'my/org-backward-heading-same-level)

(defvar my/org-property-list nil "List of properties to add with my/org-add-properties.")

(defun my/org-add-properties ()
  "Add properties listed in my/org-property-list to current entry."
  (interactive)
  (dolist (property my/org-property-list)
    (when (not (org-entry-get (point) property))
      (org-entry-put (point) property nil))))

(defun my/extract-package-name (url)
  "Extract package name from URL."
  (car (last (split-string url "/"))))

(use-package org
  :commands org-capture
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (my/set-tab-width 2)
              ;; This is needed for org-cycle to behave correctly
              (setq-local evil-move-beyond-eol t)
              (add-hook 'before-save-hook
                        (lambda () (org-update-statistics-cookies "All"))
                        nil "local")
              ;; Fix a problem with saveplace.el putting you back in a folded position.
              ;; This is done find-file-hook because saveplace uses find-file-hook
              ;; to set the position. org-mode-hook should be evaluated before find-file-hook.
              (add-hook 'find-file-hook
                        (lambda () (org-reveal '(4)))
                        "append" "local")))
  :custom
  (org-export-backends nil)
  (org-modules nil)
  (org-default-notes-file nil)
  (org-directory "~/Dropbox/notes")
  (org-startup-align-all-tables t)
  (org-startup-truncated nil)
  (org-startup-folded t)
  (org-adapt-indentation nil)
  (org-cycle-emulate-tab nil)
  (org-cycle-include-plain-lists nil)
  (org-insert-heading-respect-content nil)
  (org-M-RET-may-split-line '((default . nil)))     ; Don't split line automatically
  (org-show-context-detail '((default . lineage)))  ; Always show lineage
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  :config
  (dolist (cmd '(org-cycle org-shifttab org-ctrl-c-ctrl-c))
    (evil-declare-not-repeat cmd))
  (dolist (cmd '(outline-next-visible-heading
                 outline-previous-visible-heading
                 outline-forward-same-level
                 outline-backward-same-level))
    (evil-declare-motion cmd)
    (evil-declare-not-repeat cmd))
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (setq org-capture-templates
        '(("e" "Emacs todo" entry (file+olp "~/.emacs.d/docs/todo.org" "General")
           "** TODO %?")
          ("o" "Org-mode todo" entry (file+olp "~/.emacs.d/docs/todo.org" "Org-mode")
           "** TODO %?")
          ("p" "Package to check out" entry (file+olp "~/.emacs.d/docs/todo.org" "Packages")
           "** TODO %(my/extract-package-name \"%x\")\n- %x")
          ("t" "Personal todo" entry (file "~/Dropbox/notes/todo.org")
           "* TODO %?")
          ("r" "Recipe" entry (file+olp "~/Dropbox/notes/recipes.org" "Uncategorized")
           "%(org-chef-get-recipe-from-url)")
          ("R" "Recipe manually" entry (file+olp "~/Dropbox/notes/recipes.org" "Uncategorized")
           "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))
  (defhydra my/org-hydra (:hint nil)
    "
^Modify^             ^Tables^
_t_: Toggle TODO     _-_: Insert separator
_-_: Toggle bullet
"
    ("t" org-todo :exit nil)
    ("-" org-ctrl-c-minus :exit nil))
  :general
  (:keymaps 'org-mode-map
            :states '(normal visual)
            "; h" #'my/org-insert-heading-below
            "; H" #'my/org-insert-heading-above
            "; i" #'my/org-insert-item-below
            "; I" #'my/org-insert-item-above
            "; c" #'my/org-insert-checkbox-below
            "; C" #'my/org-insert-checkbox-above
            "; g" #'org-set-tags-command
            "; p" #'my/org-add-properties
            "; w" #'org-refile
            "; a" #'org-archive-subtree
            "; t" #'my/org-hydra/org-todo
            "; -" #'my/org-hydra/org-ctrl-c-minus
            "<down>" #'outline-next-visible-heading
            "<up>" #'outline-previous-visible-heading
            "<right>" #'my/org-forward-heading-same-level
            "<left>" #'my/org-backward-heading-same-level
            "C-M-p" #'my/outline-up-heading
            "C-M-n" #'my/outline-down-heading)
  (:keymaps 'org-mode-map
            :states '(normal visual insert)
            "M-h" #'org-metaleft
            "M-H" #'org-shiftmetaleft
            "M-l" #'org-metaright
            "M-L" #'org-shiftmetaright
            "M-j" #'org-metadown
            "M-J" #'org-shiftmetadown
            "M-k" #'org-metaup
            "M-K" #'org-shiftmetaup))

(use-package org-chef)

(provide 'my-org-setup)
;;; org-mode.el ends here
