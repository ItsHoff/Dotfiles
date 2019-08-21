;;; org-mode.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; My org-mode setup

;;; Code:
(defun my/org-insert-heading-above ()
  "Insert heading above the current one."
  (interactive)
  (outline-back-to-heading)
  (org-insert-heading))

(defun my/org-insert-heading-below ()
  "Insert heading below the current section."
  (interactive)
  (end-of-line)
  (org-insert-heading '(4)))

(defun my/org-insert-item-above (&optional checkbox)
  "Insert item above the current item.
If CHECKBOX is non-nil, add a checkbox next to the bullet."
  (interactive "P")
  (beginning-of-line)
  (org-insert-item checkbox))

(defun my/org-insert-item-below (&optional checkbox)
  "Insert item below the current item.
If CHECKBOX is non-nil, add a checkbox next to the bullet."
  (interactive "P")
  (end-of-line)
  (org-insert-item checkbox))

(defun my/org-insert-checkbox-above ()
  "Insert checkbox above the current item."
  (interactive)
  (my/org-insert-item-above t))

(defun my/org-insert-checkbox-below ()
  "Insert checkbox below the current item."
  (interactive)
  (my/org-insert-item-below t))

(use-package org
  :ensure org-plus-contrib
  :commands org-capture
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (my/set-tab-width 2)
              (add-hook 'before-save-hook
                        (lambda () (org-update-statistics-cookies "All"))
                        nil "local")
              ))
  :custom
  (org-export-backends nil)
  (org-modules nil)
  (org-default-notes-file nil)
  (org-directory "~/Dropbox/notes")
  (org-startup-align-all-tables t)
  (org-startup-truncated nil)
  (org-adapt-indentation nil)
  (org-cycle-emulate-tab nil)
  (org-cycle-include-plain-lists nil)
  (org-insert-heading-respect-content nil)
  (org-M-RET-may-split-line '((default . nil))) ; Don't split line automatically
  :config
  (dolist (cmd '(org-cycle org-shifttab org-ctrl-c-ctrl-c))
    (evil-declare-not-repeat cmd))
  ; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (setq org-capture-templates
        '(("e" "Emacs todo" entry (file+olp "~/.emacs.d/docs/todo.org" "General")
           "** TODO %?")
          ("o" "Org-mode todo" entry (file+olp "~/.emacs.d/docs/todo.org" "Org-mode")
           "** TODO %?")
          ("p" "Package to check out" entry (file+olp "~/.emacs.d/docs/todo.org" "Packages")
           "** TODO %?\n- %x")
          ("t" "Personal todo" entry (file "~/Dropbox/notes/todo.org")
           "* TODO %?")
          ("r" "Recipe" entry (file+olp "~/Dropbox/notes/recipes.org" "Uncategorized")
           "%(org-chef-get-recipe-from-url)")
          ("R" "Recipe manually" entry (file+olp "~/Dropbox/notes/recipes.org" "Uncategorized")
           "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
          ))
  (use-package org-chef)
  (defhydra my/org-hydra (:exit t :hint nil)
    "
^Insert^               ^Modify^             ^Tables^
_h_: Heading below     _t_: Toggle TODO     _-_: Insert separator
_H_: Heading above     _g_: Set tags
_i_: Item below        _p_: Set priority
_I_: Item above        _w_: Refile
_c_: Checkbox below    _-_: Toggle bullet
_C_: Checkbox above
"
    ("h" my/org-insert-heading-below)
    ("H" my/org-insert-heading-above)
    ("i" my/org-insert-item-below)
    ("I" my/org-insert-item-above)
    ("c" my/org-insert-checkbox-below)
    ("C" my/org-insert-checkbox-above)
    ("t" org-todo :exit nil)
    ("g" org-set-tags-command)
    ("p" org-priority)
    ("w" org-refile)
    ("-" org-ctrl-c-minus :exit nil)
    )
  :general
  (:keymaps 'org-mode-map
            :states '(normal visual)
            ";" #'my/org-hydra/body
            "<down>" #'outline-next-visible-heading
            "<up>" #'outline-previous-visible-heading
            "<right>" #'org-forward-heading-same-level
            "<left>" #'org-backward-heading-same-level
            "M-h" #'org-metaleft
            "M-l" #'org-metaright
            "M-j" #'org-metadown
            "M-k" #'org-metaup
            ))

(provide 'my-org-setup)
;;; org-mode.el ends here
