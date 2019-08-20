;;; org-mode.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; My org-mode setup

;;; Code:
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
  (org-insert-heading-respect-content t)
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
  (defhydra my/org-hydra ()
    ("h" org-insert-heading "Insert heading")
    ("i" org-insert-item "Insert item")
    ;("c" c-u org-insert-item "Insert checkbox item")
    ("t" org-todo "Toggle TODO")
    ("g" org-set-tags-command "Set tags")
    ("p" org-priority "Set priority")
    ("w" org-refile "Refile")
    ("-" org-ctrl-c-minus "Modify bullet / Insert separator")
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
