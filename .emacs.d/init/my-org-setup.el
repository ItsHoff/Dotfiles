;;; org-mode.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; My org-mode setup

;;; Code:
(use-package org
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
  (org-adapt-indentation nil)
  :config
  (setq org-M-RET-may-split-line '(default . nil)) ; Don't split line automatically
  ; 10.8.19
  ;; (evil-make-overriding-map org-mode-map 'motion)
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

  (use-package org-chef
    :config
    ; org-chef uses outdated workaround
    (setq org-chef-fetch-workaround
          (and
           ;; Note: For build sans gnutls, `libgnutls-version' is -1.
           (>= libgnutls-version 30603)
           (version<= emacs-version "26.2")
           t)))

  :general
  (:keymaps 'org-mode-map
            :states '(normal visual)
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
