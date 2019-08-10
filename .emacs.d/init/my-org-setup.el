;;; org-mode.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; My org-mode setup

;;; Code:
(use-package org
  :init
  (add-hook 'org-mode-hook (lambda () (my/set-tab-width 2)))
  :custom
  (org-adapt-indentation nil)
  :config
  (setq org-M-RET-may-split-line '(default . nil)) ; Don't split line automatically
  (evil-make-overriding-map org-mode-map 'motion)
  (dolist (cmd '(org-cycle org-shifttab org-ctrl-c-ctrl-c))
    (evil-declare-not-repeat cmd))
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

(use-package org-chef
  :config
  ; org-chef uses outdated workaround
  (setq org-chef-fetch-workaround
        (and
         ;; Note: For build sans gnutls, `libgnutls-version' is -1.
         (>= libgnutls-version 30603)
         (version<= emacs-version "26.2")
         t)))

(provide 'my-org-setup)
;;; org-mode.el ends here
