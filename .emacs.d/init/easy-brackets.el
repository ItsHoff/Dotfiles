;;; package --- Summary
;;; Commentary:
;;; Make brackets easier to use

;;; Code:
(defun my-no-dot (func)
  "Wrap evil-declare-not-repeat to return the FUNC for keybinding."
  (evil-declare-not-repeat func)
  func)

(defun my-indent-if-first ()
  "Indent the line if the point is at first character."
  (when (>= (+ (current-indentation) 1) (- (point) (point-at-bol)))
    (save-excursion
      (evil-indent-line (point-at-bol) (point-at-eol)))))

(general-imap "C-h" (lambda () (interactive)
                      (insert "{")
                      (my-indent-if-first))
              "C-l" (lambda () (interactive)
                      (insert "}")
                      (my-indent-if-first))
              "C-j" (kbd "[")
              "C-k" (kbd "]"))

(general-nvmap "f" (general-key-dispatch 'evil-find-char
                     "C-h" (my-no-dot (general-simulate-keys ('evil-find-char "{")))
                     "C-l" (my-no-dot (general-simulate-keys ('evil-find-char "}")))
                     "C-j" (my-no-dot (general-simulate-keys ('evil-find-char "[")))
                     "C-k" (my-no-dot (general-simulate-keys ('evil-find-char "]")))
                     )
              "F" (general-key-dispatch 'evil-find-char-backward
                    "C-h" (my-no-dot (general-simulate-keys ('evil-find-char-backward "{")))
                    "C-l" (my-no-dot (general-simulate-keys ('evil-find-char-backward "}")))
                    "C-j" (my-no-dot (general-simulate-keys ('evil-find-char-backward "[")))
                    "C-k" (my-no-dot (general-simulate-keys ('evil-find-char-backward "]")))
                    )
              "t" (general-key-dispatch 'evil-find-char-to
                    "C-h" (my-no-dot (general-simulate-keys ('evil-find-char-to "{")))
                    "C-l" (my-no-dot (general-simulate-keys ('evil-find-char-to "}")))
                    "C-j" (my-no-dot (general-simulate-keys ('evil-find-char-to "[")))
                    "C-k" (my-no-dot (general-simulate-keys ('evil-find-char-to "]")))
                    )
              "T" (general-key-dispatch 'evil-find-char-to-backward
                    "C-h" (my-no-dot (general-simulate-keys ('evil-find-char-to-backward "{")))
                    "C-l" (my-no-dot (general-simulate-keys ('evil-find-char-to-backward "}")))
                    "C-j" (my-no-dot (general-simulate-keys ('evil-find-char-to-backward "[")))
                    "C-k" (my-no-dot (general-simulate-keys ('evil-find-char-to-backward "]")))
                    )
              "r" (general-key-dispatch 'evil-replace
                    "C-h" (general-simulate-keys ('evil-replace "{"))
                    "C-l" (general-simulate-keys ('evil-replace "}"))
                    "C-j" (general-simulate-keys ('evil-replace "["))
                    "C-k" (general-simulate-keys ('evil-replace "]"))
                    ))

(general-nmap "d" (general-key-dispatch 'evil-delete
                    "f C-h" (general-simulate-keys ('evil-delete "f{"))
                    "f C-l" (general-simulate-keys ('evil-delete "f}"))
                    "f C-j" (general-simulate-keys ('evil-delete "f["))
                    "f C-k" (general-simulate-keys ('evil-delete "f]"))

                    "F C-h" (general-simulate-keys ('evil-delete "F{"))
                    "F C-l" (general-simulate-keys ('evil-delete "F}"))
                    "F C-j" (general-simulate-keys ('evil-delete "F["))
                    "F C-k" (general-simulate-keys ('evil-delete "F]"))

                    "t C-h" (general-simulate-keys ('evil-delete "t{"))
                    "t C-l" (general-simulate-keys ('evil-delete "t}"))
                    "t C-j" (general-simulate-keys ('evil-delete "t["))
                    "t C-k" (general-simulate-keys ('evil-delete "t]"))

                    "T C-h" (general-simulate-keys ('evil-delete "T{"))
                    "T C-l" (general-simulate-keys ('evil-delete "T}"))
                    "T C-j" (general-simulate-keys ('evil-delete "T["))
                    "T C-k" (general-simulate-keys ('evil-delete "T]"))

                    "i C-h" (general-simulate-keys ('evil-delete "i{"))
                    "i C-l" (general-simulate-keys ('evil-delete "i}"))
                    "i C-j" (general-simulate-keys ('evil-delete "i["))
                    "i C-k" (general-simulate-keys ('evil-delete "i]"))

                    "a C-h" (general-simulate-keys ('evil-delete "a{"))
                    "a C-l" (general-simulate-keys ('evil-delete "a}"))
                    "a C-j" (general-simulate-keys ('evil-delete "a["))
                    "a C-k" (general-simulate-keys ('evil-delete "a]"))
                    )

              "c" (general-key-dispatch 'evil-change
                    "f C-h" (general-simulate-keys ('evil-change "f{"))
                    "f C-l" (general-simulate-keys ('evil-change "f}"))
                    "f C-j" (general-simulate-keys ('evil-change "f["))
                    "f C-k" (general-simulate-keys ('evil-change "f]"))

                    "F C-h" (general-simulate-keys ('evil-change "F{"))
                    "F C-l" (general-simulate-keys ('evil-change "F}"))
                    "F C-j" (general-simulate-keys ('evil-change "F["))
                    "F C-k" (general-simulate-keys ('evil-change "F]"))

                    "t C-h" (general-simulate-keys ('evil-change "t{"))
                    "t C-l" (general-simulate-keys ('evil-change "t}"))
                    "t C-j" (general-simulate-keys ('evil-change "t["))
                    "t C-k" (general-simulate-keys ('evil-change "t]"))

                    "T C-h" (general-simulate-keys ('evil-change "T{"))
                    "T C-l" (general-simulate-keys ('evil-change "T}"))
                    "T C-j" (general-simulate-keys ('evil-change "T["))
                    "T C-k" (general-simulate-keys ('evil-change "T]"))

                    "i C-h" (general-simulate-keys ('evil-change "i{"))
                    "i C-l" (general-simulate-keys ('evil-change "i}"))
                    "i C-j" (general-simulate-keys ('evil-change "i["))
                    "i C-k" (general-simulate-keys ('evil-change "i]"))

                    "a C-h" (general-simulate-keys ('evil-change "a{"))
                    "a C-l" (general-simulate-keys ('evil-change "a}"))
                    "a C-j" (general-simulate-keys ('evil-change "a["))
                    "a C-k" (general-simulate-keys ('evil-change "a]"))
                    )

              "y" (general-key-dispatch 'evil-yank
                    "f C-h" (my-no-dot (general-simulate-keys ('evil-yank "f{")))
                    "f C-l" (my-no-dot (general-simulate-keys ('evil-yank "f}")))
                    "f C-j" (my-no-dot (general-simulate-keys ('evil-yank "f[")))
                    "f C-k" (my-no-dot (general-simulate-keys ('evil-yank "f]")))

                    "F C-h" (my-no-dot (general-simulate-keys ('evil-yank "F{")))
                    "F C-l" (my-no-dot (general-simulate-keys ('evil-yank "F}")))
                    "F C-j" (my-no-dot (general-simulate-keys ('evil-yank "F[")))
                    "F C-k" (my-no-dot (general-simulate-keys ('evil-yank "F]")))

                    "t C-h" (my-no-dot (general-simulate-keys ('evil-yank "t{")))
                    "t C-l" (my-no-dot (general-simulate-keys ('evil-yank "t}")))
                    "t C-j" (my-no-dot (general-simulate-keys ('evil-yank "t[")))
                    "t C-k" (my-no-dot (general-simulate-keys ('evil-yank "t]")))

                    "T C-h" (my-no-dot (general-simulate-keys ('evil-yank "T{")))
                    "T C-l" (my-no-dot (general-simulate-keys ('evil-yank "T}")))
                    "T C-j" (my-no-dot (general-simulate-keys ('evil-yank "T[")))
                    "T C-k" (my-no-dot (general-simulate-keys ('evil-yank "T]")))

                    "i C-h" (my-no-dot (general-simulate-keys ('evil-yank "i{")))
                    "i C-l" (my-no-dot (general-simulate-keys ('evil-yank "i}")))
                    "i C-j" (my-no-dot (general-simulate-keys ('evil-yank "i[")))
                    "i C-k" (my-no-dot (general-simulate-keys ('evil-yank "i]")))

                    "a C-h" (my-no-dot (general-simulate-keys ('evil-yank "a{")))
                    "a C-l" (my-no-dot (general-simulate-keys ('evil-yank "a}")))
                    "a C-j" (my-no-dot (general-simulate-keys ('evil-yank "a[")))
                    "a C-k" (my-no-dot (general-simulate-keys ('evil-yank "a]")))
                    ))

;; These need to be overriden in visual-map since normal-map also affects visual-state
(general-vmap "d" 'evil-delete
              "c" 'evil-change
              "y" 'evil-yank)

(provide 'easy-brackets)
;;; easy-brackets ends here
