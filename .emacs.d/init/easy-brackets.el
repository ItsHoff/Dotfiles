;; Easier brackets
(general-imap "C-h" (kbd "{")
              "C-l" (lambda () (interactive)
                      (insert "}")
                      (save-excursion ; Indent aswell
                        (evil-indent-line (point-at-bol) (point-at-eol))))
              "C-j" (kbd "[")
              "C-k" (kbd "]"))

(general-nvmap "f" (general-key-dispatch 'evil-find-char
                     "C-h" (general-simulate-keys ('evil-find-char "{"))
                     "C-l" (general-simulate-keys ('evil-find-char "}"))
                     "C-j" (general-simulate-keys ('evil-find-char "["))
                     "C-k" (general-simulate-keys ('evil-find-char "]"))
                     )
              "F" (general-key-dispatch 'evil-find-char-backward
                    "C-h" (general-simulate-keys ('evil-find-char-backward "{"))
                    "C-l" (general-simulate-keys ('evil-find-char-backward "}"))
                    "C-j" (general-simulate-keys ('evil-find-char-backward "["))
                    "C-k" (general-simulate-keys ('evil-find-char-backward "]"))
                    )
              "t" (general-key-dispatch 'evil-find-char-to
                    "C-h" (general-simulate-keys ('evil-find-char-to "{"))
                    "C-l" (general-simulate-keys ('evil-find-char-to "}"))
                    "C-j" (general-simulate-keys ('evil-find-char-to "["))
                    "C-k" (general-simulate-keys ('evil-find-char-to "]"))
                    )
              "T" (general-key-dispatch 'evil-find-char-to-backward
                    "C-h" (general-simulate-keys ('evil-find-char-to-backward "{"))
                    "C-l" (general-simulate-keys ('evil-find-char-to-backward "}"))
                    "C-j" (general-simulate-keys ('evil-find-char-to-backward "["))
                    "C-k" (general-simulate-keys ('evil-find-char-to-backward "]"))
                    )
              "r" (general-key-dispatch 'evil-replace
                    "C-h" (general-simulate-keys ('evil-replace "{"))
                    "C-l" (general-simulate-keys ('evil-replace "}"))
                    "C-j" (general-simulate-keys ('evil-replace "["))
                    "C-k" (general-simulate-keys ('evil-replace "]"))
                    ))

(general-nmap
                    "d" (general-key-dispatch 'evil-delete
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
                          )

                    "y" (general-key-dispatch 'evil-yank
                          "f C-h" (general-simulate-keys ('evil-yank "f{"))
                          "f C-l" (general-simulate-keys ('evil-yank "f}"))
                          "f C-j" (general-simulate-keys ('evil-yank "f["))
                          "f C-k" (general-simulate-keys ('evil-yank "f]"))

                          "F C-h" (general-simulate-keys ('evil-yank "F{"))
                          "F C-l" (general-simulate-keys ('evil-yank "F}"))
                          "F C-j" (general-simulate-keys ('evil-yank "F["))
                          "F C-k" (general-simulate-keys ('evil-yank "F]"))

                          "t C-h" (general-simulate-keys ('evil-yank "t{"))
                          "t C-l" (general-simulate-keys ('evil-yank "t}"))
                          "t C-j" (general-simulate-keys ('evil-yank "t["))
                          "t C-k" (general-simulate-keys ('evil-yank "t]"))

                          "T C-h" (general-simulate-keys ('evil-yank "T{"))
                          "T C-l" (general-simulate-keys ('evil-yank "T}"))
                          "T C-j" (general-simulate-keys ('evil-yank "T["))
                          "T C-k" (general-simulate-keys ('evil-yank "T]"))
                          ))

;; For some reason these need to be explicitly added to visual map
(general-vmap "d" 'evil-delete
              "c" 'evil-change
              "y" 'evil-yank)

(provide 'easy-brackets)
;;; easy-brackets ends here
