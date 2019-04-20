;;; package --- Summary
;;; Commentary:
;;; Make brackets easier to use

;;; Code:
(require 'evil)
(require 'general)

(defun my/no-dot (func)
  "Wrap evil-declare-not-repeat to return the FUNC for keybinding."
  (evil-declare-not-repeat func)
  func)

(defun my/indent-if-first ()
  "Indent the line if the point is at first character."
  (when (>= (+ (current-indentation) 1) (- (point) (point-at-bol)))
    (save-excursion
      (evil-indent-line (point-at-bol) (point-at-eol)))))

(general-imap "C-h" (lambda () (interactive)
                      (insert "{")
                      (my/indent-if-first))
              "C-l" (lambda () (interactive)
                      (insert "}")
                      (my/indent-if-first))
              "C-j" (kbd "[")
              "C-k" (kbd "]"))

(general-nvmap "r" (general-key-dispatch 'evil-replace
                    "C-h" (general-simulate-key ('evil-replace "{"))
                    "C-l" (general-simulate-key ('evil-replace "}"))
                    "C-j" (general-simulate-key ('evil-replace "["))
                    "C-k" (general-simulate-key ('evil-replace "]"))
                    ))

(defun my/advice-easy-brackets (args)
  "Filter ARGS such that C-(h j k l) get translated to { [ ] }."
  (let ((char (nth 1 args)))
    (message "hello")
    (message "%s" (char-to-string char))
    (cond ((= char ?\C-h)
           (list (nth 0 args) ?\{))
          ((= char ?\C-j)
           (list (nth 0 args) ?\[))
          ((= char ?\C-k)
           (list (nth 0 args) ?\]))
          ((= char ?\C-l)
           (list (nth 0 args) ?\}))
          (t args))
    ))

(advice-add #'evil-find-char :filter-args #'my/advice-easy-brackets)

(general-define-key :keymaps 'evil-inner-text-objects-map
                    "C-h" #'evil-inner-curly
                    "C-l" #'evil-inner-curly
                    "C-j" #'evil-inner-bracket
                    "C-k" #'evil-inner-bracket
                    )
(general-define-key :keymaps 'evil-outer-text-objects-map
                    "C-h" #'evil-a-curly
                    "C-l" #'evil-a-curly
                    "C-j" #'evil-a-bracket
                    "C-k" #'evil-a-bracket
                    )
(general-define-key :keymaps 'evil-read-key-map
                    "C-k" nil) ; Conflicts with easy-brackets

(provide 'easy-brackets)
;;; easy-brackets ends here
