;;; bind --- bind all my keys
;;; Commentary:
;;; My keybindings

;;; Code:
(require 'general)
(require 'evil)
(require 'my-functions)

;; UNBIND --------------------------------------------------------------------------------------

(general-define-key
 "C-<backspace>" nil ; Mistyped often with i-mode brackets
 )
(general-define-key :keymaps 'evil-read-key-map
                    "C-k" nil) ; Conflicts with easy-brackets

;; MOUSE----------------------------------------------------------------------------------------

(general-define-key :keymaps '(global-map evil-motion-state-map)
                    "<mouse-1>" #'mouse-select-window
                    "<drag-mouse-1>" nil ; Causes annoying accidental visual modes when clicking
                    "<down-mouse-1>" nil)

(general-define-key :keymaps 'button-map
                    "<mouse-1>" #'push-button
                    )

(general-define-key :keymaps 'org-mouse-map
                    "<mouse-1>" #'org-open-at-mouse)

;; OVERRIDES -----------------------------------------------------------------------------------
;; Bindings that should override any mode specific bindings

; Everywhere
(general-define-key :keymaps 'override
                    :states  '(motion normal visual emacs insert)
                    "M-+" help-map  ; Remap help
                    "M-<dead-acute>" #'describe-key  ; Map key help (next to +)
                    "M-/" help-map  ; Remap help for ansi
                    "M-;" #'describe-key  ; Map key help (next to +)
                    )

; Space prefix
(general-define-key :keymaps 'override
                    :states '(motion normal visual emacs)
                    :prefix "SPC"
                    "SPC" #'counsel-M-x
                    "g" #'magit-status
                    "p" #'projectile-command-map
                    "b" #'ivy-bibtex
                    "h" #'my/helm-command-map
                    "s" #'my/framegroups-command-map
                    "y" #'my/yas-command-map
                    "d" #'dired-jump
                    "o" #'olivetti-mode
                    )

; Non-insert + emacs (for special emacs state buffers)
(general-define-key :keymaps 'override
                    :states  '(motion normal visual emacs)
                    "C-M-S-s-t" #'my/test-function
                    "M-u" #'universal-argument
                    "M-g" #'keyboard-quit
                    "C-z" #'suspend-emacs
                    "C-q" #'my/quit-extra-windows
                    "C-h" #'evil-window-left
                    "C-j" #'evil-window-down
                    "C-k" #'evil-window-up
                    "C-l" #'evil-window-right
                    "C-f" #'counsel-find-file
                    "C-b" #'ivy-switch-buffer
                    "M-x" #'counsel-M-x
                    )


; Vim movement states
(general-define-key :keymaps 'override
                    :states '(motion normal visual)
                    ; Bigger movement
                    "H" #'my/beginning-of-line
                    "L" #'my/end-of-line
                    "J" #'golden-ratio-scroll-screen-up
                    "K" #'golden-ratio-scroll-screen-down
                    )

;; REGULAR BINDINGS -----------------------------------------------------------------------------

; Emacs state
(general-define-key :keymaps 'emacs
                    "M-z" #'evil-exit-emacs-state
                    )

; Motions (normal, visual and some special buffers)
(general-define-key :keymaps '(motion normal visual)
                    "<C-i>" #'evil-jump-forward
                    "<C-m>" #'helm-mini
                    "TAB" #'evil-toggle-fold
                    ; Move emacs state
                    "M-z" #'evil-emacs-state
                    ; Make j and k move visual lines
                    "j" #'evil-next-visual-line
                    "k" #'evil-previous-visual-line
                    ; _;_ is leader so use ' instead
                    "'" #'evil-repeat-find-char
                    )

(general-define-key :keymaps '(normal visual)
                    "; c" #'evilnc-comment-or-uncomment-lines
                    ; Clipboard paste and yank
                    "C-M-p" #'my/paste-clipboard-after
                    "C-M-S-p" #'my/paste-clipboard-before
                    "C-M-y" #'my/yank-clipboard
                    "C-M-S-y" #'my/yank-line-clipboard
                    )

(defhydra my/goto-change-hydra ()
  "Go to change"
  (";" goto-last-change "Backwards")
  ("'" goto-last-change-reverse "Forwards"))

(general-define-key :keymaps 'normal
                    "C-u" #'undo-tree-visualize
                    "; i" #'evil-numbers/inc-at-pt
                    "; d" #'evil-numbers/dec-at-pt
                    "g ;" #'my/goto-change-hydra/goto-last-change
                    "g '" #'my/goto-change-hydra/goto-last-change-reverse
                    ; Join + split
                    "<backspace>" #'evil-join
                    "RET" #'my/split-line
                    )

; Insert state
(general-define-key :keymaps 'insert
                    "C-M-y" #'yas-insert-snippet
                    "<tab>" #'tab-to-tab-stop
                    "<backspace>" #'my/backspace-whitespace-to-tab-stop
                    "<return>" #'newline-and-indent
                    "<C-i>" #'indent-according-to-mode
                    )


(load "easy-brackets")

(provide 'bind)
;;; bind.el ends here
