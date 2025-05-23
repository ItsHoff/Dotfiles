;;; bindings --- general keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; All non-package-specific keybinds.

;;; Code:
(require 'general)
(require 'evil)
(require 'my-functions)

;;; UNBIND --------------------------------------------------------------------------------------

;;; MOUSE----------------------------------------------------------------------------------------

(general-define-key :keymaps '(global-map evil-motion-state-map)
                    "<mouse-1>" #'mouse-select-window
                    "<drag-mouse-1>" nil ; Causes annoying accidental visual modes when clicking
                    "<down-mouse-1>" nil
                    "<mouse-3>" nil)

(general-define-key :keymaps 'button-map
                    "<mouse-1>" #'push-button)

(general-define-key :keymaps 'org-mouse-map
                    "<mouse-1>" #'org-open-at-mouse)

;;; OVERRIDES -----------------------------------------------------------------------------------
;;; Bindings that should override any mode specific bindings

;; Everywhere
(general-define-key :keymaps 'override
                    :states  '(motion normal visual emacs insert)
                    "C-." #'embark-act
                    "M-+" help-map                    ; Remap help
                    "M-<dead-acute>" #'describe-key   ; Map key help (next to +)
                    "M-/" help-map                    ; Remap help for ansi
                    "M-;" #'describe-key)             ; Map key help

;; Space prefix
(general-define-key :keymaps 'override
                    :states '(motion normal visual emacs)
                    :prefix "SPC"
                    "SPC" #'execute-extended-command
                    "b" #'magit-blame-addition
                    "c" #'org-capture
                    "d" #'dired-jump
                    "f" #'flycheck-list-errors
                    "g" #'magit-status
                    "G" #'magit-dispatch
                    "o" #'olivetti-mode
                    "p" #'projectile-command-map
                    "r" #'vertico-repeat
                    "s" #'my/framegroups-command-map
                    "y" #'my/yas-command-map)

;; Non-insert + emacs (for special emacs state buffers)
(general-define-key :keymaps 'override
                    :states  '(motion normal visual emacs)
                    "M-T" #'my/test-function
                    "M-u" #'universal-argument
                    "M-g" #'keyboard-quit
                    "C-z" #'suspend-emacs
                    "C-q" #'my/close-extra-buffers
                    ;; Use easier binds for window moves
                    "C-h" #'evil-window-left
                    "C-j" #'evil-window-down
                    "C-k" #'evil-window-up
                    "C-l" #'evil-window-right
                    ;; Window binds were freed above
                    "C-w h" #'buf-move-left
                    "C-w j" #'buf-move-down
                    "C-w k" #'buf-move-up
                    "C-w l" #'buf-move-right
                    "C-f" #'find-file
                    "C-b" #'consult-buffer)

;; Vim movement states
(general-define-key :keymaps 'override
                    :states '(motion normal visual)
                    ;; Bigger movement
                    "H" #'my/beginning-of-line
                    "L" #'evil-end-of-visual-line
                    "J" #'golden-ratio-scroll-screen-up
                    "K" #'golden-ratio-scroll-screen-down)

;;; REGULAR BINDINGS -----------------------------------------------------------------------------

;; Emacs state
(general-define-key :keymaps 'emacs
                    "M-z" #'evil-exit-emacs-state)

;; Motions (normal, visual and some special buffers)
(general-define-key :keymaps '(motion normal visual)
                    "<C-i>" #'evil-jump-forward
                    "TAB" #'evil-toggle-fold
                    "z j" #'evil-window-bottom
                    "z k" #'evil-window-top
                    ;; Move emacs state
                    "M-z" #'evil-emacs-state
                    ;; Make j and k move visual lines
                    "j" #'evil-next-visual-line
                    "k" #'evil-previous-visual-line
                    ;; _;_ is leader so use ' instead
                    "'" #'evil-repeat-find-char)

(general-define-key :keymaps '(normal visual)
                    "; c" #'evilnc-comment-or-uncomment-lines
                    "; i" #'evil-numbers/inc-at-pt
                    "; d" #'evil-numbers/dec-at-pt
                    "; r" #'lsp-rename
                    ;; Clipboard paste and yank
                    "M-Y" #'my/yank-clipboard)

(general-define-key :keymaps '(normal visual insert minibuffer-local-map ivy-minibuffer-map)
                    "M-P" #'my/paste-clipboard-after)

(defhydra my/goto-change-hydra ()
  "Go to change"
  (";" goto-last-change "Backwards")
  ("'" goto-last-change-reverse "Forwards"))

(defhydra my/cycle-paste-hydra ()
  ("p" evil-paste-after "Paste after")
  ("P" evil-paste-before "Paste before")
  (";" evil-paste-pop "Previous paste")
  ("'" evil-paste-pop-next "Next paste"))

(defhydra my/cycle-repeat-hydra ()
  ("." evil-repeat "Repeat")
  (";" evil-repeat-pop "Previous repeat")
  ("'" evil-repeat-pop-next "Next repeat"))

(general-define-key :keymaps 'normal
                    "." #'my/cycle-repeat-hydra/evil-repeat
                    "p" #'my/cycle-paste-hydra/evil-paste-after
                    "P" #'my/cycle-paste-hydra/evil-paste-before
                    "C-u" #'undo-tree-visualize
                    "g ;" #'my/goto-change-hydra/goto-last-change
                    "g '" #'my/goto-change-hydra/goto-last-change-reverse
                    ;; Join + split
                    "<backspace>" #'evil-join
                    "RET" #'my/split-line)

;; Insert state
(general-define-key :keymaps 'insert
                    "C-M-y" #'ivy-yasnippet
                    "<tab>" #'tab-to-tab-stop
                    "<backspace>" #'my/backspace-whitespace-to-tab-stop
                    "<return>" #'newline-and-indent
                    "<C-i>" #'indent-according-to-mode)

(require 'easy-brackets)

(provide 'my-keybindings)
;;; bind.el ends here
