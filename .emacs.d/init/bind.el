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
 "<drag-mouse-1>" nil ; Causes annoying accidental visual modes when clicking
 )
(general-define-key :keymaps 'evil-read-key-map
                    "C-k" nil) ; Conflicts with easy-brackets

;; BINDINGS -----------------------------------------------------------------------------------

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] #'keyboard-quit)
(define-key evil-visual-state-map [escape] #'keyboard-quit)
(define-key minibuffer-local-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-inactive-mode-map [escape] #'minibuffer-keyboard-quit)
(global-set-key [escape] #'evil-exit-emacs-state)

; Overrides
(general-define-key :keymaps 'override
                    :states  '(motion normal visual emacs)
                    "C-M-S-s-t" #'my/test-function
                    "M-+" help-map  ; Remap help
                    "M-<dead-acute>" #'describe-key  ; Map key help (next to +)
                    "M-/" help-map  ; Remap help for ansi
                    "M-;" #'describe-key  ; Map key help (next to +)
                    "M-u" #'universal-argument
                    "M-g" #'keyboard-quit
                    "C-z" #'suspend-emacs
                    "C-q" #'my/quit-extra-windows
                    "C-h" #'evil-window-left
                    "C-j" #'evil-window-down
                    "C-k" #'evil-window-up
                    "C-l" #'evil-window-right
                    "C-b" #'ivy-switch-buffer
                    "M-x" #'counsel-M-x
                    )

(general-define-key :keymaps 'override
                    :states '(motion normal visual emacs)
                    :prefix "SPC"
                    "SPC" #'counsel-M-x
                    "g" #'magit-status
                    "p" #'projectile-command-map
                    "b" #'ivy-bibtex
                    "h" #'my/helm-command-map
                    "s" #'my/framegroups-command-map
                    )

(general-define-key :keymaps 'override
                    :states '(motion normal visual)
                    ; Bigger movement
                    "H" #'my/beginning-of-line
                    "L" #'my/end-of-line
                    "J" #'golden-ratio-scroll-screen-up
                    "K" #'golden-ratio-scroll-screen-down
                    )

; Emacs state
(general-define-key :keymaps 'emacs
                    ":" #'evil-ex
                    "M-z" #'evil-exit-emacs-state
                    )

; Motions (normal, visual and some special buffers)
(general-define-key :keymaps '(motion normal visual)
                    "<C-i>" #'evil-jump-forward
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
                    ; Clipboard paste and yank
                    "C-M-p" #'my/paste-clipboard-after
                    "C-M-S-p" #'my/paste-clipboard-before
                    "C-M-y" #'my/yank-clipboard
                    "C-M-S-y" #'my/yank-line-clipboard
                    )

(general-define-key :keymaps 'normal
                    ; Join + split
                    "<backspace>" #'evil-join
                    "RET" #'my/split-line
                    )

; Insert state
(general-define-key :keymaps 'insert
                    "<tab>" #'tab-to-tab-stop
                    "<backspace>" #'my/backspace-whitespace-to-tab-stop
                    "<return>" #'newline-and-indent
                    "<C-i>" #'indent-according-to-mode
                    )


(load "easy-brackets")

(provide 'bind)
;;; bind.el ends here
