;;; bind --- bind all my keys
;;; Commentary:
;;; My keybindings

;;; Code:
(require 'general)
(require 'evil)
(require 'my-functions)

;; UNBIND --------------------------------------------------------------------------------------
(general-define-key "C-<backspace>" nil) ; Mistyped often with i-mode brackets
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

(general-define-key :keymaps 'global
                    "M-+" help-map  ; Remap help
                    "M-<dead-acute>" #'describe-key  ; Map key help (next to +)
                    "M-/" help-map  ; Remap help for ansi
                    "M-;" #'describe-key  ; Map key help (next to +)
                    "M-u" #'universal-argument
                    "M-g" #'keyboard-quit
                    )

; Window commands
(defhydra my/window-hydra (:hint nil :timeout 2)
  "Window"
  ("C-h" evil-window-left :column "jump")
  ("C-j" evil-window-down)
  ("C-k" evil-window-up)
  ("C-l" evil-window-right)
  ("C-p" evil-window-mru "previous")
  ("C-s" evil-window-split "h-split" :column "split")
  ("s" evil-window-split "h-split" :exit t)
  ("C-v" evil-window-vsplit "v-split")
  ("v" evil-window-vsplit "v-split" :exit t)
  ("C-c" evil-window-delete "close this" :column "close")
  ("c" evil-window-delete "close this" :exit t)
  ("C-o" delete-other-windows "close others" :exit t)
  ("C-r" evil-window-rotate-downwards "rotate down" :column "reorder")
  ("r" evil-window-rotate-downwards "rotate down" :exit t))

(evil-declare-not-repeat #'my/window-hydra/body)

; Everywhere except insert
(general-define-key :keymaps '(motion normal visual global emacs)
                    "C-z" #'suspend-emacs
                    "C-w" #'my/window-hydra/body
                    "C-q" #'my/quit-extra-windows
                    "C-h" (lambda (count) (interactive "p") (evil-window-left count)
                            (golden-ratio)(my/window-hydra/body))
                    "C-j" (lambda (count) (interactive "p") (evil-window-down count)
                            (golden-ratio)(my/window-hydra/body))
                    "C-k" (lambda (count) (interactive "p") (evil-window-up count)
                            (golden-ratio)(my/window-hydra/body))
                    "C-l" (lambda (count) (interactive "p") (evil-window-right count)
                            (golden-ratio)(my/window-hydra/body))
                    )

; Emacs state
(general-define-key :keymaps 'emacs
                    ":" #'evil-ex
                    "M-z" #'evil-exit-emacs-state
                    )

; Motions (normal, visual and some special buffers)
(general-define-key :keymaps '(motion normal visual)
                    "<C-i>" #'evil-jump-forward
                    ; Move emacs state
                    "M-z" #'evil-emacs-state
                    ; Make j and k move visual lines
                    "j" #'evil-next-visual-line
                    "k" #'evil-previous-visual-line
                    ; Bigger movement
                    "H" #'my/beginning-of-line
                    "L" #'my/end-of-line
                    "J" #'golden-ratio-scroll-screen-up
                    "K" #'golden-ratio-scroll-screen-down
                    ; Move sentence object
                    "s" #'evil-forward-sentence-begin
                    "S" #'evil-backward-sentence-begin
                    ; _;_ is leader so use ' instead
                    "'" #'evil-repeat-find-char
                    )

; Define separately to avoid overriding some buffers q quit
(general-define-key :keymaps '(normal visual)
                    ; Move paragraph object
                    "q" #'evil-forward-paragraph
                    "Q" #'evil-backward-paragraph
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
