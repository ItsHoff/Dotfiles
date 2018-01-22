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
                    "M-u" #'universal-argument
                    "M-g" #'keyboard-quit
                    )

; Window commands
(defmacro my-window-hydra-macro (exit)
  "Make hydra :exit match EXIT."
    `(defhydra ,(if exit
                    (intern "my-window-hydra")
                  (intern "my-window-hydra-persistant"))
                  (:exit ,exit :hint nil)
      "Window"
      ("h" evil-window-left :column "jump")
      ("j" evil-window-down)
      ("k" evil-window-up)
      ("l" evil-window-right)
      ("t" evil-window-top-left "top left")
      ("b" evil-window-bottom-right "bottom right")
      ("p" evil-window-mru "previous")
      ("s" evil-window-split "h-split" :column "split")
      ("v" evil-window-vsplit "v-split")
      ("c" evil-window-delete "close this" :column "close")
      ("o" delete-other-windows "close others" :exit t)
      ("H" evil-window-move-far-left "far left" :column "move")
      ("J" evil-window-move-very-bottom "very bottom")
      ("K" evil-window-move-very-top "very top")
      ("L" evil-window-move-far-right "far right")
      ("r" evil-window-rotate-downwards "rotate down")
      ("R" evil-window-rotate-upwards "rotate up")
      ;("_" evil-window-set-height "set height" :column "resize")
      ;("|" evil-window-set-height "set width")
      ;("<up>" evil-window-increase-height "+ height")
      ;("<down>" evil-window-decrease-height "- height")
      ;("<right>" evil-window-increase-width "+ width")
      ;("<left>" evil-window-decrease-width "- width")
      ;("=" balance-windows "equalize")
      ,(if exit
           '("w" my-window-hydra-persistant/body "sticky" :column nil)
         '("q" nil "quit" :column nil))

      ))
(my-window-hydra-macro nil)
(my-window-hydra-macro t)
(evil-declare-not-repeat #'my-window-hydra/body)
(evil-declare-not-repeat #'my-window-hydra-persistant/body)

; Everywhere except insert
(general-define-key :keymaps '(motion normal visual global emacs)
                    "C-z" #'suspend-emacs
                    "C-w" #'my-window-hydra/body
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
                    "H" #'my-beginning-of-line
                    "L" #'my-end-of-line
                    "J" #'golden-ratio-scroll-screen-up
                    "K" #'golden-ratio-scroll-screen-down
                    ; Swap , and ;
                    "," #'evil-repeat-find-char
                    ";" #'evil-repeat-find-char-reverse
                    ; Make ¤ be forward #
                    "#" #'evil-search-word-forward
                    "¤" #'evil-search-word-backward
                    ; Move sentence object
                    "s" #'evil-forward-sentence-begin
                    "S" #'evil-backward-sentence-begin
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
                    "RET" #'my-split-line
                    ; Pops
                    "C-k" #'my-paste-and-repeat-pop
                    "C-j" #'my-paste-and-repeat-pop-next
                    )

; Insert state
(general-define-key :keymaps 'insert
                    "<tab>" #'tab-to-tab-stop
                    "<backspace>" #'my-backspace-whitespace-to-tab-stop
                    "<return>" #'newline-and-indent
                    "<C-i>" #'indent-according-to-mode
                    )


(load "easy-brackets")

(provide 'bind)
;;; bind.el ends here
