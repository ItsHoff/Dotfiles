;;; bind --- bind all my keys
;;; Commentary:
;;; My keybindings

;;; Code:
(require 'general)
(require 'evil)

; Make C-i different from <tab>
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))

;; UNBIND --------------------------------------------------------------------------------------

(general-define-key "C-<backspace>" nil)    ; Mistyped often with i-mode brackets
(general-define-key :keymaps '(motion normal visual global emacs insert)
                    "C-z" 'suspend-emacs)

; Unbind old window movements until replaced
(general-define-key "C-j" nil)
(general-define-key "C-k" nil)
(general-define-key "C-l" nil)

;; MY FUNCTIONS --------------------------------------------------------------------------------

(evil-define-operator my-yank-eol (beg end type register yank-handler)
  "Yank from point to end of line."
  :motion evil-end-of-line
  :move-point nil
  (interactive "<R><x><y>")
  (evil-yank (point) (point-at-eol) type register yank-handler))

(defun my-scroll-page-down (n)
  "Scroll down N pages and place cursor at bottom."
  (interactive "P")
  (if n
      (evil-scroll-page-down n)
    (evil-scroll-page-down 1))
  (evil-window-bottom))

(defun my-scroll-page-up (n)
  "Scroll up N pages and place cursor at top."
  (interactive "P")
  (if n
      (evil-scroll-page-up n)
    (evil-scroll-page-up 1))
  (evil-window-top))

(defun my-split-line ()
  "Split line at current cursor position."
  (interactive)
  (let ((save-point (point)))
    (newline-and-indent)
    (goto-char (- save-point 1))))

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
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; BINDINGS -----------------------------------------------------------------------------------

(general-define-key "M-+" help-map)                 ; Remap help
(general-define-key "M-<dead-acute>" 'describe-key) ; Map key help (next to +)

; Window commands
(general-define-key :keymaps 'evil-window-map
                    ; Control variants of movement
                    "C-h" 'evil-window-left
                    "C-j" 'evil-window-down
                    "C-k" 'evil-window-up
                    "C-l" 'evil-window-right
                    )

(general-define-key :keymaps '(evil-normal-state-map evil-visual-state-map)
                    ; Make j and k move visual lines
                    "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    ; Bigger movement
                    "H" 'evil-first-non-blank-of-visual-line
                    "L" 'evil-end-of-line
                    "J" 'golden-ratio-scroll-screen-up
                    "K" 'golden-ratio-scroll-screen-down
                    ; Make Y function sensibly
                    "Y" 'my-yank-eol
                    ; Swap , and ;
                    "," 'evil-repeat-find-char
                    ";" 'evil-repeat-find-char-reverse
                    ; Join + split
                    "<backspace>" 'evil-join
                    "<return>" 'my-split-line
                    ; Make ¤ be forward #
                    "¤" 'evil-search-word-forward
                    ; Move sentence object
                    "s" 'evil-forward-sentence-begin
                    "S" 'evil-backward-sentence-begin
                    ; Move paragraph object
                    "q" 'evil-forward-paragraph
                    "Q" 'evil-backward-paragraph
                    ; Move emacs state
                    "M-z" 'evil-emacs-state
                    )

(general-define-key :keymaps 'evil-emacs-state-map
                    "M-z" 'evil-exit-emacs-state)

(load "easy-brackets")

;; Org
(general-define-key :keymaps 'org-mode-map
                    "TAB" nil)  ; Otherwise org overrides C-i

(general-define-key :keymaps 'org-mode-map
                    :states '(normal visual)
                    "J" 'outline-next-visible-heading
                    "K" 'outline-previous-visible-heading
                    "o" 'org-meta-return
                    "M-h" 'org-metaleft
                    "M-l" 'org-metaright
                    "M-j" 'org-metadown
                    "M-k" 'org-metaup
                    )

(evil-make-overriding-map org-mode-map 'normal)

(provide 'bind)
;;; bind.el ends here
