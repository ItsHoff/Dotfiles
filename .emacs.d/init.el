(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; GENERAL SETTINGS------------------------------------------------------------------------

(setq visible-bell 1)               ; No error beep
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(setq inhibit-startup-screen t)     ; No message at startup
(global-linum-mode 1)               ; Show line numbers
(show-paren-mode 1)                 ; Show matching parenthesis

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq hscroll-step 1)
(setq hscroll-margin 5)

;; Backups
; Put backups in .emacs.d
(defvar backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p backup-directory))
        (make-directory backup-directory t))
(setq backup-directory-alist `(("." . ,backup-directory)))

(setq create-lockfiles nil)         ; Don't create lockfiles
(setq auto-save-default nil)        ; No auto-saves

;; Font
(set-frame-font "Consolas-11")

;; Tabs & Spaces
(setq-default tab-always-indent nil)    ; Allow tabbing outside of indent
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tabs
(setq-default tab-width 4)              ; Tab = 4 spaces

;; Remaps
(global-set-key (kbd "M-ä") help-map)   ; Remap help
(global-set-key (kbd "M-ö") 'describe-key)   ; Temporary key help

;; HOOKS-----------------------------------------------------------------------------------

; Auto-save on focus lost
(add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))

; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; PACKAGES--------------------------------------------------------------------------------

;; Install all packages automatically
(setq use-package-always-ensure t)

;; General
(use-package company
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package evil
  :init (evil-mode t)
  :config
  (setq evil-ex-substitute-global t) ;; substitute replaces all occurences in line
  ;; Make Y function sensibly
  (define-key evil-normal-state-map "Y" (lambda () (interactive) (evil-yank-line (point) (point-at-eol))))
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
  :bind
  (:map evil-normal-state-map
        ;; Swap , and ;
        ("," . evil-repeat-find-char)
        (";" . evil-repeat-find-char-reverse)
        ;; Combine delete and replace char
        ("q" . evil-replace)
        ("Q" . evil-delete-char)
        ;; Join
        ("x" . evil-join)
        ; TODO split
        ;; Make ¤ be forward #
        ("¤" . evil-search-word-forward)
        ;; Undo tree
        ("U" . undo-tree-redo)
        ("C-u" . undo-tree-visualize)
        ; TODO edit undo tree bindings

        ;; Move sentence object
        ("s" . evil-forward-sentence-begin)
        ("S" . evil-backward-sentence-begin)
        ;; Move paragraph object
        ("r" . evil-forward-paragraph)
        ("R" . evil-backward-paragraph)

        ;; Make j and k move visual lines
        ("j" . evil-next-visual-line)
        ("k" . evil-previous-visual-line)
        ;; Bigger Movement
        ("J" . evil-scroll-page-down)
        ("K" . evil-scroll-page-up)
        ("H" . evil-first-non-blank-of-visual-line)
        ("L" . evil-end-of-visual-line)
        ;; Window Movement
        ("C-j" . evil-window-down)
        ("C-k" . evil-window-up)
        ("C-h" . evil-window-left)
        ("C-l" . evil-window-right)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package org)

(use-package smartparens-config
  :ensure smartparens
  :init (smartparens-global-mode))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-default-separator "wave")
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package solarized-theme
  :config
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t))

;; GLSL

(use-package glsl-mode)

;; Rust

(use-package cargo
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :init (setq racer-rust-src-path "C:\\Programming\\Rust-GNU\\src\\rustc-nightly\\src")
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rust-mode)

;; Tex
(use-package tex-site
  :ensure auctex)

(provide 'init)
;;; init.el ends here
