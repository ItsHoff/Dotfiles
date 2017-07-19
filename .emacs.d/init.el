;;; init.el --- My emacs config
;;; Commentary:
;;; My .emacs

;;; Code:
(add-to-list 'load-path "~/.emacs.d/init/")
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

;; GENERAL SETTINGS ----------------------------------------------------------------------------

(prefer-coding-system 'utf-8)       ; UTF-8 please
(setq visible-bell 1)               ; No error beep
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(setq inhibit-startup-screen t)     ; No message at startup
(global-linum-mode 1)               ; Show line numbers
(show-paren-mode 1)                 ; Show matching parenthesis
(modify-syntax-entry ?_ "w")        ; _ is now part of a word
(modify-syntax-entry ?- "w")        ; aswell as -
(setq x-select-enable-clipboard nil)  ; Disable emacs clipboard and rely on evil
(put 'dired-find-alternate-file 'disabled nil)  ; Allow dired to use the same buffer
(setq completion-styles '(basic initials partial substring))  ; Better completion
(global-auto-revert-mode t)         ; Automatically reload changed files
(setq gc-cons-threshold 20000000)   ; This should reduce emacs gc time
(fset 'yes-or-no-p 'y-or-n-p)       ; y or n should suffice

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
(defvar undo-directory (concat user-emacs-directory "undos"))
(if (not (file-exists-p undo-directory))
        (make-directory undo-directory t))
(setq backup-directory-alist `(("." . ,backup-directory)))

(setq create-lockfiles nil)         ; Don't create lockfiles
(setq auto-save-default nil)        ; No auto-saves

;; Font
(cond
  ((find-font (font-spec :name "Consolas"))
   (set-frame-font "Consolas-11"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (set-frame-font "DejaVu Sans Mono-11"))
  )

;; Tabs & Spaces
(setq tab-always-indent nil)            ; Allow tabbing outside of indent
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tabs
(setq-default tab-width 4)              ; Tab = 4 spaces

;; HOOKS ---------------------------------------------------------------------------------------

; Auto-save on focus lost
(add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))

; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; PACKAGES ------------------------------------------------------------------------------------

;; Install all packages automatically
(setq use-package-always-ensure t)

(use-package general
  :init (general-evil-setup))

(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-require-match nil)
  (dotimes (i 10)
    (general-define-key :keymaps 'company-active-map
                        (format "C-%d" i) #'company-complete-number))
  :general
  (:keymaps 'company-active-map
            "C-m" nil   ; Not <return>
            "C-i" nil   ; Not <tab>
            "C-n" #'company-select-next
            "C-p" #'company-select-previous))

(use-package company-flx
  :config (company-flx-mode t))

(use-package counsel
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  :general
  (:keymaps '(motion normal)
            "SPC" nil)
  (:keymaps '(motion normal)
            :prefix "SPC"
            "SPC" #'counsel-M-x
            "e" #'counsel-find-file
            "b" #'ivy-switch-buffer
            )
  (:keymaps 'ivy-minibuffer-map
            "<escape>" #'minibuffer-keyboard-quit))

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-w-in-emacs-state t) ; Window commands should always work
  (setq evil-ex-substitute-global t) ; substitute replaces all occurences in line
  (setq evil-normal-state-tag "NORM")
  (setq evil-visual-state-tag "VIS")
  (setq evil-motion-state-tag "MOT")
  (setq evil-insert-state-tag "INS")
  (setq evil-emacs-state-tag "EMACS")
  (evil-mode 1))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flx)

(use-package golden-ratio-scroll-screen
  :config
  (evil-declare-not-repeat #'golden-ratio-scroll-screen-down)
  (evil-declare-not-repeat #'golden-ratio-scroll-screen-up))

(use-package helm
  :diminish helm-mode
  :config
  (require 'helm-config)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (helm-autoresize-mode t)
  :general
  (:keymaps 'motion
            "M-x" #'helm-M-x
            "C-m" #'helm-mini
            "C-f" #'helm-find-files)
  (:keymaps 'motion
   :prefix "C-h"
           "f" #'helm-find-files
           "C-f" #'helm-find-files
           "m" #'helm-mini
           "C-m" #'helm-mini
           "o" #'helm-occur
           "C-o" #'helm-occur
           "k" #'helm-man-woman
           "C-k" #'helm-man-woman
           "r" #'helm-resume
           "C-r" #'helm-resume
           "a" #'helm-apropos
           "C-a" #'helm-apropos)
  (:keymaps 'helm-map
            "<tab>" #'helm-execute-persistent-action
            "C-j" #'helm-select-action))

(use-package magit
  :general
  (:keymaps 'motion
            "C-g" #'magit-status)
  (:keymaps 'magit-mode-map))

(use-package org
  :config (setq org-M-RET-may-split-line '(default . nil))) ; Don't split line automatically

(use-package recentf
  :init (setq recentf-max-saved-items 50))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-height 25)
  (setq powerline-default-separator "bar")
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package solarized-theme
  :config
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,undo-directory)))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-lazy-drawing 1000)
  :config
  (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
  :general
  (:keymaps '(normal visual)
            "C-u" #'undo-tree-visualize
            )
  (:keymaps 'undo-tree-visualizer-mode-map
            "j" #'undo-tree-visualize-redo
            "J" #'undo-tree-visualize-redo-to-x
            "k" #'undo-tree-visualize-undo
            "K" #'undo-tree-visualize-undo-to-x
            "h" #'undo-tree-visualize-switch-branch-left
            "l" #'undo-tree-visualize-switch-branch-right
            ))

(use-package whitespace
  :init
  ; Hightlight tabs and trailing whitespace
  (setq whitespace-style '(face trailing empty tabs space-before-tab tab-mark))
  (setq whitespace-tab 'highlight)
  ; Change tab mark (this removes space and newline marks)
  (setq whitespace-display-mappings '((tab-mark ?\t [?▸ ?\t] [?› ?\t] [?> ?\t])))
  (global-whitespace-mode t))

;; C
(setq c-default-style "stroustrup")
(setq c-tab-always-indent nil)

;; GLSL
(use-package glsl-mode)

;; Octave / Matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Rust

(use-package cargo
  :config (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rust-mode)

;; Tex
(use-package tex-site
  :ensure auctex
  :bind ("C-c e" . TeX-next-error))

(load "bind")
(load "local-conf" t)  ; No error if missing

(provide 'init)
;;; init.el ends here
