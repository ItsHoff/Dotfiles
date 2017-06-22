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

(setq visible-bell 1)               ; No error beep
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(setq inhibit-startup-screen t)     ; No message at startup
(global-linum-mode 1)               ; Show line numbers
(show-paren-mode 1)                 ; Show matching parenthesis
(modify-syntax-entry ?_ "w")        ; _ is now part of a word
(modify-syntax-entry ?- "w")        ; aswell as -
(setq x-select-enable-clipboard nil)    ; Disable emacs clipboard and rely on evil
(setq truncate-lines t)             ; Disable wrap by default

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
(cond
  ((find-font (font-spec :name "Consolas"))
   (set-frame-font "Consolas-11"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (set-frame-font "DejaVu Sans Mono-11"))
  )

;; Tabs & Spaces
(setq-default tab-always-indent nil)    ; Allow tabbing outside of indent
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

;; General
(use-package company
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package evil
  :init (evil-mode t)
  :config
  (setq evil-ex-substitute-global t)) ; substitute replaces all occurences in line

(use-package flycheck
  :init (global-flycheck-mode))

(use-package general
  :init (general-evil-setup))

(use-package golden-ratio-scroll-screen)

(use-package helm-config
  :ensure helm)

(use-package org
  :config (setq org-M-RET-may-split-line '(default . nil))) ; Don't split line automatically

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

;; Octave / Matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Rust

(use-package cargo
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

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

(provide 'init)
;;; init.el ends here
