(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install all packages automatically
(setq use-package-always-ensure t)

;; PACKAGES
(use-package evil
  :init (evil-mode t))
(use-package solarized-theme
  :config (load-theme 'solarized-dark t))

;; GENERAL SETTINGS
(setq visible-bell 1)              ; No error beep
(tool-bar-mode -1)                 ; No toolbar
(scroll-bar-mode -1)               ; No scrollbar
(setq inhibit-start-up-message t)  ; No message at startup
(global-linum-mode 1)              ; Show line numbers

;; FONT
(set-frame-font "Consolas-11")

;; TABS & SPACES
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq tab-width 4)                  ; Tab = 4 spaces
