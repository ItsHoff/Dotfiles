;;; init.el --- My emacs config
;;; Commentary:
;;; My .emacs

;;; Code:
(require 'package)
(add-to-list 'load-path "~/.emacs.d/init/")
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

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
(electric-indent-mode -1)           ; Handle indendation elsewhere
(setq inhibit-startup-screen t)     ; No message at startup
(show-paren-mode 1)                 ; Show matching parenthesis
(modify-syntax-entry ?_ "w" (standard-syntax-table)) ; _ is now part of a word
(modify-syntax-entry ?- "w" (standard-syntax-table)) ; aswell as -
(setq x-select-enable-clipboard nil) ; Disable emacs clipboard and rely on evil
(put 'dired-find-alternate-file 'disabled nil) ; Allow dired to use the same buffer
(setq completion-styles '(basic initials partial substring)) ; Better completion
(global-auto-revert-mode t)         ; Automatically reload changed files
(setq gc-cons-threshold 20000000)   ; This should reduce emacs gc time
(fset 'yes-or-no-p 'y-or-n-p)       ; y or n should suffice

; Make C-i and C-m different from <tab> and <return>
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

; Just define § as escape so things are sensible on bigger keyboards
(define-key input-decode-map [?§] [escape])

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
(setq-default tab-always-indent nil)    ; Allow tabbing outside of indent
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tabs
(setq-default tab-width 4)              ; Tab = 4 spaces
(setq-default evil-shift-width tab-width)

;; Make windows app key hyper
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; Save custom settings to another file and load it
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-w-in-emacs-state t) ; Window commands should always work
  (evil-mode 1)
  :config
  (setq evil-ex-substitute-global t) ; substitute replaces all occurences in line
  (setq evil-normal-state-tag "NORM")
  (setq evil-visual-state-tag "VIS")
  (setq evil-motion-state-tag "MOT")
  (setq evil-insert-state-tag "INS")
  (setq evil-operator-state-tag "OP")
  (setq evil-emacs-state-tag "EMACS")
  ; Open new splits right or below
  (setq evil-vsplit-window-right 1)
  (setq evil-split-window-below 1)
  )

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode)

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-require-match nil)
  (setq company-dabbrev-downcase nil)
  (dotimes (i 10)
    (general-define-key :keymaps 'company-active-map
                        (format "C-%d" i) #'company-complete-number))
  :general
  (:keymaps 'company-active-map
            "C-<return>" #'newline-and-indent)
  )

(use-package company-flx
  :config (company-flx-mode t))

(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-flx-limit 10000)
  (use-package ivy
    :diminish ivy-mode
    :init (ivy-mode 1))
  :config
  (evil-declare-not-repeat #'ivy-switch-buffer)
  (evil-declare-not-repeat #'counsel-find-file)
  :general
  (:keymaps '(motion normal)
            "SPC" nil
            "C-f" #'counsel-find-file
            "C-b" #'ivy-switch-buffer
            "M-x" #'counsel-M-x
            )
  (:keymaps '(motion normal)
            :prefix "SPC"
            "SPC" #'counsel-M-x
            )
  (:keymaps 'evil-ex-map
            "b SPC" #'ivy-switch-buffer
            "e SPC" #'counsel-find-file)
  (:keymaps 'ivy-minibuffer-map
            "C-h" #'ivy-alt-done
            "C-j" #'ivy-next-line
            "C-k" #'ivy-previous-line
            "<escape>" #'minibuffer-keyboard-quit))

(use-package diminish)

(use-package eldoc
  :diminish eldoc-mode)

(use-package evil-exchange
  :init
  (setq evil-exchange-key (kbd ";x"))
  (evil-exchange-install))


(use-package evil-matchit
  :init (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :config
  (define-key evil-inner-text-objects-map "c" 'evilnc-inner-comment)
  (define-key evil-outer-text-objects-map "c" 'evilnc-outer-commenter)
  :general
  (:keymaps '(normal visual)
            "; c" #'evilnc-comment-or-uncomment-lines
            ))

(use-package evil-numbers
  :general
  (:keymaps 'normal
            "; i" #'evil-numbers/inc-at-pt
            "; d" #'evil-numbers/dec-at-pt
            ))

(use-package evil-visualstar
  :init (global-evil-visualstar-mode)
  :general
  (:keymaps 'visual
            "#" #'evil-visualstar/begin-search-forward
            "¤" #'evil-visualstar/begin-search-backward))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flx)

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1)
  :config
  (setq golden-ratio-auto-scale t)
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-bottom-right
                  evil-window-delete
                  delete-other-windows
                  evil-window-split
                  evil-window-top-left
                  evil-window-vsplit
                  evil-window-left
                  evil-window-down
                  evil-window-up
                  evil-window-right
                  evil-window-move-far-left
                  evil-window-move-very-bottom
                  evil-window-move-very-top
                  evil-window-move-far-right
                  evil-window-rotate-downwards
                  evil-window-rotate-upwards
                  evil-window-set-width
                  evil-window-set-height
                  evil-window-mru
                  evil-window-next
                  evil-window-prev
                  evil-window-new
                  buf-move-left
                  buf-move-right
                  buf-move-up
                  buf-move-down
                  window-number-select
                  select-window
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9)))
  )

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
            "<C-m>" #'helm-mini
            "SPC" nil
            )
  (:keymaps 'motion
   :prefix "SPC h"
   "x" #'helm-M-x
   "f" #'helm-find-files
   "m" #'helm-mini
   "o" #'helm-occur
   "k" #'helm-man-woman
   "r" #'helm-resume
   "a" #'helm-apropos
   )
  (:keymaps 'helm-map
            "<tab>" #'helm-execute-persistent-action
            "C-h" #'helm-execute-persistent-action
            "C-p" #'helm-select-action
            "C-j" #'helm-next-line
            "C-k" #'helm-previous-line
            "C-n" #'helm-delete-minibuffer-contents
            ))

(use-package hydra)

(use-package magit
  :init
  (use-package evil-magit)
  (require 'evil-magit)
  :general
  (:keymaps 'motion
            "SPC g" #'magit-status)
  (:keymaps 'magit-mode-map
            "J" #'magit-section-forward
            "K" #'magit-section-backward
            "C-j" #'magit-section-forward-sibling
            "C-k" #'magit-section-backward-sibling
            "<return>" #'magit-visit-thing
            "SPC" nil
            )
  )

(use-package neotree
  :config
  (setq neo-auto-indent-point t)
  (setq neo-smart-open t)
  (require 'evil)
  (evil-make-overriding-map neotree-mode-map 'normal)
  :general
  (:keymaps '(motion normal)
            "C-n" #'neotree-toggle)
  (:keymaps 'neotree-mode-map
            "C-n" #'neotree-toggle)
  )

(use-package nlinum-relative
  :init
  (setq nlinum-relative-redisplay-delay 0.01)
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))

(use-package org
  :init
  (require 'my-functions)
  (add-hook 'org-mode-hook (lambda () (my-set-tab-width 2)))
  :config
  (setq org-M-RET-may-split-line '(default . nil)) ; Don't split line automatically
  (evil-make-overriding-map org-mode-map 'normal)
  (advice-add #'org-indent-line :after #'my-org-indent-advice)
  :general
  (:keymaps 'org-mode-map
            :states '(normal visual)
            "J" #'outline-next-visible-heading
            "K" #'outline-previous-visible-heading
            "C-j" #'my-org-down-heading
            "C-k" #'my-org-up-heading
            "M-h" #'org-metaleft
            "M-l" #'org-metaright
            "M-j" #'org-metadown
            "M-k" #'org-metaup
            ))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-indexing-method 'alien) ; Required tools should be installed on windows aswell
  (setq projectile-completion-system 'ivy)
  :general
  (:keymaps 'projectile-command-map
            "A" (lambda () (interactive) (projectile-add-known-project (projectile-project-p)))
            )
  (:keymaps 'motion
            "SPC p" #'projectile-command-map
            )
  )

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 50)
  (recentf-mode 1))

(use-package smex)

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-height 25)
  (setq powerline-default-separator "slant")
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package solarized-theme
  :config
  (setq solarized-high-contrast-mode-line t)
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t)
  (solarized-with-color-variables
    'dark
    (let ((active-line base02) (inactive-line base02))
      (custom-theme-set-faces
       'solarized-dark
       `(mode-line ((t (:overline ,active-line :underline ,active-line
                                  :box (:line-width 1 :color ,active-line)
                                  :background ,base02 :foreground ,base1))))
       `(mode-line-inactive ((t (:overline ,inactive-line :underline ,inactive-line
                                           :box (:line-width 1 :color ,inactive-line)
                                           :background ,base02 :foreground ,base1))))
       `(powerline-active1 ((t (:background ,base03 :foreground ,base1))))
       `(powerline-active2 ((t (:background ,base02 :foreground ,base1))))
       `(powerline-inactive1 ((t (:background ,base02 :foreground ,base1))))
       `(powerline-inactive2 ((t (:background ,base02 :foreground ,base1))))
       `(nlinum-relative-current-face ((t (:inherit linum :foreground ,base1))))
       ))))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,undo-directory)))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-lazy-drawing nil) ; Change this to improve perf
  :config
  (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
  (advice-add #'undo-tree-undo-1 :filter-args #'my-advice-preserve-timestamps)
  (advice-add #'undo-tree-redo-1 :filter-args #'my-advice-preserve-timestamps)
  :general
  (:keymaps '(normal visual)
            "C-u" #'undo-tree-visualize
            )
  (:keymaps 'visual
            "u" #'undo-tree-undo
            "C-r" #'undo-tree-redo
            )
  (:keymaps 'undo-tree-visualizer-mode-map
            "q" #'undo-tree-visualizer-abort
            "<return>" #'undo-tree-visualizer-quit
            "j" #'undo-tree-visualize-redo
            "J" #'undo-tree-visualize-redo-to-x
            "k" #'undo-tree-visualize-undo
            "K" #'undo-tree-visualize-undo-to-x
            "h" #'undo-tree-visualize-switch-branch-left
            "l" #'undo-tree-visualize-switch-branch-right
            ))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode))

(use-package whitespace
  :ensure nil
  :diminish global-whitespace-mode
  :init
  ; Hightlight tabs and trailing whitespace
  (setq whitespace-style '(face trailing empty tabs space-before-tab tab-mark))
  (setq whitespace-tab 'highlight)
  ; Change tab mark (this removes space and newline marks)
  (setq whitespace-display-mappings '((tab-mark ?\t [?▸ ?\t] [?› ?\t] [?> ?\t])))
  (global-whitespace-mode t))

(use-package yasnippet
  :diminish yas-minor-mode)

;; PROGRAMMING MODES ---------------------------------------------------------------------------

;; C
(use-package cc-mode
  :ensure nil
  :commands c-mode
  :init
  (setq c-default-style "stroustrup")
  (setq c-tab-always-indent nil)
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Preferred comment style
              (setq comment-start "// "
                    comment-end "")))
  :config
  (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?_ "w" java-mode-syntax-table) ; _ is now part of a word
  )

;; GLSL
(use-package glsl-mode
  :mode "\\.shader\\'")

;; ELisp
(use-package lisp-mode
  :ensure nil
  :commands emacs-lisp-mode
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (my-set-tab-width 2)))
  :config
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table) ; aswell as -
  )

;; Java
(use-package meghanada
  :diminish meghanada-mode
  :init
  (setq meghanada-use-flycheck nil)
  (add-hook 'java-mode-hook #'meghanada-mode)
  )

;; Octave / Matlab
(use-package octave-mode
  :ensure nil
  :mode "\\.m\\'"
  :init (add-hook 'octave-mode-hook (lambda () (my-set-tab-width 2)))
  )

;; Python
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook (lambda () (my-set-tab-width 4)))
  )

(use-package company-anaconda
  :init
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; Rust
(use-package rust-mode
  :init
  (use-package cargo
    :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

  (use-package flycheck-rust
    :init (add-hook 'rust-mode-hook #'flycheck-rust-setup))

  (use-package racer
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

;; Tex
(use-package auctex
  :bind ("C-c e" . TeX-next-error)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; Vimrc
(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(load "bind")
(load "local-conf" 'noerror) ; No error if missing

(provide 'init)
;;; init.el ends here
