;;; init.el --- My emacs config
;;; Commentary:
;;; My .emacs

;;; Code:
(require 'package)
(add-to-list 'load-path "~/.emacs.d/init/")
(add-to-list 'load-path "~/.emacs.d/manual_packages/")
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
; and general
(unless (package-installed-p 'general)
  (package-refresh-contents)
  (package-install 'general))

; Install all packages automatically
(setq use-package-always-ensure t)

;; GENERAL SETTINGS ----------------------------------------------------------------------------

(server-start)                      ; Start server for external programs
(setq visible-bell 1)               ; No error beep
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar
(electric-indent-mode -1)           ; Handle indendation elsewhere
(setq inhibit-startup-screen t)     ; No message at startup
(modify-syntax-entry ?_ "w" (standard-syntax-table)) ; _ is now part of a word
(modify-syntax-entry ?- "w" (standard-syntax-table)) ; aswell as -
(setq select-enable-clipboard nil)  ; Disable emacs clipboard and rely on evil
(setq completion-styles '(basic initials partial substring)) ; Better completion
(fset 'yes-or-no-p 'y-or-n-p)       ; y or n should suffice for confirmation
(setq-default fill-column 110)      ; Line wrap column
(setq large-file-warning-threshold 50000000)  ; Allow larger files to be opened without confirmation
(setq history-length 1000)          ; Increase the amount of history
(setq create-lockfiles nil)         ; Don't create lockfiles
(setq auto-save-default nil)        ; No auto-saves

; Use nicer window splitting method for automatic splits
(setq split-window-preferred-function #'my/split-only-root)

; Make C-i and C-m different from <tab> and <return>
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

; Just define § as escape so things are sensible on bigger keyboards
(define-key input-decode-map [?§] [escape])

; UTF-8 please
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq hscroll-step 1)
(setq hscroll-margin 5)

;; Tabs & Spaces
(setq-default tab-always-indent nil)    ; Allow tabbing outside of indent
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tabs
(setq-default tab-width 4)              ; Tab = 4 spaces
(setq-default evil-shift-width tab-width)

;; Backups
; Put backups in .emacs.d
(defvar backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p backup-directory))
        (make-directory backup-directory t))
(defvar undo-directory (concat user-emacs-directory "undos"))
(if (not (file-exists-p undo-directory))
        (make-directory undo-directory t))
(setq backup-directory-alist `(("." . ,backup-directory)))

;; Fonts
(cond
  ((find-font (font-spec :name "Consolas"))
   (add-to-list 'default-frame-alist '(font . "Consolas-11")))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11")))
  )

;; Save custom settings to another file so they don't mess up my init files
(setq custom-file "~/.emacs.d/custom.el")

;; SYSTEM SETUP --------------------------------------------------------------------------------

; Windows
(when (member system-type '(ms-dos windows-nt cygwin))
  ;; Windows key is super
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)
  ;; and windows app key hyper
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper))

; Mac
(use-package exec-path-from-shell
  :if (memq window-system '(ns))
  :config
  (exec-path-from-shell-initialize))

;; LOCAL CONF ----------------------------------------------------------------------------------

(require 'local-util)

; Environment variables
(setenv "LANG" "en_US")

; Local configuration
(load "local-conf" 'noerror) ; No error if missing

;; HOOKS ---------------------------------------------------------------------------------------

; Save when emacs loses focus
(add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))

; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; PACKAGES ------------------------------------------------------------------------------------

(use-package general
  :init
  (general-evil-setup)
  (general-override-mode))

(use-package evil
  :custom
  (evil-want-keybinding nil) ; Use evil-collection for integration
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-w-in-emacs-state t) ; Window commands should always work
  (evil-ex-substitute-global t) ; substitute replaces all occurences in line
  ; Open new splits right or below
  (evil-vsplit-window-right 1)
  (evil-split-window-below 1)
  :init
  (evil-mode 1)
  :config
  (setq evil-normal-state-tag "NORM")
  (setq evil-visual-state-tag "VIS")
  (setq evil-motion-state-tag "MOT")
  (setq evil-insert-state-tag "INS")
  (setq evil-operator-state-tag "OP")
  (setq evil-emacs-state-tag "EMACS")
  ; Allow c-o and c-i to jump to buffers matching the regexp
  (setq evil--jumps-buffer-targets "\\`magit")

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
  )

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode)

; Automatically reload changed files
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :custom
  (global-auto-revert-ignore-modes '(pdf-view-mode))
  ; Poll for changes instead of asking notification from OS
  ; On windows notifications block file deletion
  (auto-revert-use-notify nil)
  ;(auto-revert-interval 5) to change poll interval
  :config
  (global-auto-revert-mode t))

(use-package buffer-move
  :custom (buffer-move-stay-after-swap t))

(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :custom
  (company-tooltip-limit 20)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  :config
  (dotimes (i 10)
    (general-define-key :keymaps 'company-active-map
                        (format "C-%d" i) #'company-complete-number))
  :general
  (:keymaps 'company-active-map
            "C-<return>" #'newline-and-indent)
  )

(use-package company-flx
  :config (company-flx-mode t))

(use-package company-lsp
  :custom (company-lsp-enable-snippet nil))

(use-package counsel
  :custom
  (ivy-on-del-error-function nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-initial-inputs-alist nil)
  (ivy-flx-limit 10000)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  ; Abbreviate virtual buffers so files with the same name are not ignored
  (ivy-virtual-abbreviate 'abbreviate)
  :init
  (use-package ivy
    :diminish ivy-mode
    :init (ivy-mode 1))
  (use-package ivy-hydra)
  :config
  (evil-declare-not-repeat #'ivy-switch-buffer)
  (evil-declare-not-repeat #'counsel-find-file)
  (evil-declare-not-repeat #'ivy-done)
  (evil-declare-not-repeat #'ivy-alt-done)
  (evil-add-command-properties #'counsel-find-file :jump t)
  (evil-add-command-properties #'ivy-switch-buffer :jump t)
  (require 'evil-collection-ivy)
  (evil-collection-ivy-setup)
  :general
  (:keymaps 'ivy-minibuffer-map
            "C-h" #'ivy-alt-done
            "<escape>" #'minibuffer-keyboard-quit))

(use-package diminish)

(use-package dired
  :after evil-collection
  :ensure nil
  :config
  (put 'dired-find-alternate-file 'disabled nil) ; Allow dired to use the same buffer
  (evil-add-command-properties #'dired-jump :jump t)
  (require 'evil-collection-dired)
  (evil-collection-dired-setup))

(use-package display-line-numbers
  :custom (display-line-numbers-type 'visual)
  :config (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(use-package eldoc
  :diminish eldoc-mode)

(use-package evil-anzu
  :after evil
  :custom
  (anzu-cons-mode-line-p nil)
  )

(use-package evil-collection
  :after evil)

(use-package evil-goggles
  :custom (evil-goggles-duration 0.1)
  :config (evil-goggles-mode))

(use-package evil-matchit
  :after evil
  :init (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :config
  (define-key evil-inner-text-objects-map "c" 'evilnc-inner-comment)
  (define-key evil-outer-text-objects-map "c" 'evilnc-outer-commenter))

(use-package evil-numbers
  :after evil)

(use-package evil-traces
  :config
  (evil-traces-use-diff-faces)
  (evil-traces-mode))

(use-package evil-visualstar
  :after evil
  :config (global-evil-visualstar-mode))

(use-package flycheck
  :diminish flycheck-mode
  :config (global-flycheck-mode))

; Fuzzy matching for company
(use-package flx)

(use-package framegroups
  :ensure nil
  :pin manual
  ; Needed to load the package (binds) on start-up
  :commands fg-switch
  :init
  (defvar my/framegroups-command-map (make-sparse-keymap))
  (fset 'my/framegroups-command-map my/framegroups-command-map)
  (defun my/framegroup-setup (name &rest _)
    "Set up default framegroup layouts."
    (interactive)
    (pcase name
      ("default"
       (find-file "~/.emacs.d/init.el")
       (set-frame-parameter nil 'fullscreen 'maximized))
      ("emacs"
       (find-file "~/.emacs.d/init/bind.el")
       (split-window-right)
       (find-file "~/.emacs.d/init.el")
       (set-frame-parameter nil 'fullscreen 'maximized))
      (_
       (switch-to-buffer "*scratch*")
       (set-frame-parameter nil 'fullscreen 'maximized))
      ))
  (add-hook 'fg-create-hook #'my/framegroup-setup)
  ; Open initial frame
  (add-hook 'window-setup-hook (fg-switch "default"))
  :general
  (:keymaps 'my/framegroups-command-map
            "s" #'fg-switch-to-frame
            "p" #'fg-switch-to-last-frame
            "e" (fg-switch "emacs")
            "r" #'fg-rename-frame
            "c" #'delete-frame
            "o" #'delete-other-frames
            "m" #'toggle-frame-maximized
            "f" #'toggle-frame-fullscreen
            ))

(use-package gcmh
  :config (gcmh-mode 1))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)
  :custom
  (golden-ratio-auto-scale nil)
  (golden-ratio-adjust-factor 0.7)
  (golden-ratio-recenter nil)
  :config
  ; From spacemacs
  (setq window-combination-resize t)
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")
  ;; golden-ratio-exclude-modes
  (dolist (m '("bs-mode"
               "calc-mode"
               "ediff-mode"
               "dired-mode"
               "gud-mode"
               "gdb-locals-mode"
               "gdb-registers-mode"
               "gdb-breakpoints-mode"
               "gdb-threads-mode"
               "gdb-frames-mode"
               "gdb-inferior-io-mode"
               "gdb-disassembly-mode"
               "gdb-memory-mode"
               "speedbar-mode"
               ))
    (add-to-list 'golden-ratio-exclude-modes m))
  ;; golden-ratio-extra-commands
  (dolist (f '(ace-window
               ace-delete-window
               ace-select-window
               ace-swap-window
               ace-maximize-window
               avy-pop-mark
               buf-move-left
               buf-move-right
               buf-move-up
               buf-move-down
               evil-avy-goto-word-or-subword-1
               evil-avy-goto-line
               evil-window-delete
               evil-window-split
               evil-window-vsplit
               evil-window-left
               evil-window-right
               evil-window-up
               evil-window-down
               evil-window-bottom-right
               evil-window-top-left
               evil-window-mru
               evil-window-next
               evil-window-prev
               evil-window-new
               evil-window-vnew
               evil-window-rotate-upwards
               evil-window-rotate-downwards
               evil-window-move-very-top
               evil-window-move-far-left
               evil-window-move-far-right
               evil-window-move-very-bottom
               magit-status
               quit-window
               winum-select-window-0-or-10
               winum-select-window-1
               winum-select-window-2
               winum-select-window-3
               winum-select-window-4
               winum-select-window-5
               winum-select-window-6
               winum-select-window-7
               winum-select-window-8
               winum-select-window-9
               windmove-left
               windmove-right
               windmove-up
               windmove-down))
    (add-to-list 'golden-ratio-extra-commands f))
  ;; golden-ratio-exclude-buffer-names
  (dolist (n '(" *NeoTree*"
               "*LV*"
               " *transient*"
               " *which-key*"
               ))
    (add-to-list 'golden-ratio-exclude-buffer-names n))
  )

(use-package golden-ratio-scroll-screen
  :custom
  (golden-ratio-scroll-recenter nil)
  :config
  (evil-declare-motion #'golden-ratio-scroll-screen-down)
  (evil-declare-motion #'golden-ratio-scroll-screen-up))

(use-package helm
  :diminish helm-mode
  :init
  (defvar my/helm-command-map (make-sparse-keymap))
  (fset 'my/helm-command-map my/helm-command-map)
  :config
  (require 'helm-config)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (helm-autoresize-mode t)
  :general
  (:keymaps 'my/helm-command-map
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
            "C-n" #'helm-delete-minibuffer-contents
            ))

(use-package hydra)

(use-package interaction-log
  :commands interaction-log-mode)

(use-package ispell
  :ensure nil
  :custom
  (ispell-silently-savep t)
  :config
  (local/custom ispell-program-name)
  )

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

; TODO: Add binds
(use-package lsp-mode
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  :general
  ; TODO: lsp doesn't define a keymap?
  (:keymaps 'lsp-mode-map
            "C-M-d" #'lsp-find-definition
            ))

(use-package lsp-ui
  :custom
  ; TODO: add toggle for hover and doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-symbol t)
  )

(use-package magit
  :init
  (use-package evil-magit)
  (require 'evil-magit)
  :config
  (evil-add-command-properties #'magit-diff-visit-file :jump t)
  (evil-add-command-properties #'magit-status :jump t)
  :general
  (:keymaps 'magit-mode-map
            "<up>" #'magit-section-backward
            "<down>" #'magit-section-forward
            "<left>" #'magit-section-backward-sibling
            "<right>" #'magit-section-forward-sibling
            "<return>" #'magit-visit-thing
            "SPC" nil
            )
  (:keymaps 'transient-sticky-map
            "<escape>" #'transient-quit-seq
            )
  (:keymaps 'transient-map
            "<escape>" #'transient-quit-one
            )
  )

(use-package olivetti
  :commands olivetti-mode
  :custom
  (olivetti-body-width 80))

(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :init
  (add-hook 'prog-mode-hook (lambda () (outline-minor-mode)))
  :config
  ; 10.8.19
  ;; (evil-make-overriding-map outline-minor-mode-map 'motion)
  (dolist (cmd '(outline-next-visible-heading
                 outline-previous-visible-heading
                 outline-forward-same-level
                 outline-backward-same-level))
    (evil-declare-motion cmd)
    (evil-declare-not-repeat cmd))
  :general
  (:keymaps 'outline-minor-mode-map
            :states '(normal visual)
            "<down>" #'outline-next-visible-heading
            "<up>" #'outline-previous-visible-heading
            "<right>" #'outline-forward-same-level
            "<left>" #'outline-backward-same-level
            ))

(use-package package
  :after evil-collection
  :ensure nil
  :config
  (require 'evil-collection-package-menu)
  (evil-collection-package-menu-setup))

; Show matching parenthesis
(use-package paren
  :ensure nil
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode 1))

(use-package pdf-tools
  :if (memq window-system '(ns))
  :after evil-collection
  :custom
  (pdf-view-display-size 'fit-page)
  :config
  (pdf-tools-install)
  (require 'evil-collection-pdf)
  (evil-collection-pdf-setup))

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-indexing-method 'hybrid) ; Required tools should be installed on windows as well
  (projectile-completion-system 'ivy)
  :config
  (use-package counsel-projectile
    :custom (counsel-projectile-mode t))
  (evil-add-command-properties #'counsel-projectile-find-file :jump t)
  (evil-add-command-properties #'projectile-find-other-file :jump t)
  :general
  (:keymaps 'projectile-command-map
            "A" (lambda () (interactive) (projectile-add-known-project (projectile-project-p)))
            )
  )

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 50)
  :config
  (add-to-list 'recentf-exclude "autoloads\\.el\\'")
  ; Exclude files with no suffix
  (add-to-list 'recentf-exclude "/\\w*$")
  (recentf-mode 1))

(use-package ripgrep)

; Used by ivy to improve ordering
(use-package smex)

(use-package spaceline
  :after framegroups
  :config
  (require 'spaceline-config)
  (setq powerline-height 25)
  (setq powerline-default-separator "slant")
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-define-segment framegroups
    "Segment for framegroups"
    (when (fboundp 'fg-mode-line-string)
      (fg-mode-line-string)))
  (spaceline-spacemacs-theme '(framegroups :tight t)))

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

(use-package ssh-agency)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,undo-directory)))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-lazy-drawing nil) ; Change this to improve perf
  ; Disable region undo since it seems to be flaky
  (setq undo-tree-enable-undo-in-region nil)
  :config
  ; 10.8.19
  ;; (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
  (advice-add #'undo-tree-undo-1 :filter-args #'my/advice-preserve-timestamps)
  (advice-add #'undo-tree-redo-1 :filter-args #'my/advice-preserve-timestamps)
  :general
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
  (setq whitespace-style '(face trailing tabs space-before-tab tab-mark))
  (setq whitespace-tab 'highlight)
  ; Change tab mark (this removes space and newline marks)
  (setq whitespace-display-mappings '((tab-mark ?\t [?▸ ?\t] [?› ?\t] [?> ?\t])))
  (global-whitespace-mode t))

(use-package xref
  :after evil-collection
  :ensure nil
  :config
  (require 'evil-collection-xref)
  (evil-collection-xref-setup))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (defvar my/yas-command-map (make-sparse-keymap))
  (fset 'my/yas-command-map my/yas-command-map)
  (use-package ivy-yasnippet)
  (use-package yasnippet-snippets)
  :config
  (yas-global-mode)
  :general
  (:keymaps 'my/yas-command-map
            "y" #'ivy-yasnippet
            "n" #'yas-new-snippet
            "v" #'yas-visit-snippet-file
            ))

;; PROGRAMMING MODES ---------------------------------------------------------------------------

;; C and C++
(use-package cc-mode
  :ensure nil
  :init
  (setq c-default-style "bsd")
  (setq c-basic-offset 4)
  (c-set-offset 'innamespace 0)
  (setq c-tab-always-indent nil)
  (add-to-list 'auto-mode-alist '("\\inl\\'" . c++-mode))
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Preferred comment style
              (setq comment-start "// "
                    comment-end "")))
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  :config
  (modify-syntax-entry ?_ "w" c-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?_ "w" java-mode-syntax-table) ; _ is now part of a word
  )

;; C#
(use-package csharp-mode)

;; GLSL
(use-package glsl-mode
  :mode "\\.shader\\'")

;; Groovy / Gradle
(use-package groovy-mode)

;; ELisp
(use-package lisp-mode
  :ensure nil
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (my/set-tab-width 2)))
  :config
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table) ; aswell as -
  )

;; Kotlin
(use-package kotlin-mode)

;; Octave / Matlab
(use-package octave-mode
  :ensure nil
  :mode "\\.m\\'"
  :init (add-hook 'octave-mode-hook (lambda () (my/set-tab-width 2)))
  )

;; Python
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook (lambda () (my/set-tab-width 4)))
  )

(use-package company-anaconda
  :init
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; Rust
(use-package rust-mode
  :init
  (add-hook 'rust-mode-hook #'lsp)
  )

;; Tex
(use-package tex
  :ensure auctex
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  :config
  (local/custom TeX-view-program-list)
  (local/custom TeX-view-program-selection)
  (modify-syntax-entry ?_ "w" TeX-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?- "w" TeX-mode-syntax-table) ; - is now part of a word
  (require 'pdf-sync)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook (lambda () (outline-minor-mode)
                               (setq word-wrap t)))
  :general
  (:keymaps 'LaTeX-mode-map
            :states 'normal
            "C-c e" #'TeX-next-error
            "g s" #'pdf-sync-forward-search
            )
  )

(use-package reftex
  :custom
  (reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

(use-package ivy-bibtex
  :custom
  (bibtex-completion-pdf-open-function #'org-open-file-with-system)
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-format-citation-functions
   '((org-mode . bibtex-completion-format-citation-org-link-to-PDF)
   (latex-mode . bibtex-completion-format-citation-cite)
   (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
   (default . bibtex-completion-format-citation-default)))
  :config
  (local/custom bibtex-completion-bibliography)
  (local/custom bibtex-completion-library-path)
  (local/custom bibtex-completion-notes-path)
  )

;; Vimrc
(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(require 'my-functions)
(require 'my-org-setup)
(require 'my-bindings)

(provide 'init)
;;; init.el ends here
