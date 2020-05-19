;;; init.el --- My emacs config -*- lexical-binding: t -*-
;;; Commentary:
;;; My .emacs

;;; Code:

;; Save custom settings to another file so they don't mess up the init file
(setq custom-file "~/.emacs.d/custom.el")
;; Load the custom file first so personal settings will always override it
;; (load custom-file :noerror) ; Figure out a way to load only desired variables

(add-to-list 'load-path "~/.emacs.d/init/")

;;; STRAIGHT -----------------------------------------------------------------------------------

(custom-set-variables '(straight-use-package-by-default t)
                      ; Switched to develop since built-in packages messed up freezing
                      '(straight-repository-branch "develop"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; ANALYSIS ------------------------------------------------------------------------------------

(use-package benchmark-init
  :disabled ; enable for benchmarking
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Log emacs interactions
(use-package interaction-log
  :commands interaction-log-mode)

;;; GENERAL SETTINGS ----------------------------------------------------------------------------

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

;; Performance suggestions from lsp (https://github.com/emacs-lsp/lsp-mode#performance)
(setq gc-cons-threshold (* 100 1024 1024)) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1 mb

;; Don't compact font caches. Will consume more memory, but improves performance.
(setq inhibit-compacting-font-caches t)

;; Use nicer window splitting method for automatic splits
(setq split-window-preferred-function #'my/split-only-root)

;; Make C-i and C-m different from <tab> and <return>
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

;; Just define § as escape so things are sensible on bigger keyboards
(define-key input-decode-map [?§] [escape])

;; UTF-8 please
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

;; Put backups in .emacs.d
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
   (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))))

;;; SYSTEM SETUP --------------------------------------------------------------------------------

;; Windows
(when (member system-type '(ms-dos windows-nt cygwin))
  ;; Disabled 17.9. because super modifier was getting stuck when locking the pc
  ;; Windows key is super
  ;; (setq w32-lwindow-modifier 'super)
  ;; (setq w32-rwindow-modifier 'super)
  ;; and windows app key hyper
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper))

;; Mac
(use-package exec-path-from-shell
  :if (memq window-system '(ns))
  :config
  (exec-path-from-shell-initialize))

;;; LOCAL CONF ----------------------------------------------------------------------------------

(require 'local-util)

;; Environment variables
(setenv "LANG" "en_US")

;; Local configuration
(load "local-conf" 'noerror) ; No error if missing

;;; HOOKS ---------------------------------------------------------------------------------------

;; Save when emacs loses focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; PACKAGES ------------------------------------------------------------------------------------

;; Hide packages from modeline
(use-package diminish
  :demand t)

;; Keybinding utilities
(use-package general
  :demand t
  :config
  (general-evil-setup)
  (general-override-mode))

;; Vim emulation
(use-package evil
  :demand t
  :custom
  (evil-want-keybinding nil)        ; Use evil-collection for integration
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-w-in-emacs-state t)  ; Window commands should always work
  (evil-ex-substitute-global t)     ; substitute replaces all occurences in line
  ;; Open new splits right or below
  (evil-vsplit-window-right 1)
  (evil-split-window-below 1)
  :config
  (evil-mode 1)
  (setq evil-normal-state-tag "NORM")
  (setq evil-visual-state-tag "VIS")
  (setq evil-motion-state-tag "MOT")
  (setq evil-insert-state-tag "INS")
  (setq evil-operator-state-tag "OP")
  (setq evil-emacs-state-tag "EMACS")
  ;; Allow c-o and c-i to jump to buffers matching the regexp
  (setq evil--jumps-buffer-targets "\\`magit")

  (evil-declare-not-repeat #'compile-goto-error)
  (evil-declare-not-repeat #'eval-buffer)

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
  (global-set-key [escape] #'evil-exit-emacs-state))

;; Company abbrev enables this
(use-package abbrev
  :straight (:type built-in)
  :diminish abbrev-mode)

;; Automatically reload changed files
(use-package autorevert
  :demand t
  :straight (:type built-in)
  :diminish auto-revert-mode
  :custom
  (global-auto-revert-ignore-modes '(pdf-view-mode))
  ;; Poll for changes instead of asking notification from OS
  ;; On windows notifications block file deletion
  (auto-revert-use-notify nil)
  ;;(auto-revert-interval 5) to change poll interval
  :config
  (global-auto-revert-mode t))

;; Used for swapping buffer positions in a window
(use-package buffer-move
  :commands (buf-move-left buf-move-right buf-move-up buf-move-down)
  :custom (buffer-move-stay-after-swap nil))

;; Auto completion
(use-package company
  :demand t
  :diminish company-mode
  :custom
  (company-tooltip-limit 20)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  :config
  ;; Fuzzy matching for company
  (use-package company-flx
    :config (company-flx-mode t))
  (dotimes (i 10)
    (general-define-key :keymaps 'company-active-map
                        (format "C-%d" i) #'company-complete-number))
  (setq company-backends (remove #'company-clang company-backends))
  (global-company-mode)
  :general
  (:keymaps 'company-active-map
            "C-<return>" #'newline-and-indent))

;; LSP backend for company
(use-package company-lsp
  :disabled ; 28.2.20 to test lsp-prefer-capf
  :commands company-lsp
  :custom (company-lsp-enable-snippet nil))

(use-package compile
  :straight (:type built-in)
  :after evil-collection
  :commands compilation-mode
  :custom
  (compilation-scroll-output 'first-error)
  :config
  (evil-collection-compile-setup))

;; Minibuffer completion framework
(use-package counsel
  :after evil-collection
  :demand t
  :diminish counsel-mode
  :custom
  (ivy-on-del-error-function nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-initial-inputs-alist nil)
  (ivy-flx-limit 10000)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-wrap t)
  (ivy-action-wrap t)
  ;; Abbreviate virtual buffers so files with the same name are not ignored
  (ivy-virtual-abbreviate 'abbreviate)
  :config
  (dolist (command '(ivy-switch-buffer
                     counsel-find-file
                     ivy-done
                     ivy-alt-done
                     ivy-occur-previous-line
                     ivy-occur-next-line
                     ivy-occur-revert-buffer
                     counsel-M-x))
    (evil-declare-not-repeat command))
  (evil-add-command-properties #'counsel-find-file :jump t)
  (evil-add-command-properties #'ivy-switch-buffer :jump t)
  (use-package ivy
    :diminish ivy-mode
    ;; ivy-mode replaces completing-read-function
    :config (ivy-mode t))
  (use-package ivy-hydra
    :commands hydra-ivy/body)
  (use-package amx)
  (evil-collection-ivy-setup)
  ;; counsel-mode replaces built in commands with counsel alternatives
  (counsel-mode t)
  :general
  (:keymaps 'ivy-minibuffer-map
            "C-h" #'ivy-alt-done
            "<escape>" #'minibuffer-keyboard-quit)
  (:keymaps 'ivy-occur-grep-mode-map
            "n" nil) ; Conflicts with search (was next-error-no-select)
  )

;; Save and restore emacs session
(use-package desktop
  :disabled ; never used properly with this setup
  :demand t
  :custom
  (desktop-restore-eager 2)
  (desktop-lazy-verbose nil)
  (desktop-globals-to-clear nil)
  (desktop-globals-to-save nil)
  (desktop-locals-to-save nil)
  :config
  (desktop-save-mode 1))

;; Directory editor
(use-package dired
  :after evil-collection
  :straight (:type built-in)
  :config
  (evil-collection-dired-setup)
  (put 'dired-find-alternate-file 'disabled nil) ; Allow dired to use the same buffer
  (evil-add-command-properties #'dired-jump :jump t))

;; Display line numbers
(use-package display-line-numbers
  :commands display-line-numbers-mode
  :custom (display-line-numbers-type 'visual)
  :init (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(use-package doom-themes
  :disabled ; 20.3.2020
  :demand t
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (load-theme 'doom-nord t))

;; Shows documentation about symbol under point on the echo area
(use-package eldoc
  :diminish eldoc-mode)

;; Shows search matches on modeline
(use-package evil-anzu
  :after evil
  :custom
  (anzu-cons-mode-line-p nil))

(use-package evil-collection
  :config
  (evil-collection-init '((custom cus-edit) debug help (package-menu package) xref)))

;; Visual hints for evil edits
(use-package evil-goggles
  :after evil
  :demand t
  :diminish evil-goggles-mode
  :custom (evil-goggles-duration 0.1)
  :config (evil-goggles-mode))

;; Match more things with %
(use-package evil-matchit
  :after evil
  :demand t
  :config (global-evil-matchit-mode 1))

;; Toggle comments on things
(use-package evil-nerd-commenter
  :after evil
  :commands (evilnc-comment-or-uncomment-lines evilnc-outer-commenter evilnc-inner-comment)
  :config
  (define-key evil-inner-text-objects-map "c" 'evilnc-inner-comment)
  (define-key evil-outer-text-objects-map "c" 'evilnc-outer-commenter))

;; Increment and decrement numbers
(use-package evil-numbers
  :after evil)

;; Start a * or # search from the visual selection
(use-package evil-visualstar
  :after evil
  :config (global-evil-visualstar-mode))

;; On the fly syntax checking
(use-package flycheck
  :diminish flycheck-mode
  :commands flycheck-mode
  :init (add-hook 'prog-mode-hook (lambda () (flycheck-mode))))

;; Frame utility
(use-package framegroups
  :demand t
  :straight (:host github :repo "noctuid/framegroups.el")
  :config
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
  ;; Open initial frame
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
            "f" #'toggle-frame-fullscreen))

;; Automatically resize splits
(use-package golden-ratio
  :demand t
  :diminish golden-ratio-mode
  :custom
  (golden-ratio-auto-scale nil)
  (golden-ratio-adjust-factor 0.7)
  (golden-ratio-recenter nil)
  (golden-ratio-mode 1)
  :config
  ;; From spacemacs
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
               "speedbar-mode"))
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
               " *which-key*"))
    (add-to-list 'golden-ratio-exclude-buffer-names n)))

;; Move up and down the screen nicely
(use-package golden-ratio-scroll-screen
  :demand t
  :custom
  (golden-ratio-scroll-recenter nil)
  :config
  (evil-declare-motion #'golden-ratio-scroll-screen-down)
  (evil-declare-motion #'golden-ratio-scroll-screen-up))

;; Heavier 'minibuffer' completion
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
            "a" #'helm-apropos)
  (:keymaps 'helm-map
            "<tab>" #'helm-execute-persistent-action
            "C-h" #'helm-execute-persistent-action
            "C-p" #'helm-select-action
            "C-n" #'helm-delete-minibuffer-contents))

;; Additional way of keybinding
(use-package hydra
  :commands defhydra)

;; Spell checking
(use-package ispell
  :straight (:type built-in)
  :custom
  (ispell-silently-savep t)
  :config
  (local/custom ispell-program-name))

(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :config
  (evil-add-command-properties #'lsp-ivy-workspace-symbol :jump t)
  (evil-add-command-properties #'lsp-ivy-global-workspace-symbol :jump t))

;; LSP support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  (lsp-prefer-capf t)
  (lsp-clients-clangd-args '("--header-insertion=never" "--suggest-missing-includes"))
  :config
  (defun my/goto-definition-lsp (_string _position)
    (when (bound-and-true-p lsp-mode)
      (not (stringp (lsp-find-definition)))))
  (setq evil-goto-definition-functions (delete #'evil-goto-definition-search evil-goto-definition-functions))
  (add-to-list 'evil-goto-definition-functions #'my/goto-definition-lsp)
  :general
  (:keymaps 'lsp-mode-map
            "C-M-a" #'lsp-execute-code-action))

;; Fancy ui for LSP
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-symbol t))

;; Git support
(use-package magit
  :defer 2
  :commands (magit-status magit-blame-addition)
  :custom
  (magit-diff-paint-whitespace-lines 'all)
  (magit-diff-refine-hunk 'all)
  :config
  (use-package evil-magit
    :custom (evil-magit-want-horizontal-movement t))
  (evil-add-command-properties #'magit-diff-visit-file :jump t)
  (evil-add-command-properties #'magit-status :jump t)
  :general
  (:keymaps 'magit-mode-map
            "<up>" #'magit-section-backward
            "<down>" #'magit-section-forward
            "<left>" #'magit-section-backward-sibling
            "<right>" #'magit-section-forward-sibling
            "C-M-u" #'magit-section-up
            "<return>" #'magit-visit-thing
            "SPC" nil)
  (:keymaps 'transient-sticky-map
            "<escape>" #'transient-quit-seq)
  (:keymaps 'transient-map
            "<escape>" #'transient-quit-one))

;; Improved writing experience
(use-package olivetti
  :commands olivetti-mode
  :custom
  (olivetti-body-width 80))

;; Show matching parenthesis
(use-package paren
  :straight (:type built-in)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode 1))

;; PDF mode
(use-package pdf-tools
  :if (memq window-system '(ns))
  :after evil-collection
  :custom
  (pdf-view-display-size 'fit-page)
  :config
  (pdf-tools-install)
  (evil-collection-pdf-setup))

;; Project management
(use-package projectile
  :demand t
  :custom
  (projectile-indexing-method 'hybrid) ; Required tools should be installed on windows as well
  (projectile-completion-system 'ivy)
  :config
  (use-package counsel-projectile
    :config (counsel-projectile-mode t))
  (evil-add-command-properties #'counsel-projectile-find-file :jump t)
  (evil-add-command-properties #'projectile-find-other-file :jump t)
  (projectile-mode t)
  :general
  (:keymaps 'projectile-command-map
            "ESC" nil
            "s s" #'lsp-ivy-workspace-symbol
            "s a" #'projectile-ag))

;; Save recently visited files between sessions
(use-package recentf
  :demand t
  :straight (:type built-in)
  :custom
  (recentf-max-saved-items 50)
  :config
  (add-to-list 'recentf-exclude "autoloads\\.el\\'")
  ;; Exclude files with no suffix
  ;; Disabled 28.2.20 because it excluded some desired files
  ;; such as Jenkinsfile and paths with spaces
  ;; (add-to-list 'recentf-exclude "/\\w*$")
  (recentf-mode 1))

(use-package saveplace
  :demand t
  :straight (:type built-in)
  :config
  (save-place-mode t))

(use-package smerge-mode
  :straight (:type built-in)
  :commands smerge-mode
  :general
  (:definer 'minor-mode
            :keymaps 'smerge-mode
            :states 'normal
            "<up>" #'smerge-prev
            "<down>" #'smerge-next
            "C-M-w" #'smerge-keep-upper
            "C-M-s" #'smerge-keep-lower
            "C-M-a" #'smerge-keep-all))

;; Modeline
(use-package spaceline
  :demand t
  :after framegroups
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-define-segment framegroups
    "Segment for framegroups"
    (when (fboundp 'fg-mode-line-string)
      (fg-mode-line-string)))
  (spaceline-spacemacs-theme '(framegroups :tight t)))

;; Color theme
(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-dark t)
  ;; Disable mode-line over and underlines
  (set-face-attribute 'mode-line nil :overline 'unspecified :underline 'unspecified)
  (set-face-attribute 'mode-line-inactive nil :overline 'unspecified :underline 'unspecified))

;; SSH agent support for emacs
(use-package ssh-agency
  :disabled t ; 23.10.19
  :commands ssh-agency-ensure)

;; Vim like undo
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,undo-directory)))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-lazy-drawing nil) ; Change this to improve perf
  ;; Disable region undo since it seems to be flaky
  (setq undo-tree-enable-undo-in-region nil)
  :config
  ;; 10.8.19
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
            "l" #'undo-tree-visualize-switch-branch-right))

;; Show key hints
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode))

;; Visualize extra whitespace
;; Could be used to also clean whitespace
(use-package whitespace
  :demand t
  :straight (:type built-in)
  :diminish global-whitespace-mode
  :custom
  (whitespace-style '(trailing tabs space-before-tab tab-mark))
  ;; Change tab mark (this removes space and newline marks)
  (whitespace-display-mappings '((tab-mark ?\t [?▸ ?\t] [?› ?\t] [?> ?\t])))
  :hook
  (prog-mode . (lambda ()
                 (setq-local whitespace-style
                             '(face trailing tabs space-before-tab tab-mark))))
  :config (global-whitespace-mode))

;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (defvar my/yas-command-map (make-sparse-keymap))
  (fset 'my/yas-command-map my/yas-command-map)
  :config
  (use-package ivy-yasnippet)
  (use-package yasnippet-snippets)
  (yas-global-mode)
  :general
  (:keymaps 'my/yas-command-map
            "y" #'ivy-yasnippet
            "n" #'yas-new-snippet
            "v" #'yas-visit-snippet-file))

;;; PROGRAMMING MODES ---------------------------------------------------------------------------

;; C and C++
(use-package cc-mode
  :commands (c++-mode c-mode java-mode)
  :straight (:type built-in)
  :custom
  (c-default-style "bsd")
  (c-basic-offset 4)
  (c-tab-always-indent nil)
  :init
  (add-to-list 'auto-mode-alist '("\\inl\\'" . c++-mode))
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Preferred comment style
              (setq comment-start "// "
                    comment-end "")))
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'c-mode-hook #'lsp-deferred)
  :config
  (c-set-offset 'innamespace 0)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)      ; _ is now part of a word
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)    ; _ is now part of a word
  (modify-syntax-entry ?_ "w" java-mode-syntax-table))  ; _ is now part of a word

;; CMake
(use-package cmake-mode
  :commands cmake-mode
  :custom (cmake-tab-width 4))

;; C#
(use-package csharp-mode
  :commands csharp-mode)

;; GLSL
(use-package glsl-mode
  :commands glsl-mode
  :mode "\\.shader\\'")

;; Groovy / Gradle
(use-package groovy-mode
  :commands groovy-mode)

;; ELisp
(use-package elisp-mode
  :straight (:type built-in)
  :commands emacs-lisp-mode
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (my/set-tab-width 2)))
  :config
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table) ; aswell as -
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   :states 'normal
   "C-c C-c" #'eval-buffer)
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   :states 'visual
   "C-c C-c" #'eval-region))

;; Kotlin
(use-package kotlin-mode
  :commands kotlin-mode)

;; Lua
(use-package lua-mode
  :commands lua-mode)

;; Octave / Matlab
(use-package octave-mode
  :straight (:type built-in)
  :mode "\\.m\\'"
  :commands octave-mode
  :init (add-hook 'octave-mode-hook (lambda () (my/set-tab-width 2))))

;; Python
(use-package anaconda-mode
  :commands anaconda-mode
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook (lambda () (my/set-tab-width 4)))
  :config
  (use-package company-anaconda
    :config
    (add-to-list 'company-backends '(company-anaconda :with company-capf))))

(use-package python-mode
  :straight (:type built-in)
  :mode "SConstruct")

;; Rust
(use-package rust-mode
  :commands rust-mode
  :init
  (add-hook 'rust-mode-hook #'lsp-deferred))

;; Tex
(use-package tex
  :straight auctex
  :commands latex-mode
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
            "g s" #'pdf-sync-forward-search))

;; Easily create references in LaTex
(use-package reftex
  :commands turn-on-reftex
  :custom
  (reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

;; Bibliography management
(use-package ivy-bibtex
  :commands ivy-bibtex
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
  (local/custom bibtex-completion-notes-path))

;; Vimrc
(use-package vimrc-mode)

(require 'my-functions)
(require 'my-org-setup)
(require 'my-bindings)

(provide 'init)
;;; init.el ends here
