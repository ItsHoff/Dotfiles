;;; init.el --- My emacs config -*- lexical-binding: t -*-
;;; Commentary:
;;; My .emacs

;;; Code:

;; Save custom settings to another file so they don't mess up the init file
(setopt custom-file "~/.emacs.d/custom.el")

;; Load only specified variables from `custom-file'.
;; This is done to avoid customizations that were removed from init.el
;; from being loaded from custom.el.
(defun my/load-custom-file ()
  "Load only whitelisted variables from `custom-file'."
  (when (file-readable-p custom-file)
    (let ((custom-expressions
           ;; Extract the expressions from `custom-file'.
           (with-temp-buffer
             (save-excursion
               (insert-file-contents custom-file)
               (goto-char (point-max)))
             ;; Ignore the custom-set-variables call.
             (cdr (read (current-buffer)))))
          (variable-whitelist '(safe-local-variable-values)))
      ;; Load expression if it is in the whitelist.
      (dolist (expression custom-expressions)
        ;; Remove the quote.
        (setq expression (cadr expression))
        (when (memq (car expression) variable-whitelist)
          (custom-set-variables expression))))))
(with-demoted-errors "Error loading custom-file: %S" (my/load-custom-file))

(add-to-list 'load-path "~/.emacs.d/init/")

;;; STRAIGHT -----------------------------------------------------------------------------------

(custom-set-variables '(straight-use-package-by-default t)
                      '(straight-check-for-modifications '(check-on-save find-when-checking))
                      '(straight-repository-branch "master"))

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

;; Workaround https://github.com/radian-software/straight.el/issues/701.
(setopt find-file-visit-truename nil)

(straight-use-package 'use-package)

;;; ANALYSIS ------------------------------------------------------------------------------------

(use-package benchmark-init
  :disabled ; enable for benchmarking
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Report culprits for long pauses
(use-package explain-pause-mode
  :disabled ; Blocked emacs when tried 16.11.21
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :diminish explain-pause-mode
  :custom (explain-pause-logging-default-log-location (expand-file-name "explain-pause-log.socket" user-emacs-directory)))

;;; GENERAL SETTINGS ----------------------------------------------------------------------------

(setopt visible-bell t)               ; No error beep
(electric-indent-mode -1)             ; Handle indendation elsewhere
(setopt inhibit-startup-screen t)     ; No message at startup
(modify-syntax-entry ?_ "w" (standard-syntax-table)) ; _ is now part of a word
(modify-syntax-entry ?- "w" (standard-syntax-table)) ; aswell as -
(setopt select-enable-clipboard nil)  ; Disable emacs clipboard and rely on evil
(fset 'yes-or-no-p 'y-or-n-p)         ; y or n should suffice for confirmation
(setopt fill-column 110)      ; Line wrap column
(setopt large-file-warning-threshold 50000000)  ; Allow larger files to be opened without confirmation
(setopt history-length 1000)          ; Increase the amount of history
(setopt create-lockfiles nil)         ; Don't create lockfiles
(setopt auto-save-default nil)        ; No auto-saves
(setopt sentence-end-double-space nil) ; Don't require double space at end of sentence.
(setopt switch-to-buffer-obey-display-actions t) ; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setopt require-final-newline t)      ; Require new line at the end-of-file.
(setopt bookmark-save-flag 1)         ; Save bookmarks every time it is modified.
(setopt use-package-hook-name-suffix nil) ; Don't append -hook to :hook definitions

;; Start a server if it is not already running.
(require 'server)
(unless (server-running-p) (server-start))

;; Performance suggestions from lsp (https://github.com/emacs-lsp/lsp-mode#performance)
(setopt read-process-output-max (* 1024 1024)) ; 1 mb

;; Increase stack size (https://www.gnu.org/software/emacs/manual/html_mono/eintr.html#fn-13)
(setopt max-lisp-eval-depth 8000)

;; Don't compact font caches. Will consume more memory, but improves performance.
(setopt inhibit-compacting-font-caches t)

;; Use nicer window splitting method for automatic splits
(defun my/split-only-root (&optional window)
  "Split WINDOW only if there are no existing splits.
Perform the split along the longest axis."
  (when (one-window-p t)
    (if (> (window-pixel-height) (window-pixel-width))
        (with-selected-window window
          (split-window-below))
      (with-selected-window window
        (split-window-right)))))

(setopt split-window-preferred-function #'my/split-only-root)

;; Make C-i and C-m different from <tab> and <return>
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

;; Just define § as escape so things are sensible on bigger keyboards
(define-key input-decode-map [?§] [escape])

;; UTF-8 please
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setopt buffer-file-coding-system 'utf-8-unix)

;; Smooth scrolling
(setopt scroll-step 1)
(setopt scroll-margin 5)
(setopt scroll-conservatively 10000)
(setopt auto-window-vscroll nil)
(setopt hscroll-step 1)
(setopt hscroll-margin 5)

;; Tabs & Spaces
(setopt tab-always-indent 'complete)    ; Allow tabbing outside of indent
(setopt indent-tabs-mode nil)     ; Use spaces instead of tabs
;; Does this work correctly with setopt instead of setq-default?
(setopt tab-width 4)              ; Tab = 4 spaces
(setopt evil-shift-width tab-width)

;; Put backups in .emacs.d
(defvar backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
(defvar undo-directory (concat user-emacs-directory "undos"))
(if (not (file-exists-p undo-directory))
    (make-directory undo-directory t))
(setopt backup-directory-alist `(("." . ,backup-directory)))

;; Fonts
(cond
 ((find-font (font-spec :name "Consolas"))
  (add-to-list 'default-frame-alist '(font . "Consolas-11")))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))))

;;; SYSTEM SETUP --------------------------------------------------------------------------------

;; Windows
;;(when (member system-type '(ms-dos windows-nt cygwin)))

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
  (evil-undo-system 'undo-tree)
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
  (evil-declare-not-repeat #'push-button)
  (evil-add-command-properties #'find-file :jump t :repeat nil)
  (evil-add-command-properties #'switch-to-buffer :jump t :repeat nil)

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

;; Run code formatter on buffer contents.
(use-package apheleia
  :diminish apheleia-mode
  :custom
  (apheleia-formatters-respect-indent-level nil) ; Defer indent level to the formatters.
  :config
  ;; https://github.com/radian-software/apheleia/issues/108
  ;; 23.1.23 Bug is reportedly fixed.
  ;; (setf (alist-get 'clang-format apheleia-formatters)
  ;;       '("clang-format" file))
  ;; https://github.com/radian-software/apheleia/issues/150
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier)
  (apheleia-global-mode t))

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
  :after evil-collection
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
  (dotimes (i 10)
    (general-define-key :keymaps 'company-active-map
                        (format "C-%d" i) #'company-complete-number))
  (setq company-backends (remove #'company-clang company-backends))
  (evil-collection-company-setup)
  (global-company-mode)
  :general
  (:keymaps 'company-active-map
            "C-h" nil
            "C-<return>" #'newline-and-indent))

;; Fuzzy matching for company
(use-package company-flx
  :after company
  :config (company-flx-mode t))

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
  :init
  (add-hook 'compilation-mode-hook (lambda () (setq truncate-lines nil)))
  :config
  (evil-collection-compile-setup))

(use-package corfu
  :disabled ; 17.2.23 comparing with company, corfu didn't integrate properly with evil .
  :demand t
  :after evil-collection
  :straight (:files (:defaults "extensions/*"))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  :config
  (evil-collection-corfu-setup)
  (evil-make-overriding-map corfu-map 'insert)
  (global-corfu-mode)
  (corfu-indexed-mode))

;; Minibuffer completion framework
(use-package counsel
  :disabled ; trying out vertico
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
  (evil-add-command-properties #'ivy-occur-press-and-switch :jump t)
  (evil-collection-ivy-setup)
  ;; counsel-mode replaces built in commands with counsel alternatives
  (counsel-mode t)
  :general
  (:keymaps 'ivy-minibuffer-map
            "C-h" #'ivy-alt-done
            "<escape>" #'minibuffer-keyboard-quit)
  (:keymaps 'ivy-occur-grep-mode-map
            "n" nil)) ; Conflicts with search (was next-error-no-select)

;; Completion framework behind counsel
(use-package ivy
  :disabled ; trying out vertico
  :after (counsel evil-collection)
  :diminish ivy-mode
  ;; ivy-mode replaces completing-read-function
  :config
  (ivy-mode t)
  (evil-collection-ivy-setup))

;; Enhanced M-x interface
(use-package amx
  :disabled ; trying out vertico
  :after counsel)

;; A collection of opinionated keyboard-driven user interfaces for various built-in Emacs modes.
(use-package casual
  :commands casual-dired-tmenu)

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
  :custom
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  :config
  (evil-collection-dired-setup)
  (put 'dired-find-alternate-file 'disabled nil) ; Allow dired to use the same buffer
  (evil-add-command-properties #'dired-jump :jump t)
  (evil-declare-not-repeat #'dired-jump)
  (evil-declare-not-repeat #'dired-next-line)
  (evil-declare-not-repeat #'dired-previous-line)
  (evil-declare-not-repeat #'dired-find-file)
  :general
  (:keymaps 'dired-mode-map
            "M-o" #'casual-dired-tmenu))

;; Display line numbers
(use-package display-line-numbers
  :straight (:type built-in)
  :commands display-line-numbers-mode
  :custom (display-line-numbers-type 'visual)
  :init (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(use-package doom-themes
  :disabled ; 20.3.2020
  :demand t
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; (doom-themes-org-config) to try out
  (load-theme 'doom-nord t))

;; Shows documentation about symbol under point on the echo area
(use-package eldoc
  :after evil-collection
  ;; LSP-mode messes with eldoc that is bundled with emacs 27, so use a newer version from package repository.
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3295#issuecomment-1308994099
  ;; :straight (:type built-in)
  :diminish eldoc-mode
  :config (evil-collection-eldoc-setup))

;; Shows search matches on modeline
(use-package evil-anzu
  :after evil
  :custom
  (anzu-minimum-input-length 3)
  (anzu-cons-mode-line-p nil))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init '((custom cus-edit) debug eshell help (package-menu package) xref)))

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

;; Tree-sitter powered textobjects for evil mode in Emacs.
;; (use-package evil-textobj-tree-sitter
;;   :after evil
;;   :general
;;   (:keymaps 'evil-normal-state-map
;;             "<down>" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer"))
;;             "<up>" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t))))

;; Start a * or # search from the visual selection
(use-package evil-visualstar
  :after evil
  :config (global-evil-visualstar-mode))

;; Enchances compilation-mode
(use-package fancy-compilation
  :config (fancy-compilation-mode))

;; On the fly syntax checking
(use-package flycheck
  :after evil-collection
  :diminish flycheck-mode
  :commands flycheck-mode
  :init
  (add-hook 'prog-mode-hook (lambda () (flycheck-mode)))
  (add-hook 'flycheck-error-list-mode-hook (lambda () (setq truncate-lines nil)))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (evil-collection-flycheck-setup))

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
  (setopt window-combination-resize t)
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
    (add-to-list 'golden-ratio-exclude-buffer-names n))
  (add-to-list 'golden-ratio-inhibit-functions
               (lambda ()
                 (and (boundp 'which-key--buffer)
                      (window-live-p (get-buffer-window which-key--buffer))))))

;; Move up and down the screen nicely
(use-package golden-ratio-scroll-screen
  :demand t
  :after evil
  :custom
  (golden-ratio-scroll-recenter nil)
  :config
  (evil-declare-motion #'golden-ratio-scroll-screen-down)
  (evil-declare-motion #'golden-ratio-scroll-screen-up))

;; Heavier 'minibuffer' completion
(use-package helm
  :disabled ; 10.1.23 replaced with vertico
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
  :disabled ; trying out vertico
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :config
  (evil-add-command-properties #'lsp-ivy-workspace-symbol :jump t)
  (evil-add-command-properties #'lsp-ivy-global-workspace-symbol :jump t))

;; LSP support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :diminish lsp-lens-mode
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil)
  (lsp-clients-clangd-args '("--header-insertion=never" "--suggest-missing-includes"))
  (lsp-volar-take-over-mode t)
  ;; Disable modeline diagnostics due to their poor performance.
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-log-max 3000)
  (lsp-log-io nil)
  (lsp-lens-enable nil)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode-hook . my/lsp-mode-setup-completion)
  :config
  (defun my/goto-definition-lsp (_string _position)
    (when (bound-and-true-p lsp-mode)
      (not (stringp (lsp-find-definition)))))
  (setopt evil-goto-definition-functions (delete #'evil-goto-definition-search evil-goto-definition-functions))
  (add-to-list 'evil-goto-definition-functions #'my/goto-definition-lsp)
  (delete 'lsp-terraform lsp-client-packages) ; due to https://github.com/emacs-lsp/lsp-mode/issues/3577
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
  :defer 10
  :after evil-collection
  :commands (magit-status magit-dispatch magit-blame-addition)
  :custom
  (magit-diff-paint-whitespace-lines 'all)
  (magit-diff-refine-hunk 'all)
  (evil-collection-magit-want-horizontal-movement nil) ; Disabled due to https://github.com/emacs-evil/evil-collection/issues/831
  :init
  ;; Non customizable variables
  (setq magit-bind-magit-project-status nil)
  (setq magit-stash-read-message-function #'magit-stash-read-message-traditional)
  (setq magit-show-long-lines-warning nil)
  :config
  (evil-collection-magit-setup)
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

;; NPM client for emacs
(use-package npm)

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

;; Profiler
(use-package profiler
  :after evil-collection
  :straight (:type built-in)
  :config
  (evil-collection-profiler-setup))

;; Project management
(use-package projectile
  :demand t
  :custom
  (projectile-indexing-method 'alien) ; Required tools should be installed on windows as well
  ;; (projectile-completion-system 'ivy) ; trying out vertico
  (projectile-git-submodule-command nil) ; Submodules seem to be causing issues, so disabling for now 1.10.20
  :config
  (evil-add-command-properties #'projectile-find-file :jump t :repeat nil)
  (evil-add-command-properties #'projectile-find-other-file :jump t :repeat nil)
  (evil-add-command-properties #'projectile-find-other-file-other-window :jump t :repeat nil)
  ;; Configure other file support for React.
  (add-to-list 'projectile-other-file-alist '("tsx" "module.scss" "scss"))
  (add-to-list 'projectile-other-file-alist '("scss" "tsx"))
  (add-to-list 'projectile-other-file-alist '("module.scss" "tsx"))
  ;; And WPF.
  (add-to-list 'projectile-other-file-alist '("xaml" "xaml.cs"))
  (add-to-list 'projectile-other-file-alist '("xaml.cs" "xaml"))
  (projectile-mode t)
  :general
  (:keymaps 'projectile-command-map
            "ESC" nil
            "s r" #'consult-ripgrep
            "s f" #'consult-lsp-file-symbols
            "s s" #'consult-lsp-symbols))

(use-package counsel-projectile
  :disabled ; trying out vertico
  :after projectile
  :config
  (evil-add-command-properties #'counsel-projectile-find-file :jump t :repeat nil)
  ;; counsel-projectile-switch-project is super slow for some reason, so don't override it.
  (setopt counsel-projectile-key-bindings (assq-delete-all 'projectile-switch-project counsel-projectile-key-bindings))
  (counsel-projectile-mode t))

;; Save recently visited files between sessions
(use-package recentf
  :demand t
  :straight (:type built-in)
  :custom
  (recentf-max-saved-items 100)
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
  (setopt spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
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
  (require 'solarized-palettes)
  ;; Check out child-theme-example for how to probably do this better.
  (solarized-with-color-variables 'dark 'solarized-dark
    solarized-dark-color-palette-alist
    '((custom-theme-set-faces
       theme-name
       ;; Copy markdown-header-faces from org-level faces
       `(markdown-header-face-1 ((,class (:inherit ,s-variable-pitch :foreground ,orange :height ,solarized-height-plus-4))))
       `(markdown-header-face-2 ((,class (:inherit ,s-variable-pitch :foreground ,green :height ,solarized-height-plus-3))))
       `(markdown-header-face-3 ((,class (:inherit ,s-variable-pitch :foreground ,blue :height ,solarized-height-plus-2))))
       `(markdown-header-face-4 ((,class (:inherit ,s-variable-pitch :foreground ,yellow :height ,solarized-height-plus-1))))
       `(markdown-header-face-5 ((,class (:inherit ,s-variable-pitch :foreground ,cyan))))
       `(markdown-header-face-6 ((,class (:inherit ,s-variable-pitch :foreground ,green)))))))
  ;; Disable mode-line over and underlines
  (set-face-attribute 'mode-line nil :overline 'unspecified :underline 'unspecified)
  (set-face-attribute 'mode-line-inactive nil :overline 'unspecified :underline 'unspecified))

;; SSH agent support for emacs
(use-package ssh-agency
  :disabled t ; 23.10.19
  :commands ssh-agency-ensure)

;; Automatic installation, usage, and fallback for tree-sitter major modes in Emacs 29
(use-package treesit-auto
  :custom
  ;; Auto install does not work on Windows.
  (treesit-auto-install (if (member system-type '(ms-dos windows-nt cygwin))
                            nil
                          'prompt))
  :config
  ;; glsl-ts-mode got added with nil to auto-mode-alist. 4.12.24
  (delete 'glsl treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist nil)
  (global-treesit-auto-mode))

;; Tree sitter syntax highlighting
(use-package tree-sitter
  :disabled t ; 2.1.24 migrate to 29.1
  :hook ((prog-mode-hook . turn-on-tree-sitter-mode)
         (tree-sitter-mode-hook . tree-sitter-hl-mode)))
(use-package tree-sitter-langs
  :disabled t ; 2.1.24 migrate to 29.1
  :after tree-sitter)

;; Vim like undo
(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,undo-directory)))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-lazy-drawing nil) ; Change this to improve perf
  ;; Disable region undo since it seems to be flaky
  (undo-tree-enable-undo-in-region nil)
  :config
  (global-undo-tree-mode)
  ;; 10.8.19
  ;; (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)

  (defun my/advice-preserve-timestamps (args)
    "Change preserve-timestamps in ARGS to t.
     Filters arguments for undo-tree-undo-1 and undo-tree-redo-1.
     Advice type: filter-args."
    (list (nth 0 args) (nth 1 args) t))

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
  :after evil-collection
  :diminish which-key-mode
  :init (which-key-mode)
  :config (evil-collection-which-key-setup))

;; Visualize extra whitespace
;; Could be used to also clean whitespace
(use-package whitespace
  :demand t
  :straight (:type built-in)
  :diminish global-whitespace-mode
  :custom
  (whitespace-style '(trailing tabs space-before-tab tab-mark))
  ;; Change tab mark (this removes space and newline marks)
  (whitespace-display-mappings '((tab-mark ?\t [?… ?… ?… ?╷] [?▸ ?\t] [?› ?\t] [?> ?\t])))
  :hook
  (prog-mode-hook . (lambda ()
                      (setq-local whitespace-style
                                  '(face trailing tabs space-before-tab tab-mark))))
  :config
  ;; Tabs shouldn't glow red.
  (set-face-attribute 'whitespace-tab nil :inherit 'whitespace-space :foreground 'unspecified
                      :inverse-video 'unspecified :slant 'normal)
  (global-whitespace-mode))

;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (defvar my/yas-command-map (make-sparse-keymap))
  (fset 'my/yas-command-map my/yas-command-map)
  :config
  (yas-global-mode)
  :general
  (:keymaps 'my/yas-command-map
            "y" #'consult-yasnippet
            "n" #'yas-new-snippet
            "v" #'yas-visit-snippet-file))

;; Consult integration for yasnippet
(use-package consult-yasnippet
  :after yasnippet)

;; Ivy integration for yasnippet
(use-package ivy-yasnippet
  :disabled ; trying out vertico
  :after yasnippet)

;; Yasnippet snippets
(use-package yasnippet-snippets
  :after yasnippet)

;; Writable grep
(use-package wgrep)

;;; PROGRAMMING MODES ---------------------------------------------------------------------------

(defun my/set-tab-width (width)
  "Set 'tab-width' to WIDTH."
  (setq-local tab-width width)
  (setq-local evil-shift-width width))

;; AutoHotkey
(use-package ahk-mode
  :custom (ahk-indentation 4))

;; C and C++
(use-package cc-mode
  :commands (c++-mode c-mode java-mode)
  :straight (:type built-in)
  :custom
  (c-default-style "bsd")
  (c-basic-offset 4)
  (c-tab-always-indent 'complete)
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

(use-package c-ts-mode
  :commands (c++-ts-mode c-ts-mode c-or-c++-ts-mode)
  :straight (:type built-in)
  :custom
  (c-ts-mode-indent-offset 4)
  :init
  (add-hook 'c-ts-base-mode-hook #'lsp-deferred)
  :config
  (modify-syntax-entry ?_ "w" c-ts-mode--syntax-table)) ; _ is now part of a word

;; CMake
(use-package cmake-mode
  :commands cmake-mode
  :custom (cmake-tab-width 4))

(use-package cmake-ts-mode
  :straight (:type built-in))

;; C#
(use-package csharp-mode
  :straight (:type built-in)
  :commands (csharp-mode csharp-ts-mode)
  :init
  (dolist (hook '(csharp-ts-mode-hook
                  csharp-mode-hook))
    (add-hook hook (lambda ()
                     (my/set-tab-width 4)
                     (lsp-deferred)
                     ;; csharp-ls didn't provide any formatting results. 28.3.24
                     (setq-local lsp-enable-indentation nil))))
  :config
  (modify-syntax-entry ?_ "w" csharp-mode-syntax-table)) ; _ is now part of a word

;; CSS
(use-package css-mode
  :straight (:type built-in)
  :init
  (add-hook 'css-base-mode-hook #'lsp-deferred)
  :config
  (modify-syntax-entry ?- "w" css-mode-syntax-table)) ; - is now part of a word

;; Git
(use-package git-modes
  :commands (gitattributes-mode gitconfig-mode gitignore-mode))

;; GLSL
(use-package glsl-mode
  :commands glsl-mode
  :mode "\\.shader\\'")

;; Groovy / Gradle
(use-package groovy-mode
  :commands groovy-mode
  :mode "\\.Jenkinsfile\\'")

;; ELisp
(use-package elisp-mode
  :after evil-collection
  :straight (:type built-in)
  :commands emacs-lisp-mode
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (my/set-tab-width 2)))
  :config
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table) ; aswell as -
  (evil-collection-elisp-mode-setup)
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
            :states 'normal
            "C-c C-c" #'eval-buffer)
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
            :states 'visual
            "C-c C-c" #'eval-region))

;; JSON
(use-package json-mode
  :disabled t ; 2.1.2024 Hijacks auto-mode-alist
  :commands json-mode)

(use-package json-ts-mode
  :straight (:type built-in)
  :commands json-ts-mode)

;; Kotlin
(use-package kotlin-mode
  :commands kotlin-mode
  :after evil-collection
  :init
  ;; Disabled 19.10.21 kotlin lsp is not very good yet.
  ;; (add-hook 'kotlin-mode-hook #'lsp-deferred)
  :config
  (evil-collection-kotlin-mode-setup)
  (local/custom lsp-clients-kotlin-server-executable))

;; Lua
(use-package lua-mode
  :commands lua-mode)

;; Markdown
(use-package markdown-mode
  :after evil-collection
  :commands markdown-mode
  :custom
  (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (markdown-asymmetric-header t)
  :config
  (evil-collection-markdown-mode-setup))

(use-package markdown-preview-mode
  :commands markdown-preview-mode)

;; Octave / Matlab
(use-package octave-mode
  :straight (:type built-in)
  :mode "\\.m\\'"
  :commands octave-mode
  :init (add-hook 'octave-mode-hook (lambda () (my/set-tab-width 2))))

;; Python
(use-package anaconda-mode
  :after evil-collection
  :diminish anaconda-mode
  :commands anaconda-mode
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook (lambda () (my/set-tab-width 4)))
  :config
  (evil-collection-anaconda-mode-setup))

(use-package company-anaconda
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package python-mode
  :after evil-collection
  :straight (:type built-in)
  :mode "SConstruct"
  :config (evil-collection-python-setup))

;; RST
(use-package rst
  :straight (:type built-in)
  :init (add-hook 'rst-mode-hook (lambda () (my/set-tab-width 2))))

;; Rust
(use-package rustic
  :commands rustic-mode
  :init
  (add-hook 'rustic-mode-hook #'lsp-deferred))

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
  :disabled ; trying out vertico
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

;; Typescript
(use-package typescript-mode
  :disabled ; 18.1.24 Prefer tree-sitter
  :after (tree-sitter evil-collection)
  :init
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  ;; Separate mode for tree sitter ts and tsx support.
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript/TSX (TypeScript)")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  :config
  (evil-collection-typescript-mode-setup)
  (modify-syntax-entry ?_ "w" typescript-mode-syntax-table)) ; _ is now part of a word

(use-package typescript-ts-mode
  :straight (:type built-in)
  :init
  (add-hook 'typescript-ts-base-mode-hook #'lsp-deferred)
  :config
  (modify-syntax-entry ?_ "w" typescript-ts-mode--syntax-table)) ; _ is now part of a word

;; Vimrc
(use-package vimrc-mode)

;; Web
(use-package web-mode
  :commands web-mode
  :mode ("\\.vue\\'")
  :custom
  (web-mode-part-padding 0)
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  ;; workaround: https://github.com/emacs-tree-sitter/tree-sitter-langs/issues/23#issuecomment-832815710
  (tree-sitter-hl-use-font-lock-keywords nil)
  :init
  (add-hook 'web-mode-hook #'lsp-deferred)
  ;; Disabled 9.9.2022 web-mode uncommenting and syntax highlighting worked badly with ts and react.
  ;; Switched to typescript-mode instead.
  ;; Separate mode for tree sitter ts and tsx support.
  ;; (define-derived-mode web-typescript-mode web-mode "TypeScript (Web)")
  ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-typescript-mode))
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(web-typescript-mode . typescript))
  ;; (define-derived-mode web-tsx-mode web-mode "TypeScript/TSX (Web)")
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-tsx-mode))
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(web-tsx-mode . tsx))
  :config
  (modify-syntax-entry ?_ "w" web-mode-syntax-table)) ; _ is now part of a word

;; Yaml
(use-package yaml-mode
  :after evil-collection
  :commands yaml-mode
  :config (evil-collection-yaml-mode-setup))

;;; Vertico and friends ---------------------------------------------------------------------------

;; Extra completing-read commands.
(use-package consult
  :after evil-collection
  :custom
  (consult-project-function (lambda (_) (projectile-project-root)))
  :config
  (consult-customize consult-buffer :preview-key nil)
  (evil-add-command-properties #'consult-buffer :jump t :repeat nil)
  (evil-collection-consult-setup))

(use-package consult-lsp
  :after consult
  :commands (consult-lsp-symbols consult-lsp-file-symbols)
  :config
  ;; Add thing-at-point to history (M-n) (https://github.com/gagbo/consult-lsp/pull/35).
  ;; Should be fixed 3.2.23
  ;; (consult-customize consult-lsp-symbols :add-history (consult--async-split-thingatpt 'symbol))
  (evil-add-command-properties #'consult-lsp-file-symbols :jump t :repeat nil)
  (evil-add-command-properties #'consult-lsp-symbols :jump t :repeat nil))

;; Choose a command to run based on what is near point, both during a minibuffer completion session
;; and in normal buffers.
(use-package embark
  :after evil-collection
  :commands embark-act
  :custom
  ;; Use completing read to select the command.
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators
   '(embark-minimal-indicator  ; default is embark-mixed-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :config
  (evil-collection-embark-setup))

;; Provides integration between embark and consult.
(use-package embark-consult
  :after (embark consult))

;; Adds information to  minibuffer completions.
(use-package marginalia
  :init
  (marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :demand nil
  :init
  (savehist-mode))

;; Provides an orderless completion style that divides the pattern into space-separated components, and
;; matches candidates that match all of the components in any order.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; This is causing mid path matches to be hidden when path start matches exist.
  ;; However, according to orderless docs basic needs to be tried first for TRAMP hostname completion to work.
  ;; (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))
;; 26.11.24 should be implemented in orderless now. https://github.com/oantolin/orderless?tab=readme-ov-file#style-dispatchers
;; (orderless-style-dispatchers '(flex-if-twiddle
;;                                without-if-bang))
;; :init
;; (defun flex-if-twiddle (pattern _index _total)
;;   (when (string-prefix-p "~" pattern)
;;     `(orderless-flex . ,(substring pattern 1))))

;; (defun without-if-bang (pattern _index _total)
;;   (cond
;;    ((equal "!" pattern)
;;     '(orderless-literal . ""))
;;    ((string-prefix-p "!" pattern)
;;     `(orderless-without-literal . ,(substring pattern 1))))))

;; Minimalistic vertical completion UI.
(use-package vertico
  :after evil-collection
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :config
  (evil-collection-vertico-setup)
  :general
  (:keymaps 'vertico-map
            "<backspace>" #'vertico-directory-delete-char
            "C-." #'embark-act))

;; Configure multiform extension.
(use-package vertico-multiform
  :after vertico
  :straight nil
  :demand t
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode))

(load "my-functions")
(load "my-org-setup")
(load "my-bindings")

(provide 'init)
;;; init.el ends here
