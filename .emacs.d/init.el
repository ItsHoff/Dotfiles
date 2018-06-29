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

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install all packages automatically
(setq use-package-always-ensure t)

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
(setq select-enable-clipboard nil) ; Disable emacs clipboard and rely on evil
(put 'dired-find-alternate-file 'disabled nil) ; Allow dired to use the same buffer
(setq completion-styles '(basic initials partial substring)) ; Better completion
(global-auto-revert-mode t)         ; Automatically reload changed files
(setq gc-cons-threshold 20000000)   ; This should reduce emacs gc time
(fset 'yes-or-no-p 'y-or-n-p)       ; y or n should suffice
(setq-default fill-column 110)      ; Line wrap column
(setq large-file-warning-threshold 50000000)  ; Allow larger files to be opened without confirmation

; Reverse the splitting order from default
; First try vertical split and only then horizontal split
(setq split-window-preferred-function 'my/split-window-sensibly)

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
   (add-to-list 'default-frame-alist '(font . "Consolas-11")))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
   (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11")))
  )

;; Tabs & Spaces
(setq-default tab-always-indent nil)    ; Allow tabbing outside of indent
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tabs
(setq-default tab-width 4)              ; Tab = 4 spaces
(setq-default evil-shift-width tab-width)

;; Save custom settings to another file and load it
(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file 'noerror)

;; OS Specifics--------------------------------------------------------------------------------

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

;; HOOKS ---------------------------------------------------------------------------------------

; Auto-save on focus lost
(add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))

; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; PACKAGES ------------------------------------------------------------------------------------

(use-package general
  :init
  (general-evil-setup)
  (general-override-mode))

(use-package evil
  :init
  (setq evil-want-integration nil) ; Use evil-collection for integration
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
  ; Allow c-o and c-i to jump to buffers matching the regexp
  (setq evil--jumps-buffer-targets "\\`magit")
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
  (evil-declare-not-repeat #'ivy-done)
  (evil-declare-not-repeat #'ivy-alt-done)
  :general
  (:keymaps '(motion normal)
            "C-f" #'counsel-find-file
            )
  (:keymaps 'evil-ex-map
            "b SPC" #'ivy-switch-buffer
            "e SPC" #'counsel-find-file)
  (:keymaps 'ivy-minibuffer-map
            "C-h" #'ivy-alt-done
            "<escape>" #'minibuffer-keyboard-quit))

(use-package diminish)

(use-package desktop
  :custom (desktop-save-mode t))

(use-package eldoc
  :diminish eldoc-mode)

(use-package evil-collection
  :after evil)

(use-package evil-matchit
  :after evil
  :init (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :config
  (define-key evil-inner-text-objects-map "c" 'evilnc-inner-comment)
  (define-key evil-outer-text-objects-map "c" 'evilnc-outer-commenter)
  :general
  (:keymaps '(normal visual)
            "; c" #'evilnc-comment-or-uncomment-lines
            ))

(use-package evil-numbers
  :after evil
  :general
  (:keymaps 'normal
            "; i" #'evil-numbers/inc-at-pt
            "; d" #'evil-numbers/dec-at-pt
            ))

(use-package evil-visualstar
  :after evil
  :init (global-evil-visualstar-mode)
  :general
  (:keymaps 'visual
            "#" #'evil-visualstar/begin-search-forward
            "¤" #'evil-visualstar/begin-search-backward))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flx)

(use-package framegroups
  :ensure nil
  :pin manual
  :commands fg-switch
  :init
  (defvar my/framegroups-command-map (make-sparse-keymap))
  (fset 'my/framegroups-command-map my/framegroups-command-map)
  (defun my/framegroup-setup (name &rest _)
    "Set up default framegroup layouts."
    (interactive)
    (pcase name
      ;; emacs configuration
      ("emacs"
       (find-file "~/.emacs.d/init/bind.el")
       (split-window-right)
       (find-file "~/.emacs.d/init.el")
       (set-frame-parameter nil 'fullscreen 'maximized))
      (_
       (set-frame-parameter nil 'fullscreen 'maximized))
      ))
  (add-hook 'fg-create-hook #'my/framegroup-setup)
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
  (:keymaps 'motion
            "<C-m>" #'helm-mini
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
  (ispell-silently-savep t))

(use-package magit
  :init
  (use-package evil-magit)
  (require 'evil-magit)
  :config
  (evil-add-command-properties #'magit-diff-visit-file :jump t)
  :general
  (:keymaps 'magit-mode-map
            "<up>" #'magit-section-backward
            "<down>" #'magit-section-forward
            "<left>" #'magit-section-backward-sibling
            "<right>" #'magit-section-forward-sibling
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
  (add-hook 'org-mode-hook (lambda () (my/set-tab-width 2)))
  :config
  (setq org-M-RET-may-split-line '(default . nil)) ; Don't split line automatically
  (evil-make-overriding-map org-mode-map 'motion)
  (advice-add #'org-indent-line :after #'my/org-indent-advice)
  :general
  (:keymaps 'org-mode-map
            :states '(normal visual)
            "<down>" #'outline-next-visible-heading
            "<up>" #'outline-previous-visible-heading
            "<right>" #'org-forward-heading-same-level
            "<left>" #'org-backward-heading-same-level
            "M-h" #'org-metaleft
            "M-l" #'org-metaright
            "M-j" #'org-metadown
            "M-k" #'org-metaup
            ))

(use-package origami)

(use-package outline
  :ensure nil
  :init
  (add-hook 'prog-mode-hook (lambda () (outline-minor-mode)))
  :config
  (evil-make-overriding-map outline-minor-mode-map 'motion)
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
  :config
  (setq projectile-indexing-method 'alien) ; Required tools should be installed on windows aswell
  (setq projectile-completion-system 'ivy)
  :general
  (:keymaps 'projectile-command-map
            "A" (lambda () (interactive) (projectile-add-known-project (projectile-project-p)))
            )
  )

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 50)
  (recentf-mode 1))

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
  (advice-add #'undo-tree-undo-1 :filter-args #'my/advice-preserve-timestamps)
  (advice-add #'undo-tree-redo-1 :filter-args #'my/advice-preserve-timestamps)
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
  (use-package cargo
    :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

  (use-package flycheck-rust
    :init (add-hook 'rust-mode-hook #'flycheck-rust-setup))

  (use-package racer
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

;; Tex
(use-package tex
  :ensure auctex
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (modify-syntax-entry ?_ "w" TeX-mode-syntax-table) ; _ is now part of a word
  (modify-syntax-entry ?- "w" TeX-mode-syntax-table) ; - is now part of a word
  (require 'pdf-sync)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook (lambda () (outline-minor-mode)
                               (flyspell-mode)
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
  (bibtex-completion-notes-path "~/thesis/bibliography/notes.org")
  (bibtex-completion-bibliography "~/thesis/bibliography/bibliography.bib")
  (bibtex-completion-library-path "~/Google Drive File Stream/My Drive/papers")
  (bibtex-completion-pdf-open-function #'org-open-file-with-system)
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-format-citation-functions
   '((org-mode . bibtex-completion-format-citation-org-link-to-PDF)
   (latex-mode . bibtex-completion-format-citation-cite)
   (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
   (default . bibtex-completion-format-citation-default)))
  )

;; Vimrc
(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(load "bind")
(load "local-conf" 'noerror) ; No error if missing

(provide 'init)
;;; init.el ends here
