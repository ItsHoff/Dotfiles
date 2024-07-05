;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; Ensure garbage collection does not run during init.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar backup--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; We are using straight to manage packages, so don't enable package.
(setq package-enable-at-startup nil)
(tool-bar-mode -1)                  ; No toolbar
(scroll-bar-mode -1)                ; No scrollbar

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

(add-hook 'after-init-hook
          (lambda ()
            ;; Reset gc-cons-threshold to more sensible value.
            (setq gc-cons-threshold (* 10 1024 1024) ; 10 mb
                  gc-cons-percentage 0.1
                  ;; Restore file-name-handler-alist.
                  file-name-handler-alist backup--file-name-handler-alist)))
