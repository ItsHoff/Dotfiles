;;; local-util --- local definition utilities -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Example usage:
;;;
;;; local-conf.el
;;; (require 'local-util)
;;; (local/defvar ispell-program-name "c:/tools/msys64/mingw64/bin/hunspell.exe")
;;;
;;; init.el
;;; (use-package ispell
;;;   :config
;;;   (local/custom ispell-program-name))

;;; Code:
(defconst local/variable-prefix "local-value/" "Prefix for locally set values.")

(defmacro local/defvar (variable value)
  "Define a local VALUE for a VARIABLE."
  `(defvar ,(intern (concat local/variable-prefix (symbol-name variable))) ,value))

(defmacro local/setq (variable)
  "Set a VARIABLE to a local value if a local value has been defined."
  (let ((symbol (intern (concat local/variable-prefix (symbol-name variable)))))
    (when (boundp symbol)
      `(setq ,variable ,symbol))))

(defmacro local/custom (variable)
  "Set VARIABLE to a local value using custom if a local value has been defined."
  (let ((symbol (intern (concat local/variable-prefix (symbol-name variable)))))
    (when (boundp symbol)
      `(customize-set-variable ',variable ,(symbol-value symbol) "Customized by local-util"))))

(defun local/setenv (variable value)
  "Set environment variable VARIABLE to VALUE if the environment variable has not been set already."
  (prin1 (getenv variable))
  (when (null (getenv variable))
      (setenv variable value)))

(provide 'local-util)
;;; local-util.el ends here
