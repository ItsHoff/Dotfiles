;;; local-util --- local definition utilities -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defconst local/variable-prefix "local-value/" "Prefix for locally set values.")

(defmacro local/defvar (variable value)
  "Define local VALUE for VARIABLE."
  `(defvar ,(intern (concat local/variable-prefix (symbol-name variable))) ,value))

(defmacro local/setq (variable)
  "Set VARIABLE to a local value if it has been defined."
  (let ((symbol (intern (concat local/variable-prefix (symbol-name variable)))))
    (when (boundp symbol)
      `(setq ,variable ,symbol))))

(defmacro local/custom (variable)
  "Set VARIABLE to a local value if it has been defined using custom."
  (let ((symbol (intern (concat local/variable-prefix (symbol-name variable)))))
    (when (boundp symbol)
      `(customize-set-variable ',variable ,(symbol-value symbol) "Customized by local-util"))))

(defun local/setenv (variable value)
  "Set environment variable VARIABLE to VALUE if it has not been set already."
  (prin1 (getenv variable))
  (when (null (getenv variable))
      (setenv variable value)))

(provide 'local-util)
;;; local-util.el ends here
