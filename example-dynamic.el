;;; example-dynamic.el --- Dynamic configuration example for eldc

;;; Commentary:
;; This example demonstrates using Emacs Lisp expressions to dynamically
;; generate configuration data. The last s-expression is evaluated to
;; produce the alist that gets converted.

;;; Code:

;; Example: Using let and backquote for dynamic values
(let ((app-name "my-dynamic-app")
      (build-date (format-time-string "%Y-%m-%d"))
      (emacs-ver emacs-version))
  `((name . ,app-name)
    (version . "2.0.0")
    (buildDate . ,build-date)
    (environment
     ((emacsVersion . ,emacs-ver)
      (platform . ,(symbol-name system-type))
      (user . ,(user-login-name))))
    (features
     ((enabled . t)
      (count . ,(length features))))))

;;; example-dynamic.el ends here
