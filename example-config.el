;;; example-config.el --- Example configuration using eldc

;;; Commentary:
;; This file demonstrates how to use eldc (Emacs Lisp Dictionary Converter)
;;
;; The last s-expression in the file will be evaluated and converted.
;; This can be a literal alist, a function call, a lambda, or any expression
;; that evaluates to an alist.
;;
;; Usage:
;;   1. Open this file in Emacs
;;   2. M-x load-file RET eldc.el RET
;;   3. M-x eldc-json  (creates example-config.json)
;;   4. M-x eldc-yaml  (creates example-config.yaml)

;;; Code:

'((name . "my-application")
  (version . "1.0.0")
  (description . "Example configuration file")
  (author . "Your Name")
  (dependencies
   ((react . "^18.2.0")
    (typescript . "^5.0.0")
    (vite . "^4.3.0")))
  (scripts
   ((dev . "vite")
    (build . "vite build")
    (preview . "vite preview")))
  (database
   ((host . "localhost")
    (port . 5432)
    (name . "mydb")
    (credentials
     ((username . "admin")
      (password . "secret"))))))

;;; example-config.el ends here
