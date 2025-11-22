;;; dynamic.el --- Dynamic generation test data

;;; Commentary:
;; Tests dynamic alist generation using let bindings and backquotes

;;; Code:

(let ((project-name "dynamic-project")
      (current-version "2.0.0")
      (build-number 42))
  `((name . ,project-name)
    (version . ,current-version)
    (build . ,build-number)
    (timestamp . ,(format-time-string "%Y-%m-%d"))
    (computed . ,(* 2 21))))

;;; dynamic.el ends here
