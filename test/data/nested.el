;;; nested.el --- Nested structure test data

;;; Commentary:
;; Nested alist for testing complex structures

;;; Code:

'((name . "nested-test")
  (config . ((key . "value")
             (number . 42)
             (flag . t)))
  (arrays . ((tags . ["one" "two" "three"])
             (numbers . [1 2 3])))
  (metadata . ((author . "Test Author")
               (license . "MIT"))))

;;; nested.el ends here
