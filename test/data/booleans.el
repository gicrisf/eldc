;;; booleans.el --- Test data for boolean encoding

;;; Commentary:
;; This file tests the encoding of boolean and null values.
;; Demonstrates the correct usage of t, :json-false, and nil.

;;; Code:

'((enabled . t)              ; JSON true
  (disabled . :json-false)   ; JSON false
  (optional . nil))          ; JSON null

;;; booleans.el ends here
