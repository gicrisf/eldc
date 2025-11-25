;;; eldc-test.el --- Tests for eldc.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for eldc (Emacs Lisp Dictionary Converter)

;;; Code:

(require 'ert)
(require 'eldc)
(require 'json)

;;; Test Helpers

(defvar eldc-test-data-dir
  (expand-file-name "data"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing test data files.")

(defun eldc-test--get-data-file (filename)
  "Get full path to test data FILENAME."
  (expand-file-name filename eldc-test-data-dir))

(defun eldc-test--cleanup-output-files (source-file)
  "Clean up generated output files for SOURCE-FILE."
  (let* ((base (file-name-sans-extension source-file))
         (json-file (concat base ".json"))
         (yaml-file (concat base ".yaml"))
         (xml-file (concat base ".xml")))
    (when (file-exists-p json-file)
      (delete-file json-file))
    (when (file-exists-p yaml-file)
      (delete-file yaml-file))
    (when (file-exists-p xml-file)
      (delete-file xml-file))))

(defun eldc-test--with-data-file (filename callback)
  "Open test data FILENAME, execute CALLBACK, then clean up outputs."
  (let ((data-file (eldc-test--get-data-file filename)))
    (unwind-protect
        (with-current-buffer (find-file-noselect data-file)
          (funcall callback))
      (eldc-test--cleanup-output-files data-file)
      (when (get-file-buffer data-file)
        (with-current-buffer (get-file-buffer data-file)
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(defun eldc-test--read-json-file (json-file)
  "Read and parse JSON-FILE, return as alist."
  (with-temp-buffer
    (insert-file-contents json-file)
    (goto-char (point-min))
    (json-read)))

(defun eldc-test--read-yaml-file (yaml-file)
  "Read YAML-FILE and return contents as string."
  (with-temp-buffer
    (insert-file-contents yaml-file)
    (buffer-string)))

(defun eldc-test--read-xml-file (xml-file)
  "Read XML-FILE and return contents as string."
  (with-temp-buffer
    (insert-file-contents xml-file)
    (buffer-string)))

;;; Tests for Helper Functions

(ert-deftest eldc-test-get-output-filename ()
  "Test output filename generation."
  (eldc-test--with-data-file
   "package.el"
   (lambda ()
     (let ((json-output (eldc--get-output-filename "json"))
           (yaml-output (eldc--get-output-filename "yaml")))
       (should (string-suffix-p "package.json" json-output))
       (should (string-suffix-p "package.yaml" yaml-output))))))

(ert-deftest eldc-test-get-output-filename-no-buffer ()
  "Test that get-output-filename errors when buffer has no file."
  (with-temp-buffer
    (should-error (eldc--get-output-filename "json"))))

(ert-deftest eldc-test-get-alist-literal ()
  "Test parsing literal alist from simple.el."
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((result (eldc--get-alist)))
       (should (equal (alist-get 'name result) "test-package"))
       (should (equal (alist-get 'version result) "1.0.0"))
       (should (equal (alist-get 'enabled result) t))
       (should (equal (alist-get 'disabled result) nil))))))

(ert-deftest eldc-test-get-alist-nested ()
  "Test parsing nested alist from nested.el."
  (eldc-test--with-data-file
   "nested.el"
   (lambda ()
     (let* ((result (eldc--get-alist))
            (config (alist-get 'config result))
            (arrays (alist-get 'arrays result)))
       (should (equal (alist-get 'name result) "nested-test"))
       (should (equal (alist-get 'key config) "value"))
       (should (equal (alist-get 'number config) 42))
       (should (equal (alist-get 'tags arrays) ["one" "two" "three"]))))))

(ert-deftest eldc-test-get-alist-dynamic ()
  "Test parsing dynamic alist with let binding from dynamic.el."
  (eldc-test--with-data-file
   "dynamic.el"
   (lambda ()
     (let ((result (eldc--get-alist)))
       (should (equal (alist-get 'name result) "dynamic-project"))
       (should (equal (alist-get 'version result) "2.0.0"))
       (should (equal (alist-get 'build result) 42))
       (should (equal (alist-get 'computed result) 42))
       (should (stringp (alist-get 'timestamp result)))))))

;;; Tests for Converter Discovery

(ert-deftest eldc-test-find-converter-debug ()
  "Debug converter discovery - show what paths are being checked."
  (let* ((binary-name (eldc--binary-name))
         (binary-path (expand-file-name binary-name eldc-binary-dir))
         (converter (eldc--find-converter)))
    (message "Binary name: %s" binary-name)
    (message "Binary directory: %s" eldc-binary-dir)
    (message "Full binary path: %s" binary-path)
    (message "Binary exists: %s" (file-exists-p binary-path))
    (message "Converter found: %s" converter)
    (should (stringp binary-name))
    (should (stringp eldc-binary-dir))))

;;; Tests for JSON Conversion

(ert-deftest eldc-test-json-simple ()
  "Test JSON conversion with simple.el."
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (eldc-json)
     (let* ((json-file (eldc--get-output-filename "json"))
            (json-data (eldc-test--read-json-file json-file)))
       (should (file-exists-p json-file))
       (should (equal (alist-get 'name json-data) "test-package"))
       (should (equal (alist-get 'version json-data) "1.0.0"))
       (should (equal (alist-get 'enabled json-data) t))
       ;; should this be equal to :json-false?
       (should (equal (alist-get 'disabled json-data) nil))))))

(ert-deftest eldc-test-json-nested ()
  "Test JSON conversion with nested.el."
  (eldc-test--with-data-file
   "nested.el"
   (lambda ()
     (eldc-json)
     (let* ((json-file (eldc--get-output-filename "json"))
            (json-data (eldc-test--read-json-file json-file))
            (config (alist-get 'config json-data))
            (arrays (alist-get 'arrays json-data)))
       (should (file-exists-p json-file))
       (should (equal (alist-get 'name json-data) "nested-test"))
       (should (equal (alist-get 'key config) "value"))
       (should (equal (alist-get 'number config) 42))
       (should (equal (alist-get 'flag config) t))
       (should (equal (alist-get 'tags arrays) ["one" "two" "three"]))
       (should (equal (alist-get 'numbers arrays) [1 2 3]))))))

(ert-deftest eldc-test-json-package ()
  "Test JSON conversion with package.el matches expected package.json."
  (eldc-test--with-data-file
   "package.el"
   (lambda ()
     (eldc-json)
     (let* ((json-file (eldc--get-output-filename "json"))
            (expected-file (eldc-test--get-data-file "package.json"))
            (json-data (eldc-test--read-json-file json-file))
            (expected-data (eldc-test--read-json-file expected-file)))
       (should (file-exists-p json-file))
       (should (equal (alist-get 'name json-data)
                      (alist-get 'name expected-data)))
       (should (equal (alist-get 'version json-data)
                      (alist-get 'version expected-data)))
       (should (equal (alist-get 'description json-data)
                      (alist-get 'description expected-data)))
       (should (equal (alist-get 'license json-data)
                      (alist-get 'license expected-data)))))))

(ert-deftest eldc-test-json-dynamic ()
  "Test JSON conversion with dynamic.el."
  (eldc-test--with-data-file
   "dynamic.el"
   (lambda ()
     (eldc-json)
     (let* ((json-file (eldc--get-output-filename "json"))
            (json-data (eldc-test--read-json-file json-file)))
       (should (file-exists-p json-file))
       (should (equal (alist-get 'name json-data) "dynamic-project"))
       (should (equal (alist-get 'version json-data) "2.0.0"))
       (should (equal (alist-get 'build json-data) 42))
       (should (equal (alist-get 'computed json-data) 42))
       (should (stringp (alist-get 'timestamp json-data)))))))

;;; Tests for YAML Conversion (requires converter)

(ert-deftest eldc-test-yaml-simple ()
  "Test YAML conversion with simple.el."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((yaml-file (eldc--get-output-filename "yaml"))
           (d (eldc-yaml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p yaml-file))
       (let ((yaml-content (eldc-test--read-yaml-file yaml-file)))
         (should (string-match-p "name: test-package" yaml-content))
         (should (string-match-p "version: ['\"]?1\\.0\\.0['\"]?" yaml-content)))))))

(ert-deftest eldc-test-yaml-package ()
  "Test YAML conversion with package.el."
  :tags '(:integration)
  (eldc-test--with-data-file
   "package.el"
   (lambda ()
     (let ((yaml-file (eldc--get-output-filename "yaml"))
           (d (eldc-yaml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p yaml-file))
       (let ((yaml-content (eldc-test--read-yaml-file yaml-file)))
         (should (string-match-p "name: eldc-converter" yaml-content))
         (should (string-match-p "version: ['\"]?0\\.0\\.1['\"]?" yaml-content))
         (should (string-match-p "js-yaml:" yaml-content)))))))

(ert-deftest eldc-test-roundtrip-json-yaml-json ()
  "Test roundtrip conversion: alist → JSON → YAML → JSON."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let* ((original-alist (eldc--get-alist))
            (json-file (eldc--get-output-filename "json"))
            (yaml-file (eldc--get-output-filename "yaml")))

       ;; Step 1: Convert to JSON
       (eldc-json)
       (should (file-exists-p json-file))
       (let ((json-data-1 (eldc-test--read-json-file json-file)))

         ;; Step 2: Convert to YAML
         (let ((d (eldc-yaml)))
           (deferred:sync! d))
         (should (file-exists-p yaml-file))

         ;; Step 3: Convert YAML back to JSON using the binary
         (let* ((yaml-content (eldc-test--read-yaml-file yaml-file))
                (yaml-b64 (base64-encode-string yaml-content t))
                (converter-path (car (eldc--find-converter)))
                (command (concat converter-path " --reverse " yaml-b64))
                (json-b64 (shell-command-to-string command))
                (json-string (base64-decode-string (string-trim json-b64)))
                (json-data-2 (json-read-from-string json-string)))

           ;; Verify: original alist matches final JSON
           (should (equal (alist-get 'name original-alist)
                         (alist-get 'name json-data-1)))
           (should (equal (alist-get 'name original-alist)
                         (alist-get 'name json-data-2)))
           (should (equal (alist-get 'version original-alist)
                         (alist-get 'version json-data-1)))
           (should (equal (alist-get 'version original-alist)
                         (alist-get 'version json-data-2)))
           (should (equal (alist-get 'enabled original-alist)
                         (alist-get 'enabled json-data-1)))
           (should (equal (alist-get 'enabled original-alist)
                         (alist-get 'enabled json-data-2)))

           ;; Verify: first JSON matches final JSON
           (should (equal (alist-get 'name json-data-1)
                         (alist-get 'name json-data-2)))
           (should (equal (alist-get 'version json-data-1)
                         (alist-get 'version json-data-2)))
           (should (equal (alist-get 'enabled json-data-1)
                         (alist-get 'enabled json-data-2)))))))))

(ert-deftest eldc-test-plist-order-preservation ()
  "Test that property list order is preserved through JSON conversion."
  (eldc-test--with-data-file
   "plist.el"
   (lambda ()
     (eldc-json)
     (let* ((json-file (eldc--get-output-filename "json"))
            (json-string (with-temp-buffer
                          (insert-file-contents json-file)
                          (buffer-string))))
       (should (file-exists-p json-file))
       (message "Generated JSON:\n%s" json-string)
       ;; Check if keys appear in order in the JSON string
       (let ((name-pos (string-match "\"name\"" json-string))
             (version-pos (string-match "\"version\"" json-string))
             (dependencies-pos (string-match "\"dependencies\"" json-string)))
         (should name-pos)
         (should version-pos)
         (should dependencies-pos)
         (message "Positions: name=%s version=%s dependencies=%s"
                  name-pos version-pos dependencies-pos)
         ;; Verify order: name should come before version before dependencies
         (should (< name-pos version-pos))
         (should (< version-pos dependencies-pos)))))))

(ert-deftest eldc-test-plist-yaml-order-preservation ()
  "Test that property list order is preserved through YAML conversion."
  :tags '(:integration)
  (eldc-test--with-data-file
   "plist.el"
   (lambda ()
     (let ((yaml-file (eldc--get-output-filename "yaml"))
           (d (eldc-yaml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p yaml-file))
       (let ((yaml-content (eldc-test--read-yaml-file yaml-file)))
         (message "Generated YAML:\n%s" yaml-content)
         ;; Check if keys appear in order in the YAML string
         (let ((name-pos (string-match "^name:" yaml-content))
               (version-pos (string-match "^version:" yaml-content))
               (dependencies-pos (string-match "^dependencies:" yaml-content)))
           (should name-pos)
           (should version-pos)
           (should dependencies-pos)
           (message "Positions: name=%s version=%s dependencies=%s"
                    name-pos version-pos dependencies-pos)
           ;; Verify order: name should come before version before dependencies
           (should (< name-pos version-pos))
           (should (< version-pos dependencies-pos))))))))

(ert-deftest eldc-test-boolean-encoding-json ()
  "Test that t and :json-false encode correctly to JSON booleans."
  (eldc-test--with-data-file
   "booleans.el"
   (lambda ()
     (eldc-json)
     (let* ((json-file (eldc--get-output-filename "json"))
            (json-data (eldc-test--read-json-file json-file)))
       (should (file-exists-p json-file))
       ;; t should encode to JSON true
       (should (eq (alist-get 'enabled json-data) t))
       ;; :json-false should encode to JSON false (reads back as :json-false)
       (should (eq (alist-get 'disabled json-data) :json-false))
       ;; nil should encode to JSON null
       (should (eq (alist-get 'optional json-data) nil))))))

(ert-deftest eldc-test-boolean-encoding-yaml ()
  "Test that t and :json-false encode correctly to YAML booleans."
  :tags '(:integration)
  (eldc-test--with-data-file
   "booleans.el"
   (lambda ()
     (let ((yaml-file (eldc--get-output-filename "yaml"))
           (d (eldc-yaml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p yaml-file))
       (let ((yaml-content (eldc-test--read-yaml-file yaml-file)))
         (message "Generated YAML:\n%s" yaml-content)
         ;; Check that booleans are not quoted
         (should (string-match-p "enabled: true" yaml-content))
         (should (string-match-p "disabled: false" yaml-content))
         ;; null values can be represented as null or ~
         (should (or (string-match-p "optional: null" yaml-content)
                     (string-match-p "optional: ~" yaml-content))))))))

;;; Tests for Hooks

(ert-deftest eldc-test-json-before-export-hook ()
  "Test that eldc-json-before-export-hook is called with correct arguments."
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((hook-called nil)
           (hook-data nil)
           (hook-file nil))
       ;; Add test hook
       (add-hook 'eldc-json-before-export-hook
                 (lambda (data file)
                   (setq hook-called t
                         hook-data data
                         hook-file file)))
       (unwind-protect
           (progn
             (eldc-json)
             ;; Verify hook was called
             (should hook-called)
             ;; Verify data is JSON-encodable object
             (should hook-data)
             (should (equal (alist-get 'name hook-data) "test-package"))
             ;; Verify file path is correct
             (should hook-file)
             (should (string-suffix-p "simple.json" hook-file)))
         ;; Cleanup: remove test hook
         (setq eldc-json-before-export-hook nil))))))

(ert-deftest eldc-test-json-after-export-hook ()
  "Test that eldc-json-after-export-hook is called after successful export."
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((hook-called nil)
           (hook-file nil))
       ;; Add test hook
       (add-hook 'eldc-json-after-export-hook
                 (lambda (file)
                   (setq hook-called t
                         hook-file file)))
       (unwind-protect
           (progn
             (eldc-json)
             ;; Verify hook was called
             (should hook-called)
             ;; Verify file exists
             (should hook-file)
             (should (file-exists-p hook-file))
             (should (string-suffix-p "simple.json" hook-file)))
         ;; Cleanup: remove test hook
         (setq eldc-json-after-export-hook nil))))))

(ert-deftest eldc-test-json-hooks-execution-order ()
  "Test that JSON hooks execute in correct order (before, then after)."
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((execution-order nil))
       ;; Add hooks that record execution order
       (add-hook 'eldc-json-before-export-hook
                 (lambda (data file)
                   (push 'before execution-order)))
       (add-hook 'eldc-json-after-export-hook
                 (lambda (file)
                   (push 'after execution-order)))
       (unwind-protect
           (progn
             (eldc-json)
             ;; Verify hooks executed in correct order
             (should (equal (reverse execution-order) '(before after))))
         ;; Cleanup: remove test hooks
         (setq eldc-json-before-export-hook nil
               eldc-json-after-export-hook nil))))))

(ert-deftest eldc-test-yaml-before-export-hook ()
  "Test that eldc-yaml-before-export-hook is called with correct arguments."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((hook-called nil)
           (hook-data nil)
           (hook-file nil))
       ;; Add test hook
       (add-hook 'eldc-yaml-before-export-hook
                 (lambda (data file)
                   (setq hook-called t
                         hook-data data
                         hook-file file)))
       (unwind-protect
           (progn
             (let ((d (eldc-yaml)))
               ;; Wait for deferred to complete
               (deferred:sync! d))
             ;; Verify hook was called
             (should hook-called)
             ;; Verify data is JSON-encodable object
             (should hook-data)
             (should (equal (alist-get 'name hook-data) "test-package"))
             ;; Verify file path is correct
             (should hook-file)
             (should (string-suffix-p "simple.yaml" hook-file)))
         ;; Cleanup: remove test hook
         (setq eldc-yaml-before-export-hook nil))))))

(ert-deftest eldc-test-yaml-after-export-hook ()
  "Test that eldc-yaml-after-export-hook is called after successful export."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((hook-called nil)
           (hook-file nil))
       ;; Add test hook
       (add-hook 'eldc-yaml-after-export-hook
                 (lambda (file)
                   (setq hook-called t
                         hook-file file)))
       (unwind-protect
           (progn
             (let ((d (eldc-yaml)))
               ;; Wait for deferred to complete
               (deferred:sync! d))
             ;; Verify hook was called
             (should hook-called)
             ;; Verify file exists
             (should hook-file)
             (should (file-exists-p hook-file))
             (should (string-suffix-p "simple.yaml" hook-file)))
         ;; Cleanup: remove test hook
         (setq eldc-yaml-after-export-hook nil))))))

(ert-deftest eldc-test-yaml-hooks-execution-order ()
  "Test that YAML hooks execute in correct order (before, then after)."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((execution-order nil))
       ;; Add hooks that record execution order
       (add-hook 'eldc-yaml-before-export-hook
                 (lambda (data file)
                   (push 'before execution-order)))
       (add-hook 'eldc-yaml-after-export-hook
                 (lambda (file)
                   (push 'after execution-order)))
       (unwind-protect
           (progn
             (let ((d (eldc-yaml)))
               ;; Wait for deferred to complete
               (deferred:sync! d))
             ;; Verify hooks executed in correct order
             (should (equal (reverse execution-order) '(before after))))
         ;; Cleanup: remove test hooks
         (setq eldc-yaml-before-export-hook nil
               eldc-yaml-after-export-hook nil))))))

(ert-deftest eldc-test-multiple-hooks ()
  "Test that multiple hooks can be added and all execute."
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((hook1-called nil)
           (hook2-called nil)
           (hook3-called nil))
       ;; Add multiple hooks
       (add-hook 'eldc-json-before-export-hook
                 (lambda (data file) (setq hook1-called t)))
       (add-hook 'eldc-json-before-export-hook
                 (lambda (data file) (setq hook2-called t)))
       (add-hook 'eldc-json-after-export-hook
                 (lambda (file) (setq hook3-called t)))
       (unwind-protect
           (progn
             (eldc-json)
             ;; Verify all hooks were called
             (should hook1-called)
             (should hook2-called)
             (should hook3-called))
         ;; Cleanup: remove test hooks
         (setq eldc-json-before-export-hook nil
               eldc-json-after-export-hook nil))))))

;;; Tests for XML Conversion (requires converter)

(ert-deftest eldc-test-xml-simple ()
  "Test XML conversion with simple.el."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((xml-file (eldc--get-output-filename "xml"))
           (d (eldc-xml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p xml-file))
       (let ((xml-content (eldc-test--read-xml-file xml-file)))
         (should (string-match-p "<root>" xml-content))
         (should (string-match-p "<name>test-package</name>" xml-content))
         (should (string-match-p "<version>1\\.0\\.0</version>" xml-content))
         (should (string-match-p "</root>" xml-content)))))))

(ert-deftest eldc-test-xml-nested ()
  "Test XML conversion with nested.el."
  :tags '(:integration)
  (eldc-test--with-data-file
   "nested.el"
   (lambda ()
     (let ((xml-file (eldc--get-output-filename "xml"))
           (d (eldc-xml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p xml-file))
       (let ((xml-content (eldc-test--read-xml-file xml-file)))
         (should (string-match-p "<root>" xml-content))
         (should (string-match-p "<name>nested-test</name>" xml-content))
         (should (string-match-p "<config>" xml-content))
         (should (string-match-p "</root>" xml-content)))))))

(ert-deftest eldc-test-xml-package ()
  "Test XML conversion with package.el."
  :tags '(:integration)
  (eldc-test--with-data-file
   "package.el"
   (lambda ()
     (let ((xml-file (eldc--get-output-filename "xml"))
           (d (eldc-xml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p xml-file))
       (let ((xml-content (eldc-test--read-xml-file xml-file)))
         (should (string-match-p "<root>" xml-content))
         (should (string-match-p "<name>eldc-converter</name>" xml-content))
         (should (string-match-p "<version>0\\.0\\.1</version>" xml-content))
         (should (string-match-p "</root>" xml-content)))))))

(ert-deftest eldc-test-xml-dynamic ()
  "Test XML conversion with dynamic.el."
  :tags '(:integration)
  (eldc-test--with-data-file
   "dynamic.el"
   (lambda ()
     (let ((xml-file (eldc--get-output-filename "xml"))
           (d (eldc-xml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p xml-file))
       (let ((xml-content (eldc-test--read-xml-file xml-file)))
         (should (string-match-p "<root>" xml-content))
         (should (string-match-p "<name>dynamic-project</name>" xml-content))
         (should (string-match-p "<version>2\\.0\\.0</version>" xml-content))
         (should (string-match-p "<build>42</build>" xml-content))
         (should (string-match-p "</root>" xml-content)))))))

(ert-deftest eldc-test-xml-boolean-values ()
  "Test that t and :json-false encode correctly to XML booleans."
  :tags '(:integration)
  (eldc-test--with-data-file
   "booleans.el"
   (lambda ()
     (let ((xml-file (eldc--get-output-filename "xml"))
           (d (eldc-xml)))
       ;; Wait for deferred to complete
       (deferred:sync! d)
       (should (file-exists-p xml-file))
       (let ((xml-content (eldc-test--read-xml-file xml-file)))
         (message "Generated XML:\n%s" xml-content)
         ;; Check that booleans are properly represented
         (should (string-match-p "<enabled>true</enabled>" xml-content))
         (should (string-match-p "<disabled>false</disabled>" xml-content)))))))

;;; Tests for XML Hooks

(ert-deftest eldc-test-xml-before-export-hook ()
  "Test that eldc-xml-before-export-hook is called with correct arguments."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((hook-called nil)
           (hook-data nil)
           (hook-file nil))
       ;; Add test hook
       (add-hook 'eldc-xml-before-export-hook
                 (lambda (data file)
                   (setq hook-called t
                         hook-data data
                         hook-file file)))
       (unwind-protect
           (progn
             (let ((d (eldc-xml)))
               ;; Wait for deferred to complete
               (deferred:sync! d))
             ;; Verify hook was called
             (should hook-called)
             ;; Verify data is JSON-encodable object
             (should hook-data)
             (should (equal (alist-get 'name hook-data) "test-package"))
             ;; Verify file path is correct
             (should hook-file)
             (should (string-suffix-p "simple.xml" hook-file)))
         ;; Cleanup: remove test hook
         (setq eldc-xml-before-export-hook nil))))))

(ert-deftest eldc-test-xml-after-export-hook ()
  "Test that eldc-xml-after-export-hook is called after successful export."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((hook-called nil)
           (hook-file nil))
       ;; Add test hook
       (add-hook 'eldc-xml-after-export-hook
                 (lambda (file)
                   (setq hook-called t
                         hook-file file)))
       (unwind-protect
           (progn
             (let ((d (eldc-xml)))
               ;; Wait for deferred to complete
               (deferred:sync! d))
             ;; Verify hook was called
             (should hook-called)
             ;; Verify file exists
             (should hook-file)
             (should (file-exists-p hook-file))
             (should (string-suffix-p "simple.xml" hook-file)))
         ;; Cleanup: remove test hook
         (setq eldc-xml-after-export-hook nil))))))

(ert-deftest eldc-test-xml-hooks-execution-order ()
  "Test that XML hooks execute in correct order (before, then after)."
  :tags '(:integration)
  (eldc-test--with-data-file
   "simple.el"
   (lambda ()
     (let ((execution-order nil))
       ;; Add hooks that record execution order
       (add-hook 'eldc-xml-before-export-hook
                 (lambda (data file)
                   (push 'before execution-order)))
       (add-hook 'eldc-xml-after-export-hook
                 (lambda (file)
                   (push 'after execution-order)))
       (unwind-protect
           (progn
             (let ((d (eldc-xml)))
               ;; Wait for deferred to complete
               (deferred:sync! d))
             ;; Verify hooks executed in correct order
             (should (equal (reverse execution-order) '(before after))))
         ;; Cleanup: remove test hooks
         (setq eldc-xml-before-export-hook nil
               eldc-xml-after-export-hook nil))))))

(provide 'eldc-test)
;;; eldc-test.el ends here
