;;; eldc.el --- Emacs Lisp Dictionary Converter -*- lexical-binding: t -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Giovanni Crisalfi
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (deferred "0.5.1"))
;; Keywords: tools, data, conversion, yaml, json
;; URL: https://github.com/gicrisf/eldc

;;; Commentary:

;; This package provides functions to convert Emacs Lisp data structures
;; (alists) to various output formats like JSON and YAML.
;;
;; Key features:
;; - Convert alists to JSON or YAML format
;; - Automatic output file naming based on source file
;; - Pre-compiled binaries for YAML conversion
;; - Supports dynamic alist generation with any Emacs Lisp expression
;;
;; Usage:
;;   1. In your .el file, write an expression that evaluates to an alist.
;;      This can be:
;;      - A literal alist: '((key . "value") ...)
;;      - A function call: (my-config-generator)
;;      - A lambda: ((lambda () ...))
;;      - Any expression using let, backquote, etc.
;;   2. Run: M-x eldc-json  (creates foo.json from foo.el)
;;      Or:  M-x eldc-yaml (creates foo.yaml from foo.el)
;;
;; The last s-expression in the buffer will be evaluated and converted.
;;
;; For YAML conversion, you need the converter binary.
;; Build it with: cd eldc-j2y && cargo build --release
;; Or download from: https://github.com/gicrisf/eldc/releases

;;; Code:

(require 'json)
(require 'cl)
(require 'deferred)

;;; Configuration

(defgroup eldc nil
  "Emacs Lisp Dictionary Converter."
  :group 'tools
  :prefix "eldc-")

(defcustom eldc-converter-url
  "https://github.com/gicrisf/eldc/releases/latest/download/"
  "Base URL for downloading converter binaries from GitHub releases."
  :type 'string
  :group 'eldc)


(defvar eldc-binary-dir
  (expand-file-name "bin" user-emacs-directory)
  "Directory to store downloaded converter binaries.")

;;; Helper Functions

(defun eldc--get-output-filename (extension)
  "Generate output filename with EXTENSION based on current buffer's filename.
If buffer file is config.el, returns config.json for extension json."
  (let* ((source-file (or buffer-file-name
                          (error "Buffer is not visiting a file")))
         (dir (file-name-directory source-file))
         (base (file-name-sans-extension (file-name-nondirectory source-file)))
         (output-name (concat base "." extension)))
    (expand-file-name output-name dir)))

(defun eldc--get-alist ()
  "Get alist by parsing the last s-expression in the buffer.
Returns the evaluated alist or signals an error if parsing fails."
  (save-excursion
    (goto-char (point-max))
    (backward-sexp)
    (eval (read (current-buffer)))))

(defun eldc--binary-name ()
  "Get the appropriate binary name for the current platform."
  (if (eq system-type 'windows-nt)
      "eldc-j2y.exe"
    "eldc-j2y"))

(defun eldc--find-converter ()
  "Find available JSON-to-YAML converter binary in eldc-binary-dir.
Returns converter command list if found, nil otherwise.

TODO: If binary not found, automatically download from eldc-converter-url.
This will require:
  1. Release binary upstream to GitHub releases
  2. Implement download function using url-retrieve or similar
  3. Handle platform detection for correct binary download
  4. Verify binary integrity (checksum validation)
  5. Set executable permissions on Unix-like systems"
  (let ((binary-path (expand-file-name (eldc--binary-name) eldc-binary-dir)))
    (when (file-exists-p binary-path)
      (list binary-path))))

;;; Public API

;;;###autoload
(defun eldc-download-binary ()
  "Download the eldc-j2y converter binary from GitHub releases.
Automatically detects platform and downloads the appropriate binary
to eldc-binary-dir."
  (interactive)
  (let* ((binary-name (eldc--binary-name))
         (download-url (concat eldc-converter-url binary-name))
         (target-dir eldc-binary-dir)
         (target-path (expand-file-name binary-name target-dir)))

    ;; Create binary directory if it doesn't exist
    (unless (file-directory-p target-dir)
      (make-directory target-dir t))

    (message "Downloading %s from %s..." binary-name download-url)

    (url-retrieve
     download-url
     (lambda (status target-path binary-name)
       (if (plist-get status :error)
           (message "Failed to download binary: %s" (plist-get status :error))
         ;; Skip HTTP headers
         (goto-char (point-min))
         (re-search-forward "\n\n")
         ;; Write binary content to file
         (let ((binary-content (buffer-substring (point) (point-max))))
           (with-temp-file target-path
             (set-buffer-multibyte nil)
             (insert binary-content))
           ;; Set executable permissions on Unix-like systems
           (unless (eq system-type 'windows-nt)
             (set-file-modes target-path #o755))
           (message "Successfully downloaded %s to %s" binary-name target-path))))
     (list target-path binary-name))))

;;;###autoload
(defun eldc-json ()
  "Export alist from buffer to JSON file.
Parses the last s-expression in the buffer as an alist.
Output filename is derived from current buffer's filename.
Example: config.el -> config.json"
  (interactive)
  (let* ((alist (eldc--get-alist))
         (json-encoding-pretty-print t)
         (json-content (json-encode alist))
         (output-file (eldc--get-output-filename "json")))
    (with-temp-file output-file
      (insert json-content)
      (insert "\n"))
    (message "Generated %s successfully!" output-file)))

;;;###autoload
(defun eldc-yaml ()
  "Export alist from buffer to YAML file.
Parses the last s-expression in the buffer as an alist.
Output filename is derived from current buffer's filename.
Example: config.el -> config.yaml
Requires JSON-to-YAML converter binary."
  (interactive)
  (lexical-let* ((alist (eldc--get-alist))
                 (json-encoding-pretty-print nil)  ; Compact JSON
                 (json-content (json-encode alist))
                 ;; Base64 encode JSON to avoid shell escaping issues
                 (json-base64 (base64-encode-string json-content t))
                 (output-file (eldc--get-output-filename "yaml"))
                 (converter (eldc--find-converter)))
    (if converter
        (let ((default-directory (file-name-directory output-file)))
          (deferred:$
           ;; Pass base64-encoded JSON as command line argument
           (apply 'deferred:process (append converter (list json-base64)))
           (deferred:nextc it
                           (lambda (output)
                             ;; Decode base64 output to get YAML content
                             (let ((yaml-content (base64-decode-string output)))
                               ;; Write YAML output to file
                               (with-temp-file output-file
                                 (insert yaml-content))
                               (message "Generated %s successfully!" output-file))))
           (deferred:error it
                           (lambda (err)
                             (message "Error running converter: %s" err)))))
      ;; Binary not found - attempt to download it
      (if (yes-or-no-p "Converter binary not found. Download it now? ")
          (progn
            (eldc-download-binary)
            (message "Binary download started. Please wait and retry eldc-yaml once download completes."))
        (message "No converter binary found. Download from %s or build from source." eldc-converter-url)))))

(provide 'eldc)
;;; eldc.el ends here
