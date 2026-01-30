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

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: 2025-11-22
;; Modified: 2025-11-23
;; Version: 0.4.0
;; Package-Requires: ((emacs "25.1") (deferred "0.5.1"))
;; Keywords: tools, data, conversion, yaml, json
;; URL: https://github.com/gicrisf/eldc

;;; Commentary:

;; Convert Emacs Lisp data structures to JSON and YAML files.
;;
;; Particularly useful to manage configuration files (package.json, GitHub
;; Actions, etc.) by writing them in Emacs Lisp and exporting to standard
;; formats.  The last s-expression in the buffer is evaluated, enabling
;; both static data and dynamic generation.
;;
;; Quick start:
;;   1. Create a .el file with an alist or plist:
;;      '(:name "my-app" :version "1.0.0")
;;   2. Run M-x eldc-json or M-x eldc-yaml
;;   3. Output file is created automatically (foo.el -> foo.json)
;;
;; Features:
;; - JSON conversion using built-in json-encode (no dependencies)
;; - YAML conversion via optional binary converter (auto-downloads on first use)
;; - Dynamic data generation using any Emacs Lisp expression
;; - Hooks for pre/post-processing
;;
;; For more examples and documentation, see:
;; https://github.com/gicrisf/eldc

;;; Code:

(require 'json)
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


(defvar eldc--available-binaries
  '("linux-x86_64")
  "List of available pre-built binary architectures.")

(defvar eldc-binary-dir
  (expand-file-name "bin" user-emacs-directory)
  "Directory to store downloaded converter binaries.")

(defvar eldc-json-extension "json"
  "Preferred file extension for JSON output files.")

(defvar eldc-yaml-extension "yaml"
  "Preferred file extension for YAML output files.
Common alternatives: \"yaml\" or \"yml\".")

(defvar eldc-json-before-export-hook nil
  "Hook run before exporting to JSON.
Functions are called with two arguments: DATA and OUTPUT-FILE.
DATA is the JSON-encodable Emacs Lisp object to be exported.
OUTPUT-FILE is the target file path.
Hook functions can modify the data or perform side effects.")

(defvar eldc-json-after-export-hook nil
  "Hook run after successfully exporting to JSON.
Functions are called with one argument: OUTPUT-FILE.
OUTPUT-FILE is the path to the generated JSON file.")

(defvar eldc-yaml-before-export-hook nil
  "Hook run before exporting to YAML.
Functions are called with two arguments: DATA and OUTPUT-FILE.
DATA is the JSON-encodable Emacs Lisp object to be exported.
OUTPUT-FILE is the target file path.
Hook functions can modify the data or perform side effects.")

(defvar eldc-yaml-after-export-hook nil
  "Hook run after successfully exporting to YAML.
Functions are called with one argument: OUTPUT-FILE.
OUTPUT-FILE is the path to the generated YAML file.")

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
  "Find available JSON-to-YAML converter binary.
Checks `eldc-binary-dir' first, then falls back to PATH lookup.
Returns converter command list if found, nil otherwise."
  (let ((binary-path (expand-file-name (eldc--binary-name) eldc-binary-dir)))
    (if (file-exists-p binary-path)
        (list binary-path)
      (let ((found (executable-find "eldc-j2y")))
        (when found (list found))))))

;;; Public API

;;;###autoload
(defun eldc-download-binary ()
  "Download the eldc-j2y converter binary from GitHub releases.
Presents a list of available pre-built binaries to choose from,
or instructions to compile from source using cargo."
  (interactive)
  (let* ((choices (append eldc--available-binaries '("Compile from source")))
         (selection (completing-read "Select binary architecture: " choices nil t)))
    (if (string= selection "Compile from source")
        (message "Run: cargo install --git https://github.com/gicrisf/eldc eldc-j2y\nThe binary will be installed to your cargo bin directory on PATH.")
      (let* ((binary-name (eldc--binary-name))
             (asset-name (concat "eldc-j2y-" selection))
             (download-url (concat eldc-converter-url asset-name))
             (target-dir eldc-binary-dir)
             (target-path (expand-file-name binary-name target-dir)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t))
        (message "Downloading %s from %s..." asset-name download-url)
        (url-retrieve
         download-url
         (lambda (status target-path binary-name)
           (if (plist-get status :error)
               (message "Failed to download binary: %s" (plist-get status :error))
             (goto-char (point-min))
             (re-search-forward "\n\n")
             (let ((binary-content (buffer-substring (point) (point-max))))
               (with-temp-file target-path
                 (set-buffer-multibyte nil)
                 (insert binary-content))
               (unless (eq system-type 'windows-nt)
                 (set-file-modes target-path #o755))
               (message "Successfully downloaded %s to %s" binary-name target-path))))
         (list target-path binary-name))))))

;;;###autoload
(defun eldc-json ()
  "Export data from buffer to JSON file.
Parses the last s-expression in the buffer as a JSON-encodable object.
Output filename is derived from current buffer's filename.
Example: config.el -> config.json"
  (interactive)
  (let* ((data (eldc--get-alist))
         (output-file (eldc--get-output-filename eldc-json-extension)))
    (run-hook-with-args 'eldc-json-before-export-hook data output-file)
    (let* ((json-encoding-pretty-print t)
           (json-content (json-encode data)))
      (with-temp-file output-file
        (insert json-content)
        (insert "\n")))
    (run-hook-with-args 'eldc-json-after-export-hook output-file)
    (message "Generated %s successfully!" output-file)))

;;;###autoload
(defun eldc-yaml ()
  "Export data from buffer to YAML file.
Parses the last s-expression in the buffer as a JSON-encodable object.
Output filename is derived from current buffer's filename.
Example: config.el -> config.yaml
Requires JSON-to-YAML converter binary."
  (interactive)
  (let* ((data (eldc--get-alist))
         (output-file (eldc--get-output-filename eldc-yaml-extension))
         (converter (eldc--find-converter)))
    (run-hook-with-args 'eldc-yaml-before-export-hook data output-file)
    (if converter
        (let* ((json-encoding-pretty-print nil)  ; Compact JSON
               (json-content (json-encode data))
               ;; Base64 encode JSON to avoid shell escaping issues
               (json-base64 (base64-encode-string json-content t))
               (default-directory (file-name-directory output-file)))
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
                               (run-hook-with-args 'eldc-yaml-after-export-hook output-file)
                               (message "Generated %s successfully!" output-file))))
           (deferred:error it
                           (lambda (err)
                             (message "Error running converter: %s" err)))))
      (message "Converter binary not found. Run M-x eldc-download-binary to install it."))))

(provide 'eldc)
;;; eldc.el ends here
