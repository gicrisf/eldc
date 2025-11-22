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
;; Version: 0.0.1
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
;; - TODO Pre-compiled binaries for conversion
;; - Fallback on python or javascript (bun/node) runtimes
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

(defcustom eldc-preferred-converter nil
  "Preferred YAML converter backend to use.
When set, eldc will attempt to use this converter first before falling
back to the default priority order.

Valid values:
  nil       - Use default priority (binary > python > bun > node)
  binary    - Prefer downloaded or local binary
  python    - Prefer Python with json-to-yaml.py
  bun       - Prefer Bun with json-to-yaml.js
  node      - Prefer Node.js with json-to-yaml.js

If the preferred converter is not available, eldc will fall back to
checking all converters in the default priority order."
  :type '(choice (const :tag "Auto-detect (default priority)" nil)
                 (const :tag "Binary (downloaded or local)" binary)
                 (const :tag "Python" python)
                 (const :tag "Bun" bun)
                 (const :tag "Node.js" node))
  :group 'eldc)

(defvar eldc-binary-dir
  (expand-file-name "bin" user-emacs-directory)
  "Directory to store downloaded converter binaries.")

;;; Embedded Converter Scripts

(defconst eldc--python-converter-script
  "#!/usr/bin/env python3
import sys
import json
import base64
import yaml

try:
    if len(sys.argv) > 1:
        json_string = base64.b64decode(sys.argv[1]).decode('utf-8')
    else:
        json_string = sys.stdin.read()

    data = json.loads(json_string)
    yaml_content = yaml.dump(data, default_flow_style=False, allow_unicode=True, sort_keys=False, width=float('inf'))
    yaml_base64 = base64.b64encode(yaml_content.encode('utf-8')).decode('ascii')
    print(yaml_base64)
except Exception as error:
    print(f'Error: {error}', file=sys.stderr)
    sys.exit(1)
"
  "Python script for JSON to YAML conversion.")

(defconst eldc--js-converter-script
  "import { load, dump } from 'js-yaml';

try {
  let jsonString;
  if (process.argv[2]) {
    jsonString = Buffer.from(process.argv[2], 'base64').toString('utf8');
  } else {
    const fs = require('fs');
    jsonString = fs.readFileSync(0, 'utf-8');
  }
  const data = load(jsonString);
  const yamlContent = dump(data, { indent: 2, lineWidth: -1, noRefs: true, sortKeys: false });
  const yamlBase64 = Buffer.from(yamlContent, 'utf8').toString('base64');
  console.log(yamlBase64);
} catch (error) {
  console.error('Error:', error.message);
  process.exit(1);
}
"
  "JavaScript script for JSON to YAML conversion.")

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
  (cond
   ((eq system-type 'windows-nt) "eldc-converter.exe")
   ((eq system-type 'gnu/linux) "eldc-converter-linux")
   ((eq system-type 'darwin) "eldc-converter-macos")
   (t "eldc-converter")))

(defun eldc--try-binary-converter (dir)
  "Try to find binary converter in DIR.
Returns converter command list if found, nil otherwise."
  (let ((user-binary (expand-file-name (eldc--binary-name) eldc-binary-dir)))
    (cond
     ;; Try user's downloaded binary from ~/.emacs.d/bin/
     ((file-exists-p user-binary)
      (list user-binary))
     ;; Try Windows executable in package directory
     ((and (eq system-type 'windows-nt)
           (file-exists-p (expand-file-name "json-to-yaml.exe" dir)))
      (list (expand-file-name "json-to-yaml.exe" dir)))
     ;; Try Linux/Unix executable in package directory
     ((file-exists-p (expand-file-name "json-to-yaml" dir))
      (list (expand-file-name "json-to-yaml" dir)))
     (t nil))))

(defun eldc--create-temp-script (script-content extension)
  "Create a temporary script file with SCRIPT-CONTENT and EXTENSION.
Returns the path to the created file."
  (let ((temp-file (make-temp-file "eldc-converter-" nil extension)))
    (with-temp-file temp-file
      (insert script-content))
    ;; Make executable on Unix-like systems
    (unless (eq system-type 'windows-nt)
      (set-file-modes temp-file #o755))
    temp-file))

(defun eldc--try-python-converter ()
  "Try to find Python runtime and create temporary converter script.
Returns converter command list if found, nil otherwise."
  (when (or (executable-find "python3")
            (executable-find "python"))
    (let ((script-file (eldc--create-temp-script eldc--python-converter-script ".py")))
      (list (or (executable-find "python3")
                (executable-find "python"))
            script-file))))

(defun eldc--try-bun-converter ()
  "Try to find Bun runtime and create temporary converter script.
Returns converter command list if found, nil otherwise."
  (when (or (executable-find "bun")
            (file-exists-p (expand-file-name "~/.bun/bin/bun")))
    (let ((script-file (eldc--create-temp-script eldc--js-converter-script ".js")))
      (list (or (executable-find "bun")
                (expand-file-name "~/.bun/bin/bun"))
            "run"
            script-file))))

(defun eldc--try-node-converter ()
  "Try to find Node.js runtime and create temporary converter script.
Returns converter command list if found, nil otherwise."
  (when (executable-find "node")
    (let ((script-file (eldc--create-temp-script eldc--js-converter-script ".js")))
      (list (executable-find "node")
            script-file))))

(defun eldc--find-converter ()
  "Find available JSON-to-YAML converter.
Respects `eldc-preferred-converter' if set, otherwise uses default priority:
binary > python > bun > node.
Creates temporary converter scripts on-the-fly as needed."
  (let ((dir (or (and load-file-name (file-name-directory load-file-name))
                 (and buffer-file-name (file-name-directory buffer-file-name))
                 default-directory)))
    (or
     ;; Try preferred converter first if specified
     (when eldc-preferred-converter
       (cond
        ((eq eldc-preferred-converter 'binary)
         (eldc--try-binary-converter dir))
        ((eq eldc-preferred-converter 'python)
         (eldc--try-python-converter))
        ((eq eldc-preferred-converter 'bun)
         (eldc--try-bun-converter))
        ((eq eldc-preferred-converter 'node)
         (eldc--try-node-converter))))
     ;; Fall back to default priority order
     (eldc--try-binary-converter dir)
     (eldc--try-python-converter)
     (eldc--try-bun-converter)
     (eldc--try-node-converter))))

;;; Public API

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
Requires JSON-to-YAML converter (binary, python, bun, or node).
Creates temporary converter scripts on-the-fly as needed."
  (interactive)
  (lexical-let* ((alist (eldc--get-alist))
                 (json-encoding-pretty-print nil)  ; Compact JSON
                 (json-content (json-encode alist))
                 ;; Base64 encode JSON to avoid shell escaping issues
                 (json-base64 (base64-encode-string json-content t))
                 (output-file (eldc--get-output-filename "yaml"))
                 (converter (eldc--find-converter))
                 ;; Extract script file path for cleanup (second or third element)
                 (script-file (when converter
                                (or (and (string-match-p "\\.\\(py\\|js\\)$" (nth 1 converter))
                                         (nth 1 converter))
                                    (and (>= (length converter) 3)
                                         (string-match-p "\\.js$" (nth 2 converter))
                                         (nth 2 converter))))))
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
                               (message "Generated %s successfully!" output-file))
                             ;; Clean up temporary script file
                             (when (and script-file (file-exists-p script-file))
                               (delete-file script-file))))
           (deferred:error it
                           (lambda (err)
                             ;; Clean up temporary script file on error
                             (when (and script-file (file-exists-p script-file))
                               (delete-file script-file))
                             (message "Error running converter: %s" err)))))
      (message "No converter found. Install python/bun/node or set eldc-preferred-converter."))))

(provide 'eldc)
;;; eldc.el ends here
