;;; package.el --- Test package configuration

;;; Commentary:
;; Test data for eldc converter tests

;;; Code:

'((name . "eldc-converter")
  (version . "0.0.1")
  (description . "JSON to YAML converter for eldc")
  (type . "module")
  (main . "json-to-yaml.js")
  (scripts .
   ((build . "bun build --compile --minify --sourcemap ./json-to-yaml.js --outfile eldc-converter")))
  (dependencies .
   ((js-yaml . "^4.1.1")))
  (keywords . ["json" "yaml" "converter"])
  (author . "Giovanni Crisalfi")
  (license . "GPL-3.0-or-later"))

;;; package.el ends here
