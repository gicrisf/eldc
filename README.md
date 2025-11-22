# eldc

Emacs Lisp Dictionary Converter.
Convert Emacs Lisp to JSON and YAML.

## Quick Start

1. Install the package

2. Create a config file `my-config.el`:

```elisp
'((name . "my-app")
  (version . "1.0.0")
  (dependencies
   ((react . "^18.2.0")
    (vite . "^4.3.0"))))
```

3. Convert it:

```elisp
M-x eldc-json  ; Creates my-config.json
M-x eldc-yaml  ; Creates my-config.yaml
```

## Requirements

- **Emacs 25.1+**
- **deferred.el** package
- **For YAML**: Python with PyYAML (`pip install pyyaml`) OR Javascript runtime like Node or Bun with js-yaml

## Dynamic Configuration

Use any Emacs Lisp expression:

```elisp
(let ((build-date (format-time-string "%Y-%m-%d")))
  `((name . "my-app")
    (buildDate . ,build-date)
    (platform . ,(symbol-name system-type))))
```

## Configuration

```elisp
;; Prefer Python converter (optional)
(setq eldc-preferred-converter 'python)

;; Options: nil (auto), 'binary, 'python, 'bun, 'node
```

(I have no binaries to release yet)

## License

GPL-3.0-or-later
