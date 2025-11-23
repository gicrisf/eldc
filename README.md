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
- **For YAML**: Pre-compiled binary converter (Rust-based, available from releases)

## Dynamic Configuration

Use any Emacs Lisp expression:

```elisp
(let ((build-date (format-time-string "%Y-%m-%d")))
  `((name . "my-app")
    (buildDate . ,build-date)
    (platform . ,(symbol-name system-type))))
```

## Installation

For YAML conversion, download the pre-compiled binary for your platform from [GitHub Releases](https://github.com/gicrisf/eldc/releases) and place it in `~/.emacs.d/bin/`.

Or build from source:
```bash
cd eldc-j2y
cargo build --release
# Copy binary to ~/.emacs.d/bin/
```

## Configuration

You can customize the file extensions for output files:

```elisp
;; Change YAML extension from "yaml" to "yml"
(setq eldc-yaml-extension "yml")

;; JSON extension (default is "json")
(setq eldc-json-extension "json")
```

## License

GPL-3.0-or-later
