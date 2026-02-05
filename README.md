# eldc - Emacs Lisp Dictionary Converter

Convert Emacs Lisp data files to structured data formats like JSON and YAML.

## Quick Start

1. Install the package

2. Create and open an elisp file `my-config.el`:

```elisp
'(:name "my-app"
  :version "1.0.0"
  :dependencies (:react "^18.2.0"
                 :vite "^4.3.0"))
```

3. Convert it:

```elisp
M-x eldc-json  ; Creates my-config.json
M-x eldc-yaml  ; Creates my-config.yaml
```

YAML output scenario (`my-config.yaml`):

```yaml
name: my-app
version: 1.0.0
dependencies:
  react: ^18.2.0
  vite: ^4.3.0
```

## How It Works

**eldc** is designed to help you manage configuration files in Emacs Lisp format and export them to standard structured data formats.

### Architecture

1. **JSON Conversion** (always the first step)
   - Uses Emacs' native `json-encode` function
   - No external dependencies required
   - Follows JSON encoder conventions for booleans (`:json-false`), null (`:null`), and other data types

2. **YAML Conversion** (optional)
   - JSON output is converted to YAML via a Rust/serde-powered binary converter
   - Fast, lightweight, and reliable
   - Handles base64-encoded data to avoid shell escaping issues

3. **Future Formats**
   - Architecture designed for extensibility
   - JSON conversion is always the foundation
   - Additional format converters (TOML, XML, etc.) can be added following the same pattern

## Dynamic Configuration

The last s-expression in your `.el` file is evaluated before conversion, so you can use any Emacs Lisp code to generate configuration dynamically:

```elisp
;; In my-config.el
(let ((project-name "dynamic-project")
      (current-version "2.0.0")
      (build-number 42))
  `(:name ,project-name
    :version ,current-version
    :build ,build-number
    :timestamp ,(format-time-string "%Y-%m-%d")
    :computed ,(* 2 21)))
```

When you run `M-x eldc-json` or `M-x eldc-yaml`, this code executes and generates the output with computed values. This is useful for:
- Inserting timestamps or build numbers
- Reading from environment variables
- Computing values based on system properties
- Generating configuration programmatically

## Data Type Encoding

Since **eldc** uses Emacs' native JSON encoder, follow these conventions for proper encoding:

| Emacs Lisp    | JSON    | YAML    |
|---------------|---------|---------|
| `t`           | `true`  | `true`  |
| `:json-false` | `false` | `false` |
| `nil`         | `null`  | `null`  |

**Example:**
```elisp
'((enabled . t)                  ; → true
  (disabled . :json-false)       ; → false
  (missing . nil))               ; → null
```

**Note:** Emacs Lisp's `nil` represents both false and null. Use `:json-false` when you specifically need a boolean `false` value in the output.

## Installation

Install the package using your preferred method.

From [MELPA](https://melpa.org/) using `package.el`:

```
M-x package-install RET eldc RET
```

Using Straight:

```elisp
(straight-use-package 'eldc)
```

With use-package:

``` elisp
(use-package eldc
  :straight t
  ...)
```

Using Doom Emacs:

``` elisp
(package! eldc)
```

Directly from the source repo instead of MELPA:

```elisp
;; Using straight.el
(straight-use-package
 '(eldc :type git :host github :repo "gicrisf/eldc"))

;; Or use-package with straight
(use-package eldc
  :straight (eldc :type git :host github :repo "gicrisf/eldc"))
  
;; Or Doom
(package! eldc :recipe (:host github :repo "gicrisf/eldc"))
```

**For YAML conversion:** The package will automatically prompt you to `M-x eldc-download-binary` (which downloads the required binary converter). Alternatively, download it manually from [GitHub Releases](https://github.com/gicrisf/eldc/releases) and place it in `~/.emacs.d/bin/`. If your architecture is not among those for which I have precompiled binaries, you can also compile it yourself:

```
cargo install --git https://github.com/gicrisf/eldc eldc-j2y
```

In this case, you will need the Rust toolchain on your system.

## Real-World Example: GitHub Actions Workflow

This package uses **eldc** to manage its own CI/CD pipeline! The GitHub Actions workflow is defined in Emacs Lisp and converted to YAML.

**Source:** `.github/workflows/build-binaries.el`
```elisp
'(:name "Build Binaries"

  :on (:push (:tags ["v*"])
       :workflow_dispatch nil)

  :permissions (:contents "write")

  :jobs (:build ;; ... build job steps ...

         :release (:name "Create Release"
                   :needs "build"
                   :runs-on "ubuntu-latest"
                   :if "startsWith(github.ref, 'refs/tags/')"
                   :steps [;; ... download artifacts steps ...
                           (:name "Create Release"
                            :uses "softprops/action-gh-release@v1"
                            :with (:files "artifacts/eldc-j2y/eldc-j2y"
                                   :draft :json-false
                                   :prerelease :json-false
                                   :generate_release_notes t)
                            :env (:GITHUB_TOKEN "${{ secrets.GITHUB_TOKEN }}"))])))
```

Run `M-x eldc-yaml` to generate the YAML output:

**Output:** `.github/workflows/build-binaries.yml`
```yaml
name: Build Binaries
on:
  push:
    tags:
    - v*
  workflow_dispatch: null
permissions:
  contents: write
jobs:
  build: # ... build job steps ...
  release:
    name: Create Release
    needs: build
    runs-on: ubuntu-latest
    if: startsWith(github.ref, 'refs/tags/')
    steps:
    # ... download artifacts steps ...
    - name: Create Release
      uses: softprops/action-gh-release@v1
      with:
        files: artifacts/eldc-j2y/eldc-j2y
        draft: false
        prerelease: false
        generate_release_notes: true
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

A fully functional GitHub Actions workflow that builds and releases the Rust binary converter used by this very package!

**Note:** This example uses **plists** (`:key value` syntax) instead of alists for complex nested structures. Plists are recommended because the JSON encoder can more reliably distinguish between objects and arrays, avoiding ambiguous encoding issues. Use vectors `[...]` for explicit arrays.

See the full source at `.github/workflows/build-binaries.el` in this repository.

## Configuration

You can customize the file extensions for output files:

```elisp
;; Change YAML extension from "yaml" to "yml"
(setq eldc-yaml-extension "yml")

;; JSON extension (default is "json")
(setq eldc-json-extension "json")
```

## Hooks

**eldc** provides hooks to customize the export process:

### Before Export Hooks

Run before the conversion, allowing you to modify data or perform validation:

```elisp
;; Modify data before JSON export
(add-hook 'eldc-json-before-export-hook
          (lambda (data output-file)
            (message "Exporting to JSON: %s" output-file)
            ;; You can inspect or modify data here
            ))

;; Modify data before YAML export
(add-hook 'eldc-yaml-before-export-hook
          (lambda (data output-file)
            (message "Exporting to YAML: %s" output-file)))
```

### After Export Hooks

Run after successful export, useful for post-processing:

```elisp
;; Run formatter after JSON export
(add-hook 'eldc-json-after-export-hook
          (lambda (output-file)
            (shell-command (format "prettier --write %s" output-file))))

;; Commit generated YAML to git
(add-hook 'eldc-yaml-after-export-hook
          (lambda (output-file)
            (shell-command (format "git add %s" output-file))))
```

**Available hooks:**
- `eldc-json-before-export-hook` - Called with `(data output-file)` before JSON export
- `eldc-json-after-export-hook` - Called with `(output-file)` after JSON export
- `eldc-yaml-before-export-hook` - Called with `(data output-file)` before YAML export
- `eldc-yaml-after-export-hook` - Called with `(output-file)` after YAML export

## License

GPL-3.0-or-later
