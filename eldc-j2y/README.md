# eldc-j2y

A fast CLI tool for converting between JSON and YAML formats using base64-encoded data.

## Usage

```bash
# JSON to YAML conversion (default)
eldc-j2y <base64-encoded-json>

# YAML to JSON conversion
eldc-j2y --reverse <base64-encoded-yaml>
```

## Why Base64?

Base64 encoding avoids shell escaping issues when passing data between processes, making the tool reliable across different platforms and shell environments.
