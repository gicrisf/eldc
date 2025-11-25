# eldc-j2y

A fast CLI tool for converting between JSON, YAML, and XML formats using base64-encoded data.

## Usage

```bash
# JSON to YAML conversion (default)
eldc-j2y <base64-encoded-json>

# YAML to JSON conversion
eldc-j2y --reverse <base64-encoded-yaml>

# JSON to XML conversion
eldc-j2y --xml <base64-encoded-json>
```

## Examples

```bash
# Convert JSON to YAML
echo '{"name":"test","value":42}' | base64 | eldc-j2y

# Convert JSON to XML
echo '{"name":"test","value":42}' | base64 | eldc-j2y --xml

# Convert YAML to JSON
echo 'name: test\nvalue: 42' | base64 | eldc-j2y --reverse
```

## Why Base64?

Base64 encoding avoids shell escaping issues when passing data between processes, making the tool reliable across different platforms and shell environments.

## Features

- **JSON to YAML**: Convert JSON data to YAML format
- **YAML to JSON**: Convert YAML data back to JSON format
- **JSON to XML**: Convert JSON data to XML format with automatic root element wrapping
- **Order preservation**: Maintains key order from original JSON (using `preserve_order` feature)
- **Fast**: Built in Rust with `serde_json`, `serde_yaml`, and `quick-xml`
- **Cross-platform**: Works on Windows, macOS, and Linux
