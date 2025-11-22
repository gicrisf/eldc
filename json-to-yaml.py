#!/usr/bin/env python3
"""
Converts JSON to YAML via base64-encoded argument or stdin
Outputs base64-encoded YAML to avoid stdout encoding issues
Usage: python eldc-converter.py <base64-encoded-json>
       echo '{"key": "value"}' | python eldc-converter.py
"""

import sys
import json
import base64
import yaml


def main():
    try:
        # Check if input is provided as command line argument (base64-encoded)
        if len(sys.argv) > 1:
            json_string = base64.b64decode(sys.argv[1]).decode('utf-8')
        else:
            # Read from stdin
            json_string = sys.stdin.read()

        # Parse JSON
        data = json.loads(json_string)

        # Convert to YAML
        yaml_content = yaml.dump(
            data,
            default_flow_style=False,
            allow_unicode=True,
            sort_keys=False,
            width=float('inf')
        )

        # Output as base64 to avoid stdout encoding issues
        yaml_base64 = base64.b64encode(yaml_content.encode('utf-8')).decode('ascii')
        print(yaml_base64)

    except Exception as error:
        print(f"Error: {error}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
