#!/usr/bin/env bun
/**
 * Converts JSON to YAML via base64-encoded argument or stdin
 * Outputs base64-encoded YAML to avoid stdout encoding issues
 * Usage: bun run json-to-yaml.js <base64-encoded-json>
 *        echo '{"key": "value"}' | bun run json-to-yaml.js
 */

import { load, dump } from 'js-yaml';

try {
  let jsonString;
  
  // Check if input is provided as command line argument (base64-encoded)
  if (process.argv[2]) {
    jsonString = Buffer.from(process.argv[2], 'base64').toString('utf8');
  } else {
    // Read from stdin
    jsonString = await Bun.stdin.text();
  }
  
  // Load JSON/YAML string directly
  const data = load(jsonString);
  
  // Dump to YAML
  const yamlContent = dump(data, {
    indent: 2,
    lineWidth: -1,
    noRefs: true,
    sortKeys: false
  });
  
  // Output as base64 to avoid stdout encoding issues
  const yamlBase64 = Buffer.from(yamlContent, 'utf8').toString('base64');
  console.log(yamlBase64);
} catch (error) {
  console.error('Error:', error.message);
  process.exit(1);
}
