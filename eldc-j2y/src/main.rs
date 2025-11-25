use base64::prelude::*;
use std::process;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: eldc-j2y [--reverse|--xml] <base64-encoded-data>");
        eprintln!("  Default: JSON to YAML conversion");
        eprintln!("  --reverse: YAML to JSON conversion");
        eprintln!("  --xml: JSON to XML conversion");
        process::exit(1);
    }

    let (mode, base64_input) = if args.len() == 3 {
        match args[1].as_str() {
            "--reverse" => ("reverse", &args[2]),
            "--xml" => ("xml", &args[2]),
            _ => {
                eprintln!("Error: Invalid flag '{}'", args[1]);
                eprintln!("Usage: eldc-j2y [--reverse|--xml] <base64-encoded-data>");
                process::exit(1);
            }
        }
    } else if args.len() == 2 {
        ("yaml", &args[1])
    } else {
        eprintln!("Error: Invalid arguments");
        eprintln!("Usage: eldc-j2y [--reverse|--xml] <base64-encoded-data>");
        process::exit(1);
    };

    let result = match mode {
        "reverse" => convert_yaml_to_json(base64_input),
        "xml" => convert_json_to_xml(base64_input),
        _ => convert_json_to_yaml(base64_input),
    };

    match result {
        Ok(output) => println!("{}", output),
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    }
}

fn convert_json_to_yaml(base64_input: &str) -> Result<String, String> {
    let bytes = BASE64_STANDARD
        .decode(base64_input)
        .map_err(|e| format!("Failed to decode base64: {}", e))?;

    let json_string = String::from_utf8(bytes)
        .map_err(|e| format!("Invalid UTF-8 in decoded input: {}", e))?;

    let json_value: serde_json::Value = serde_json::from_str(&json_string)
        .map_err(|e| format!("Failed to parse JSON: {}", e))?;

    let yaml_string = serde_yaml::to_string(&json_value)
        .map_err(|e| format!("Failed to convert to YAML: {}", e))?;

    let base64_output = BASE64_STANDARD.encode(yaml_string.as_bytes());

    Ok(base64_output)
}

fn convert_yaml_to_json(base64_input: &str) -> Result<String, String> {
    let bytes = BASE64_STANDARD
        .decode(base64_input)
        .map_err(|e| format!("Failed to decode base64: {}", e))?;

    let yaml_string = String::from_utf8(bytes)
        .map_err(|e| format!("Invalid UTF-8 in decoded input: {}", e))?;

    let yaml_value: serde_yaml::Value = serde_yaml::from_str(&yaml_string)
        .map_err(|e| format!("Failed to parse YAML: {}", e))?;

    let json_value: serde_json::Value = serde_json::to_value(&yaml_value)
        .map_err(|e| format!("Failed to convert to JSON: {}", e))?;

    let json_string = serde_json::to_string(&json_value)
        .map_err(|e| format!("Failed to serialize JSON: {}", e))?;

    let base64_output = BASE64_STANDARD.encode(json_string.as_bytes());

    Ok(base64_output)
}

fn convert_json_to_xml(base64_input: &str) -> Result<String, String> {
    let bytes = BASE64_STANDARD
        .decode(base64_input)
        .map_err(|e| format!("Failed to decode base64: {}", e))?;

    let json_string = String::from_utf8(bytes)
        .map_err(|e| format!("Invalid UTF-8 in decoded input: {}", e))?;

    let json_value: serde_json::Value = serde_json::from_str(&json_string)
        .map_err(|e| format!("Failed to parse JSON: {}", e))?;

    // Use to_string_with_root to provide the root element name
    // This is a quick solution, but I should improve the root selection
    // Possibly, the content should come with a root already
    let xml_string = quick_xml::se::to_string_with_root("root", &json_value)
        .map_err(|e| format!("Failed to convert to XML: {}", e))?;

    let base64_output = BASE64_STANDARD.encode(xml_string.as_bytes());

    Ok(base64_output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_json_to_yaml() {
        let input = "eyJrZXkiOiAidmFsdWUifQ==";
        let expected = "a2V5OiB2YWx1ZQo=";
        let result = convert_json_to_yaml(input).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_nested_json_to_yaml() {
        let input = "eyJwZXJzb24iOiB7Im5hbWUiOiAiSm9obiIsICJhZ2UiOiAzMH19";
        let result = convert_json_to_yaml(input).unwrap();

        let decoded = BASE64_STANDARD.decode(result).unwrap();
        let yaml_str = String::from_utf8(decoded).unwrap();

        assert!(yaml_str.contains("person:"));
        assert!(yaml_str.contains("name: John"));
        assert!(yaml_str.contains("age: 30"));
    }

    #[test]
    fn test_array_json_to_yaml() {
        let input = "WyJhIiwgImIiLCAiYyJd";
        let result = convert_json_to_yaml(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_invalid_base64() {
        let input = "not-valid-base64!!!";
        let result = convert_json_to_yaml(input);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to decode base64"));
    }

    #[test]
    fn test_invalid_json() {
        let input = BASE64_STANDARD.encode(b"{invalid json}");
        let result = convert_json_to_yaml(&input);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to parse JSON"));
    }

    #[test]
    fn test_invalid_utf8() {
        let invalid_utf8 = vec![0xFF, 0xFE, 0xFD];
        let input = BASE64_STANDARD.encode(&invalid_utf8);
        let result = convert_json_to_yaml(&input);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Invalid UTF-8"));
    }

    #[test]
    fn test_simple_yaml_to_json() {
        let yaml = "key: value\n";
        let input = BASE64_STANDARD.encode(yaml.as_bytes());
        let result = convert_yaml_to_json(&input).unwrap();

        let decoded = BASE64_STANDARD.decode(result).unwrap();
        let json_str = String::from_utf8(decoded).unwrap();
        let json_value: serde_json::Value = serde_json::from_str(&json_str).unwrap();

        assert_eq!(json_value["key"], "value");
    }

    #[test]
    fn test_nested_yaml_to_json() {
        let yaml = "person:\n  name: John\n  age: 30\n";
        let input = BASE64_STANDARD.encode(yaml.as_bytes());
        let result = convert_yaml_to_json(&input).unwrap();

        let decoded = BASE64_STANDARD.decode(result).unwrap();
        let json_str = String::from_utf8(decoded).unwrap();
        let json_value: serde_json::Value = serde_json::from_str(&json_str).unwrap();

        assert_eq!(json_value["person"]["name"], "John");
        assert_eq!(json_value["person"]["age"], 30);
    }

    #[test]
    fn test_invalid_yaml() {
        let invalid_yaml = "key: [unclosed array";
        let input = BASE64_STANDARD.encode(invalid_yaml.as_bytes());
        let result = convert_yaml_to_json(&input);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to parse YAML"));
    }

    #[test]
    fn test_roundtrip_json_yaml_json() {
        let original_json = r#"{"name":"Alice","scores":[95,87,92]}"#;
        let json_input = BASE64_STANDARD.encode(original_json.as_bytes());

        let yaml_output = convert_json_to_yaml(&json_input).unwrap();
        let json_output = convert_yaml_to_json(&yaml_output).unwrap();

        let decoded = BASE64_STANDARD.decode(json_output).unwrap();
        let json_str = String::from_utf8(decoded).unwrap();
        let final_value: serde_json::Value = serde_json::from_str(&json_str).unwrap();
        let original_value: serde_json::Value = serde_json::from_str(original_json).unwrap();

        assert_eq!(final_value, original_value);
    }

    #[test]
    fn test_boolean_values() {
        let json_with_bools = r#"{"enabled":true,"disabled":false,"maybe":null}"#;
        let input = BASE64_STANDARD.encode(json_with_bools.as_bytes());
        let result = convert_json_to_yaml(&input).unwrap();

        let decoded = BASE64_STANDARD.decode(result).unwrap();
        let yaml_str = String::from_utf8(decoded).unwrap();

        println!("YAML output:\n{}", yaml_str);

        // Check that booleans are not quoted
        assert!(yaml_str.contains("enabled: true"));
        assert!(yaml_str.contains("disabled: false"));
        assert!(yaml_str.contains("maybe: null") || yaml_str.contains("maybe: ~"));
    }

    #[test]
    fn test_simple_json_to_xml() {
        let input = "eyJrZXkiOiAidmFsdWUifQ=="; // {"key": "value"}
        let result = convert_json_to_xml(input).unwrap();

        let decoded = BASE64_STANDARD.decode(result).unwrap();
        let xml_str = String::from_utf8(decoded).unwrap();

        println!("XML output:\n{}", xml_str);

        assert!(xml_str.contains("<root>"));
        assert!(xml_str.contains("<key>value</key>"));
        assert!(xml_str.contains("</root>"));
    }

    #[test]
    fn test_nested_json_to_xml() {
        let json = r#"{"person":{"name":"John","age":30}}"#;
        let input = BASE64_STANDARD.encode(json.as_bytes());
        let result = convert_json_to_xml(&input).unwrap();

        let decoded = BASE64_STANDARD.decode(result).unwrap();
        let xml_str = String::from_utf8(decoded).unwrap();

        println!("Nested XML output:\n{}", xml_str);

        assert!(xml_str.contains("<root>"));
        assert!(xml_str.contains("<person>"));
        assert!(xml_str.contains("<name>John</name>"));
        assert!(xml_str.contains("<age>30</age>"));
        assert!(xml_str.contains("</root>"));
    }

    #[test]
    fn test_array_json_to_xml() {
        let json = r#"{"items":["a","b","c"]}"#;
        let input = BASE64_STANDARD.encode(json.as_bytes());
        let result = convert_json_to_xml(&input);

        assert!(result.is_ok());

        let decoded = BASE64_STANDARD.decode(result.unwrap()).unwrap();
        let xml_str = String::from_utf8(decoded).unwrap();

        println!("Array XML output:\n{}", xml_str);
    }

    #[test]
    fn test_xml_boolean_values() {
        let json_with_bools = r#"{"enabled":true,"disabled":false,"maybe":null}"#;
        let input = BASE64_STANDARD.encode(json_with_bools.as_bytes());
        let result = convert_json_to_xml(&input).unwrap();

        let decoded = BASE64_STANDARD.decode(result).unwrap();
        let xml_str = String::from_utf8(decoded).unwrap();

        println!("XML with booleans:\n{}", xml_str);

        assert!(xml_str.contains("<root>"));
        assert!(xml_str.contains("<enabled>true</enabled>"));
        assert!(xml_str.contains("<disabled>false</disabled>"));
        assert!(xml_str.contains("</root>"));
    }

    #[test]
    fn test_invalid_json_to_xml() {
        let input = BASE64_STANDARD.encode(b"{invalid json}");
        let result = convert_json_to_xml(&input);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to parse JSON"));
    }
}
