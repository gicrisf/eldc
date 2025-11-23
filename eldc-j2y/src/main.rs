use base64::prelude::*;
use std::process;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Error: Missing base64-encoded JSON argument");
        process::exit(1);
    }

    let base64_input = &args[1];

    match convert_json_to_yaml(base64_input) {
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
}
