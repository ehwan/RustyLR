mod parser_expanded;

use std::time::Instant;

use parser_expanded as parser;
use rusty_lr::parser::Parser;
use rusty_lr::parser::table::ParserTables;

const TEST_JSON: &'static str = r#"
{
    "user": {
        "id": 12345,
        "name": "Anonymous",
        "active": true,
        "roles": ["engineer", "researcher"],
        "profile": {
            "email": "anonymous@example.com",
            "location": "Seoul",
            "social": {
                "github": "https://github.com/anonymous",
                "twitter": null
            }
        }
    },
    "projects": [
        {
            "id": "proj_001",
            "name": "High-Speed Simulation Engine",
            "description": "A tool used by scientists to simulate how air flows over objects like airplane wings or car bodies.",
            aa
            "technologies": ["High-performance computing", "Graphics cards"],
            "team_size": 5
        },
        {
            "id": "proj_002",
            "name": "Smart Investment Assistant",
            "description": "An automated system that studies price movements and suggests when to buy or sell stocks.",
            "technologies": ["Artificial Intelligence", "Data Analysis"],
            "team_size": 1,
            "status": "experimental"
        }
    ],
    "timestamp": "2025-04-18T16:00:00+09:00",
    "metadata": null
}
"#;

fn main() {
    println!("#rules: {}", parser::JsonParser::get_tables().rule_count());
    println!(
        "#states: {}",
        parser::JsonParser::get_tables().state_count()
    );

    fn try_once() {
        let mut context = parser::JsonContext::new(Vec::new());
        let mut range_start = 0;
        for ch in TEST_JSON.chars() {
            let range_end = range_start + ch.len_utf8();
            context
                .feed_location(ch, range_start..range_end)
                .expect("Error parsing character");
            range_start = range_end;
        }
    }

    let start = Instant::now();
    for _ in 0..1000 {
        try_once();
    }
    let duration = start.elapsed();
    println!("Parsed 1000 times in {:?}", duration);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_parse_success() {
        let mut context = parser::JsonContext::new(Vec::new());
        let input = r#"{"name": "test", "active": true}"#;

        let mut range_start = 0;
        for ch in input.chars() {
            let range_end = range_start + ch.len_utf8();
            context
                .feed_location(ch, range_start..range_end)
                .expect("Failed to feed character");
            range_start = range_end;
        }

        // Verify accept yields Ok(()) - start data of Json (empty type)
        let result = context.accept();
        assert!(result.is_ok(), "Accept failed: {:?}", result.err());
        let (_, userdata) = result.unwrap();

        // No errors should have occurred
        assert!(userdata.is_empty());
    }

    #[test]
    fn test_json_error_recovery_exact_location() {
        let mut context = parser::JsonContext::new(Vec::new());

        // "invalid" causes syntax error inside { }
        let input = r#"{"name": "test", invalid}"#;

        let mut range_start = 0;
        for ch in input.chars() {
            let range_end = range_start + ch.len_utf8();
            // We ignore individual feed errors here, as panic mode / recovery will proceed
            let _ = context.feed_location(ch, range_start..range_end);
            range_start = range_end;
        }

        // Trigger accept (which feeds EOF and resolves recovery)
        let (_, userdata) = context.accept().expect("Failed to accept recovered JSON");

        // Verify that the error recovery action successfully captured the exact range of the error token
        // In an LR parser, the 'error' token location spans the entire popped/unwound region:
        // from right after '{' (index 1) to right before '}' (index 24)
        assert!(
            !userdata.is_empty(),
            "Error recovery action did not write location to userdata"
        );

        let recovered_range = &userdata[0];
        assert_eq!(recovered_range.start, 1);
        assert_eq!(recovered_range.end, 24);
    }

    #[test]
    fn test_unclosed_object_eof_location() {
        let mut context = parser::JsonContext::new(Vec::new());
        let input = r#"{"name": "test""#; // unclosed {

        let mut range_start = 0;
        for ch in input.chars() {
            let range_end = range_start + ch.len_utf8();
            context
                .feed_location(ch, range_start..range_end)
                .expect("Failed to feed character");
            range_start = range_end;
        }

        // Accept should fail because of EOF while parsing object
        let result = context.accept();
        assert!(result.is_err());

        let err = result.err().unwrap();
        // Since we refine 0-length EOF location generation,
        // the error location should point to the end of input as a 0-length range
        let err_location = err.location();
        assert_eq!(err_location.start, input.len());
        assert_eq!(err_location.end, input.len());
    }
}
