mod parser_expanded;

use parser_expanded as parser;

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

use rusty_lr::lr::Parser;

fn main() {
    let parser = parser::JsonParser::new();
    println!("#rules: {}", parser.get_rules().len());
    println!("#states: {}", parser.get_states().len());
    println!("#terminals: {}", parser.classes.len());

    // success
    {
        let mut context = parser::JsonContext::new();
        for ch in TEST_JSON.chars().chain(['\0']) {
            context
                .feed(&parser, ch, &mut ())
                .expect("Error parsing character");
        }
        println!("Parsed successfully");
    }

    // error
    {
        let mut context = parser::JsonContext::new();
        for ch in TEST_JSON.chars() {
            context
                .feed(&parser, ch, &mut ())
                .expect("Error parsing character");
        }
        context
            .feed(&parser, 'a', &mut ())
            .expect("Error finalizing parsing");
    }
}
