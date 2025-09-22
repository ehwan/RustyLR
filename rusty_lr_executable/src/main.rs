use clap::Parser;

use std::fs::write;

mod arg;

fn main() {
    let args = match arg::Args::try_parse() {
        Ok(args) => args,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let mut builder = rusty_lr_buildscript::Builder::new();
    builder.is_executable = true;
    builder.file(&args.input_file);
    if args.no_conflict {
        builder.note_conflicts(false);
    }
    if args.no_conflict_resolve {
        builder.note_conflicts_resolving(false);
    }
    if args.no_optimization {
        builder.note_optimization(false);
    }
    if args.no_backtrace {
        builder.note_backtrace(false);
    }
    if let Some(glr) = args.glr {
        builder.glr(glr);
    }
    if let Some(dense) = args.dense {
        builder.dense(dense);
    }

    let out = match builder.build_impl() {
        Ok(out) => out,
        Err(_) => {
            return;
        }
    };

    if let Some(state_idx) = args.state {
        let term_class_map = |term| out.grammar.class_pretty_name_list(term, 4);
        let nonterm_map = |nonterm| out.grammar.nonterm_pretty_name(nonterm);

        if let Some(state) = out.grammar.states.get(state_idx) {
            let mut from_states = Vec::new();
            for (i, s) in out.grammar.states.iter().enumerate() {
                if s.shift_goto_map_term
                    .iter()
                    .any(|(_, t)| t.state == state_idx)
                    || s.shift_goto_map_nonterm
                        .iter()
                        .any(|(_, t)| t.state == state_idx)
                {
                    from_states.push(i);
                }
            }

            println!("State {state_idx}:");
            println!("Production Rules: {{");
            for rule in &state.ruleset {
                let rule = out.grammar.builder.rules[rule.rule]
                    .rule
                    .clone()
                    .map(&term_class_map, &nonterm_map)
                    .into_shifted(rule.shifted);
                println!("    {}", rule);
            }
            println!("}}");
            if !state.shift_goto_map_term.is_empty() {
                println!("Shift/Goto on Terminals: {{");
                for (term, target) in &state.shift_goto_map_term {
                    let term = term_class_map(*term);
                    println!("    {term:>4} => State {}", target.state);
                }
                println!("}}");
            }
            if !state.shift_goto_map_nonterm.is_empty() {
                println!("Shift/Goto on Non-Terminals: {{");
                for (nonterm, target) in &state.shift_goto_map_nonterm {
                    let nonterm = nonterm_map(*nonterm);
                    println!("    {nonterm:>4} => State {}", target.state);
                }
                println!("}}");
            }
            if !state.reduce_map.is_empty() {
                println!("Reduce on Terminals: {{");
                for (term, rules) in &state.reduce_map {
                    let term = term_class_map(*term);
                    let preline = format!("    {term:>4} => {{ ");
                    let mut rules = rules
                        .iter()
                        .map(|rule| {
                            out.grammar.builder.rules[*rule]
                                .rule
                                .clone()
                                .map(&term_class_map, &nonterm_map)
                                .to_string()
                        })
                        .collect::<Vec<_>>();
                    for r in rules.iter_mut().skip(1) {
                        *r = format!("\n{}{}", " ".repeat(preline.len()), r);
                    }

                    println!("{}{} }}", preline, rules.join(""));
                }
                println!("}}");
            }
            if !from_states.is_empty() {
                println!("From States: {{");
                for from_state in from_states {
                    println!("    State {}", from_state);
                }
                println!("}}");
            }
        } else {
            println!("State {state_idx} does not exist.");
        }
    }

    let (major, minor, patch) = rusty_lr_buildscript::target_rusty_lr_version();
    println!(">> The generated code is targeting rusty_lr version {major}.{minor}.x.");
    println!(">> There might be a build error if the version is not matched.");

    // format the generated code
    let user_code = if args.no_format {
        out.user_stream.to_string()
    } else {
        match syn::parse2(out.user_stream.clone()) {
            Ok(file) => prettyplease::unparse(&file),
            Err(e) => {
                eprintln!("Error parsing user code: {}", e);
                out.user_stream.to_string()
            }
        }
    };
    let generated_code = if args.no_format {
        out.generated_stream.to_string()
    } else {
        match syn::parse2(out.generated_stream.clone()) {
            Ok(file) => prettyplease::unparse(&file),
            Err(e) => {
                eprintln!("Error parsing generated code: {}", e);
                out.generated_stream.to_string()
            }
        }
    };

    let this_name = env!("CARGO_PKG_NAME");
    let this_version = env!("CARGO_PKG_VERSION");
    let output_string = format!(
        r#"
// This file was generated by {} {}
// This generated code is targeting rusty_lr version {major}.{minor}.{patch}.
// There might be a build error if the version is not matched.
//
// {:=^80}
{}
// {:=^80}
/*
{}
*/
// {:=^80}
{}
// {:=^80}
        "#,
        this_name,
        this_version,
        "User Codes Begin",
        user_code,
        "User Codes End",
        out.debug_comments,
        "Generated Codes Begin",
        generated_code,
        "Generated Codes End"
    );
    match write(args.output_file.clone(), output_string) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error writing output file: {}", e);
            return;
        }
    }
}
