use clap::Parser;

use std::fs::write;
use std::process::Command;

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
    builder.file(&args.input_file);
    if args.lalr {
        builder.lalr();
    }
    if args.verbose {
        builder.verbose();
    }
    if args.verbose_conflict {
        builder.verbose_conflicts();
    }
    if args.verbose_generated_source {
        builder.verbose_generated_source();
    }
    if args.verbose_conflict_resolve {
        builder.verbose_conflicts_resolving();
    }

    let out = match builder.build_impl() {
        Ok(out) => out,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let this_name = env!("CARGO_PKG_NAME");
    let this_version = env!("CARGO_PKG_VERSION");
    let output_string = format!(
        r#"
// This file was generated by {} {}
//
// Input file: {}
// Output file: {}
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
        args.input_file,
        args.output_file.clone(),
        "User Codes Begin",
        out.user_stream,
        "User Codes End",
        out.debug_comments,
        "Generated Codes Begin",
        out.generated_stream,
        "Generated Codes End"
    );
    match write(args.output_file.clone(), output_string) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error writing output file: {}", e);
            return;
        }
    }

    if !args.no_format {
        let mut child = Command::new("rustfmt")
            .arg(args.output_file)
            .spawn()
            .expect("Failed to run rustfmt");
        child.wait().expect("Failed to wait on rustfmt");
    }
}
