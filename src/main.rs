//! Tidewalker: run Move package tests and observe/log state changes per test and function.

mod commands;

use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "tidewalker")]
#[command(about = "Run Move package tests and log state changes per test/function")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(clap::Subcommand)]
enum Commands {
    /// Run tests for the Move package at PATH and observe/log state changes.
    Run {
        /// Path to the Move package (directory containing Move.toml).
        #[arg(value_name = "PATH", default_value = ".")]
        path: PathBuf,
        /// Static analysis only: do not run tests, just analyze entry functions and intra-module calls.
        #[arg(long = "static")]
        static_only: bool,
    },
    /// Generate tests/tidewalker_generated_tests_setup.move with test_only helpers for shared objects and admin caps.
    /// Alerts (to stderr) for any module/file where setup could not be generated.
    GenerateSetup {
        /// Path to the Move package (directory containing Move.toml).
        #[arg(value_name = "PATH", default_value = ".")]
        path: PathBuf,
    },
    /// Generate tests that call public/entry functions (best effort).
    /// This will first run the same setup injection as `generate-setup`.
    Generate {
        /// Path to the Move package (directory containing Move.toml).
        #[arg(value_name = "PATH", default_value = ".")]
        path: PathBuf,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Run { path, static_only } => {
            if static_only {
                commands::static_analysis::run_static_analysis(&path)
            } else {
                commands::run::run_tests_and_log(&path)
            }
        }
        Commands::GenerateSetup { path } => commands::setup::run_generate_setup(&path),
        Commands::Generate { path } => commands::generate::run_generate(&path),
    }
}
