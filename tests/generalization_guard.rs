use std::fs;
use std::path::{Path, PathBuf};

fn collect_rs_files(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
                out.push(path);
            }
        }
    }
    out
}

#[test]
fn no_protocol_specific_hardcoding_in_generator() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/commands/generate");
    let files = collect_rs_files(&root);
    let banned = [
        "liquidation_auth",
        "repayment_auth",
        "authorize_liquidation",
        "authorize_repayment",
        "deepbook::",
        "octopus::",
        "rain::",
        "mid_protocol::",
        "move_project::",
        "witness_project::",
        "pyth::i64::new",
    ];

    let mut hits = Vec::new();
    for file in files {
        let Ok(content) = fs::read_to_string(&file) else {
            continue;
        };
        for token in &banned {
            if content.contains(token) {
                hits.push(format!("{} contains '{}'", file.display(), token));
            }
        }
    }

    assert!(
        hits.is_empty(),
        "Protocol-specific hardcoding found in generator code:\n{}",
        hits.join("\n")
    );
}

#[test]
fn constructor_synthesis_is_wired_in_generation_paths() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/commands/generate");
    let orchestrator = root.join("render/orchestrator.rs");
    let guards = root.join("guards.rs");

    let orchestrator_src = fs::read_to_string(orchestrator).expect("orchestrator.rs missing");
    let guards_src = fs::read_to_string(guards).expect("guards.rs missing");

    assert!(
        orchestrator_src.contains("synthesize_value_expr_for_type("),
        "orchestrator should use generic constructor synthesis"
    );
    assert!(
        guards_src.contains("synthesize_value_expr_for_type("),
        "guards should use generic constructor synthesis"
    );
}
