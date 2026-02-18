//! Tidewalker: run Move package tests and observe/log state changes per test and function.

use clap::Parser;
use serde_json::Value;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Parser)]
#[command(name = "tidewalker")]
#[command(about = "Run Move package tests and log state changes per test/function")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(clap::Subcommand)]
enum Commands {
    /// Run tests for the Move package at PATH and observe state changes.
    Run {
        /// Path to the Move package (directory containing Move.toml).
        #[arg(value_name = "PATH", default_value = ".")]
        path: PathBuf,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Run { path } => run_tests_and_log(&path),
    }
}

fn run_tests_and_log(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let path = package_path.canonicalize().unwrap_or_else(|_| package_path.to_path_buf());
    let move_toml = path.join("Move.toml");
    if !move_toml.is_file() {
        return Err(format!("Not a Move package: no Move.toml at {}", path.display()).into());
    }

    println!("Running tests (with trace) for package: {}", path.display());
    let status = Command::new("sui")
        .args(["move", "test", "--trace", "-p", path.to_str().unwrap()])
        .status()?;
    if !status.success() {
        return Err("sui move test failed".into());
    }

    let traces_dir = path.join("traces");
    if !traces_dir.is_dir() {
        println!("No traces directory found; nothing to log.");
        return Ok(());
    }

    let mut entries: Vec<_> = fs::read_dir(&traces_dir)?
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map_or(false, |ext| ext == "zst" || ext == "json")
        })
        .collect();
    entries.sort_by_key(|e| e.file_name());

    for entry in entries {
        let p = entry.path();
        let name = p
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("?")
            .replace("__", "::");
        let test_name = name.strip_suffix(".json").unwrap_or(&name);
        if let Err(e) = log_trace_for_test(&p, test_name) {
            eprintln!("Warning: could not parse trace {}: {}", p.display(), e);
        }
    }

    Ok(())
}

fn log_trace_for_test(trace_path: &Path, test_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    let bytes = fs::read(trace_path)?;
    let mut decoder = zstd::Decoder::new(bytes.as_slice())?;
    let mut ndjson = String::new();
    decoder.read_to_string(&mut ndjson)?;
    drop(decoder);

    let mut call_stack: Vec<String> = Vec::new();
    let mut functions_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut state_changes: Vec<(String, String)> = Vec::new();
    let skip_modules = ["tx_context", "bcs", "test_scenario"];

    for line in ndjson.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let v: Value = serde_json::from_str(line).unwrap_or(Value::Null);
        if let Some(obj) = v.as_object() {
            if obj.contains_key("OpenFrame") {
                if let Some(frame) = obj.get("OpenFrame").and_then(|f| f.get("frame")) {
                    let module = frame
                        .get("module")
                        .and_then(|m| m.get("name"))
                        .and_then(|n| n.as_str())
                        .unwrap_or("?");
                    let func = frame
                        .get("function_name")
                        .and_then(|n| n.as_str())
                        .unwrap_or("?");
                    let name = format!("{}::{}", module, func);
                    call_stack.push(name.clone());
                    if !skip_modules.iter().any(|m| name.starts_with(&format!("{}::", m))) {
                        functions_seen.insert(name);
                    }
                }
            } else if obj.contains_key("CloseFrame") {
                call_stack.pop();
            } else if let Some(effect) = obj.get("Effect") {
                if let Some(write) = effect.get("Write") {
                    if let Some(after) = write.get("root_value_after_write") {
                        if let Some(rv) = after.get("RuntimeValue") {
                            if let Some(val) = rv.get("value") {
                                let ty_s = val.get("type").and_then(|t| t.as_str()).unwrap_or("?");
                                let in_fn = call_stack
                                    .last()
                                    .filter(|f| !skip_modules.iter().any(|m| f.starts_with(&format!("{}::", m))))
                                    .cloned()
                                    .unwrap_or_else(|| "?".to_string());
                                if in_fn != "?" && ty_s != "Vector" {
                                    let val_s = format_runtime_value(val, 0);
                                    state_changes.push((in_fn, format!("{} = {}", ty_s, val_s)));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    println!("\n--- Test: {} ---", test_name);
    println!("  Functions called:");
    let mut order: Vec<_> = functions_seen.into_iter().collect();
    order.sort();
    for f in order {
        println!("    {}", f);
    }
    if !state_changes.is_empty() {
        println!("  State changes (by function):");
        let mut by_fn: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
        for (fn_name, change) in state_changes {
            by_fn.entry(fn_name).or_default().push(change);
        }
        for (fn_name, changes) in by_fn {
            println!("    [{}]", fn_name);
            for c in changes.iter().take(15) {
                println!("      {}", c);
            }
            if changes.len() > 15 {
                println!("      ... and {} more", changes.len() - 15);
            }
        }
    }
    Ok(())
}

/// Format a trace "RuntimeValue.value" node (object with "type" and "value" keys).
fn format_runtime_value(node: &Value, depth: usize) -> String {
    let obj = match node.as_object() {
        Some(o) => o,
        None => return format_primitive(node),
    };
    let ty = obj.get("type").and_then(|t| t.as_str()).unwrap_or("?");
    let val = match obj.get("value") {
        Some(v) => v,
        None => return "?".to_string(),
    };
    match ty {
        "Struct" => format_struct(val, depth),
        "Vector" => format_vector(val, depth),
        "U64" | "U8" | "U16" | "U32" | "U128" | "U256" => format_primitive(val),
        "Address" | "Bool" | "String" => format_primitive(val),
        _ => format_primitive(val),
    }
}

fn format_struct(val: &Value, depth: usize) -> String {
    let obj = match val.as_object() {
        Some(o) => o,
        None => return "(object)".to_string(),
    };
    let type_ = obj.get("type_").and_then(|t| t.as_object());
    let module = type_
        .and_then(|t| t.get("module"))
        .and_then(|m| m.as_str())
        .unwrap_or("?");
    let name = type_
        .and_then(|t| t.get("name"))
        .and_then(|n| n.as_str())
        .unwrap_or("?");
    let type_name = format!("{}::{}", module, name);
    let fields = match obj.get("fields") {
        Some(Value::Array(arr)) => arr,
        _ => return format!("{} {{}}", type_name),
    };
    let max_depth = 3;
    if depth >= max_depth {
        return format!("{} {{ ... }}", type_name);
    }
    let mut parts: Vec<String> = Vec::new();
    for pair in fields {
        if let Value::Array(p) = pair {
            if p.len() >= 2 {
                let field_name = p[0].as_str().unwrap_or("?");
                let field_val = &p[1];
                let field_str = format_runtime_value(field_val, depth + 1);
                parts.push(format!("{}: {}", field_name, field_str));
            }
        }
    }
    format!("{} {{ {} }}", type_name, parts.join(", "))
}

fn format_vector(val: &Value, depth: usize) -> String {
    let arr = match val.as_array() {
        Some(a) => a,
        None => return "[]".to_string(),
    };
    if arr.is_empty() {
        return "[]".to_string();
    }
    let max_show = 4;
    let mut parts: Vec<String> = Vec::new();
    for (i, item) in arr.iter().enumerate() {
        if i >= max_show {
            parts.push(format!("... +{} more", arr.len() - max_show));
            break;
        }
        parts.push(format_runtime_value(item, depth + 1));
    }
    format!("[{}]", parts.join(", "))
}

fn format_primitive(v: &Value) -> String {
    match v {
        Value::Number(n) => n.to_string(),
        Value::String(s) => {
            if s.len() > 20 {
                format!("{}...", &s[..20])
            } else {
                s.clone()
            }
        }
        Value::Bool(b) => b.to_string(),
        Value::Array(arr) => format!("[{} items]", arr.len()),
        Value::Object(_) => "(object)".to_string(),
        Value::Null => "null".to_string(),
    }
}
