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
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Run { path, static_only } => {
            if static_only {
                run_static_analysis(&path)
            } else {
                run_tests_and_log(&path)
            }
        }
        Commands::GenerateSetup { path } => run_generate_setup(&path),
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

/// Run static analysis only: list entry functions and intra-module calls without running tests.
fn run_static_analysis(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use serde::Deserialize;

    #[derive(Deserialize)]
    struct MovePackage {
        package: Package,
    }
    #[derive(Deserialize)]
    struct Package {
        name: String,
    }

    let path = package_path.canonicalize().unwrap_or_else(|_| package_path.to_path_buf());
    let move_toml_path = path.join("Move.toml");
    if !move_toml_path.is_file() {
        return Err(format!("Not a Move package: no Move.toml at {}", path.display()).into());
    }

    let toml_str = fs::read_to_string(&move_toml_path)?;
    let manifest: MovePackage = toml::from_str(&toml_str)?;
    let pkg_name = manifest.package.name;

    // Ensure build artifacts exist (but do not run tests).
    let status = Command::new("sui")
        .args(["move", "build", "-p", path.to_str().unwrap()])
        .status()?;
    if !status.success() {
        return Err("sui move build failed".into());
    }

    let src_root = path.join("build").join(&pkg_name).join("sources");
    if !src_root.is_dir() {
        return Err(format!("No build sources found at {}", src_root.display()).into());
    }

    println!("Static analysis for package '{}' at {}", pkg_name, path.display());

    let mut move_files: Vec<PathBuf> = Vec::new();
    for entry in fs::read_dir(&src_root)? {
        let entry = entry?;
        let p = entry.path();
        if p.extension().map_or(false, |ext| ext == "move") {
            move_files.push(p);
        }
    }
    move_files.sort();

    for file in move_files {
        analyze_module_file(&file)?;
    }

    Ok(())
}

/// Generate tests/tidewalker_generated_tests_setup.move and print alerts for modules that couldn't be fully generated.
fn run_generate_setup(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use serde::Deserialize;

    #[derive(Deserialize)]
    struct MovePackage {
        package: Package,
    }
    #[derive(Deserialize)]
    struct Package {
        name: String,
    }

    let path = package_path.canonicalize().unwrap_or_else(|_| package_path.to_path_buf());
    let move_toml_path = path.join("Move.toml");
    if !move_toml_path.is_file() {
        return Err(format!("Not a Move package: no Move.toml at {}", path.display()).into());
    }

    let toml_str = fs::read_to_string(&move_toml_path)?;
    let manifest: MovePackage = toml::from_str(&toml_str)?;
    let pkg_name = manifest.package.name;

    let status = Command::new("sui")
        .args(["move", "build", "-p", path.to_str().unwrap()])
        .status()?;
    if !status.success() {
        return Err("sui move build failed".into());
    }

    let src_root = path.join("build").join(&pkg_name).join("sources");
    if !src_root.is_dir() {
        return Err(format!("No build sources at {}", src_root.display()).into());
    }

    let mut move_files: Vec<PathBuf> = Vec::new();
    for entry in fs::read_dir(&src_root)? {
        let entry = entry?;
        let p = entry.path();
        if p.extension().map_or(false, |ext| ext == "move") {
            move_files.push(p);
        }
    }
    move_files.sort();

    let mut all_shared: Vec<(String, String)> = Vec::new();
    let mut all_caps: Vec<(String, String)> = Vec::new();
    let mut alerts: Vec<(String, String)> = Vec::new();
    let mut file_for_module: std::collections::HashMap<String, String> = std::collections::HashMap::new();

    for file in &move_files {
        let file_name = file.file_name().unwrap_or_default().to_string_lossy().to_string();
        match extract_setup_info(file) {
            Ok(info) => {
                file_for_module.insert(info.module_name.clone(), file_name.clone());
                for (mod_name, type_name) in info.shared_types {
                    all_shared.push((mod_name.clone(), type_name));
                }
                for (mod_name, type_name) in info.cap_types {
                    all_caps.push((mod_name.clone(), type_name));
                }
                for (target, reason) in info.cannot_generate {
                    alerts.push((target, reason));
                }
                for (type_name, fn_name) in info.non_init_shared {
                    alerts.push((
                        file_name.clone(),
                        format!(
                            "Type '{}' is created/shared in function '{}' (not init). Add a #[test_only] helper that calls {} with test parameters, or implement manually (will be tackled in test-generation phase).",
                            type_name, fn_name, fn_name
                        ),
                    ));
                }
                for (type_name, fn_name) in info.non_init_caps {
                    alerts.push((
                        file_name.clone(),
                        format!(
                            "Cap type '{}' is created/transferred in function '{}' (not init). Add a #[test_only] helper that calls {} with test parameters, or implement manually (will be tackled in test-generation phase).",
                            type_name, fn_name, fn_name
                        ),
                    ));
                }
            }
            Err(e) => {
                alerts.push((file_name, format!("analysis failed: {}", e)));
            }
        }
    }

    // Generate test_only helpers in the defining module (protocol source), where struct
    // instantiation is allowed. Group shared/caps by module and inject into sources/<file>.
    let mut shared_by_module: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    let mut caps_by_module: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    for (mod_name, type_name) in &all_shared {
        shared_by_module.entry(mod_name.clone()).or_default().push(type_name.clone());
    }
    for (mod_name, type_name) in &all_caps {
        caps_by_module.entry(mod_name.clone()).or_default().push(type_name.clone());
    }

    let sources_dir = path.join("sources");
    for (mod_name, file_name) in &file_for_module {
        let shared = shared_by_module.get(mod_name).map(Vec::as_slice).unwrap_or(&[]);
        let caps = caps_by_module.get(mod_name).map(Vec::as_slice).unwrap_or(&[]);
        if shared.is_empty() && caps.is_empty() {
            continue;
        }
        let source_path = sources_dir.join(file_name);
        if !source_path.is_file() {
            alerts.push((
                file_name.clone(),
                "Source file not found in package sources/ (only build output was analyzed).".to_string(),
            ));
            continue;
        }
        // Skip files that have #[test_only] code not generated by us (dev-written); we only update our own block.
        let content = match fs::read_to_string(&source_path) {
            Ok(c) => c,
            Err(e) => {
                alerts.push((file_name.clone(), format!("Could not read source file: {}", e)));
                continue;
            }
        };
        let lines: Vec<String> = content.lines().map(String::from).collect();
        if file_has_test_only_outside_tidewalker_block(&lines) {
            continue; // file has dev-written test_only → skip entirely, do not touch
        }
        match inject_test_only_helpers(&source_path, mod_name, shared, caps) {
            Ok(()) => println!("Injected test_only helpers into {}", source_path.display()),
            Err(e) => alerts.push((file_name.clone(), format!("Failed to inject helpers: {}", e))),
        }
    }

    if !alerts.is_empty() {
        eprintln!("\n--- Tidewalker: setup generation alerts ---");
        for (target, reason) in &alerts {
            eprintln!("  ALERT [{}]: {}", target, reason);
        }
        eprintln!("--------------------------------------------");
    }

    Ok(())
}

#[derive(Debug, Default)]
struct ModuleSetupInfo {
    module_name: String,
    shared_types: Vec<(String, String)>,
    cap_types: Vec<(String, String)>,
    cannot_generate: Vec<(String, String)>,
    /// (type_name, function_name) for share_object in non-init functions
    non_init_shared: Vec<(String, String)>,
    /// (type_name, function_name) for transfer(..., sender) in non-init functions
    non_init_caps: Vec<(String, String)>,
    /// If init takes a one-time witness (e.g. REGISTRY), we don't generate for init-created types
    init_one_time_witness: Option<String>,
}

fn extract_setup_info(path: &Path) -> Result<ModuleSetupInfo, Box<dyn std::error::Error>> {
    let content = fs::read_to_string(path)?;
    let lines: Vec<&str> = content.lines().collect();
    let mut info = ModuleSetupInfo::default();

    for line in &lines {
        let t = line.trim();
        if t.starts_with("module ") {
            if let Some(rest) = t.strip_prefix("module ") {
                let until = rest.split_whitespace().next().unwrap_or(rest);
                info.module_name = until.trim_end_matches(';').to_string();
            }
            break;
        }
    }

    let mut public_structs: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim_start();
        if t.starts_with("public struct ") {
            let after = t.strip_prefix("public struct ").unwrap_or("");
            let name = after
                .split(|c: char| c.is_whitespace() || c == '<' || c == '(')
                .next()
                .unwrap_or("")
                .to_string();
            if !name.is_empty() && (t.contains("key") || t.contains("store")) {
                public_structs.insert(name.clone());
            }
        }
        i += 1;
    }

    let file_name = path.file_name().unwrap_or_default().to_string_lossy().to_string();
    info.init_one_time_witness = get_init_one_time_witness(&lines);

    let init_body = find_init_body(&lines);
    let init_complex_trigger = init_body
        .as_ref()
        .and_then(|body| first_disallowed_call_in_body(body));
    let (shared_vars, cap_vars) = if let Some(body) = init_body {
        parse_init_body(body)
    } else {
        (Vec::new(), Vec::new())
    };

    let skip_init_generation = info.init_one_time_witness.is_some() || init_complex_trigger.is_some();
    if skip_init_generation {
        if let Some(ref w) = info.init_one_time_witness {
            info.cannot_generate.push((
                file_name.clone(),
                format!(
                    "Init uses one-time witness ({}); add a #[test_only] helper that receives the witness, or rely on test flow that simulates publish.",
                    w
                ),
            ));
        }
        if let Some(trigger) = init_complex_trigger {
            info.cannot_generate.push((
                file_name.clone(),
                format!(
                    "Init appears complex (found call `{}`); add a #[test_only] helper manually in the defining module or call existing test API.",
                    trigger
                ),
            ));
        }
    } else {
        for (_, type_name) in &shared_vars {
            if public_structs.contains(type_name) {
                info.shared_types.push((info.module_name.clone(), type_name.clone()));
            } else if !type_name.is_empty() {
                info.cannot_generate.push((
                    file_name.clone(),
                    format!(
                        "shared object type '{}' is not a public struct (cannot generate create_and_share). Add a #[test_only] helper manually in the defining module.",
                        type_name
                    ),
                ));
            }
        }
        for (_, type_name) in &cap_vars {
            if public_structs.contains(type_name) {
                info.cap_types.push((info.module_name.clone(), type_name.clone()));
            } else if !type_name.is_empty() {
                info.cannot_generate.push((
                    file_name.clone(),
                    format!(
                        "admin/cap type '{}' is not a public struct (cannot generate cap helper). Add a #[test_only] helper manually in the defining module.",
                        type_name
                    ),
                ));
            }
        }
    }

    // Non-init: find share_object / transfer(..., sender) in all other functions (exclude our generated helpers)
    for (fn_name, body) in find_all_function_bodies(&lines) {
        if fn_name == "init" {
            continue;
        }
        if fn_name.starts_with("create_and_share_") && fn_name.ends_with("_for_testing") {
            continue;
        }
        if fn_name.starts_with("create_") && fn_name.ends_with("_for_testing") {
            continue;
        }
        let (shared, caps) = parse_init_body(body);
        for (_, type_name) in shared {
            if !type_name.is_empty() {
                info.non_init_shared.push((type_name, fn_name.clone()));
            }
        }
        for (_, type_name) in caps {
            if !type_name.is_empty() {
                info.non_init_caps.push((type_name, fn_name.clone()));
            }
        }
    }

    Ok(info)
}

/// Find all function definitions and return (function_name, body_lines). Includes init.
fn find_all_function_bodies<'a>(lines: &'a [&'a str]) -> Vec<(String, Vec<&'a str>)> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim_start();
        let (is_fun, name) = if t.starts_with("public entry fun ") {
            let after = &t["public entry fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if t.starts_with("entry fun ") {
            let after = &t["entry fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if t.starts_with("public fun ") {
            let after = &t["public fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if t.starts_with("public(package) fun ") {
            let after = &t["public(package) fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if t.starts_with("fun ") {
            let after = &t["fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else {
            (false, String::new())
        };
        if is_fun && !name.is_empty() {
            let mut depth = 0;
            let mut start = i;
            let mut end_line = i;
            for (j, l) in lines.iter().enumerate().skip(i) {
                for c in l.chars() {
                    if c == '{' {
                        depth += 1;
                        if depth == 1 {
                            start = j + 1;
                        }
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 0 {
                            end_line = j;
                            out.push((name.clone(), lines[start..=j].to_vec()));
                            break;
                        }
                    }
                }
                if depth == 0 {
                    break;
                }
            }
            i = end_line + 1;
            continue;
        }
        i += 1;
    }
    out
}

/// If init's first parameter is a one-time witness (e.g. `_: REGISTRY`), return the type name.
fn get_init_one_time_witness(lines: &[&str]) -> Option<String> {
    for line in lines {
        let t = line.trim();
        if t.starts_with("fun init(") {
            let after_paren = t.strip_prefix("fun init(")?;
            let first_param = after_paren.split(',').next()?.trim().trim_end_matches(')');
            let first_param = first_param.trim();
            if first_param.starts_with("_: ") {
                return Some(first_param.strip_prefix("_: ").unwrap_or(first_param).trim().to_string());
            }
            if first_param.starts_with('_') && first_param.contains(':') {
                if let Some(after_colon) = first_param.split(':').nth(1) {
                    return Some(after_colon.trim().to_string());
                }
            }
            break;
        }
    }
    None
}

fn find_init_body<'a>(lines: &'a [&'a str]) -> Option<Vec<&'a str>> {
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim();
        if t.starts_with("fun init(") {
            let mut depth = 0;
            let mut start = i;
            for (j, l) in lines.iter().enumerate().skip(i) {
                for c in l.chars() {
                    if c == '{' {
                        depth += 1;
                        if depth == 1 {
                            start = j + 1;
                        }
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 0 {
                            return Some(lines[start..=j].to_vec());
                        }
                    }
                }
            }
            return None;
        }
        i += 1;
    }
    None
}

fn parse_init_body(body: Vec<&str>) -> (Vec<(String, String)>, Vec<(String, String)>) {
    let mut var_to_type: std::collections::BTreeMap<String, String> = std::collections::BTreeMap::new();
    for line in &body {
        let t = line.trim();
        if t.starts_with("let ") {
            if let Some(rest) = t.strip_prefix("let ") {
                let mut name = rest
                    .split_whitespace()
                    .next()
                    .unwrap_or("")
                    .to_string();
                if name.ends_with(',') {
                    name.pop();
                }
                if let Some(eq) = rest.find('=') {
                    let rhs = rest[eq + 1..].trim();
                    let type_name = if rhs.contains(" {") {
                        rhs.split_whitespace().next().unwrap_or("").to_string()
                    } else {
                        String::new()
                    };
                    if !name.is_empty() && !type_name.is_empty() {
                        var_to_type.insert(name, type_name);
                    }
                }
            }
        }
    }
    let mut shared = Vec::new();
    let mut caps = Vec::new();
    for line in &body {
        let t = line.trim();
        if t.contains("share_object(") {
            if let Some(arg) = extract_first_arg(t, "share_object(") {
                if let Some(ty) = var_to_type.get(&arg) {
                    shared.push((arg.clone(), ty.clone()));
                }
            }
        }
        if t.contains("transfer(") && (t.contains("sender(") || t.contains("tx_context::sender")) {
            if let Some(arg) = extract_first_arg(t, "transfer(") {
                if let Some(ty) = var_to_type.get(&arg) {
                    caps.push((arg.clone(), ty.clone()));
                }
            }
        }
    }
    (shared, caps)
}

fn extract_first_arg(line: &str, prefix: &str) -> Option<String> {
    let t = line.trim();
    let after = t.find(prefix).map(|i| &t[i + prefix.len()..])?;
    let arg = after
        .trim_start()
        .split(|c: char| c == ',' || c == ')')
        .next()?
        .trim()
        .trim_end_matches(')');
    if arg.is_empty() {
        None
    } else {
        Some(arg.to_string())
    }
}

fn first_disallowed_call_in_body(body: &[&str]) -> Option<String> {
    // Allowlist is checked by suffix match so it works with prefixes like `sui::object::new`.
    const ALLOWED_SUFFIXES: [&str; 10] = [
        "object::new",
        "transfer::share_object",
        "transfer::public_share_object",
        "transfer::transfer",
        "transfer::public_transfer",
        "tx_context::sender",
        "std::string::utf8",
        "option::none",
        "option::some",
        "vector::empty",
    ];

    for line in body {
        // Strip line comments.
        let t = line.split("//").next().unwrap_or("").trim_end();
        if t.is_empty() {
            continue;
        }
        let chars: Vec<char> = t.chars().collect();
        for (idx, ch) in chars.iter().enumerate() {
            if *ch != '(' || idx == 0 {
                continue;
            }

            // Walk back from '(' to find the call "token" (optionally skipping generics).
            let mut j: isize = (idx as isize) - 1;
            while j >= 0 && chars[j as usize].is_whitespace() {
                j -= 1;
            }
            if j < 0 {
                continue;
            }

            // Skip a single generic parameter block like `<T, U>` immediately before '('.
            if chars[j as usize] == '>' {
                let mut depth: i32 = 0;
                while j >= 0 {
                    let c = chars[j as usize];
                    if c == '>' {
                        depth += 1;
                    } else if c == '<' {
                        depth -= 1;
                        if depth == 0 {
                            j -= 1;
                            while j >= 0 && chars[j as usize].is_whitespace() {
                                j -= 1;
                            }
                            break;
                        }
                    }
                    j -= 1;
                }
                if j < 0 {
                    continue;
                }
            }

            // Collect token chars: [A-Za-z0-9_:_]
            let end = j as usize;
            let mut start = end;
            while start > 0 {
                let c = chars[start];
                if c.is_ascii_alphanumeric() || c == '_' || c == ':' {
                    start -= 1;
                } else {
                    break;
                }
            }
            if !(chars[start].is_ascii_alphanumeric() || chars[start] == '_' || chars[start] == ':') {
                start += 1;
            }
            if start > end {
                continue;
            }
            let token: String = chars[start..=end].iter().collect();
            if token.is_empty() {
                continue;
            }
            if !token.chars().any(|c| c.is_ascii_alphabetic() || c == '_') {
                continue;
            }

            // Ignore method calls like `x.to_inner()` (token is preceded by '.').
            if start > 0 && chars[start - 1] == '.' {
                continue;
            }

            // Qualified call: `foo::bar(` (or deeper). Local call: `helper(`.
            if token.contains("::") {
                if ALLOWED_SUFFIXES.iter().any(|a| token.ends_with(a)) {
                    continue;
                }
                return Some(token);
            } else {
                // Any local helper call makes init complex in the refined heuristic.
                return Some(token);
            }
        }
    }

    None
}

/// Inner width of the ASCII box (space between the two slashes).
const TIDEWALKER_BOX_WIDTH: usize = 59;

fn tidewalker_box_lines() -> Vec<String> {
    // Use "// " (comment + space) so "///" is not parsed as doc comment by Move.
    let border = format!("// {}", "/".repeat(TIDEWALKER_BOX_WIDTH + 2));
    let empty_line = format!("// {}{}{}", "/", " ".repeat(TIDEWALKER_BOX_WIDTH), "/");
    let title = " Tidewalker generated test helpers ";
    let pad = TIDEWALKER_BOX_WIDTH.saturating_sub(title.len()) / 2;
    let pad_end = TIDEWALKER_BOX_WIDTH - pad - title.len();
    let title_line = format!("// {}{}{}{}/", "/", " ".repeat(pad), title, " ".repeat(pad_end));
    vec![
        border.clone(),
        empty_line.clone(),
        title_line,
        empty_line,
        border,
    ]
}

/// Generate Move source for #[test_only] create_and_share_* and create_*_for_testing (in defining module).
fn generate_in_module_test_only_snippet(
    _mod_name: &str,
    shared: &[String],
    caps: &[String],
) -> Vec<String> {
    let mut out = Vec::new();
    out.push("".to_string());
    out.extend(tidewalker_box_lines());
    for type_name in shared {
        out.push("#[test_only]".to_string());
        out.push(format!(
            "/// Create and share {} for testing (replicating init).",
            type_name
        ));
        out.push(format!(
            "public fun create_and_share_{}_for_testing(ctx: &mut sui::tx_context::TxContext) {{",
            type_name.to_lowercase()
        ));
        out.push(format!("    let obj = {} {{", type_name));
        out.push("        id: sui::object::new(ctx),".to_string());
        if type_name == "Counter" {
            out.push("        value: 0,".to_string());
        }
        out.push("    };".to_string());
        out.push("    transfer::public_share_object(obj);".to_string());
        out.push("}".to_string());
        out.push("".to_string());
    }
    for type_name in caps {
        out.push("#[test_only]".to_string());
        out.push(format!("/// Create {} for testing (caller receives it).", type_name));
        out.push(format!(
            "public fun create_{}_for_testing(ctx: &mut sui::tx_context::TxContext): {} {{",
            type_name.to_lowercase(),
            type_name
        ));
        out.push(format!("    {} {{", type_name));
        out.push("        id: sui::object::new(ctx),".to_string());
        out.push("    }".to_string());
        out.push("}".to_string());
        out.push("".to_string());
    }
    out.extend(tidewalker_box_lines());
    out
}

/// Detect existing Tidewalker block: (start, end) inclusive. Supports old and new box format.
/// Prefers the block at the end of the file (last occurrence of the title).
///
/// Important: We only ever match our own block (Tidewalker box/sentinels). We never touch
/// #[test_only] helpers that are not inside this block—hand-written or from other tools—they are left unchanged.
fn find_tidewalker_block(lines: &[String]) -> Option<(usize, usize)> {
    // Old format: "// ----- Tidewalker generated test helpers -----" ... "// ----- End Tidewalker -----"
    let old_start = "// ----- Tidewalker generated test helpers -----";
    let old_end = "// ----- End Tidewalker -----";
    if let Some(s) = lines.iter().position(|l| l.trim() == old_start) {
        if let Some(e) = lines[s..].iter().position(|l| l.trim() == old_end).map(|i| s + i) {
            return Some((s, e));
        }
    }

    // New format: find the *last* "Tidewalker generated test helpers" so we only remove the block at the end.
    let is_border = |l: &str| {
        let t = l.trim();
        t.starts_with("//") && t.len() > 4 && t[2..].trim_start().starts_with('/')
    };
    let last_title_idx = lines
        .iter()
        .enumerate()
        .rev()
        .find(|(_, l)| l.contains("Tidewalker generated test helpers"))
        .map(|(i, _)| i)?;
    // First box: border is a few lines before this title. Go back at most 5 lines.
    let first_box_start = (last_title_idx.saturating_sub(4))..=last_title_idx;
    let first = first_box_start
        .into_iter()
        .find(|&i| is_border(lines[i].as_str()))?;
    // Second box: after the first box (5 lines) and the helpers, find the closing box.
    let after_first_box = first + 5;
    if after_first_box >= lines.len() {
        return None;
    }
    let second_box_start = lines[after_first_box..]
        .iter()
        .position(|l| is_border(l.as_str()))
        .map(|i| after_first_box + i)?;
    let second_box_end = second_box_start + 4;
    if second_box_end >= lines.len() {
        return None;
    }
    Some((first, second_box_end))
}

/// Returns true if we should skip this file: it has #[test_only] code that is not inside our
/// Tidewalker block (i.e. dev-written). We only touch files that have no test_only, or only our block.
fn file_has_test_only_outside_tidewalker_block(lines: &[String]) -> bool {
    let block_range = find_tidewalker_block(lines);
    for (i, line) in lines.iter().enumerate() {
        if line.contains("#[test_only]") {
            let inside_our_block = block_range.map(|(s, e)| (s..=e).contains(&i)).unwrap_or(false);
            if !inside_our_block {
                return true; // found test_only not generated by us → skip file
            }
        }
    }
    false
}

/// Inject or replace Tidewalker-generated test_only helpers at the end of the protocol source file.
///
/// Rule: We only remove/replace the block delimited by our Tidewalker box or sentinels. Any other
/// #[test_only] code in the file (hand-written or from other tools) is never touched.
fn inject_test_only_helpers(
    source_path: &Path,
    mod_name: &str,
    shared: &[String],
    caps: &[String],
) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(source_path)?;
    let mut lines: Vec<String> = content.lines().map(String::from).collect();

    let snippet = generate_in_module_test_only_snippet(mod_name, shared, caps);

    // Only remove our own block (identified by Tidewalker box/sentinels). Do not touch other test_only code.
    if let Some((s, e)) = find_tidewalker_block(&lines) {
        lines.drain(s..=e);
        // Trim trailing blank line if any
        while lines.last().map(|l| l.trim().is_empty()) == Some(true) {
            lines.pop();
        }
    }

    // Always place our block at the end of the file.
    if !lines.is_empty() && !lines.last().map(|l| l.is_empty()).unwrap_or(true) {
        lines.push("".to_string());
    }
    lines.extend(snippet);
    fs::write(source_path, lines.join("\n"))?;
    Ok(())
}

/// Very simple textual analysis: find entry functions and calls to other functions in same module.
fn analyze_module_file(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use std::collections::{BTreeMap, BTreeSet};

    let content = fs::read_to_string(path)?;
    let lines: Vec<&str> = content.lines().collect();

    let mut module_name = String::new();
    for line in &lines {
        let t = line.trim();
        if t.starts_with("module ") {
            // e.g., module move_project::move_project;
            if let Some(rest) = t.strip_prefix("module ") {
                let until = rest.split_whitespace().next().unwrap_or(rest);
                module_name = until.trim_end_matches(';').to_string();
            }
            break;
        }
    }

    #[derive(Debug, Clone)]
    struct FnInfo {
        name: String,
        is_entry: bool,  // has `entry` keyword
        is_public: bool, // declared `public` (with or without `entry`)
        start: usize,
        end: usize,
    }

    // First pass: find function definitions and approximate their span.
    let mut fns: Vec<FnInfo> = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim_start();
        let (is_entry, is_public, is_fun) = if t.starts_with("public entry fun ") {
            (true, true, true)
        } else if t.starts_with("entry fun ") {
            (true, false, true)
        } else if t.starts_with("public fun ") {
            (false, true, true)
        } else if t.starts_with("fun ") {
            (false, false, true)
        } else {
            (false, false, false)
        };
        if !is_fun {
            i += 1;
            continue;
        }

        // Extract function name: word after 'fun ' until '(' or '<'.
        let after_fun = if let Some(pos) = t.find("fun ") {
            &t[pos + 4..]
        } else {
            i += 1;
            continue;
        };
        let name_part = after_fun
            .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
            .next()
            .unwrap_or("?");
        let name = name_part.to_string();

        // Approximate span using brace depth starting from this line.
        let mut brace_depth = 0i32;
        let start_line = i;
        let mut end_line = i;
        for (j, l) in lines.iter().enumerate().skip(i) {
            let mut line_brace_delta = 0i32;
            for c in l.chars() {
                match c {
                    '{' => line_brace_delta += 1,
                    '}' => line_brace_delta -= 1,
                    _ => {}
                }
            }
            brace_depth += line_brace_delta;
            if brace_depth <= 0 && j > i {
                end_line = j;
                break;
            }
            end_line = j;
        }

        fns.push(FnInfo { name, is_entry, is_public, start: start_line, end: end_line });
        i = end_line + 1;
    }

    if fns.is_empty() {
        return Ok(());
    }

    // Second pass: for each function, see which other functions in this module it calls,
    // and which functions from other modules it calls (cross-module).
    let mut calls: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut cross_calls: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let all_names: Vec<String> = fns.iter().map(|f| f.name.clone()).collect();
    let module_short = module_name
        .split("::")
        .last()
        .unwrap_or(&module_name)
        .to_string();

    let local_name_set: BTreeSet<String> = all_names.iter().cloned().collect();

    for f in &fns {
        let mut intra = BTreeSet::new();
        let mut cross = BTreeSet::new();
        for other in &all_names {
            if other == &f.name {
                continue;
            }
            let needle_plain = format!("{}(", other);
            let needle_mod = format!("{}::{}(", module_short, other);
            let needle_self = format!("Self::{}(", other);
            'outer: for line in &lines[f.start..=f.end.min(lines.len() - 1)] {
                let t = line.trim();
                if t.starts_with("//") {
                    continue;
                }
                if t.contains(&needle_plain) || t.contains(&needle_mod) || t.contains(&needle_self) {
                    intra.insert(other.clone());
                    break 'outer;
                }
            }
        }
        // Scan for cross-module calls in this function body.
        for line in &lines[f.start..=f.end.min(lines.len() - 1)] {
            let t = line.trim();
            if t.starts_with("//") {
                continue;
            }
            for target in find_cross_module_calls(t, &module_short, &local_name_set) {
                cross.insert(target);
            }
        }
        calls.insert(f.name.clone(), intra);
        cross_calls.insert(f.name.clone(), cross);
    }

    // Print summary for this module.
    println!("\nModule: {}", if module_name.is_empty() { "<unknown>" } else { &module_name });

    // Explicit entry functions (those with `entry` keyword).
    println!("  Entry functions (with `entry` keyword):");
    let mut any_entry = false;
    for f in &fns {
        if f.is_entry {
            any_entry = true;
            let called = calls.get(&f.name);
            let mut descr = format!("    {}", f.name);
            if let Some(set) = called {
                if !set.is_empty() {
                    let list: Vec<_> = set.iter().cloned().collect();
                    descr.push_str(&format!(" (calls: {})", list.join(", ")));
                }
            }
            println!("{}", descr);
            if let Some(xset) = cross_calls.get(&f.name) {
                if !xset.is_empty() {
                    let list: Vec<_> = xset.iter().cloned().collect();
                    println!("      cross-module: {}", list.join(", "));
                }
            }
        }
    }
    if !any_entry {
        println!("    (none)");
    }

    // Public (non-entry) functions.
    println!("  Public functions (no `entry` keyword):");
    let mut any_public = false;
    for f in &fns {
        if f.is_public && !f.is_entry {
            any_public = true;
            let called = calls.get(&f.name);
            let mut descr = format!("    {}", f.name);
            if let Some(set) = called {
                if !set.is_empty() {
                    let list: Vec<_> = set.iter().cloned().collect();
                    descr.push_str(&format!(" (calls: {})", list.join(", ")));
                }
            }
            println!("{}", descr);
            if let Some(xset) = cross_calls.get(&f.name) {
                if !xset.is_empty() {
                    let list: Vec<_> = xset.iter().cloned().collect();
                    println!("      cross-module: {}", list.join(", "));
                }
            }
        }
    }
    if !any_public {
        println!("    (none)");
    }

    Ok(())
}

/// Naive detection of cross-module calls in a single line of Move source.
/// Returns targets like `module::func` or `0x2::transfer::public_transfer`.
fn find_cross_module_calls(
    line: &str,
    module_short: &str,
    local_names: &std::collections::BTreeSet<String>,
) -> Vec<String> {
    let mut out = Vec::new();
    // Strip comments.
    let t = if let Some(idx) = line.find("//") {
        &line[..idx]
    } else {
        line
    };
    for part in t.split(|c: char| c.is_whitespace() || c == ';' || c == ',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        // Only consider things that look like calls: something with '('.
        if let Some(before_paren) = part.split('(').next() {
            if !before_paren.contains("::") {
                continue;
            }
            let candidate = before_paren.trim_end_matches(|c: char| c == '<' || c == '>');
            // Skip same-module calls we already accounted for.
            if candidate.starts_with(module_short)
                || candidate.starts_with("Self::")
                || local_names.contains(candidate)
            {
                continue;
            }
            // Basic sanity: ensure last segment looks like a function name.
            if let Some(last_seg) = candidate.split("::").last() {
                if last_seg.chars().all(|ch| ch.is_alphanumeric() || ch == '_') {
                    out.push(candidate.to_string());
                }
            }
        }
    }
    out
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
