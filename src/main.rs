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
                run_static_analysis(&path)
            } else {
                run_tests_and_log(&path)
            }
        }
        Commands::GenerateSetup { path } => run_generate_setup(&path),
        Commands::Generate { path } => run_generate(&path),
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
    let mut struct_fields_by_module: std::collections::HashMap<
        String,
        std::collections::HashMap<String, Vec<(String, String)>>,
    > = std::collections::HashMap::new();
    let mut alerts: Vec<(String, String)> = Vec::new();
    let mut file_for_module: std::collections::HashMap<String, String> = std::collections::HashMap::new();

    for file in &move_files {
        let file_name = file.file_name().unwrap_or_default().to_string_lossy().to_string();
        match extract_setup_info(file) {
            Ok(info) => {
                file_for_module.insert(info.module_name.clone(), file_name.clone());
                struct_fields_by_module.insert(info.module_name.clone(), info.struct_field_types.clone());
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
        let empty_fields: std::collections::HashMap<String, Vec<(String, String)>> =
            std::collections::HashMap::new();
        let struct_fields = struct_fields_by_module.get(mod_name).unwrap_or(&empty_fields);
        match inject_test_only_helpers(&source_path, mod_name, shared, caps, struct_fields) {
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

fn run_generate(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Always do setup first (same behavior as `generate-setup`).
    run_generate_setup(package_path)?;
    generate_tests(package_path)
}

#[derive(Debug, Clone)]
struct FnDecl {
    module_name: String,
    fn_name: String,
    params: Vec<ParamDecl>,
    return_ty: Option<String>,
    is_public: bool,
    is_entry: bool,
    is_test_only: bool,
    /// Tidewalker directive preconditions: functions that must be called first.
    requires: Vec<String>,
    numeric_effects: Vec<NumericEffect>,
    calls: Vec<CallSite>,
}

#[derive(Debug, Clone)]
struct ParamDecl {
    name: String,
    ty: String,
}

#[derive(Debug, Clone)]
struct NumericEffect {
    base_var: String,
    field: String,
    op: NumericOp,
}

#[derive(Debug, Clone)]
struct AccessorSig {
    fn_name: String,
    param_ty: String,
}

#[derive(Debug, Clone)]
enum NumericOp {
    Add(String),
    Sub(String),
    Mul(String),
    Div(String),
    Mod(String),
    Set(String),
    Changed,
}

#[derive(Debug, Clone)]
struct CallSite {
    callee_fn: String,
    arg_exprs: Vec<String>,
}

fn generate_tests(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
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

    let mut decls: Vec<FnDecl> = Vec::new();
    for file in &move_files {
        let content = fs::read_to_string(file)?;
        decls.extend(extract_public_fns(&content));
    }

    let mut skipped: Vec<(String, String, String)> = Vec::new(); // (module, fn, reason)
    let mut out: Vec<String> = Vec::new();
    out.push("// Generated by Tidewalker. Do not edit by hand.".to_string());
    out.push("#[test_only]".to_string());
    out.push(format!("module {}::tidewalker_generated_tests {{", pkg_name));
    out.push("    use sui::test_scenario;".to_string());
    out.push("    use sui::coin;".to_string());
    out.push("    use sui::coin::Coin;".to_string());
    out.push("    use sui::transfer;".to_string());
    out.push("".to_string());
    out.push("    const SUPER_USER: address = @0xA;".to_string());
    out.push("    const OTHER: address = @0xB;".to_string());
    out.push("".to_string());

    let accessor_map = build_accessor_map(&decls);
    let helper_catalog = build_helper_catalog(&decls);
    let fn_lookup = build_fn_lookup(&decls);
    let effects_map = build_chained_effect_map(&decls, 4);
    for d in decls {
        if d.is_test_only {
            continue;
        }
        if !d.is_public && !d.is_entry {
            continue;
        }
        let key = format!("{}::{}", d.module_name, d.fn_name);
        let resolved_effects = effects_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.numeric_effects.clone());
        if let Some(test_lines) =
            render_best_effort_test(&d, &accessor_map, &helper_catalog, &fn_lookup, &resolved_effects)
        {
            for l in test_lines {
                out.push(format!("    {}", l));
            }
            out.push("".to_string());
        } else {
            skipped.push((d.module_name, d.fn_name, "unsupported signature for first-pass generation".to_string()));
        }
    }

    if !skipped.is_empty() {
        out.push("    // ---- Skipped functions (best-effort generator couldn’t synthesize calls yet) ----".to_string());
        for (m, f, r) in &skipped {
            out.push(format!("    // {}::{} — {}", m, f, r));
        }
        out.push("".to_string());
    }

    out.push("}".to_string());

    let tests_dir = path.join("tests");
    fs::create_dir_all(&tests_dir)?;
    let out_path = tests_dir.join("tidewalker_generated_tests.move");
    fs::write(&out_path, out.join("\n"))?;
    println!("Wrote {}", out_path.display());

    Ok(())
}

fn extract_public_fns(content: &str) -> Vec<FnDecl> {
    let lines: Vec<&str> = content.lines().collect();
    let mut module_name = String::new();
    for line in &lines {
        let t = line.trim();
        if t.starts_with("module ") {
            if let Some(rest) = t.strip_prefix("module ") {
                let until = rest.split_whitespace().next().unwrap_or(rest);
                module_name = until.trim_end_matches(';').to_string();
            }
            break;
        }
    }

    let mut out = Vec::new();
    let mut pending_test_only_attr = false;
    let mut pending_requires: Vec<String> = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim();
        if t == "#[test_only]" {
            pending_test_only_attr = true;
            i += 1;
            continue;
        }
        if t.contains("@tidewalker") && t.contains("requires") {
            // Example: `/// @tidewalker requires increment`
            if let Some(after) = t.splitn(2, "requires").nth(1) {
                for part in after
                    .trim()
                    .split(|c: char| c == ',' || c.is_whitespace())
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty())
                {
                    pending_requires.push(part.to_string());
                }
            }
            i += 1;
            continue;
        }
        let (is_fn_decl, is_public, is_entry, name) = if t.starts_with("public entry fun ") {
            let after = &t["public entry fun ".len()..];
            let name = after.split(|c: char| c == '(' || c == '<' || c.is_whitespace()).next().unwrap_or("").to_string();
            (true, true, true, name)
        } else if t.starts_with("entry fun ") {
            let after = &t["entry fun ".len()..];
            let name = after.split(|c: char| c == '(' || c == '<' || c.is_whitespace()).next().unwrap_or("").to_string();
            (true, false, true, name)
        } else if t.starts_with("public fun ") {
            let after = &t["public fun ".len()..];
            let name = after.split(|c: char| c == '(' || c == '<' || c.is_whitespace()).next().unwrap_or("").to_string();
            (true, true, false, name)
        } else if t.starts_with("public(package) fun ") {
            let after = &t["public(package) fun ".len()..];
            let name = after.split(|c: char| c == '(' || c == '<' || c.is_whitespace()).next().unwrap_or("").to_string();
            (true, true, false, name)
        } else if t.starts_with("fun ") {
            let after = &t["fun ".len()..];
            let name = after.split(|c: char| c == '(' || c == '<' || c.is_whitespace()).next().unwrap_or("").to_string();
            (true, false, false, name)
        } else {
            (false, false, false, String::new())
        };
        if !is_fn_decl || name.is_empty() {
            // Keep #[test_only] pending across blank lines and comments (docs often sit between
            // the attribute and the function). Clear only on non-comment, non-empty content.
            if pending_test_only_attr
                && (t.is_empty() || t.starts_with("//") || t.starts_with("///"))
            {
                i += 1;
                continue;
            }
            if !t.is_empty() && !t.starts_with("//") && !t.starts_with("///") {
                pending_test_only_attr = false;
                pending_requires.clear();
            }
            i += 1;
            continue;
        }
        if name == "init" {
            pending_test_only_attr = false;
            pending_requires.clear();
            i += 1;
            continue;
        }
        let is_test_only = pending_test_only_attr;

        let fn_start = i;
        // Collect header across lines until we see ')'.
        let mut header = t.to_string();
        let mut j = i + 1;
        while j < lines.len() && !header.contains(')') {
            header.push(' ');
            header.push_str(lines[j].trim());
            j += 1;
        }
        pending_test_only_attr = false;

        let params_str = match header.split_once('(').and_then(|(_, rest)| rest.split_once(')')).map(|(p, _)| p) {
            Some(p) => p,
            None => continue,
        };
        let return_ty = extract_return_type(&header);
        let params = split_params(params_str)
            .into_iter()
            .filter_map(|p| {
                p.split_once(':').map(|(name, ty)| ParamDecl {
                    name: name.trim().to_string(),
                    ty: ty.trim().to_string(),
                })
            })
            .collect::<Vec<_>>();

        let mut brace_depth = 0i32;
        let mut body_start: Option<usize> = None;
        let mut body_end: Option<usize> = None;
        for (k, l) in lines.iter().enumerate().skip(fn_start) {
            for c in l.chars() {
                if c == '{' {
                    brace_depth += 1;
                    if brace_depth == 1 {
                        body_start = Some(k + 1);
                    }
                } else if c == '}' {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        body_end = Some(k);
                        break;
                    }
                }
            }
            if body_end.is_some() {
                break;
            }
        }
        let body_lines = match (body_start, body_end) {
            (Some(s), Some(e)) if s <= e => lines[s..=e].iter().map(|x| x.to_string()).collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        let numeric_effects = extract_numeric_effects_from_body(&body_lines);
        let calls = extract_same_module_calls_from_body(&body_lines, &module_name);

        out.push(FnDecl {
            module_name: module_name.clone(),
            fn_name: name,
            params,
            return_ty,
            is_public,
            is_entry,
            is_test_only,
            requires: std::mem::take(&mut pending_requires),
            numeric_effects,
            calls,
        });
        i = body_end.map(|e| e + 1).unwrap_or(j);
    }
    out
}

fn split_params(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut angle = 0i32;
    for ch in s.chars() {
        match ch {
            '<' => { angle += 1; cur.push(ch); }
            '>' => { angle -= 1; cur.push(ch); }
            ',' if angle == 0 => {
                let t = cur.trim();
                if !t.is_empty() {
                    out.push(t.to_string());
                }
                cur.clear();
            }
            _ => cur.push(ch),
        }
    }
    let t = cur.trim();
    if !t.is_empty() {
        out.push(t.to_string());
    }
    out
}

fn render_best_effort_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    numeric_effects: &[NumericEffect],
) -> Option<Vec<String>> {
    let fq = format!("{}::{}", d.module_name, d.fn_name);
    let mut lines = Vec::new();
    lines.push("#[test]".to_string());
    lines.push(format!("fun test_call_{}_{}() {{", d.module_name.split("::").last().unwrap_or("m"), d.fn_name));
    lines.push("    let mut scenario = test_scenario::begin(SUPER_USER);".to_string());

    let mut args: Vec<String> = Vec::new();
    let mut needs_coin = false;
    let mut param_runtime: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut param_arg_values: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut object_needs: Vec<ObjectNeed> = Vec::new();

    for param in &d.params {
        let t = param.ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if t == "address" {
            let v = "OTHER".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "u64" {
            let v = "1".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "bool" {
            let v = "false".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t.contains("Coin<") || t.contains("Coin <") {
            needs_coin = true;
            if t.starts_with("&mut") {
                args.push("&mut coin".to_string());
            } else {
                args.push("&coin".to_string());
            }
        } else if t.starts_with('&') {
            let obj = ObjectNeed::new(param);
            if obj.is_mut {
                args.push(format!("&mut {}", obj.var_name));
            } else {
                args.push(format!("&{}", obj.var_name));
            }
            param_runtime.insert(param.name.clone(), obj.var_name.clone());
            object_needs.push(obj);
        } else {
            return None;
        }
    }

    let mut shared_objects: Vec<ObjectNeed> = Vec::new();
    let mut owned_objects: Vec<ObjectNeed> = Vec::new();
    if !object_needs.is_empty() {
        let module_helpers = helper_catalog.get(&d.module_name)?;
        for obj in &object_needs {
            if module_helpers.shared_types.contains(&obj.type_key) {
                shared_objects.push(obj.clone());
            } else if module_helpers.owned_types.contains(&obj.type_key) {
                owned_objects.push(obj.clone());
            } else {
                return None;
            }
        }
    }
    let has_shared_objects = !shared_objects.is_empty();

    if has_shared_objects {
        lines.push("    {".to_string());
        for obj in &shared_objects {
            lines.push(format!(
                "        {}::create_and_share_{}_for_testing(test_scenario::ctx(&mut scenario));",
                d.module_name, obj.type_key
            ));
        }
        lines.push("    };".to_string());
        lines.push("    test_scenario::next_tx(&mut scenario, SUPER_USER);".to_string());
    }

    let mut object_vars_by_type: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for obj in &object_needs {
        object_vars_by_type
            .entry(obj.type_key.clone())
            .or_insert_with(|| obj.var_name.clone());
    }

    let (snapshot_before_lines, snapshot_after_lines) = build_numeric_assertion_lines(
        d,
        numeric_effects,
        &param_runtime,
        &param_arg_values,
        accessor_map,
    );

    lines.push("    {".to_string());
    if needs_coin {
        lines.push("        let mut coin = coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario));".to_string());
    }
    for obj in &owned_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        lines.push(format!(
            "        let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
            maybe_mut, obj.var_name, d.module_name, obj.type_key
        ));
    }
    for obj in &shared_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        let obj_ty = qualify_type_for_module(&d.module_name, &obj.type_name);
        lines.push(format!(
            "        let {}{} = scenario.take_shared<{}>();",
            maybe_mut, obj.var_name, obj_ty
        ));
    }

    for req in &d.requires {
        if let Some(module_fns) = fn_lookup.get(&d.module_name) {
            if let Some(req_decl) = module_fns.get(req) {
                if let Some(req_args) = synthesize_call_args_for_fn(req_decl, &object_vars_by_type, needs_coin) {
                    lines.push(format!(
                        "        {}::{}({});",
                        d.module_name,
                        req_decl.fn_name,
                        req_args.join(", ")
                    ));
                }
            }
        }
    }
    for l in snapshot_before_lines {
        lines.push(format!("        {}", l));
    }
    lines.push(format!("        {}({});", fq, args.join(", ")));
    for l in snapshot_after_lines {
        lines.push(format!("        {}", l));
    }
    for obj in &shared_objects {
        lines.push(format!("        test_scenario::return_shared({});", obj.var_name));
    }
    for obj in &owned_objects {
        lines.push(format!("        transfer::public_transfer({}, SUPER_USER);", obj.var_name));
    }
    if needs_coin {
        lines.push("        transfer::public_transfer(coin, SUPER_USER);".to_string());
    }
    lines.push("    };".to_string());

    // Ensure scenario is closed.
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
}

#[derive(Debug, Clone)]
struct ObjectNeed {
    type_name: String,
    type_key: String,
    var_name: String,
    is_mut: bool,
}

impl ObjectNeed {
    fn new(param: &ParamDecl) -> Self {
        let type_name = normalize_param_object_type(&param.ty);
        let type_key = type_key_from_type_name(&type_name);
        Self {
            type_name,
            type_key,
            var_name: format!("obj_{}", sanitize_ident(&param.name)),
            is_mut: param.ty.trim().starts_with("&mut"),
        }
    }
}

fn extract_numeric_effects_from_body(body_lines: &[String]) -> Vec<NumericEffect> {
    let mut effects = Vec::new();
    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() || no_comments.starts_with("assert!") {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        let (lhs, rhs) = match stmt.split_once('=') {
            Some((l, r)) => (l.trim(), r.trim()),
            None => continue,
        };
        if lhs.ends_with('!') || lhs.contains("==") {
            continue;
        }
        let (base_var, field) = match parse_field_access(lhs) {
            Some(parts) => parts,
            None => continue,
        };
        let lhs_norm = remove_whitespace(lhs);
        let rhs_norm = remove_whitespace(rhs);

        let op = if let Some(rest) = rhs_norm.strip_prefix(&lhs_norm) {
            let (op_char, raw_lit) = match rest.chars().next() {
                Some(c @ ('+' | '-' | '*' | '/' | '%')) => (c, rest[1..].trim()),
                _ => continue,
            };
            if let Some(lit) = parse_numeric_literal(raw_lit) {
                match op_char {
                    '+' => NumericOp::Add(lit),
                    '-' => NumericOp::Sub(lit),
                    '*' => NumericOp::Mul(lit),
                    '/' => NumericOp::Div(lit),
                    '%' => NumericOp::Mod(lit),
                    _ => continue,
                }
            } else if is_ident(raw_lit) {
                match op_char {
                    '+' => NumericOp::Add(raw_lit.to_string()),
                    '-' => NumericOp::Sub(raw_lit.to_string()),
                    '*' => NumericOp::Mul(raw_lit.to_string()),
                    '/' => NumericOp::Div(raw_lit.to_string()),
                    '%' => NumericOp::Mod(raw_lit.to_string()),
                    _ => continue,
                }
            } else {
                // We can still assert a state change happened even when exact delta is unknown.
                NumericOp::Changed
            }
        } else if let Some(v) = parse_numeric_literal(&rhs_norm) {
            NumericOp::Set(v)
        } else if is_ident(&rhs_norm) {
            NumericOp::Set(rhs_norm)
        } else {
            continue;
        };

        effects.push(NumericEffect {
            base_var,
            field,
            op,
        });
    }
    effects
}

fn extract_return_type(header: &str) -> Option<String> {
    let after_paren = header.split_once(')')?.1.trim();
    let after_colon = after_paren.strip_prefix(':')?;
    let before_brace = after_colon.split('{').next().unwrap_or(after_colon).trim();
    if before_brace.is_empty() {
        None
    } else {
        Some(before_brace.to_string())
    }
}

fn extract_same_module_calls_from_body(body_lines: &[String], module_name: &str) -> Vec<CallSite> {
    let module_short = module_name.split("::").last().unwrap_or(module_name);
    let mut out = Vec::new();

    for line in body_lines {
        let text = line.split("//").next().unwrap_or("");
        let chars: Vec<char> = text.chars().collect();
        let mut i = 0usize;
        while i < chars.len() {
            if chars[i] != '(' || i == 0 {
                i += 1;
                continue;
            }
            let mut j: isize = (i as isize) - 1;
            while j >= 0 && chars[j as usize].is_whitespace() {
                j -= 1;
            }
            if j < 0 {
                i += 1;
                continue;
            }
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
                i += 1;
                continue;
            }
            if start > 0 && chars[start - 1] == '.' {
                i += 1;
                continue;
            }
            let token: String = chars[start..=end].iter().collect();
            if token.is_empty() || token == "assert" {
                i += 1;
                continue;
            }

            let callee = if token.contains("::") {
                if token.starts_with("Self::") {
                    token.split("::").last().unwrap_or("").to_string()
                } else if token.starts_with(&format!("{}::", module_short)) {
                    token.split("::").last().unwrap_or("").to_string()
                } else {
                    i += 1;
                    continue;
                }
            } else {
                token
            };
            if callee.is_empty() {
                i += 1;
                continue;
            }

            let mut depth = 1i32;
            let mut k = i + 1;
            while k < chars.len() {
                if chars[k] == '(' {
                    depth += 1;
                } else if chars[k] == ')' {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                k += 1;
            }
            if k >= chars.len() {
                i += 1;
                continue;
            }

            let args_raw: String = chars[i + 1..k].iter().collect();
            out.push(CallSite {
                callee_fn: callee,
                arg_exprs: split_args(&args_raw),
            });
            i = k + 1;
        }
    }

    out
}

fn split_args(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut angle = 0i32;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    for ch in s.chars() {
        match ch {
            '<' => {
                angle += 1;
                cur.push(ch);
            }
            '>' => {
                angle -= 1;
                cur.push(ch);
            }
            '(' => {
                paren += 1;
                cur.push(ch);
            }
            ')' => {
                paren -= 1;
                cur.push(ch);
            }
            '[' => {
                bracket += 1;
                cur.push(ch);
            }
            ']' => {
                bracket -= 1;
                cur.push(ch);
            }
            ',' if angle == 0 && paren == 0 && bracket == 0 => {
                let t = cur.trim();
                if !t.is_empty() {
                    out.push(t.to_string());
                }
                cur.clear();
            }
            _ => cur.push(ch),
        }
    }
    let t = cur.trim();
    if !t.is_empty() {
        out.push(t.to_string());
    }
    out
}

fn parse_field_access(expr: &str) -> Option<(String, String)> {
    let norm = remove_whitespace(expr);
    if norm.contains('(') || norm.contains(')') || norm.matches('.').count() != 1 {
        return None;
    }
    let (base, field) = norm.split_once('.')?;
    if base.is_empty() || field.is_empty() {
        return None;
    }
    if !is_ident(base) || !is_ident(field) {
        return None;
    }
    Some((base.to_string(), field.to_string()))
}

fn remove_whitespace(s: &str) -> String {
    s.chars().filter(|c| !c.is_whitespace()).collect::<String>()
}

fn parse_numeric_literal(raw: &str) -> Option<String> {
    let mut lit = raw.trim();
    while lit.starts_with('(') && lit.ends_with(')') && lit.len() >= 2 {
        lit = &lit[1..lit.len() - 1];
        lit = lit.trim();
    }
    if lit.is_empty() {
        return None;
    }
    if lit.chars().all(|c| c.is_ascii_digit() || c == '_') && lit.chars().any(|c| c.is_ascii_digit()) {
        return Some(lit.to_string());
    }
    None
}

fn is_ident(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn sanitize_ident(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
        .collect::<String>()
}

fn is_numeric_type(ty: &str) -> bool {
    matches!(
        ty.trim(),
        "u8" | "u16" | "u32" | "u64" | "u128" | "u256"
    )
}

fn normalize_param_object_type(ty: &str) -> String {
    ty.replace("&mut", "")
        .replace('&', "")
        .trim()
        .to_string()
}

fn type_key_from_type_name(type_name: &str) -> String {
    let clean = type_name.trim();
    let no_generics = clean.split('<').next().unwrap_or(clean).trim();
    no_generics
        .split("::")
        .last()
        .unwrap_or(no_generics)
        .trim()
        .to_lowercase()
}

fn qualify_type_for_module(module_name: &str, type_name: &str) -> String {
    let clean = type_name.trim();
    if clean.contains("::") {
        clean.to_string()
    } else {
        format!("{}::{}", module_name, clean)
    }
}

fn build_accessor_map(
    decls: &[FnDecl],
) -> std::collections::HashMap<String, Vec<AccessorSig>> {
    let mut out: std::collections::HashMap<String, Vec<AccessorSig>> =
        std::collections::HashMap::new();
    for d in decls {
        if d.is_test_only || !d.is_public {
            continue;
        }
        if d.params.len() != 1 {
            continue;
        }
        let ret = match &d.return_ty {
            Some(r) if is_numeric_type(r) => r,
            _ => continue,
        };
        let pty = d.params[0].ty.trim();
        if !pty.starts_with('&') {
            continue;
        }
        let _ = ret; // keep explicit intent: numeric accessor only
        out.entry(d.module_name.clone())
            .or_default()
            .push(AccessorSig {
                fn_name: d.fn_name.clone(),
                param_ty: normalize_param_object_type(pty),
            });
    }
    out
}

#[derive(Debug, Default)]
struct ModuleHelperCatalog {
    shared_types: std::collections::HashSet<String>,
    owned_types: std::collections::HashSet<String>,
}

fn build_helper_catalog(
    decls: &[FnDecl],
) -> std::collections::HashMap<String, ModuleHelperCatalog> {
    let mut out: std::collections::HashMap<String, ModuleHelperCatalog> =
        std::collections::HashMap::new();
    for d in decls {
        if !d.is_test_only {
            continue;
        }
        if let Some(type_key) = d
            .fn_name
            .strip_prefix("create_and_share_")
            .and_then(|x| x.strip_suffix("_for_testing"))
        {
            out.entry(d.module_name.clone())
                .or_default()
                .shared_types
                .insert(type_key.to_string());
            continue;
        }
        if let Some(type_key) = d
            .fn_name
            .strip_prefix("create_")
            .and_then(|x| x.strip_suffix("_for_testing"))
        {
            out.entry(d.module_name.clone())
                .or_default()
                .owned_types
                .insert(type_key.to_string());
        }
    }
    out
}

fn build_fn_lookup(
    decls: &[FnDecl],
) -> std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>> {
    let mut out: std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>> =
        std::collections::HashMap::new();
    for d in decls {
        out.entry(d.module_name.clone())
            .or_default()
            .insert(d.fn_name.clone(), d.clone());
    }
    out
}

fn synthesize_call_args_for_fn(
    d: &FnDecl,
    object_vars_by_type: &std::collections::HashMap<String, String>,
    has_coin: bool,
) -> Option<Vec<String>> {
    let mut args = Vec::new();
    for p in &d.params {
        let t = p.ty.trim();
        if t.starts_with('&') && !t.contains("TxContext") && !t.contains("Coin<") && !t.contains("Coin <") {
            let ty = normalize_param_object_type(t);
            let key = type_key_from_type_name(&ty);
            let var = object_vars_by_type.get(&key)?;
            if t.starts_with("&mut") {
                args.push(format!("&mut {}", var));
            } else {
                args.push(format!("&{}", var));
            }
        } else if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if t.contains("Coin<") || t.contains("Coin <") {
            if !has_coin {
                return None;
            }
            if t.starts_with("&mut") {
                args.push("&mut coin".to_string());
            } else {
                args.push("&coin".to_string());
            }
        } else if t == "u64" {
            args.push("1".to_string());
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            args.push("OTHER".to_string());
        } else {
            return None;
        }
    }
    Some(args)
}

fn build_chained_effect_map(
    decls: &[FnDecl],
    max_depth: usize,
) -> std::collections::HashMap<String, Vec<NumericEffect>> {
    let mut by_module_and_fn: std::collections::HashMap<String, std::collections::HashMap<String, &FnDecl>> =
        std::collections::HashMap::new();
    for d in decls {
        by_module_and_fn
            .entry(d.module_name.clone())
            .or_default()
            .insert(d.fn_name.clone(), d);
    }

    let mut cache: std::collections::HashMap<String, Vec<NumericEffect>> =
        std::collections::HashMap::new();
    for d in decls {
        let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
        let key = format!("{}::{}", d.module_name, d.fn_name);
        let resolved = collect_effects_for_fn(
            d,
            &by_module_and_fn,
            &mut cache,
            &mut visiting,
            0,
            max_depth,
        );
        cache.insert(key, resolved);
    }
    cache
}

fn collect_effects_for_fn(
    d: &FnDecl,
    by_module_and_fn: &std::collections::HashMap<String, std::collections::HashMap<String, &FnDecl>>,
    cache: &mut std::collections::HashMap<String, Vec<NumericEffect>>,
    visiting: &mut std::collections::HashSet<String>,
    depth: usize,
    max_depth: usize,
) -> Vec<NumericEffect> {
    let key = format!("{}::{}", d.module_name, d.fn_name);
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if depth > max_depth || visiting.contains(&key) {
        return d.numeric_effects.clone();
    }
    visiting.insert(key.clone());

    let mut out = d.numeric_effects.clone();
    if let Some(module_map) = by_module_and_fn.get(&d.module_name) {
        for call in &d.calls {
            let callee = match module_map.get(&call.callee_fn) {
                Some(c) => *c,
                None => continue,
            };
            let callee_effects = collect_effects_for_fn(
                callee,
                by_module_and_fn,
                cache,
                visiting,
                depth + 1,
                max_depth,
            );
            for eff in callee_effects {
                let callee_idx = callee.params.iter().position(|p| p.name == eff.base_var);
                let idx = match callee_idx {
                    Some(v) => v,
                    None => continue,
                };
                let arg = match call.arg_exprs.get(idx) {
                    Some(a) => a.trim(),
                    None => continue,
                };
                if !is_ident(arg) {
                    continue;
                }
                out.push(NumericEffect {
                    base_var: arg.to_string(),
                    field: eff.field,
                    op: eff.op,
                });
            }
        }
    }

    let mut deduped = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for eff in out {
        let sig = format!("{}::{}::{:?}", eff.base_var, eff.field, eff.op);
        if seen.insert(sig) {
            deduped.push(eff);
        }
    }

    visiting.remove(&key);
    cache.insert(key, deduped.clone());
    deduped
}

fn resolve_read_expr(
    d: &FnDecl,
    eff: &NumericEffect,
    runtime_var: &str,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
) -> Option<String> {
    let param = d.params.iter().find(|p| p.name == eff.base_var)?;
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = accessor_map.get(&d.module_name)?;
    let has_accessor = module_accessors.iter().any(|a| {
        a.param_ty == param_obj_ty && a.fn_name == eff.field
    });
    if has_accessor {
        Some(format!("{}::{}(&{})", d.module_name, eff.field, runtime_var))
    } else {
        None
    }
}

fn build_numeric_assertion_lines(
    d: &FnDecl,
    numeric_effects: &[NumericEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    param_arg_values: &std::collections::HashMap<String, String>,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
) -> (Vec<String>, Vec<String>) {
    let mut before = Vec::new();
    let mut after = Vec::new();
    let mut seen_targets: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut direct_effect_counts: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for eff in &d.numeric_effects {
        let key = format!("{}.{}", eff.base_var, eff.field);
        *direct_effect_counts.entry(key).or_insert(0) += 1;
    }
    let has_internal_calls = !d.calls.is_empty();
    let mut idx = 0usize;

    for eff in numeric_effects {
        let runtime_var = match param_runtime.get(&eff.base_var) {
            Some(v) => v,
            None => continue,
        };
        let key = format!("{}.{}", runtime_var, eff.field);
        if seen_targets.contains(&key) {
            continue;
        }
        let read_expr = match resolve_read_expr(d, eff, runtime_var, accessor_map) {
            Some(expr) => expr,
            None => continue,
        };
        seen_targets.insert(key.clone());

        let before_name = format!(
            "before_{}_{}",
            sanitize_ident(runtime_var),
            sanitize_ident(&eff.field)
        );
        let after_name = format!(
            "after_{}_{}",
            sanitize_ident(runtime_var),
            sanitize_ident(&eff.field)
        );
        before.push(format!("let {} = {};", before_name, read_expr));
        after.push(format!("let {} = {};", after_name, read_expr));

        let direct_key = format!("{}.{}", eff.base_var, eff.field);
        let direct_count = direct_effect_counts.get(&direct_key).copied().unwrap_or(0);
        let prefer_unqualified = has_internal_calls || direct_count > 1;

        let exact_assert = if !prefer_unqualified {
            build_exact_assert_line(
                &eff.op,
                &after_name,
                &before_name,
                param_arg_values,
                900 + idx as u64,
            )
        } else {
            None
        };

        if let Some(line) = exact_assert {
            after.push(line);
        } else {
            after.push(format!(
                "assert!({} != {}, {});",
                after_name,
                before_name,
                900 + idx as u64
            ));
        }
        idx += 1;
    }

    (before, after)
}

fn resolve_numeric_operand(
    token: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
) -> Option<String> {
    if parse_numeric_literal(token).is_some() {
        return Some(token.to_string());
    }
    if is_ident(token) {
        let bound = param_arg_values.get(token)?;
        if parse_numeric_literal(bound).is_some() {
            return Some(bound.clone());
        }
    }
    None
}

fn build_exact_assert_line(
    op: &NumericOp,
    after_name: &str,
    before_name: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
    code: u64,
) -> Option<String> {
    match op {
        NumericOp::Add(v) => Some(format!(
            "assert!({} == {} + {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Sub(v) => Some(format!(
            "assert!({} == {} - {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Mul(v) => Some(format!(
            "assert!({} == {} * {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Div(v) => Some(format!(
            "assert!({} == {} / {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Mod(v) => Some(format!(
            "assert!({} == {} % {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Set(v) => Some(format!(
            "assert!({} == {}, {});",
            after_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Changed => None,
    }
}

#[derive(Debug, Default)]
struct ModuleSetupInfo {
    module_name: String,
    shared_types: Vec<(String, String)>,
    cap_types: Vec<(String, String)>,
    struct_field_types: std::collections::HashMap<String, Vec<(String, String)>>,
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
    let mut struct_field_types: std::collections::HashMap<String, Vec<(String, String)>> =
        std::collections::HashMap::new();
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
            let mut fields: Vec<(String, String)> = Vec::new();
            while i < lines.len() && !lines[i].contains('{') {
                i += 1;
            }
            i += 1;
            while i < lines.len() {
                let f = lines[i].trim();
                if f.starts_with('}') {
                    break;
                }
                if let Some((field_name, field_ty)) = parse_struct_field_line(f) {
                    fields.push((field_name, field_ty));
                }
                i += 1;
            }
            if !name.is_empty() && !fields.is_empty() {
                struct_field_types.insert(name, fields);
            }
        }
        i += 1;
    }
    info.struct_field_types = struct_field_types;

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

fn parse_struct_field_line(line: &str) -> Option<(String, String)> {
    let t = line
        .split("//")
        .next()
        .unwrap_or("")
        .trim()
        .trim_end_matches(',')
        .trim();
    if t.is_empty() || t.starts_with('#') {
        return None;
    }
    let (name, ty) = t.split_once(':')?;
    let name = name.trim();
    let ty = ty.trim();
    if name.is_empty() || ty.is_empty() {
        return None;
    }
    Some((name.to_string(), ty.to_string()))
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
    struct_fields: &std::collections::HashMap<String, Vec<(String, String)>>,
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
        if let Some(fields) = struct_fields.get(type_name) {
            for (field_name, field_ty) in fields {
                let init = default_field_initializer(field_name, field_ty);
                out.push(format!("        {}: {},", field_name, init));
            }
        } else {
            out.push("        id: sui::object::new(ctx),".to_string());
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
        if let Some(fields) = struct_fields.get(type_name) {
            for (field_name, field_ty) in fields {
                let init = default_field_initializer(field_name, field_ty);
                out.push(format!("        {}: {},", field_name, init));
            }
        } else {
            out.push("        id: sui::object::new(ctx),".to_string());
        }
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
    struct_fields: &std::collections::HashMap<String, Vec<(String, String)>>,
) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(source_path)?;
    let mut lines: Vec<String> = content.lines().map(String::from).collect();

    let snippet = generate_in_module_test_only_snippet(mod_name, shared, caps, struct_fields);

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

fn default_field_initializer(field_name: &str, field_ty: &str) -> String {
    if field_name == "id" || field_ty.contains("UID") {
        return "sui::object::new(ctx)".to_string();
    }
    let t = field_ty.trim();
    if matches!(t, "u8" | "u16" | "u32" | "u64" | "u128" | "u256") {
        return "0".to_string();
    }
    if t == "bool" {
        return "false".to_string();
    }
    if t == "address" {
        return "@0x0".to_string();
    }
    if t.starts_with("vector<") {
        return "vector[]".to_string();
    }
    "0".to_string()
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
