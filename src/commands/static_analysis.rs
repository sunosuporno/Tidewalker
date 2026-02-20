//! Static source analysis for Move modules.
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn run_static_analysis(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use serde::Deserialize;

    #[derive(Deserialize)]
    struct MovePackage {
        package: Package,
    }
    #[derive(Deserialize)]
    struct Package {
        name: String,
    }

    let path = package_path
        .canonicalize()
        .unwrap_or_else(|_| package_path.to_path_buf());
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

    println!(
        "Static analysis for package '{}' at {}",
        pkg_name,
        path.display()
    );

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

        fns.push(FnInfo {
            name,
            is_entry,
            is_public,
            start: start_line,
            end: end_line,
        });
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
                if t.contains(&needle_plain) || t.contains(&needle_mod) || t.contains(&needle_self)
                {
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
    println!(
        "\nModule: {}",
        if module_name.is_empty() {
            "<unknown>"
        } else {
            &module_name
        }
    );

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
