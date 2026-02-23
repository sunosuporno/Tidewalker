//! Test generation logic.
use crate::commands::setup::run_generate_setup;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

mod catalog;
mod chain;
mod render;

pub fn run_generate(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
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
    body_lines: Vec<String>,
    is_public: bool,
    is_entry: bool,
    is_test_only: bool,
    /// Tidewalker directive preconditions: functions that must be called first.
    requires: Vec<String>,
    numeric_effects: Vec<NumericEffect>,
    vector_effects: Vec<VectorEffect>,
    coin_effects: Vec<CoinEffect>,
    coin_notes: Vec<CoinNote>,
    option_effects: Vec<OptionEffect>,
    string_effects: Vec<StringEffect>,
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
struct VectorEffect {
    base_var: String,
    field: String,
    op: VectorOp,
}

#[derive(Debug, Clone)]
struct CoinEffect {
    base_var: String,
    op: CoinOp,
}

#[derive(Debug, Clone)]
struct OptionEffect {
    base_var: String,
    field: String,
    op: OptionOp,
}

#[derive(Debug, Clone)]
struct StringEffect {
    base_var: String,
    field: String,
}

#[derive(Debug, Clone)]
struct AccessorSig {
    fn_name: String,
    param_ty: String,
}

#[derive(Debug, Clone)]
struct OptionAccessorSig {
    fn_name: String,
    param_ty: String,
    field: String,
    is_some_when_true: bool,
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
enum VectorOp {
    PushBack,
    PopBack,
    Insert,
    Remove,
    SwapRemove,
    Append {
        src_base_var: String,
        src_field: String,
    },
    ContentChanged,
}

#[derive(Debug, Clone)]
enum CoinOp {
    Split(String),
    Join { src_base_var: String },
    Mint(String),
    Burn(String),
    Changed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CoinNote {
    MintFlow,
    BurnFlow,
    StakeFlow,
}

#[derive(Debug, Clone)]
enum OptionOp {
    SetSome,
    SetNone,
    Changed,
}

#[derive(Debug, Default, Clone)]
struct StateChangeSummary {
    asserted: std::collections::BTreeSet<String>,
    potential: std::collections::BTreeSet<String>,
}

impl StateChangeSummary {
    fn add_asserted(&mut self, target: String) {
        self.potential.remove(&target);
        self.asserted.insert(target);
    }

    fn add_potential(&mut self, target: String) {
        if !self.asserted.contains(&target) {
            self.potential.insert(target);
        }
    }

    fn merge(&mut self, other: StateChangeSummary) {
        for t in other.asserted {
            self.add_asserted(t);
        }
        for t in other.potential {
            self.add_potential(t);
        }
    }
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
    out.push(format!(
        "module {}::tidewalker_generated_tests {{",
        pkg_name
    ));
    out.push("    use sui::test_scenario;".to_string());
    out.push("    use sui::coin;".to_string());
    out.push("    use sui::coin::Coin;".to_string());
    out.push("    use sui::transfer;".to_string());
    out.push("".to_string());
    out.push("    const SUPER_USER: address = @0xA;".to_string());
    out.push("    const OTHER: address = @0xB;".to_string());
    out.push("".to_string());

    let accessor_map = catalog::build_accessor_map(&decls);
    let option_accessor_map = catalog::build_option_accessor_map(&decls);
    let helper_catalog = catalog::build_helper_catalog(&decls);
    let fn_lookup = catalog::build_fn_lookup(&decls);
    let effects_map = chain::build_chained_effect_map(&decls, 4);
    let vector_effects_map = chain::build_chained_vector_effect_map(&decls, 4);
    let coin_effects_map = chain::build_chained_coin_effect_map(&decls, 4);
    let coin_notes_map = chain::build_chained_coin_note_map(&decls, 4);
    let option_effects_map = chain::build_chained_option_effect_map(&decls, 4);
    let string_effects_map = chain::build_chained_string_effect_map(&decls, 4);
    let deep_overflow_map = chain::build_deep_overflow_map(&decls, 4);
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
        let resolved_vector_effects = vector_effects_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.vector_effects.clone());
        let resolved_coin_effects = coin_effects_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.coin_effects.clone());
        let resolved_coin_notes = coin_notes_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.coin_notes.clone());
        let resolved_option_effects = option_effects_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.option_effects.clone());
        let resolved_string_effects = string_effects_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.string_effects.clone());
        let deep_overflow_paths = deep_overflow_map
            .get(&key)
            .cloned()
            .unwrap_or_default();
        if let Some(test_lines) = render::render_best_effort_test(
            &d,
            &accessor_map,
            &option_accessor_map,
            &helper_catalog,
            &fn_lookup,
            &resolved_effects,
            &resolved_vector_effects,
            &resolved_coin_effects,
            &resolved_coin_notes,
            &resolved_option_effects,
            &resolved_string_effects,
            &deep_overflow_paths,
        ) {
            for l in test_lines {
                out.push(format!("    {}", l));
            }
            out.push("".to_string());
        } else {
            skipped.push((
                d.module_name,
                d.fn_name,
                "unsupported signature for first-pass generation".to_string(),
            ));
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
    let option_fields_by_type = extract_option_fields_by_type(&lines);
    let string_fields_by_type = extract_string_fields_by_type(&lines);

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
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, true, true, name)
        } else if t.starts_with("entry fun ") {
            let after = &t["entry fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, false, true, name)
        } else if t.starts_with("public fun ") {
            let after = &t["public fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, true, false, name)
        } else if t.starts_with("public(package) fun ") {
            let after = &t["public(package) fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, true, false, name)
        } else if t.starts_with("fun ") {
            let after = &t["fun ".len()..];
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
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

        let params_str = match header
            .split_once('(')
            .and_then(|(_, rest)| rest.split_once(')'))
            .map(|(p, _)| p)
        {
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
            (Some(s), Some(e)) if s <= e => lines[s..=e]
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        let numeric_effects = extract_numeric_effects_from_body(&body_lines);
        let vector_effects = extract_vector_effects_from_body(&body_lines);
        let coin_effects = extract_coin_effects_from_body(&body_lines, &params);
        let coin_notes = extract_coin_notes_from_body(&body_lines);
        let option_effects =
            extract_option_effects_from_body(&body_lines, &params, &option_fields_by_type);
        let string_effects =
            extract_string_effects_from_body(&body_lines, &params, &string_fields_by_type);
        let calls = extract_same_module_calls_from_body(&body_lines, &module_name);

        out.push(FnDecl {
            module_name: module_name.clone(),
            fn_name: name,
            params,
            return_ty,
            body_lines,
            is_public,
            is_entry,
            is_test_only,
            requires: std::mem::take(&mut pending_requires),
            numeric_effects,
            vector_effects,
            coin_effects,
            coin_notes,
            option_effects,
            string_effects,
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
            '<' => {
                angle += 1;
                cur.push(ch);
            }
            '>' => {
                angle -= 1;
                cur.push(ch);
            }
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

fn extract_option_fields_by_type(
    lines: &[&str],
) -> std::collections::HashMap<String, std::collections::HashSet<String>> {
    let mut out: std::collections::HashMap<String, std::collections::HashSet<String>> =
        std::collections::HashMap::new();
    let mut i = 0usize;
    while i < lines.len() {
        let t = lines[i].trim();
        let rest = if let Some(r) = t.strip_prefix("public struct ") {
            r
        } else if let Some(r) = t.strip_prefix("struct ") {
            r
        } else {
            i += 1;
            continue;
        };
        let struct_name = rest
            .split(|c: char| c == '{' || c == '<' || c.is_whitespace())
            .next()
            .unwrap_or("")
            .trim();
        if struct_name.is_empty() {
            i += 1;
            continue;
        }

        let mut brace_depth = 0i32;
        let mut saw_open = false;
        let mut j = i;
        let mut fields: std::collections::HashSet<String> = std::collections::HashSet::new();
        while j < lines.len() {
            let line = lines[j].trim();
            if saw_open {
                if let Some((field, ty_raw)) = line.split_once(':') {
                    let field = field.trim().trim_end_matches(',').trim();
                    let ty = ty_raw
                        .split("//")
                        .next()
                        .unwrap_or("")
                        .trim()
                        .trim_end_matches(',')
                        .trim();
                    if is_ident(field) && is_option_type(ty) {
                        fields.insert(field.to_string());
                    }
                }
            }
            for ch in line.chars() {
                if ch == '{' {
                    brace_depth += 1;
                    saw_open = true;
                } else if ch == '}' {
                    brace_depth -= 1;
                }
            }
            if saw_open && brace_depth <= 0 {
                break;
            }
            j += 1;
        }
        if !fields.is_empty() {
            out.insert(type_key_from_type_name(struct_name), fields);
        }
        i = j.saturating_add(1);
    }
    out
}

fn extract_string_fields_by_type(
    lines: &[&str],
) -> std::collections::HashMap<String, std::collections::HashSet<String>> {
    let mut out: std::collections::HashMap<String, std::collections::HashSet<String>> =
        std::collections::HashMap::new();
    let mut i = 0usize;
    while i < lines.len() {
        let t = lines[i].trim();
        let rest = if let Some(r) = t.strip_prefix("public struct ") {
            r
        } else if let Some(r) = t.strip_prefix("struct ") {
            r
        } else {
            i += 1;
            continue;
        };
        let struct_name = rest
            .split(|c: char| c == '{' || c == '<' || c.is_whitespace())
            .next()
            .unwrap_or("")
            .trim();
        if struct_name.is_empty() {
            i += 1;
            continue;
        }

        let mut brace_depth = 0i32;
        let mut saw_open = false;
        let mut j = i;
        let mut fields: std::collections::HashSet<String> = std::collections::HashSet::new();
        while j < lines.len() {
            let line = lines[j].trim();
            if saw_open {
                if let Some((field, ty_raw)) = line.split_once(':') {
                    let field = field.trim().trim_end_matches(',').trim();
                    let ty = ty_raw
                        .split("//")
                        .next()
                        .unwrap_or("")
                        .trim()
                        .trim_end_matches(',')
                        .trim();
                    if is_ident(field) && is_string_type(ty) {
                        fields.insert(field.to_string());
                    }
                }
            }
            for ch in line.chars() {
                if ch == '{' {
                    brace_depth += 1;
                    saw_open = true;
                } else if ch == '}' {
                    brace_depth -= 1;
                }
            }
            if saw_open && brace_depth <= 0 {
                break;
            }
            j += 1;
        }
        if !fields.is_empty() {
            out.insert(type_key_from_type_name(struct_name), fields);
        }
        i = j.saturating_add(1);
    }
    out
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
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() || no_comments.starts_with("assert!") {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        if let Some((name, target)) = parse_alias_binding(stmt) {
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
            }
        }
        let (lhs, rhs) = match stmt.split_once('=') {
            Some((l, r)) => (l.trim(), r.trim()),
            None => continue,
        };
        if lhs.ends_with('!') || lhs.contains("==") {
            continue;
        }
        let (base_var, field) = match parse_field_access(lhs)
            .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
        {
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

fn extract_vector_effects_from_body(body_lines: &[String]) -> Vec<VectorEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for line in body_lines {
        let t = line
            .split("//")
            .next()
            .unwrap_or("")
            .trim()
            .trim_end_matches(';')
            .trim();
        if t.is_empty() {
            continue;
        }
        if let Some((name, target)) = parse_alias_binding(t) {
            aliases.insert(name, target);
            continue;
        }
        if t.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(t) {
                aliases.remove(&name);
            }
        }
        if t.contains("==") {
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "push_back(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PushBack,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "pop_back(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PopBack,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "insert(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Insert,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "remove(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Remove,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "swap_remove(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::SwapRemove,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "append(", &aliases) {
            let src = extract_method_args(t, "append(")
                .and_then(|args| args.first().cloned())
                .and_then(|arg| parse_vector_target_expr(&arg, &aliases));
            if let Some((src_base_var, src_field)) = src {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::Append {
                        src_base_var,
                        src_field,
                    },
                });
            } else {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::ContentChanged,
                });
            }
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "reverse(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "swap(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::push_back(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PushBack,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::pop_back(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PopBack,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::insert(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Insert,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::remove(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Remove,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::swap_remove(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::SwapRemove,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::append(", &aliases) {
            let src = extract_namespace_args(t, "vector::append(")
                .and_then(|args| args.get(1).cloned())
                .and_then(|arg| parse_vector_target_expr(&arg, &aliases));
            if let Some((src_base_var, src_field)) = src {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::Append {
                        src_base_var,
                        src_field,
                    },
                });
            } else {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::ContentChanged,
                });
            }
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::reverse(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::swap(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((lhs, _)) = t.split_once('=') {
            let lhs = lhs.trim();
            if lhs.contains('[') && lhs.contains(']') {
                let base = lhs.split('[').next().unwrap_or("").trim();
                if let Some((base_var, field)) = parse_vector_target_expr(base, &aliases) {
                    out.push(VectorEffect {
                        base_var,
                        field,
                        op: VectorOp::ContentChanged,
                    });
                }
            }
        }
    }
    out
}

fn extract_coin_effects_from_body(body_lines: &[String], params: &[ParamDecl]) -> Vec<CoinEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut minted_vars: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut coin_params: std::collections::HashSet<String> = std::collections::HashSet::new();
    for p in params {
        if is_coin_type(&normalize_param_object_type(&p.ty)) {
            coin_params.insert(p.name.clone());
        }
    }

    for line in body_lines {
        let t = line
            .split("//")
            .next()
            .unwrap_or("")
            .trim()
            .trim_end_matches(';')
            .trim();
        if t.is_empty() {
            continue;
        }
        if let Some((name, target)) = parse_alias_binding(t) {
            aliases.insert(name, target);
            continue;
        }
        if t.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(t) {
                aliases.remove(&name);
                minted_vars.remove(&name);
                if let Some((_, rhs)) = t.split_once('=') {
                    if let Some(amount) = parse_mint_amount_from_expr(rhs.trim()) {
                        minted_vars.insert(name, amount);
                    }
                }
            }
        }
        if t.contains("==") {
            continue;
        }

        if let Some((base_var, amount)) = parse_coin_burn_with_split(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Burn(amount),
                });
            }
            continue;
        }

        if let Some((base_var, amount)) = parse_coin_split_method(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Split(amount),
                });
            }
            continue;
        }
        if let Some((base_var, amount)) = parse_coin_split_namespace(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Split(amount),
                });
            }
            continue;
        }
        if let Some((base_var, amount)) = parse_coin_join_mint_method(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Mint(amount),
                });
            }
            continue;
        }
        if let Some((base_var, amount)) = parse_coin_join_mint_namespace(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Mint(amount),
                });
            }
            continue;
        }
        if let Some((base_var, src_base_var)) = parse_coin_join_method(t, &aliases) {
            if let Some(amount) = minted_vars.get(&src_base_var) {
                if coin_params.contains(&base_var) {
                    out.push(CoinEffect {
                        base_var,
                        op: CoinOp::Mint(amount.clone()),
                    });
                }
            } else if coin_params.contains(&base_var) && coin_params.contains(&src_base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Join { src_base_var },
                });
            } else if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Changed,
                });
            }
            continue;
        }
        if let Some((base_var, src_base_var)) = parse_coin_join_namespace(t, &aliases) {
            if let Some(amount) = minted_vars.get(&src_base_var) {
                if coin_params.contains(&base_var) {
                    out.push(CoinEffect {
                        base_var,
                        op: CoinOp::Mint(amount.clone()),
                    });
                }
            } else if coin_params.contains(&base_var) && coin_params.contains(&src_base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Join { src_base_var },
                });
            } else if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Changed,
                });
            }
            continue;
        }
        if let Some(base_var) = parse_coin_transfer_arg(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Changed,
                });
            }
        }
    }

    let mut deduped = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for eff in out {
        let sig = format!("{}::{:?}", eff.base_var, eff.op);
        if seen.insert(sig) {
            deduped.push(eff);
        }
    }
    deduped
}

fn extract_coin_notes_from_body(body_lines: &[String]) -> Vec<CoinNote> {
    let mut out = Vec::new();
    let mut seen: std::collections::HashSet<CoinNote> = std::collections::HashSet::new();
    for line in body_lines {
        let stmt = line
            .split("//")
            .next()
            .unwrap_or("")
            .trim()
            .trim_end_matches(';')
            .trim();
        if stmt.is_empty() {
            continue;
        }
        let lower = stmt.to_ascii_lowercase();
        let has_mint = (lower.contains("coin::mint(") || lower.contains("::mint("))
            && !lower.contains("mint_for_testing(");
        if has_mint && seen.insert(CoinNote::MintFlow) {
            out.push(CoinNote::MintFlow);
        }
        if lower.contains("coin::burn(") || lower.contains("::burn(") {
            if seen.insert(CoinNote::BurnFlow) {
                out.push(CoinNote::BurnFlow);
            }
        }
        let has_stake = lower.contains("request_add_stake(")
            || lower.contains("request_withdraw_stake(")
            || lower.contains("staking::")
            || lower.contains("staking_pool::")
            || lower.contains("validator::")
            || lower.contains("stake::");
        if has_stake && seen.insert(CoinNote::StakeFlow) {
            out.push(CoinNote::StakeFlow);
        }
    }
    out
}

fn parse_coin_target_expr(
    arg: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let first_arg = arg
        .trim()
        .trim_start_matches("&mut")
        .trim_start_matches('&')
        .trim();
    if !is_ident(first_arg) {
        return None;
    }
    let resolved = resolve_alias_path(first_arg, aliases);
    if is_ident(&resolved) {
        Some(resolved)
    } else {
        None
    }
}

fn normalize_coin_amount_expr(raw: &str) -> Option<String> {
    let t = raw.trim();
    if t.is_empty() {
        return None;
    }
    if let Some(lit) = parse_numeric_literal(t) {
        return Some(lit);
    }
    if is_ident(t) {
        return Some(t.to_string());
    }
    Some(remove_whitespace(t))
}

fn parse_coin_split_method(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let idx = line.find(".split(")?;
    let before = line[..idx].split('=').last()?.trim().trim_end_matches('.').trim();
    let base_var = parse_coin_target_expr(before, aliases)?;
    let args = extract_method_args(line, "split(")?;
    let amount = normalize_coin_amount_expr(args.first()?.as_str())?;
    Some((base_var, amount))
}

fn parse_coin_split_namespace(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::split(")?;
    let base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    let amount = normalize_coin_amount_expr(args.get(1)?.as_str())?;
    Some((base_var, amount))
}

fn parse_coin_join_method(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let idx = line.find(".join(")?;
    let before = line[..idx].split('=').last()?.trim().trim_end_matches('.').trim();
    let base_var = parse_coin_target_expr(before, aliases)?;
    let args = extract_method_args(line, "join(")?;
    let src_base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    Some((base_var, src_base_var))
}

fn parse_coin_join_namespace(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::join(")?;
    let base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    let src_base_var = parse_coin_target_expr(args.get(1)?.as_str(), aliases)?;
    Some((base_var, src_base_var))
}

fn parse_coin_transfer_arg(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let args = extract_namespace_args(line, "transfer::public_transfer(")
        .or_else(|| extract_namespace_args(line, "transfer::transfer("))?;
    parse_coin_target_expr(args.first()?.as_str(), aliases)
}

fn parse_coin_join_mint_method(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let idx = line.find(".join(")?;
    let before = line[..idx].split('=').last()?.trim().trim_end_matches('.').trim();
    let base_var = parse_coin_target_expr(before, aliases)?;
    let args = extract_method_args(line, "join(")?;
    let mint_amount = parse_mint_amount_from_expr(args.first()?.as_str())?;
    Some((base_var, mint_amount))
}

fn parse_coin_join_mint_namespace(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::join(")?;
    let base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    let mint_amount = parse_mint_amount_from_expr(args.get(1)?.as_str())?;
    Some((base_var, mint_amount))
}

fn parse_coin_burn_with_split(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::burn(")
        .or_else(|| extract_suffix_call_args(line, "::burn("))?;
    let burn_arg = args.get(1)?.as_str();
    parse_coin_split_expr(burn_arg, aliases)
}

fn parse_coin_split_expr(
    expr: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let t = expr.trim();
    if let Some((base, amount)) = parse_coin_split_method(t, aliases) {
        return Some((base, amount));
    }
    if let Some((base, amount)) = parse_coin_split_namespace(t, aliases) {
        return Some((base, amount));
    }
    None
}

fn parse_mint_amount_from_expr(expr: &str) -> Option<String> {
    let t = expr.trim();
    if t.contains("mint_for_testing(") {
        return None;
    }
    let args = extract_namespace_args(t, "coin::mint(")
        .or_else(|| extract_suffix_call_args(t, "::mint("))?;
    let raw = args.get(1)?.as_str();
    normalize_coin_amount_expr(raw)
}

fn extract_suffix_call_args(line: &str, suffix_call_prefix: &str) -> Option<Vec<String>> {
    let idx = line.find(suffix_call_prefix)?;
    let after = line[idx + suffix_call_prefix.len()..].trim();
    let args_raw = after.strip_suffix(')')?;
    Some(split_args(args_raw))
}

fn extract_option_effects_from_body(
    body_lines: &[String],
    params: &[ParamDecl],
    option_fields_by_type: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Vec<OptionEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    let mut param_option_fields: std::collections::HashMap<String, std::collections::HashSet<String>> =
        std::collections::HashMap::new();
    for p in params {
        let ty = normalize_param_object_type(&p.ty);
        let key = type_key_from_type_name(&ty);
        if let Some(fields) = option_fields_by_type.get(&key) {
            param_option_fields.insert(p.name.clone(), fields.clone());
        }
    }

    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        for borrow_target in extract_mut_borrow_targets(stmt) {
            let resolved = if let Some((base, field)) = parse_field_access(&borrow_target)
                .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
            {
                Some((base, field))
            } else if is_ident(&borrow_target) {
                parse_simple_base_field(&resolve_alias_path(&borrow_target, &aliases))
            } else {
                None
            };
            if let Some((base_var, field)) = resolved {
                if let Some(fields) = param_option_fields.get(&base_var) {
                    if fields.contains(&field) {
                        let sig = format!("{}::{}::{:?}", base_var, field, OptionOp::Changed);
                        if seen.insert(sig) {
                            out.push(OptionEffect {
                                base_var,
                                field,
                                op: OptionOp::Changed,
                            });
                        }
                    }
                }
            }
        }
        if let Some((name, target)) = parse_alias_binding(stmt) {
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
            }
        }
        let (lhs, rhs) = match stmt.split_once('=') {
            Some((l, r)) if !l.contains("==") && !r.contains("==") => (l.trim(), r.trim()),
            _ => continue,
        };
        if lhs.ends_with('!') || lhs.contains('[') || lhs.contains(']') {
            continue;
        }

        let (base_var, field) = match parse_field_access(lhs)
            .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
        {
            Some(parts) => parts,
            None => continue,
        };
        let fields = match param_option_fields.get(&base_var) {
            Some(v) => v,
            None => continue,
        };
        if !fields.contains(&field) {
            continue;
        }

        let op = parse_option_write_op(rhs);
        let sig = format!("{}::{}::{:?}", base_var, field, op);
        if seen.insert(sig) {
            out.push(OptionEffect {
                base_var,
                field,
                op,
            });
        }
    }

    out
}

fn parse_option_write_op(rhs: &str) -> OptionOp {
    let norm = remove_whitespace(rhs);
    if norm.contains("option::some(") {
        OptionOp::SetSome
    } else if norm.contains("option::none(") {
        OptionOp::SetNone
    } else {
        OptionOp::Changed
    }
}

fn extract_string_effects_from_body(
    body_lines: &[String],
    params: &[ParamDecl],
    string_fields_by_type: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Vec<StringEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    let mut param_string_fields: std::collections::HashMap<String, std::collections::HashSet<String>> =
        std::collections::HashMap::new();
    for p in params {
        let ty = normalize_param_object_type(&p.ty);
        let key = type_key_from_type_name(&ty);
        if let Some(fields) = string_fields_by_type.get(&key) {
            param_string_fields.insert(p.name.clone(), fields.clone());
        }
    }

    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        for borrow_target in extract_mut_borrow_targets(stmt) {
            let resolved = if let Some((base, field)) = parse_field_access(&borrow_target)
                .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
            {
                Some((base, field))
            } else if is_ident(&borrow_target) {
                parse_simple_base_field(&resolve_alias_path(&borrow_target, &aliases))
            } else {
                None
            };
            if let Some((base_var, field)) = resolved {
                if let Some(fields) = param_string_fields.get(&base_var) {
                    if fields.contains(&field) {
                        let sig = format!("{}.{}", base_var, field);
                        if seen.insert(sig) {
                            out.push(StringEffect { base_var, field });
                        }
                    }
                }
            }
        }
        if let Some((name, target)) = parse_alias_binding(stmt) {
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
            }
        }
        let (lhs, _rhs) = match stmt.split_once('=') {
            Some((l, r)) if !l.contains("==") && !r.contains("==") => (l.trim(), r.trim()),
            _ => continue,
        };
        if lhs.ends_with('!') || lhs.contains('[') || lhs.contains(']') {
            continue;
        }

        let (base_var, field) = match parse_field_access(lhs)
            .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
        {
            Some(parts) => parts,
            None => continue,
        };
        let fields = match param_string_fields.get(&base_var) {
            Some(v) => v,
            None => continue,
        };
        if !fields.contains(&field) {
            continue;
        }
        let sig = format!("{}.{}", base_var, field);
        if seen.insert(sig) {
            out.push(StringEffect { base_var, field });
        }
    }

    out
}

fn extract_mut_borrow_targets(stmt: &str) -> Vec<String> {
    let mut out = Vec::new();
    for part in stmt.split("&mut").skip(1) {
        let mut cand = part.trim();
        while let Some(rest) = cand.strip_prefix('(') {
            cand = rest.trim_start();
        }
        let end = cand
            .find(|c: char| c == ',' || c == ')' || c == ';' || c == '=')
            .unwrap_or(cand.len());
        let token = cand[..end]
            .trim()
            .trim_matches('(')
            .trim_matches(')')
            .trim();
        if !token.is_empty() {
            out.push(token.to_string());
        }
    }
    out
}

fn parse_vector_method_target(
    line: &str,
    method_prefix: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    // Example: counter.history.push_back(x)
    let idx = line.find(method_prefix)?;
    let lhs = line[..idx].trim_end_matches('.').trim();
    parse_vector_target_expr(lhs, aliases)
}

fn parse_vector_namespace_target(
    line: &str,
    call_prefix: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    // Example: vector::push_back(&mut counter.history, x)
    let args = extract_namespace_args(line, call_prefix)?;
    parse_vector_target_expr(args.first()?.as_str(), aliases)
}

fn parse_vector_target_expr(
    arg: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let first_arg = arg
        .trim()
        .trim_start_matches("&mut")
        .trim_start_matches('&')
        .trim();
    if let Some((base, field)) = parse_field_access(first_arg) {
        return resolve_effect_target(&base, &field, aliases);
    }
    if is_ident(first_arg) {
        let resolved = resolve_alias_path(first_arg, aliases);
        return parse_simple_base_field(&resolved);
    }
    None
}

fn parse_let_binding_name(stmt: &str) -> Option<String> {
    let rest = stmt.strip_prefix("let ")?.trim();
    let lhs = rest.split_once('=').map(|(l, _)| l).unwrap_or(rest).trim();
    let lhs = lhs.strip_prefix("mut ").unwrap_or(lhs).trim();
    let name = lhs.split(|c: char| c == ':' || c.is_whitespace()).next()?.trim();
    if is_ident(name) {
        Some(name.to_string())
    } else {
        None
    }
}

fn parse_alias_binding(stmt: &str) -> Option<(String, String)> {
    let name = parse_let_binding_name(stmt)?;
    let rhs = stmt.split_once('=')?.1.trim().trim_end_matches(';').trim();
    let rhs = rhs.trim().trim_matches('(').trim_matches(')').trim();
    let target = rhs
        .strip_prefix("&mut ")
        .or_else(|| rhs.strip_prefix('&'))
        .unwrap_or(rhs)
        .trim()
        .trim_matches('(')
        .trim_matches(')')
        .trim();
    if is_ident(target) || parse_simple_base_field(target).is_some() {
        Some((name, target.to_string()))
    } else {
        None
    }
}

fn resolve_alias_path(
    name: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> String {
    let mut cur = name.to_string();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    while is_ident(&cur) {
        if !seen.insert(cur.clone()) {
            break;
        }
        match aliases.get(&cur) {
            Some(next) => cur = next.clone(),
            None => break,
        }
    }
    cur
}

fn parse_simple_base_field(path: &str) -> Option<(String, String)> {
    let mut parts = path.split('.');
    let base = parts.next()?.trim();
    let field = parts.next()?.trim();
    if parts.next().is_some() {
        return None;
    }
    if is_ident(base) && is_ident(field) {
        Some((base.to_string(), field.to_string()))
    } else {
        None
    }
}

fn resolve_effect_target(
    base: &str,
    field: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let resolved = resolve_alias_path(base, aliases);
    if is_ident(&resolved) {
        return Some((resolved, field.to_string()));
    }
    if let Some((resolved_base, resolved_field)) = parse_simple_base_field(&resolved) {
        // If alias points to a field itself (e.g. alias -> counter.history), we only
        // support direct tracking of that field target.
        if field == resolved_field {
            return Some((resolved_base, resolved_field));
        }
    }
    None
}

fn extract_method_args(line: &str, method_prefix: &str) -> Option<Vec<String>> {
    let idx = line.find(method_prefix)?;
    let after = line[idx + method_prefix.len()..].trim();
    let args_raw = after.strip_suffix(')')?;
    Some(split_args(args_raw))
}

fn extract_namespace_args(line: &str, call_prefix: &str) -> Option<Vec<String>> {
    let after = line.split_once(call_prefix)?.1.trim();
    let args_raw = after.strip_suffix(')')?;
    Some(split_args(args_raw))
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
            if !(chars[start].is_ascii_alphanumeric() || chars[start] == '_' || chars[start] == ':')
            {
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
    if lit.chars().all(|c| c.is_ascii_digit() || c == '_')
        && lit.chars().any(|c| c.is_ascii_digit())
    {
        return Some(lit.to_string());
    }
    None
}

fn is_ident(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn is_string_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    norm == "String" || norm.ends_with("string::String")
}

fn is_coin_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    norm.contains("Coin<")
}

fn is_option_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    norm.starts_with("Option<") || norm.contains("::option::Option<")
}

fn option_none_expr_for_type(ty: &str) -> String {
    if let Some(inner) = extract_option_inner_type(ty) {
        format!("std::option::none<{}>()", inner)
    } else {
        "std::option::none()".to_string()
    }
}

fn extract_option_inner_type(ty: &str) -> Option<String> {
    let start = ty.find('<')?;
    let mut depth = 0i32;
    let mut end_idx: Option<usize> = None;
    for (i, ch) in ty.char_indices().skip(start) {
        if ch == '<' {
            depth += 1;
        } else if ch == '>' {
            depth -= 1;
            if depth == 0 {
                end_idx = Some(i);
                break;
            }
        }
    }
    let end = end_idx?;
    let inner = ty[start + 1..end].trim();
    if inner.is_empty() {
        None
    } else {
        Some(inner.to_string())
    }
}

fn sanitize_ident(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect::<String>()
}

fn is_numeric_type(ty: &str) -> bool {
    matches!(ty.trim(), "u8" | "u16" | "u32" | "u64" | "u128" | "u256")
}

fn normalize_param_object_type(ty: &str) -> String {
    ty.replace("&mut", "").replace('&', "").trim().to_string()
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
