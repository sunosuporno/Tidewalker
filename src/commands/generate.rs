//! Test generation logic.
use crate::commands::setup::run_generate_setup;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

mod catalog;
mod chain;
mod guards;
mod parser;
mod render;

use parser::*;

pub fn run_generate(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Always do setup first (same behavior as `generate-setup`).
    run_generate_setup(package_path)?;
    generate_tests(package_path)
}

#[derive(Debug, Clone)]
struct FnDecl {
    module_name: String,
    fn_name: String,
    type_params: Vec<String>,
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
    treasury_cap_effects: Vec<TreasuryCapEffect>,
    coin_notes: Vec<CoinNote>,
    option_effects: Vec<OptionEffect>,
    string_effects: Vec<StringEffect>,
    container_effects: Vec<ContainerEffect>,
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
struct TreasuryCapEffect {
    base_var: String,
    op: TreasuryCapOp,
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
struct ContainerEffect {
    base_var: String,
    field: String,
    kind: ContainerKind,
    op: ContainerOp,
}

#[derive(Debug, Clone)]
struct ContainerAccessorSig {
    fn_name: String,
    param_ty: String,
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

#[derive(Debug, Clone)]
enum TreasuryCapOp {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ContainerKind {
    Table,
    VecMap,
    VecSet,
    Bag,
    DynamicField,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ContainerOp {
    Insert(String),
    Remove(String),
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
        if p.extension().is_some_and(|ext| ext == "move") {
            move_files.push(p);
        }
    }
    move_files.sort();

    let mut decls: Vec<FnDecl> = Vec::new();
    let mut module_sources: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut key_structs_by_module: std::collections::HashMap<
        String,
        std::collections::HashSet<String>,
    > = std::collections::HashMap::new();
    for file in &move_files {
        let content = fs::read_to_string(file)?;
        decls.extend(extract_public_fns(&content));
        if let Some(module_name) = content.lines().find_map(|line| {
            let t = line.trim();
            if t.starts_with("module ") {
                let rest = t.strip_prefix("module ")?;
                let until = rest.split_whitespace().next().unwrap_or(rest);
                Some(until.trim_end_matches(';').to_string())
            } else {
                None
            }
        }) {
            key_structs_by_module.insert(module_name.clone(), extract_key_struct_types(&content));
            module_sources.insert(module_name, content);
        }
    }

    let mut skipped: Vec<(String, String, String)> = Vec::new(); // (module, fn, reason)
    let mut unverified_abort_paths: Vec<String> = Vec::new();
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
    out.push("    // Tidewalker generated assertion code map:".to_string());
    out.push(
        "    // `i` = 0-based assertion index for that category within a single generated test function."
            .to_string(),
    );
    out.push("    // Example: first coin assert = 990, second coin assert = 991.".to_string());
    out.push("    // - numeric checks: 900 + i".to_string());
    out.push("    // - container checks: 940 + i".to_string());
    out.push("    // - vector checks: 960 + i".to_string());
    out.push("    // - option checks: 980 + i".to_string());
    out.push("    // - coin checks: 990 + i".to_string());
    out.push("    // - treasury supply checks: 995 + i".to_string());
    out.push("".to_string());

    let accessor_map = catalog::build_accessor_map(&decls);
    let option_accessor_map = catalog::build_option_accessor_map(&decls);
    let container_accessor_map = catalog::build_container_accessor_map(&decls);
    let helper_catalog = catalog::build_helper_catalog(&decls);
    let bootstrap_catalog = catalog::build_bootstrap_catalog(&module_sources);
    let fn_lookup = catalog::build_fn_lookup(&decls);
    let effects_map = chain::build_chained_effect_map(&decls, 4);
    let vector_effects_map = chain::build_chained_vector_effect_map(&decls, 4);
    let coin_effects_map = chain::build_chained_coin_effect_map(&decls, 4);
    let treasury_cap_effects_map = chain::build_chained_treasury_cap_effect_map(&decls, 4);
    let coin_notes_map = chain::build_chained_coin_note_map(&decls, 4);
    let option_effects_map = chain::build_chained_option_effect_map(&decls, 4);
    let string_effects_map = chain::build_chained_string_effect_map(&decls, 4);
    let container_effects_map = chain::build_chained_container_effect_map(&decls, 4);
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
        let resolved_treasury_cap_effects = treasury_cap_effects_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.treasury_cap_effects.clone());
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
        let resolved_container_effects = container_effects_map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| d.container_effects.clone());
        let deep_overflow_paths = deep_overflow_map.get(&key).cloned().unwrap_or_default();
        let render_inputs = render::RenderInputs {
            accessor_map: &accessor_map,
            option_accessor_map: &option_accessor_map,
            container_accessor_map: &container_accessor_map,
            bootstrap_catalog: &bootstrap_catalog,
            fn_lookup: &fn_lookup,
            key_structs_by_module: &key_structs_by_module,
            numeric_effects: &resolved_effects,
            vector_effects: &resolved_vector_effects,
            coin_effects: &resolved_coin_effects,
            treasury_cap_effects: &resolved_treasury_cap_effects,
            coin_notes: &resolved_coin_notes,
            option_effects: &resolved_option_effects,
            string_effects: &resolved_string_effects,
            container_effects: &resolved_container_effects,
            deep_overflow_paths: &deep_overflow_paths,
        };
        if let Some(test_lines) = render::render_best_effort_test(&d, &render_inputs) {
            for l in test_lines {
                out.push(format!("    {}", l));
            }
            out.push("".to_string());
            let (guard_tests, guard_unverified) = guards::render_guard_tests_for_function(
                &d,
                &accessor_map,
                &helper_catalog,
                &bootstrap_catalog,
                &fn_lookup,
                &key_structs_by_module,
            );
            for test in guard_tests {
                for l in test {
                    out.push(format!("    {}", l));
                }
                out.push("".to_string());
            }
            unverified_abort_paths.extend(guard_unverified);
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
    if !unverified_abort_paths.is_empty() {
        unverified_abort_paths.sort();
        unverified_abort_paths.dedup();
        out.push("    // ---- Unverified abort/assert paths ----".to_string());
        out.push(
            "    // Tidewalker could not synthesize safe expected-failure tests for:".to_string(),
        );
        for note in &unverified_abort_paths {
            out.push(format!("    // {}", note));
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

#[derive(Debug, Clone)]
struct ObjectNeed {
    type_name: String,
    type_key: String,
    var_name: String,
    is_mut: bool,
    is_ref: bool,
}

impl ObjectNeed {
    fn from_resolved(param: &ParamDecl, type_name: String) -> Self {
        let raw_ty = param.ty.trim();
        let type_key = type_key_from_type_name(&type_name);
        Self {
            type_name,
            type_key,
            var_name: format!("obj_{}", sanitize_ident(&param.name)),
            is_mut: raw_ty.starts_with("&mut"),
            is_ref: raw_ty.starts_with('&'),
        }
    }
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

fn is_bool_literal(raw: &str) -> bool {
    matches!(raw.trim(), "true" | "false")
}

fn is_address_literal(raw: &str) -> bool {
    let t = raw.trim();
    t.starts_with('@') && t.len() > 1
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

fn is_treasury_cap_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    norm.contains("TreasuryCap<")
}

fn is_cap_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    if norm.is_empty() {
        return false;
    }
    if is_treasury_cap_type(&norm) {
        return true;
    }
    let base = norm.split('<').next().unwrap_or(&norm);
    base.ends_with("Cap") || base.ends_with("::Cap")
}

fn is_option_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    norm.starts_with("Option<") || norm.contains("::option::Option<")
}

fn is_id_type(ty: &str) -> bool {
    let norm = normalize_param_object_type(ty).replace(' ', "");
    norm == "ID" || norm.ends_with("::ID")
}

fn is_vector_type(ty: &str) -> bool {
    let norm = normalize_param_object_type(ty).replace(' ', "");
    norm.starts_with("vector<")
}

fn vector_literal_expr_for_type(ty: &str) -> Option<String> {
    let norm = normalize_param_object_type(ty).replace(' ', "");
    let inner = extract_vector_inner_type(&norm)?;
    let inner_norm = inner.replace(' ', "");
    if inner_norm != "u8" {
        return None;
    }
    let elems = std::iter::repeat("0u8")
        .take(32)
        .collect::<Vec<_>>()
        .join(", ");
    Some(format!("vector[{}]", elems))
}

fn extract_vector_inner_type(ty: &str) -> Option<String> {
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

fn default_id_arg_expr() -> String {
    "sui::object::id_from_address(OTHER)".to_string()
}

fn default_u64_arg_for_param(name: &str) -> String {
    let lower = name.to_ascii_lowercase();
    if lower.contains("ratio") {
        "101".to_string()
    } else {
        "1".to_string()
    }
}

fn normalize_helper_key(key: &str) -> String {
    key.chars()
        .filter(|c| *c != '_')
        .collect::<String>()
        .to_ascii_lowercase()
}

fn split_type_module_and_base<'a>(ty: &'a str, default_module: &'a str) -> (&'a str, &'a str) {
    let clean = ty.trim();
    let no_ref = clean
        .strip_prefix("&mut ")
        .or_else(|| clean.strip_prefix('&'))
        .unwrap_or(clean);
    let no_generics = no_ref.split('<').next().unwrap_or(no_ref).trim();
    if let Some((module, base)) = no_generics.rsplit_once("::") {
        (module, base.trim())
    } else {
        (default_module, no_generics)
    }
}

fn is_known_key_struct(
    ty: &str,
    default_module: &str,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> bool {
    let (module, base) = split_type_module_and_base(ty, default_module);
    key_structs_by_module
        .get(module)
        .map(|types| types.contains(base))
        .unwrap_or(false)
}

fn should_transfer_call_return_basic(ret_ty: &str, _module_name: &str) -> bool {
    let t = ret_ty.trim();
    if t.is_empty() || t == "()" || t.starts_with('&') {
        return false;
    }
    if is_numeric_type(t)
        || t == "bool"
        || t == "address"
        || is_string_type(t)
        || is_option_type(t)
        || t.starts_with("vector<")
    {
        return false;
    }
    is_coin_type(t) || is_treasury_cap_type(t)
}

fn should_transfer_call_return_with_keys(
    ret_ty: &str,
    module_name: &str,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> bool {
    if should_transfer_call_return_basic(ret_ty, module_name) {
        return true;
    }
    is_known_key_struct(ret_ty, module_name, key_structs_by_module)
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

fn type_mentions_param(ty: &str, param: &str) -> bool {
    let mut token = String::new();
    for ch in ty.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            token.push(ch);
            continue;
        }
        if token == param {
            return true;
        }
        token.clear();
    }
    token == param
}

fn has_unbound_type_params(d: &FnDecl) -> bool {
    if d.type_params.is_empty() {
        return false;
    }
    d.type_params
        .iter()
        .any(|tp| !d.params.iter().any(|p| type_mentions_param(&p.ty, tp)))
}

fn default_type_args_for_params(type_params: &[String]) -> Vec<String> {
    if type_params.is_empty() {
        return Vec::new();
    }
    type_params
        .iter()
        .map(|_| "0x2::sui::SUI".to_string())
        .collect::<Vec<_>>()
}

fn concretize_type_params(ty: &str, type_params: &[String], concrete_args: &[String]) -> String {
    if type_params.is_empty() || concrete_args.is_empty() {
        return ty.to_string();
    }
    let mut out = String::new();
    let mut token = String::new();
    let flush_token = |out: &mut String, token: &mut String| {
        if token.is_empty() {
            return;
        }
        if let Some(idx) = type_params.iter().position(|tp| tp == token) {
            if let Some(arg) = concrete_args.get(idx) {
                out.push_str(arg);
            } else {
                out.push_str(token);
            }
        } else {
            out.push_str(token);
        }
        token.clear();
    };
    for ch in ty.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            token.push(ch);
        } else {
            flush_token(&mut out, &mut token);
            out.push(ch);
        }
    }
    flush_token(&mut out, &mut token);
    out
}

fn qualify_type_for_module(module_name: &str, type_name: &str) -> String {
    let clean = type_name.trim();
    let base = clean.split('<').next().unwrap_or(clean).trim();
    if base.contains("::") {
        clean.to_string()
    } else {
        format!("{}::{}", module_name, clean)
    }
}

fn is_constructor_candidate_name(name: &str) -> bool {
    name.starts_with("new")
        || name.starts_with("create")
        || name.starts_with("from_")
        || name == "from_u64"
}

fn constructor_name_score(name: &str) -> usize {
    if name == "new" || name.starts_with("new_") {
        0
    } else if name == "from_u64" || name.starts_with("from_") {
        1
    } else if name == "create" || name.starts_with("create_") {
        2
    } else {
        3
    }
}

fn is_type_return_match(target_ty: &str, ret_ty: &str, current_module: &str) -> bool {
    let target = normalize_param_object_type(target_ty).replace(' ', "");
    let ret = normalize_param_object_type(ret_ty).replace(' ', "");
    if target.is_empty() || ret.is_empty() {
        return false;
    }
    if target == ret {
        return true;
    }
    let target_base = target
        .split('<')
        .next()
        .unwrap_or(&target)
        .rsplit("::")
        .next()
        .unwrap_or(&target);
    let ret_base = ret
        .split('<')
        .next()
        .unwrap_or(&ret)
        .rsplit("::")
        .next()
        .unwrap_or(&ret);
    if target_base != ret_base {
        return false;
    }
    if !target.contains("::") {
        return ret == format!("{}::{}", current_module, target) || ret_base == target;
    }
    let Some((target_mod, _)) = target.rsplit_once("::") else {
        return false;
    };
    let Some((ret_mod, _)) = ret.rsplit_once("::") else {
        return false;
    };
    if target_mod == ret_mod {
        return true;
    }
    if !target_mod.contains("::") && ret_mod.ends_with(&format!("::{}", target_mod)) {
        return true;
    }
    false
}

fn constructor_candidate_modules(
    target_ty: &str,
    current_module: &str,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
) -> Vec<String> {
    let mut out = Vec::new();
    let target = normalize_param_object_type(target_ty).replace(' ', "");
    if target.contains("::") {
        if let Some((mod_hint, _)) = target.rsplit_once("::") {
            if fn_lookup.contains_key(mod_hint) {
                out.push(mod_hint.to_string());
            } else if !mod_hint.contains("::") {
                for module_name in fn_lookup.keys() {
                    if module_name == mod_hint || module_name.ends_with(&format!("::{}", mod_hint))
                    {
                        out.push(module_name.clone());
                    }
                }
            }
        }
    } else if fn_lookup.contains_key(current_module) {
        out.push(current_module.to_string());
    }
    if out.is_empty() {
        out.extend(fn_lookup.keys().cloned());
    }
    out.sort();
    out.dedup();
    out
}

fn synthesize_value_expr_for_type(
    target_ty: &str,
    current_module: &str,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    skip_fq: Option<&str>,
    depth: usize,
) -> Option<String> {
    if depth > 3 {
        return None;
    }
    let t = normalize_param_object_type(target_ty).trim().to_string();
    if t.is_empty() {
        return None;
    }
    if t == "u64" || is_numeric_type(&t) {
        return Some("1".to_string());
    }
    if t == "bool" {
        return Some("false".to_string());
    }
    if t == "address" {
        return Some("SUPER_USER".to_string());
    }
    if is_id_type(&t) {
        return Some("sui::object::id_from_address(OTHER)".to_string());
    }
    if is_option_type(&t) {
        return Some(option_none_expr_for_type(&t));
    }
    if is_string_type(&t) {
        return Some("std::string::utf8(b\"tidewalker\")".to_string());
    }
    if is_vector_type(&t) {
        return vector_literal_expr_for_type(&t);
    }
    if is_coin_type(&t) {
        return Some(
            "coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario))"
                .to_string(),
        );
    }

    let candidate_modules = constructor_candidate_modules(&t, current_module, fn_lookup);
    let mut best: Option<(usize, usize, String)> = None;
    for module_name in candidate_modules {
        let Some(module_fns) = fn_lookup.get(&module_name) else {
            continue;
        };
        for f in module_fns.values() {
            if f.fn_name == "init" {
                continue;
            }
            if let Some(skip) = skip_fq {
                let fq = format!("{}::{}", f.module_name, f.fn_name);
                if fq == skip {
                    continue;
                }
            }
            if !(f.is_public || f.is_entry || f.is_test_only) {
                continue;
            }
            if !is_constructor_candidate_name(&f.fn_name) {
                continue;
            }
            let ret = match &f.return_ty {
                Some(v) => v,
                None => continue,
            };
            if !is_type_return_match(&t, ret, current_module) {
                continue;
            }

            let mut args = Vec::new();
            let mut ok = true;
            for p in &f.params {
                let pty = p.ty.trim();
                if pty.contains("TxContext") {
                    args.push("test_scenario::ctx(&mut scenario)".to_string());
                    continue;
                }
                if pty.starts_with('&') {
                    ok = false;
                    break;
                }
                let arg_expr = match synthesize_value_expr_for_type(
                    pty,
                    &f.module_name,
                    fn_lookup,
                    Some(&format!("{}::{}", f.module_name, f.fn_name)),
                    depth + 1,
                ) {
                    Some(v) => v,
                    None => {
                        ok = false;
                        break;
                    }
                };
                args.push(arg_expr);
            }
            if !ok {
                continue;
            }
            let type_args = if has_unbound_type_params(f) {
                default_type_args_for_params(&f.type_params)
            } else {
                Vec::new()
            };
            let call_path = if type_args.is_empty() {
                format!("{}::{}", f.module_name, f.fn_name)
            } else {
                format!("{}::{}<{}>", f.module_name, f.fn_name, type_args.join(", "))
            };
            let expr = format!("{}({})", call_path, args.join(", "));
            let score = (f.params.len(), constructor_name_score(&f.fn_name));
            if best.as_ref().map(|b| score < (b.0, b.1)).unwrap_or(true) {
                best = Some((score.0, score.1, expr));
            }
        }
    }
    best.map(|(_, _, expr)| expr)
}

fn extract_key_struct_types(content: &str) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    for line in content.lines() {
        let t = line.trim();
        let after = if let Some(rest) = t.strip_prefix("public struct ") {
            rest
        } else if let Some(rest) = t.strip_prefix("struct ") {
            rest
        } else {
            continue;
        };
        let Some((name_part, ability_part_raw)) = after.split_once(" has ") else {
            continue;
        };
        let name = name_part
            .trim()
            .split('<')
            .next()
            .unwrap_or("")
            .trim()
            .to_string();
        if name.is_empty() {
            continue;
        }
        let ability_part = ability_part_raw
            .split('{')
            .next()
            .unwrap_or(ability_part_raw)
            .trim();
        let has_key = ability_part
            .split(',')
            .map(|a| a.trim())
            .any(|a| a == "key");
        if has_key {
            out.insert(name);
        }
    }
    out
}
