//! Setup helper generation and source injection.
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Default)]
struct ModuleSetupInfo {
    module_name: String,
    shared_types: Vec<(String, String)>,
    cap_types: Vec<(String, String)>,
    struct_field_types: std::collections::HashMap<String, Vec<(String, String)>>,
    init_field_values_by_type:
        std::collections::HashMap<String, std::collections::HashMap<String, String>>,
    cannot_generate: Vec<(String, String)>,
    /// (type_name, function_name) for share_object in non-init functions
    non_init_shared: Vec<(String, String)>,
    /// (type_name, function_name) for transfer(..., sender) in non-init functions
    non_init_caps: Vec<(String, String)>,
    /// If init takes a one-time witness (e.g. REGISTRY), we don't generate for init-created types
    init_one_time_witness: Option<String>,
}

fn normalize_setup_type(ty: &str) -> String {
    ty.replace("&mut", "").replace('&', "").trim().to_string()
}

fn is_treasury_cap_type(ty: &str) -> bool {
    ty.trim().replace(' ', "").contains("TreasuryCap<")
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

fn split_params_top_level(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut angle = 0i32;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    for ch in s.chars() {
        match ch {
            '<' => angle += 1,
            '>' => angle -= 1,
            '(' => paren += 1,
            ')' => paren -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            ',' if angle == 0 && paren == 0 && bracket == 0 && brace == 0 => {
                let t = cur.trim();
                if !t.is_empty() {
                    out.push(t.to_string());
                }
                cur.clear();
                continue;
            }
            _ => {}
        }
        cur.push(ch);
    }
    let tail = cur.trim();
    if !tail.is_empty() {
        out.push(tail.to_string());
    }
    out
}

fn find_function_headers(lines: &[&str]) -> Vec<String> {
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < lines.len() {
        let t = lines[i].trim();
        let is_fun = t.starts_with("public entry fun ")
            || t.starts_with("entry fun ")
            || t.starts_with("public fun ")
            || t.starts_with("public(package) fun ")
            || t.starts_with("fun ");
        if !is_fun {
            i += 1;
            continue;
        }
        let mut header = t.to_string();
        let mut j = i + 1;
        while j < lines.len() && !header.contains(')') {
            header.push(' ');
            header.push_str(lines[j].trim());
            j += 1;
        }
        out.push(header);
        i = j;
    }
    out
}

fn extract_param_types_from_header(header: &str) -> Vec<(bool, String)> {
    let params_src = header
        .split_once('(')
        .and_then(|(_, rest)| rest.split_once(')'))
        .map(|(p, _)| p)
        .unwrap_or("");
    let parts = split_params_top_level(params_src);
    let mut out = Vec::new();
    for p in parts {
        let Some((_name, ty)) = p.split_once(':') else {
            continue;
        };
        let ty_raw = ty.trim();
        let by_ref = ty_raw.starts_with('&');
        let ty_norm = normalize_setup_type(ty_raw);
        if ty_norm.is_empty() {
            continue;
        }
        out.push((by_ref, ty_norm));
    }
    out
}

fn is_container_field_type(ty: &str) -> bool {
    let t = ty.trim().replace(' ', "");
    t.contains("::table::Table<")
        || t.contains("::vec_map::VecMap<")
        || t.contains("::vec_set::VecSet<")
        || t.contains("::bag::Bag")
}

fn extract_treasury_cap_inner_type(ty: &str) -> Option<String> {
    let start = ty.find('<')?;
    let end = ty.rfind('>')?;
    if end <= start + 1 {
        return None;
    }
    let inner = ty[start + 1..end].trim();
    if inner.is_empty() {
        None
    } else {
        Some(inner.to_string())
    }
}

fn dedup_type_pairs(v: &mut Vec<(String, String)>) {
    let mut seen: std::collections::BTreeSet<(String, String)> = std::collections::BTreeSet::new();
    v.retain(|item| seen.insert(item.clone()));
}

pub fn run_generate_setup(package_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
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

    let mut all_shared: Vec<(String, String)> = Vec::new();
    let mut all_caps: Vec<(String, String)> = Vec::new();
    let mut struct_fields_by_module: std::collections::HashMap<
        String,
        std::collections::HashMap<String, Vec<(String, String)>>,
    > = std::collections::HashMap::new();
    let mut init_field_values_by_module: std::collections::HashMap<
        String,
        std::collections::HashMap<String, std::collections::HashMap<String, String>>,
    > = std::collections::HashMap::new();
    let mut init_one_time_witness_by_module: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut alerts: Vec<(String, String)> = Vec::new();
    let mut file_for_module: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();

    for file in &move_files {
        let file_name = file
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        match extract_setup_info(file) {
            Ok(info) => {
                file_for_module.insert(info.module_name.clone(), file_name.clone());
                struct_fields_by_module
                    .insert(info.module_name.clone(), info.struct_field_types.clone());
                init_field_values_by_module.insert(
                    info.module_name.clone(),
                    info.init_field_values_by_type.clone(),
                );
                if let Some(w) = &info.init_one_time_witness {
                    init_one_time_witness_by_module.insert(info.module_name.clone(), w.clone());
                }
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
    let mut shared_by_module: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    let mut caps_by_module: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    for (mod_name, type_name) in &all_shared {
        shared_by_module
            .entry(mod_name.clone())
            .or_default()
            .push(type_name.clone());
    }
    for (mod_name, type_name) in &all_caps {
        caps_by_module
            .entry(mod_name.clone())
            .or_default()
            .push(type_name.clone());
    }

    let sources_dir = path.join("sources");
    for (mod_name, file_name) in &file_for_module {
        let shared = shared_by_module
            .get(mod_name)
            .map(Vec::as_slice)
            .unwrap_or(&[]);
        let caps = caps_by_module
            .get(mod_name)
            .map(Vec::as_slice)
            .unwrap_or(&[]);
        let source_path = sources_dir.join(file_name);
        if !source_path.is_file() {
            alerts.push((
                file_name.clone(),
                "Source file not found in package sources/ (only build output was analyzed)."
                    .to_string(),
            ));
            continue;
        }
        let empty_fields: std::collections::HashMap<String, Vec<(String, String)>> =
            std::collections::HashMap::new();
        let struct_fields = struct_fields_by_module
            .get(mod_name)
            .unwrap_or(&empty_fields);
        let empty_init_values: std::collections::HashMap<
            String,
            std::collections::HashMap<String, String>,
        > = std::collections::HashMap::new();
        let init_field_values = init_field_values_by_module
            .get(mod_name)
            .unwrap_or(&empty_init_values);
        let init_one_time_witness = init_one_time_witness_by_module.get(mod_name).cloned();
        if shared.is_empty() && caps.is_empty() && init_one_time_witness.is_none() {
            continue;
        }
        match inject_test_only_helpers(
            &source_path,
            mod_name,
            shared,
            caps,
            struct_fields,
            init_field_values,
            init_one_time_witness,
        ) {
            Ok(()) => println!("Injected test_only helpers into {}", source_path.display()),
            Err(e) => alerts.push((
                file_name.clone(),
                format!("Failed to inject helpers: {}", e),
            )),
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

    let mut demanded_ref_public_structs: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    let mut demanded_treasury_caps: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    for header in find_function_headers(&lines) {
        for (by_ref, ty) in extract_param_types_from_header(&header) {
            if ty.contains("TxContext") {
                continue;
            }
            if ty.replace(' ', "").contains("Coin<") {
                continue;
            }
            if is_treasury_cap_type(&ty) {
                demanded_treasury_caps.insert(ty);
                continue;
            }
            if by_ref && public_structs.contains(&ty) {
                demanded_ref_public_structs.insert(ty);
            }
        }
    }

    let file_name = path
        .file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();
    info.init_one_time_witness = get_init_one_time_witness(&lines);

    let init_body = find_init_body(&lines);
    let init_complex_trigger = init_body
        .as_ref()
        .and_then(|body| first_disallowed_call_in_body(body));
    let (shared_vars, cap_vars, init_struct_field_values_by_var) = if let Some(body) = init_body {
        parse_init_body(body)
    } else {
        (
            Vec::new(),
            Vec::new(),
            std::collections::HashMap::<String, std::collections::HashMap<String, String>>::new(),
        )
    };
    for (var_name, type_name) in shared_vars.iter().chain(cap_vars.iter()) {
        if let Some(field_values) = init_struct_field_values_by_var.get(var_name) {
            info.init_field_values_by_type
                .entry(type_name.clone())
                .or_insert_with(|| field_values.clone());
        }
    }

    // Generic helper synthesis for TreasuryCap params used in module APIs.
    for cap_ty in demanded_treasury_caps {
        info.cap_types.push((info.module_name.clone(), cap_ty));
    }

    // Generic helper synthesis for container-backed public structs used by ref.
    for struct_ty in demanded_ref_public_structs {
        let Some(fields) = info.struct_field_types.get(&struct_ty) else {
            continue;
        };
        if fields
            .iter()
            .any(|(_, field_ty)| is_container_field_type(field_ty))
        {
            info.shared_types
                .push((info.module_name.clone(), struct_ty));
        }
    }

    let skip_init_generation =
        info.init_one_time_witness.is_some() || init_complex_trigger.is_some();
    if skip_init_generation {
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
                info.shared_types
                    .push((info.module_name.clone(), type_name.clone()));
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
                info.cap_types
                    .push((info.module_name.clone(), type_name.clone()));
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
        let (shared, caps, _) = parse_init_body(body);
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

    dedup_type_pairs(&mut info.shared_types);
    dedup_type_pairs(&mut info.cap_types);

    Ok(info)
}

/// Find all function definitions and return (function_name, body_lines). Includes init.
fn find_all_function_bodies<'a>(lines: &'a [&'a str]) -> Vec<(String, Vec<&'a str>)> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim_start();
        let (is_fun, name) = if let Some(after) = t.strip_prefix("public entry fun ") {
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if let Some(after) = t.strip_prefix("entry fun ") {
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if let Some(after) = t.strip_prefix("public fun ") {
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if let Some(after) = t.strip_prefix("public(package) fun ") {
            let name = after
                .split(|c: char| c == '(' || c == '<' || c.is_whitespace())
                .next()
                .unwrap_or("")
                .to_string();
            (true, name)
        } else if let Some(after) = t.strip_prefix("fun ") {
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
                return Some(
                    first_param
                        .strip_prefix("_: ")
                        .unwrap_or(first_param)
                        .trim()
                        .to_string(),
                );
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

fn parse_init_body(
    body: Vec<&str>,
) -> (
    Vec<(String, String)>,
    Vec<(String, String)>,
    std::collections::HashMap<String, std::collections::HashMap<String, String>>,
) {
    let mut var_to_type: std::collections::BTreeMap<String, String> =
        std::collections::BTreeMap::new();
    for line in &body {
        let t = line.trim();
        if t.starts_with("let ") {
            if let Some(rest) = t.strip_prefix("let ") {
                let lhs = rest.split('=').next().unwrap_or("").trim();
                let mut lhs_tokens = lhs.split_whitespace();
                let first = lhs_tokens.next().unwrap_or("");
                let mut name = if first == "mut" {
                    lhs_tokens.next().unwrap_or("").to_string()
                } else {
                    first.to_string()
                };
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
    let struct_field_values_by_var = parse_struct_literal_fields_from_body(&body);
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
    (shared, caps, struct_field_values_by_var)
}

fn parse_struct_literal_fields_from_body(
    body: &[&str],
) -> std::collections::HashMap<String, std::collections::HashMap<String, String>> {
    let mut out: std::collections::HashMap<String, std::collections::HashMap<String, String>> =
        std::collections::HashMap::new();
    let mut i = 0usize;
    while i < body.len() {
        let t = strip_line_comment(body[i]).trim().to_string();
        if !t.starts_with("let ") || !t.contains('=') || !t.contains('{') {
            i += 1;
            continue;
        }
        let after_let = t.trim_start_matches("let ").trim();
        let lhs = after_let.split('=').next().unwrap_or("").trim();
        let mut lhs_tokens = lhs.split_whitespace();
        let first = lhs_tokens.next().unwrap_or("");
        let var_name = if first == "mut" {
            lhs_tokens.next().unwrap_or("").to_string()
        } else {
            first.to_string()
        };
        if var_name.is_empty() {
            i += 1;
            continue;
        }
        let mut depth: i32 = 0;
        for ch in t.chars() {
            if ch == '{' {
                depth += 1;
            } else if ch == '}' {
                depth -= 1;
            }
        }
        if depth <= 0 {
            i += 1;
            continue;
        }
        let mut fields: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        i += 1;
        while i < body.len() && depth > 0 {
            let line_clean = strip_line_comment(body[i]).trim().to_string();
            if depth == 1 {
                if let Some((field_name, expr)) = parse_struct_field_value_line(&line_clean) {
                    if is_safe_helper_initializer_expr(&expr) {
                        fields.entry(field_name).or_insert(expr);
                    }
                }
            }
            for ch in line_clean.chars() {
                if ch == '{' {
                    depth += 1;
                } else if ch == '}' {
                    depth -= 1;
                }
            }
            i += 1;
        }
        if !fields.is_empty() {
            out.entry(var_name).or_insert(fields);
        }
    }
    out
}

fn strip_line_comment(line: &str) -> &str {
    line.split("//").next().unwrap_or("")
}

fn parse_struct_field_value_line(line: &str) -> Option<(String, String)> {
    let t = line.trim().trim_end_matches(',').trim();
    if t.is_empty() || t.starts_with('}') || t.starts_with('{') {
        return None;
    }
    let (field_name, expr) = t.split_once(':')?;
    let field = field_name.trim();
    let value = expr.trim();
    if field.is_empty() || value.is_empty() {
        return None;
    }
    Some((field.to_string(), value.to_string()))
}

fn is_safe_helper_initializer_expr(expr: &str) -> bool {
    let e = expr.trim();
    if e.is_empty() {
        return false;
    }
    if e == "true" || e == "false" {
        return true;
    }
    if e.starts_with('@') || e.starts_with("b\"") || e.starts_with("x\"") {
        return true;
    }
    if e.starts_with("vector[") {
        return true;
    }
    if e.chars().all(|c| c.is_ascii_digit() || c == '_') {
        return true;
    }
    e.contains("::")
}

fn extract_first_arg(line: &str, prefix: &str) -> Option<String> {
    let t = line.trim();
    let after = t.find(prefix).map(|i| &t[i + prefix.len()..])?;
    let arg = after
        .trim_start()
        .split([',', ')'])
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
        "string::utf8",
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
            if !(chars[start].is_ascii_alphanumeric() || chars[start] == '_' || chars[start] == ':')
            {
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
    let title_line = format!(
        "// {}{}{}{}/",
        "/",
        " ".repeat(pad),
        title,
        " ".repeat(pad_end)
    );
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
    init_field_values: &std::collections::HashMap<
        String,
        std::collections::HashMap<String, String>,
    >,
    init_one_time_witness: Option<String>,
    has_existing_bootstrap: bool,
) -> Vec<String> {
    let mut body = Vec::new();
    if let Some(witness_type) = init_one_time_witness {
        if !has_existing_bootstrap {
            let witness_type = witness_type.trim().to_string();
            body.push("#[test_only]".to_string());
            body.push("/// Bootstrap one-time-witness init state for generated tests.".to_string());
            body.push(
                "public fun bootstrap_init_for_testing(ctx: &mut sui::tx_context::TxContext) {"
                    .to_string(),
            );
            body.push(format!(
                "    let witness = sui::test_utils::create_one_time_witness<{}>();",
                witness_type
            ));
            body.push("    init(witness, ctx);".to_string());
            body.push("}".to_string());
            body.push("".to_string());
        }
    }
    for type_name in shared {
        let helper_key = type_key_from_type_name(type_name);
        body.push("#[test_only]".to_string());
        body.push(format!(
            "/// Create and share {} for testing (replicating init).",
            type_name
        ));
        body.push(format!(
            "public fun create_and_share_{}_for_testing(ctx: &mut sui::tx_context::TxContext) {{",
            helper_key
        ));
        body.push(format!("    let obj = {} {{", type_name));
        if let Some(fields) = struct_fields.get(type_name) {
            for (field_name, field_ty) in fields {
                let init = init_field_values
                    .get(type_name)
                    .and_then(|vals| vals.get(field_name))
                    .cloned()
                    .unwrap_or_else(|| default_field_initializer(field_name, field_ty));
                body.push(format!("        {}: {},", field_name, init));
            }
        } else {
            body.push("        id: sui::object::new(ctx),".to_string());
        }
        body.push("    };".to_string());
        body.push("    transfer::public_share_object(obj);".to_string());
        body.push("}".to_string());
        body.push("".to_string());
    }
    for type_name in caps {
        let helper_key = type_key_from_type_name(type_name);
        body.push("#[test_only]".to_string());
        body.push(format!(
            "/// Create {} for testing (caller receives it).",
            type_name
        ));
        body.push(format!(
            "public fun create_{}_for_testing(ctx: &mut sui::tx_context::TxContext): {} {{",
            helper_key, type_name
        ));
        if is_treasury_cap_type(type_name) {
            if let Some(inner_ty) = extract_treasury_cap_inner_type(type_name) {
                body.push(format!(
                    "    sui::coin::create_treasury_cap_for_testing<{}>(ctx)",
                    inner_ty
                ));
            } else {
                body.push("    abort 100001; // malformed TreasuryCap helper type".to_string());
            }
        } else {
            body.push(format!("    {} {{", type_name));
            if let Some(fields) = struct_fields.get(type_name) {
                for (field_name, field_ty) in fields {
                    let init = init_field_values
                        .get(type_name)
                        .and_then(|vals| vals.get(field_name))
                        .cloned()
                        .unwrap_or_else(|| default_field_initializer(field_name, field_ty));
                    body.push(format!("        {}: {},", field_name, init));
                }
            } else {
                body.push("        id: sui::object::new(ctx),".to_string());
            }
            body.push("    }".to_string());
        }
        body.push("}".to_string());
        body.push("".to_string());
    }

    if body.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    out.push("".to_string());
    out.extend(tidewalker_box_lines());
    out.extend(body);
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
        if let Some(e) = lines[s..]
            .iter()
            .position(|l| l.trim() == old_end)
            .map(|i| s + i)
        {
            return Some((s, e));
        }
    }

    // New format: strict box matching to avoid partial/incorrect removals.
    let is_border_line = |l: &str| {
        let t = l.trim();
        t.starts_with("//") && t.len() > 4 && t[2..].trim_start().starts_with('/')
    };
    let is_tidewalker_box_at = |idx: usize| -> bool {
        if idx + 4 >= lines.len() {
            return false;
        }
        is_border_line(lines[idx].as_str())
            && lines[idx + 2].contains("Tidewalker generated test helpers")
            && is_border_line(lines[idx + 4].as_str())
    };

    let mut box_starts: Vec<usize> = Vec::new();
    for i in 0..lines.len() {
        if is_tidewalker_box_at(i) {
            box_starts.push(i);
        }
    }
    for start in box_starts.iter().rev().copied() {
        let search_from = start + 5;
        if search_from >= lines.len() {
            continue;
        }
        if let Some(end_start) = box_starts
            .iter()
            .copied()
            .find(|&i| i >= search_from && is_tidewalker_box_at(i))
        {
            return Some((start, end_start + 4));
        }
    }
    None
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
    init_field_values: &std::collections::HashMap<
        String,
        std::collections::HashMap<String, String>,
    >,
    init_one_time_witness: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(source_path)?;
    let mut lines: Vec<String> = content.lines().map(String::from).collect();

    // Only remove our own block (identified by Tidewalker box/sentinels). Do not touch other test_only code.
    if let Some((s, e)) = find_tidewalker_block(&lines) {
        lines.drain(s..=e);
        // Trim trailing blank line if any
        while lines.last().map(|l| l.trim().is_empty()) == Some(true) {
            lines.pop();
        }
    }
    let has_existing_bootstrap = lines
        .iter()
        .any(|l| l.contains("fun bootstrap_init_for_testing("));
    let snippet = generate_in_module_test_only_snippet(
        mod_name,
        shared,
        caps,
        struct_fields,
        init_field_values,
        init_one_time_witness,
        has_existing_bootstrap,
    );

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
    let tn = t.replace(' ', "");
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
    if tn.contains("::table::Table<") {
        return "sui::table::new(ctx)".to_string();
    }
    if tn.contains("::vec_map::VecMap<") {
        return "sui::vec_map::empty()".to_string();
    }
    if tn.contains("::vec_set::VecSet<") {
        return "sui::vec_set::empty()".to_string();
    }
    if tn.contains("::bag::Bag") {
        return "sui::bag::new(ctx)".to_string();
    }
    if tn == "String" || tn.ends_with("string::String") {
        return "std::string::utf8(b\"tidewalker\")".to_string();
    }
    if tn.starts_with("Option<") || tn.contains("::option::Option<") {
        return "std::option::none()".to_string();
    }
    "0".to_string()
}
