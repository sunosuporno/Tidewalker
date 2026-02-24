use super::*;

pub(super) fn build_accessor_map(
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

pub(super) fn build_option_accessor_map(
    decls: &[FnDecl],
) -> std::collections::HashMap<String, Vec<OptionAccessorSig>> {
    let mut out: std::collections::HashMap<String, Vec<OptionAccessorSig>> =
        std::collections::HashMap::new();
    for d in decls {
        if d.is_test_only || !d.is_public {
            continue;
        }
        let sig = match parse_option_accessor_sig(d) {
            Some(v) => v,
            None => continue,
        };
        out.entry(d.module_name.clone()).or_default().push(sig);
    }
    out
}

pub(super) fn build_container_accessor_map(
    decls: &[FnDecl],
) -> std::collections::HashMap<String, Vec<ContainerAccessorSig>> {
    let mut out: std::collections::HashMap<String, Vec<ContainerAccessorSig>> =
        std::collections::HashMap::new();
    for d in decls {
        if d.is_test_only || !d.is_public {
            continue;
        }
        let sig = match parse_container_accessor_sig(d) {
            Some(v) => v,
            None => continue,
        };
        out.entry(d.module_name.clone()).or_default().push(sig);
    }
    out
}

fn parse_container_accessor_sig(d: &FnDecl) -> Option<ContainerAccessorSig> {
    if d.params.len() != 2 {
        return None;
    }
    let ret = d.return_ty.as_ref()?.trim();
    if ret != "bool" {
        return None;
    }
    let obj_param = d.params.first()?;
    if !obj_param.ty.trim().starts_with('&') {
        return None;
    }
    let key_param = d.params.get(1)?;
    let param_ty = normalize_param_object_type(&obj_param.ty);
    let obj_name = obj_param.name.as_str();
    let key_name = key_param.name.as_str();

    for line in &d.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        let stmt = stmt.trim_end_matches(';').trim();
        if let Some(field) = parse_container_presence_from_expr(stmt, obj_name, key_name) {
            return Some(ContainerAccessorSig {
                fn_name: d.fn_name.clone(),
                param_ty: param_ty.clone(),
                field,
            });
        }
    }
    None
}

fn parse_container_presence_from_expr(
    expr: &str,
    obj_param_name: &str,
    key_param_name: &str,
) -> Option<String> {
    for prefix in [
        "table::contains",
        "vec_map::contains",
        "vec_set::contains",
        "bag::contains",
        "dynamic_field::exists_",
    ] {
        let args = match extract_namespace_args_flexible(expr, prefix) {
            Some(v) => v,
            None => continue,
        };
        let object_arg = args.first()?.as_str();
        let key_arg = args.get(1)?.as_str();
        let key_token = strip_ref_and_parens_text(key_arg);
        if key_token != key_param_name {
            continue;
        }
        let object_path = strip_ref_and_parens_text(object_arg);
        let (base, field) = parse_field_access(object_path)?;
        if base == obj_param_name {
            return Some(field);
        }
    }
    None
}

fn parse_option_accessor_sig(d: &FnDecl) -> Option<OptionAccessorSig> {
    if d.params.len() != 1 {
        return None;
    }
    let ret = d.return_ty.as_ref()?.trim();
    if ret != "bool" {
        return None;
    }
    let param = d.params.first()?;
    if !param.ty.trim().starts_with('&') {
        return None;
    }
    let param_ty = normalize_param_object_type(&param.ty);
    let param_name = param.name.as_str();

    for line in &d.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        let stmt = stmt.trim_end_matches(';').trim();
        if let Some((field, is_some_when_true)) = parse_option_presence_from_expr(stmt, param_name)
        {
            return Some(OptionAccessorSig {
                fn_name: d.fn_name.clone(),
                param_ty: param_ty.clone(),
                field,
                is_some_when_true,
            });
        }
    }
    None
}

fn parse_option_presence_from_expr(expr: &str, param_name: &str) -> Option<(String, bool)> {
    let norm = remove_whitespace(expr);
    for (needle, is_some_when_true) in [("option::is_some(", true), ("option::is_none(", false)] {
        let idx = match norm.find(needle) {
            Some(v) => v,
            None => continue,
        };
        let after = &norm[idx + needle.len()..];
        let arg = match after.split(')').next() {
            Some(v) => v.trim_start_matches('&').trim(),
            None => continue,
        };
        let (base, field) = match parse_field_access(arg) {
            Some(v) => v,
            None => continue,
        };
        if base == param_name {
            return Some((field, is_some_when_true));
        }
    }
    None
}

#[derive(Debug, Default)]
pub(super) struct ModuleHelperCatalog {
    pub(super) shared_types: std::collections::HashSet<String>,
    pub(super) owned_types: std::collections::HashSet<String>,
}

pub(super) fn build_helper_catalog(
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

pub(super) fn build_fn_lookup(
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

#[derive(Debug, Default, Clone)]
pub(super) struct ModuleBootstrapCatalog {
    pub(super) one_time_witness_init: bool,
    pub(super) init_shared_types: std::collections::HashSet<String>,
    pub(super) init_owned_types: std::collections::HashSet<String>,
}

pub(super) fn build_bootstrap_catalog(
    module_sources: &std::collections::HashMap<String, String>,
) -> std::collections::HashMap<String, ModuleBootstrapCatalog> {
    let mut out = std::collections::HashMap::new();
    for (module_name, content) in module_sources {
        let info = parse_module_bootstrap_info(content);
        if info.one_time_witness_init
            || !info.init_shared_types.is_empty()
            || !info.init_owned_types.is_empty()
        {
            out.insert(module_name.clone(), info);
        }
    }
    out
}

fn parse_module_bootstrap_info(content: &str) -> ModuleBootstrapCatalog {
    let lines: Vec<&str> = content.lines().collect();
    let mut out = ModuleBootstrapCatalog::default();

    let Some((header, body_lines)) = find_init_header_and_body(&lines) else {
        return out;
    };
    out.one_time_witness_init = init_uses_one_time_witness(&header);

    let mut var_to_type: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for line in &body_lines {
        let t = line.split("//").next().unwrap_or("").trim();
        if let Some((var, ty)) = parse_init_struct_binding(t) {
            var_to_type.insert(var, ty);
        }
    }

    for line in &body_lines {
        let t = line.split("//").next().unwrap_or("").trim();
        if t.is_empty() {
            continue;
        }
        if let Some(var) = extract_first_arg_for_prefixes(
            t,
            &[
                "transfer::share_object",
                "transfer::public_share_object",
                "share_object",
                "public_share_object",
            ],
        ) {
            if let Some(ty) = var_to_type.get(&var) {
                out.init_shared_types.insert(type_key_from_type_name(ty));
            }
            continue;
        }

        if let Some((obj_var, recipient)) = parse_transfer_call(t) {
            if recipient.contains("sender(")
                || recipient.contains("tx_context::sender(")
                || recipient.contains("ctx.sender(")
            {
                if let Some(ty) = var_to_type.get(&obj_var) {
                    out.init_owned_types.insert(type_key_from_type_name(ty));
                }
            }
        }
    }

    out
}

fn find_init_header_and_body(lines: &[&str]) -> Option<(String, Vec<String>)> {
    let mut i = 0usize;
    while i < lines.len() {
        let t = lines[i].trim();
        if t.starts_with("fun init(") {
            let mut header = t.to_string();
            let mut j = i + 1;
            while j < lines.len() && !header.contains(')') {
                header.push(' ');
                header.push_str(lines[j].trim());
                j += 1;
            }

            let mut brace_depth = 0i32;
            let mut body_start: Option<usize> = None;
            let mut body_end: Option<usize> = None;
            for (k, l) in lines.iter().enumerate().skip(i) {
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

            let body = match (body_start, body_end) {
                (Some(s), Some(e)) if s <= e => lines[s..=e]
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>(),
                _ => Vec::new(),
            };
            return Some((header, body));
        }
        i += 1;
    }
    None
}

fn init_uses_one_time_witness(header: &str) -> bool {
    let params = header
        .split_once('(')
        .and_then(|(_, rest)| rest.split_once(')'))
        .map(|(p, _)| p)
        .unwrap_or("");
    let parts = split_params(params);
    let Some(first) = parts.first() else {
        return false;
    };
    let Some((name, ty)) = first.split_once(':') else {
        return false;
    };
    let pname = name.trim();
    let pty = ty.trim().replace(' ', "");
    if pty.contains("TxContext") || pty.contains("tx_context::TxContext") {
        return false;
    }
    pname.starts_with('_')
}

fn parse_init_struct_binding(stmt: &str) -> Option<(String, String)> {
    if !stmt.starts_with("let ") || !stmt.contains('=') || !stmt.contains('{') {
        return None;
    }
    let after_let = stmt.strip_prefix("let ")?;
    let (lhs, rhs) = after_let.split_once('=')?;
    let var = lhs.trim().trim_start_matches("mut ").trim();
    if !is_ident(var) {
        return None;
    }
    let ty = rhs
        .split_whitespace()
        .next()
        .unwrap_or("")
        .trim_end_matches('{')
        .trim();
    if ty.is_empty() {
        return None;
    }
    Some((var.to_string(), ty.to_string()))
}

fn extract_first_arg_for_prefixes(stmt: &str, prefixes: &[&str]) -> Option<String> {
    for prefix in prefixes {
        if let Some(args) = extract_namespace_args_flexible(stmt, prefix) {
            if let Some(first) = args.first() {
                let raw = strip_ref_and_parens_text(first);
                if is_ident(raw) {
                    return Some(raw.to_string());
                }
            }
        }
    }
    None
}

fn parse_transfer_call(stmt: &str) -> Option<(String, String)> {
    let args = extract_namespace_args_flexible(stmt, "transfer::transfer")
        .or_else(|| extract_namespace_args_flexible(stmt, "transfer::public_transfer"))
        .or_else(|| extract_namespace_args_flexible(stmt, "transfer"))
        .or_else(|| extract_namespace_args_flexible(stmt, "public_transfer"))?;
    if args.len() < 2 {
        return None;
    }
    let obj = strip_ref_and_parens_text(args[0].as_str()).to_string();
    let recipient = strip_ref_and_parens_text(args[1].as_str()).to_string();
    if !is_ident(&obj) {
        return None;
    }
    Some((obj, recipient))
}
