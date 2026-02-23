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
