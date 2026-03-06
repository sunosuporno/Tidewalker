use super::*;

pub(super) fn default_type_args_for_decl(d: &FnDecl) -> Vec<String> {
    default_type_args_for_params(&d.type_params)
}

pub(super) fn resolved_param_ty(
    owner: &FnDecl,
    param: &ParamDecl,
    concrete_type_args: &[String],
) -> String {
    concretize_type_params(&param.ty, &owner.type_params, concrete_type_args)
}

pub(super) fn infer_creator_provided_cap_type_keys(
    target_fn: &FnDecl,
    creator_fn: &FnDecl,
    creator_type_args: &[String],
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    if let Some(ret_ty) = creator_fn.return_ty.as_ref() {
        if is_cap_type(ret_ty) {
            out.insert(type_key_from_type_name(ret_ty));
        }
    }

    for param in &creator_fn.params {
        let resolved_ty = resolved_param_ty(creator_fn, param, creator_type_args);
        let cap_ty = normalize_param_object_type(&resolved_ty);
        if resolved_ty.trim().starts_with('&') && is_cap_type(&cap_ty) {
            out.insert(type_key_from_type_name(&cap_ty));
        }
    }

    let target_type_args = default_type_args_for_decl(target_fn);
    for param in &target_fn.params {
        let resolved_ty = resolved_param_ty(target_fn, param, &target_type_args);
        let cap_ty = normalize_param_object_type(&resolved_ty);
        if !is_cap_type(&cap_ty) {
            continue;
        }
        let key = type_key_from_type_name(&cap_ty);
        if fn_transfers_type_to_sender(creator_fn, &key, module_fns) {
            out.insert(key);
        }
    }

    out
}

fn parse_transfer_call(stmt: &str) -> Option<(String, String)> {
    for prefix in [
        "transfer::transfer(",
        "transfer::public_transfer(",
        "sui::transfer::transfer(",
        "sui::transfer::public_transfer(",
    ] {
        let Some(start) = stmt.find(prefix) else {
            continue;
        };
        let args_start = start + prefix.len();
        let mut depth = 1i32;
        let mut args_end: Option<usize> = None;
        for (off, ch) in stmt[args_start..].char_indices() {
            if ch == '(' {
                depth += 1;
            } else if ch == ')' {
                depth -= 1;
                if depth == 0 {
                    args_end = Some(args_start + off);
                    break;
                }
            }
        }
        let end = args_end?;
        let args = split_args(&stmt[args_start..end]);
        if args.len() < 2 {
            continue;
        }
        return Some((args[0].trim().to_string(), args[1].trim().to_string()));
    }
    None
}

fn parse_simple_let_binding(stmt: &str) -> Option<(String, String)> {
    if !stmt.starts_with("let ") || !stmt.contains('=') {
        return None;
    }
    let after = stmt.strip_prefix("let ")?;
    let (lhs, rhs) = after.split_once('=')?;
    let lhs_name = lhs.trim().trim_start_matches("mut ").trim();
    if !is_ident(lhs_name) {
        return None;
    }
    Some((
        lhs_name.to_string(),
        rhs.trim().trim_end_matches(';').trim().to_string(),
    ))
}

fn parse_function_call_name(expr: &str) -> Option<(Option<String>, String)> {
    let token = expr.split('(').next()?.trim();
    if token.is_empty() {
        return None;
    }
    if let Some((module, name)) = token.rsplit_once("::") {
        if module.is_empty() || name.is_empty() || !is_ident(name) {
            return None;
        }
        return Some((Some(module.to_string()), name.to_string()));
    }
    let name_clean = token
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
        .collect::<String>();
    if !is_ident(&name_clean) {
        return None;
    }
    Some((None, name_clean))
}

fn infer_expr_type_key_from_module_calls(
    expr: &str,
    default_module: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> Option<String> {
    let trimmed = strip_ref_and_parens_text(expr).trim();
    if let Some((left, _)) = trimmed.split_once('{') {
        let ty = left.trim();
        if !ty.is_empty() {
            return Some(type_key_from_type_name(ty));
        }
    }
    let (module_opt, fn_name) = parse_function_call_name(trimmed)?;
    if let Some(module) = module_opt {
        if module != default_module {
            return None;
        }
    }
    let callee = module_fns.get(&fn_name)?;
    let ret = callee.return_ty.as_ref()?;
    Some(type_key_from_type_name(ret))
}

pub(super) fn fn_transfers_type_to_sender(
    f: &FnDecl,
    target_type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> bool {
    let mut local_sender_aliases: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut local_var_type_keys: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for line in &f.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        if let Some((lhs, rhs)) = parse_simple_let_binding(stmt) {
            let rhs_clean = remove_whitespace(&rhs);
            if rhs_clean.contains("sender(") || rhs_clean.contains("tx_context::sender(") {
                local_sender_aliases.insert(lhs.clone());
            }
            if let Some(inferred_ty) =
                infer_expr_type_key_from_module_calls(&rhs, &f.module_name, module_fns)
            {
                local_var_type_keys.insert(lhs, inferred_ty);
            }
        }
    }
    for line in &f.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        let Some((obj_expr, recipient_expr)) = parse_transfer_call(stmt) else {
            continue;
        };
        let recipient_norm = strip_ref_and_parens_text(&recipient_expr).trim().to_string();
        let recipient_is_sender = remove_whitespace(&recipient_expr).contains("sender(")
            || local_sender_aliases.contains(&recipient_norm);
        if !recipient_is_sender {
            continue;
        }
        let obj_norm = strip_ref_and_parens_text(&obj_expr).trim().to_string();
        if let Some(key) = local_var_type_keys.get(&obj_norm) {
            if key == target_type_key {
                return true;
            }
        }
        if let Some(key) = infer_expr_type_key_from_module_calls(&obj_expr, &f.module_name, module_fns)
        {
            if key == target_type_key {
                return true;
            }
        }
    }
    false
}
