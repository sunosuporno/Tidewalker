fn coin_var_names(needs: &[CoinNeed]) -> Vec<String> {
    needs.iter().map(|n| n.var_name.clone()).collect::<Vec<_>>()
}

fn parse_inline_object_id_arg(expr: &str) -> Option<String> {
    let trimmed = remove_whitespace(expr);
    let prefixes = ["sui::object::id(&", "object::id(&", "id(&"];
    for prefix in prefixes {
        if let Some(rest) = trimmed.strip_prefix(prefix) {
            if let Some(inner) = rest.strip_suffix(')') {
                let name = inner.trim();
                if is_ident(name) {
                    return Some(name.to_string());
                }
            }
        }
    }
    None
}

fn rewrite_inline_object_id_args_for_call(
    d: &FnDecl,
    args: &mut [String],
) -> Vec<String> {
    let mut pre_call = Vec::new();
    let mut mut_borrowed = std::collections::HashSet::new();
    for a in args.iter() {
        if let Some(rest) = a.trim().strip_prefix("&mut ") {
            let name = strip_ref_and_parens_text(rest).trim().to_string();
            if is_ident(&name) {
                mut_borrowed.insert(name);
            }
        }
    }

    for (idx, arg) in args.iter_mut().enumerate() {
        let Some(obj_var) = parse_inline_object_id_arg(arg) else {
            continue;
        };
        let param_ty = d.params.get(idx).map(|p| p.ty.trim()).unwrap_or("");
        let needs_ref_id = param_ty.starts_with('&') && is_id_type(param_ty);
        let conflicts_with_mut = mut_borrowed.contains(&obj_var);
        if !needs_ref_id && !conflicts_with_mut {
            continue;
        }
        let tmp = format!("tw_arg_id_{}", idx);
        pre_call.push(format!(
            "let {} = sui::object::id(&{});",
            tmp, obj_var
        ));
        *arg = if needs_ref_id {
            format!("&{}", tmp)
        } else {
            tmp
        };
    }

    pre_call
}

fn normalize_chain_reuse_type(ty: &str) -> String {
    ty.trim()
        .trim_start_matches("&mut")
        .trim()
        .trim_start_matches('&')
        .trim()
        .to_string()
}

fn is_copyish_chain_reuse_type(ty: &str) -> bool {
    let base = normalize_chain_reuse_type(ty);
    base == "address"
        || base == "bool"
        || is_numeric_type(&base)
        || is_id_type(&base)
        || base == "vector<u8>"
        || base == "vector<u64>"
        || base == "vector<address>"
        || base == "vector<bool>"
}

fn chain_reuse_key(param_name: &str, resolved_ty: &str) -> Option<String> {
    let base = normalize_chain_reuse_type(resolved_ty);
    if base.is_empty()
        || base.contains("TxContext")
        || is_clock_type(&base)
        || is_coin_type(&base)
    {
        return None;
    }
    let supported = base == "address"
        || base == "bool"
        || is_numeric_type(&base)
        || is_id_type(&base)
        || is_option_type(&base)
        || is_string_type(&base)
        || is_vector_type(&base);
    if !supported {
        return None;
    }
    Some(format!(
        "{}::{}",
        sanitize_ident(param_name),
        sanitize_ident(&base)
    ))
}

fn record_shared_chain_arg_expr(
    shared_chain_args: &mut std::collections::HashMap<String, SharedChainArgSpec>,
    reuse_key: Option<&String>,
    expr: &str,
) {
    let Some(key) = reuse_key else {
        return;
    };
    shared_chain_args.entry(key.clone()).or_insert_with(|| SharedChainArgSpec {
        expr: expr.to_string(),
        binding_var: None,
    });
}

fn materialize_existing_shared_chain_arg(
    shared_chain_args: &mut std::collections::HashMap<String, SharedChainArgSpec>,
    reuse_key: &str,
    resolved_ty: &str,
    prep_lines: &mut Vec<String>,
) -> Option<String> {
    let spec = shared_chain_args.get_mut(reuse_key)?;
    if resolved_ty.trim().starts_with('&') {
        let var = if let Some(existing) = &spec.binding_var {
            existing.clone()
        } else {
            let var = format!("tw_chain_arg_{}", sanitize_ident(reuse_key));
            prep_lines.push(format!("let mut {} = {};", var, spec.expr));
            spec.binding_var = Some(var.clone());
            var
        };
        if resolved_ty.trim().starts_with("&mut") {
            Some(format!("&mut {}", var))
        } else {
            Some(format!("&{}", var))
        }
    } else if is_copyish_chain_reuse_type(resolved_ty) {
        let var = if let Some(existing) = &spec.binding_var {
            existing.clone()
        } else {
            let var = format!("tw_chain_arg_{}", sanitize_ident(reuse_key));
            prep_lines.push(format!("let {} = {};", var, spec.expr));
            spec.binding_var = Some(var.clone());
            var
        };
        Some(var)
    } else if let Some(var) = &spec.binding_var {
        Some(var.clone())
    } else {
        Some(spec.expr.clone())
    }
}

fn parse_top_level_tuple_types(ret_ty: &str) -> Option<Vec<String>> {
    let t = ret_ty.trim();
    if !(t.starts_with('(') && t.ends_with(')')) {
        return None;
    }
    let inner = &t[1..t.len() - 1];
    let parts = split_args(inner)
        .into_iter()
        .map(|p| p.trim().to_string())
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>();
    if parts.len() < 2 {
        return None;
    }
    Some(parts)
}

fn has_overflow_prone_mul(body_lines: &[String]) -> bool {
    body_lines.iter().any(|line| {
        let raw = line.split("//").next().unwrap_or("").trim();
        if raw.is_empty() || !raw.contains('*') {
            return false;
        }
        let norm = remove_whitespace(raw);
        let has_billion_scale_mul = norm.contains("*1_000_000_000") || norm.contains("*1000000000");
        let has_u128_cast = norm.contains("asu128");
        has_billion_scale_mul || !has_u128_cast
    })
}

fn parse_simple_let_binding(stmt: &str) -> Option<(String, String)> {
    let trimmed = stmt.trim().trim_end_matches(';').trim();
    let mut rest = trimmed.strip_prefix("let ")?;
    if let Some(after_mut) = rest.strip_prefix("mut ") {
        rest = after_mut;
    }
    let (lhs_raw, rhs_raw) = rest.split_once('=')?;
    let lhs = lhs_raw.split(':').next()?.trim();
    if !is_ident(lhs) {
        return None;
    }
    let rhs = rhs_raw.trim();
    if rhs.is_empty() {
        return None;
    }
    Some((lhs.to_string(), rhs.to_string()))
}

fn parse_function_call_name(expr: &str) -> Option<(Option<String>, String)> {
    let trimmed = expr.trim();
    let open = trimmed.find('(')?;
    let token = trimmed[..open].trim();
    if token.is_empty() {
        return None;
    }
    if let Some((module, name)) = token.rsplit_once("::") {
        let name_clean = name
            .split('<')
            .next()
            .unwrap_or(name)
            .trim()
            .to_string();
        if !is_ident(&name_clean) {
            return None;
        }
        return Some((Some(module.trim().to_string()), name_clean));
    }
    let name_clean = token
        .split('<')
        .next()
        .unwrap_or(token)
        .trim()
        .to_string();
    if !is_ident(&name_clean) {
        return None;
    }
    Some((None, name_clean))
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

fn infer_expr_type_name_from_module_calls(
    expr: &str,
    default_module: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> Option<String> {
    let trimmed = strip_ref_and_parens_text(expr).trim();
    if let Some((left, _)) = trimmed.split_once('{') {
        let ty = left.trim();
        if ty.is_empty() {
            return None;
        }
        if ty.contains("::") {
            return Some(ty.to_string());
        }
        return Some(format!("{}::{}", default_module, ty));
    }
    let (module_opt, fn_name) = parse_function_call_name(trimmed)?;
    if let Some(module) = module_opt {
        if module != default_module {
            return None;
        }
    }
    let callee = module_fns.get(&fn_name)?;
    let ret = callee.return_ty.as_ref()?.trim();
    if ret.is_empty() || ret == "()" {
        return None;
    }
    if ret.contains("::") {
        Some(ret.to_string())
    } else {
        Some(format!("{}::{}", default_module, ret))
    }
}

fn infer_transferred_object_types_for_fn(
    f: &FnDecl,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> Vec<String> {
    let mut local_var_type_names: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for line in &f.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        if let Some((lhs, rhs)) = parse_simple_let_binding(stmt) {
            if let Some(inferred) =
                infer_expr_type_name_from_module_calls(&rhs, &f.module_name, module_fns)
            {
                local_var_type_names.insert(lhs, inferred);
            }
        }
    }
    let mut out: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for line in &f.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        let Some((obj_expr, _recipient_expr)) = parse_transfer_call(stmt) else {
            continue;
        };
        let obj_norm = strip_ref_and_parens_text(&obj_expr).trim().to_string();
        if let Some(ty) = local_var_type_names.get(&obj_norm) {
            out.insert(ty.clone());
            continue;
        }
        if let Some(ty) = infer_expr_type_name_from_module_calls(&obj_expr, &f.module_name, module_fns)
        {
            out.insert(ty);
        }
    }
    out.into_iter().collect()
}

fn per_call_coin_mint_amount(
    d: &FnDecl,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
) -> &'static str {
    // Keep defaults high for broad protocol compatibility, but avoid
    // overflow-prone reserve math patterns in the function under test and
    // direct same-module callees (entry wrapper patterns).
    let mut overflow_prone = has_overflow_prone_mul(&d.body_lines);
    if !overflow_prone {
        if let Some(module_fns) = fn_lookup.get(&d.module_name) {
            'outer: for line in &d.body_lines {
                let stmt = line.split("//").next().unwrap_or("").trim();
                if stmt.is_empty() {
                    continue;
                }
                for (name, callee) in module_fns {
                    if name == &d.fn_name {
                        continue;
                    }
                    if stmt.contains(&format!("{}(", name))
                        && has_overflow_prone_mul(&callee.body_lines)
                    {
                        overflow_prone = true;
                        break 'outer;
                    }
                }
            }
        }
    }
    if overflow_prone {
        "1_000_000_000"
    } else {
        "1_000_000_000_000"
    }
}

fn cleanup_stmt_for_type(
    default_module: &str,
    ty: &str,
    var: &str,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Option<String> {
    if is_known_key_struct(ty, default_module, key_structs_by_module) {
        let (decl_module, _) = split_type_module_and_base(ty, default_module);
        let destroy_name = format!("destroy_{}_for_testing", type_key_from_type_name(ty));
        if fn_lookup
            .get(decl_module)
            .and_then(|fns| fns.get(&destroy_name))
            .map(|f| f.is_test_only)
            .unwrap_or(false)
        {
            return Some(format!("{}::{}({});", decl_module, destroy_name, var));
        }
        if !is_known_store_struct(ty, default_module, store_structs_by_module) {
            return None;
        }
    }
    Some(format!("transfer::public_transfer({}, SUPER_USER);", var))
}

fn requires_chain_uses_coin_slot_later(
    req_chain: &[&FnDecl],
    current_step_idx: usize,
    coin_slot_idx: usize,
    main_coin_slots: usize,
) -> bool {
    // Main function call happens after all preconditions; if it has this slot,
    // consuming here would starve the function under test.
    if coin_slot_idx < main_coin_slots {
        return true;
    }
    for req in req_chain.iter().skip(current_step_idx + 1) {
        let mut slot_idx = 0usize;
        for p in &req.params {
            if is_coin_type(p.ty.trim()) {
                if slot_idx == coin_slot_idx {
                    return true;
                }
                slot_idx += 1;
            }
        }
    }
    false
}

fn extract_assert_condition_local(stmt: &str) -> Option<String> {
    let idx = stmt.find("assert!(")?;
    let start = idx + "assert!(".len();
    let mut depth = 1i32;
    let mut end: Option<usize> = None;
    for (offset, ch) in stmt[start..].char_indices() {
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                end = Some(start + offset);
                break;
            }
        }
    }
    let end_idx = end?;
    let inside = &stmt[start..end_idx];
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    for (i, ch) in inside.char_indices() {
        match ch {
            '(' => paren += 1,
            ')' => paren -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            ',' if paren == 0 && bracket == 0 && brace == 0 => {
                return Some(inside[..i].trim().to_string());
            }
            _ => {}
        }
    }
    Some(inside.trim().to_string())
}

fn extract_mut_local_names(body_lines: &[String]) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    for line in body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if let Some(rest) = stmt.strip_prefix("let mut ") {
            let name = rest
                .split(|c: char| c.is_whitespace() || c == ':' || c == '=' || c == ',' || c == ';')
                .next()
                .unwrap_or("")
                .trim();
            if is_ident(name) {
                out.insert(name.to_string());
            }
        }
    }
    out
}

fn has_assert_on_mut_local(body_lines: &[String], params: &[ParamDecl]) -> bool {
    let mut_locals = extract_mut_local_names(body_lines);
    if mut_locals.is_empty() {
        return false;
    }
    let param_names = params
        .iter()
        .map(|p| p.name.as_str())
        .collect::<std::collections::HashSet<_>>();
    for line in body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if !stmt.contains("assert!(") {
            continue;
        }
        let Some(cond) = extract_assert_condition_local(stmt) else {
            continue;
        };
        for token in cond.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_')) {
            if token.is_empty() || param_names.contains(token) {
                continue;
            }
            if mut_locals.contains(token) {
                return true;
            }
        }
    }
    false
}

fn has_clock_ge_guard(body_lines: &[String]) -> bool {
    for line in body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if !stmt.contains("assert!(") {
            continue;
        }
        let Some(cond) = extract_assert_condition_local(stmt) else {
            continue;
        };
        let norm = remove_whitespace(&cond);
        if norm.contains("timestamp_ms(") && (norm.contains(">=") || norm.contains("<=")) {
            return true;
        }
    }
    false
}

fn detect_index_of_guard_pattern(d: &FnDecl) -> Option<(String, String, String)> {
    let mut candidates: Vec<(String, String, String, String)> = Vec::new();
    for line in &d.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if !stmt.starts_with("let ") || !stmt.contains(".index_of(&") {
            continue;
        }
        let Some((lhs_raw, rhs_raw)) = stmt.trim_end_matches(';').split_once('=') else {
            continue;
        };
        let lhs = lhs_raw.trim().trim_start_matches("let").trim();
        if !(lhs.starts_with('(') && lhs.contains(')')) {
            continue;
        }
        let tuple_inner = lhs.trim_start_matches('(').split(')').next().unwrap_or("");
        let bool_var = tuple_inner
            .split(',')
            .next()
            .map(|s| s.trim())
            .unwrap_or("");
        if !is_ident(bool_var) {
            continue;
        }
        let rhs = remove_whitespace(rhs_raw);
        let marker = ".index_of(&";
        let Some(idx) = rhs.find(marker) else {
            continue;
        };
        let base = rhs[..idx].trim();
        let arg_start = idx + marker.len();
        let Some(arg_end_rel) = rhs[arg_start..].find(')') else {
            continue;
        };
        let value_var = rhs[arg_start..arg_start + arg_end_rel].trim();
        let Some((obj_param, field)) = base.split_once('.') else {
            continue;
        };
        if !is_ident(obj_param) || !is_ident(field) || !is_ident(value_var) {
            continue;
        }
        candidates.push((
            bool_var.to_string(),
            obj_param.to_string(),
            field.to_string(),
            value_var.to_string(),
        ));
    }
    if candidates.is_empty() {
        return None;
    }
    let assert_conds = d
        .body_lines
        .iter()
        .filter_map(|line| {
            let stmt = line.split("//").next().unwrap_or("").trim();
            extract_assert_condition_local(stmt).map(|c| remove_whitespace(&c))
        })
        .collect::<Vec<_>>();
    for (bool_var, obj_param, field, value_var) in &candidates {
        let direct = bool_var.clone();
        let wrapped = format!("({})", bool_var);
        if assert_conds
            .iter()
            .any(|c| c == &direct || c == &wrapped || c.starts_with(&format!("{}&&", direct)))
        {
            return Some((obj_param.clone(), field.clone(), value_var.clone()));
        }
    }
    candidates
        .first()
        .map(|(_, obj_param, field, value_var)| (obj_param.clone(), field.clone(), value_var.clone()))
}

fn find_seed_function_for_index_of_pattern<'a>(
    d: &FnDecl,
    module_fns: &'a std::collections::HashMap<String, FnDecl>,
    object_param: &str,
    field: &str,
    value_param: &str,
) -> Option<(&'a FnDecl, String)> {
    let target_obj_ty = d
        .params
        .iter()
        .find(|p| p.name == object_param)
        .map(|p| normalize_param_object_type(&p.ty))?;
    let target_value_ty = normalize_param_object_type(
        &d.params
            .iter()
            .find(|p| p.name == value_param)?
            .ty,
    );
    let mut best: Option<(usize, &FnDecl, String)> = None;
    for f in module_fns.values() {
        if f.fn_name == d.fn_name || f.is_test_only || !(f.is_public || f.is_entry) {
            continue;
        }
        let obj_param_in_seed = f
            .params
            .iter()
            .find(|p| {
                let t = p.ty.trim();
                t.starts_with("&mut")
                    && normalize_param_object_type(t) == target_obj_ty
                    && !t.contains("TxContext")
            })
            .map(|p| p.name.clone());
        let Some(obj_param_in_seed) = obj_param_in_seed else {
            continue;
        };
        let value_param_in_seed = f
            .params
            .iter()
            .find(|p| {
                !p.ty.trim().starts_with('&')
                    && normalize_param_object_type(&p.ty) == target_value_ty
            })
            .map(|p| p.name.clone());
        let Some(value_param_in_seed) = value_param_in_seed else {
            continue;
        };
        let mut matches_push = false;
        let needle = format!(
            "push_back(&mut{}.{},{})",
            obj_param_in_seed, field, value_param_in_seed
        );
        for line in &f.body_lines {
            let stmt = remove_whitespace(line.split("//").next().unwrap_or("").trim());
            if stmt.contains(&needle) {
                matches_push = true;
                break;
            }
        }
        if !matches_push {
            continue;
        }
        let score = f.params.len();
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some((score, f, value_param_in_seed));
        }
    }
    best.map(|(_, f, seed_value)| (f, seed_value))
}

fn synthesize_auto_prestate_calls(
    d: &FnDecl,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    object_vars_by_type: &std::collections::HashMap<String, String>,
    coin_vars_for_calls: &[String],
    requires_extra_coin_slots: &mut std::collections::HashMap<usize, String>,
    pre_coin_counter: &mut usize,
    requires_cleanup_coin_vars: &mut Vec<String>,
    requires_cleanup_clock_vars: &mut Vec<String>,
    requires_consumed_coin_vars: &mut std::collections::HashSet<String>,
    param_arg_values: &std::collections::HashMap<String, String>,
) -> Vec<String> {
    let Some((object_param, field, value_param)) = detect_index_of_guard_pattern(d) else {
        return Vec::new();
    };
    let Some(module_fns) = fn_lookup.get(&d.module_name) else {
        return Vec::new();
    };
    let Some((seed_decl, seed_value_param)) = find_seed_function_for_index_of_pattern(
        d,
        module_fns,
        &object_param,
        &field,
        &value_param,
    ) else {
        return Vec::new();
    };
    let mut pre_lines: Vec<String> = Vec::new();
    let mut seed_args: Vec<String> = Vec::new();
    for p in &seed_decl.params {
        let t = p.ty.trim();
        if t.contains("TxContext") {
            seed_args.push("test_scenario::ctx(&mut scenario)".to_string());
            continue;
        }
        if is_clock_type(t) {
            let var = format!("prestate_clock_{}", sanitize_ident(&p.name));
            if t.starts_with("&mut") {
                pre_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                pre_lines.push(format!("sui::clock::set_for_testing(&mut {}, 1000000);", var));
                seed_args.push(format!("&mut {}", var));
            } else if t.starts_with('&') {
                pre_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                pre_lines.push(format!("sui::clock::set_for_testing(&mut {}, 1000000);", var));
                seed_args.push(format!("&{}", var));
            } else {
                pre_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                pre_lines.push(format!("sui::clock::set_for_testing(&mut {}, 1000000);", var));
                seed_args.push(var.clone());
            }
            requires_cleanup_clock_vars.push(var);
            continue;
        }
        if p.name == seed_value_param {
            if let Some(seed_value_expr) = param_arg_values.get(&value_param) {
                seed_args.push(seed_value_expr.clone());
            } else if t == "u64" {
                seed_args.push(choose_u64_arg_for_param_in_fn(&p.name, &seed_decl.body_lines));
            } else if is_numeric_type(t) {
                seed_args.push("1".to_string());
            } else {
                return Vec::new();
            }
            continue;
        }
        if t.starts_with('&') && !is_coin_type(t) {
            let key = type_key_from_type_name(&normalize_param_object_type(t));
            let Some(var) = object_vars_by_type.get(&key) else {
                return Vec::new();
            };
            if t.starts_with("&mut") {
                seed_args.push(format!("&mut {}", var));
            } else {
                seed_args.push(format!("&{}", var));
            }
            continue;
        }
        if is_coin_type(t) {
            let _ = (
                coin_vars_for_calls,
                requires_extra_coin_slots,
                pre_coin_counter,
                requires_cleanup_coin_vars,
                requires_consumed_coin_vars,
            );
            return Vec::new();
        }
        if t == "u64" {
            seed_args.push(choose_u64_arg_for_param_in_fn(&p.name, &seed_decl.body_lines));
        } else if is_numeric_type(t) {
            seed_args.push("1".to_string());
        } else if t == "bool" {
            seed_args.push("false".to_string());
        } else if t == "address" {
            seed_args.push("SUPER_USER".to_string());
        } else if is_id_type(t) {
            seed_args.push(default_id_arg_expr());
        } else if is_option_type(t) {
            seed_args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            seed_args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else {
            let Some(expr) = synthesize_value_expr_for_type(
                t,
                &d.module_name,
                fn_lookup,
                Some(&format!("{}::{}", d.module_name, seed_decl.fn_name)),
                0,
            ) else {
                return Vec::new();
            };
            seed_args.push(expr);
        }
    }
    let mut out = pre_lines;
    out.push(format!(
        "{}::{}({});",
        d.module_name,
        seed_decl.fn_name,
        seed_args.join(", ")
    ));
    out
}

fn synthesize_requires_call_args(
    d: &FnDecl,
    req_step_idx: usize,
    req_chain: &[&FnDecl],
    object_vars_by_type: &std::collections::HashMap<String, String>,
    shared_chain_args: &mut std::collections::HashMap<String, SharedChainArgSpec>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    main_coin_vars: &[String],
    requires_extra_coin_slots: &mut std::collections::HashMap<usize, String>,
    pre_coin_counter: &mut usize,
    requires_cleanup_coin_vars: &mut Vec<String>,
    requires_cleanup_clock_vars: &mut Vec<String>,
    requires_consumed_coin_vars: &mut std::collections::HashSet<String>,
    coin_mint_amount: &str,
) -> Option<(Vec<String>, Vec<String>)> {
    let mut args = Vec::new();
    let mut prep = Vec::new();
    let mut coin_slot_idx = 0usize;
    let req_default_type_args = default_type_args_for_params(&d.type_params);
    let req_aliases = req_chain
        .get(req_step_idx)
        .map(|r| &r.module_use_aliases)
        .unwrap_or(&d.module_use_aliases);
    for p in &d.params {
        let resolved_ty = concretize_type_params(&p.ty, &d.type_params, &req_default_type_args);
        let t = resolved_ty.trim();
        let reuse_key = chain_reuse_key(&p.name, t);
        if let Some(key) = reuse_key.as_ref() {
            if let Some(arg) =
                materialize_existing_shared_chain_arg(shared_chain_args, key, t, &mut prep)
            {
                args.push(arg);
                continue;
            }
        }
        if is_vector_type(t) {
            let vec_expr = vector_literal_expr_for_type(t)?;
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &vec_expr);
            if t.starts_with('&') {
                if let Some(key) = reuse_key.as_ref() {
                    args.push(materialize_existing_shared_chain_arg(
                        shared_chain_args,
                        key,
                        t,
                        &mut prep,
                    )?);
                } else {
                    let var = format!("pre_req_vec_{}_{}", req_step_idx, sanitize_ident(&p.name));
                    let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                    prep.push(format!("let {}{} = {};", maybe_mut, var, vec_expr));
                    if t.starts_with("&mut") {
                        args.push(format!("&mut {}", var));
                    } else {
                        args.push(format!("&{}", var));
                    }
                }
            } else {
                args.push(
                    reuse_key
                        .as_ref()
                        .and_then(|key| {
                            materialize_existing_shared_chain_arg(
                                shared_chain_args,
                                key,
                                t,
                                &mut prep,
                            )
                        })
                        .unwrap_or(vec_expr),
                );
            }
        } else if t.starts_with('&') && !t.contains("TxContext") && !is_coin_type(t) {
            let ty = normalize_param_object_type(t);
            let key = type_key_from_type_name(&ty);
            if let Some(var) = object_vars_by_type.get(&key) {
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", var));
                } else {
                    args.push(format!("&{}", var));
                }
            } else {
                let expr = synthesize_value_expr_for_type(
                    t,
                    &d.module_name,
                    fn_lookup,
                    Some(&format!("{}::{}", d.module_name, d.fn_name)),
                    0,
                )?;
                let var = format!("pre_req_value_{}_{}", req_step_idx, sanitize_ident(&p.name));
                let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                prep.push(format!("let {}{} = {};", maybe_mut, var, expr));
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", var));
                } else {
                    args.push(format!("&{}", var));
                }
            }
        } else if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_clock_type(t) {
            let var = format!("pre_req_clock_{}_{}", req_step_idx, sanitize_ident(&p.name));
            if t.starts_with("&mut") {
                prep.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&mut {}", var));
                requires_cleanup_clock_vars.push(var);
            } else if t.starts_with('&') {
                prep.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&{}", var));
                requires_cleanup_clock_vars.push(var);
            } else {
                prep.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(var);
            }
        } else if is_coin_type(t) {
            if t.starts_with('&') {
                if let Some(var) = main_coin_vars.get(coin_slot_idx) {
                    if t.starts_with("&mut") {
                        args.push(format!("&mut {}", var));
                    } else {
                        args.push(format!("&{}", var));
                    }
                } else if let Some(var) = requires_extra_coin_slots.get(&coin_slot_idx) {
                    if t.starts_with("&mut") {
                        args.push(format!("&mut {}", var));
                    } else {
                        args.push(format!("&{}", var));
                    }
                } else {
                    let var = format!("pre_req_coin_{}", *pre_coin_counter);
                    *pre_coin_counter += 1;
                    let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                    let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, req_aliases);
                    if !is_qualified_type_tag(&coin_ty) {
                        return None;
                    }
                    prep.push(format!(
                        "let {}{} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
                        maybe_mut, var, coin_ty, coin_mint_amount
                    ));
                    if t.starts_with("&mut") {
                        args.push(format!("&mut {}", var));
                    } else {
                        args.push(format!("&{}", var));
                    }
                    requires_extra_coin_slots.insert(coin_slot_idx, var.clone());
                    requires_cleanup_coin_vars.push(var);
                }
                coin_slot_idx += 1;
            } else {
                let must_preserve_for_later = requires_chain_uses_coin_slot_later(
                    req_chain,
                    req_step_idx,
                    coin_slot_idx,
                    main_coin_vars.len(),
                );
                if must_preserve_for_later {
                    // This precondition consumes a coin slot still needed later.
                    // Mint a dedicated object only for this call.
                    let var = format!("pre_req_coin_{}", *pre_coin_counter);
                    *pre_coin_counter += 1;
                    let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, req_aliases);
                    if !is_qualified_type_tag(&coin_ty) {
                        return None;
                    }
                    prep.push(format!(
                        "let {} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
                        var, coin_ty, coin_mint_amount
                    ));
                    args.push(var);
                } else if let Some(var) = main_coin_vars.get(coin_slot_idx) {
                    args.push(var.clone());
                } else if let Some(var) = requires_extra_coin_slots.remove(&coin_slot_idx) {
                    // Reuse and consume an existing precondition slot coin when no later
                    // step needs this slot.
                    requires_consumed_coin_vars.insert(var.clone());
                    requires_cleanup_coin_vars.retain(|v| v != &var);
                    args.push(var);
                } else {
                    // One-off by-value precondition coin (consumed in this call).
                    let var = format!("pre_req_coin_{}", *pre_coin_counter);
                    *pre_coin_counter += 1;
                    let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, req_aliases);
                    if !is_qualified_type_tag(&coin_ty) {
                        return None;
                    }
                    prep.push(format!(
                        "let {} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
                        var, coin_ty, coin_mint_amount
                    ));
                    args.push(var);
                }
                coin_slot_idx += 1;
            }
        } else if t == "u64" {
            let expr = choose_u64_arg_for_param_in_fn(&p.name, &d.body_lines);
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &expr);
            args.push(
                reuse_key
                    .as_ref()
                    .and_then(|key| {
                        materialize_existing_shared_chain_arg(
                            shared_chain_args,
                            key,
                            t,
                            &mut prep,
                        )
                    })
                    .unwrap_or(expr),
            );
        } else if is_numeric_type(t) {
            let expr = "1".to_string();
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &expr);
            args.push(
                reuse_key
                    .as_ref()
                    .and_then(|key| {
                        materialize_existing_shared_chain_arg(
                            shared_chain_args,
                            key,
                            t,
                            &mut prep,
                        )
                    })
                    .unwrap_or(expr),
            );
        } else if t == "bool" {
            let expr = "false".to_string();
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &expr);
            args.push(
                reuse_key
                    .as_ref()
                    .and_then(|key| {
                        materialize_existing_shared_chain_arg(
                            shared_chain_args,
                            key,
                            t,
                            &mut prep,
                        )
                    })
                    .unwrap_or(expr),
            );
        } else if t == "address" {
            // Helper/factory objects are usually consumed in SUPER_USER tx by default.
            let expr = "SUPER_USER".to_string();
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &expr);
            args.push(
                reuse_key
                    .as_ref()
                    .and_then(|key| {
                        materialize_existing_shared_chain_arg(
                            shared_chain_args,
                            key,
                            t,
                            &mut prep,
                        )
                    })
                    .unwrap_or(expr),
            );
        } else if is_id_type(t) {
            let expr = default_id_arg_expr();
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &expr);
            args.push(
                reuse_key
                    .as_ref()
                    .and_then(|key| {
                        materialize_existing_shared_chain_arg(
                            shared_chain_args,
                            key,
                            t,
                            &mut prep,
                        )
                    })
                    .unwrap_or(expr),
            );
        } else if is_option_type(t) {
            let expr = option_none_expr_for_type(t);
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &expr);
            args.push(
                reuse_key
                    .as_ref()
                    .and_then(|key| {
                        materialize_existing_shared_chain_arg(
                            shared_chain_args,
                            key,
                            t,
                            &mut prep,
                        )
                    })
                    .unwrap_or(expr),
            );
        } else if is_string_type(t) {
            let expr = "std::string::utf8(b\"tidewalker\")".to_string();
            record_shared_chain_arg_expr(shared_chain_args, reuse_key.as_ref(), &expr);
            args.push(
                reuse_key
                    .as_ref()
                    .and_then(|key| {
                        materialize_existing_shared_chain_arg(
                            shared_chain_args,
                            key,
                            t,
                            &mut prep,
                        )
                    })
                    .unwrap_or(expr),
            );
        } else {
            args.push(synthesize_value_expr_for_type(
                t,
                &d.module_name,
                fn_lookup,
                Some(&format!("{}::{}", d.module_name, d.fn_name)),
                0,
            )?);
        }
    }
    Some((args, prep))
}

fn synthesize_factory_args(
    factory: &FnDecl,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    coin_mint_amount: &str,
) -> Option<Vec<String>> {
    let mut args = Vec::new();
    let factory_type_args = default_type_args_for_decl(factory);
    for p in &factory.params {
        let resolved_ty = resolved_param_ty(factory, p, &factory_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_coin_type(t) && !t.starts_with('&') {
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &factory.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            args.push(format!(
                "coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario))",
                coin_ty, coin_mint_amount
            ));
        } else if is_vector_type(t) && !t.starts_with('&') {
            args.push(vector_literal_expr_for_type(t)?);
        } else if t == "u64" {
            args.push(choose_u64_arg_for_param_in_fn(&p.name, &factory.body_lines));
        } else if is_numeric_type(t) {
            args.push("1".to_string());
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            // Helper/factory objects are usually consumed in SUPER_USER tx by default.
            args.push("SUPER_USER".to_string());
        } else if is_id_type(t) {
            args.push(default_id_arg_expr());
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else if !t.starts_with('&') {
            args.push(synthesize_value_expr_for_type(
                t,
                &factory.module_name,
                fn_lookup,
                Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                0,
            )?);
        } else {
            return None;
        }
    }
    Some(args)
}

fn synthesize_factory_args_with_refs(
    factory: &FnDecl,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    coin_mint_amount: &str,
) -> Option<FactoryArgPlan> {
    let mut args = Vec::new();
    let mut prep_lines = Vec::new();
    let mut cleanup_lines = Vec::new();
    let factory_type_args = default_type_args_for_decl(factory);
    for (idx, p) in factory.params.iter().enumerate() {
        let resolved_ty = resolved_param_ty(factory, p, &factory_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
            continue;
        }
        if is_clock_type(t) {
            let var = format!("factory_clock_{}_{}", idx, sanitize_ident(&p.name));
            if t.starts_with("&mut") {
                prep_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&mut {}", var));
            } else if t.starts_with('&') {
                prep_lines.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&{}", var));
            } else {
                prep_lines.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(var.clone());
            }
            cleanup_lines.push(format!("sui::clock::destroy_for_testing({});", var));
            continue;
        }
        if is_coin_type(t) {
            let var = format!("factory_coin_{}_{}", idx, sanitize_ident(&p.name));
            let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &factory.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            prep_lines.push(format!(
                "let {}{} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
                maybe_mut, var, coin_ty, coin_mint_amount
            ));
            if t.starts_with("&mut") {
                args.push(format!("&mut {}", var));
            } else if t.starts_with('&') {
                args.push(format!("&{}", var));
            } else {
                args.push(var.clone());
            }
            if t.starts_with('&') {
                cleanup_lines.push(format!("transfer::public_transfer({}, SUPER_USER);", var));
            }
            continue;
        }
        if t.starts_with('&') {
            let inner_ty = normalize_param_object_type(t);
            let expr = synthesize_value_expr_for_type(
                &inner_ty,
                &factory.module_name,
                fn_lookup,
                Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                0,
            )?;
            let var = format!("factory_ref_{}_{}", idx, sanitize_ident(&p.name));
            let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
            prep_lines.push(format!("let {}{} = {};", maybe_mut, var, expr));
            if t.starts_with("&mut") {
                args.push(format!("&mut {}", var));
            } else {
                args.push(format!("&{}", var));
            }
            if should_transfer_call_return_with_keys(
                &inner_ty,
                &factory.module_name,
                key_structs_by_module,
            ) {
                cleanup_lines.push(cleanup_stmt_for_type(
                    &factory.module_name,
                    &inner_ty,
                    &var,
                    fn_lookup,
                    key_structs_by_module,
                    store_structs_by_module,
                )?);
            }
            continue;
        }

        if is_vector_type(t) {
            args.push(vector_literal_expr_for_type(t)?);
        } else if t == "u64" {
            args.push(choose_u64_arg_for_param_in_fn(&p.name, &factory.body_lines));
        } else if is_numeric_type(t) {
            args.push("1".to_string());
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            args.push("SUPER_USER".to_string());
        } else if is_id_type(t) {
            args.push(default_id_arg_expr());
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else {
            args.push(synthesize_value_expr_for_type(
                t,
                &factory.module_name,
                fn_lookup,
                Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                0,
            )?);
        }
    }
    Some(FactoryArgPlan {
        args,
        prep_lines,
        cleanup_lines,
    })
}

fn synthesize_owned_factory_call_plan(
    d: &FnDecl,
    factory: &FnDecl,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    coin_mint_amount: &str,
) -> Option<FactoryCallPlan> {
    let mut args = Vec::new();
    let mut prep_lines = Vec::new();
    let mut cleanup_lines = Vec::new();
    let mut shared_ref_deps: Vec<(String, bool)> = Vec::new();
    let factory_type_args = default_type_args_for_decl(factory);
    for (idx, p) in factory.params.iter().enumerate() {
        let resolved_ty = resolved_param_ty(factory, p, &factory_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
            continue;
        }
        if is_clock_type(t) {
            let var = format!("factory_clock_{}_{}", idx, sanitize_ident(&p.name));
            if t.starts_with("&mut") {
                prep_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&mut {}", var));
            } else if t.starts_with('&') {
                prep_lines.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&{}", var));
            } else {
                prep_lines.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(var.clone());
            }
            cleanup_lines.push(format!("sui::clock::destroy_for_testing({});", var));
            continue;
        }
        if is_coin_type(t) {
            let var = format!("factory_coin_{}_{}", idx, sanitize_ident(&p.name));
            let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &factory.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            prep_lines.push(format!(
                "let {}{} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
                maybe_mut, var, coin_ty, coin_mint_amount
            ));
            if t.starts_with("&mut") {
                args.push(format!("&mut {}", var));
            } else if t.starts_with('&') {
                args.push(format!("&{}", var));
            } else {
                args.push(var.clone());
            }
            if t.starts_with('&') {
                cleanup_lines.push(format!("transfer::public_transfer({}, SUPER_USER);", var));
            }
            continue;
        }
        if t.starts_with('&') {
            let inner_ty = normalize_param_object_type(t);
            if is_known_key_struct(&inner_ty, &d.module_name, key_structs_by_module) {
                let type_key = type_key_from_type_name(&inner_ty);
                let token = format!("__TW_OBJ_BY_TYPE_{}__", sanitize_ident(&type_key));
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", token));
                    shared_ref_deps.push((inner_ty, true));
                } else {
                    args.push(format!("&{}", token));
                    shared_ref_deps.push((inner_ty, false));
                }
            } else {
                let expr = synthesize_value_expr_for_type(
                    &inner_ty,
                    &factory.module_name,
                    fn_lookup,
                    Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                    0,
                )?;
                let var = format!("factory_ref_{}_{}", idx, sanitize_ident(&p.name));
                let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                prep_lines.push(format!("let {}{} = {};", maybe_mut, var, expr));
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", var));
                } else {
                    args.push(format!("&{}", var));
                }
            }
            continue;
        }
        if is_vector_type(t) {
            args.push(vector_literal_expr_for_type(t)?);
        } else if t == "u64" {
            args.push(choose_u64_arg_for_param_in_fn(&p.name, &factory.body_lines));
        } else if is_numeric_type(t) {
            args.push("1".to_string());
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            args.push("SUPER_USER".to_string());
        } else if is_id_type(t) {
            args.push(default_id_arg_expr());
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else {
            args.push(synthesize_value_expr_for_type(
                t,
                &factory.module_name,
                fn_lookup,
                Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                0,
            )?);
        }
    }
    let type_args = factory_type_args;
    Some(FactoryCallPlan {
        module_name: factory.module_name.clone(),
        fn_name: factory.fn_name.clone(),
        provision_mode: OwnedFactoryProvision::ReturnValue,
        type_args,
        args,
        prep_lines,
        cleanup_lines,
        shared_ref_deps,
    })
}

fn pick_factory_call_for_type(
    d: &FnDecl,
    type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    coin_mint_amount: &str,
) -> Option<FactoryCallPlan> {
    let mut best: Option<(usize, FactoryCallPlan)> = None;
    for f in module_fns.values() {
        if f.is_test_only {
            continue;
        }
        if !(f.is_public || f.is_entry) {
            continue;
        }
        if f.fn_name == d.fn_name || f.fn_name == "init" {
            continue;
        }
        let returns_target = f
            .return_ty
            .as_ref()
            .map(|ret| type_key_from_type_name(ret) == type_key)
            .unwrap_or(false);
        let transfers_target = fn_transfers_type_to_sender(f, type_key, module_fns);
        if !returns_target && !transfers_target {
            continue;
        }
        let Some(plan) = synthesize_owned_factory_call_plan(
            d,
            f,
            fn_lookup,
            key_structs_by_module,
            coin_mint_amount,
        ) else {
            continue;
        };
        let mut plan = plan;
        if transfers_target && !returns_target {
            plan.provision_mode = OwnedFactoryProvision::SenderTransfer;
        }
        let score = f.params.len();
        let candidate = (score, plan);
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some(candidate);
        }
    }
    best.map(|(_, plan)| plan)
}

fn pick_owned_test_helper_for_type(
    d: &FnDecl,
    type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    helper_module_name: &str,
    coin_mint_amount: &str,
) -> Option<HelperCallPlan> {
    let wanted = normalize_helper_key(type_key);
    let mut best: Option<(usize, HelperCallPlan)> = None;
    for f in module_fns.values() {
        if !f.is_test_only || f.fn_name == d.fn_name || f.fn_name == "init" {
            continue;
        }
        if !(f.fn_name.starts_with("create_") && f.fn_name.ends_with("_for_testing")) {
            continue;
        }
        if f.fn_name.starts_with("create_and_share_") {
            continue;
        }
        let helper_name_match = f
            .fn_name
            .strip_prefix("create_")
            .and_then(|x| x.strip_suffix("_for_testing"))
            .map(|x| normalize_helper_key(x) == wanted)
            .unwrap_or(false);
        let ret_type_match = f
            .return_ty
            .as_ref()
            .map(|r| type_key_from_type_name(r) == type_key)
            .unwrap_or(false);
        if !helper_name_match && !ret_type_match {
            continue;
        }
        let Some(args) = synthesize_factory_args(f, fn_lookup, coin_mint_amount) else {
            continue;
        };
        let type_args = if has_unbound_type_params(f) {
            default_type_args_for_params(&f.type_params)
        } else {
            Vec::new()
        };
        let plan = HelperCallPlan {
            module_name: helper_module_name.to_string(),
            fn_name: f.fn_name.clone(),
            args,
            type_args,
        };
        let score = f.params.len();
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some((score, plan));
        }
    }
    best.map(|(_, plan)| plan)
}

fn pick_shared_test_helper_for_type(
    d: &FnDecl,
    type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    helper_module_name: &str,
    coin_mint_amount: &str,
) -> Option<HelperCallPlan> {
    let wanted = normalize_helper_key(type_key);
    let mut best: Option<(usize, HelperCallPlan)> = None;
    for f in module_fns.values() {
        if !f.is_test_only || f.fn_name == d.fn_name || f.fn_name == "init" {
            continue;
        }
        if !(f.fn_name.starts_with("create_and_share_") && f.fn_name.ends_with("_for_testing")) {
            continue;
        }
        let helper_name_match = f
            .fn_name
            .strip_prefix("create_and_share_")
            .and_then(|x| x.strip_suffix("_for_testing"))
            .map(|x| normalize_helper_key(x) == wanted)
            .unwrap_or(false);
        let ret_type_match = f
            .return_ty
            .as_ref()
            .map(|r| type_key_from_type_name(r) == type_key)
            .unwrap_or(false);
        if !helper_name_match && !ret_type_match {
            continue;
        }
        let Some(args) = synthesize_factory_args(f, fn_lookup, coin_mint_amount) else {
            continue;
        };
        let type_args = if has_unbound_type_params(f) {
            default_type_args_for_params(&f.type_params)
        } else {
            Vec::new()
        };
        let plan = HelperCallPlan {
            module_name: helper_module_name.to_string(),
            fn_name: f.fn_name.clone(),
            args,
            type_args,
        };
        let score = f.params.len();
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some((score, plan));
        }
    }
    best.map(|(_, plan)| plan)
}

fn pick_shared_creator_plan(
    d: &FnDecl,
    module_fns: &std::collections::HashMap<String, FnDecl>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    _required_cap_type_keys: &std::collections::BTreeSet<String>,
    required_shared_type_keys: &std::collections::BTreeSet<String>,
    coin_mint_amount: &str,
) -> Option<SharedCreatorPlan> {
    let mut candidates = module_fns
        .values()
        .filter(|f| {
            !f.is_test_only
                && (f.is_public || f.is_entry)
                && f.fn_name != d.fn_name
                && f.params.iter().any(|p| p.ty.contains("TxContext"))
                && fn_body_shares_object(&f.body_lines)
        })
        .collect::<Vec<_>>();
    candidates.sort_by_key(|f| f.params.len());
    let mut best: Option<(usize, SharedCreatorPlan)> = None;
    for f in candidates {
        let shared_type_keys =
            infer_shared_type_keys_from_fn_body(&f.body_lines, &d.module_name, key_structs_by_module);
        if !required_shared_type_keys.is_empty()
            && !required_shared_type_keys
                .iter()
                .all(|k| shared_type_keys.contains(k))
        {
            continue;
        }
        if let Some(ret_ty) = f.return_ty.as_ref() {
            if !should_transfer_call_return_with_keys(ret_ty, &d.module_name, key_structs_by_module)
                && shared_type_keys.is_empty()
            {
                continue;
            }
        } else if shared_type_keys.is_empty() {
            continue;
        }
        let Some(arg_plan) =
            synthesize_factory_args_with_refs(
                f,
                fn_lookup,
                key_structs_by_module,
                store_structs_by_module,
                coin_mint_amount,
            )
        else {
            continue;
        };
        let type_args = default_type_args_for_decl(f);
        let provided_cap_type_keys =
            infer_creator_provided_cap_type_keys(d, f, &type_args, module_fns);
        let plan = SharedCreatorPlan {
            fn_name: f.fn_name.clone(),
            args: arg_plan.args,
            type_args,
            return_ty: f.return_ty.clone(),
            shared_type_keys,
            provided_cap_type_keys,
            prep_lines: arg_plan.prep_lines,
            cleanup_lines: arg_plan.cleanup_lines,
        };
        let score = f.params.len();
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some((score, plan));
        }
    }
    let (_, best) = best?;
    Some(best)
}

fn upsert_object_need(
    object_needs: &mut Vec<ObjectNeed>,
    type_name: String,
    is_mut: bool,
    is_ref: bool,
) {
    let type_key = type_key_from_type_name(&type_name);
    if let Some(existing) = object_needs.iter_mut().find(|o| o.type_key == type_key) {
        existing.is_mut |= is_mut;
        existing.is_ref |= is_ref;
        return;
    }
    object_needs.push(ObjectNeed {
        type_name,
        type_key: type_key.clone(),
        var_name: format!("obj_dep_{}", sanitize_ident(&type_key)),
        is_mut,
        is_ref,
    });
}

