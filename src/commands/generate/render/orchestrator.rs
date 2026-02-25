use super::catalog::ModuleBootstrapCatalog;
use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ObjectProvisionSource {
    SharedHelper,
    SharedInit,
    SharedCreator,
    OwnedHelper,
    OwnedInit,
    OwnedFactory,
    OwnedCreatorSender,
}

#[derive(Debug, Clone)]
struct SharedCreatorPlan {
    fn_name: String,
    args: Vec<String>,
    type_args: Vec<String>,
    return_ty: Option<String>,
    shared_type_keys: std::collections::HashSet<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
}

#[derive(Debug, Clone)]
struct HelperCallPlan {
    fn_name: String,
    args: Vec<String>,
    type_args: Vec<String>,
}

#[derive(Debug, Clone)]
struct FactoryArgPlan {
    args: Vec<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
}

#[derive(Debug, Clone)]
struct FactoryCallPlan {
    fn_name: String,
    type_args: Vec<String>,
    args: Vec<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
    shared_ref_deps: Vec<(String, bool)>,
}

fn coin_var_names(needs: &[CoinNeed]) -> Vec<String> {
    needs.iter().map(|n| n.var_name.clone()).collect::<Vec<_>>()
}

fn cleanup_stmt_for_type(
    default_module: &str,
    ty: &str,
    var: &str,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> String {
    if is_known_key_struct(ty, default_module, key_structs_by_module) {
        let (decl_module, _) = split_type_module_and_base(ty, default_module);
        let destroy_name = format!("destroy_{}_for_testing", type_key_from_type_name(ty));
        if fn_lookup
            .get(decl_module)
            .and_then(|fns| fns.get(&destroy_name))
            .map(|f| f.is_test_only)
            .unwrap_or(false)
        {
            return format!("{}::{}({});", decl_module, destroy_name, var);
        }
    }
    format!("transfer::public_transfer({}, SUPER_USER);", var)
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
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    main_coin_vars: &[String],
    requires_extra_coin_slots: &mut std::collections::HashMap<usize, String>,
    pre_coin_counter: &mut usize,
    requires_cleanup_coin_vars: &mut Vec<String>,
    requires_cleanup_clock_vars: &mut Vec<String>,
    requires_consumed_coin_vars: &mut std::collections::HashSet<String>,
) -> Option<(Vec<String>, Vec<String>)> {
    let mut args = Vec::new();
    let mut prep = Vec::new();
    let mut coin_slot_idx = 0usize;
    let req_aliases = req_chain
        .get(req_step_idx)
        .map(|r| &r.module_use_aliases)
        .unwrap_or(&d.module_use_aliases);
    for p in &d.params {
        let t = p.ty.trim();
        if is_vector_type(t) {
            let vec_expr = vector_literal_expr_for_type(t)?;
            if t.starts_with('&') {
                let var = format!("pre_req_vec_{}_{}", req_step_idx, sanitize_ident(&p.name));
                let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                prep.push(format!("let {}{} = {};", maybe_mut, var, vec_expr));
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", var));
                } else {
                    args.push(format!("&{}", var));
                }
            } else {
                args.push(vec_expr);
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
                        "let {}{} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
                        maybe_mut, var, coin_ty
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
                        "let {} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
                        var, coin_ty
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
                        "let {} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
                        var, coin_ty
                    ));
                    args.push(var);
                }
                coin_slot_idx += 1;
            }
        } else if t == "u64" {
            args.push(choose_u64_arg_for_param_in_fn(&p.name, &d.body_lines));
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
) -> Option<Vec<String>> {
    let mut args = Vec::new();
    for p in &factory.params {
        let t = p.ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_coin_type(t) && !t.starts_with('&') {
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &factory.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            args.push(format!(
                "coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario))",
                coin_ty
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
) -> Option<FactoryArgPlan> {
    let mut args = Vec::new();
    let mut prep_lines = Vec::new();
    let mut cleanup_lines = Vec::new();
    for (idx, p) in factory.params.iter().enumerate() {
        let t = p.ty.trim();
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
                "let {}{} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
                maybe_mut, var, coin_ty
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
                ));
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
    module_fns: &std::collections::HashMap<String, FnDecl>,
    module_bootstrap: &ModuleBootstrapCatalog,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Option<FactoryCallPlan> {
    let mut args = Vec::new();
    let mut prep_lines = Vec::new();
    let mut cleanup_lines = Vec::new();
    let mut shared_ref_deps: Vec<(String, bool)> = Vec::new();
    for (idx, p) in factory.params.iter().enumerate() {
        let t = p.ty.trim();
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
                "let {}{} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
                maybe_mut, var, coin_ty
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
                let has_shared_helper = module_fns
                    .contains_key(&format!("create_and_share_{}_for_testing", type_key));
                let from_witness_init =
                    module_bootstrap.one_time_witness_init
                        && module_bootstrap.init_shared_types.contains(&type_key);
                if !has_shared_helper && !from_witness_init {
                    return None;
                }
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
    let type_args = if has_unbound_type_params(factory) {
        default_type_args_for_params(&factory.type_params)
    } else {
        Vec::new()
    };
    Some(FactoryCallPlan {
        fn_name: factory.fn_name.clone(),
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
    module_bootstrap: &ModuleBootstrapCatalog,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
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
        let ret = match &f.return_ty {
            Some(v) => v,
            None => continue,
        };
        if type_key_from_type_name(ret) != type_key {
            continue;
        }
        let Some(plan) = synthesize_owned_factory_call_plan(
            d,
            f,
            module_fns,
            module_bootstrap,
            fn_lookup,
            key_structs_by_module,
        ) else {
            continue;
        };
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
        let Some(args) = synthesize_factory_args(f, fn_lookup) else {
            continue;
        };
        let type_args = if has_unbound_type_params(f) {
            default_type_args_for_params(&f.type_params)
        } else {
            Vec::new()
        };
        let plan = HelperCallPlan {
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
        let Some(args) = synthesize_factory_args(f, fn_lookup) else {
            continue;
        };
        let type_args = if has_unbound_type_params(f) {
            default_type_args_for_params(&f.type_params)
        } else {
            Vec::new()
        };
        let plan = HelperCallPlan {
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
    required_cap_type_keys: &std::collections::BTreeSet<String>,
    required_shared_type_keys: &std::collections::BTreeSet<String>,
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
        if !required_cap_type_keys.is_empty() {
            let Some(ret_ty) = f.return_ty.as_ref() else {
                continue;
            };
            let ret_key = type_key_from_type_name(ret_ty);
            if !required_cap_type_keys.contains(&ret_key) {
                continue;
            }
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
        let Some(arg_plan) = synthesize_factory_args_with_refs(f, fn_lookup, key_structs_by_module)
        else {
            continue;
        };
        let type_args = if has_unbound_type_params(f) {
            default_type_args_for_params(&f.type_params)
        } else {
            Vec::new()
        };
        let plan = SharedCreatorPlan {
            fn_name: f.fn_name.clone(),
            args: arg_plan.args,
            type_args,
            return_ty: f.return_ty.clone(),
            shared_type_keys,
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

pub(super) fn render_best_effort_test(
    d: &FnDecl,
    inputs: &RenderInputs<'_>,
) -> Option<Vec<String>> {
    let accessor_map = inputs.accessor_map;
    let option_accessor_map = inputs.option_accessor_map;
    let container_accessor_map = inputs.container_accessor_map;
    let bootstrap_catalog = inputs.bootstrap_catalog;
    let fn_lookup = inputs.fn_lookup;
    let key_structs_by_module = inputs.key_structs_by_module;
    let numeric_effects = inputs.numeric_effects;
    let vector_effects = inputs.vector_effects;
    let coin_effects = inputs.coin_effects;
    let treasury_cap_effects = inputs.treasury_cap_effects;
    let coin_notes = inputs.coin_notes;
    let option_effects = inputs.option_effects;
    let string_effects = inputs.string_effects;
    let container_effects = inputs.container_effects;
    let deep_overflow_paths = inputs.deep_overflow_paths;
    if d.params
        .iter()
        .any(|p| p.name.to_ascii_lowercase().contains("public_inputs"))
    {
        return None;
    }
    if has_assert_on_mut_local(&d.body_lines, &d.params) {
        return None;
    }
    if requires_table_key_prestate(&d.body_lines) {
        return None;
    }
    let fq = format!("{}::{}", d.module_name, d.fn_name);
    let mut lines = Vec::new();
    lines.push("#[test]".to_string());
    lines.push(format!(
        "fun test_call_{}_{}() {{",
        d.module_name.split("::").last().unwrap_or("m"),
        d.fn_name
    ));
    lines.push("    let mut scenario = test_scenario::begin(SUPER_USER);".to_string());

    let mut args: Vec<String> = Vec::new();
    let mut param_runtime: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_coin_runtime: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_coin_is_ref: std::collections::HashMap<String, bool> =
        std::collections::HashMap::new();
    let mut param_arg_values: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut object_needs: Vec<ObjectNeed> = Vec::new();
    let mut coin_needs: Vec<CoinNeed> = Vec::new();
    let mut arg_setup_lines: Vec<String> = Vec::new();
    let mut clock_cleanup_vars: Vec<String> = Vec::new();
    let default_type_args = default_type_args_for_params(&d.type_params);
    let prefer_advanced_clock = has_clock_ge_guard(&d.body_lines);

    for param in &d.params {
        let resolved_ty = concretize_type_params(&param.ty, &d.type_params, &default_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_clock_type(t) {
            let var_name = format!("clock_{}", sanitize_ident(&param.name));
            if t.starts_with("&mut") {
                arg_setup_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var_name
                ));
                if prefer_advanced_clock {
                    arg_setup_lines.push(format!(
                        "sui::clock::set_for_testing(&mut {}, 1000000);",
                        var_name
                    ));
                }
                args.push(format!("&mut {}", var_name));
                clock_cleanup_vars.push(var_name);
            } else if t.starts_with('&') {
                if prefer_advanced_clock {
                    arg_setup_lines.push(format!(
                        "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                    arg_setup_lines.push(format!(
                        "sui::clock::set_for_testing(&mut {}, 1000000);",
                        var_name
                    ));
                } else {
                    arg_setup_lines.push(format!(
                        "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                }
                args.push(format!("&{}", var_name));
                clock_cleanup_vars.push(var_name);
            } else {
                if prefer_advanced_clock {
                    arg_setup_lines.push(format!(
                        "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                    arg_setup_lines.push(format!(
                        "sui::clock::set_for_testing(&mut {}, 1000000);",
                        var_name
                    ));
                } else {
                    arg_setup_lines.push(format!(
                        "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                }
                args.push(var_name);
            }
        } else if t == "address" {
            let v = "OTHER".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "u64" {
            let v = choose_u64_arg_for_param_in_fn(&param.name, &d.body_lines);
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if is_numeric_type(t) {
            let v = "1".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "bool" {
            let v = "false".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if is_id_type(t) {
            let v = default_id_arg_expr();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            let v = "std::string::utf8(b\"tidewalker\")".to_string();
            args.push(v);
        } else if is_vector_type(t) {
            let vec_expr = vector_literal_expr_for_type(t)?;
            if t.starts_with('&') {
                let var_name = format!("vec_{}", sanitize_ident(&param.name));
                let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                arg_setup_lines.push(format!("let {}{} = {};", maybe_mut, var_name, vec_expr));
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", var_name));
                } else {
                    args.push(format!("&{}", var_name));
                }
            } else {
                args.push(vec_expr);
            }
        } else if is_coin_type(t) {
            let var_name = format!("coin_{}", sanitize_ident(&param.name));
            let is_ref = t.starts_with('&');
            let arg = if t.starts_with("&mut") {
                format!("&mut {}", var_name)
            } else if t.starts_with('&') {
                format!("&{}", var_name)
            } else {
                var_name.clone()
            };
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &d.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            args.push(arg);
            param_coin_runtime.insert(param.name.clone(), var_name.clone());
            param_coin_is_ref.insert(param.name.clone(), is_ref);
            coin_needs.push(CoinNeed {
                var_name,
                coin_type: coin_ty,
                moved_on_main_call: !is_ref,
                needs_mut_binding: t.starts_with("&mut"),
            });
        } else {
            let should_try_constructor = {
                let norm = normalize_param_object_type(t);
                let avoid_constructor = is_cap_type(&norm)
                    || is_treasury_cap_type(&norm)
                    || is_known_key_struct(&norm, &d.module_name, key_structs_by_module);
                !avoid_constructor
            };
            if should_try_constructor {
                let expr =
                    synthesize_value_expr_for_type(t, &d.module_name, fn_lookup, Some(&fq), 0)?;
                if t.starts_with('&') {
                    let var_name = format!("value_{}", sanitize_ident(&param.name));
                    let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                    arg_setup_lines.push(format!("let {}{} = {};", maybe_mut, var_name, expr));
                    if t.starts_with("&mut") {
                        args.push(format!("&mut {}", var_name));
                    } else {
                        args.push(format!("&{}", var_name));
                    }
                } else {
                    args.push(expr);
                }
            } else if t.starts_with('&') {
                let obj = ObjectNeed::from_resolved(param, normalize_param_object_type(t));
                if obj.is_mut {
                    args.push(format!("&mut {}", obj.var_name));
                } else {
                    args.push(format!("&{}", obj.var_name));
                }
                param_runtime.insert(param.name.clone(), obj.var_name.clone());
                object_needs.push(obj);
            } else {
                let obj = ObjectNeed::from_resolved(param, normalize_param_object_type(t));
                args.push(obj.var_name.clone());
                param_runtime.insert(param.name.clone(), obj.var_name.clone());
                object_needs.push(obj);
            }
        }
    }
    let moved_object_vars_on_main_call: std::collections::HashSet<String> = object_needs
        .iter()
        .filter(|o| !o.is_ref)
        .map(|o| o.var_name.clone())
        .collect();

    let mut shared_objects: Vec<ObjectNeed> = Vec::new();
    let mut owned_objects: Vec<ObjectNeed> = Vec::new();
    let mut object_sources: std::collections::HashMap<String, ObjectProvisionSource> =
        std::collections::HashMap::new();
    let mut owned_helper_calls: std::collections::HashMap<String, HelperCallPlan> =
        std::collections::HashMap::new();
    let mut shared_helper_calls: std::collections::HashMap<String, HelperCallPlan> =
        std::collections::HashMap::new();
    let mut factory_calls: std::collections::HashMap<String, FactoryCallPlan> =
        std::collections::HashMap::new();
    let mut shared_creator_plan: Option<SharedCreatorPlan> = None;
    let mut creator_non_cap_type_keys: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    if !object_needs.is_empty() {
        let empty_bootstrap = ModuleBootstrapCatalog::default();
        let module_bootstrap = bootstrap_catalog
            .get(&d.module_name)
            .unwrap_or(&empty_bootstrap);
        let module_fns = fn_lookup.get(&d.module_name);
        let mut idx = 0usize;
        while idx < object_needs.len() {
            let obj = object_needs[idx].clone();
            let needs_shared_non_cap_object = object_needs
                .iter()
                .any(|o| o.is_ref && !is_cap_type(&o.type_name));
            let required_creator_cap_type_keys: std::collections::BTreeSet<String> = object_needs
                .iter()
                .filter(|o| o.is_ref && is_cap_type(&o.type_name))
                .map(|o| o.type_key.clone())
                .collect();
            let required_creator_shared_type_keys: std::collections::BTreeSet<String> = object_needs
                .iter()
                .filter(|o| o.is_ref && !is_cap_type(&o.type_name))
                .map(|o| o.type_key.clone())
                .collect();
            let shared_helper_plan = module_fns.and_then(|fns| {
                if obj.is_ref {
                    pick_shared_test_helper_for_type(d, &obj.type_key, fns, fn_lookup)
                } else {
                    None
                }
            });
            let owned_helper_plan = module_fns
                .and_then(|fns| pick_owned_test_helper_for_type(d, &obj.type_key, fns, fn_lookup));

            if let Some(plan) = shared_helper_plan {
                shared_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::SharedHelper);
                shared_helper_calls.insert(obj.var_name.clone(), plan);
            } else if let Some(plan) = owned_helper_plan {
                owned_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedHelper);
                owned_helper_calls.insert(obj.var_name.clone(), plan);
            } else if obj.is_ref
                && module_bootstrap.one_time_witness_init
                && module_bootstrap.init_shared_types.contains(&obj.type_key)
            {
                shared_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::SharedInit);
            } else if module_bootstrap.one_time_witness_init
                && module_bootstrap.init_owned_types.contains(&obj.type_key)
            {
                owned_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedInit);
            } else {
                let shared_helper = format!("create_and_share_{}_for_testing", obj.type_key);
                let owned_helper = format!("create_{}_for_testing", obj.type_key);
                let has_shared_helper = module_fns
                    .map(|fns| fns.contains_key(&shared_helper))
                    .unwrap_or(false);
                let has_owned_helper = module_fns
                    .map(|fns| fns.contains_key(&owned_helper))
                    .unwrap_or(false);
                if obj.is_ref && has_shared_helper {
                    shared_objects.push(obj.clone());
                    object_sources
                        .insert(obj.var_name.clone(), ObjectProvisionSource::SharedHelper);
                } else if has_owned_helper {
                    owned_objects.push(obj.clone());
                    object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedHelper);
                } else if obj.is_ref && is_cap_type(&obj.type_name) && needs_shared_non_cap_object {
                    if shared_creator_plan.is_none() {
                        shared_creator_plan = module_fns.and_then(|fns| {
                            pick_shared_creator_plan(
                                d,
                                fns,
                                fn_lookup,
                                key_structs_by_module,
                                &required_creator_cap_type_keys,
                                &required_creator_shared_type_keys,
                            )
                        });
                    }
                    if shared_creator_plan.is_some() {
                        owned_objects.push(obj.clone());
                        object_sources.insert(
                            obj.var_name.clone(),
                            ObjectProvisionSource::OwnedCreatorSender,
                        );
                    } else {
                        return None;
                    }
                } else if let Some(plan) = module_fns.and_then(|fns| {
                    pick_factory_call_for_type(
                        d,
                        &obj.type_key,
                        fns,
                        module_bootstrap,
                        fn_lookup,
                        key_structs_by_module,
                    )
                }) {
                    for (dep_ty, dep_mut) in &plan.shared_ref_deps {
                        upsert_object_need(&mut object_needs, dep_ty.clone(), *dep_mut, true);
                    }
                    owned_objects.push(obj.clone());
                    object_sources
                        .insert(obj.var_name.clone(), ObjectProvisionSource::OwnedFactory);
                    factory_calls.insert(obj.var_name.clone(), plan);
                } else {
                    if shared_creator_plan.is_none() {
                        shared_creator_plan = module_fns.and_then(|fns| {
                            pick_shared_creator_plan(
                                d,
                                fns,
                                fn_lookup,
                                key_structs_by_module,
                                &required_creator_cap_type_keys,
                                &required_creator_shared_type_keys,
                            )
                        });
                    }
                    if obj.is_ref && shared_creator_plan.is_some() {
                        if is_cap_type(&obj.type_name) {
                            owned_objects.push(obj.clone());
                            object_sources.insert(
                                obj.var_name.clone(),
                                ObjectProvisionSource::OwnedCreatorSender,
                            );
                        } else {
                            let creator_supports_type = shared_creator_plan
                                .as_ref()
                                .map(|p| p.shared_type_keys.contains(&obj.type_key))
                                .unwrap_or(false);
                            if !creator_supports_type {
                                return None;
                            }
                            creator_non_cap_type_keys.insert(obj.type_key.clone());
                            if creator_non_cap_type_keys.len() > 1 {
                                // A single create_shared_* bootstrap can safely seed only one
                                // concrete shared object type in first-pass generation.
                                return None;
                            }
                            shared_objects.push(obj.clone());
                            object_sources
                                .insert(obj.var_name.clone(), ObjectProvisionSource::SharedCreator);
                        }
                    } else {
                        return None;
                    }
                }
            }
            idx += 1;
        }
    }
    let needs_init_bootstrap = object_sources.values().any(|src| {
        matches!(
            src,
            ObjectProvisionSource::SharedInit | ObjectProvisionSource::OwnedInit
        )
    });
    let init_bootstrap_helper_name = fn_lookup
        .get(&d.module_name)
        .and_then(pick_init_bootstrap_helper_name);
    if needs_init_bootstrap && init_bootstrap_helper_name.is_none() {
        return None;
    }

    let has_shared_objects = !shared_objects.is_empty();
    let preexisting_shared_type_keys: std::collections::HashSet<String> =
        shared_objects.iter().map(|o| o.type_key.clone()).collect();

    let mut needs_next_tx_before_use = false;
    if has_shared_objects {
        let helper_shared = shared_objects
            .iter()
            .filter(|obj| {
                matches!(
                    object_sources.get(&obj.var_name),
                    Some(ObjectProvisionSource::SharedHelper)
                )
            })
            .collect::<Vec<_>>();
        if !helper_shared.is_empty() {
            lines.push("    {".to_string());
            for obj in helper_shared {
                let plan = shared_helper_calls.get(&obj.var_name)?;
                let call_path = if plan.type_args.is_empty() {
                    format!("{}::{}", d.module_name, plan.fn_name)
                } else {
                    format!(
                        "{}::{}<{}>",
                        d.module_name,
                        plan.fn_name,
                        plan.type_args.join(", ")
                    )
                };
                lines.push(format!("        {}({});", call_path, plan.args.join(", ")));
            }
            lines.push("    };".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if object_sources.values().any(|src| {
        matches!(
            src,
            ObjectProvisionSource::SharedCreator | ObjectProvisionSource::OwnedCreatorSender
        )
    }) {
        if let Some(plan) = &shared_creator_plan {
            lines.push("    {".to_string());
            for prep in &plan.prep_lines {
                lines.push(format!("        {}", prep));
            }
            let call_path = if plan.type_args.is_empty() {
                format!("{}::{}", d.module_name, plan.fn_name)
            } else {
                format!(
                    "{}::{}<{}>",
                    d.module_name,
                    plan.fn_name,
                    plan.type_args.join(", ")
                )
            };
            let call_expr = format!("{}({})", call_path, plan.args.join(", "));
            if let Some(ret_ty) = plan.return_ty.as_ref() {
                if should_transfer_call_return_with_keys(
                    ret_ty,
                    &d.module_name,
                    key_structs_by_module,
                ) {
                    lines.push(format!("        let seed_ret_obj = {};", call_expr));
                    let seed_cleanup = cleanup_stmt_for_type(
                        &d.module_name,
                        ret_ty,
                        "seed_ret_obj",
                        fn_lookup,
                        key_structs_by_module,
                    );
                    lines.push(format!("        {}", seed_cleanup));
                } else {
                    lines.push(format!("        {};", call_expr));
                }
            } else {
                lines.push(format!("        {};", call_expr));
            }
            for cleanup in &plan.cleanup_lines {
                lines.push(format!("        {}", cleanup));
            }
            lines.push("    };".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if needs_init_bootstrap {
        lines.push("    {".to_string());
        let bootstrap_name = init_bootstrap_helper_name?;
        lines.push(format!(
            "        {}::{}(test_scenario::ctx(&mut scenario));",
            d.module_name, bootstrap_name
        ));
        lines.push("    };".to_string());
        needs_next_tx_before_use = true;
    }
    if needs_next_tx_before_use {
        lines.push("    test_scenario::next_tx(&mut scenario, SUPER_USER);".to_string());
    }

    let mut object_vars_by_type: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for obj in &object_needs {
        object_vars_by_type
            .entry(obj.type_key.clone())
            .or_insert_with(|| obj.var_name.clone());
    }
    let coin_vars_for_calls = coin_var_names(&coin_needs);
    let mut pre_coin_counter = 0usize;
    let mut requires_cleanup_coin_vars: Vec<String> = Vec::new();
    let mut requires_cleanup_clock_vars: Vec<String> = Vec::new();
    let mut requires_consumed_coin_vars: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut requires_extra_coin_slots: std::collections::HashMap<usize, String> =
        std::collections::HashMap::new();
    let mut requires_chain: Vec<&FnDecl> = Vec::new();
    if let Some(module_fns) = fn_lookup.get(&d.module_name) {
        for req in &d.requires {
            if let Some(req_decl) = module_fns.get(req) {
                requires_chain.push(req_decl);
            }
        }
    }
    if !requires_chain.is_empty() {
        let mut requires_mut_type_keys: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for req_decl in &requires_chain {
            for p in &req_decl.params {
                let t = p.ty.trim();
                if !t.starts_with("&mut")
                    || t.contains("TxContext")
                    || is_clock_type(t)
                    || is_vector_type(t)
                    || is_coin_type(t)
                {
                    continue;
                }
                let key = type_key_from_type_name(&normalize_param_object_type(t));
                requires_mut_type_keys.insert(key);
            }
        }
        if !requires_mut_type_keys.is_empty() {
            for obj in &mut object_needs {
                if requires_mut_type_keys.contains(&obj.type_key) {
                    obj.is_mut = true;
                }
            }
            for obj in &mut shared_objects {
                if requires_mut_type_keys.contains(&obj.type_key) {
                    obj.is_mut = true;
                }
            }
            for obj in &mut owned_objects {
                if requires_mut_type_keys.contains(&obj.type_key) {
                    obj.is_mut = true;
                }
            }
        }
    }

    let (snapshot_before_lines, snapshot_after_lines, mut numeric_summary) =
        build_numeric_assertion_lines(
            d,
            numeric_effects,
            &param_runtime,
            &param_arg_values,
            accessor_map,
            deep_overflow_paths,
        );
    let (vector_before_lines, vector_after_lines, vector_summary) = build_vector_assertion_lines(
        d,
        vector_effects,
        &param_runtime,
        accessor_map,
        deep_overflow_paths,
    );
    let (coin_before_lines, coin_after_lines, coin_summary) = build_coin_assertion_lines(
        d,
        coin_effects,
        &param_coin_runtime,
        &param_coin_is_ref,
        &param_arg_values,
        deep_overflow_paths,
    );
    let (treasury_before_lines, treasury_after_lines, treasury_summary) =
        build_treasury_cap_assertion_lines(
            d,
            treasury_cap_effects,
            &param_runtime,
            &param_arg_values,
            deep_overflow_paths,
        );
    let (option_before_lines, option_after_lines, option_summary) = build_option_assertion_lines(
        d,
        option_effects,
        &param_runtime,
        option_accessor_map,
        deep_overflow_paths,
    );
    let (container_before_lines, container_after_lines, container_summary) =
        build_container_assertion_lines(
            d,
            container_effects,
            &param_runtime,
            &param_arg_values,
            container_accessor_map,
            deep_overflow_paths,
        );
    let (ownership_transfer_checks, ownership_share_checks, ownership_summary) =
        build_ownership_checks(d, &param_arg_values, &preexisting_shared_type_keys);
    let cap_auth_summary = build_cap_auth_summary(d);
    let string_summary = build_string_summary(string_effects, &param_runtime, deep_overflow_paths);
    for eff in string_effects {
        let operator_target = format!("operator {}.{}", eff.base_var, eff.field);
        numeric_summary.asserted.remove(&operator_target);
        numeric_summary.potential.remove(&operator_target);
    }
    let mut state_summary = StateChangeSummary::default();
    state_summary.merge(numeric_summary);
    state_summary.merge(vector_summary);
    state_summary.merge(coin_summary);
    state_summary.merge(treasury_summary);
    state_summary.merge(build_coin_note_summary(coin_notes));
    state_summary.merge(cap_auth_summary);
    state_summary.merge(ownership_summary);
    state_summary.merge(option_summary);
    state_summary.merge(container_summary);
    state_summary.merge(string_summary);
    state_summary.merge(build_deep_chain_summary(deep_overflow_paths));
    state_summary.merge(build_mut_param_fallback_summary(d, &state_summary));
    let summary_lines = render_state_change_summary_lines(&state_summary);

    lines.push("    {".to_string());
    for coin in &coin_needs {
        let maybe_mut = if coin.needs_mut_binding { "mut " } else { "" };
        lines.push(format!(
            "        let {}{} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
            maybe_mut,
            coin.var_name,
            coin.coin_type
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
    for obj in &owned_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        match object_sources.get(&obj.var_name) {
            Some(ObjectProvisionSource::OwnedHelper) => {
                let plan = owned_helper_calls.get(&obj.var_name)?;
                let call_path = if plan.type_args.is_empty() {
                    format!("{}::{}", d.module_name, plan.fn_name)
                } else {
                    format!(
                        "{}::{}<{}>",
                        d.module_name,
                        plan.fn_name,
                        plan.type_args.join(", ")
                    )
                };
                lines.push(format!(
                    "        let {}{} = {}({});",
                    maybe_mut,
                    obj.var_name,
                    call_path,
                    plan.args.join(", ")
                ));
            }
            Some(ObjectProvisionSource::OwnedInit) => {
                lines.push(format!(
                    "        let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
                    maybe_mut,
                    obj.var_name,
                    qualify_type_for_module(&d.module_name, &obj.type_name)
                ));
            }
            Some(ObjectProvisionSource::OwnedFactory) => {
                let plan = factory_calls.get(&obj.var_name)?;
                for l in &plan.prep_lines {
                    lines.push(format!("        {}", l));
                }
                let mut resolved_args: Vec<String> = Vec::new();
                for arg in &plan.args {
                    let mut resolved = arg.clone();
                    for (dep_ty, _) in &plan.shared_ref_deps {
                        let dep_key = type_key_from_type_name(dep_ty);
                        let token =
                            format!("__TW_OBJ_BY_TYPE_{}__", sanitize_ident(&dep_key));
                        if resolved.contains(&token) {
                            let dep_var = object_vars_by_type.get(&dep_key)?;
                            resolved = resolved.replace(&token, dep_var);
                        }
                    }
                    resolved_args.push(resolved);
                }
                let call_path = if plan.type_args.is_empty() {
                    format!("{}::{}", d.module_name, plan.fn_name)
                } else {
                    format!(
                        "{}::{}<{}>",
                        d.module_name,
                        plan.fn_name,
                        plan.type_args.join(", ")
                    )
                };
                lines.push(format!(
                    "        let {}{} = {}({});",
                    maybe_mut,
                    obj.var_name,
                    call_path,
                    resolved_args.join(", ")
                ));
                for l in &plan.cleanup_lines {
                    lines.push(format!("        {}", l));
                }
            }
            Some(ObjectProvisionSource::OwnedCreatorSender) => {
                lines.push(format!(
                    "        let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
                    maybe_mut,
                    obj.var_name,
                    qualify_type_for_module(&d.module_name, &obj.type_name)
                ));
            }
            _ => {
                lines.push(format!(
                    "        let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
                    maybe_mut, obj.var_name, d.module_name, obj.type_key
                ));
            }
        }
    }
    for l in &arg_setup_lines {
        lines.push(format!("        {}", l));
    }

    for (req_idx, req_decl) in requires_chain.iter().enumerate() {
        if let Some((req_args, pre_lines)) = synthesize_requires_call_args(
            req_decl,
            req_idx,
            &requires_chain,
            &object_vars_by_type,
            fn_lookup,
            &coin_vars_for_calls,
            &mut requires_extra_coin_slots,
            &mut pre_coin_counter,
            &mut requires_cleanup_coin_vars,
            &mut requires_cleanup_clock_vars,
            &mut requires_consumed_coin_vars,
        ) {
            for l in pre_lines {
                lines.push(format!("        {}", l));
            }
            lines.push(format!(
                "        {}::{}({});",
                d.module_name,
                req_decl.fn_name,
                req_args.join(", ")
            ));
        }
    }
    let auto_prestate_lines = synthesize_auto_prestate_calls(
        d,
        fn_lookup,
        &object_vars_by_type,
        &coin_vars_for_calls,
        &mut requires_extra_coin_slots,
        &mut pre_coin_counter,
        &mut requires_cleanup_coin_vars,
        &mut requires_cleanup_clock_vars,
        &mut requires_consumed_coin_vars,
        &param_arg_values,
    );
    for l in auto_prestate_lines {
        lines.push(format!("        {}", l));
    }
    for l in snapshot_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in vector_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in coin_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in treasury_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in option_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in container_before_lines {
        lines.push(format!("        {}", l));
    }
    let call_target = if has_unbound_type_params(d) {
        if default_type_args.is_empty() {
            fq.clone()
        } else {
            format!("{}<{}>", fq, default_type_args.join(", "))
        }
    } else {
        fq.clone()
    };
    let call_expr = format!("{}({})", call_target, args.join(", "));
    if let Some(ret_ty) = d.return_ty.as_ref() {
        if should_transfer_call_return_with_keys(ret_ty, &d.module_name, key_structs_by_module) {
            lines.push(format!("        let tw_ret_obj = {};", call_expr));
            let ret_cleanup = cleanup_stmt_for_type(
                &d.module_name,
                ret_ty,
                "tw_ret_obj",
                fn_lookup,
                key_structs_by_module,
            );
            lines.push(format!("        {}", ret_cleanup));
        } else {
            lines.push(format!("        {};", call_expr));
        }
    } else {
        lines.push(format!("        {};", call_expr));
    }
    for l in snapshot_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in vector_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in coin_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in treasury_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in option_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in container_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in summary_lines {
        lines.push(format!("        {}", l));
    }
    for obj in &shared_objects {
        lines.push(format!(
            "        test_scenario::return_shared({});",
            obj.var_name
        ));
    }
    let mut seen_req_coin_cleanup: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for var in &requires_cleanup_coin_vars {
        if requires_consumed_coin_vars.contains(var) {
            continue;
        }
        if seen_req_coin_cleanup.insert(var.clone()) {
            lines.push(format!(
                "        transfer::public_transfer({}, SUPER_USER);",
                var
            ));
        }
    }
    let mut seen_req_clock_cleanup: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for var in &requires_cleanup_clock_vars {
        if seen_req_clock_cleanup.insert(var.clone()) {
            lines.push(format!("        sui::clock::destroy_for_testing({});", var));
        }
    }
    for obj in &owned_objects {
        if moved_object_vars_on_main_call.contains(&obj.var_name) {
            continue;
        }
        if is_coin_type(&obj.type_name)
            || is_treasury_cap_type(&obj.type_name)
            || is_known_key_struct(&obj.type_name, &d.module_name, key_structs_by_module)
        {
            let cleanup_stmt = cleanup_stmt_for_type(
                &d.module_name,
                &obj.type_name,
                &obj.var_name,
                fn_lookup,
                key_structs_by_module,
            );
            lines.push(format!("        {}", cleanup_stmt));
        }
    }
    for coin in &coin_needs {
        if !coin.moved_on_main_call {
            lines.push(format!(
                "        transfer::public_transfer({}, SUPER_USER);",
                coin.var_name
            ));
        }
    }
    let mut seen_clock_cleanup: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for var in &clock_cleanup_vars {
        if seen_clock_cleanup.insert(var.clone()) {
            lines.push(format!("        sui::clock::destroy_for_testing({});", var));
        }
    }
    lines.push("    };".to_string());
    for (idx, check) in ownership_transfer_checks.iter().enumerate() {
        lines.push(format!(
            "    test_scenario::next_tx(&mut scenario, {});",
            check.recipient
        ));
        lines.push("    {".to_string());
        lines.push(format!(
            "        let moved_obj_{} = test_scenario::take_from_sender<{}>(&scenario);",
            idx, check.object_type
        ));
        lines.push(format!(
            "        test_scenario::return_to_sender(&scenario, moved_obj_{});",
            idx
        ));
        lines.push("    };".to_string());
    }
    for (idx, check) in ownership_share_checks.iter().enumerate() {
        lines.push("    test_scenario::next_tx(&mut scenario, SUPER_USER);".to_string());
        lines.push("    {".to_string());
        lines.push(format!(
            "        let shared_obj_{} = scenario.take_shared<{}>();",
            idx, check.object_type
        ));
        lines.push(format!(
            "        test_scenario::return_shared(shared_obj_{});",
            idx
        ));
        lines.push("    };".to_string());
    }

    // Ensure scenario is closed.
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
}
