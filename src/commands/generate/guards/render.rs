fn render_cap_role_guard_test(
    d: &FnDecl,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    case_idx: usize,
    cap_param: &str,
    cap_type: &str,
) -> Option<Vec<String>> {
    let module_fns = fn_lookup.get(&d.module_name);
    let empty_helpers = ModuleHelperCatalog::default();
    let module_helpers = helper_catalog.get(&d.module_name).unwrap_or(&empty_helpers);
    let empty_bootstrap = ModuleBootstrapCatalog::default();
    let module_bootstrap = bootstrap_catalog
        .get(&d.module_name)
        .unwrap_or(&empty_bootstrap);
    let init_bootstrap_helper_name = fn_lookup
        .get(&d.module_name)
        .and_then(pick_init_bootstrap_helper_name);
    let cap_type_key = type_key_from_type_name(cap_type);
    let use_helper_path = module_helpers.owned_types.contains(&cap_type_key)
        || module_helpers.shared_types.contains(&cap_type_key);
    let use_bootstrap_path = module_bootstrap.one_time_witness_init
        && module_bootstrap.init_owned_types.contains(&cap_type_key)
        && init_bootstrap_helper_name.is_some();
    let creator_plan = if !use_helper_path && !use_bootstrap_path {
        module_fns.and_then(|fns| {
            let required_cap_type_keys = std::collections::BTreeSet::from([cap_type_key.clone()]);
            let required_shared_type_keys = std::collections::BTreeSet::new();
            pick_guard_shared_creator_plan(
                d,
                fns,
                fn_lookup,
                key_structs_by_module,
                store_structs_by_module,
                &required_cap_type_keys,
                &required_shared_type_keys,
            )
        })
    } else {
        None
    };
    if !use_helper_path && !use_bootstrap_path && creator_plan.is_none() {
        return None;
    }

    let module_short = d.module_name.split("::").last().unwrap_or("m");
    let mut lines = Vec::new();
    lines.push("#[test]".to_string());
    lines.push("#[expected_failure]".to_string());
    lines.push(format!(
        "fun test_abort_{}_{}_role_{}_{}() {{",
        module_short,
        d.fn_name,
        sanitize_ident(cap_param),
        case_idx
    ));
    lines.push("    let mut scenario = test_scenario::begin(SUPER_USER);".to_string());
    if use_helper_path {
        lines.push("    {".to_string());
        lines.push(format!(
            "        let cap_obj = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
            d.module_name, cap_type_key
        ));
        lines.push("        transfer::public_transfer(cap_obj, SUPER_USER);".to_string());
        lines.push("    };".to_string());
    } else if use_bootstrap_path {
        let bootstrap_name = init_bootstrap_helper_name?;
        lines.push("    {".to_string());
        lines.push(format!(
            "        {}::{}(test_scenario::ctx(&mut scenario));",
            d.module_name, bootstrap_name
        ));
        lines.push("    };".to_string());
    } else if let Some(plan) = creator_plan {
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
            if should_transfer_call_return_with_keys(ret_ty, &d.module_name, key_structs_by_module)
            {
                lines.push(format!("        let seed_ret_obj = {};", call_expr));
                let seed_cleanup = cleanup_stmt_for_type(
                    &d.module_name,
                    ret_ty,
                    "seed_ret_obj",
                    fn_lookup,
                    key_structs_by_module,
                    store_structs_by_module,
                )?;
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
    }
    lines.push("    test_scenario::next_tx(&mut scenario, OTHER);".to_string());
    lines.push("    {".to_string());
    lines.push(format!(
        "        let missing_cap = test_scenario::take_from_sender<{}>(&scenario);",
        qualify_type_for_module(&d.module_name, cap_type)
    ));
    lines.push("        test_scenario::return_to_sender(&scenario, missing_cap);".to_string());
    lines.push(format!(
        "        // expected to fail before this line because OTHER should not own {}",
        qualify_type_for_module(&d.module_name, cap_type)
    ));
    lines.push("    };".to_string());
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
}

fn render_param_guard_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    target_param: &str,
    prep_lines: &[String],
    bad_expr: &str,
) -> Option<Vec<String>> {
    let (setup_lines, mut call_args, cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let param_idx = d.params.iter().position(|p| p.name == target_param)?;
    call_args[param_idx] = bad_expr.to_string();

    render_guard_call_test(
        d,
        env,
        case_idx,
        &setup_lines,
        prep_lines,
        &call_args,
        &cleanup,
    )
}

fn render_coin_value_guard_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    coin_param: &str,
    op: &str,
    rhs_literal: &str,
) -> Option<Vec<String>> {
    let bad_amount = violating_const_arg(op, rhs_literal)?;
    let _ = parse_numeric_literal(&bad_amount)?;
    let (mut setup_lines, mut call_args, mut cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let param_idx = d.params.iter().position(|p| p.name == coin_param)?;
    let param_ty = d.params.get(param_idx)?.ty.trim();
    if !is_coin_type(param_ty) {
        return None;
    }
    let default_coin_var = format!("coin_{}", sanitize_ident(coin_param));
    setup_lines
        .retain(|l| !(l.contains("coin::mint_for_testing") && l.contains(&default_coin_var)));
    cleanup.retain(|l| !l.contains(&format!("{}{}", default_coin_var, ",")));

    let bad_coin_var = format!("guard_bad_coin_{}", case_idx);
    let mut prep = Vec::new();
    let maybe_mut = if param_ty.starts_with("&mut") {
        "mut "
    } else {
        ""
    };
    prep.push(format!(
        "let {}{} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
        maybe_mut,
        bad_coin_var,
        coin_type_tag_from_coin_type_with_aliases(param_ty, &d.module_use_aliases),
        bad_amount
    ));
    if !is_qualified_type_tag(&coin_type_tag_from_coin_type_with_aliases(
        param_ty,
        &d.module_use_aliases,
    )) {
        return None;
    }
    call_args[param_idx] = if param_ty.starts_with("&mut") {
        format!("&mut {}", bad_coin_var)
    } else if param_ty.starts_with('&') {
        format!("&{}", bad_coin_var)
    } else {
        bad_coin_var.clone()
    };
    if param_ty.starts_with('&') {
        cleanup.push(format!(
            "transfer::public_transfer({}, SUPER_USER);",
            bad_coin_var
        ));
    }
    render_guard_call_test(d, env, case_idx, &setup_lines, &prep, &call_args, &cleanup)
}

fn render_guard_call_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    setup_lines: &[String],
    prep_lines: &[String],
    call_args: &[String],
    cleanup: &[String],
) -> Option<Vec<String>> {
    if call_args.is_empty() && !d.params.is_empty() {
        return None;
    }

    let fq = module_fn_label(d);
    let module_short = d.module_name.split("::").last().unwrap_or("m");
    let mut lines = Vec::new();
    lines.push("#[test]".to_string());
    lines.push("#[expected_failure]".to_string());
    lines.push(format!(
        "fun test_abort_{}_{}_guard_{}() {{",
        module_short, d.fn_name, case_idx
    ));
    lines.push("    let mut scenario = test_scenario::begin(SUPER_USER);".to_string());
    for l in setup_lines {
        lines.push(format!("    {}", l));
    }
    lines.push("    {".to_string());
    for l in prep_lines {
        lines.push(format!("        {}", l));
    }
    let default_type_args = default_type_args_for_params(&d.type_params);
    let call_target = if has_unbound_type_params(d) {
        if default_type_args.is_empty() {
            fq.clone()
        } else {
            format!("{}<{}>", fq, default_type_args.join(", "))
        }
    } else {
        fq.clone()
    };
    let call_expr = format!("{}({})", call_target, call_args.join(", "));
    if let Some(ret_ty) = d.return_ty.as_ref() {
        if should_transfer_call_return_with_keys(ret_ty, &d.module_name, env.key_structs_by_module)
        {
            lines.push(format!("        let tw_ret_obj = {};", call_expr));
            let ret_cleanup = cleanup_stmt_for_type(
                &d.module_name,
                ret_ty,
                "tw_ret_obj",
                env.fn_lookup,
                env.key_structs_by_module,
                env.store_structs_by_module,
            )?;
            lines.push(format!("        {}", ret_cleanup));
        } else {
            lines.push(format!("        {};", call_expr));
        }
    } else {
        lines.push(format!("        {};", call_expr));
    }
    for l in cleanup {
        lines.push(format!("        {}", l));
    }
    lines.push("    };".to_string());
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
}

fn vector_inner_elem_literal(inner: &str) -> Option<String> {
    let t = inner.trim().replace(' ', "");
    match t.as_str() {
        "u8" => Some("0u8".to_string()),
        "u16" => Some("1u16".to_string()),
        "u32" => Some("1u32".to_string()),
        "u64" => Some("1u64".to_string()),
        "u128" => Some("1u128".to_string()),
        "u256" => Some("1u256".to_string()),
        "bool" => Some("false".to_string()),
        "address" => Some("SUPER_USER".to_string()),
        _ if t == "ID" || t.ends_with("::ID") => Some(default_id_arg_expr()),
        _ => None,
    }
}

fn build_vector_with_len_prep(var: &str, ty: &str, len: usize) -> Option<Vec<String>> {
    let norm = normalize_param_object_type(ty).replace(' ', "");
    let inner = extract_vector_inner_type(&norm)?;
    let elem = vector_inner_elem_literal(&inner)?;
    let mut out = Vec::new();
    out.push(format!("let mut {}: {} = vector[];", var, norm));
    for _ in 0..len {
        out.push(format!("std::vector::push_back(&mut {}, {});", var, elem));
    }
    Some(out)
}

fn apply_param_arg(call_args: &mut [String], idx: usize, param_ty: &str, var_name: &str) {
    call_args[idx] = if param_ty.starts_with("&mut") {
        format!("&mut {}", var_name)
    } else if param_ty.starts_with('&') {
        format!("&{}", var_name)
    } else {
        var_name.to_string()
    };
}

fn violating_vector_len(op: &str, rhs_literal: &str) -> Option<usize> {
    let rhs = parse_numeric_literal(rhs_literal)?
        .replace('_', "")
        .parse::<usize>()
        .ok()?;
    match op {
        ">" | "<" | "!=" => Some(rhs),
        "==" | "<=" => rhs.checked_add(1),
        ">=" => rhs.checked_sub(1),
        _ => None,
    }
}

fn violating_vector_len_pair(op: &str) -> Option<(usize, usize)> {
    match op {
        "==" => Some((0, 1)),
        "!=" => Some((1, 1)),
        ">" => Some((0, 1)),
        ">=" => Some((0, 1)),
        "<" => Some((1, 0)),
        "<=" => Some((1, 0)),
        _ => None,
    }
}

fn render_vector_len_const_guard_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    param_name: &str,
    op: &str,
    rhs_literal: &str,
) -> Option<Vec<String>> {
    let bad_len = violating_vector_len(op, rhs_literal)?;
    let (setup_lines, mut call_args, cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let param_idx = d.params.iter().position(|p| p.name == param_name)?;
    let param_ty = d.params.get(param_idx)?.ty.trim().to_string();
    if !is_vector_type(&param_ty) {
        return None;
    }
    let vec_var = format!("guard_bad_vec_{}", case_idx);
    let prep = build_vector_with_len_prep(&vec_var, &param_ty, bad_len)?;
    apply_param_arg(&mut call_args, param_idx, &param_ty, &vec_var);
    render_guard_call_test(d, env, case_idx, &setup_lines, &prep, &call_args, &cleanup)
}

fn render_vector_len_param_guard_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    lhs_param: &str,
    rhs_param: &str,
    op: &str,
) -> Option<Vec<String>> {
    let (lhs_len, rhs_len) = violating_vector_len_pair(op)?;
    let (setup_lines, mut call_args, cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let lhs_idx = d.params.iter().position(|p| p.name == lhs_param)?;
    let rhs_idx = d.params.iter().position(|p| p.name == rhs_param)?;
    let lhs_ty = d.params.get(lhs_idx)?.ty.trim().to_string();
    let rhs_ty = d.params.get(rhs_idx)?.ty.trim().to_string();
    if !is_vector_type(&lhs_ty) || !is_vector_type(&rhs_ty) {
        return None;
    }
    let lhs_var = format!("guard_bad_vec_lhs_{}", case_idx);
    let rhs_var = format!("guard_bad_vec_rhs_{}", case_idx);
    let mut prep = build_vector_with_len_prep(&lhs_var, &lhs_ty, lhs_len)?;
    prep.extend(build_vector_with_len_prep(&rhs_var, &rhs_ty, rhs_len)?);
    apply_param_arg(&mut call_args, lhs_idx, &lhs_ty, &lhs_var);
    apply_param_arg(&mut call_args, rhs_idx, &rhs_ty, &rhs_var);
    render_guard_call_test(d, env, case_idx, &setup_lines, &prep, &call_args, &cleanup)
}

fn extract_helper_field_literal(helper: &FnDecl, field_name: &str) -> Option<String> {
    for line in &helper.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        let mut candidate = stmt.trim_end_matches(';').trim();
        if let Some((_, rhs)) = candidate.split_once('=') {
            candidate = rhs.trim();
        }
        if let Some((lhs, rhs_raw)) = candidate.split_once(':') {
            if lhs.trim() != field_name {
                continue;
            }
            let rhs = rhs_raw.trim().trim_end_matches(',').trim();
            if let Some(v) = parse_numeric_literal(rhs) {
                return Some(v);
            }
            if is_bool_literal(rhs) || is_address_literal(rhs) {
                return Some(rhs.to_string());
            }
        }
    }
    None
}

fn lookup_helper_field_literal(
    module_fns: &std::collections::HashMap<String, FnDecl>,
    type_key: &str,
    field_name: &str,
) -> Option<String> {
    let shared_helper = format!("create_and_share_{}_for_testing", type_key);
    let owned_helper = format!("create_{}_for_testing", type_key);
    if let Some(f) = module_fns.get(&shared_helper) {
        if let Some(v) = extract_helper_field_literal(f, field_name) {
            return Some(v);
        }
    }
    if let Some(f) = module_fns.get(&owned_helper) {
        if let Some(v) = extract_helper_field_literal(f, field_name) {
            return Some(v);
        }
    }
    None
}

fn literal_compare(lhs: &str, op: &str, rhs: &str) -> Option<bool> {
    if let (Some(lhs_n), Some(rhs_n)) = (parse_numeric_literal(lhs), parse_numeric_literal(rhs)) {
        let lhs_v: u128 = lhs_n.replace('_', "").parse().ok()?;
        let rhs_v: u128 = rhs_n.replace('_', "").parse().ok()?;
        return match op {
            ">" => Some(lhs_v > rhs_v),
            "<" => Some(lhs_v < rhs_v),
            ">=" => Some(lhs_v >= rhs_v),
            "<=" => Some(lhs_v <= rhs_v),
            "==" => Some(lhs_v == rhs_v),
            "!=" => Some(lhs_v != rhs_v),
            _ => None,
        };
    }
    if is_bool_literal(lhs) && is_bool_literal(rhs) {
        let lhs_v = lhs.trim() == "true";
        let rhs_v = rhs.trim() == "true";
        return match op {
            "==" => Some(lhs_v == rhs_v),
            "!=" => Some(lhs_v != rhs_v),
            _ => None,
        };
    }
    if is_address_literal(lhs) && is_address_literal(rhs) {
        let lhs_v = lhs.trim();
        let rhs_v = rhs.trim();
        return match op {
            "==" => Some(lhs_v == rhs_v),
            "!=" => Some(lhs_v != rhs_v),
            _ => None,
        };
    }
    None
}

fn render_object_field_const_guard_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    param_name: &str,
    field_name: &str,
    op: &str,
    rhs_literal: &str,
) -> Option<Vec<String>> {
    let (setup_lines, call_args, cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let param = d.params.iter().find(|p| p.name == param_name)?;
    let type_name = normalize_param_object_type(&param.ty);
    let type_key = type_key_from_type_name(&type_name);
    let module_fns = env.fn_lookup.get(&d.module_name)?;
    let helper_default = lookup_helper_field_literal(module_fns, &type_key, field_name)?;
    let guard_holds = literal_compare(&helper_default, op, rhs_literal)?;
    if guard_holds {
        return None;
    }
    render_guard_call_test(d, env, case_idx, &setup_lines, &[], &call_args, &cleanup)
}

fn render_object_field_sender_guard_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    param_name: &str,
    _field_name: &str,
    op: &str,
) -> Option<Vec<String>> {
    // For now we only synthesize strict equality sender guards:
    // assert!(sender(ctx) == obj.field, ...)
    if op != "==" {
        return None;
    }
    let (mut setup_lines, call_args, cleanup, param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let target_param = d.params.iter().find(|p| p.name == param_name)?;
    let runtime_obj = param_runtime.get(param_name)?.clone();
    let target_ty = qualify_type_for_module(
        &d.module_name,
        &normalize_param_object_type(&target_param.ty),
    );

    // Shared objects can't be transferred across sender accounts in this path.
    let shared_take_hint = format!("scenario.take_shared<{}>()", target_ty);
    if setup_lines
        .iter()
        .any(|l| l.contains(&shared_take_hint) && l.contains(&runtime_obj))
    {
        return None;
    }

    let default_type_args = default_type_args_for_params(&d.type_params);
    let mut move_vars: Vec<(String, String, bool)> = Vec::new();
    move_vars.push((
        runtime_obj.clone(),
        target_ty,
        target_param.ty.trim().starts_with("&mut"),
    ));

    for p in &d.params {
        if p.name == param_name {
            continue;
        }
        let resolved_ty = concretize_type_params(&p.ty, &d.type_params, &default_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            continue;
        }
        if is_coin_type(t) {
            let coin_tag = coin_type_tag_from_coin_type_with_aliases(t, &d.module_use_aliases);
            if !is_qualified_type_tag(&coin_tag) {
                return None;
            }
            move_vars.push((
                format!("coin_{}", sanitize_ident(&p.name)),
                format!("sui::coin::Coin<{}>", coin_tag),
                t.starts_with("&mut"),
            ));
            continue;
        }
        if let Some(var) = param_runtime.get(&p.name) {
            let shared_binding = setup_lines.iter().any(|l| {
                l.contains("scenario.take_shared<")
                    && (l.contains(&format!("let {} = ", var))
                        || l.contains(&format!("let mut {} = ", var)))
            });
            if shared_binding {
                continue;
            }
            move_vars.push((
                var.clone(),
                qualify_type_for_module(&d.module_name, &normalize_param_object_type(t)),
                t.starts_with("&mut"),
            ));
        }
    }

    let mut seen = std::collections::HashSet::new();
    move_vars.retain(|(var, _, _)| seen.insert(var.clone()));

    for (var, _, _) in &move_vars {
        setup_lines.push(format!("transfer::public_transfer({}, OTHER);", var));
    }
    setup_lines.push("test_scenario::next_tx(&mut scenario, OTHER);".to_string());
    for (var, ty, needs_mut) in &move_vars {
        let maybe_mut = if *needs_mut { "mut " } else { "" };
        setup_lines.push(format!(
            "let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
            maybe_mut, var, ty
        ));
    }

    render_guard_call_test(d, env, case_idx, &setup_lines, &[], &call_args, &cleanup)
}

fn render_param_object_field_guard_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    env: &GuardEnv<'_>,
    case_idx: usize,
    param_name: &str,
    object_param: &str,
    field_name: &str,
    op: &str,
) -> Option<Vec<String>> {
    let (_, _, _, param_runtime, _) = build_common_call_plan(d, env)?;
    let runtime_obj = param_runtime.get(object_param)?;
    let getter_fn =
        pick_numeric_accessor_for_object_field(d, accessor_map, object_param, field_name)?;
    let bound_var = format!("guard_obj_bound_{}", case_idx);
    let prep = vec![format!(
        "let {} = {}::{}(&{});",
        bound_var, d.module_name, getter_fn, runtime_obj
    )];
    let bad_expr = violating_getter_arg(op, &bound_var)?;
    render_param_guard_test(d, env, case_idx, param_name, &prep, &bad_expr)
}

fn set_clock_for_guard(
    setup_lines: &mut Vec<String>,
    clock_param: &str,
    timestamp_expr: &str,
) -> bool {
    let clock_var = format!("clock_{}", sanitize_ident(clock_param));
    let mut matched_idx: Option<usize> = None;
    for (idx, line) in setup_lines.iter().enumerate() {
        if line.contains(&format!("{} = sui::clock::create_for_testing(", clock_var)) {
            matched_idx = Some(idx);
            break;
        }
    }
    let Some(idx) = matched_idx else {
        return false;
    };
    if setup_lines[idx].contains(&format!("let {} = ", clock_var)) {
        setup_lines[idx] = setup_lines[idx].replacen("let ", "let mut ", 1);
    }
    setup_lines.insert(
        idx + 1,
        format!(
            "sui::clock::set_for_testing(&mut {}, {});",
            clock_var, timestamp_expr
        ),
    );
    true
}

fn render_param_clock_guard_test(
    d: &FnDecl,
    env: &GuardEnv<'_>,
    case_idx: usize,
    param_name: &str,
    clock_param: &str,
    op: &str,
) -> Option<Vec<String>> {
    let (mut setup_lines, mut call_args, cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    if !set_clock_for_guard(&mut setup_lines, clock_param, "1000000") {
        return None;
    }
    let bad_expr = violating_getter_arg(op, "1000000")?;
    let param_idx = d.params.iter().position(|p| p.name == param_name)?;
    call_args[param_idx] = bad_expr;
    render_guard_call_test(d, env, case_idx, &setup_lines, &[], &call_args, &cleanup)
}

fn render_object_field_clock_guard_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    env: &GuardEnv<'_>,
    case_idx: usize,
    param_name: &str,
    field_name: &str,
    clock_param: &str,
    op: &str,
) -> Option<Vec<String>> {
    let (mut setup_lines, call_args, cleanup, param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let runtime_obj = param_runtime.get(param_name)?;
    let getter_fn =
        pick_numeric_accessor_for_object_field(d, accessor_map, param_name, field_name)?;
    let bound_var = format!("guard_obj_clock_bound_{}", case_idx);
    let prep = vec![format!(
        "let {} = {}::{}(&{});",
        bound_var, d.module_name, getter_fn, runtime_obj
    )];
    let timestamp_expr = match op {
        ">" | "<" | "!=" => bound_var.clone(),
        "==" | ">=" => format!("({} + 1)", bound_var),
        // Without proving bound_var > 0, synthesizing "<=" safely is not deterministic.
        "<=" => return None,
        _ => return None,
    };
    if !set_clock_for_guard(&mut setup_lines, clock_param, &timestamp_expr) {
        return None;
    }
    render_guard_call_test(d, env, case_idx, &setup_lines, &prep, &call_args, &cleanup)
}

fn render_coin_value_object_field_guard_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    env: &GuardEnv<'_>,
    case_idx: usize,
    coin_param: &str,
    object_param: &str,
    field_name: &str,
    op: &str,
) -> Option<Vec<String>> {
    let (mut setup_lines, mut call_args, mut cleanup, param_runtime, _param_arg_values) =
        build_common_call_plan(d, env)?;
    let param_idx = d.params.iter().position(|p| p.name == coin_param)?;
    let param_ty = d.params.get(param_idx)?.ty.trim();
    if !is_coin_type(param_ty) {
        return None;
    }
    let runtime_obj = param_runtime.get(object_param)?;
    let getter_fn = pick_numeric_accessor_for_object_field(d, accessor_map, object_param, field_name);
    let bound_var = format!("guard_coin_bound_{}", case_idx);

    let default_coin_var = format!("coin_{}", sanitize_ident(coin_param));
    setup_lines
        .retain(|l| !(l.contains("coin::mint_for_testing") && l.contains(&default_coin_var)));
    cleanup.retain(|l| !l.contains(&format!("{}{}", default_coin_var, ",")));

    let bad_coin_var = format!("guard_bad_coin_{}", case_idx);
    let mut prep = Vec::new();
    let bad_amount = if let Some(getter_fn) = getter_fn {
        prep.push(format!(
            "let {} = {}::{}(&{});",
            bound_var, d.module_name, getter_fn, runtime_obj
        ));
        violating_getter_arg(op, &bound_var)?
    } else {
        let obj_param = d.params.iter().find(|p| p.name == object_param)?;
        let obj_ty = normalize_param_object_type(&obj_param.ty);
        let type_key = type_key_from_type_name(&obj_ty);
        let module_fns = env.fn_lookup.get(&d.module_name)?;
        let helper_default = lookup_helper_field_literal(module_fns, &type_key, field_name)?;
        violating_getter_arg(op, &helper_default)?
    };
    let maybe_mut = if param_ty.starts_with("&mut") {
        "mut "
    } else {
        ""
    };
    prep.push(format!(
        "let {}{} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
        maybe_mut,
        bad_coin_var,
        coin_type_tag_from_coin_type_with_aliases(param_ty, &d.module_use_aliases),
        bad_amount
    ));
    if !is_qualified_type_tag(&coin_type_tag_from_coin_type_with_aliases(
        param_ty,
        &d.module_use_aliases,
    )) {
        return None;
    }
    call_args[param_idx] = if param_ty.starts_with("&mut") {
        format!("&mut {}", bad_coin_var)
    } else if param_ty.starts_with('&') {
        format!("&{}", bad_coin_var)
    } else {
        bad_coin_var.clone()
    };
    if param_ty.starts_with('&') {
        cleanup.push(format!(
            "transfer::public_transfer({}, SUPER_USER);",
            bad_coin_var
        ));
    }
    render_guard_call_test(d, env, case_idx, &setup_lines, &prep, &call_args, &cleanup)
}

pub(super) fn render_guard_tests_for_function(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> (Vec<Vec<String>>, Vec<String>) {
    let mut tests = Vec::new();
    let mut notes = Vec::new();
    let env = GuardEnv {
        helper_catalog,
        bootstrap_catalog,
        fn_lookup,
        key_structs_by_module,
        store_structs_by_module,
    };

    let (cases, parse_notes) = parse_assert_guard_cases(d, accessor_map);
    notes.extend(parse_notes);

    for (idx, case) in cases.iter().enumerate() {
        match &case.kind {
            GuardKind::CapRole {
                cap_param,
                cap_type,
            } => {
                if let Some(lines) = render_cap_role_guard_test(
                    d,
                    helper_catalog,
                    bootstrap_catalog,
                    fn_lookup,
                    key_structs_by_module,
                    store_structs_by_module,
                    idx,
                    cap_param,
                    cap_type,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize role/cap failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ParamConst {
                param_name,
                op,
                rhs_literal,
            } => {
                let Some(bad_expr) = violating_const_arg(op, rhs_literal) else {
                    notes.push(format!(
                        "{}: unsupported param-vs-const guard '{}'",
                        module_fn_label(d),
                        case.source
                    ));
                    continue;
                };
                if let Some(lines) =
                    render_param_guard_test(d, &env, idx, param_name, &[], &bad_expr)
                {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize param-vs-const failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ParamGetter {
                param_name,
                op,
                getter_fn,
                getter_param,
            } => {
                let Some((_, _, _, param_runtime, _)) = build_common_call_plan(d, &env) else {
                    notes.push(format!(
                        "{}: could not synthesize getter-bound failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                    continue;
                };
                let runtime_obj = match param_runtime.get(getter_param) {
                    Some(v) => v,
                    None => {
                        notes.push(format!(
                            "{}: missing runtime object for getter-bound guard ({})",
                            module_fn_label(d),
                            case.source
                        ));
                        continue;
                    }
                };
                let bound_var = format!("guard_bound_{}", idx);
                let prep = vec![format!(
                    "let {} = {}::{}(&{});",
                    bound_var, d.module_name, getter_fn, runtime_obj
                )];
                let Some(bad_expr) = violating_getter_arg(op, &bound_var) else {
                    notes.push(format!(
                        "{}: unsupported param-vs-getter guard '{}'",
                        module_fn_label(d),
                        case.source
                    ));
                    continue;
                };
                if let Some(lines) =
                    render_param_guard_test(d, &env, idx, param_name, &prep, &bad_expr)
                {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize param-vs-getter failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ObjectFieldConst {
                param_name,
                field_name,
                op,
                rhs_literal,
            } => {
                if let Some(lines) = render_object_field_const_guard_test(
                    d,
                    &env,
                    idx,
                    param_name,
                    field_name,
                    op,
                    rhs_literal,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize object-field failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ObjectFieldSender {
                param_name,
                field_name,
                op,
            } => {
                if let Some(lines) =
                    render_object_field_sender_guard_test(d, &env, idx, param_name, field_name, op)
                {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize sender/object-field failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ParamObjectField {
                param_name,
                object_param,
                field_name,
                op,
            } => {
                if let Some(lines) = render_param_object_field_guard_test(
                    d,
                    accessor_map,
                    &env,
                    idx,
                    param_name,
                    object_param,
                    field_name,
                    op,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize param-vs-object-field failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::CoinValueConst {
                coin_param,
                op,
                rhs_literal,
            } => {
                if let Some(lines) =
                    render_coin_value_guard_test(d, &env, idx, coin_param, op, rhs_literal)
                {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize coin-value failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::CoinValueObjectField {
                coin_param,
                object_param,
                field_name,
                op,
            } => {
                if let Some(lines) = render_coin_value_object_field_guard_test(
                    d,
                    accessor_map,
                    &env,
                    idx,
                    coin_param,
                    object_param,
                    field_name,
                    op,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize coin-value/object-field failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ParamClockTimestamp {
                param_name,
                clock_param,
                op,
            } => {
                if let Some(lines) =
                    render_param_clock_guard_test(d, &env, idx, param_name, clock_param, op)
                {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize param-vs-clock failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ObjectFieldClockTimestamp {
                param_name,
                field_name,
                clock_param,
                op,
            } => {
                if let Some(lines) = render_object_field_clock_guard_test(
                    d,
                    accessor_map,
                    &env,
                    idx,
                    param_name,
                    field_name,
                    clock_param,
                    op,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize object-field/clock failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::VectorLenConst {
                param_name,
                op,
                rhs_literal,
            } => {
                if let Some(lines) = render_vector_len_const_guard_test(
                    d,
                    &env,
                    idx,
                    param_name,
                    op,
                    rhs_literal,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize vector-length/const failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::VectorLenParam {
                lhs_param,
                rhs_param,
                op,
            } => {
                if let Some(lines) = render_vector_len_param_guard_test(
                    d, &env, idx, lhs_param, rhs_param, op,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize vector-length/vector-length failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
        }
    }

    (tests, notes)
}
