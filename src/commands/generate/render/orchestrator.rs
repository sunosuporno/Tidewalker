use super::catalog::ModuleHelperCatalog;
use super::*;
fn coin_var_names(needs: &[CoinNeed]) -> Vec<String> {
    needs.iter().map(|n| n.var_name.clone()).collect::<Vec<_>>()
}

fn default_u64_arg_for_param(name: &str) -> String {
    let lower = name.to_ascii_lowercase();
    if lower.contains("ratio") {
        "101".to_string()
    } else {
        "1".to_string()
    }
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

fn synthesize_requires_call_args(
    d: &FnDecl,
    req_step_idx: usize,
    req_chain: &[&FnDecl],
    object_vars_by_type: &std::collections::HashMap<String, String>,
    main_coin_vars: &[String],
    requires_extra_coin_slots: &mut std::collections::HashMap<usize, String>,
    pre_coin_counter: &mut usize,
    requires_cleanup_coin_vars: &mut Vec<String>,
    requires_consumed_coin_vars: &mut std::collections::HashSet<String>,
) -> Option<(Vec<String>, Vec<String>)> {
    let mut args = Vec::new();
    let mut prep = Vec::new();
    let mut coin_slot_idx = 0usize;
    for p in &d.params {
        let t = p.ty.trim();
        if t.starts_with('&') && !t.contains("TxContext") && !is_coin_type(t) {
            let ty = normalize_param_object_type(t);
            let key = type_key_from_type_name(&ty);
            let var = object_vars_by_type.get(&key)?;
            if t.starts_with("&mut") {
                args.push(format!("&mut {}", var));
            } else {
                args.push(format!("&{}", var));
            }
        } else if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
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
                    prep.push(format!(
                        "let {}{} = coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario));",
                        maybe_mut, var
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
                    prep.push(format!(
                        "let {} = coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario));",
                        var
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
                    prep.push(format!(
                        "let {} = coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario));",
                        var
                    ));
                    args.push(var);
                }
                coin_slot_idx += 1;
            }
        } else if t == "u64" {
            args.push(default_u64_arg_for_param(&p.name));
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            args.push("OTHER".to_string());
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else {
            return None;
        }
    }
    Some((args, prep))
}

pub(super) fn render_best_effort_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    option_accessor_map: &std::collections::HashMap<String, Vec<OptionAccessorSig>>,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    numeric_effects: &[NumericEffect],
    vector_effects: &[VectorEffect],
    coin_effects: &[CoinEffect],
    treasury_cap_effects: &[TreasuryCapEffect],
    coin_notes: &[CoinNote],
    option_effects: &[OptionEffect],
    string_effects: &[StringEffect],
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> Option<Vec<String>> {
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

    for param in &d.params {
        let t = param.ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if t == "address" {
            let v = "OTHER".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "u64" {
            let v = default_u64_arg_for_param(&param.name);
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "bool" {
            let v = "false".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            let v = "std::string::utf8(b\"tidewalker\")".to_string();
            args.push(v);
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
            args.push(arg);
            param_coin_runtime.insert(param.name.clone(), var_name.clone());
            param_coin_is_ref.insert(param.name.clone(), is_ref);
            coin_needs.push(CoinNeed {
                var_name,
                moved_on_main_call: !is_ref,
                needs_mut_binding: t.starts_with("&mut"),
            });
        } else if t.starts_with('&') {
            let obj = ObjectNeed::new(param);
            if obj.is_mut {
                args.push(format!("&mut {}", obj.var_name));
            } else {
                args.push(format!("&{}", obj.var_name));
            }
            param_runtime.insert(param.name.clone(), obj.var_name.clone());
            object_needs.push(obj);
        } else {
            return None;
        }
    }

    let mut shared_objects: Vec<ObjectNeed> = Vec::new();
    let mut owned_objects: Vec<ObjectNeed> = Vec::new();
    if !object_needs.is_empty() {
        let module_helpers = helper_catalog.get(&d.module_name)?;
        for obj in &object_needs {
            if module_helpers.shared_types.contains(&obj.type_key) {
                shared_objects.push(obj.clone());
            } else if module_helpers.owned_types.contains(&obj.type_key) {
                owned_objects.push(obj.clone());
            } else {
                return None;
            }
        }
    }
    let has_shared_objects = !shared_objects.is_empty();
    let preexisting_shared_type_keys: std::collections::HashSet<String> =
        shared_objects.iter().map(|o| o.type_key.clone()).collect();

    if has_shared_objects {
        lines.push("    {".to_string());
        for obj in &shared_objects {
            lines.push(format!(
                "        {}::create_and_share_{}_for_testing(test_scenario::ctx(&mut scenario));",
                d.module_name, obj.type_key
            ));
        }
        lines.push("    };".to_string());
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
    let mut requires_consumed_coin_vars: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut requires_extra_coin_slots: std::collections::HashMap<usize, String> =
        std::collections::HashMap::new();

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
    state_summary.merge(string_summary);
    state_summary.merge(build_deep_chain_summary(deep_overflow_paths));
    let summary_lines = render_state_change_summary_lines(&state_summary);

    lines.push("    {".to_string());
    for coin in &coin_needs {
        let maybe_mut = if coin.needs_mut_binding { "mut " } else { "" };
        lines.push(format!(
            "        let {}{} = coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario));",
            maybe_mut,
            coin.var_name
        ));
    }
    for obj in &owned_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        lines.push(format!(
            "        let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
            maybe_mut, obj.var_name, d.module_name, obj.type_key
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

    let mut requires_chain: Vec<&FnDecl> = Vec::new();
    if let Some(module_fns) = fn_lookup.get(&d.module_name) {
        for req in &d.requires {
            if let Some(req_decl) = module_fns.get(req) {
                requires_chain.push(req_decl);
            }
        }
    }
    for (req_idx, req_decl) in requires_chain.iter().enumerate() {
        if let Some((req_args, pre_lines)) = synthesize_requires_call_args(
            req_decl,
            req_idx,
            &requires_chain,
            &object_vars_by_type,
            &coin_vars_for_calls,
            &mut requires_extra_coin_slots,
            &mut pre_coin_counter,
            &mut requires_cleanup_coin_vars,
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
    lines.push(format!("        {}({});", fq, args.join(", ")));
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
    for obj in &owned_objects {
        lines.push(format!(
            "        transfer::public_transfer({}, SUPER_USER);",
            obj.var_name
        ));
    }
    for coin in &coin_needs {
        if !coin.moved_on_main_call {
            lines.push(format!(
                "        transfer::public_transfer({}, SUPER_USER);",
                coin.var_name
            ));
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
