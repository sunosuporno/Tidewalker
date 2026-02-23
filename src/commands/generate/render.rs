use super::catalog::ModuleHelperCatalog;
use super::*;

#[derive(Debug, Clone)]
struct CoinNeed {
    var_name: String,
    moved_on_main_call: bool,
    needs_mut_binding: bool,
}

#[derive(Debug, Clone)]
struct OwnershipTransferCheck {
    recipient: String,
    object_type: String,
}

#[derive(Debug, Clone)]
struct OwnershipShareCheck {
    object_type: String,
}

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

fn has_branch_or_loop(body_lines: &[String]) -> bool {
    for line in body_lines {
        let t = line.split("//").next().unwrap_or("").trim();
        if t.is_empty() {
            continue;
        }
        if t.starts_with("if ")
            || t.starts_with("if(")
            || t.starts_with("else")
            || t.starts_with("while ")
            || t.starts_with("while(")
            || t.starts_with("loop ")
            || t.starts_with("loop{")
            || t.starts_with("for ")
            || t.starts_with("for(")
        {
            return true;
        }
    }
    false
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

fn build_coin_note_summary(coin_notes: &[CoinNote]) -> StateChangeSummary {
    let mut summary = StateChangeSummary::default();
    for note in coin_notes {
        let target = match note {
            CoinNote::MintFlow => "coin mint flow".to_string(),
            CoinNote::BurnFlow => "coin burn flow".to_string(),
            CoinNote::StakeFlow => "staking flow".to_string(),
        };
        summary.add_potential(target);
    }
    summary
}

fn build_cap_auth_summary(d: &FnDecl) -> StateChangeSummary {
    let mut summary = StateChangeSummary::default();
    for p in &d.params {
        let ty = normalize_param_object_type(&p.ty);
        if p.ty.trim().starts_with('&') && is_cap_type(&ty) && !is_treasury_cap_type(&ty) {
            summary.add_asserted(format!("cap auth {}", p.name));
        }
    }
    summary
}

fn is_scalar_like_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    if norm.is_empty() {
        return true;
    }
    if is_numeric_type(&norm) || norm == "bool" || norm == "address" {
        return true;
    }
    if norm.starts_with("vector<") {
        return true;
    }
    if is_option_type(&norm) || is_string_type(&norm) {
        return true;
    }
    if norm == "ID" || norm.ends_with("::ID") || norm == "UID" || norm.ends_with("::UID") {
        return true;
    }
    false
}

fn is_trackable_object_type(ty: &str) -> bool {
    let norm = normalize_param_object_type(ty);
    if norm.contains("TxContext") || is_coin_type(&norm) {
        return false;
    }
    !is_scalar_like_type(&norm)
}

fn parse_let_binding_type_annotation(stmt: &str) -> Option<String> {
    let rest = stmt.strip_prefix("let ")?.trim();
    let lhs = rest.split_once('=').map(|(l, _)| l).unwrap_or(rest).trim();
    let lhs = lhs.strip_prefix("mut ").unwrap_or(lhs).trim();
    let (_, ty) = lhs.split_once(':')?;
    let out = normalize_param_object_type(ty);
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

fn parse_struct_destructure_id_bindings(stmt: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    let rest = match stmt.strip_prefix("let ") {
        Some(v) => v.trim(),
        None => return out,
    };
    let lhs = rest.split_once('=').map(|(l, _)| l).unwrap_or(rest).trim();
    let (ty_raw, fields_raw) = match lhs.split_once('{') {
        Some(v) => v,
        None => return out,
    };
    let fields = match fields_raw.rsplit_once('}') {
        Some((inner, _)) => inner,
        None => return out,
    };
    let ty =
        normalize_param_object_type(ty_raw.trim().strip_prefix("mut ").unwrap_or(ty_raw).trim());
    if !is_trackable_object_type(&ty) {
        return out;
    }
    for part in fields.split(',') {
        let token = part.trim();
        if token == "id" {
            out.push(("id".to_string(), ty.clone()));
            continue;
        }
        if let Some((left, right)) = token.split_once(':') {
            if left.trim() != "id" {
                continue;
            }
            let id_var = right.trim();
            if is_ident(id_var) {
                out.push((id_var.to_string(), ty.clone()));
            }
        }
    }
    out
}

fn resolve_object_var_and_type(
    obj_arg: &str,
    aliases: &std::collections::HashMap<String, String>,
    param_object_types: &std::collections::HashMap<String, String>,
    local_object_types: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let obj_token = strip_ref_and_parens_text(obj_arg);
    if !is_ident(obj_token) {
        return None;
    }
    let resolved = resolve_alias_path(obj_token, aliases);
    if let Some(ty) = param_object_types.get(&resolved) {
        return Some((resolved, ty.clone()));
    }
    if let Some(ty) = local_object_types.get(&resolved) {
        return Some((resolved, ty.clone()));
    }
    if let Some(ty) = param_object_types.get(obj_token) {
        return Some((obj_token.to_string(), ty.clone()));
    }
    if let Some(ty) = local_object_types.get(obj_token) {
        return Some((obj_token.to_string(), ty.clone()));
    }
    None
}

fn resolve_deleted_object_type(
    stmt: &str,
    aliases: &std::collections::HashMap<String, String>,
    param_object_types: &std::collections::HashMap<String, String>,
    local_object_types: &std::collections::HashMap<String, String>,
    id_owner_types: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let prefix = stmt.split_once(".delete(")?.0.trim();
    let token = strip_ref_and_parens_text(prefix);
    if let Some(base) = token.strip_suffix(".id") {
        let base = base.trim();
        if !is_ident(base) {
            return None;
        }
        let resolved = resolve_alias_path(base, aliases);
        return param_object_types
            .get(&resolved)
            .or_else(|| local_object_types.get(&resolved))
            .or_else(|| param_object_types.get(base))
            .or_else(|| local_object_types.get(base))
            .cloned();
    }
    if !is_ident(token) {
        return None;
    }
    let resolved = resolve_alias_path(token, aliases);
    id_owner_types
        .get(&resolved)
        .or_else(|| id_owner_types.get(token))
        .cloned()
}

fn build_ownership_checks(
    d: &FnDecl,
    param_arg_values: &std::collections::HashMap<String, String>,
    preexisting_shared_type_keys: &std::collections::HashSet<String>,
) -> (
    Vec<OwnershipTransferCheck>,
    Vec<OwnershipShareCheck>,
    StateChangeSummary,
) {
    let mut transfer_checks = Vec::new();
    let mut share_checks = Vec::new();
    let mut summary = StateChangeSummary::default();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut local_object_types: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_object_types: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut id_owner_types: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut minted_cap_vars: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut coin_vars: std::collections::HashSet<String> = std::collections::HashSet::new();
    for p in &d.params {
        if is_coin_type(&p.ty) {
            coin_vars.insert(p.name.clone());
        }
        let ty = normalize_param_object_type(&p.ty);
        if p.ty.trim().starts_with('&') {
            continue;
        }
        if is_trackable_object_type(&ty) {
            param_object_types.insert(p.name.clone(), ty);
        }
    }

    for line in &d.body_lines {
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
        if let Some((name, target)) = parse_alias_binding(stmt) {
            let resolved_target = resolve_alias_path(&target, &aliases);
            if coin_vars.contains(&target) || coin_vars.contains(&resolved_target) {
                coin_vars.insert(name.clone());
            }
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            for (id_var, owner_ty) in parse_struct_destructure_id_bindings(stmt) {
                id_owner_types.insert(id_var, owner_ty);
            }
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
                local_object_types.remove(&name);
                minted_cap_vars.remove(&name);
                coin_vars.remove(&name);
                if let Some(ty) = parse_let_binding_type_annotation(stmt) {
                    if is_trackable_object_type(&ty) {
                        local_object_types.insert(name.clone(), ty);
                    }
                }
                if let Some((_, rhs)) = stmt.split_once('=') {
                    let rhs = rhs.trim();
                    let rhs_lower = rhs.to_ascii_lowercase();
                    let coin_like_rhs = rhs_lower.contains("coin::")
                        || rhs_lower.contains(".into_coin(")
                        || rhs_lower.contains(".split(")
                        || rhs_lower.contains(".join(");
                    if coin_like_rhs {
                        coin_vars.insert(name.clone());
                    }
                    if rhs.contains('{') && rhs.contains('}') {
                        if let Some(type_token) = rhs.split('{').next() {
                            let ty = normalize_param_object_type(type_token.trim());
                            if is_trackable_object_type(&ty) {
                                local_object_types.insert(name.clone(), ty.clone());
                                if is_cap_type(&ty)
                                    && (rhs.contains("object::new(")
                                        || rhs.contains("sui::object::new("))
                                {
                                    minted_cap_vars.insert(name.clone(), ty);
                                }
                            }
                        }
                    }
                }
            }
        }

        if stmt.contains(".delete(") {
            if let Some(obj_ty) = resolve_deleted_object_type(
                stmt,
                &aliases,
                &param_object_types,
                &local_object_types,
                &id_owner_types,
            ) {
                let label_ty = if obj_ty.contains("::") {
                    obj_ty
                } else {
                    qualify_type_for_module(&d.module_name, &obj_ty)
                };
                summary.add_potential(format!("object delete {}", label_ty));
            } else {
                summary.add_potential("object delete".to_string());
            }
        }

        let transfer_args = extract_namespace_args(stmt, "transfer::public_transfer(")
            .or_else(|| extract_namespace_args(stmt, "transfer::transfer("));
        if let Some(args) = transfer_args {
            let obj_arg = match args.first() {
                Some(v) => v.as_str(),
                None => continue,
            };
            let recipient_arg = match args.get(1) {
                Some(v) => v.as_str(),
                None => continue,
            };
            let Some((resolved_obj, obj_ty)) = resolve_object_var_and_type(
                obj_arg,
                &aliases,
                &param_object_types,
                &local_object_types,
            ) else {
                let obj_token = strip_ref_and_parens_text(obj_arg);
                if is_ident(obj_token) {
                    let resolved_token = resolve_alias_path(obj_token, &aliases);
                    if coin_vars.contains(obj_token) || coin_vars.contains(&resolved_token) {
                        continue;
                    }
                }
                summary.add_potential("ownership transfer object".to_string());
                continue;
            };
            let label_ty = if obj_ty.contains("::") {
                obj_ty.clone()
            } else {
                qualify_type_for_module(&d.module_name, &obj_ty)
            };
            let transfer_label = if is_cap_type(&obj_ty) {
                format!("cap transfer {}", label_ty)
            } else {
                format!("ownership transfer {}", label_ty)
            };
            if has_non_linear_flow {
                summary.add_potential(transfer_label);
            } else if let Some(recipient) =
                resolve_cap_transfer_recipient(recipient_arg, param_arg_values)
            {
                transfer_checks.push(OwnershipTransferCheck {
                    recipient,
                    object_type: label_ty.clone(),
                });
                summary.add_asserted(transfer_label);
            } else {
                summary.add_potential(transfer_label);
            }
            if is_cap_type(&obj_ty) && minted_cap_vars.contains_key(&resolved_obj) {
                let mint_label = format!("cap mint {}", label_ty);
                if has_non_linear_flow {
                    summary.add_potential(mint_label);
                } else {
                    summary.add_asserted(mint_label);
                }
            }
        }

        let share_args = extract_namespace_args(stmt, "transfer::share_object(")
            .or_else(|| extract_namespace_args(stmt, "transfer::public_share_object("));
        if let Some(args) = share_args {
            let obj_arg = match args.first() {
                Some(v) => v.as_str(),
                None => continue,
            };
            let Some((resolved_obj, obj_ty)) = resolve_object_var_and_type(
                obj_arg,
                &aliases,
                &param_object_types,
                &local_object_types,
            ) else {
                summary.add_potential("ownership share object".to_string());
                continue;
            };
            let label_ty = if obj_ty.contains("::") {
                obj_ty.clone()
            } else {
                qualify_type_for_module(&d.module_name, &obj_ty)
            };
            let share_label = if is_cap_type(&obj_ty) {
                format!("cap share {}", label_ty)
            } else {
                format!("ownership share {}", label_ty)
            };
            if has_non_linear_flow {
                summary.add_potential(share_label);
            } else {
                let type_key = type_key_from_type_name(&obj_ty);
                if preexisting_shared_type_keys.contains(&type_key) {
                    summary.add_potential(share_label);
                } else {
                    share_checks.push(OwnershipShareCheck {
                        object_type: label_ty.clone(),
                    });
                    summary.add_asserted(share_label);
                }
            }
            if is_cap_type(&obj_ty) && minted_cap_vars.contains_key(&resolved_obj) {
                let mint_label = format!("cap mint {}", label_ty);
                if has_non_linear_flow {
                    summary.add_potential(mint_label);
                } else {
                    summary.add_asserted(mint_label);
                }
            }
        }
    }

    for (_var, cap_ty) in minted_cap_vars {
        let label_ty = if cap_ty.contains("::") {
            cap_ty
        } else {
            qualify_type_for_module(&d.module_name, &cap_ty)
        };
        summary.add_potential(format!("cap mint {}", label_ty));
    }

    (transfer_checks, share_checks, summary)
}

fn resolve_cap_transfer_recipient(
    raw: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let t = raw.trim();
    if t.contains("sender(") || t.contains("tx_context::sender") {
        return Some("SUPER_USER".to_string());
    }
    if t == "SUPER_USER" || t == "OTHER" {
        return Some(t.to_string());
    }
    if t.starts_with('@') {
        return Some(t.to_string());
    }
    if is_ident(t) {
        return param_arg_values.get(t).cloned();
    }
    None
}

fn overflow_may_affect_treasury_target(
    deep_overflow_paths: &std::collections::HashSet<String>,
    base_var: &str,
) -> bool {
    deep_overflow_paths.contains(base_var)
}

fn build_treasury_cap_assertion_lines(
    d: &FnDecl,
    treasury_cap_effects: &[TreasuryCapEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    param_arg_values: &std::collections::HashMap<String, String>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    let mut before = Vec::new();
    let mut after = Vec::new();
    let mut summary = StateChangeSummary::default();
    let mut mutable_treasury_params: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    for p in &d.params {
        let ty = normalize_param_object_type(&p.ty);
        if p.ty.trim().starts_with("&mut") && is_treasury_cap_type(&ty) {
            mutable_treasury_params.insert(p.name.clone());
        }
    }
    if mutable_treasury_params.is_empty() {
        return (before, after, summary);
    }

    let mut grouped: std::collections::BTreeMap<String, Vec<&TreasuryCapEffect>> =
        std::collections::BTreeMap::new();
    for eff in treasury_cap_effects {
        grouped.entry(eff.base_var.clone()).or_default().push(eff);
    }
    let has_internal_calls = !d.calls.is_empty();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut idx = 0u64;

    for base_var in mutable_treasury_params {
        let target_label = format!("treasury_cap {}", base_var);
        if overflow_may_affect_treasury_target(deep_overflow_paths, &base_var) {
            summary.add_potential(target_label);
            continue;
        }
        let runtime_var = match param_runtime.get(&base_var) {
            Some(v) => v,
            None => {
                summary.add_potential(target_label);
                continue;
            }
        };

        let effects = grouped.get(&base_var).cloned().unwrap_or_default();
        if has_non_linear_flow {
            summary.add_potential(target_label);
            idx += 1;
            continue;
        }
        if has_internal_calls {
            if effects.len() != 1 {
                summary.add_potential(target_label);
                idx += 1;
                continue;
            }

            let first = effects[0];
            let assert_expr = match &first.op {
                TreasuryCapOp::Mint(amount) => {
                    let resolved_amount = match resolve_numeric_operand(amount, param_arg_values) {
                        Some(v) => v,
                        None => {
                            summary.add_potential(target_label);
                            idx += 1;
                            continue;
                        }
                    };
                    Some(format!("before_total_supply + {}", resolved_amount))
                }
                TreasuryCapOp::Burn(amount) => {
                    let resolved_amount = match resolve_numeric_operand(amount, param_arg_values) {
                        Some(v) => v,
                        None => {
                            summary.add_potential(target_label);
                            idx += 1;
                            continue;
                        }
                    };
                    Some(format!("before_total_supply - {}", resolved_amount))
                }
                TreasuryCapOp::Changed => None,
            };
            let before_name = format!("before_{}_total_supply", sanitize_ident(runtime_var));
            let after_name = format!("after_{}_total_supply", sanitize_ident(runtime_var));
            before.push(format!(
                "let {} = coin::total_supply(&{});",
                before_name, runtime_var
            ));
            after.push(format!(
                "let {} = coin::total_supply(&{});",
                after_name, runtime_var
            ));
            if let Some(expr) = assert_expr {
                after.push(format!(
                    "assert!({} == {}, {});",
                    after_name,
                    expr.replace("before_total_supply", &before_name),
                    995 + idx
                ));
            } else {
                after.push(format!(
                    "assert!({} != {}, {});",
                    after_name,
                    before_name,
                    995 + idx
                ));
            }
            summary.add_asserted(target_label);
            idx += 1;
            continue;
        }

        let mut expr = "before_total_supply".to_string();
        let mut exact = true;
        if effects.is_empty() {
            expr = "before_total_supply".to_string();
        } else {
            for eff in &effects {
                match &eff.op {
                    TreasuryCapOp::Mint(amount) => {
                        let resolved_amount =
                            match resolve_numeric_operand(amount, param_arg_values) {
                                Some(v) => v,
                                None => {
                                    exact = false;
                                    break;
                                }
                            };
                        expr = format!("({} + {})", expr, resolved_amount);
                    }
                    TreasuryCapOp::Burn(amount) => {
                        let resolved_amount =
                            match resolve_numeric_operand(amount, param_arg_values) {
                                Some(v) => v,
                                None => {
                                    exact = false;
                                    break;
                                }
                            };
                        expr = format!("({} - {})", expr, resolved_amount);
                    }
                    TreasuryCapOp::Changed => {
                        exact = false;
                        break;
                    }
                }
            }
        }

        if !exact {
            if effects.len() == 1 && matches!(&effects[0].op, TreasuryCapOp::Changed) {
                let before_name = format!("before_{}_total_supply", sanitize_ident(runtime_var));
                let after_name = format!("after_{}_total_supply", sanitize_ident(runtime_var));
                before.push(format!(
                    "let {} = coin::total_supply(&{});",
                    before_name, runtime_var
                ));
                after.push(format!(
                    "let {} = coin::total_supply(&{});",
                    after_name, runtime_var
                ));
                after.push(format!(
                    "assert!({} != {}, {});",
                    after_name,
                    before_name,
                    995 + idx
                ));
                summary.add_asserted(target_label);
            } else {
                summary.add_potential(target_label);
            }
            idx += 1;
            continue;
        }

        let before_name = format!("before_{}_total_supply", sanitize_ident(runtime_var));
        let after_name = format!("after_{}_total_supply", sanitize_ident(runtime_var));
        before.push(format!(
            "let {} = coin::total_supply(&{});",
            before_name, runtime_var
        ));
        after.push(format!(
            "let {} = coin::total_supply(&{});",
            after_name, runtime_var
        ));
        after.push(format!(
            "assert!({} == {}, {});",
            after_name,
            expr.replace("before_total_supply", &before_name),
            995 + idx
        ));
        summary.add_asserted(target_label);
        idx += 1;
    }

    (before, after, summary)
}

fn resolve_read_expr(
    d: &FnDecl,
    eff: &NumericEffect,
    runtime_var: &str,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
) -> Option<String> {
    let param = d.params.iter().find(|p| p.name == eff.base_var)?;
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = accessor_map.get(&d.module_name)?;
    let has_accessor = module_accessors
        .iter()
        .any(|a| a.param_ty == param_obj_ty && a.fn_name == eff.field);
    if has_accessor {
        Some(format!(
            "{}::{}(&{})",
            d.module_name, eff.field, runtime_var
        ))
    } else {
        None
    }
}

fn overflow_may_affect_target(
    deep_overflow_paths: &std::collections::HashSet<String>,
    base_var: &str,
    field: &str,
) -> bool {
    let exact = format!("{}.{}", base_var, field);
    for p in deep_overflow_paths {
        if p == base_var || p == &exact {
            return true;
        }
    }
    false
}

fn build_numeric_assertion_lines(
    d: &FnDecl,
    numeric_effects: &[NumericEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    param_arg_values: &std::collections::HashMap<String, String>,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    let mut before = Vec::new();
    let mut after = Vec::new();
    let mut summary = StateChangeSummary::default();
    if numeric_effects.is_empty() {
        return (before, after, summary);
    }
    let mut grouped: std::collections::BTreeMap<String, Vec<&NumericEffect>> =
        std::collections::BTreeMap::new();
    for eff in numeric_effects {
        grouped
            .entry(format!("{}.{}", eff.base_var, eff.field))
            .or_default()
            .push(eff);
    }
    let has_internal_calls = !d.calls.is_empty();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut idx = 0usize;

    for (_target, effects) in grouped {
        let first = effects[0];
        let target_label = format!("operator {}.{}", first.base_var, first.field);
        if overflow_may_affect_target(deep_overflow_paths, &first.base_var, &first.field) {
            summary.add_potential(target_label);
            continue;
        }
        let runtime_var = match param_runtime.get(&first.base_var) {
            Some(v) => v,
            None => {
                summary.add_potential(target_label);
                continue;
            }
        };
        let read_expr = match resolve_read_expr(d, first, runtime_var, accessor_map) {
            Some(expr) => expr,
            None => {
                summary.add_potential(target_label);
                continue;
            }
        };

        let before_name = format!(
            "before_{}_{}",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );
        let after_name = format!(
            "after_{}_{}",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );

        if has_non_linear_flow {
            summary.add_potential(target_label);
            idx += 1;
            continue;
        }

        if has_internal_calls {
            if effects.len() != 1 {
                summary.add_potential(target_label);
                idx += 1;
                continue;
            }
            before.push(format!("let {} = {};", before_name, read_expr.clone()));
            after.push(format!("let {} = {};", after_name, read_expr));
            if let Some(line) = build_exact_assert_line(
                &effects[0].op,
                &after_name,
                &before_name,
                param_arg_values,
                900 + idx as u64,
            ) {
                after.push(line);
                summary.add_asserted(target_label);
            } else {
                after.push(format!(
                    "assert!({} != {}, {});",
                    after_name,
                    before_name,
                    900 + idx as u64
                ));
                summary.add_asserted(target_label);
            }
            idx += 1;
            continue;
        }

        let mut expr = "before_value".to_string();
        let mut exact = true;
        for eff in &effects {
            match &eff.op {
                NumericOp::Add(v) => {
                    let resolved = match resolve_numeric_operand(v, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = format!("({} + {})", expr, resolved);
                }
                NumericOp::Sub(v) => {
                    let resolved = match resolve_numeric_operand(v, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = format!("({} - {})", expr, resolved);
                }
                NumericOp::Mul(v) => {
                    let resolved = match resolve_numeric_operand(v, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = format!("({} * {})", expr, resolved);
                }
                NumericOp::Div(v) => {
                    let resolved = match resolve_numeric_operand(v, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = format!("({} / {})", expr, resolved);
                }
                NumericOp::Mod(v) => {
                    let resolved = match resolve_numeric_operand(v, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = format!("({} % {})", expr, resolved);
                }
                NumericOp::Set(v) => {
                    let resolved = match resolve_numeric_operand(v, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = resolved;
                }
                NumericOp::Changed => {
                    exact = false;
                    break;
                }
            }
        }

        if !exact {
            if effects.len() == 1 && matches!(&effects[0].op, NumericOp::Changed) {
                before.push(format!("let {} = {};", before_name, read_expr.clone()));
                after.push(format!("let {} = {};", after_name, read_expr));
                after.push(format!(
                    "assert!({} != {}, {});",
                    after_name,
                    before_name,
                    900 + idx as u64
                ));
                summary.add_asserted(target_label);
            } else {
                summary.add_potential(target_label);
            }
            idx += 1;
            continue;
        }

        before.push(format!("let {} = {};", before_name, read_expr.clone()));
        after.push(format!("let {} = {};", after_name, read_expr));
        after.push(format!(
            "assert!({} == {}, {});",
            after_name,
            expr.replace("before_value", &before_name),
            900 + idx as u64
        ));
        summary.add_asserted(target_label);
        idx += 1;
    }

    (before, after, summary)
}

fn resolve_numeric_operand(
    token: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
) -> Option<String> {
    if parse_numeric_literal(token).is_some() {
        return Some(token.to_string());
    }
    if is_ident(token) {
        let bound = param_arg_values.get(token)?;
        if parse_numeric_literal(bound).is_some() {
            return Some(bound.clone());
        }
    }
    None
}

fn build_exact_assert_line(
    op: &NumericOp,
    after_name: &str,
    before_name: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
    code: u64,
) -> Option<String> {
    match op {
        NumericOp::Add(v) => Some(format!(
            "assert!({} == {} + {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Sub(v) => Some(format!(
            "assert!({} == {} - {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Mul(v) => Some(format!(
            "assert!({} == {} * {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Div(v) => Some(format!(
            "assert!({} == {} / {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Mod(v) => Some(format!(
            "assert!({} == {} % {}, {});",
            after_name,
            before_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Set(v) => Some(format!(
            "assert!({} == {}, {});",
            after_name,
            resolve_numeric_operand(v, param_arg_values)?,
            code
        )),
        NumericOp::Changed => None,
    }
}

fn resolve_vector_len_expr(
    d: &FnDecl,
    base_var: &str,
    field: &str,
    runtime_var: &str,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
) -> Option<String> {
    let param = d.params.iter().find(|p| p.name == base_var)?;
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = accessor_map.get(&d.module_name)?;
    let preferred = format!("{}_len", field);
    let alt = format!("len_{}", field);
    let accessor = module_accessors
        .iter()
        .find(|a| a.param_ty == param_obj_ty && (a.fn_name == preferred || a.fn_name == alt))?;
    Some(format!(
        "{}::{}(&{})",
        d.module_name, accessor.fn_name, runtime_var
    ))
}

fn build_vector_assertion_lines(
    d: &FnDecl,
    vector_effects: &[VectorEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    let mut before = Vec::new();
    let mut after = Vec::new();
    let mut summary = StateChangeSummary::default();
    if vector_effects.is_empty() {
        return (before, after, summary);
    }

    let mut grouped: std::collections::BTreeMap<String, Vec<&VectorEffect>> =
        std::collections::BTreeMap::new();
    for eff in vector_effects {
        grouped
            .entry(format!("{}.{}", eff.base_var, eff.field))
            .or_default()
            .push(eff);
    }
    let modified_targets: std::collections::HashSet<String> = grouped.keys().cloned().collect();

    let has_internal_calls = !d.calls.is_empty();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut potential_targets: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    let mut seen_len_snapshots: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut idx = 0u64;

    for (target, effects) in grouped {
        let first = effects[0];
        if overflow_may_affect_target(deep_overflow_paths, &first.base_var, &first.field) {
            potential_targets.insert(target);
            continue;
        }
        let runtime_var = match param_runtime.get(&first.base_var) {
            Some(v) => v,
            None => {
                potential_targets.insert(target);
                continue;
            }
        };
        if has_internal_calls || has_non_linear_flow {
            potential_targets.insert(target);
            continue;
        }
        let read_expr = match resolve_vector_len_expr(
            d,
            &first.base_var,
            &first.field,
            runtime_var,
            accessor_map,
        ) {
            Some(v) => v,
            None => {
                potential_targets.insert(target);
                continue;
            }
        };
        let before_name_tpl = format!(
            "before_{}_{}_len",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );
        let after_name_tpl = format!(
            "after_{}_{}_len",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );

        let mut expr = "before_len".to_string();
        let mut required_src: Vec<(String, String)> = Vec::new(); // (name, read_expr)
        let mut exact = true;
        for eff in &effects {
            match &eff.op {
                VectorOp::PushBack | VectorOp::Insert => {
                    expr = format!("({} + 1)", expr);
                }
                VectorOp::PopBack | VectorOp::Remove | VectorOp::SwapRemove => {
                    expr = format!("({} - 1)", expr);
                }
                VectorOp::Append {
                    src_base_var,
                    src_field,
                } => {
                    let src_target = format!("{}.{}", src_base_var, src_field);
                    if modified_targets.contains(&src_target) {
                        exact = false;
                        break;
                    }
                    let src_runtime_var = match param_runtime.get(src_base_var) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    let src_len_expr = match resolve_vector_len_expr(
                        d,
                        src_base_var,
                        src_field,
                        src_runtime_var,
                        accessor_map,
                    ) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    let src_before_name = format!(
                        "before_{}_{}_len",
                        sanitize_ident(src_runtime_var),
                        sanitize_ident(src_field)
                    );
                    required_src.push((src_before_name.clone(), src_len_expr));
                    expr = format!("({} + {})", expr, src_before_name);
                }
                VectorOp::ContentChanged => {
                    exact = false;
                    break;
                }
            }
        }
        if !exact {
            if effects.len() == 1 {
                match &first.op {
                    VectorOp::PushBack
                    | VectorOp::Insert
                    | VectorOp::PopBack
                    | VectorOp::Remove
                    | VectorOp::SwapRemove => {}
                    _ => {
                        potential_targets.insert(target);
                        idx += 1;
                        continue;
                    }
                }
            } else {
                potential_targets.insert(target);
                idx += 1;
                continue;
            }
        }

        let before_name = before_name_tpl;
        let after_name = after_name_tpl;
        if seen_len_snapshots.insert(before_name.clone()) {
            before.push(format!("let {} = {};", before_name, read_expr.clone()));
        }
        for (src_before_name, src_len_expr) in required_src {
            if seen_len_snapshots.insert(src_before_name.clone()) {
                before.push(format!("let {} = {};", src_before_name, src_len_expr));
            }
        }
        after.push(format!("let {} = {};", after_name, read_expr));
        after.push(format!(
            "assert!({} == {}, {});",
            after_name,
            expr.replace("before_len", &before_name),
            960 + idx
        ));
        summary.add_asserted(format!("vector {}", target));
        idx += 1;
    }

    for t in potential_targets {
        summary.add_potential(format!("vector {}", t));
    }

    (before, after, summary)
}

fn overflow_may_affect_coin_target(
    deep_overflow_paths: &std::collections::HashSet<String>,
    base_var: &str,
) -> bool {
    deep_overflow_paths.contains(base_var)
}

fn build_coin_assertion_lines(
    d: &FnDecl,
    coin_effects: &[CoinEffect],
    param_coin_runtime: &std::collections::HashMap<String, String>,
    param_coin_is_ref: &std::collections::HashMap<String, bool>,
    param_arg_values: &std::collections::HashMap<String, String>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    let mut before = Vec::new();
    let mut after = Vec::new();
    let mut summary = StateChangeSummary::default();
    if coin_effects.is_empty() {
        return (before, after, summary);
    }

    let mut grouped: std::collections::BTreeMap<String, Vec<&CoinEffect>> =
        std::collections::BTreeMap::new();
    for eff in coin_effects {
        grouped.entry(eff.base_var.clone()).or_default().push(eff);
    }
    let modified_coin_targets: std::collections::HashSet<String> =
        grouped.keys().cloned().collect();

    let has_internal_calls = !d.calls.is_empty();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut seen_before_snapshots: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut idx = 0u64;

    for (base_var, effects) in grouped {
        let target_label = format!("coin {}", base_var);
        if overflow_may_affect_coin_target(deep_overflow_paths, &base_var) {
            summary.add_potential(target_label);
            continue;
        }
        let runtime_var = match param_coin_runtime.get(&base_var) {
            Some(v) => v,
            None => {
                summary.add_potential(target_label);
                continue;
            }
        };
        if !param_coin_is_ref.get(&base_var).copied().unwrap_or(false) {
            // Cannot observe post-call value for by-value coin params.
            summary.add_potential(target_label);
            continue;
        }
        if has_internal_calls || has_non_linear_flow {
            summary.add_potential(target_label);
            continue;
        }

        let before_name = format!("before_{}_coin_value", sanitize_ident(runtime_var));
        let after_name = format!("after_{}_coin_value", sanitize_ident(runtime_var));
        let mut expr = "before_coin_value".to_string();
        let mut exact = true;
        let mut required_src_snapshots: Vec<(String, String)> = Vec::new();
        for eff in &effects {
            match &eff.op {
                CoinOp::Split(amount) | CoinOp::Burn(amount) => {
                    let resolved_amount = match resolve_numeric_operand(amount, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = format!("({} - {})", expr, resolved_amount);
                }
                CoinOp::Mint(amount) => {
                    let resolved_amount = match resolve_numeric_operand(amount, param_arg_values) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    expr = format!("({} + {})", expr, resolved_amount);
                }
                CoinOp::Join { src_base_var } => {
                    if modified_coin_targets.contains(src_base_var) {
                        exact = false;
                        break;
                    }
                    let src_runtime = match param_coin_runtime.get(src_base_var) {
                        Some(v) => v,
                        None => {
                            exact = false;
                            break;
                        }
                    };
                    let src_before_name =
                        format!("before_{}_coin_value", sanitize_ident(src_runtime));
                    required_src_snapshots.push((src_before_name.clone(), src_runtime.clone()));
                    expr = format!("({} + {})", expr, src_before_name);
                }
                CoinOp::Changed => {
                    exact = false;
                    break;
                }
            }
        }

        if !exact {
            if effects.len() == 1 && matches!(&effects[0].op, CoinOp::Changed) {
                if seen_before_snapshots.insert(before_name.clone()) {
                    before.push(format!(
                        "let {} = coin::value(&{});",
                        before_name, runtime_var
                    ));
                }
                after.push(format!(
                    "let {} = coin::value(&{});",
                    after_name, runtime_var
                ));
                after.push(format!(
                    "assert!({} != {}, {});",
                    after_name,
                    before_name,
                    990 + idx
                ));
                summary.add_asserted(target_label);
            } else {
                summary.add_potential(target_label);
            }
            idx += 1;
            continue;
        }

        if seen_before_snapshots.insert(before_name.clone()) {
            before.push(format!(
                "let {} = coin::value(&{});",
                before_name, runtime_var
            ));
        }
        for (src_before_name, src_runtime) in required_src_snapshots {
            if seen_before_snapshots.insert(src_before_name.clone()) {
                before.push(format!(
                    "let {} = coin::value(&{});",
                    src_before_name, src_runtime
                ));
            }
        }
        after.push(format!(
            "let {} = coin::value(&{});",
            after_name, runtime_var
        ));
        after.push(format!(
            "assert!({} == {}, {});",
            after_name,
            expr.replace("before_coin_value", &before_name),
            990 + idx
        ));
        summary.add_asserted(target_label);
        idx += 1;
    }

    (before, after, summary)
}

fn resolve_option_is_some_expr(
    d: &FnDecl,
    eff: &OptionEffect,
    runtime_var: &str,
    option_accessor_map: &std::collections::HashMap<String, Vec<OptionAccessorSig>>,
) -> Option<String> {
    let param = d.params.iter().find(|p| p.name == eff.base_var)?;
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = option_accessor_map.get(&d.module_name)?;
    let accessor = module_accessors
        .iter()
        .find(|a| a.param_ty == param_obj_ty && a.field == eff.field)?;
    if accessor.is_some_when_true {
        Some(format!(
            "{}::{}(&{})",
            d.module_name, accessor.fn_name, runtime_var
        ))
    } else {
        Some(format!(
            "!({}::{}(&{}))",
            d.module_name, accessor.fn_name, runtime_var
        ))
    }
}

fn build_option_assertion_lines(
    d: &FnDecl,
    option_effects: &[OptionEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    option_accessor_map: &std::collections::HashMap<String, Vec<OptionAccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    let mut before = Vec::new();
    let mut after = Vec::new();
    let mut summary = StateChangeSummary::default();
    if option_effects.is_empty() {
        return (before, after, summary);
    }

    let mut grouped: std::collections::BTreeMap<String, Vec<&OptionEffect>> =
        std::collections::BTreeMap::new();
    for eff in option_effects {
        grouped
            .entry(format!("{}.{}", eff.base_var, eff.field))
            .or_default()
            .push(eff);
    }

    let has_internal_calls = !d.calls.is_empty();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut seen_before_snapshots: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut idx = 0u64;

    for (target, effects) in grouped {
        let first = effects[0];
        if overflow_may_affect_target(deep_overflow_paths, &first.base_var, &first.field) {
            summary.add_potential(format!("option {}", target));
            continue;
        }
        let runtime_var = match param_runtime.get(&first.base_var) {
            Some(v) => v,
            None => {
                summary.add_potential(format!("option {}", target));
                continue;
            }
        };
        let read_expr =
            match resolve_option_is_some_expr(d, first, runtime_var, option_accessor_map) {
                Some(v) => v,
                None => {
                    summary.add_potential(format!("option {}", target));
                    continue;
                }
            };
        let before_name = format!(
            "before_{}_{}_is_some",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );
        let after_name = format!(
            "after_{}_{}_is_some",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );

        if has_internal_calls || has_non_linear_flow {
            summary.add_potential(format!("option {}", target));
            idx += 1;
            continue;
        }

        let mut final_known: Option<bool> = None;
        let mut exact = true;
        for eff in &effects {
            match eff.op {
                OptionOp::SetSome => final_known = Some(true),
                OptionOp::SetNone => final_known = Some(false),
                OptionOp::Changed => {
                    final_known = None;
                    exact = false;
                }
            }
        }

        if let Some(v) = final_known {
            after.push(format!("let {} = {};", after_name, read_expr));
            if v {
                after.push(format!("assert!({}, {});", after_name, 980 + idx));
            } else {
                after.push(format!("assert!(!{}, {});", after_name, 980 + idx));
            }
            summary.add_asserted(format!("option {}", target));
            idx += 1;
            continue;
        }

        if !exact && effects.len() == 1 && matches!(&effects[0].op, OptionOp::Changed) {
            if seen_before_snapshots.insert(before_name.clone()) {
                before.push(format!("let {} = {};", before_name, read_expr.clone()));
            }
            after.push(format!("let {} = {};", after_name, read_expr));
            after.push(format!(
                "assert!({} != {}, {});",
                after_name,
                before_name,
                980 + idx
            ));
            summary.add_asserted(format!("option {}", target));
        } else {
            summary.add_potential(format!("option {}", target));
        }
        idx += 1;
    }

    (before, after, summary)
}

fn build_string_summary(
    string_effects: &[StringEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> StateChangeSummary {
    let mut summary = StateChangeSummary::default();
    for eff in string_effects {
        if overflow_may_affect_target(deep_overflow_paths, &eff.base_var, &eff.field) {
            summary.add_potential(format!("string {}.{}", eff.base_var, eff.field));
            continue;
        }
        if param_runtime.contains_key(&eff.base_var) {
            summary.add_potential(format!("string {}.{}", eff.base_var, eff.field));
        }
    }
    summary
}

fn build_deep_chain_summary(
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> StateChangeSummary {
    let mut summary = StateChangeSummary::default();
    for path in deep_overflow_paths {
        if path.contains('.') {
            summary.add_potential(format!("state {}", path));
        }
    }
    summary
}

fn render_state_change_summary_lines(summary: &StateChangeSummary) -> Vec<String> {
    let mut out = Vec::new();
    if summary.asserted.is_empty() && summary.potential.is_empty() {
        return out;
    }
    out.push("// Tidewalker state changes:".to_string());
    if summary.asserted.is_empty() {
        out.push("// asserted: (none)".to_string());
    } else {
        out.push(format!(
            "// asserted: {}",
            summary
                .asserted
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    if summary.potential.is_empty() {
        out.push("// potential_change: (none)".to_string());
    } else {
        out.push(format!(
            "// potential_change: {}",
            summary
                .potential
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    out
}
