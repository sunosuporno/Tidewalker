use super::catalog::{ModuleBootstrapCatalog, ModuleHelperCatalog};
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

fn should_transfer_call_return(ret_ty: &str, module_name: &str) -> bool {
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
    if is_coin_type(t) || is_treasury_cap_type(t) {
        return true;
    }
    // Heuristic: local module return types are usually key objects in constructor/factory APIs.
    !t.contains("::") || t.starts_with(&format!("{}::", module_name))
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
        if is_vector_type(t) {
            let vec_expr = vector_literal_expr_for_type(t)?;
            if t.starts_with('&') {
                let var = format!(
                    "pre_req_vec_{}_{}",
                    req_step_idx,
                    sanitize_ident(&p.name)
                );
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

fn synthesize_factory_args(factory: &FnDecl) -> Option<Vec<String>> {
    let mut args = Vec::new();
    for p in &factory.params {
        let t = p.ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_vector_type(t) && !t.starts_with('&') {
            args.push(vector_literal_expr_for_type(t)?);
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
    Some(args)
}

fn pick_factory_call_for_type(
    d: &FnDecl,
    type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> Option<(String, Vec<String>)> {
    let mut best: Option<(usize, String, Vec<String>)> = None;
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
        let Some(args) = synthesize_factory_args(f) else {
            continue;
        };
        let score = f.params.len();
        let candidate = (score, f.fn_name.clone(), args);
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some(candidate);
        }
    }
    best.map(|(_, name, args)| (name, args))
}

fn pick_shared_creator_plan(
    d: &FnDecl,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> Option<SharedCreatorPlan> {
    let mut candidates = module_fns
        .values()
        .filter(|f| {
            !f.is_test_only
                && (f.is_public || f.is_entry)
                && f.fn_name != d.fn_name
                && f.fn_name.starts_with("create_shared_")
                && f.params.iter().any(|p| p.ty.contains("TxContext"))
        })
        .collect::<Vec<_>>();
    candidates.sort_by_key(|f| f.params.len());
    let best = candidates.first()?;
    let args = synthesize_factory_args(best)?;
    let type_args = if has_unbound_type_params(best) {
        default_type_args_for_params(&best.type_params)
    } else {
        Vec::new()
    };
    Some(SharedCreatorPlan {
        fn_name: best.fn_name.clone(),
        args,
        type_args,
    })
}

pub(super) fn render_best_effort_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    option_accessor_map: &std::collections::HashMap<String, Vec<OptionAccessorSig>>,
    container_accessor_map: &std::collections::HashMap<String, Vec<ContainerAccessorSig>>,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    numeric_effects: &[NumericEffect],
    vector_effects: &[VectorEffect],
    coin_effects: &[CoinEffect],
    treasury_cap_effects: &[TreasuryCapEffect],
    coin_notes: &[CoinNote],
    option_effects: &[OptionEffect],
    string_effects: &[StringEffect],
    container_effects: &[ContainerEffect],
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> Option<Vec<String>> {
    if d.params.iter().any(|p| {
        p.name
            .to_ascii_lowercase()
            .contains("public_inputs")
    }) {
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
    let default_type_args = default_type_args_for_params(&d.type_params);

    for param in &d.params {
        let resolved_ty = concretize_type_params(&param.ty, &d.type_params, &default_type_args);
        let t = resolved_ty.trim();
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
            args.push(arg);
            param_coin_runtime.insert(param.name.clone(), var_name.clone());
            param_coin_is_ref.insert(param.name.clone(), is_ref);
            coin_needs.push(CoinNeed {
                var_name,
                moved_on_main_call: !is_ref,
                needs_mut_binding: t.starts_with("&mut"),
            });
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
            return None;
        }
    }

    let mut shared_objects: Vec<ObjectNeed> = Vec::new();
    let mut owned_objects: Vec<ObjectNeed> = Vec::new();
    let mut object_sources: std::collections::HashMap<String, ObjectProvisionSource> =
        std::collections::HashMap::new();
    let mut factory_calls: std::collections::HashMap<String, (String, Vec<String>)> =
        std::collections::HashMap::new();
    let mut shared_creator_plan: Option<SharedCreatorPlan> = None;
    let mut creator_non_cap_type_keys: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    if !object_needs.is_empty() {
        let empty_helpers = ModuleHelperCatalog::default();
        let module_helpers = helper_catalog.get(&d.module_name).unwrap_or(&empty_helpers);
        let empty_bootstrap = ModuleBootstrapCatalog::default();
        let module_bootstrap = bootstrap_catalog
            .get(&d.module_name)
            .unwrap_or(&empty_bootstrap);
        let module_fns = fn_lookup.get(&d.module_name);
        for obj in &object_needs {
            if module_helpers.shared_types.contains(&obj.type_key) {
                shared_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::SharedHelper);
            } else if module_helpers.owned_types.contains(&obj.type_key) {
                owned_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedHelper);
            } else if module_bootstrap.one_time_witness_init
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
                if has_shared_helper {
                    shared_objects.push(obj.clone());
                    object_sources
                        .insert(obj.var_name.clone(), ObjectProvisionSource::SharedHelper);
                } else if has_owned_helper {
                    owned_objects.push(obj.clone());
                    object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedHelper);
                } else if let Some((factory_fn, factory_args)) =
                    module_fns.and_then(|fns| pick_factory_call_for_type(d, &obj.type_key, fns))
                {
                    owned_objects.push(obj.clone());
                    object_sources
                        .insert(obj.var_name.clone(), ObjectProvisionSource::OwnedFactory);
                    factory_calls.insert(obj.var_name.clone(), (factory_fn, factory_args));
                } else {
                    if shared_creator_plan.is_none() {
                        shared_creator_plan =
                            module_fns.and_then(|fns| pick_shared_creator_plan(d, fns));
                    }
                    if shared_creator_plan.is_some() {
                        if obj.type_key.contains("cap") {
                            owned_objects.push(obj.clone());
                            object_sources.insert(
                                obj.var_name.clone(),
                                ObjectProvisionSource::OwnedCreatorSender,
                            );
                        } else {
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
        }
    }
    let needs_init_bootstrap = object_sources.values().any(|src| {
        matches!(
            src,
            ObjectProvisionSource::SharedInit | ObjectProvisionSource::OwnedInit
        )
    });
    let has_init_bootstrap_helper = fn_lookup
        .get(&d.module_name)
        .map(|fns| fns.contains_key("bootstrap_init_for_testing"))
        .unwrap_or(false);
    if needs_init_bootstrap && !has_init_bootstrap_helper {
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
                lines.push(format!(
                    "        {}::create_and_share_{}_for_testing(test_scenario::ctx(&mut scenario));",
                    d.module_name, obj.type_key
                ));
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
            lines.push("    };".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if needs_init_bootstrap {
        lines.push("    {".to_string());
        lines.push(format!(
            "        {}::bootstrap_init_for_testing(test_scenario::ctx(&mut scenario));",
            d.module_name
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
        match object_sources.get(&obj.var_name) {
            Some(ObjectProvisionSource::OwnedHelper) => {
                lines.push(format!(
                    "        let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
                    maybe_mut, obj.var_name, d.module_name, obj.type_key
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
                let (factory_fn, factory_args) = factory_calls.get(&obj.var_name)?;
                lines.push(format!(
                    "        let {}{} = {}::{}({});",
                    maybe_mut,
                    obj.var_name,
                    d.module_name,
                    factory_fn,
                    factory_args.join(", ")
                ));
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
    for obj in &shared_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        let obj_ty = qualify_type_for_module(&d.module_name, &obj.type_name);
        lines.push(format!(
            "        let {}{} = scenario.take_shared<{}>();",
            maybe_mut, obj.var_name, obj_ty
        ));
    }
    for l in &arg_setup_lines {
        lines.push(format!("        {}", l));
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
        if should_transfer_call_return(ret_ty, &d.module_name) {
            lines.push(format!("        let tw_ret_obj = {};", call_expr));
            lines.push("        transfer::public_transfer(tw_ret_obj, SUPER_USER);".to_string());
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
