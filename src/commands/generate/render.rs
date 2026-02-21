use super::*;
use super::catalog::{synthesize_call_args_for_fn, ModuleHelperCatalog};

pub(super) fn render_best_effort_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    option_accessor_map: &std::collections::HashMap<String, Vec<OptionAccessorSig>>,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    numeric_effects: &[NumericEffect],
    vector_effects: &[VectorEffect],
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
    let mut needs_coin = false;
    let mut param_runtime: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_arg_values: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut object_needs: Vec<ObjectNeed> = Vec::new();

    for param in &d.params {
        let t = param.ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if t == "address" {
            let v = "OTHER".to_string();
            args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "u64" {
            let v = "1".to_string();
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
        } else if t.contains("Coin<") || t.contains("Coin <") {
            needs_coin = true;
            if t.starts_with("&mut") {
                args.push("&mut coin".to_string());
            } else {
                args.push("&coin".to_string());
            }
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
    let (option_before_lines, option_after_lines, option_summary) = build_option_assertion_lines(
        d,
        option_effects,
        &param_runtime,
        option_accessor_map,
        deep_overflow_paths,
    );
    let string_summary = build_string_summary(string_effects, &param_runtime, deep_overflow_paths);
    for eff in string_effects {
        let operator_target = format!("operator {}.{}", eff.base_var, eff.field);
        numeric_summary.asserted.remove(&operator_target);
        numeric_summary.potential.remove(&operator_target);
    }
    let mut state_summary = StateChangeSummary::default();
    state_summary.merge(numeric_summary);
    state_summary.merge(vector_summary);
    state_summary.merge(option_summary);
    state_summary.merge(string_summary);
    state_summary.merge(build_deep_chain_summary(deep_overflow_paths));
    let summary_lines = render_state_change_summary_lines(&state_summary);

    lines.push("    {".to_string());
    if needs_coin {
        lines.push("        let mut coin = coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario));".to_string());
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

    for req in &d.requires {
        if let Some(module_fns) = fn_lookup.get(&d.module_name) {
            if let Some(req_decl) = module_fns.get(req) {
                if let Some(req_args) =
                    synthesize_call_args_for_fn(req_decl, &object_vars_by_type, needs_coin)
                {
                    lines.push(format!(
                        "        {}::{}({});",
                        d.module_name,
                        req_decl.fn_name,
                        req_args.join(", ")
                    ));
                }
            }
        }
    }
    for l in snapshot_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in vector_before_lines {
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
    for obj in &owned_objects {
        lines.push(format!(
            "        transfer::public_transfer({}, SUPER_USER);",
            obj.var_name
        ));
    }
    if needs_coin {
        lines.push("        transfer::public_transfer(coin, SUPER_USER);".to_string());
    }
    lines.push("    };".to_string());

    // Ensure scenario is closed.
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
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
    let mut seen_targets: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut direct_effect_counts: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for eff in &d.numeric_effects {
        let key = format!("{}.{}", eff.base_var, eff.field);
        *direct_effect_counts.entry(key).or_insert(0) += 1;
    }
    let has_internal_calls = !d.calls.is_empty();
    let mut idx = 0usize;

    for eff in numeric_effects {
        let target_label = format!("operator {}.{}", eff.base_var, eff.field);
        if overflow_may_affect_target(deep_overflow_paths, &eff.base_var, &eff.field) {
            summary.add_potential(target_label);
            continue;
        }
        let runtime_var = match param_runtime.get(&eff.base_var) {
            Some(v) => v,
            None => {
                summary.add_potential(target_label);
                continue;
            }
        };
        let key = format!("{}.{}", runtime_var, eff.field);
        if seen_targets.contains(&key) {
            continue;
        }
        let read_expr = match resolve_read_expr(d, eff, runtime_var, accessor_map) {
            Some(expr) => expr,
            None => {
                summary.add_potential(target_label);
                continue;
            }
        };
        seen_targets.insert(key.clone());

        let before_name = format!(
            "before_{}_{}",
            sanitize_ident(runtime_var),
            sanitize_ident(&eff.field)
        );
        let after_name = format!(
            "after_{}_{}",
            sanitize_ident(runtime_var),
            sanitize_ident(&eff.field)
        );
        before.push(format!("let {} = {};", before_name, read_expr));
        after.push(format!("let {} = {};", after_name, read_expr));

        let direct_key = format!("{}.{}", eff.base_var, eff.field);
        let direct_count = direct_effect_counts.get(&direct_key).copied().unwrap_or(0);
        let prefer_unqualified = has_internal_calls || direct_count > 1;

        let exact_assert = if !prefer_unqualified {
            build_exact_assert_line(
                &eff.op,
                &after_name,
                &before_name,
                param_arg_values,
                900 + idx as u64,
            )
        } else {
            None
        };

        if let Some(line) = exact_assert {
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

    let has_internal_calls = !d.calls.is_empty();
    let mut potential_targets: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut seen_len_snapshots: std::collections::HashSet<String> = std::collections::HashSet::new();
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
        if has_internal_calls || effects.len() != 1 {
            potential_targets.insert(target);
            continue;
        }
        let read_expr =
            match resolve_vector_len_expr(d, &first.base_var, &first.field, runtime_var, accessor_map) {
                Some(v) => v,
                None => {
                    potential_targets.insert(target);
                    continue;
                }
            };
        let before_name = format!(
            "before_{}_{}_len",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );
        let after_name = format!(
            "after_{}_{}_len",
            sanitize_ident(runtime_var),
            sanitize_ident(&first.field)
        );
        if seen_len_snapshots.insert(before_name.clone()) {
            before.push(format!("let {} = {};", before_name, read_expr));
        }
        after.push(format!("let {} = {};", after_name, read_expr));
        match &first.op {
            VectorOp::PushBack | VectorOp::Insert => after.push(format!(
                "assert!({} == {} + 1, {});",
                after_name,
                before_name,
                960 + idx
            )),
            VectorOp::PopBack | VectorOp::Remove | VectorOp::SwapRemove => after.push(format!(
                "assert!({} + 1 == {}, {});",
                after_name,
                before_name,
                960 + idx
            )),
            VectorOp::Append {
                src_base_var,
                src_field,
            } => {
                let src_runtime_var = match param_runtime.get(src_base_var) {
                    Some(v) => v,
                    None => {
                        potential_targets.insert(target);
                        idx += 1;
                        continue;
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
                        potential_targets.insert(target);
                        idx += 1;
                        continue;
                    }
                };
                let src_before_name = format!(
                    "before_{}_{}_len",
                    sanitize_ident(src_runtime_var),
                    sanitize_ident(src_field)
                );
                if seen_len_snapshots.insert(src_before_name.clone()) {
                    before.push(format!("let {} = {};", src_before_name, src_len_expr));
                }
                after.push(format!(
                    "assert!({} == {} + {}, {});",
                    after_name,
                    before_name,
                    src_before_name,
                    960 + idx
                ));
            }
            VectorOp::ContentChanged => {
                potential_targets.insert(target);
                idx += 1;
                continue;
            }
        }
        summary.add_asserted(format!("vector {}", target));
        idx += 1;
    }

    for t in potential_targets {
        summary.add_potential(format!("vector {}", t));
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

    let mut direct_effect_counts: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for eff in &d.option_effects {
        let key = format!("{}.{}", eff.base_var, eff.field);
        *direct_effect_counts.entry(key).or_insert(0) += 1;
    }

    let has_internal_calls = !d.calls.is_empty();
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

        let direct_key = format!("{}.{}", first.base_var, first.field);
        let direct_count = direct_effect_counts.get(&direct_key).copied().unwrap_or(0);
        let prefer_generic = has_internal_calls || direct_count > 1 || effects.len() != 1;
        let needs_before = prefer_generic || matches!(first.op, OptionOp::Changed);

        if needs_before && seen_before_snapshots.insert(before_name.clone()) {
            before.push(format!("let {} = {};", before_name, read_expr.clone()));
        }
        after.push(format!("let {} = {};", after_name, read_expr));

        if prefer_generic {
            after.push(format!(
                "assert!({} != {}, {});",
                after_name,
                before_name,
                980 + idx
            ));
            summary.add_asserted(format!("option {}", target));
            idx += 1;
            continue;
        }

        match first.op {
            OptionOp::SetSome => after.push(format!("assert!({}, {});", after_name, 980 + idx)),
            OptionOp::SetNone => {
                after.push(format!("assert!(!{}, {});", after_name, 980 + idx))
            }
            OptionOp::Changed => after.push(format!(
                "assert!({} != {}, {});",
                after_name,
                before_name,
                980 + idx
            )),
        }
        summary.add_asserted(format!("option {}", target));
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
            summary.asserted.iter().cloned().collect::<Vec<_>>().join(", ")
        ));
    }
    if summary.potential.is_empty() {
        out.push("// potential_change: (none)".to_string());
    } else {
        out.push(format!(
            "// potential_change: {}",
            summary.potential.iter().cloned().collect::<Vec<_>>().join(", ")
        ));
    }
    out
}
