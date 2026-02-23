use super::*;

fn resolve_container_accessor_fn(
    d: &FnDecl,
    base_var: &str,
    field: &str,
    container_accessor_map: &std::collections::HashMap<String, Vec<ContainerAccessorSig>>,
) -> Option<String> {
    let param = d.params.iter().find(|p| p.name == base_var)?;
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = container_accessor_map.get(&d.module_name)?;
    let acc = module_accessors
        .iter()
        .find(|a| a.param_ty == param_obj_ty && a.field == field)?;
    Some(acc.fn_name.clone())
}

fn resolve_container_key_expr(
    token: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let t = strip_ref_and_parens_text(token)
        .trim()
        .trim_end_matches(',')
        .trim();
    if let Some(v) = parse_numeric_literal(t) {
        return Some(v);
    }
    if is_bool_literal(t) || is_address_literal(t) {
        return Some(t.to_string());
    }
    if is_ident(t) {
        if let Some(v) = param_arg_values.get(t) {
            return Some(v.clone());
        }
        if param_arg_values.len() == 1 {
            return param_arg_values.values().next().cloned();
        }
        if t == "k" || t.contains("key") {
            for (name, value) in param_arg_values {
                if name == "k" || name.contains("key") {
                    return Some(value.clone());
                }
            }
        }
    }
    None
}

pub(super) fn build_container_assertion_lines(
    d: &FnDecl,
    container_effects: &[ContainerEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    param_arg_values: &std::collections::HashMap<String, String>,
    container_accessor_map: &std::collections::HashMap<String, Vec<ContainerAccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    let before = Vec::new();
    let mut after = Vec::new();
    let mut summary = StateChangeSummary::default();
    if container_effects.is_empty() {
        return (before, after, summary);
    }

    let mut grouped: std::collections::BTreeMap<String, Vec<&ContainerEffect>> =
        std::collections::BTreeMap::new();
    for eff in container_effects {
        grouped
            .entry(format!("{}.{}", eff.base_var, eff.field))
            .or_default()
            .push(eff);
    }

    let has_internal_calls = !d.calls.is_empty();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut idx = 0u64;

    for (target, effects) in grouped {
        let first = effects[0];
        let kind = match first.kind {
            ContainerKind::Table => "table",
            ContainerKind::VecMap => "vec_map",
            ContainerKind::VecSet => "vec_set",
            ContainerKind::Bag => "bag",
            ContainerKind::DynamicField => "dynamic_field",
        };
        let label = format!("{} {}", kind, target);
        if overflow_may_affect_target(deep_overflow_paths, &first.base_var, &first.field)
            || has_internal_calls
            || has_non_linear_flow
        {
            summary.add_potential(label);
            continue;
        }

        let runtime_var = match param_runtime.get(&first.base_var) {
            Some(v) => v,
            None => {
                summary.add_potential(label);
                continue;
            }
        };
        let accessor_fn = match resolve_container_accessor_fn(
            d,
            &first.base_var,
            &first.field,
            container_accessor_map,
        ) {
            Some(v) => v,
            None => {
                summary.add_potential(label);
                continue;
            }
        };

        let mut final_states: std::collections::BTreeMap<String, bool> =
            std::collections::BTreeMap::new();
        let mut exact = true;
        for eff in effects {
            match &eff.op {
                ContainerOp::Insert(key) => {
                    let Some(resolved) = resolve_container_key_expr(key, param_arg_values) else {
                        exact = false;
                        break;
                    };
                    final_states.insert(resolved, true);
                }
                ContainerOp::Remove(key) => {
                    let Some(resolved) = resolve_container_key_expr(key, param_arg_values) else {
                        exact = false;
                        break;
                    };
                    final_states.insert(resolved, false);
                }
                ContainerOp::Changed => {
                    exact = false;
                    break;
                }
            }
        }

        if !exact || final_states.is_empty() {
            summary.add_potential(label);
            continue;
        }

        for (key_expr, expected) in final_states {
            let suffix = sanitize_ident(&format!("{}_{}_{}", runtime_var, first.field, key_expr));
            let after_name = format!("after_{}_has", suffix);
            after.push(format!(
                "let {} = {}::{}(&{}, {});",
                after_name, d.module_name, accessor_fn, runtime_var, key_expr
            ));
            after.push(format!(
                "assert!({} == {}, {});",
                after_name,
                if expected { "true" } else { "false" },
                940 + idx
            ));
            idx += 1;
        }
        summary.add_asserted(label);
    }

    (before, after, summary)
}
