use super::*;
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

pub(super) fn build_option_assertion_lines(
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
