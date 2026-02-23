use super::*;
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

pub(super) fn build_vector_assertion_lines(
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
