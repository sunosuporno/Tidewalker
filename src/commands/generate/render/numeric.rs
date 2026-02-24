use super::*;
fn resolve_read_expr(
    d: &FnDecl,
    eff: &NumericEffect,
    runtime_var: &str,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
) -> Option<String> {
    let param = d.params.iter().find(|p| p.name == eff.base_var)?;
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = accessor_map.get(&d.module_name)?;
    let getter_name = module_accessors
        .iter()
        .find(|a| {
            a.param_ty == param_obj_ty
                && (a.fn_name == eff.field || a.fn_name == format!("get_{}", eff.field))
        })
        .map(|a| a.fn_name.clone())?;
    Some(format!(
        "{}::{}(&{})",
        d.module_name, getter_name, runtime_var
    ))
}

pub(super) fn overflow_may_affect_target(
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

pub(super) fn build_numeric_assertion_lines(
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

pub(super) fn resolve_numeric_operand(
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
