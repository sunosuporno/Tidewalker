use super::*;
fn overflow_may_affect_coin_target(
    deep_overflow_paths: &std::collections::HashSet<String>,
    base_var: &str,
) -> bool {
    deep_overflow_paths.contains(base_var)
}

pub(super) fn build_coin_assertion_lines(
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
