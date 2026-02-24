use super::catalog::ModuleBootstrapCatalog;
use super::*;
mod coin;
mod container;
mod numeric;
mod option;
mod orchestrator;
mod ownership;
mod summary;
mod vector;

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

fn parse_unsigned_literal_u64(raw: &str) -> Option<u64> {
    let mut lit = raw.trim();
    while lit.starts_with('(') && lit.ends_with(')') && lit.len() >= 2 {
        lit = &lit[1..lit.len() - 1];
        lit = lit.trim();
    }
    let lit = lit
        .strip_suffix("u64")
        .or_else(|| lit.strip_suffix("u128"))
        .or_else(|| lit.strip_suffix("u32"))
        .unwrap_or(lit)
        .trim();
    if lit.is_empty() {
        return None;
    }
    if !lit.chars().all(|c| c.is_ascii_digit() || c == '_') {
        return None;
    }
    let normalized = lit.replace('_', "");
    normalized.parse::<u64>().ok()
}

fn extract_assert_condition_for_render(stmt: &str) -> Option<String> {
    let idx = stmt.find("assert!(")?;
    let start = idx + "assert!(".len();
    let mut depth = 1i32;
    let mut end: Option<usize> = None;
    for (offset, ch) in stmt[start..].char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    end = Some(start + offset);
                    break;
                }
            }
            _ => {}
        }
    }
    let end_idx = end?;
    let inside = &stmt[start..end_idx];
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    for (i, ch) in inside.char_indices() {
        match ch {
            '(' => paren += 1,
            ')' => paren -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            ',' if paren == 0 && bracket == 0 && brace == 0 => {
                return Some(inside[..i].trim().to_string());
            }
            _ => {}
        }
    }
    Some(inside.trim().to_string())
}

fn choose_u64_arg_for_param_in_fn(name: &str, body_lines: &[String]) -> String {
    let param = remove_whitespace(name);
    let lower = name.to_ascii_lowercase();
    let mut min_inclusive: Option<u64> = None;
    let mut min_exclusive: Option<u64> = None;
    let mut max_inclusive: Option<u64> = None;
    let mut max_exclusive: Option<u64> = None;
    let mut equals: Option<u64> = None;
    let mut not_equals: std::collections::BTreeSet<u64> = std::collections::BTreeSet::new();

    for line in body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if !stmt.contains("assert!(") {
            continue;
        }
        let Some(cond) = extract_assert_condition_for_render(stmt) else {
            continue;
        };
        let norm = remove_whitespace(&cond);
        if !norm.contains(&param) {
            continue;
        }
        if norm.contains("||") {
            continue;
        }
        for clause in norm.split("&&") {
            if clause.is_empty() || !clause.contains(&param) {
                continue;
            }
            let apply_side = |lhs: &str,
                              rhs: &str,
                              op: &str,
                              rev: bool,
                              min_inclusive: &mut Option<u64>,
                              min_exclusive: &mut Option<u64>,
                              max_inclusive: &mut Option<u64>,
                              max_exclusive: &mut Option<u64>,
                              equals: &mut Option<u64>,
                              not_equals: &mut std::collections::BTreeSet<u64>| {
                if lhs != param {
                    return;
                }
                let Some(v) = parse_unsigned_literal_u64(rhs) else {
                    return;
                };
                match (op, rev) {
                    (">=", false) | ("<=", true) => {
                        *min_inclusive = Some(min_inclusive.map_or(v, |x| x.max(v)));
                    }
                    (">", false) | ("<", true) => {
                        *min_exclusive = Some(min_exclusive.map_or(v, |x| x.max(v)));
                    }
                    ("<=", false) | (">=", true) => {
                        *max_inclusive = Some(max_inclusive.map_or(v, |x| x.min(v)));
                    }
                    ("<", false) | (">", true) => {
                        *max_exclusive = Some(max_exclusive.map_or(v, |x| x.min(v)));
                    }
                    ("==", _) => {
                        *equals = Some(v);
                    }
                    ("!=", _) => {
                        not_equals.insert(v);
                    }
                    _ => {}
                }
            };

            for op in [">=", "<=", "==", "!=", ">", "<"] {
                if let Some((lhs, rhs)) = clause.split_once(op) {
                    apply_side(
                        lhs,
                        rhs,
                        op,
                        false,
                        &mut min_inclusive,
                        &mut min_exclusive,
                        &mut max_inclusive,
                        &mut max_exclusive,
                        &mut equals,
                        &mut not_equals,
                    );
                    apply_side(
                        rhs,
                        lhs,
                        op,
                        true,
                        &mut min_inclusive,
                        &mut min_exclusive,
                        &mut max_inclusive,
                        &mut max_exclusive,
                        &mut equals,
                        &mut not_equals,
                    );
                    break;
                }
            }
        }
    }

    let base = if lower.contains("ratio") { 50 } else { 1 };
    let mut candidate = equals.unwrap_or(base);

    if let Some(v) = min_inclusive {
        candidate = candidate.max(v);
    }
    if let Some(v) = min_exclusive {
        candidate = candidate.max(v.saturating_add(1));
    }
    if let Some(v) = max_inclusive {
        candidate = candidate.min(v);
    }
    if let Some(v) = max_exclusive {
        candidate = candidate.min(v.saturating_sub(1));
    }

    while not_equals.contains(&candidate) {
        candidate = candidate.saturating_add(1);
    }
    if let Some(v) = max_inclusive {
        if candidate > v {
            candidate = v;
        }
    }
    if let Some(v) = max_exclusive {
        if candidate >= v {
            candidate = v.saturating_sub(1);
        }
    }
    if let Some(v) = min_inclusive {
        if candidate < v {
            candidate = v;
        }
    }
    if let Some(v) = min_exclusive {
        if candidate <= v {
            candidate = v.saturating_add(1);
        }
    }
    while not_equals.contains(&candidate) {
        candidate = candidate.saturating_add(1);
    }

    candidate.to_string()
}

pub(super) struct RenderInputs<'a> {
    pub(super) accessor_map: &'a std::collections::HashMap<String, Vec<AccessorSig>>,
    pub(super) option_accessor_map: &'a std::collections::HashMap<String, Vec<OptionAccessorSig>>,
    pub(super) container_accessor_map:
        &'a std::collections::HashMap<String, Vec<ContainerAccessorSig>>,
    pub(super) bootstrap_catalog: &'a std::collections::HashMap<String, ModuleBootstrapCatalog>,
    pub(super) fn_lookup:
        &'a std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    pub(super) key_structs_by_module:
        &'a std::collections::HashMap<String, std::collections::HashSet<String>>,
    pub(super) numeric_effects: &'a [NumericEffect],
    pub(super) vector_effects: &'a [VectorEffect],
    pub(super) coin_effects: &'a [CoinEffect],
    pub(super) treasury_cap_effects: &'a [TreasuryCapEffect],
    pub(super) coin_notes: &'a [CoinNote],
    pub(super) option_effects: &'a [OptionEffect],
    pub(super) string_effects: &'a [StringEffect],
    pub(super) container_effects: &'a [ContainerEffect],
    pub(super) deep_overflow_paths: &'a std::collections::HashSet<String>,
}

pub(super) fn render_best_effort_test(
    d: &FnDecl,
    inputs: &RenderInputs<'_>,
) -> Option<Vec<String>> {
    orchestrator::render_best_effort_test(d, inputs)
}

fn build_coin_note_summary(coin_notes: &[CoinNote]) -> StateChangeSummary {
    summary::build_coin_note_summary(coin_notes)
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

fn build_ownership_checks(
    d: &FnDecl,
    param_arg_values: &std::collections::HashMap<String, String>,
    preexisting_shared_type_keys: &std::collections::HashSet<String>,
) -> (
    Vec<OwnershipTransferCheck>,
    Vec<OwnershipShareCheck>,
    StateChangeSummary,
) {
    ownership::build_ownership_checks(d, param_arg_values, preexisting_shared_type_keys)
}

fn resolve_numeric_operand(
    token: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
) -> Option<String> {
    numeric::resolve_numeric_operand(token, param_arg_values)
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

fn overflow_may_affect_target(
    deep_overflow_paths: &std::collections::HashSet<String>,
    base_var: &str,
    field: &str,
) -> bool {
    numeric::overflow_may_affect_target(deep_overflow_paths, base_var, field)
}

fn build_numeric_assertion_lines(
    d: &FnDecl,
    numeric_effects: &[NumericEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    param_arg_values: &std::collections::HashMap<String, String>,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    numeric::build_numeric_assertion_lines(
        d,
        numeric_effects,
        param_runtime,
        param_arg_values,
        accessor_map,
        deep_overflow_paths,
    )
}

fn build_vector_assertion_lines(
    d: &FnDecl,
    vector_effects: &[VectorEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    vector::build_vector_assertion_lines(
        d,
        vector_effects,
        param_runtime,
        accessor_map,
        deep_overflow_paths,
    )
}

fn build_coin_assertion_lines(
    d: &FnDecl,
    coin_effects: &[CoinEffect],
    param_coin_runtime: &std::collections::HashMap<String, String>,
    param_coin_is_ref: &std::collections::HashMap<String, bool>,
    param_arg_values: &std::collections::HashMap<String, String>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    coin::build_coin_assertion_lines(
        d,
        coin_effects,
        param_coin_runtime,
        param_coin_is_ref,
        param_arg_values,
        deep_overflow_paths,
    )
}

fn build_option_assertion_lines(
    d: &FnDecl,
    option_effects: &[OptionEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    option_accessor_map: &std::collections::HashMap<String, Vec<OptionAccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    option::build_option_assertion_lines(
        d,
        option_effects,
        param_runtime,
        option_accessor_map,
        deep_overflow_paths,
    )
}

fn build_string_summary(
    string_effects: &[StringEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> StateChangeSummary {
    summary::build_string_summary(string_effects, param_runtime, deep_overflow_paths)
}

fn build_container_assertion_lines(
    d: &FnDecl,
    container_effects: &[ContainerEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    param_arg_values: &std::collections::HashMap<String, String>,
    container_accessor_map: &std::collections::HashMap<String, Vec<ContainerAccessorSig>>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> (Vec<String>, Vec<String>, StateChangeSummary) {
    container::build_container_assertion_lines(
        d,
        container_effects,
        param_runtime,
        param_arg_values,
        container_accessor_map,
        deep_overflow_paths,
    )
}

fn build_deep_chain_summary(
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> StateChangeSummary {
    summary::build_deep_chain_summary(deep_overflow_paths)
}

fn build_mut_param_fallback_summary(
    d: &FnDecl,
    existing_summary: &StateChangeSummary,
) -> StateChangeSummary {
    summary::build_mut_param_fallback_summary(d, existing_summary)
}

fn render_state_change_summary_lines(summary: &StateChangeSummary) -> Vec<String> {
    summary::render_state_change_summary_lines(summary)
}
