use super::*;

fn simplify_state_target(raw: &str) -> String {
    let s = raw.trim();
    for prefix in [
        "operator ",
        "vector ",
        "option ",
        "string ",
        "state ",
        "coin ",
        "treasury_cap ",
        "cap auth ",
        "cap mint ",
        "cap transfer ",
        "cap share ",
        "ownership transfer ",
        "ownership share ",
        "object delete ",
        "table ",
        "vec_map ",
        "vec_set ",
        "bag ",
        "dynamic_field ",
    ] {
        if let Some(rest) = s.strip_prefix(prefix) {
            return rest.trim().to_string();
        }
    }
    s.to_string()
}

fn simplified_targets(
    items: &std::collections::BTreeSet<String>,
) -> std::collections::BTreeSet<String> {
    let mut out = std::collections::BTreeSet::new();
    for item in items {
        let simplified = simplify_state_target(item);
        if !simplified.is_empty() {
            out.insert(simplified);
        }
    }
    out
}

pub(super) fn build_coin_note_summary(coin_notes: &[CoinNote]) -> StateChangeSummary {
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

pub(super) fn build_string_summary(
    string_effects: &[StringEffect],
    param_runtime: &std::collections::HashMap<String, String>,
    deep_overflow_paths: &std::collections::HashSet<String>,
) -> StateChangeSummary {
    let mut summary = StateChangeSummary::default();
    for eff in string_effects {
        if super::overflow_may_affect_target(deep_overflow_paths, &eff.base_var, &eff.field) {
            summary.add_potential(format!("string {}.{}", eff.base_var, eff.field));
            continue;
        }
        if param_runtime.contains_key(&eff.base_var) {
            summary.add_potential(format!("string {}.{}", eff.base_var, eff.field));
        }
    }
    summary
}

pub(super) fn build_deep_chain_summary(
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

pub(super) fn render_state_change_summary_lines(summary: &StateChangeSummary) -> Vec<String> {
    let asserted = simplified_targets(&summary.asserted);
    let mut potential = simplified_targets(&summary.potential);
    for a in &asserted {
        potential.remove(a);
    }

    let mut out = Vec::new();
    out.push("// Tidewalker state changes:".to_string());
    if asserted.is_empty() {
        out.push("// asserted: (none)".to_string());
    } else {
        out.push(format!(
            "// asserted: {}",
            asserted.iter().cloned().collect::<Vec<_>>().join(", ")
        ));
    }
    if potential.is_empty() {
        out.push("// potential_change: (none)".to_string());
    } else {
        out.push(format!(
            "// potential_change: {}",
            potential.iter().cloned().collect::<Vec<_>>().join(", ")
        ));
    }
    out
}
