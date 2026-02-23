use super::*;

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
