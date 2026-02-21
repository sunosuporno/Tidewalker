use super::*;

pub(super) fn build_chained_effect_map(
    decls: &[FnDecl],
    max_depth: usize,
) -> std::collections::HashMap<String, Vec<NumericEffect>> {
    let mut by_module_and_fn: std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    > = std::collections::HashMap::new();
    for d in decls {
        by_module_and_fn
            .entry(d.module_name.clone())
            .or_default()
            .insert(d.fn_name.clone(), d);
    }

    let mut cache: std::collections::HashMap<String, Vec<NumericEffect>> =
        std::collections::HashMap::new();
    for d in decls {
        let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
        let key = format!("{}::{}", d.module_name, d.fn_name);
        let resolved = collect_effects_for_fn(
            d,
            &by_module_and_fn,
            &mut cache,
            &mut visiting,
            0,
            max_depth,
        );
        cache.insert(key, resolved);
    }
    cache
}

pub(super) fn build_chained_vector_effect_map(
    decls: &[FnDecl],
    max_depth: usize,
) -> std::collections::HashMap<String, Vec<VectorEffect>> {
    let mut by_module_and_fn: std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    > = std::collections::HashMap::new();
    for d in decls {
        by_module_and_fn
            .entry(d.module_name.clone())
            .or_default()
            .insert(d.fn_name.clone(), d);
    }
    let mut cache: std::collections::HashMap<String, Vec<VectorEffect>> =
        std::collections::HashMap::new();
    for d in decls {
        let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
        let key = format!("{}::{}", d.module_name, d.fn_name);
        let resolved = collect_vector_effects_for_fn(
            d,
            &by_module_and_fn,
            &mut cache,
            &mut visiting,
            0,
            max_depth,
        );
        cache.insert(key, resolved);
    }
    cache
}

pub(super) fn build_chained_option_effect_map(
    decls: &[FnDecl],
    max_depth: usize,
) -> std::collections::HashMap<String, Vec<OptionEffect>> {
    let mut by_module_and_fn: std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    > = std::collections::HashMap::new();
    for d in decls {
        by_module_and_fn
            .entry(d.module_name.clone())
            .or_default()
            .insert(d.fn_name.clone(), d);
    }
    let mut cache: std::collections::HashMap<String, Vec<OptionEffect>> =
        std::collections::HashMap::new();
    for d in decls {
        let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
        let key = format!("{}::{}", d.module_name, d.fn_name);
        let resolved = collect_option_effects_for_fn(
            d,
            &by_module_and_fn,
            &mut cache,
            &mut visiting,
            0,
            max_depth,
        );
        cache.insert(key, resolved);
    }
    cache
}

pub(super) fn build_chained_string_effect_map(
    decls: &[FnDecl],
    max_depth: usize,
) -> std::collections::HashMap<String, Vec<StringEffect>> {
    let mut by_module_and_fn: std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    > = std::collections::HashMap::new();
    for d in decls {
        by_module_and_fn
            .entry(d.module_name.clone())
            .or_default()
            .insert(d.fn_name.clone(), d);
    }
    let mut cache: std::collections::HashMap<String, Vec<StringEffect>> =
        std::collections::HashMap::new();
    for d in decls {
        let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
        let key = format!("{}::{}", d.module_name, d.fn_name);
        let resolved = collect_string_effects_for_fn(
            d,
            &by_module_and_fn,
            &mut cache,
            &mut visiting,
            0,
            max_depth,
        );
        cache.insert(key, resolved);
    }
    cache
}

pub(super) fn build_deep_overflow_map(
    decls: &[FnDecl],
    max_depth: usize,
) -> std::collections::HashMap<String, std::collections::HashSet<String>> {
    let mut by_module_and_fn: std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    > = std::collections::HashMap::new();
    for d in decls {
        by_module_and_fn
            .entry(d.module_name.clone())
            .or_default()
            .insert(d.fn_name.clone(), d);
    }

    let mut cache: std::collections::HashMap<String, std::collections::HashSet<String>> =
        std::collections::HashMap::new();
    for d in decls {
        let mut visiting: std::collections::HashSet<String> = std::collections::HashSet::new();
        let key = format!("{}::{}", d.module_name, d.fn_name);
        let resolved = collect_deep_overflow_for_fn(
            d,
            &by_module_and_fn,
            &mut cache,
            &mut visiting,
            0,
            max_depth,
        );
        cache.insert(key, resolved);
    }
    cache
}

fn collect_deep_overflow_for_fn(
    d: &FnDecl,
    by_module_and_fn: &std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    >,
    cache: &mut std::collections::HashMap<String, std::collections::HashSet<String>>,
    visiting: &mut std::collections::HashSet<String>,
    depth: usize,
    max_depth: usize,
) -> std::collections::HashSet<String> {
    let key = format!("{}::{}", d.module_name, d.fn_name);
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if depth > max_depth || visiting.contains(&key) {
        return collect_forwarded_mut_paths_at_cutoff(d, by_module_and_fn.get(&d.module_name));
    }
    visiting.insert(key.clone());

    let mut out: std::collections::HashSet<String> = std::collections::HashSet::new();
    if let Some(module_map) = by_module_and_fn.get(&d.module_name) {
        for call in &d.calls {
            let callee = match module_map.get(&call.callee_fn) {
                Some(c) => *c,
                None => continue,
            };
            let child_paths = collect_deep_overflow_for_fn(
                callee,
                by_module_and_fn,
                cache,
                visiting,
                depth + 1,
                max_depth,
            );
            for path in child_paths {
                if let Some(mapped) = map_child_overflow_path_to_caller(&path, callee, call) {
                    out.insert(mapped);
                } else {
                    // If remapping fails (alias/expression), conservatively mark all mutable
                    // parameters of the caller as potentially changed.
                    for p in &d.params {
                        if p.ty.trim().starts_with("&mut") {
                            out.insert(p.name.clone());
                        }
                    }
                }
            }
        }
    }

    visiting.remove(&key);
    cache.insert(key, out.clone());
    out
}

fn collect_forwarded_mut_paths_at_cutoff(
    d: &FnDecl,
    module_map: Option<&std::collections::HashMap<String, &FnDecl>>,
) -> std::collections::HashSet<String> {
    let mut out: std::collections::HashSet<String> = std::collections::HashSet::new();
    let Some(module_map) = module_map else {
        return out;
    };
    let mut had_unresolved = false;
    for call in &d.calls {
        let callee = match module_map.get(&call.callee_fn) {
            Some(c) => *c,
            None => continue,
        };
        for (idx, param) in callee.params.iter().enumerate() {
            if !param.ty.trim().starts_with("&mut") {
                continue;
            }
            let arg = match call.arg_exprs.get(idx) {
                Some(a) => a,
                None => {
                    had_unresolved = true;
                    continue;
                }
            };
            if let Some(path) = canonicalize_arg_path(arg) {
                out.insert(path);
            } else {
                had_unresolved = true;
            }
        }
    }

    if had_unresolved {
        for p in &d.params {
            if p.ty.trim().starts_with("&mut") {
                out.insert(p.name.clone());
            }
        }
    }
    out
}

fn map_child_overflow_path_to_caller(
    child_path: &str,
    callee: &FnDecl,
    call: &CallSite,
) -> Option<String> {
    let (head, tail) = split_path_head_tail(child_path)?;
    let idx = callee.params.iter().position(|p| p.name == head)?;
    let arg = call.arg_exprs.get(idx)?;
    let arg_path = canonicalize_arg_path(arg)?;
    Some(format!("{}{}", arg_path, tail))
}

fn split_path_head_tail(path: &str) -> Option<(&str, &str)> {
    let first = path.split('.').next()?.trim();
    if !is_ident(first) {
        return None;
    }
    let tail = path.strip_prefix(first)?;
    Some((first, tail))
}

fn canonicalize_arg_path(expr: &str) -> Option<String> {
    let stripped = strip_ref_and_parens(expr.trim());
    if let Some((base, field)) = parse_field_access(stripped) {
        return Some(format!("{}.{}", base, field));
    }
    if is_ident(stripped) {
        return Some(stripped.to_string());
    }
    None
}

fn strip_ref_and_parens(raw: &str) -> &str {
    let mut s = raw.trim();
    loop {
        let mut changed = false;
        if let Some(rest) = s.strip_prefix("&mut") {
            s = rest.trim();
            changed = true;
        } else if let Some(rest) = s.strip_prefix('&') {
            s = rest.trim();
            changed = true;
        }
        if s.starts_with('(') && s.ends_with(')') && s.len() >= 2 {
            s = s[1..s.len() - 1].trim();
            changed = true;
        }
        if !changed {
            break;
        }
    }
    s
}

fn collect_option_effects_for_fn(
    d: &FnDecl,
    by_module_and_fn: &std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    >,
    cache: &mut std::collections::HashMap<String, Vec<OptionEffect>>,
    visiting: &mut std::collections::HashSet<String>,
    depth: usize,
    max_depth: usize,
) -> Vec<OptionEffect> {
    let key = format!("{}::{}", d.module_name, d.fn_name);
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if depth > max_depth || visiting.contains(&key) {
        return d.option_effects.clone();
    }
    visiting.insert(key.clone());

    let mut out = d.option_effects.clone();
    if let Some(module_map) = by_module_and_fn.get(&d.module_name) {
        for call in &d.calls {
            let callee = match module_map.get(&call.callee_fn) {
                Some(c) => *c,
                None => continue,
            };
            let callee_effects = collect_option_effects_for_fn(
                callee,
                by_module_and_fn,
                cache,
                visiting,
                depth + 1,
                max_depth,
            );
            for eff in callee_effects {
                let callee_idx = callee.params.iter().position(|p| p.name == eff.base_var);
                let idx = match callee_idx {
                    Some(v) => v,
                    None => continue,
                };
                let arg = match call.arg_exprs.get(idx) {
                    Some(a) => a.trim(),
                    None => continue,
                };
                if !is_ident(arg) {
                    continue;
                }
                out.push(OptionEffect {
                    base_var: arg.to_string(),
                    field: eff.field,
                    op: eff.op,
                });
            }
        }
    }

    let mut deduped = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for eff in out {
        let sig = format!("{}::{}::{:?}", eff.base_var, eff.field, eff.op);
        if seen.insert(sig) {
            deduped.push(eff);
        }
    }

    visiting.remove(&key);
    cache.insert(key, deduped.clone());
    deduped
}

fn collect_vector_effects_for_fn(
    d: &FnDecl,
    by_module_and_fn: &std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    >,
    cache: &mut std::collections::HashMap<String, Vec<VectorEffect>>,
    visiting: &mut std::collections::HashSet<String>,
    depth: usize,
    max_depth: usize,
) -> Vec<VectorEffect> {
    let key = format!("{}::{}", d.module_name, d.fn_name);
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if depth > max_depth || visiting.contains(&key) {
        return d.vector_effects.clone();
    }
    visiting.insert(key.clone());

    let mut out = d.vector_effects.clone();
    if let Some(module_map) = by_module_and_fn.get(&d.module_name) {
        for call in &d.calls {
            let callee = match module_map.get(&call.callee_fn) {
                Some(c) => *c,
                None => continue,
            };
            let callee_effects = collect_vector_effects_for_fn(
                callee,
                by_module_and_fn,
                cache,
                visiting,
                depth + 1,
                max_depth,
            );
            for eff in callee_effects {
                let callee_idx = callee.params.iter().position(|p| p.name == eff.base_var);
                let idx = match callee_idx {
                    Some(v) => v,
                    None => continue,
                };
                let arg = match call.arg_exprs.get(idx) {
                    Some(a) => a.trim(),
                    None => continue,
                };
                if !is_ident(arg) {
                    continue;
                }
                out.push(VectorEffect {
                    base_var: arg.to_string(),
                    field: eff.field,
                    op: eff.op,
                });
            }
        }
    }

    visiting.remove(&key);
    cache.insert(key, out.clone());
    out
}

fn collect_string_effects_for_fn(
    d: &FnDecl,
    by_module_and_fn: &std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    >,
    cache: &mut std::collections::HashMap<String, Vec<StringEffect>>,
    visiting: &mut std::collections::HashSet<String>,
    depth: usize,
    max_depth: usize,
) -> Vec<StringEffect> {
    let key = format!("{}::{}", d.module_name, d.fn_name);
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if depth > max_depth || visiting.contains(&key) {
        return d.string_effects.clone();
    }
    visiting.insert(key.clone());

    let mut out = d.string_effects.clone();
    if let Some(module_map) = by_module_and_fn.get(&d.module_name) {
        for call in &d.calls {
            let callee = match module_map.get(&call.callee_fn) {
                Some(c) => *c,
                None => continue,
            };
            let callee_effects = collect_string_effects_for_fn(
                callee,
                by_module_and_fn,
                cache,
                visiting,
                depth + 1,
                max_depth,
            );
            for eff in callee_effects {
                let callee_idx = callee.params.iter().position(|p| p.name == eff.base_var);
                let idx = match callee_idx {
                    Some(v) => v,
                    None => continue,
                };
                let arg = match call.arg_exprs.get(idx) {
                    Some(a) => a.trim(),
                    None => continue,
                };
                if !is_ident(arg) {
                    continue;
                }
                out.push(StringEffect {
                    base_var: arg.to_string(),
                    field: eff.field,
                });
            }
        }
    }

    let mut deduped = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for eff in out {
        let sig = format!("{}::{}", eff.base_var, eff.field);
        if seen.insert(sig) {
            deduped.push(eff);
        }
    }

    visiting.remove(&key);
    cache.insert(key, deduped.clone());
    deduped
}

fn collect_effects_for_fn(
    d: &FnDecl,
    by_module_and_fn: &std::collections::HashMap<
        String,
        std::collections::HashMap<String, &FnDecl>,
    >,
    cache: &mut std::collections::HashMap<String, Vec<NumericEffect>>,
    visiting: &mut std::collections::HashSet<String>,
    depth: usize,
    max_depth: usize,
) -> Vec<NumericEffect> {
    let key = format!("{}::{}", d.module_name, d.fn_name);
    if let Some(cached) = cache.get(&key) {
        return cached.clone();
    }
    if depth > max_depth || visiting.contains(&key) {
        return d.numeric_effects.clone();
    }
    visiting.insert(key.clone());

    let mut out = d.numeric_effects.clone();
    if let Some(module_map) = by_module_and_fn.get(&d.module_name) {
        for call in &d.calls {
            let callee = match module_map.get(&call.callee_fn) {
                Some(c) => *c,
                None => continue,
            };
            let callee_effects = collect_effects_for_fn(
                callee,
                by_module_and_fn,
                cache,
                visiting,
                depth + 1,
                max_depth,
            );
            for eff in callee_effects {
                let callee_idx = callee.params.iter().position(|p| p.name == eff.base_var);
                let idx = match callee_idx {
                    Some(v) => v,
                    None => continue,
                };
                let arg = match call.arg_exprs.get(idx) {
                    Some(a) => a.trim(),
                    None => continue,
                };
                if !is_ident(arg) {
                    continue;
                }
                out.push(NumericEffect {
                    base_var: arg.to_string(),
                    field: eff.field,
                    op: eff.op,
                });
            }
        }
    }

    let mut deduped = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    for eff in out {
        let sig = format!("{}::{}::{:?}", eff.base_var, eff.field, eff.op);
        if seen.insert(sig) {
            deduped.push(eff);
        }
    }

    visiting.remove(&key);
    cache.insert(key, deduped.clone());
    deduped
}
