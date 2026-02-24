use super::*;

pub(super) fn extract_public_fns(content: &str) -> Vec<FnDecl> {
    let lines: Vec<&str> = content.lines().collect();
    let mut module_name = String::new();
    for line in &lines {
        let t = line.trim();
        if t.starts_with("module ") {
            if let Some(rest) = t.strip_prefix("module ") {
                let until = rest.split_whitespace().next().unwrap_or(rest);
                module_name = until.trim_end_matches(';').to_string();
            }
            break;
        }
    }
    let option_fields_by_type = extract_option_fields_by_type(&lines);
    let string_fields_by_type = extract_string_fields_by_type(&lines);

    let mut out = Vec::new();
    let mut pending_test_only_attr = false;
    let mut pending_requires: Vec<String> = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim();
        if t == "#[test_only]" {
            pending_test_only_attr = true;
            i += 1;
            continue;
        }
        if t.contains("@tidewalker") && t.contains("requires") {
            // Example: `/// @tidewalker requires increment`
            if let Some((_, after)) = t.split_once("requires") {
                for part in after
                    .trim()
                    .split(|c: char| c == ',' || c.is_whitespace())
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty())
                {
                    pending_requires.push(part.to_string());
                }
            }
            i += 1;
            continue;
        }
        let (is_fn_decl, is_public, is_entry, name, type_params) =
            if let Some(after) = t.strip_prefix("public entry fun ") {
                let (name, type_params) = extract_fn_name_and_type_params(after);
                (true, true, true, name, type_params)
            } else if let Some(after) = t.strip_prefix("entry fun ") {
                let (name, type_params) = extract_fn_name_and_type_params(after);
                (true, false, true, name, type_params)
            } else if let Some(after) = t.strip_prefix("public fun ") {
                let (name, type_params) = extract_fn_name_and_type_params(after);
                (true, true, false, name, type_params)
            } else if let Some(after) = t.strip_prefix("public(package) fun ") {
                let (name, type_params) = extract_fn_name_and_type_params(after);
                // Treat package-visibility functions as internal helpers for Tidewalker test-target
                // selection. We still parse them for call-chain analysis.
                (true, false, false, name, type_params)
            } else if let Some(after) = t.strip_prefix("fun ") {
                let (name, type_params) = extract_fn_name_and_type_params(after);
                (true, false, false, name, type_params)
            } else {
                (false, false, false, String::new(), Vec::new())
            };
        if !is_fn_decl || name.is_empty() {
            // Keep #[test_only] pending across blank lines and comments (docs often sit between
            // the attribute and the function). Clear only on non-comment, non-empty content.
            if pending_test_only_attr
                && (t.is_empty() || t.starts_with("//") || t.starts_with("///"))
            {
                i += 1;
                continue;
            }
            if !t.is_empty() && !t.starts_with("//") && !t.starts_with("///") {
                pending_test_only_attr = false;
                pending_requires.clear();
            }
            i += 1;
            continue;
        }
        if name == "init" {
            pending_test_only_attr = false;
            pending_requires.clear();
            i += 1;
            continue;
        }
        let is_test_only = pending_test_only_attr;

        let fn_start = i;
        // Collect header across lines until we see ')'.
        let mut header = t.to_string();
        let mut j = i + 1;
        while j < lines.len() && !header.contains(')') {
            header.push(' ');
            header.push_str(lines[j].trim());
            j += 1;
        }
        pending_test_only_attr = false;

        let params_str = match header
            .split_once('(')
            .and_then(|(_, rest)| rest.split_once(')'))
            .map(|(p, _)| p)
        {
            Some(p) => p,
            None => continue,
        };
        let return_ty = extract_return_type(&header);
        let params = split_params(params_str)
            .into_iter()
            .filter_map(|p| {
                p.split_once(':').map(|(name, ty)| ParamDecl {
                    name: name.trim().to_string(),
                    ty: ty.trim().to_string(),
                })
            })
            .collect::<Vec<_>>();

        let mut brace_depth = 0i32;
        let mut body_start: Option<usize> = None;
        let mut body_end: Option<usize> = None;
        for (k, l) in lines.iter().enumerate().skip(fn_start) {
            for c in l.chars() {
                if c == '{' {
                    brace_depth += 1;
                    if brace_depth == 1 {
                        body_start = Some(k + 1);
                    }
                } else if c == '}' {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        body_end = Some(k);
                        break;
                    }
                }
            }
            if body_end.is_some() {
                break;
            }
        }
        let body_lines = match (body_start, body_end) {
            (Some(s), Some(e)) if s <= e => lines[s..=e]
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        let numeric_effects = extract_numeric_effects_from_body(&body_lines);
        let vector_effects = extract_vector_effects_from_body(&body_lines);
        let coin_effects = extract_coin_effects_from_body(&body_lines, &params);
        let treasury_cap_effects = extract_treasury_cap_effects_from_body(&body_lines, &params);
        let coin_notes = extract_coin_notes_from_body(&body_lines);
        let option_effects =
            extract_option_effects_from_body(&body_lines, &params, &option_fields_by_type);
        let string_effects =
            extract_string_effects_from_body(&body_lines, &params, &string_fields_by_type);
        let container_effects = extract_container_effects_from_body(&body_lines);
        let calls = extract_same_module_calls_from_body(&body_lines, &module_name);

        out.push(FnDecl {
            module_name: module_name.clone(),
            fn_name: name,
            type_params,
            params,
            return_ty,
            body_lines,
            is_public,
            is_entry,
            is_test_only,
            requires: std::mem::take(&mut pending_requires),
            numeric_effects,
            vector_effects,
            coin_effects,
            treasury_cap_effects,
            coin_notes,
            option_effects,
            string_effects,
            container_effects,
            calls,
        });
        i = body_end.map(|e| e + 1).unwrap_or(j);
    }
    out
}

fn extract_fn_name_and_type_params(after: &str) -> (String, Vec<String>) {
    let trimmed = after.trim();
    let mut name = String::new();
    let mut idx = 0usize;
    for (i, ch) in trimmed.char_indices() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            name.push(ch);
            idx = i + ch.len_utf8();
        } else {
            idx = i;
            break;
        }
    }
    if name.is_empty() {
        return (String::new(), Vec::new());
    }

    let rest = trimmed[idx..].trim_start();
    if !rest.starts_with('<') {
        return (name, Vec::new());
    }
    let mut depth = 0i32;
    let mut close_idx: Option<usize> = None;
    for (i, ch) in rest.char_indices() {
        if ch == '<' {
            depth += 1;
        } else if ch == '>' {
            depth -= 1;
            if depth == 0 {
                close_idx = Some(i);
                break;
            }
        }
    }
    let Some(end) = close_idx else {
        return (name, Vec::new());
    };
    let generic_body = &rest[1..end];
    let mut params = Vec::new();
    for raw in split_params(generic_body) {
        let base = raw.split(':').next().unwrap_or("").trim();
        if !base.is_empty() {
            params.push(base.to_string());
        }
    }
    (name, params)
}

pub(super) fn split_params(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut angle = 0i32;
    for ch in s.chars() {
        match ch {
            '<' => {
                angle += 1;
                cur.push(ch);
            }
            '>' => {
                angle -= 1;
                cur.push(ch);
            }
            ',' if angle == 0 => {
                let t = cur.trim();
                if !t.is_empty() {
                    out.push(t.to_string());
                }
                cur.clear();
            }
            _ => cur.push(ch),
        }
    }
    let t = cur.trim();
    if !t.is_empty() {
        out.push(t.to_string());
    }
    out
}

pub(super) fn extract_option_fields_by_type(
    lines: &[&str],
) -> std::collections::HashMap<String, std::collections::HashSet<String>> {
    let mut out: std::collections::HashMap<String, std::collections::HashSet<String>> =
        std::collections::HashMap::new();
    let mut i = 0usize;
    while i < lines.len() {
        let t = lines[i].trim();
        let rest = if let Some(r) = t.strip_prefix("public struct ") {
            r
        } else if let Some(r) = t.strip_prefix("struct ") {
            r
        } else {
            i += 1;
            continue;
        };
        let struct_name = rest
            .split(|c: char| c == '{' || c == '<' || c.is_whitespace())
            .next()
            .unwrap_or("")
            .trim();
        if struct_name.is_empty() {
            i += 1;
            continue;
        }

        let mut brace_depth = 0i32;
        let mut saw_open = false;
        let mut j = i;
        let mut fields: std::collections::HashSet<String> = std::collections::HashSet::new();
        while j < lines.len() {
            let line = lines[j].trim();
            if saw_open {
                if let Some((field, ty_raw)) = line.split_once(':') {
                    let field = field.trim().trim_end_matches(',').trim();
                    let ty = ty_raw
                        .split("//")
                        .next()
                        .unwrap_or("")
                        .trim()
                        .trim_end_matches(',')
                        .trim();
                    if is_ident(field) && is_option_type(ty) {
                        fields.insert(field.to_string());
                    }
                }
            }
            for ch in line.chars() {
                if ch == '{' {
                    brace_depth += 1;
                    saw_open = true;
                } else if ch == '}' {
                    brace_depth -= 1;
                }
            }
            if saw_open && brace_depth <= 0 {
                break;
            }
            j += 1;
        }
        if !fields.is_empty() {
            out.insert(type_key_from_type_name(struct_name), fields);
        }
        i = j.saturating_add(1);
    }
    out
}

pub(super) fn extract_string_fields_by_type(
    lines: &[&str],
) -> std::collections::HashMap<String, std::collections::HashSet<String>> {
    let mut out: std::collections::HashMap<String, std::collections::HashSet<String>> =
        std::collections::HashMap::new();
    let mut i = 0usize;
    while i < lines.len() {
        let t = lines[i].trim();
        let rest = if let Some(r) = t.strip_prefix("public struct ") {
            r
        } else if let Some(r) = t.strip_prefix("struct ") {
            r
        } else {
            i += 1;
            continue;
        };
        let struct_name = rest
            .split(|c: char| c == '{' || c == '<' || c.is_whitespace())
            .next()
            .unwrap_or("")
            .trim();
        if struct_name.is_empty() {
            i += 1;
            continue;
        }

        let mut brace_depth = 0i32;
        let mut saw_open = false;
        let mut j = i;
        let mut fields: std::collections::HashSet<String> = std::collections::HashSet::new();
        while j < lines.len() {
            let line = lines[j].trim();
            if saw_open {
                if let Some((field, ty_raw)) = line.split_once(':') {
                    let field = field.trim().trim_end_matches(',').trim();
                    let ty = ty_raw
                        .split("//")
                        .next()
                        .unwrap_or("")
                        .trim()
                        .trim_end_matches(',')
                        .trim();
                    if is_ident(field) && is_string_type(ty) {
                        fields.insert(field.to_string());
                    }
                }
            }
            for ch in line.chars() {
                if ch == '{' {
                    brace_depth += 1;
                    saw_open = true;
                } else if ch == '}' {
                    brace_depth -= 1;
                }
            }
            if saw_open && brace_depth <= 0 {
                break;
            }
            j += 1;
        }
        if !fields.is_empty() {
            out.insert(type_key_from_type_name(struct_name), fields);
        }
        i = j.saturating_add(1);
    }
    out
}

pub(super) fn extract_numeric_effects_from_body(body_lines: &[String]) -> Vec<NumericEffect> {
    let mut effects = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() || no_comments.starts_with("assert!") {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        if let Some((name, target)) = parse_alias_binding(stmt) {
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
            }
        }
        if let Some((base_var, field, op)) = parse_balance_numeric_effect(stmt, &aliases) {
            effects.push(NumericEffect {
                base_var,
                field,
                op,
            });
            continue;
        }
        let (lhs, rhs) = match stmt.split_once('=') {
            Some((l, r)) => (l.trim(), r.trim()),
            None => continue,
        };
        if lhs.ends_with('!') || lhs.contains("==") {
            continue;
        }
        let (base_var, field) = match parse_field_access(lhs)
            .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
        {
            Some(parts) => parts,
            None => continue,
        };
        let lhs_norm = remove_whitespace(lhs);
        let rhs_norm = remove_whitespace(rhs);

        let op = if let Some(rest) = rhs_norm.strip_prefix(&lhs_norm) {
            let (op_char, raw_lit) = match rest.chars().next() {
                Some(c @ ('+' | '-' | '*' | '/' | '%')) => (c, rest[1..].trim()),
                _ => continue,
            };
            if let Some(lit) = parse_numeric_literal(raw_lit) {
                match op_char {
                    '+' => NumericOp::Add(lit),
                    '-' => NumericOp::Sub(lit),
                    '*' => NumericOp::Mul(lit),
                    '/' => NumericOp::Div(lit),
                    '%' => NumericOp::Mod(lit),
                    _ => continue,
                }
            } else if is_ident(raw_lit) {
                match op_char {
                    '+' => NumericOp::Add(raw_lit.to_string()),
                    '-' => NumericOp::Sub(raw_lit.to_string()),
                    '*' => NumericOp::Mul(raw_lit.to_string()),
                    '/' => NumericOp::Div(raw_lit.to_string()),
                    '%' => NumericOp::Mod(raw_lit.to_string()),
                    _ => continue,
                }
            } else {
                // We can still assert a state change happened even when exact delta is unknown.
                NumericOp::Changed
            }
        } else if let Some(v) = parse_numeric_literal(&rhs_norm) {
            NumericOp::Set(v)
        } else if is_ident(&rhs_norm) {
            NumericOp::Set(rhs_norm)
        } else {
            continue;
        };

        effects.push(NumericEffect {
            base_var,
            field,
            op,
        });
    }
    effects
}

pub(super) fn extract_vector_effects_from_body(body_lines: &[String]) -> Vec<VectorEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for line in body_lines {
        let t = line
            .split("//")
            .next()
            .unwrap_or("")
            .trim()
            .trim_end_matches(';')
            .trim();
        if t.is_empty() {
            continue;
        }
        if let Some((name, target)) = parse_alias_binding(t) {
            aliases.insert(name, target);
            continue;
        }
        if t.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(t) {
                aliases.remove(&name);
            }
        }
        if t.contains("==") {
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "push_back(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PushBack,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "pop_back(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PopBack,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "insert(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Insert,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "remove(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Remove,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "swap_remove(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::SwapRemove,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "append(", &aliases) {
            let src = extract_method_args(t, "append(")
                .and_then(|args| args.first().cloned())
                .and_then(|arg| parse_vector_target_expr(&arg, &aliases));
            if let Some((src_base_var, src_field)) = src {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::Append {
                        src_base_var,
                        src_field,
                    },
                });
            } else {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::ContentChanged,
                });
            }
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "reverse(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_method_target(t, "swap(", &aliases) {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((base_var, field)) =
            parse_vector_namespace_target(t, "vector::push_back(", &aliases)
        {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PushBack,
            });
            continue;
        }
        if let Some((base_var, field)) =
            parse_vector_namespace_target(t, "vector::pop_back(", &aliases)
        {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::PopBack,
            });
            continue;
        }
        if let Some((base_var, field)) =
            parse_vector_namespace_target(t, "vector::insert(", &aliases)
        {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Insert,
            });
            continue;
        }
        if let Some((base_var, field)) =
            parse_vector_namespace_target(t, "vector::remove(", &aliases)
        {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::Remove,
            });
            continue;
        }
        if let Some((base_var, field)) =
            parse_vector_namespace_target(t, "vector::swap_remove(", &aliases)
        {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::SwapRemove,
            });
            continue;
        }
        if let Some((base_var, field)) =
            parse_vector_namespace_target(t, "vector::append(", &aliases)
        {
            let src = extract_namespace_args(t, "vector::append(")
                .and_then(|args| args.get(1).cloned())
                .and_then(|arg| parse_vector_target_expr(&arg, &aliases));
            if let Some((src_base_var, src_field)) = src {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::Append {
                        src_base_var,
                        src_field,
                    },
                });
            } else {
                out.push(VectorEffect {
                    base_var,
                    field,
                    op: VectorOp::ContentChanged,
                });
            }
            continue;
        }
        if let Some((base_var, field)) =
            parse_vector_namespace_target(t, "vector::reverse(", &aliases)
        {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((base_var, field)) = parse_vector_namespace_target(t, "vector::swap(", &aliases)
        {
            out.push(VectorEffect {
                base_var,
                field,
                op: VectorOp::ContentChanged,
            });
            continue;
        }
        if let Some((lhs, _)) = t.split_once('=') {
            let lhs = lhs.trim();
            if lhs.contains('[') && lhs.contains(']') {
                let base = lhs.split('[').next().unwrap_or("").trim();
                if let Some((base_var, field)) = parse_vector_target_expr(base, &aliases) {
                    out.push(VectorEffect {
                        base_var,
                        field,
                        op: VectorOp::ContentChanged,
                    });
                }
            }
        }
    }
    out
}

pub(super) fn extract_coin_effects_from_body(
    body_lines: &[String],
    params: &[ParamDecl],
) -> Vec<CoinEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut minted_vars: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut coin_params: std::collections::HashSet<String> = std::collections::HashSet::new();
    for p in params {
        if is_coin_type(&normalize_param_object_type(&p.ty)) {
            coin_params.insert(p.name.clone());
        }
    }

    for line in body_lines {
        let t = line
            .split("//")
            .next()
            .unwrap_or("")
            .trim()
            .trim_end_matches(';')
            .trim();
        if t.is_empty() {
            continue;
        }
        if let Some((name, target)) = parse_alias_binding(t) {
            aliases.insert(name, target);
            continue;
        }
        if t.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(t) {
                aliases.remove(&name);
                minted_vars.remove(&name);
                if let Some((_, rhs)) = t.split_once('=') {
                    if let Some(amount) = parse_mint_amount_from_expr(rhs.trim()) {
                        minted_vars.insert(name, amount);
                    }
                }
            }
        }
        if t.contains("==") {
            continue;
        }

        if let Some((base_var, amount)) = parse_coin_burn_with_split(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Burn(amount),
                });
            }
            continue;
        }

        if let Some((base_var, amount)) = parse_coin_split_method(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Split(amount),
                });
            }
            continue;
        }
        if let Some((base_var, amount)) = parse_coin_split_namespace(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Split(amount),
                });
            }
            continue;
        }
        if let Some((base_var, amount)) = parse_coin_join_mint_method(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Mint(amount),
                });
            }
            continue;
        }
        if let Some((base_var, amount)) = parse_coin_join_mint_namespace(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Mint(amount),
                });
            }
            continue;
        }
        if let Some((base_var, src_base_var)) = parse_coin_join_method(t, &aliases) {
            if let Some(amount) = minted_vars.get(&src_base_var) {
                if coin_params.contains(&base_var) {
                    out.push(CoinEffect {
                        base_var,
                        op: CoinOp::Mint(amount.clone()),
                    });
                }
            } else if coin_params.contains(&base_var) && coin_params.contains(&src_base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Join { src_base_var },
                });
            } else if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Changed,
                });
            }
            continue;
        }
        if let Some((base_var, src_base_var)) = parse_coin_join_namespace(t, &aliases) {
            if let Some(amount) = minted_vars.get(&src_base_var) {
                if coin_params.contains(&base_var) {
                    out.push(CoinEffect {
                        base_var,
                        op: CoinOp::Mint(amount.clone()),
                    });
                }
            } else if coin_params.contains(&base_var) && coin_params.contains(&src_base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Join { src_base_var },
                });
            } else if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Changed,
                });
            }
            continue;
        }
        if let Some(base_var) = parse_coin_transfer_arg(t, &aliases) {
            if coin_params.contains(&base_var) {
                out.push(CoinEffect {
                    base_var,
                    op: CoinOp::Changed,
                });
            }
        }
    }

    out
}

pub(super) fn extract_treasury_cap_effects_from_body(
    body_lines: &[String],
    params: &[ParamDecl],
) -> Vec<TreasuryCapEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut split_coin_amounts: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut treasury_cap_params: std::collections::HashSet<String> =
        std::collections::HashSet::new();

    for p in params {
        if p.ty.trim().starts_with("&mut")
            && is_treasury_cap_type(&normalize_param_object_type(&p.ty))
        {
            treasury_cap_params.insert(p.name.clone());
        }
    }

    for line in body_lines {
        let t = line
            .split("//")
            .next()
            .unwrap_or("")
            .trim()
            .trim_end_matches(';')
            .trim();
        if t.is_empty() {
            continue;
        }
        if let Some((name, target)) = parse_alias_binding(t) {
            aliases.insert(name, target);
            continue;
        }
        if t.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(t) {
                aliases.remove(&name);
                split_coin_amounts.remove(&name);
                if let Some((_, rhs)) = t.split_once('=') {
                    if let Some((_, amount)) = parse_coin_split_expr(rhs.trim(), &aliases) {
                        split_coin_amounts.insert(name, amount);
                    }
                }
            }
        }
        if t.contains("==") {
            continue;
        }

        if let Some((cap_var, amount)) = parse_treasury_mint_call(t, &aliases) {
            if treasury_cap_params.contains(&cap_var) {
                out.push(TreasuryCapEffect {
                    base_var: cap_var,
                    op: TreasuryCapOp::Mint(amount),
                });
            }
            continue;
        }
        if let Some((cap_var, burn_amount)) =
            parse_treasury_burn_call(t, &aliases, &split_coin_amounts)
        {
            if treasury_cap_params.contains(&cap_var) {
                out.push(TreasuryCapEffect {
                    base_var: cap_var,
                    op: match burn_amount {
                        Some(amount) => TreasuryCapOp::Burn(amount),
                        None => TreasuryCapOp::Changed,
                    },
                });
            }
        }
    }

    out
}

pub(super) fn extract_coin_notes_from_body(body_lines: &[String]) -> Vec<CoinNote> {
    let mut out = Vec::new();
    let mut seen: std::collections::HashSet<CoinNote> = std::collections::HashSet::new();
    for line in body_lines {
        let stmt = line
            .split("//")
            .next()
            .unwrap_or("")
            .trim()
            .trim_end_matches(';')
            .trim();
        if stmt.is_empty() {
            continue;
        }
        let lower = stmt.to_ascii_lowercase();
        let has_mint = (lower.contains("coin::mint(") || lower.contains("::mint("))
            && !lower.contains("mint_for_testing(");
        if has_mint && seen.insert(CoinNote::MintFlow) {
            out.push(CoinNote::MintFlow);
        }
        if (lower.contains("coin::burn(") || lower.contains("::burn("))
            && seen.insert(CoinNote::BurnFlow)
        {
            out.push(CoinNote::BurnFlow);
        }
        let has_stake = lower.contains("request_add_stake(")
            || lower.contains("request_withdraw_stake(")
            || lower.contains("staking::")
            || lower.contains("staking_pool::")
            || lower.contains("validator::")
            || lower.contains("stake::");
        if has_stake && seen.insert(CoinNote::StakeFlow) {
            out.push(CoinNote::StakeFlow);
        }
    }
    out
}

pub(super) fn parse_coin_target_expr(
    arg: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let first_arg = arg
        .trim()
        .trim_start_matches("&mut")
        .trim_start_matches('&')
        .trim();
    if !is_ident(first_arg) {
        return None;
    }
    let resolved = resolve_alias_path(first_arg, aliases);
    if is_ident(&resolved) {
        Some(resolved)
    } else {
        None
    }
}

pub(super) fn normalize_coin_amount_expr(raw: &str) -> Option<String> {
    let t = raw.trim();
    if t.is_empty() {
        return None;
    }
    if let Some(lit) = parse_numeric_literal(t) {
        return Some(lit);
    }
    if is_ident(t) {
        return Some(t.to_string());
    }
    Some(remove_whitespace(t))
}

pub(super) fn parse_coin_split_method(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let idx = line.find(".split(")?;
    let before = line[..idx]
        .split('=')
        .next_back()?
        .trim()
        .trim_end_matches('.')
        .trim();
    let base_var = parse_coin_target_expr(before, aliases)?;
    let args = extract_method_args(line, "split(")?;
    let amount = normalize_coin_amount_expr(args.first()?.as_str())?;
    Some((base_var, amount))
}

pub(super) fn parse_coin_split_namespace(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::split(")?;
    let base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    let amount = normalize_coin_amount_expr(args.get(1)?.as_str())?;
    Some((base_var, amount))
}

pub(super) fn parse_coin_join_method(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let idx = line.find(".join(")?;
    let before = line[..idx]
        .split('=')
        .next_back()?
        .trim()
        .trim_end_matches('.')
        .trim();
    let base_var = parse_coin_target_expr(before, aliases)?;
    let args = extract_method_args(line, "join(")?;
    let src_base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    Some((base_var, src_base_var))
}

pub(super) fn parse_coin_join_namespace(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::join(")?;
    let base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    let src_base_var = parse_coin_target_expr(args.get(1)?.as_str(), aliases)?;
    Some((base_var, src_base_var))
}

pub(super) fn parse_coin_transfer_arg(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let args = extract_namespace_args(line, "transfer::public_transfer(")
        .or_else(|| extract_namespace_args(line, "transfer::transfer("))?;
    parse_coin_target_expr(args.first()?.as_str(), aliases)
}

pub(super) fn parse_coin_join_mint_method(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let idx = line.find(".join(")?;
    let before = line[..idx]
        .split('=')
        .next_back()?
        .trim()
        .trim_end_matches('.')
        .trim();
    let base_var = parse_coin_target_expr(before, aliases)?;
    let args = extract_method_args(line, "join(")?;
    let mint_amount = parse_mint_amount_from_expr(args.first()?.as_str())?;
    Some((base_var, mint_amount))
}

pub(super) fn parse_coin_join_mint_namespace(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::join(")?;
    let base_var = parse_coin_target_expr(args.first()?.as_str(), aliases)?;
    let mint_amount = parse_mint_amount_from_expr(args.get(1)?.as_str())?;
    Some((base_var, mint_amount))
}

pub(super) fn parse_coin_burn_with_split(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let args = extract_namespace_args(line, "coin::burn(")
        .or_else(|| extract_call_args_by_suffix(line, "::burn("))?;
    let burn_arg = args.get(1)?.as_str();
    parse_coin_split_expr(burn_arg, aliases)
}

pub(super) fn parse_coin_split_expr(
    expr: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let t = expr.trim();
    if let Some((base, amount)) = parse_coin_split_method(t, aliases) {
        return Some((base, amount));
    }
    if let Some((base, amount)) = parse_coin_split_namespace(t, aliases) {
        return Some((base, amount));
    }
    None
}

pub(super) fn parse_mint_amount_from_expr(expr: &str) -> Option<String> {
    let t = expr.trim();
    if t.contains("mint_for_testing(") {
        return None;
    }
    let args = extract_namespace_args(t, "coin::mint(")
        .or_else(|| extract_call_args_by_suffix(t, "::mint("))?;
    let raw = args.get(1)?.as_str();
    normalize_coin_amount_expr(raw)
}

pub(super) fn parse_treasury_mint_call(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    if !line.contains("coin::mint(") || line.contains("mint_for_testing(") {
        return None;
    }
    let args = extract_call_args_by_suffix(line, "::mint(")?;
    let cap_var = parse_treasury_cap_arg(args.first()?.as_str(), aliases)?;
    let amount = normalize_coin_amount_expr(args.get(1)?.as_str())?;
    Some((cap_var, amount))
}

pub(super) fn parse_treasury_burn_call(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
    split_coin_amounts: &std::collections::HashMap<String, String>,
) -> Option<(String, Option<String>)> {
    if !line.contains("coin::burn(") {
        return None;
    }
    let args = extract_call_args_by_suffix(line, "::burn(")?;
    let cap_var = parse_treasury_cap_arg(args.first()?.as_str(), aliases)?;
    let burn_arg = args.get(1)?.as_str();
    if let Some((_, amount)) = parse_coin_split_expr(burn_arg, aliases) {
        return Some((cap_var, Some(amount)));
    }
    let burn_token = strip_ref_and_parens_text(burn_arg);
    if is_ident(burn_token) {
        let resolved = resolve_alias_path(burn_token, aliases);
        if let Some(amount) = split_coin_amounts.get(&resolved) {
            return Some((cap_var, Some(amount.clone())));
        }
        if let Some(amount) = split_coin_amounts.get(burn_token) {
            return Some((cap_var, Some(amount.clone())));
        }
    }
    Some((cap_var, None))
}

pub(super) fn parse_treasury_cap_arg(
    arg: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let token = strip_ref_and_parens_text(arg);
    if !is_ident(token) {
        return None;
    }
    let resolved = resolve_alias_path(token, aliases);
    if is_ident(&resolved) {
        Some(resolved)
    } else {
        None
    }
}

pub(super) fn extract_call_args_by_suffix(
    line: &str,
    suffix_call_prefix: &str,
) -> Option<Vec<String>> {
    let idx = line.find(suffix_call_prefix)?;
    let start = idx + suffix_call_prefix.len();
    let mut depth = 1i32;
    let mut end: Option<usize> = None;
    for (offset, ch) in line[start..].char_indices() {
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                end = Some(start + offset);
                break;
            }
        }
    }
    let end_idx = end?;
    let args_raw = &line[start..end_idx];
    Some(split_args(args_raw))
}

pub(super) fn strip_ref_and_parens_text(raw: &str) -> &str {
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

pub(super) fn extract_option_effects_from_body(
    body_lines: &[String],
    params: &[ParamDecl],
    option_fields_by_type: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Vec<OptionEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    let mut param_option_fields: std::collections::HashMap<
        String,
        std::collections::HashSet<String>,
    > = std::collections::HashMap::new();
    for p in params {
        let ty = normalize_param_object_type(&p.ty);
        let key = type_key_from_type_name(&ty);
        if let Some(fields) = option_fields_by_type.get(&key) {
            param_option_fields.insert(p.name.clone(), fields.clone());
        }
    }

    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        for borrow_target in extract_mut_borrow_targets(stmt) {
            let resolved = if let Some((base, field)) = parse_field_access(&borrow_target)
                .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
            {
                Some((base, field))
            } else if is_ident(&borrow_target) {
                parse_simple_base_field(&resolve_alias_path(&borrow_target, &aliases))
            } else {
                None
            };
            if let Some((base_var, field)) = resolved {
                if let Some(fields) = param_option_fields.get(&base_var) {
                    if fields.contains(&field) {
                        let sig = format!("{}::{}::{:?}", base_var, field, OptionOp::Changed);
                        if seen.insert(sig) {
                            out.push(OptionEffect {
                                base_var,
                                field,
                                op: OptionOp::Changed,
                            });
                        }
                    }
                }
            }
        }
        if let Some((name, target)) = parse_alias_binding(stmt) {
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
            }
        }
        let (lhs, rhs) = match stmt.split_once('=') {
            Some((l, r)) if !l.contains("==") && !r.contains("==") => (l.trim(), r.trim()),
            _ => continue,
        };
        if lhs.ends_with('!') || lhs.contains('[') || lhs.contains(']') {
            continue;
        }

        let (base_var, field) = match parse_field_access(lhs)
            .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
        {
            Some(parts) => parts,
            None => continue,
        };
        let fields = match param_option_fields.get(&base_var) {
            Some(v) => v,
            None => continue,
        };
        if !fields.contains(&field) {
            continue;
        }

        let op = parse_option_write_op(rhs);
        let sig = format!("{}::{}::{:?}", base_var, field, op);
        if seen.insert(sig) {
            out.push(OptionEffect {
                base_var,
                field,
                op,
            });
        }
    }

    out
}

pub(super) fn parse_option_write_op(rhs: &str) -> OptionOp {
    let norm = remove_whitespace(rhs);
    if norm.contains("option::some(") {
        OptionOp::SetSome
    } else if norm.contains("option::none(") {
        OptionOp::SetNone
    } else {
        OptionOp::Changed
    }
}

pub(super) fn extract_string_effects_from_body(
    body_lines: &[String],
    params: &[ParamDecl],
    string_fields_by_type: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Vec<StringEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    let mut param_string_fields: std::collections::HashMap<
        String,
        std::collections::HashSet<String>,
    > = std::collections::HashMap::new();
    for p in params {
        let ty = normalize_param_object_type(&p.ty);
        let key = type_key_from_type_name(&ty);
        if let Some(fields) = string_fields_by_type.get(&key) {
            param_string_fields.insert(p.name.clone(), fields.clone());
        }
    }

    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        for borrow_target in extract_mut_borrow_targets(stmt) {
            let resolved = if let Some((base, field)) = parse_field_access(&borrow_target)
                .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
            {
                Some((base, field))
            } else if is_ident(&borrow_target) {
                parse_simple_base_field(&resolve_alias_path(&borrow_target, &aliases))
            } else {
                None
            };
            if let Some((base_var, field)) = resolved {
                if let Some(fields) = param_string_fields.get(&base_var) {
                    if fields.contains(&field) {
                        let sig = format!("{}.{}", base_var, field);
                        if seen.insert(sig) {
                            out.push(StringEffect { base_var, field });
                        }
                    }
                }
            }
        }
        if let Some((name, target)) = parse_alias_binding(stmt) {
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
            }
        }
        let (lhs, _rhs) = match stmt.split_once('=') {
            Some((l, r)) if !l.contains("==") && !r.contains("==") => (l.trim(), r.trim()),
            _ => continue,
        };
        if lhs.ends_with('!') || lhs.contains('[') || lhs.contains(']') {
            continue;
        }

        let (base_var, field) = match parse_field_access(lhs)
            .and_then(|(base, field)| resolve_effect_target(&base, &field, &aliases))
        {
            Some(parts) => parts,
            None => continue,
        };
        let fields = match param_string_fields.get(&base_var) {
            Some(v) => v,
            None => continue,
        };
        if !fields.contains(&field) {
            continue;
        }
        let sig = format!("{}.{}", base_var, field);
        if seen.insert(sig) {
            out.push(StringEffect { base_var, field });
        }
    }

    out
}

pub(super) fn extract_container_effects_from_body(body_lines: &[String]) -> Vec<ContainerEffect> {
    let mut out = Vec::new();
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    for line in body_lines {
        let no_comments = line.split("//").next().unwrap_or("").trim();
        if no_comments.is_empty() || no_comments.starts_with("assert!") {
            continue;
        }
        let stmt = no_comments.trim_end_matches(';').trim();
        if let Some((name, target)) = parse_alias_binding(stmt) {
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
            }
        }

        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "table::add", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::Table,
                ContainerOp::Insert(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "table::remove", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::Table,
                ContainerOp::Remove(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "bag::add", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::Bag,
                ContainerOp::Insert(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "bag::remove", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::Bag,
                ContainerOp::Remove(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "vec_map::insert", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::VecMap,
                ContainerOp::Insert(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "vec_map::remove", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::VecMap,
                ContainerOp::Remove(key),
            );
        }
        if let Some((base_var, field)) =
            parse_container_namespace_target(stmt, "vec_map::pop", &aliases, 0)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::VecMap,
                ContainerOp::Changed,
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "vec_set::insert", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::VecSet,
                ContainerOp::Insert(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "vec_set::remove", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::VecSet,
                ContainerOp::Remove(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "dynamic_field::add", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::DynamicField,
                ContainerOp::Insert(key),
            );
        }
        if let Some((base_var, field, key)) =
            parse_container_namespace_effect(stmt, "dynamic_field::remove", &aliases, 0, 1)
        {
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::DynamicField,
                ContainerOp::Remove(key),
            );
        }
        if let Some((base_var, field, key)) = parse_container_namespace_effect(
            stmt,
            "dynamic_field::remove_if_exists",
            &aliases,
            0,
            1,
        ) {
            let _ = key;
            push_container_effect(
                &mut out,
                &mut seen,
                base_var,
                field,
                ContainerKind::DynamicField,
                ContainerOp::Changed,
            );
        }
    }

    out
}

pub(super) fn push_container_effect(
    out: &mut Vec<ContainerEffect>,
    seen: &mut std::collections::HashSet<String>,
    base_var: String,
    field: String,
    kind: ContainerKind,
    op: ContainerOp,
) {
    let sig = format!("{}::{}::{:?}::{:?}", base_var, field, kind, op);
    if seen.insert(sig) {
        out.push(ContainerEffect {
            base_var,
            field,
            kind,
            op,
        });
    }
}

pub(super) fn extract_mut_borrow_targets(stmt: &str) -> Vec<String> {
    let mut out = Vec::new();
    for part in stmt.split("&mut").skip(1) {
        let mut cand = part.trim();
        while let Some(rest) = cand.strip_prefix('(') {
            cand = rest.trim_start();
        }
        let end = cand.find([',', ')', ';', '=']).unwrap_or(cand.len());
        let token = cand[..end]
            .trim()
            .trim_matches('(')
            .trim_matches(')')
            .trim();
        if !token.is_empty() {
            out.push(token.to_string());
        }
    }
    out
}

pub(super) fn parse_container_namespace_effect(
    line: &str,
    call_name: &str,
    aliases: &std::collections::HashMap<String, String>,
    target_idx: usize,
    key_idx: usize,
) -> Option<(String, String, String)> {
    let args = extract_namespace_args_flexible(line, call_name)?;
    let target = parse_container_target_expr(args.get(target_idx)?.as_str(), aliases)?;
    let key = normalize_container_key_expr(args.get(key_idx)?.as_str())?;
    Some((target.0, target.1, key))
}

pub(super) fn parse_container_namespace_target(
    line: &str,
    call_name: &str,
    aliases: &std::collections::HashMap<String, String>,
    target_idx: usize,
) -> Option<(String, String)> {
    let args = extract_namespace_args_flexible(line, call_name)?;
    parse_container_target_expr(args.get(target_idx)?.as_str(), aliases)
}

fn parse_balance_numeric_effect(
    line: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String, NumericOp)> {
    if let Some(args) = extract_namespace_args(line, "balance::split(") {
        let (base_var, field) = parse_container_target_expr(args.first()?.as_str(), aliases)?;
        let amount_token = normalize_container_key_expr(args.get(1)?.as_str())?;
        let op = if let Some(v) = parse_numeric_literal(&amount_token) {
            NumericOp::Sub(v)
        } else if is_ident(&amount_token) {
            NumericOp::Sub(amount_token)
        } else {
            NumericOp::Changed
        };
        return Some((base_var, field, op));
    }
    if let Some(args) = extract_namespace_args(line, "balance::join(") {
        let (base_var, field) = parse_container_target_expr(args.first()?.as_str(), aliases)?;
        let rhs_token = normalize_container_key_expr(args.get(1)?.as_str())?;
        let op = if let Some(v) = parse_numeric_literal(&rhs_token) {
            NumericOp::Add(v)
        } else if is_ident(&rhs_token) {
            NumericOp::Add(rhs_token)
        } else {
            NumericOp::Changed
        };
        return Some((base_var, field, op));
    }
    None
}

pub(super) fn parse_container_target_expr(
    arg: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let first_arg = strip_ref_and_parens_text(arg);
    if let Some((base, field)) = parse_field_access(first_arg) {
        return resolve_effect_target(&base, &field, aliases);
    }
    if is_ident(first_arg) {
        let resolved = resolve_alias_path(first_arg, aliases);
        return parse_simple_base_field(&resolved);
    }
    None
}

pub(super) fn normalize_container_key_expr(raw: &str) -> Option<String> {
    let token = strip_ref_and_parens_text(raw).trim();
    if token.is_empty() {
        None
    } else {
        Some(token.to_string())
    }
}

pub(super) fn parse_vector_method_target(
    line: &str,
    method_prefix: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    // Example: counter.history.push_back(x)
    let idx = line.find(method_prefix)?;
    let lhs = line[..idx].trim_end_matches('.').trim();
    parse_vector_target_expr(lhs, aliases)
}

pub(super) fn parse_vector_namespace_target(
    line: &str,
    call_prefix: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    // Example: vector::push_back(&mut counter.history, x)
    let args = extract_namespace_args(line, call_prefix)?;
    parse_vector_target_expr(args.first()?.as_str(), aliases)
}

pub(super) fn parse_vector_target_expr(
    arg: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let first_arg = arg
        .trim()
        .trim_start_matches("&mut")
        .trim_start_matches('&')
        .trim();
    if let Some((base, field)) = parse_field_access(first_arg) {
        return resolve_effect_target(&base, &field, aliases);
    }
    if is_ident(first_arg) {
        let resolved = resolve_alias_path(first_arg, aliases);
        return parse_simple_base_field(&resolved);
    }
    None
}

pub(super) fn parse_let_binding_name(stmt: &str) -> Option<String> {
    let rest = stmt.strip_prefix("let ")?.trim();
    let lhs = rest.split_once('=').map(|(l, _)| l).unwrap_or(rest).trim();
    let lhs = lhs.strip_prefix("mut ").unwrap_or(lhs).trim();
    let name = lhs
        .split(|c: char| c == ':' || c.is_whitespace())
        .next()?
        .trim();
    if is_ident(name) {
        Some(name.to_string())
    } else {
        None
    }
}

pub(super) fn parse_alias_binding(stmt: &str) -> Option<(String, String)> {
    let name = parse_let_binding_name(stmt)?;
    let rhs = stmt.split_once('=')?.1.trim().trim_end_matches(';').trim();
    let rhs = rhs.trim().trim_matches('(').trim_matches(')').trim();
    let target = rhs
        .strip_prefix("&mut ")
        .or_else(|| rhs.strip_prefix('&'))
        .unwrap_or(rhs)
        .trim()
        .trim_matches('(')
        .trim_matches(')')
        .trim();
    if is_ident(target) || parse_simple_base_field(target).is_some() {
        Some((name, target.to_string()))
    } else {
        None
    }
}

pub(super) fn resolve_alias_path(
    name: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> String {
    let mut cur = name.to_string();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    while is_ident(&cur) {
        if !seen.insert(cur.clone()) {
            break;
        }
        match aliases.get(&cur) {
            Some(next) => cur = next.clone(),
            None => break,
        }
    }
    cur
}

pub(super) fn parse_simple_base_field(path: &str) -> Option<(String, String)> {
    let mut parts = path.split('.');
    let base = parts.next()?.trim();
    let field = parts.next()?.trim();
    if parts.next().is_some() {
        return None;
    }
    if is_ident(base) && is_ident(field) {
        Some((base.to_string(), field.to_string()))
    } else {
        None
    }
}

pub(super) fn resolve_effect_target(
    base: &str,
    field: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let resolved = resolve_alias_path(base, aliases);
    if is_ident(&resolved) {
        return Some((resolved, field.to_string()));
    }
    if let Some((resolved_base, resolved_field)) = parse_simple_base_field(&resolved) {
        // If alias points to a field itself (e.g. alias -> counter.history), we only
        // support direct tracking of that field target.
        if field == resolved_field {
            return Some((resolved_base, resolved_field));
        }
    }
    None
}

pub(super) fn extract_method_args(line: &str, method_prefix: &str) -> Option<Vec<String>> {
    let idx = line.find(method_prefix)?;
    let after = line[idx + method_prefix.len()..].trim();
    let args_raw = after.strip_suffix(')')?;
    Some(split_args(args_raw))
}

pub(super) fn extract_namespace_args(line: &str, call_prefix: &str) -> Option<Vec<String>> {
    let after = line.split_once(call_prefix)?.1.trim();
    let args_raw = after.strip_suffix(')')?;
    Some(split_args(args_raw))
}

pub(super) fn extract_namespace_args_flexible(line: &str, call_name: &str) -> Option<Vec<String>> {
    let idx = line.find(call_name)?;
    let mut i = idx + call_name.len();
    let bytes = line.as_bytes();
    while i < line.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    if i < line.len() && bytes[i] == b'<' {
        let mut depth = 1i32;
        i += 1;
        while i < line.len() {
            let ch = bytes[i] as char;
            if ch == '<' {
                depth += 1;
            } else if ch == '>' {
                depth -= 1;
                if depth == 0 {
                    i += 1;
                    break;
                }
            }
            i += 1;
        }
    }
    while i < line.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    if i >= line.len() || bytes[i] != b'(' {
        return None;
    }
    let start = i + 1;
    let mut depth = 1i32;
    let mut end: Option<usize> = None;
    for (offset, ch) in line[start..].char_indices() {
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                end = Some(start + offset);
                break;
            }
        }
    }
    let end_idx = end?;
    Some(split_args(&line[start..end_idx]))
}

pub(super) fn extract_return_type(header: &str) -> Option<String> {
    let after_paren = header.split_once(')')?.1.trim();
    let after_colon = after_paren.strip_prefix(':')?;
    let before_brace = after_colon.split('{').next().unwrap_or(after_colon).trim();
    if before_brace.is_empty() {
        None
    } else {
        Some(before_brace.to_string())
    }
}

pub(super) fn extract_same_module_calls_from_body(
    body_lines: &[String],
    module_name: &str,
) -> Vec<CallSite> {
    let module_short = module_name.split("::").last().unwrap_or(module_name);
    let mut out = Vec::new();

    for line in body_lines {
        let text = line.split("//").next().unwrap_or("");
        let chars: Vec<char> = text.chars().collect();
        let mut i = 0usize;
        while i < chars.len() {
            if chars[i] != '(' || i == 0 {
                i += 1;
                continue;
            }
            let mut j: isize = (i as isize) - 1;
            while j >= 0 && chars[j as usize].is_whitespace() {
                j -= 1;
            }
            if j < 0 {
                i += 1;
                continue;
            }
            let end = j as usize;
            let mut start = end;
            while start > 0 {
                let c = chars[start];
                if c.is_ascii_alphanumeric() || c == '_' || c == ':' {
                    start -= 1;
                } else {
                    break;
                }
            }
            if !(chars[start].is_ascii_alphanumeric() || chars[start] == '_' || chars[start] == ':')
            {
                start += 1;
            }
            if start > end {
                i += 1;
                continue;
            }
            if start > 0 && chars[start - 1] == '.' {
                i += 1;
                continue;
            }
            let token: String = chars[start..=end].iter().collect();
            if token.is_empty() || token == "assert" {
                i += 1;
                continue;
            }

            let callee = if token.contains("::") {
                if token.starts_with("Self::") || token.starts_with(&format!("{}::", module_short))
                {
                    token.split("::").last().unwrap_or("").to_string()
                } else {
                    i += 1;
                    continue;
                }
            } else {
                token
            };
            if callee.is_empty() {
                i += 1;
                continue;
            }

            let mut depth = 1i32;
            let mut k = i + 1;
            while k < chars.len() {
                if chars[k] == '(' {
                    depth += 1;
                } else if chars[k] == ')' {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                k += 1;
            }
            if k >= chars.len() {
                i += 1;
                continue;
            }

            let args_raw: String = chars[i + 1..k].iter().collect();
            out.push(CallSite {
                callee_fn: callee,
                arg_exprs: split_args(&args_raw),
            });
            i = k + 1;
        }
    }

    out
}

pub(super) fn split_args(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut angle = 0i32;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    for ch in s.chars() {
        match ch {
            '<' => {
                angle += 1;
                cur.push(ch);
            }
            '>' => {
                angle -= 1;
                cur.push(ch);
            }
            '(' => {
                paren += 1;
                cur.push(ch);
            }
            ')' => {
                paren -= 1;
                cur.push(ch);
            }
            '[' => {
                bracket += 1;
                cur.push(ch);
            }
            ']' => {
                bracket -= 1;
                cur.push(ch);
            }
            ',' if angle == 0 && paren == 0 && bracket == 0 => {
                let t = cur.trim();
                if !t.is_empty() {
                    out.push(t.to_string());
                }
                cur.clear();
            }
            _ => cur.push(ch),
        }
    }
    let t = cur.trim();
    if !t.is_empty() {
        out.push(t.to_string());
    }
    out
}
