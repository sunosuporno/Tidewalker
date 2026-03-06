
fn module_fn_label(d: &FnDecl) -> String {
    format!("{}::{}", d.module_name, d.fn_name)
}

fn cleanup_stmt_for_type(
    default_module: &str,
    ty: &str,
    var: &str,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Option<String> {
    if is_known_key_struct(ty, default_module, key_structs_by_module) {
        let (decl_module, _) = split_type_module_and_base(ty, default_module);
        let destroy_name = format!("destroy_{}_for_testing", type_key_from_type_name(ty));
        if fn_lookup
            .get(decl_module)
            .and_then(|fns| fns.get(&destroy_name))
            .map(|f| f.is_test_only)
            .unwrap_or(false)
        {
            return Some(format!("{}::{}({});", decl_module, destroy_name, var));
        }
        if !is_known_store_struct(ty, default_module, store_structs_by_module) {
            return None;
        }
    }
    Some(format!("transfer::public_transfer({}, SUPER_USER);", var))
}

fn reverse_comparison_op(op: &str) -> String {
    match op {
        ">" => "<".to_string(),
        "<" => ">".to_string(),
        ">=" => "<=".to_string(),
        "<=" => ">=".to_string(),
        _ => op.to_string(),
    }
}

fn parse_ast_comparison(expr: &str) -> Option<(Expr, BinOp, Expr)> {
    let parsed = expr_ast::parse_expr(expr)?;
    expr_ast::as_comparison(&parsed)
}

fn parse_param_led_comparison(
    expr: &str,
    param_names: &[String],
) -> Option<(String, String, String, bool)> {
    if let Some((lhs, op, rhs)) = parse_ast_comparison(expr) {
        if let Some(param) = expr_ast::as_ident(&lhs) {
            if param_names.iter().any(|p| p == &param) {
                return Some((
                    param,
                    op.as_str().to_string(),
                    expr_ast::render_expr(&rhs),
                    false,
                ));
            }
        }
        if let Some(param) = expr_ast::as_ident(&rhs) {
            if param_names.iter().any(|p| p == &param) {
                return Some((
                    param,
                    op.as_str().to_string(),
                    expr_ast::render_expr(&lhs),
                    true,
                ));
            }
        }
    }

    let mut norm = remove_whitespace(expr);
    while norm.starts_with('(') && norm.ends_with(')') && norm.len() > 2 {
        norm = norm[1..norm.len() - 1].to_string();
    }
    if norm.contains("&&") || norm.contains("||") {
        return None;
    }

    let mut names = param_names.to_vec();
    names.sort_by_key(|n| std::cmp::Reverse(n.len()));

    for param in &names {
        for op in [">=", "<=", "==", "!=", ">", "<"] {
            let left_prefix = format!("{}{}", param, op);
            if norm.starts_with(&left_prefix) {
                let rhs = norm[left_prefix.len()..].trim();
                if !rhs.is_empty() {
                    return Some((param.clone(), op.to_string(), rhs.to_string(), false));
                }
            }
            let right_suffix = format!("{}{}", op, param);
            if norm.ends_with(&right_suffix) {
                let lhs = norm[..norm.len() - right_suffix.len()].trim();
                if !lhs.is_empty() {
                    return Some((param.clone(), op.to_string(), lhs.to_string(), true));
                }
            }
        }
    }
    None
}

fn parse_object_field_led_comparison(
    expr: &str,
    param_names: &[String],
) -> Option<(String, String, String, String, bool)> {
    if let Some((lhs, op, rhs)) = parse_ast_comparison(expr) {
        if let Some((base, field)) = expr_ast::as_field_ident(&lhs) {
            if param_names.iter().any(|p| p == &base) {
                return Some((
                    base,
                    field,
                    op.as_str().to_string(),
                    expr_ast::render_expr(&rhs),
                    false,
                ));
            }
        }
        if let Some((base, field)) = expr_ast::as_field_ident(&rhs) {
            if param_names.iter().any(|p| p == &base) {
                return Some((
                    base,
                    field,
                    op.as_str().to_string(),
                    expr_ast::render_expr(&lhs),
                    true,
                ));
            }
        }
    }

    let mut norm = remove_whitespace(expr);
    while norm.starts_with('(') && norm.ends_with(')') && norm.len() > 2 {
        norm = norm[1..norm.len() - 1].to_string();
    }
    if norm.contains("&&") || norm.contains("||") {
        return None;
    }

    let mut names = param_names.to_vec();
    names.sort_by_key(|n| std::cmp::Reverse(n.len()));

    for param in &names {
        let left_prefix = format!("{}.", param);
        if norm.starts_with(&left_prefix) {
            let rem = &norm[left_prefix.len()..];
            for op in [">=", "<=", "==", "!=", ">", "<"] {
                if let Some(idx) = rem.find(op) {
                    let field = rem[..idx].trim();
                    let rhs = rem[idx + op.len()..].trim();
                    if !field.is_empty() && !rhs.is_empty() && is_ident(field) {
                        return Some((
                            param.clone(),
                            field.to_string(),
                            op.to_string(),
                            rhs.to_string(),
                            false,
                        ));
                    }
                }
            }
        }
        for op in [">=", "<=", "==", "!=", ">", "<"] {
            let marker = format!("{}{}.", op, param);
            if let Some(idx) = norm.rfind(&marker) {
                let lhs = norm[..idx].trim();
                let field = norm[idx + marker.len()..].trim();
                if !lhs.is_empty() && !field.is_empty() && is_ident(field) {
                    return Some((
                        param.clone(),
                        field.to_string(),
                        op.to_string(),
                        lhs.to_string(),
                        true,
                    ));
                }
            }
        }
    }
    None
}

fn split_top_level_and_terms(expr: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    let chars = expr.char_indices().collect::<Vec<_>>();
    let mut start = 0usize;
    let mut i = 0usize;
    while i < chars.len() {
        let (idx, ch) = chars[i];
        match ch {
            '(' => paren += 1,
            ')' => paren -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            '&' if paren == 0 && bracket == 0 && brace == 0 => {
                if i + 1 < chars.len() && chars[i + 1].1 == '&' {
                    let part = expr[start..idx].trim();
                    if !part.is_empty() {
                        out.push(part.to_string());
                    }
                    start = chars[i + 1].0 + 1;
                    i += 1;
                }
            }
            _ => {}
        }
        i += 1;
    }
    let tail = expr[start..].trim();
    if !tail.is_empty() {
        out.push(tail.to_string());
    }
    if out.is_empty() {
        vec![expr.trim().to_string()]
    } else {
        out
    }
}

fn parse_negated_object_field_expr(
    expr: &str,
    param_names: &[String],
) -> Option<(String, String)> {
    let mut s = expr.trim();
    while s.starts_with('(') && s.ends_with(')') && s.len() > 2 {
        s = s[1..s.len() - 1].trim();
    }
    if !s.starts_with('!') {
        return None;
    }
    let mut inner = s[1..].trim();
    while inner.starts_with('(') && inner.ends_with(')') && inner.len() > 2 {
        inner = inner[1..inner.len() - 1].trim();
    }
    let norm = remove_whitespace(inner);
    parse_direct_object_field_side(&norm, param_names)
}

fn parse_negated_param_expr(expr: &str, param_names: &[String]) -> Option<String> {
    let mut s = expr.trim();
    while s.starts_with('(') && s.ends_with(')') && s.len() > 2 {
        s = s[1..s.len() - 1].trim();
    }
    if !s.starts_with('!') {
        return None;
    }
    let mut inner = s[1..].trim();
    while inner.starts_with('(') && inner.ends_with(')') && inner.len() > 2 {
        inner = inner[1..inner.len() - 1].trim();
    }
    let ident = remove_whitespace(inner);
    if !is_ident(&ident) {
        return None;
    }
    if !param_names.iter().any(|p| p == &ident) {
        return None;
    }
    Some(ident)
}

fn parse_direct_object_field_side(expr: &str, param_names: &[String]) -> Option<(String, String)> {
    if let Some(parsed) = expr_ast::parse_expr(expr) {
        if let Some((base, field)) = expr_ast::as_field_ident(&parsed) {
            if param_names.iter().any(|p| p == &base) {
                return Some((base, field));
            }
        }
    }

    let mut norm = remove_whitespace(expr);
    while norm.starts_with('(') && norm.ends_with(')') && norm.len() > 2 {
        norm = norm[1..norm.len() - 1].to_string();
    }
    if norm.contains('(') || norm.contains(')') || norm.matches('.').count() != 1 {
        return None;
    }
    let (base, field) = norm.split_once('.')?;
    if !is_ident(base) || !is_ident(field) {
        return None;
    }
    if !param_names.iter().any(|p| p == base) {
        return None;
    }
    Some((base.to_string(), field.to_string()))
}

fn parse_coin_value_side(
    expr: &str,
    param_names: &[String],
) -> Option<(String, String, String, bool)> {
    let coin_value_param_from_expr = |e: &Expr| -> Option<String> {
        let (func, args) = expr_ast::as_call(e)?;
        let is_coin_value = matches!(func.as_str(), "coin::value" | "sui::coin::value" | "value");
        if !is_coin_value || args.len() != 1 {
            return None;
        }
        let coin_param = expr_ast::as_ident(&args[0])?;
        if param_names.iter().any(|p| p == &coin_param) {
            Some(coin_param)
        } else {
            None
        }
    };
    if let Some((lhs, op, rhs)) = parse_ast_comparison(expr) {
        if let Some(param) = coin_value_param_from_expr(&lhs) {
            return Some((
                param,
                op.as_str().to_string(),
                expr_ast::render_expr(&rhs),
                false,
            ));
        }
        if let Some(param) = coin_value_param_from_expr(&rhs) {
            return Some((
                param,
                op.as_str().to_string(),
                expr_ast::render_expr(&lhs),
                true,
            ));
        }
    }

    let mut norm = remove_whitespace(expr);
    while norm.starts_with('(') && norm.ends_with(')') && norm.len() > 2 {
        norm = norm[1..norm.len() - 1].to_string();
    }
    if norm.contains("&&") || norm.contains("||") {
        return None;
    }

    let mut names = param_names.to_vec();
    names.sort_by_key(|n| std::cmp::Reverse(n.len()));

    let mut coin_call_forms = Vec::new();
    for p in &names {
        coin_call_forms.push(format!("coin::value(&{})", p));
        coin_call_forms.push(format!("sui::coin::value(&{})", p));
        coin_call_forms.push(format!("coin::value({})", p));
        coin_call_forms.push(format!("sui::coin::value({})", p));
    }

    for (idx, call_expr) in coin_call_forms.iter().enumerate() {
        let param = names.get(idx / 4)?.clone();
        for op in [">=", "<=", "==", "!=", ">", "<"] {
            let left_prefix = format!("{}{}", call_expr, op);
            if norm.starts_with(&left_prefix) {
                let rhs = norm[left_prefix.len()..].trim();
                if !rhs.is_empty() {
                    return Some((param, op.to_string(), rhs.to_string(), false));
                }
            }
            let right_suffix = format!("{}{}", op, call_expr);
            if norm.ends_with(&right_suffix) {
                let lhs = norm[..norm.len() - right_suffix.len()].trim();
                if !lhs.is_empty() {
                    return Some((param, op.to_string(), lhs.to_string(), true));
                }
            }
        }
    }
    None
}

fn parse_coin_mod_zero_object_field(
    expr: &str,
    param_names: &[String],
) -> Option<(String, String, String)> {
    let coin_value_param_from_expr = |e: &Expr| -> Option<String> {
        let (func, args) = expr_ast::as_call(e)?;
        let is_coin_value = matches!(func.as_str(), "coin::value" | "sui::coin::value" | "value");
        if !is_coin_value || args.len() != 1 {
            return None;
        }
        let coin_param = expr_ast::as_ident(&args[0])?;
        if param_names.iter().any(|p| p == &coin_param) {
            Some(coin_param)
        } else {
            None
        }
    };
    let is_zero = |e: &Expr| -> bool {
        matches!(expr_ast::strip_wrappers(e), Expr::Number(n) if n.replace('_', "") == "0")
    };

    let (lhs_cmp, op_cmp, rhs_cmp) = parse_ast_comparison(expr)?;
    if op_cmp != BinOp::Eq {
        return None;
    }
    let mod_expr = if is_zero(&lhs_cmp) {
        rhs_cmp
    } else if is_zero(&rhs_cmp) {
        lhs_cmp
    } else {
        return None;
    };
    let Expr::Binary { op, lhs, rhs } = expr_ast::strip_wrappers(&mod_expr) else {
        return None;
    };
    if *op != BinOp::Mod {
        return None;
    }

    if let Some(coin_param) = coin_value_param_from_expr(lhs) {
        if let Some((object_param, field_name)) = expr_ast::as_field_ident(rhs) {
            return Some((coin_param, object_param, field_name));
        }
    }
    if let Some(coin_param) = coin_value_param_from_expr(rhs) {
        if let Some((object_param, field_name)) = expr_ast::as_field_ident(lhs) {
            return Some((coin_param, object_param, field_name));
        }
    }
    None
}

fn parse_clock_timestamp_side(expr: &str, d: &FnDecl) -> Option<String> {
    let is_clock_param = |name: &str| -> bool {
        d.params
            .iter()
            .any(|p| p.name == name && is_clock_type(&p.ty))
    };
    let clock_param_from_expr = |e: &Expr| -> Option<String> {
        let (func, args) = expr_ast::as_call(e)?;
        let is_ts = matches!(
            func.as_str(),
            "timestamp_ms" | "clock::timestamp_ms" | "sui::clock::timestamp_ms"
        );
        if !is_ts || args.len() != 1 {
            return None;
        }
        let arg = expr_ast::as_ident(&args[0])?;
        if is_clock_param(&arg) {
            Some(arg)
        } else {
            None
        }
    };
    if let Some(parsed) = expr_ast::parse_expr(expr) {
        if let Some(param) = clock_param_from_expr(&parsed) {
            return Some(param);
        }
    }

    let mut norm = remove_whitespace(expr);
    while norm.starts_with('(') && norm.ends_with(')') && norm.len() > 2 {
        norm = norm[1..norm.len() - 1].to_string();
    }
    let clock_params = d
        .params
        .iter()
        .filter(|p| is_clock_type(&p.ty))
        .map(|p| p.name.as_str())
        .collect::<Vec<_>>();
    if clock_params.is_empty() {
        return None;
    }

    if let Some(base) = norm.strip_suffix(".timestamp_ms()") {
        let base_clean = strip_ref_and_parens_text(base).trim();
        if clock_params.iter().any(|p| *p == base_clean) {
            return Some(base_clean.to_string());
        }
    }

    for prefix in ["clock::timestamp_ms(", "sui::clock::timestamp_ms("] {
        if let Some(rest) = norm.strip_prefix(prefix) {
            if !rest.ends_with(')') {
                continue;
            }
            let inner = strip_ref_and_parens_text(&rest[..rest.len() - 1])
                .trim()
                .to_string();
            if clock_params.iter().any(|p| *p == inner) {
                return Some(inner);
            }
        }
    }

    None
}

fn vector_length_param_from_expr(expr: &Expr, d: &FnDecl, param_names: &[String]) -> Option<String> {
    let (func, args) = expr_ast::as_call(expr)?;
    let is_len = matches!(
        func.as_str(),
        "length" | "vector::length" | "std::vector::length"
    );
    if !is_len || args.len() != 1 {
        return None;
    }
    let arg = expr_ast::as_ident(&args[0])?;
    if !param_names.iter().any(|p| p == &arg) {
        return None;
    }
    let p = d.params.iter().find(|p| p.name == arg)?;
    if !is_vector_type(&p.ty) {
        return None;
    }
    Some(arg)
}

fn parse_vector_len_comparison(
    expr: &str,
    d: &FnDecl,
    param_names: &[String],
) -> Option<(String, String, String, bool, bool)> {
    let (lhs, op, rhs) = parse_ast_comparison(expr)?;
    if let Some(param) = vector_length_param_from_expr(&lhs, d, param_names) {
        if let Some(rhs_param) = vector_length_param_from_expr(&rhs, d, param_names) {
            return Some((param, op.as_str().to_string(), rhs_param, false, true));
        }
        return Some((
            param,
            op.as_str().to_string(),
            expr_ast::render_expr(&rhs),
            false,
            false,
        ));
    }
    if let Some(param) = vector_length_param_from_expr(&rhs, d, param_names) {
        if let Some(lhs_param) = vector_length_param_from_expr(&lhs, d, param_names) {
            return Some((param, op.as_str().to_string(), lhs_param, true, true));
        }
        return Some((
            param,
            op.as_str().to_string(),
            expr_ast::render_expr(&lhs),
            true,
            false,
        ));
    }
    None
}

fn parse_object_id_side(expr: &str, param_names: &[String]) -> Option<String> {
    if let Some(parsed) = expr_ast::parse_expr(expr) {
        if let Some((func, args)) = expr_ast::as_call(&parsed) {
            if matches!(func.as_str(), "object::id" | "sui::object::id") && args.len() == 1 {
                if let Some(arg) = expr_ast::as_ident(&args[0]) {
                    if param_names.iter().any(|p| p == &arg) {
                        return Some(arg);
                    }
                }
            }
        }
    }

    let mut norm = remove_whitespace(expr);
    while norm.starts_with('(') && norm.ends_with(')') && norm.len() > 2 {
        norm = norm[1..norm.len() - 1].to_string();
    }
    let prefixes = ["object::id(", "sui::object::id("];
    for prefix in prefixes {
        if let Some(rest) = norm.strip_prefix(prefix) {
            if !rest.ends_with(')') {
                continue;
            }
            let inner = &rest[..rest.len() - 1];
            let stripped = strip_ref_and_parens_text(inner).trim().to_string();
            if param_names.iter().any(|p| p == &stripped) {
                return Some(stripped);
            }
        }
    }
    None
}

fn parse_sender_side(expr: &str, d: &FnDecl) -> bool {
    if let Some(parsed) = expr_ast::parse_expr(expr) {
        if let Some((func, args)) = expr_ast::as_call(&parsed) {
            if matches!(
                func.as_str(),
                "sender" | "tx_context::sender" | "sui::tx_context::sender"
            ) && args.len() == 1
            {
                if let Some(inner) = expr_ast::as_ident(&args[0]) {
                    return d
                        .params
                        .iter()
                        .any(|p| p.name == inner && p.ty.contains("TxContext"));
                }
            }
        }
    }

    let mut norm = remove_whitespace(expr);
    while norm.starts_with('(') && norm.ends_with(')') && norm.len() > 2 {
        norm = norm[1..norm.len() - 1].to_string();
    }
    let prefixes = ["sender(", "tx_context::sender(", "sui::tx_context::sender("];
    for prefix in prefixes {
        if let Some(rest) = norm.strip_prefix(prefix) {
            if !rest.ends_with(')') {
                continue;
            }
            let inner = strip_ref_and_parens_text(&rest[..rest.len() - 1])
                .trim()
                .to_string();
            if !is_ident(&inner) {
                continue;
            }
            if d.params
                .iter()
                .any(|p| p.name == inner && p.ty.contains("TxContext"))
            {
                return true;
            }
        }
    }
    false
}

fn extract_assert_condition(stmt: &str) -> Option<String> {
    let idx = stmt.find("assert!(")?;
    let start = idx + "assert!(".len();
    let mut depth = 1i32;
    let mut end: Option<usize> = None;
    for (offset, ch) in stmt[start..].char_indices() {
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

fn parse_getter_side(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    expr: &str,
) -> Option<(String, String)> {
    if let Some(parsed) = expr_ast::parse_expr(expr) {
        if let Some((func, args)) = expr_ast::as_call(&parsed) {
            if args.len() == 1 {
                if let Some(arg) = expr_ast::as_ident(&args[0]) {
                    let getter_fn = if func.contains("::") {
                        if !func.starts_with(&format!("{}::", d.module_name)) {
                            String::new()
                        } else {
                            func.split("::").last().unwrap_or("").to_string()
                        }
                    } else {
                        func
                    };
                    if !getter_fn.is_empty() {
                        let param = d.params.iter().find(|p| p.name == arg)?;
                        if !param.ty.trim().starts_with('&') {
                            return None;
                        }
                        let param_obj_ty = normalize_param_object_type(&param.ty);
                        let module_accessors = accessor_map.get(&d.module_name)?;
                        if module_accessors
                            .iter()
                            .any(|a| a.fn_name == getter_fn && a.param_ty == param_obj_ty)
                        {
                            return Some((getter_fn, arg));
                        }
                    }
                }
            }
        }
    }

    let open = expr.find('(')?;
    let close = expr.rfind(')')?;
    if close <= open {
        return None;
    }
    let fn_token = expr[..open].trim();
    let args = split_args(&expr[open + 1..close]);
    if args.len() != 1 {
        return None;
    }
    let arg = strip_ref_and_parens_text(args[0].as_str()).trim();
    if !is_ident(arg) {
        return None;
    }
    let getter_fn = if fn_token.contains("::") {
        if !fn_token.starts_with(&format!("{}::", d.module_name)) {
            return None;
        }
        fn_token.split("::").last()?.to_string()
    } else {
        fn_token.to_string()
    };

    let param = d.params.iter().find(|p| p.name == arg)?;
    if !param.ty.trim().starts_with('&') {
        return None;
    }
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = accessor_map.get(&d.module_name)?;
    if !module_accessors
        .iter()
        .any(|a| a.fn_name == getter_fn && a.param_ty == param_obj_ty)
    {
        return None;
    }
    Some((getter_fn, arg.to_string()))
}

fn parse_getter_side_expr(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    expr: &Expr,
) -> Option<(String, String)> {
    let (func, args) = expr_ast::as_call(expr)?;
    if args.len() != 1 {
        return None;
    }
    let arg = expr_ast::as_ident(&args[0])?;
    let getter_fn = if func.contains("::") {
        if !func.starts_with(&format!("{}::", d.module_name)) {
            return None;
        }
        func.split("::").last()?.to_string()
    } else {
        func.to_string()
    };

    let param = d.params.iter().find(|p| p.name == arg)?;
    if !param.ty.trim().starts_with('&') {
        return None;
    }
    let param_obj_ty = normalize_param_object_type(&param.ty);
    let module_accessors = accessor_map.get(&d.module_name)?;
    if !module_accessors
        .iter()
        .any(|a| a.fn_name == getter_fn && a.param_ty == param_obj_ty)
    {
        return None;
    }
    Some((getter_fn, arg))
}

fn parse_simple_let_alias(stmt: &str) -> Option<(String, String)> {
    let trimmed = stmt.trim().trim_end_matches(';').trim();
    let mut rest = trimmed.strip_prefix("let ")?;
    if let Some(after_mut) = rest.strip_prefix("mut ") {
        rest = after_mut;
    }
    let (lhs_raw, rhs_raw) = rest.split_once('=')?;
    let lhs = lhs_raw.split(':').next()?.trim();
    if !is_ident(lhs) {
        return None;
    }
    let rhs = rhs_raw.trim();
    if rhs.is_empty() {
        return None;
    }
    Some((lhs.to_string(), rhs.to_string()))
}

fn resolve_alias_value(
    expr: &str,
    aliases: &std::collections::HashMap<String, String>,
    depth: usize,
) -> String {
    if depth > 8 {
        return expr.trim().to_string();
    }
    let stripped = strip_ref_and_parens_text(expr).trim().to_string();
    if !is_ident(&stripped) {
        return expr.trim().to_string();
    }
    let Some(next) = aliases.get(&stripped) else {
        return expr.trim().to_string();
    };
    resolve_alias_value(next, aliases, depth + 1)
}

fn resolve_condition_aliases(
    cond: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> String {
    for op in [">=", "<=", "==", "!=", ">", "<"] {
        if let Some(idx) = cond.find(op) {
            let lhs = cond[..idx].trim();
            let rhs = cond[idx + op.len()..].trim();
            let resolved_lhs = resolve_alias_value(lhs, aliases, 0);
            let resolved_rhs = resolve_alias_value(rhs, aliases, 0);
            return format!("{} {} {}", resolved_lhs, op, resolved_rhs);
        }
    }
    cond.to_string()
}

fn expand_aliases_in_expr(
    expr: &Expr,
    aliases: &std::collections::HashMap<String, String>,
    depth: usize,
) -> Expr {
    if depth > 6 {
        return expr.clone();
    }
    match expr {
        Expr::Path(p) if !p.contains("::") => {
            if let Some(alias_src) = aliases.get(p) {
                if let Some(parsed_alias) = expr_ast::parse_expr(alias_src) {
                    return expand_aliases_in_expr(&parsed_alias, aliases, depth + 1);
                }
            }
            expr.clone()
        }
        Expr::Ref { mutable, expr } => Expr::Ref {
            mutable: *mutable,
            expr: Box::new(expand_aliases_in_expr(expr, aliases, depth)),
        },
        Expr::Field { base, field } => Expr::Field {
            base: Box::new(expand_aliases_in_expr(base, aliases, depth)),
            field: field.clone(),
        },
        Expr::Call { func, args } => Expr::Call {
            func: func.clone(),
            args: args
                .iter()
                .map(|a| expand_aliases_in_expr(a, aliases, depth))
                .collect::<Vec<_>>(),
        },
        Expr::Cast { expr, ty } => Expr::Cast {
            expr: Box::new(expand_aliases_in_expr(expr, aliases, depth)),
            ty: ty.clone(),
        },
        Expr::Binary { op, lhs, rhs } => Expr::Binary {
            op: *op,
            lhs: Box::new(expand_aliases_in_expr(lhs, aliases, depth)),
            rhs: Box::new(expand_aliases_in_expr(rhs, aliases, depth)),
        },
        _ => expr.clone(),
    }
}

fn expand_aliases_in_condition(
    cond: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> String {
    let Some(parsed) = expr_ast::parse_expr(cond) else {
        return cond.to_string();
    };
    let expanded = expand_aliases_in_expr(&parsed, aliases, 0);
    expr_ast::render_expr(&expanded)
}

fn parse_assert_guard_cases(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
) -> (Vec<GuardCase>, Vec<String>) {
    let mut out = Vec::new();
    let mut notes = Vec::new();
    let fq = module_fn_label(d);
    let default_type_args = default_type_args_for_params(&d.type_params);

    for p in &d.params {
        let resolved_ty = concretize_type_params(&p.ty, &d.type_params, &default_type_args);
        let ty = normalize_param_object_type(&resolved_ty);
        if resolved_ty.trim().starts_with('&') && is_cap_type(&ty) && !is_treasury_cap_type(&ty) {
            out.push(GuardCase {
                kind: GuardKind::CapRole {
                    cap_param: p.name.clone(),
                    cap_type: ty,
                },
                source: format!("{} role/cap requirement via parameter '{}'", fq, p.name),
            });
        }
    }

    let param_names = d.params.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
    let mut local_aliases: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();

    for line in &d.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        if let Some((lhs, rhs)) = parse_simple_let_alias(stmt) {
            local_aliases.insert(lhs, rhs);
        }
        if !stmt.contains("assert!(") {
            continue;
        }
        let cond = match extract_assert_condition(stmt) {
            Some(v) => v,
            None => continue,
        };
        let resolved_cond = resolve_condition_aliases(&cond, &local_aliases);
        let expanded_cond = expand_aliases_in_condition(&resolved_cond, &local_aliases);
        if let Some((base, field)) = parse_negated_object_field_expr(&expanded_cond, &param_names) {
            out.push(GuardCase {
                kind: GuardKind::ObjectFieldConst {
                    param_name: base,
                    field_name: field,
                    op: "==".to_string(),
                    rhs_literal: "false".to_string(),
                },
                source: format!("{}: {}", fq, cond),
            });
            continue;
        }
        if let Some(param_name) = parse_negated_param_expr(&expanded_cond, &param_names) {
            out.push(GuardCase {
                kind: GuardKind::ParamConst {
                    param_name,
                    op: "==".to_string(),
                    rhs_literal: "false".to_string(),
                },
                source: format!("{}: {}", fq, cond),
            });
            continue;
        }
        let and_terms = split_top_level_and_terms(&expanded_cond);
        if and_terms.len() > 1 && !expanded_cond.contains("||") {
            let mut parsed_all = true;
            for term in &and_terms {
                if let Some((object_param, field_name, op_raw, other_side, flipped)) =
                    parse_object_field_led_comparison(term, &param_names)
                {
                    if d.params
                        .iter()
                        .any(|p| p.name == other_side && !p.ty.trim().starts_with('&'))
                    {
                        let op = if flipped {
                            op_raw.clone()
                        } else {
                            reverse_comparison_op(&op_raw)
                        };
                        out.push(GuardCase {
                            kind: GuardKind::ParamObjectField {
                                param_name: other_side,
                                object_param,
                                field_name,
                                op,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                    let op = if flipped {
                        reverse_comparison_op(&op_raw)
                    } else {
                        op_raw
                    };
                    if let Some(lit) = parse_numeric_literal(&other_side) {
                        out.push(GuardCase {
                            kind: GuardKind::ObjectFieldConst {
                                param_name: object_param,
                                field_name,
                                op,
                                rhs_literal: lit,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                    if is_bool_literal(&other_side) || is_address_literal(&other_side) {
                        out.push(GuardCase {
                            kind: GuardKind::ObjectFieldConst {
                                param_name: object_param,
                                field_name,
                                op,
                                rhs_literal: other_side,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                    if parse_sender_side(&other_side, d) {
                        out.push(GuardCase {
                            kind: GuardKind::ObjectFieldSender {
                                param_name: object_param,
                                field_name,
                                op,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                    if let Some(clock_param) = parse_clock_timestamp_side(&other_side, d) {
                        out.push(GuardCase {
                            kind: GuardKind::ObjectFieldClockTimestamp {
                                param_name: object_param,
                                field_name,
                                clock_param,
                                op,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                }
                if let Some((coin_param, op_raw, other_side, flipped)) =
                    parse_coin_value_side(term, &param_names)
                {
                    let op = if flipped {
                        reverse_comparison_op(&op_raw)
                    } else {
                        op_raw
                    };
                    if let Some(lit) = parse_numeric_literal(&other_side) {
                        out.push(GuardCase {
                            kind: GuardKind::CoinValueConst {
                                coin_param,
                                op,
                                rhs_literal: lit,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                    if let Some((object_param, field_name)) =
                        parse_direct_object_field_side(&other_side, &param_names)
                    {
                        out.push(GuardCase {
                            kind: GuardKind::CoinValueObjectField {
                                coin_param,
                                object_param,
                                field_name,
                                op,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                }
                if let Some((coin_param, object_param, field_name)) =
                    parse_coin_mod_zero_object_field(term, &param_names)
                {
                    out.push(GuardCase {
                        kind: GuardKind::CoinValueObjectField {
                            coin_param,
                            object_param,
                            field_name,
                            // Reuse getter-bound synthesis; == yields bound+1, which violates divisibility.
                            op: "==".to_string(),
                        },
                        source: format!("{}: {}", fq, term),
                    });
                    continue;
                }
                if let Some((param_side, term_op, other_side, flipped)) =
                    parse_param_led_comparison(term, &param_names)
                {
                    let op = if flipped {
                        reverse_comparison_op(&term_op)
                    } else {
                        term_op
                    };
                    if let Some(lit) = parse_numeric_literal(&other_side) {
                        out.push(GuardCase {
                            kind: GuardKind::ParamConst {
                                param_name: param_side,
                                op,
                                rhs_literal: lit,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                    if is_bool_literal(&other_side) || is_address_literal(&other_side) {
                        out.push(GuardCase {
                            kind: GuardKind::ParamConst {
                                param_name: param_side,
                                op,
                                rhs_literal: other_side,
                            },
                            source: format!("{}: {}", fq, term),
                        });
                        continue;
                    }
                }
                parsed_all = false;
                break;
            }
            if parsed_all {
                continue;
            }
        }
        if let Some((lhs, op_raw, rhs)) = parse_ast_comparison(&expanded_cond) {
            let lhs_param = expr_ast::as_ident(&lhs).filter(|name| {
                d.params
                    .iter()
                    .any(|p| p.name == *name && !p.ty.trim().starts_with('&'))
            });
            let rhs_param = expr_ast::as_ident(&rhs).filter(|name| {
                d.params
                    .iter()
                    .any(|p| p.name == *name && !p.ty.trim().starts_with('&'))
            });
            let lhs_getter = parse_getter_side_expr(d, accessor_map, &lhs);
            let rhs_getter = parse_getter_side_expr(d, accessor_map, &rhs);
            if let (Some(param_name), Some((getter_fn, getter_param))) = (lhs_param, rhs_getter) {
                out.push(GuardCase {
                    kind: GuardKind::ParamGetter {
                        param_name,
                        op: op_raw.as_str().to_string(),
                        getter_fn,
                        getter_param,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            if let (Some(param_name), Some((getter_fn, getter_param))) = (rhs_param, lhs_getter) {
                out.push(GuardCase {
                    kind: GuardKind::ParamGetter {
                        param_name,
                        op: reverse_comparison_op(op_raw.as_str()),
                        getter_fn,
                        getter_param,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
        }
        if let Some((object_param, field_name, op_raw, other_side, flipped)) =
            parse_object_field_led_comparison(&expanded_cond, &param_names)
        {
            if d.params
                .iter()
                .any(|p| p.name == other_side && !p.ty.trim().starts_with('&'))
            {
                let op = if flipped {
                    op_raw.clone()
                } else {
                    reverse_comparison_op(&op_raw)
                };
                out.push(GuardCase {
                    kind: GuardKind::ParamObjectField {
                        param_name: other_side,
                        object_param,
                        field_name,
                        op,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }

            let op = if flipped {
                reverse_comparison_op(&op_raw)
            } else {
                op_raw
            };
            if let Some(lit) = parse_numeric_literal(&other_side) {
                out.push(GuardCase {
                    kind: GuardKind::ObjectFieldConst {
                        param_name: object_param,
                        field_name,
                        op,
                        rhs_literal: lit,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            if is_bool_literal(&other_side) || is_address_literal(&other_side) {
                out.push(GuardCase {
                    kind: GuardKind::ObjectFieldConst {
                        param_name: object_param,
                        field_name,
                        op,
                        rhs_literal: other_side,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            if parse_object_id_side(&other_side, &param_names).is_some() {
                // Simple object-id equality guards are covered by cap-role style mismatch tests.
                continue;
            }
            if parse_sender_side(&other_side, d) {
                out.push(GuardCase {
                    kind: GuardKind::ObjectFieldSender {
                        param_name: object_param,
                        field_name,
                        op,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            if let Some((coin_param, coin_op_raw, coin_other_side, coin_flipped)) =
                parse_coin_value_side(&resolved_cond, &param_names)
            {
                if let Some((coin_object_param, coin_field_name)) =
                    parse_direct_object_field_side(&coin_other_side, &param_names)
                {
                    let coin_op = if coin_flipped {
                        reverse_comparison_op(&coin_op_raw)
                    } else {
                        coin_op_raw
                    };
                    out.push(GuardCase {
                        kind: GuardKind::CoinValueObjectField {
                            coin_param,
                            object_param: coin_object_param,
                            field_name: coin_field_name,
                            op: coin_op,
                        },
                        source: format!("{}: {}", fq, cond),
                    });
                    continue;
                }
            }
            if let Some(clock_param) = parse_clock_timestamp_side(&other_side, d) {
                out.push(GuardCase {
                    kind: GuardKind::ObjectFieldClockTimestamp {
                        param_name: object_param,
                        field_name,
                        clock_param,
                        op,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            notes.push(format!(
                "{}: unresolved object-field assert side '{}'",
                fq, other_side
            ));
            continue;
        }
        if let Some((coin_param, op_raw, other_side, flipped)) =
            parse_coin_value_side(&expanded_cond, &param_names)
        {
            let op = if flipped {
                reverse_comparison_op(&op_raw)
            } else {
                op_raw
            };
            if let Some(lit) = parse_numeric_literal(&other_side) {
                out.push(GuardCase {
                    kind: GuardKind::CoinValueConst {
                        coin_param,
                        op,
                        rhs_literal: lit,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            if let Some((object_param, field_name)) =
                parse_direct_object_field_side(&other_side, &param_names)
            {
                out.push(GuardCase {
                    kind: GuardKind::CoinValueObjectField {
                        coin_param,
                        object_param,
                        field_name,
                        op,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            notes.push(format!(
                "{}: unresolved non-literal assert side '{}'",
                fq, other_side
            ));
            continue;
        }
        if let Some((coin_param, object_param, field_name)) =
            parse_coin_mod_zero_object_field(&expanded_cond, &param_names)
        {
            out.push(GuardCase {
                kind: GuardKind::CoinValueObjectField {
                    coin_param,
                    object_param,
                    field_name,
                    // Reuse getter-bound synthesis; == yields bound+1, which violates divisibility.
                    op: "==".to_string(),
                },
                source: format!("{}: {}", fq, cond),
            });
            continue;
        }
        if let Some((vec_param, vec_op_raw, vec_other, vec_flipped, rhs_is_vec_len)) =
            parse_vector_len_comparison(&expanded_cond, d, &param_names)
        {
            let op = if vec_flipped {
                reverse_comparison_op(&vec_op_raw)
            } else {
                vec_op_raw
            };
            if rhs_is_vec_len {
                out.push(GuardCase {
                    kind: GuardKind::VectorLenParam {
                        lhs_param: vec_param,
                        rhs_param: vec_other,
                        op,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            if let Some(lit) = parse_numeric_literal(&vec_other) {
                out.push(GuardCase {
                    kind: GuardKind::VectorLenConst {
                        param_name: vec_param,
                        op,
                        rhs_literal: lit,
                    },
                    source: format!("{}: {}", fq, cond),
                });
                continue;
            }
            notes.push(format!(
                "{}: unresolved non-literal vector-length assert side '{}'",
                fq, vec_other
            ));
            continue;
        }
        let (param_side, op, other_side, flipped) =
            match parse_param_led_comparison(&expanded_cond, &param_names) {
                Some(v) => v,
                None => {
                    notes.push(format!("{}: unsupported assert condition '{}'", fq, cond));
                    continue;
                }
            };

        let op = if flipped {
            reverse_comparison_op(&op)
        } else {
            op
        };

        if let Some((getter_fn, getter_param)) = parse_getter_side(d, accessor_map, &other_side) {
            out.push(GuardCase {
                kind: GuardKind::ParamGetter {
                    param_name: param_side,
                    op,
                    getter_fn,
                    getter_param,
                },
                source: format!("{}: {}", fq, cond),
            });
            continue;
        }
        if let Some(clock_param) = parse_clock_timestamp_side(&other_side, d) {
            out.push(GuardCase {
                kind: GuardKind::ParamClockTimestamp {
                    param_name: param_side,
                    clock_param,
                    op,
                },
                source: format!("{}: {}", fq, cond),
            });
            continue;
        }

        if let Some(lit) = parse_numeric_literal(&other_side) {
            out.push(GuardCase {
                kind: GuardKind::ParamConst {
                    param_name: param_side,
                    op,
                    rhs_literal: lit,
                },
                source: format!("{}: {}", fq, cond),
            });
            continue;
        }
        if is_bool_literal(&other_side) || is_address_literal(&other_side) {
            out.push(GuardCase {
                kind: GuardKind::ParamConst {
                    param_name: param_side,
                    op,
                    rhs_literal: other_side,
                },
                source: format!("{}: {}", fq, cond),
            });
            continue;
        }

        notes.push(format!(
            "{}: unresolved non-literal assert side '{}'",
            fq, other_side
        ));
    }

    (out, notes)
}

fn numeric_plus_one(v: &str) -> Option<String> {
    let cleaned = v.replace('_', "");
    let parsed: u128 = cleaned.parse().ok()?;
    Some((parsed + 1).to_string())
}

fn numeric_minus_one(v: &str) -> Option<String> {
    let cleaned = v.replace('_', "");
    let parsed: u128 = cleaned.parse().ok()?;
    if parsed == 0 {
        None
    } else {
        Some((parsed - 1).to_string())
    }
}

fn default_u64_guard_arg_for_param(name: &str) -> String {
    let lower = name.to_ascii_lowercase();
    if lower.contains("ratio") {
        "50".to_string()
    } else {
        "1".to_string()
    }
}

fn violating_const_arg(op: &str, rhs_literal: &str) -> Option<String> {
    if let Some(n) = parse_numeric_literal(rhs_literal) {
        match op {
            ">" | "<" | "!=" => Some(n),
            "==" | "<=" => numeric_plus_one(&n),
            ">=" => numeric_minus_one(&n),
            _ => None,
        }
    } else if is_bool_literal(rhs_literal) {
        match op {
            "!=" => Some(rhs_literal.to_string()),
            "==" => {
                if rhs_literal.trim() == "true" {
                    Some("false".to_string())
                } else {
                    Some("true".to_string())
                }
            }
            _ => None,
        }
    } else if is_address_literal(rhs_literal) {
        match op {
            "!=" => Some(rhs_literal.to_string()),
            "==" => Some("OTHER".to_string()),
            _ => None,
        }
    } else {
        None
    }
}

fn violating_getter_arg(op: &str, getter_var: &str) -> Option<String> {
    match op {
        ">" | "<" | "!=" => Some(getter_var.to_string()),
        "==" | "<=" => Some(format!("({} + 1)", getter_var)),
        ">=" => Some(format!("({} - 1)", getter_var)),
        _ => None,
    }
}

fn pick_numeric_accessor_for_object_field(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    object_param: &str,
    field_name: &str,
) -> Option<String> {
    let obj_param = d.params.iter().find(|p| p.name == object_param)?;
    let obj_ty = normalize_param_object_type(&obj_param.ty);
    let module_accessors = accessor_map.get(&d.module_name)?;
    let candidates = module_accessors
        .iter()
        .filter(|a| a.param_ty == obj_ty)
        .collect::<Vec<_>>();
    if candidates.is_empty() {
        return None;
    }
    if candidates.len() == 1 {
        return Some(candidates[0].fn_name.clone());
    }
    let field_lower = field_name.to_ascii_lowercase();
    if let Some(best) = candidates
        .iter()
        .find(|a| a.fn_name.to_ascii_lowercase().contains(&field_lower))
    {
        return Some(best.fn_name.clone());
    }
    candidates.first().map(|a| a.fn_name.clone())
}

