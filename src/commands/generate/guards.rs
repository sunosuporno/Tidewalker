use super::catalog::{ModuleBootstrapCatalog, ModuleHelperCatalog};
use super::*;

#[derive(Debug, Clone)]
enum GuardKind {
    ParamConst {
        param_name: String,
        op: String,
        rhs_literal: String,
    },
    ParamGetter {
        param_name: String,
        op: String,
        getter_fn: String,
        getter_param: String,
    },
    CapRole {
        cap_param: String,
        cap_type: String,
    },
    ObjectFieldConst {
        param_name: String,
        field_name: String,
        op: String,
        rhs_literal: String,
    },
    ParamObjectField {
        param_name: String,
        object_param: String,
        field_name: String,
        op: String,
    },
    CoinValueConst {
        coin_param: String,
        op: String,
        rhs_literal: String,
    },
}

#[derive(Debug, Clone)]
struct GuardCase {
    kind: GuardKind,
    source: String,
}

#[derive(Debug, Clone)]
struct GuardCoinNeed {
    var_name: String,
    moved_on_call: bool,
    needs_mut_binding: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GuardObjectProvisionSource {
    SharedHelper,
    SharedInit,
    SharedCreator,
    OwnedHelper,
    OwnedInit,
    OwnedFactory,
    OwnedCreatorSender,
}

#[derive(Debug, Clone)]
struct GuardSharedCreatorPlan {
    fn_name: String,
    args: Vec<String>,
    type_args: Vec<String>,
}

fn default_u64_arg_for_param(name: &str) -> String {
    let lower = name.to_ascii_lowercase();
    if lower.contains("ratio") {
        "101".to_string()
    } else {
        "1".to_string()
    }
}

fn should_transfer_call_return(ret_ty: &str, module_name: &str) -> bool {
    let t = ret_ty.trim();
    if t.is_empty() || t == "()" || t.starts_with('&') {
        return false;
    }
    if is_numeric_type(t)
        || t == "bool"
        || t == "address"
        || is_string_type(t)
        || is_option_type(t)
        || t.starts_with("vector<")
    {
        return false;
    }
    if is_coin_type(t) || is_treasury_cap_type(t) {
        return true;
    }
    !t.contains("::") || t.starts_with(&format!("{}::", module_name))
}

fn module_fn_label(d: &FnDecl) -> String {
    format!("{}::{}", d.module_name, d.fn_name)
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

fn parse_param_led_comparison(
    expr: &str,
    param_names: &[String],
) -> Option<(String, String, String, bool)> {
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

fn parse_coin_value_side(
    expr: &str,
    param_names: &[String],
) -> Option<(String, String, String, bool)> {
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

fn parse_object_id_side(expr: &str, param_names: &[String]) -> Option<String> {
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

fn parse_assert_guard_cases(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
) -> (Vec<GuardCase>, Vec<String>) {
    let mut out = Vec::new();
    let mut notes = Vec::new();
    let fq = module_fn_label(d);

    for p in &d.params {
        let ty = normalize_param_object_type(&p.ty);
        if p.ty.trim().starts_with('&') && is_cap_type(&ty) && !is_treasury_cap_type(&ty) {
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

    for line in &d.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() || !stmt.contains("assert!(") {
            continue;
        }
        let cond = match extract_assert_condition(stmt) {
            Some(v) => v,
            None => continue,
        };
        if let Some((object_param, field_name, op_raw, other_side, flipped)) =
            parse_object_field_led_comparison(&cond, &param_names)
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
            notes.push(format!(
                "{}: unresolved object-field assert side '{}'",
                fq, other_side
            ));
            continue;
        }
        if let Some((coin_param, op_raw, other_side, flipped)) =
            parse_coin_value_side(&cond, &param_names)
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
            notes.push(format!(
                "{}: unresolved non-literal assert side '{}'",
                fq, other_side
            ));
            continue;
        }
        let (param_side, op, other_side, flipped) =
            match parse_param_led_comparison(&cond, &param_names) {
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

fn synthesize_guard_factory_args(factory: &FnDecl) -> Option<Vec<String>> {
    let mut args = Vec::new();
    for p in &factory.params {
        let t = p.ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_vector_type(t) && !t.starts_with('&') {
            args.push(vector_literal_expr_for_type(t)?);
        } else if t == "u64" {
            args.push(default_u64_arg_for_param(&p.name));
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            args.push("OTHER".to_string());
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else {
            return None;
        }
    }
    Some(args)
}

fn pick_guard_factory_call_for_type(
    d: &FnDecl,
    type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> Option<(String, Vec<String>)> {
    let mut best: Option<(usize, String, Vec<String>)> = None;
    for f in module_fns.values() {
        if f.is_test_only {
            continue;
        }
        if !(f.is_public || f.is_entry) {
            continue;
        }
        if f.fn_name == d.fn_name || f.fn_name == "init" {
            continue;
        }
        let ret = match &f.return_ty {
            Some(v) => v,
            None => continue,
        };
        if type_key_from_type_name(ret) != type_key {
            continue;
        }
        let Some(args) = synthesize_guard_factory_args(f) else {
            continue;
        };
        let score = f.params.len();
        let candidate = (score, f.fn_name.clone(), args);
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some(candidate);
        }
    }
    best.map(|(_, name, args)| (name, args))
}

fn pick_guard_shared_creator_plan(
    d: &FnDecl,
    module_fns: &std::collections::HashMap<String, FnDecl>,
) -> Option<GuardSharedCreatorPlan> {
    let mut candidates = module_fns
        .values()
        .filter(|f| {
            !f.is_test_only
                && (f.is_public || f.is_entry)
                && f.fn_name != d.fn_name
                && f.fn_name.starts_with("create_shared_")
                && f.params.iter().any(|p| p.ty.contains("TxContext"))
        })
        .collect::<Vec<_>>();
    candidates.sort_by_key(|f| f.params.len());
    let best = candidates.first()?;
    let args = synthesize_guard_factory_args(best)?;
    let type_args = if has_unbound_type_params(best) {
        default_type_args_for_params(&best.type_params)
    } else {
        Vec::new()
    };
    Some(GuardSharedCreatorPlan {
        fn_name: best.fn_name.clone(),
        args,
        type_args,
    })
}

fn build_common_call_plan(
    d: &FnDecl,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
) -> Option<(
    Vec<String>,
    Vec<String>,
    Vec<String>,
    std::collections::HashMap<String, String>,
    std::collections::HashMap<String, String>,
)> {
    let mut lines_before_tx = Vec::new();
    let mut call_args = Vec::new();
    let mut cleanup = Vec::new();

    let mut param_runtime: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_arg_values: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();

    let mut object_needs: Vec<ObjectNeed> = Vec::new();
    let mut coin_needs: Vec<GuardCoinNeed> = Vec::new();
    let default_type_args = default_type_args_for_params(&d.type_params);

    for param in &d.params {
        let resolved_ty = concretize_type_params(&param.ty, &d.type_params, &default_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            call_args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if t == "address" {
            call_args.push("OTHER".to_string());
            param_arg_values.insert(param.name.clone(), "OTHER".to_string());
        } else if t == "u64" {
            let v = default_u64_arg_for_param(&param.name);
            call_args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "bool" {
            call_args.push("false".to_string());
            param_arg_values.insert(param.name.clone(), "false".to_string());
        } else if is_option_type(t) {
            call_args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            call_args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else if is_vector_type(t) {
            let vec_expr = vector_literal_expr_for_type(t)?;
            if t.starts_with('&') {
                let var_name = format!("vec_{}", sanitize_ident(&param.name));
                let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                lines_before_tx.push(format!("let {}{} = {};", maybe_mut, var_name, vec_expr));
                if t.starts_with("&mut") {
                    call_args.push(format!("&mut {}", var_name));
                } else {
                    call_args.push(format!("&{}", var_name));
                }
            } else {
                call_args.push(vec_expr);
            }
        } else if is_coin_type(t) {
            let var_name = format!("coin_{}", sanitize_ident(&param.name));
            let is_ref = t.starts_with('&');
            let arg = if t.starts_with("&mut") {
                format!("&mut {}", var_name)
            } else if t.starts_with('&') {
                format!("&{}", var_name)
            } else {
                var_name.clone()
            };
            call_args.push(arg);
            coin_needs.push(GuardCoinNeed {
                var_name,
                moved_on_call: !is_ref,
                needs_mut_binding: t.starts_with("&mut"),
            });
        } else if t.starts_with('&') {
            let obj = ObjectNeed::from_resolved(param, normalize_param_object_type(t));
            if obj.is_mut {
                call_args.push(format!("&mut {}", obj.var_name));
            } else {
                call_args.push(format!("&{}", obj.var_name));
            }
            param_runtime.insert(param.name.clone(), obj.var_name.clone());
            object_needs.push(obj);
        } else {
            return None;
        }
    }

    let empty_helpers = ModuleHelperCatalog::default();
    let module_helpers = helper_catalog.get(&d.module_name).unwrap_or(&empty_helpers);
    let mut shared_objects: Vec<ObjectNeed> = Vec::new();
    let mut owned_objects: Vec<ObjectNeed> = Vec::new();
    let mut object_sources: std::collections::HashMap<String, GuardObjectProvisionSource> =
        std::collections::HashMap::new();
    let mut factory_calls: std::collections::HashMap<String, (String, Vec<String>)> =
        std::collections::HashMap::new();
    let mut shared_creator_plan: Option<GuardSharedCreatorPlan> = None;
    let mut creator_non_cap_type_keys: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    let empty_bootstrap = ModuleBootstrapCatalog::default();
    let module_bootstrap = bootstrap_catalog
        .get(&d.module_name)
        .unwrap_or(&empty_bootstrap);
    let module_fns = fn_lookup.get(&d.module_name);
    for obj in &object_needs {
        if module_helpers.shared_types.contains(&obj.type_key) {
            shared_objects.push(obj.clone());
            object_sources.insert(
                obj.var_name.clone(),
                GuardObjectProvisionSource::SharedHelper,
            );
        } else if module_helpers.owned_types.contains(&obj.type_key) {
            owned_objects.push(obj.clone());
            object_sources.insert(
                obj.var_name.clone(),
                GuardObjectProvisionSource::OwnedHelper,
            );
        } else if module_bootstrap.one_time_witness_init
            && module_bootstrap.init_shared_types.contains(&obj.type_key)
        {
            shared_objects.push(obj.clone());
            object_sources.insert(obj.var_name.clone(), GuardObjectProvisionSource::SharedInit);
        } else if module_bootstrap.one_time_witness_init
            && module_bootstrap.init_owned_types.contains(&obj.type_key)
        {
            owned_objects.push(obj.clone());
            object_sources.insert(obj.var_name.clone(), GuardObjectProvisionSource::OwnedInit);
        } else if let Some((factory_fn, factory_args)) =
            module_fns.and_then(|fns| pick_guard_factory_call_for_type(d, &obj.type_key, fns))
        {
            owned_objects.push(obj.clone());
            object_sources.insert(
                obj.var_name.clone(),
                GuardObjectProvisionSource::OwnedFactory,
            );
            factory_calls.insert(obj.var_name.clone(), (factory_fn, factory_args));
        } else {
            if shared_creator_plan.is_none() {
                shared_creator_plan =
                    module_fns.and_then(|fns| pick_guard_shared_creator_plan(d, fns));
            }
            if shared_creator_plan.is_some() {
                if obj.type_key.contains("cap") {
                    owned_objects.push(obj.clone());
                    object_sources.insert(
                        obj.var_name.clone(),
                        GuardObjectProvisionSource::OwnedCreatorSender,
                    );
                } else {
                    creator_non_cap_type_keys.insert(obj.type_key.clone());
                    if creator_non_cap_type_keys.len() > 1 {
                        return None;
                    }
                    shared_objects.push(obj.clone());
                    object_sources.insert(
                        obj.var_name.clone(),
                        GuardObjectProvisionSource::SharedCreator,
                    );
                }
            } else {
                return None;
            }
        }
    }

    let needs_init_bootstrap = object_sources.values().any(|src| {
        matches!(
            src,
            GuardObjectProvisionSource::SharedInit | GuardObjectProvisionSource::OwnedInit
        )
    });
    let has_init_bootstrap_helper = module_fns
        .map(|fns| fns.contains_key("bootstrap_init_for_testing"))
        .unwrap_or(false);
    if needs_init_bootstrap && !has_init_bootstrap_helper {
        return None;
    }

    let mut needs_next_tx_before_use = false;
    if !shared_objects.is_empty() {
        let helper_shared = shared_objects
            .iter()
            .filter(|obj| {
                matches!(
                    object_sources.get(&obj.var_name),
                    Some(GuardObjectProvisionSource::SharedHelper)
                )
            })
            .collect::<Vec<_>>();
        if !helper_shared.is_empty() {
            lines_before_tx.push("{".to_string());
            for obj in helper_shared {
                lines_before_tx.push(format!(
                    "    {}::create_and_share_{}_for_testing(test_scenario::ctx(&mut scenario));",
                    d.module_name, obj.type_key
                ));
            }
            lines_before_tx.push("};".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if object_sources.values().any(|src| {
        matches!(
            src,
            GuardObjectProvisionSource::SharedCreator
                | GuardObjectProvisionSource::OwnedCreatorSender
        )
    }) {
        if let Some(plan) = &shared_creator_plan {
            lines_before_tx.push("{".to_string());
            let call_path = if plan.type_args.is_empty() {
                format!("{}::{}", d.module_name, plan.fn_name)
            } else {
                format!(
                    "{}::{}<{}>",
                    d.module_name,
                    plan.fn_name,
                    plan.type_args.join(", ")
                )
            };
            lines_before_tx.push(format!("    {}({});", call_path, plan.args.join(", ")));
            lines_before_tx.push("};".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if needs_init_bootstrap {
        lines_before_tx.push("{".to_string());
        lines_before_tx.push(format!(
            "    {}::bootstrap_init_for_testing(test_scenario::ctx(&mut scenario));",
            d.module_name
        ));
        lines_before_tx.push("};".to_string());
        needs_next_tx_before_use = true;
    }
    if needs_next_tx_before_use {
        lines_before_tx.push("test_scenario::next_tx(&mut scenario, SUPER_USER);".to_string());
    }

    for coin in &coin_needs {
        let maybe_mut = if coin.needs_mut_binding { "mut " } else { "" };
        lines_before_tx.push(format!(
            "let {}{} = coin::mint_for_testing<0x2::sui::SUI>(1000, test_scenario::ctx(&mut scenario));",
            maybe_mut, coin.var_name
        ));
    }
    for obj in &owned_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        match object_sources.get(&obj.var_name) {
            Some(GuardObjectProvisionSource::OwnedHelper) => {
                lines_before_tx.push(format!(
                    "let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
                    maybe_mut, obj.var_name, d.module_name, obj.type_key
                ));
            }
            Some(GuardObjectProvisionSource::OwnedInit) => {
                lines_before_tx.push(format!(
                    "let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
                    maybe_mut,
                    obj.var_name,
                    qualify_type_for_module(&d.module_name, &obj.type_name)
                ));
            }
            Some(GuardObjectProvisionSource::OwnedFactory) => {
                let (factory_fn, factory_args) = factory_calls.get(&obj.var_name)?;
                lines_before_tx.push(format!(
                    "let {}{} = {}::{}({});",
                    maybe_mut,
                    obj.var_name,
                    d.module_name,
                    factory_fn,
                    factory_args.join(", ")
                ));
            }
            Some(GuardObjectProvisionSource::OwnedCreatorSender) => {
                lines_before_tx.push(format!(
                    "let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
                    maybe_mut,
                    obj.var_name,
                    qualify_type_for_module(&d.module_name, &obj.type_name)
                ));
            }
            _ => {
                lines_before_tx.push(format!(
                    "let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
                    maybe_mut, obj.var_name, d.module_name, obj.type_key
                ));
            }
        }
    }
    for obj in &shared_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        let obj_ty = qualify_type_for_module(&d.module_name, &obj.type_name);
        lines_before_tx.push(format!(
            "let {}{} = scenario.take_shared<{}>();",
            maybe_mut, obj.var_name, obj_ty
        ));
    }

    for obj in &shared_objects {
        cleanup.push(format!("test_scenario::return_shared({});", obj.var_name));
    }
    for obj in &owned_objects {
        cleanup.push(format!(
            "transfer::public_transfer({}, SUPER_USER);",
            obj.var_name
        ));
    }
    for coin in &coin_needs {
        if !coin.moved_on_call {
            cleanup.push(format!(
                "transfer::public_transfer({}, SUPER_USER);",
                coin.var_name
            ));
        }
    }

    Some((
        lines_before_tx,
        call_args,
        cleanup,
        param_runtime,
        param_arg_values,
    ))
}

fn render_cap_role_guard_test(
    d: &FnDecl,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    case_idx: usize,
    cap_param: &str,
    cap_type: &str,
) -> Option<Vec<String>> {
    let module_fns = fn_lookup.get(&d.module_name);
    let empty_helpers = ModuleHelperCatalog::default();
    let module_helpers = helper_catalog.get(&d.module_name).unwrap_or(&empty_helpers);
    let empty_bootstrap = ModuleBootstrapCatalog::default();
    let module_bootstrap = bootstrap_catalog
        .get(&d.module_name)
        .unwrap_or(&empty_bootstrap);
    let has_bootstrap_helper = fn_lookup
        .get(&d.module_name)
        .map(|fns| fns.contains_key("bootstrap_init_for_testing"))
        .unwrap_or(false);
    let cap_type_key = type_key_from_type_name(cap_type);
    let use_helper_path = module_helpers.owned_types.contains(&cap_type_key)
        || module_helpers.shared_types.contains(&cap_type_key);
    let use_bootstrap_path = module_bootstrap.one_time_witness_init
        && module_bootstrap.init_owned_types.contains(&cap_type_key)
        && has_bootstrap_helper;
    let creator_plan = if !use_helper_path && !use_bootstrap_path {
        module_fns.and_then(|fns| pick_guard_shared_creator_plan(d, fns))
    } else {
        None
    };
    if !use_helper_path && !use_bootstrap_path && creator_plan.is_none() {
        return None;
    }

    let module_short = d.module_name.split("::").last().unwrap_or("m");
    let mut lines = Vec::new();
    lines.push("#[test]".to_string());
    lines.push("#[expected_failure]".to_string());
    lines.push(format!(
        "fun test_abort_{}_{}_role_{}_{}() {{",
        module_short,
        d.fn_name,
        sanitize_ident(cap_param),
        case_idx
    ));
    lines.push("    let mut scenario = test_scenario::begin(SUPER_USER);".to_string());
    if use_helper_path {
        lines.push("    {".to_string());
        lines.push(format!(
            "        let cap_obj = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
            d.module_name, cap_type_key
        ));
        lines.push("        transfer::public_transfer(cap_obj, SUPER_USER);".to_string());
        lines.push("    };".to_string());
    } else if use_bootstrap_path {
        lines.push("    {".to_string());
        lines.push(format!(
            "        {}::bootstrap_init_for_testing(test_scenario::ctx(&mut scenario));",
            d.module_name
        ));
        lines.push("    };".to_string());
    } else if let Some(plan) = creator_plan {
        lines.push("    {".to_string());
        let call_path = if plan.type_args.is_empty() {
            format!("{}::{}", d.module_name, plan.fn_name)
        } else {
            format!(
                "{}::{}<{}>",
                d.module_name,
                plan.fn_name,
                plan.type_args.join(", ")
            )
        };
        lines.push(format!("        {}({});", call_path, plan.args.join(", ")));
        lines.push("    };".to_string());
    }
    lines.push("    test_scenario::next_tx(&mut scenario, OTHER);".to_string());
    lines.push("    {".to_string());
    lines.push(format!(
        "        let missing_cap = test_scenario::take_from_sender<{}>(&scenario);",
        qualify_type_for_module(&d.module_name, cap_type)
    ));
    lines.push(format!(
        "        test_scenario::return_to_sender(&scenario, missing_cap);"
    ));
    lines.push(format!(
        "        // expected to fail before this line because OTHER should not own {}",
        qualify_type_for_module(&d.module_name, cap_type)
    ));
    lines.push("    };".to_string());
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
}

fn render_param_guard_test(
    d: &FnDecl,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    case_idx: usize,
    target_param: &str,
    prep_lines: &[String],
    bad_expr: &str,
) -> Option<Vec<String>> {
    let (setup_lines, mut call_args, cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, helper_catalog, bootstrap_catalog, fn_lookup)?;
    let param_idx = d.params.iter().position(|p| p.name == target_param)?;
    call_args[param_idx] = bad_expr.to_string();

    render_guard_call_test(d, case_idx, &setup_lines, prep_lines, &call_args, &cleanup)
}

fn render_coin_value_guard_test(
    d: &FnDecl,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    case_idx: usize,
    coin_param: &str,
    op: &str,
    rhs_literal: &str,
) -> Option<Vec<String>> {
    let bad_amount = violating_const_arg(op, rhs_literal)?;
    let _ = parse_numeric_literal(&bad_amount)?;
    let (mut setup_lines, mut call_args, mut cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, helper_catalog, bootstrap_catalog, fn_lookup)?;
    let param_idx = d.params.iter().position(|p| p.name == coin_param)?;
    let param_ty = d.params.get(param_idx)?.ty.trim();
    if !is_coin_type(param_ty) {
        return None;
    }
    let default_coin_var = format!("coin_{}", sanitize_ident(coin_param));
    setup_lines.retain(|l| !(l.contains("coin::mint_for_testing") && l.contains(&default_coin_var)));
    cleanup.retain(|l| !l.contains(&format!("{}{}", default_coin_var, ",")));

    let bad_coin_var = format!("guard_bad_coin_{}", case_idx);
    let mut prep = Vec::new();
    let maybe_mut = if param_ty.starts_with("&mut") { "mut " } else { "" };
    prep.push(format!(
        "let {}{} = coin::mint_for_testing<0x2::sui::SUI>({}, test_scenario::ctx(&mut scenario));",
        maybe_mut, bad_coin_var, bad_amount
    ));
    call_args[param_idx] = if param_ty.starts_with("&mut") {
        format!("&mut {}", bad_coin_var)
    } else if param_ty.starts_with('&') {
        format!("&{}", bad_coin_var)
    } else {
        bad_coin_var.clone()
    };
    if param_ty.starts_with('&') {
        cleanup.push(format!(
            "transfer::public_transfer({}, SUPER_USER);",
            bad_coin_var
        ));
    }
    render_guard_call_test(d, case_idx, &setup_lines, &prep, &call_args, &cleanup)
}

fn render_guard_call_test(
    d: &FnDecl,
    case_idx: usize,
    setup_lines: &[String],
    prep_lines: &[String],
    call_args: &[String],
    cleanup: &[String],
) -> Option<Vec<String>> {
    if call_args.is_empty() && !d.params.is_empty() {
        return None;
    }

    let fq = module_fn_label(d);
    let module_short = d.module_name.split("::").last().unwrap_or("m");
    let mut lines = Vec::new();
    lines.push("#[test]".to_string());
    lines.push("#[expected_failure]".to_string());
    lines.push(format!(
        "fun test_abort_{}_{}_guard_{}() {{",
        module_short, d.fn_name, case_idx
    ));
    lines.push("    let mut scenario = test_scenario::begin(SUPER_USER);".to_string());
    for l in setup_lines {
        lines.push(format!("    {}", l));
    }
    lines.push("    {".to_string());
    for l in prep_lines {
        lines.push(format!("        {}", l));
    }
    let default_type_args = default_type_args_for_params(&d.type_params);
    let call_target = if has_unbound_type_params(d) {
        if default_type_args.is_empty() {
            fq.clone()
        } else {
            format!("{}<{}>", fq, default_type_args.join(", "))
        }
    } else {
        fq.clone()
    };
    let call_expr = format!("{}({})", call_target, call_args.join(", "));
    if let Some(ret_ty) = d.return_ty.as_ref() {
        if should_transfer_call_return(ret_ty, &d.module_name) {
            lines.push(format!("        let tw_ret_obj = {};", call_expr));
            lines.push("        transfer::public_transfer(tw_ret_obj, SUPER_USER);".to_string());
        } else {
            lines.push(format!("        {};", call_expr));
        }
    } else {
        lines.push(format!("        {};", call_expr));
    }
    for l in cleanup {
        lines.push(format!("        {}", l));
    }
    lines.push("    };".to_string());
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
}

fn extract_helper_field_literal(helper: &FnDecl, field_name: &str) -> Option<String> {
    for line in &helper.body_lines {
        let stmt = line.split("//").next().unwrap_or("").trim();
        if stmt.is_empty() {
            continue;
        }
        let mut candidate = stmt.trim_end_matches(';').trim();
        if let Some((_, rhs)) = candidate.split_once('=') {
            candidate = rhs.trim();
        }
        if let Some((lhs, rhs_raw)) = candidate.split_once(':') {
            if lhs.trim() != field_name {
                continue;
            }
            let rhs = rhs_raw.trim().trim_end_matches(',').trim();
            if let Some(v) = parse_numeric_literal(rhs) {
                return Some(v);
            }
            if is_bool_literal(rhs) || is_address_literal(rhs) {
                return Some(rhs.to_string());
            }
        }
    }
    None
}

fn lookup_helper_field_literal(
    module_fns: &std::collections::HashMap<String, FnDecl>,
    type_key: &str,
    field_name: &str,
) -> Option<String> {
    let shared_helper = format!("create_and_share_{}_for_testing", type_key);
    let owned_helper = format!("create_{}_for_testing", type_key);
    if let Some(f) = module_fns.get(&shared_helper) {
        if let Some(v) = extract_helper_field_literal(f, field_name) {
            return Some(v);
        }
    }
    if let Some(f) = module_fns.get(&owned_helper) {
        if let Some(v) = extract_helper_field_literal(f, field_name) {
            return Some(v);
        }
    }
    None
}

fn literal_compare(lhs: &str, op: &str, rhs: &str) -> Option<bool> {
    if let (Some(lhs_n), Some(rhs_n)) = (parse_numeric_literal(lhs), parse_numeric_literal(rhs)) {
        let lhs_v: u128 = lhs_n.replace('_', "").parse().ok()?;
        let rhs_v: u128 = rhs_n.replace('_', "").parse().ok()?;
        return match op {
            ">" => Some(lhs_v > rhs_v),
            "<" => Some(lhs_v < rhs_v),
            ">=" => Some(lhs_v >= rhs_v),
            "<=" => Some(lhs_v <= rhs_v),
            "==" => Some(lhs_v == rhs_v),
            "!=" => Some(lhs_v != rhs_v),
            _ => None,
        };
    }
    if is_bool_literal(lhs) && is_bool_literal(rhs) {
        let lhs_v = lhs.trim() == "true";
        let rhs_v = rhs.trim() == "true";
        return match op {
            "==" => Some(lhs_v == rhs_v),
            "!=" => Some(lhs_v != rhs_v),
            _ => None,
        };
    }
    if is_address_literal(lhs) && is_address_literal(rhs) {
        let lhs_v = lhs.trim();
        let rhs_v = rhs.trim();
        return match op {
            "==" => Some(lhs_v == rhs_v),
            "!=" => Some(lhs_v != rhs_v),
            _ => None,
        };
    }
    None
}

fn render_object_field_const_guard_test(
    d: &FnDecl,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    case_idx: usize,
    param_name: &str,
    field_name: &str,
    op: &str,
    rhs_literal: &str,
) -> Option<Vec<String>> {
    let (setup_lines, call_args, cleanup, _param_runtime, _param_arg_values) =
        build_common_call_plan(d, helper_catalog, bootstrap_catalog, fn_lookup)?;
    let param = d.params.iter().find(|p| p.name == param_name)?;
    let type_name = normalize_param_object_type(&param.ty);
    let type_key = type_key_from_type_name(&type_name);
    let module_fns = fn_lookup.get(&d.module_name)?;
    let helper_default = lookup_helper_field_literal(module_fns, &type_key, field_name)?;
    let guard_holds = literal_compare(&helper_default, op, rhs_literal)?;
    if guard_holds {
        return None;
    }
    render_guard_call_test(d, case_idx, &setup_lines, &[], &call_args, &cleanup)
}

fn render_param_object_field_guard_test(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    case_idx: usize,
    param_name: &str,
    object_param: &str,
    field_name: &str,
    op: &str,
) -> Option<Vec<String>> {
    let (_, _, _, param_runtime, _) =
        build_common_call_plan(d, helper_catalog, bootstrap_catalog, fn_lookup)?;
    let runtime_obj = param_runtime.get(object_param)?;
    let getter_fn = pick_numeric_accessor_for_object_field(d, accessor_map, object_param, field_name)?;
    let bound_var = format!("guard_obj_bound_{}", case_idx);
    let prep = vec![format!(
        "let {} = {}::{}(&{});",
        bound_var, d.module_name, getter_fn, runtime_obj
    )];
    let bad_expr = violating_getter_arg(op, &bound_var)?;
    render_param_guard_test(
        d,
        helper_catalog,
        bootstrap_catalog,
        fn_lookup,
        case_idx,
        param_name,
        &prep,
        &bad_expr,
    )
}

pub(super) fn render_guard_tests_for_function(
    d: &FnDecl,
    accessor_map: &std::collections::HashMap<String, Vec<AccessorSig>>,
    helper_catalog: &std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
) -> (Vec<Vec<String>>, Vec<String>) {
    let mut tests = Vec::new();
    let mut notes = Vec::new();

    let (cases, parse_notes) = parse_assert_guard_cases(d, accessor_map);
    notes.extend(parse_notes);

    for (idx, case) in cases.iter().enumerate() {
        match &case.kind {
            GuardKind::CapRole {
                cap_param,
                cap_type,
            } => {
                if let Some(lines) = render_cap_role_guard_test(
                    d,
                    helper_catalog,
                    bootstrap_catalog,
                    fn_lookup,
                    idx,
                    cap_param,
                    cap_type,
                )
                {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize role/cap failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ParamConst {
                param_name,
                op,
                rhs_literal,
            } => {
                let Some(bad_expr) = violating_const_arg(op, rhs_literal) else {
                    notes.push(format!(
                        "{}: unsupported param-vs-const guard '{}'",
                        module_fn_label(d),
                        case.source
                    ));
                    continue;
                };
                if let Some(lines) = render_param_guard_test(
                    d,
                    helper_catalog,
                    bootstrap_catalog,
                    fn_lookup,
                    idx,
                    param_name,
                    &[],
                    &bad_expr,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize param-vs-const failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ParamGetter {
                param_name,
                op,
                getter_fn,
                getter_param,
            } => {
                let Some((_, _, _, param_runtime, _)) =
                    build_common_call_plan(d, helper_catalog, bootstrap_catalog, fn_lookup)
                else {
                    notes.push(format!(
                        "{}: could not synthesize getter-bound failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                    continue;
                };
                let runtime_obj = match param_runtime.get(getter_param) {
                    Some(v) => v,
                    None => {
                        notes.push(format!(
                            "{}: missing runtime object for getter-bound guard ({})",
                            module_fn_label(d),
                            case.source
                        ));
                        continue;
                    }
                };
                let bound_var = format!("guard_bound_{}", idx);
                let prep = vec![format!(
                    "let {} = {}::{}(&{});",
                    bound_var, d.module_name, getter_fn, runtime_obj
                )];
                let Some(bad_expr) = violating_getter_arg(op, &bound_var) else {
                    notes.push(format!(
                        "{}: unsupported param-vs-getter guard '{}'",
                        module_fn_label(d),
                        case.source
                    ));
                    continue;
                };
                if let Some(lines) = render_param_guard_test(
                    d,
                    helper_catalog,
                    bootstrap_catalog,
                    fn_lookup,
                    idx,
                    param_name,
                    &prep,
                    &bad_expr,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize param-vs-getter failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ObjectFieldConst {
                param_name,
                field_name,
                op,
                rhs_literal,
            } => {
                if let Some(lines) = render_object_field_const_guard_test(
                    d,
                    helper_catalog,
                    bootstrap_catalog,
                    fn_lookup,
                    idx,
                    param_name,
                    field_name,
                    op,
                    rhs_literal,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize object-field failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::ParamObjectField {
                param_name,
                object_param,
                field_name,
                op,
            } => {
                if let Some(lines) = render_param_object_field_guard_test(
                    d,
                    accessor_map,
                    helper_catalog,
                    bootstrap_catalog,
                    fn_lookup,
                    idx,
                    param_name,
                    object_param,
                    field_name,
                    op,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize param-vs-object-field failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
            GuardKind::CoinValueConst {
                coin_param,
                op,
                rhs_literal,
            } => {
                if let Some(lines) = render_coin_value_guard_test(
                    d,
                    helper_catalog,
                    bootstrap_catalog,
                    fn_lookup,
                    idx,
                    coin_param,
                    op,
                    rhs_literal,
                ) {
                    tests.push(lines);
                } else {
                    notes.push(format!(
                        "{}: could not synthesize coin-value failure test ({})",
                        module_fn_label(d),
                        case.source
                    ));
                }
            }
        }
    }

    (tests, notes)
}
