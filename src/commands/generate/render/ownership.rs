use super::*;

fn is_scalar_like_type(ty: &str) -> bool {
    let norm = ty.trim().replace(' ', "");
    if norm.is_empty() {
        return true;
    }
    if is_numeric_type(&norm) || norm == "bool" || norm == "address" {
        return true;
    }
    if norm.starts_with("vector<") {
        return true;
    }
    if is_option_type(&norm) || is_string_type(&norm) {
        return true;
    }
    if norm == "ID" || norm.ends_with("::ID") || norm == "UID" || norm.ends_with("::UID") {
        return true;
    }
    false
}

fn is_trackable_object_type(ty: &str) -> bool {
    let norm = normalize_param_object_type(ty);
    if norm.contains("TxContext") || is_coin_type(&norm) {
        return false;
    }
    !is_scalar_like_type(&norm)
}

fn parse_let_binding_type_annotation(stmt: &str) -> Option<String> {
    let rest = stmt.strip_prefix("let ")?.trim();
    let lhs = rest.split_once('=').map(|(l, _)| l).unwrap_or(rest).trim();
    let lhs = lhs.strip_prefix("mut ").unwrap_or(lhs).trim();
    let (_, ty) = lhs.split_once(':')?;
    let out = normalize_param_object_type(ty);
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

fn parse_struct_destructure_id_bindings(stmt: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    let rest = match stmt.strip_prefix("let ") {
        Some(v) => v.trim(),
        None => return out,
    };
    let lhs = rest.split_once('=').map(|(l, _)| l).unwrap_or(rest).trim();
    let (ty_raw, fields_raw) = match lhs.split_once('{') {
        Some(v) => v,
        None => return out,
    };
    let fields = match fields_raw.rsplit_once('}') {
        Some((inner, _)) => inner,
        None => return out,
    };
    let ty =
        normalize_param_object_type(ty_raw.trim().strip_prefix("mut ").unwrap_or(ty_raw).trim());
    if !is_trackable_object_type(&ty) {
        return out;
    }
    for part in fields.split(',') {
        let token = part.trim();
        if token == "id" {
            out.push(("id".to_string(), ty.clone()));
            continue;
        }
        if let Some((left, right)) = token.split_once(':') {
            if left.trim() != "id" {
                continue;
            }
            let id_var = right.trim();
            if is_ident(id_var) {
                out.push((id_var.to_string(), ty.clone()));
            }
        }
    }
    out
}

fn resolve_object_var_and_type(
    obj_arg: &str,
    aliases: &std::collections::HashMap<String, String>,
    param_object_types: &std::collections::HashMap<String, String>,
    local_object_types: &std::collections::HashMap<String, String>,
) -> Option<(String, String)> {
    let obj_token = strip_ref_and_parens_text(obj_arg);
    if !is_ident(obj_token) {
        return None;
    }
    let resolved = resolve_alias_path(obj_token, aliases);
    if let Some(ty) = param_object_types.get(&resolved) {
        return Some((resolved, ty.clone()));
    }
    if let Some(ty) = local_object_types.get(&resolved) {
        return Some((resolved, ty.clone()));
    }
    if let Some(ty) = param_object_types.get(obj_token) {
        return Some((obj_token.to_string(), ty.clone()));
    }
    if let Some(ty) = local_object_types.get(obj_token) {
        return Some((obj_token.to_string(), ty.clone()));
    }
    None
}

fn resolve_deleted_object_type(
    stmt: &str,
    aliases: &std::collections::HashMap<String, String>,
    param_object_types: &std::collections::HashMap<String, String>,
    local_object_types: &std::collections::HashMap<String, String>,
    id_owner_types: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let prefix = stmt.split_once(".delete(")?.0.trim();
    let token = strip_ref_and_parens_text(prefix);
    if let Some(base) = token.strip_suffix(".id") {
        let base = base.trim();
        if !is_ident(base) {
            return None;
        }
        let resolved = resolve_alias_path(base, aliases);
        return param_object_types
            .get(&resolved)
            .or_else(|| local_object_types.get(&resolved))
            .or_else(|| param_object_types.get(base))
            .or_else(|| local_object_types.get(base))
            .cloned();
    }
    if !is_ident(token) {
        return None;
    }
    let resolved = resolve_alias_path(token, aliases);
    id_owner_types
        .get(&resolved)
        .or_else(|| id_owner_types.get(token))
        .cloned()
}

pub(super) fn resolve_cap_transfer_recipient(
    raw: &str,
    param_arg_values: &std::collections::HashMap<String, String>,
) -> Option<String> {
    let t = raw.trim();
    if t.contains("sender(") || t.contains("tx_context::sender") {
        return Some("SUPER_USER".to_string());
    }
    if t == "SUPER_USER" || t == "OTHER" {
        return Some(t.to_string());
    }
    if t.starts_with('@') {
        return Some(t.to_string());
    }
    if is_ident(t) {
        return param_arg_values.get(t).cloned();
    }
    None
}

pub(super) fn build_ownership_checks(
    d: &FnDecl,
    param_arg_values: &std::collections::HashMap<String, String>,
    preexisting_shared_type_keys: &std::collections::HashSet<String>,
) -> (
    Vec<OwnershipTransferCheck>,
    Vec<OwnershipShareCheck>,
    StateChangeSummary,
) {
    let mut transfer_checks = Vec::new();
    let mut share_checks = Vec::new();
    let mut summary = StateChangeSummary::default();
    let has_non_linear_flow = has_branch_or_loop(&d.body_lines);
    let mut aliases: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut local_object_types: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_object_types: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut id_owner_types: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut minted_cap_vars: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut coin_vars: std::collections::HashSet<String> = std::collections::HashSet::new();
    for p in &d.params {
        if is_coin_type(&p.ty) {
            coin_vars.insert(p.name.clone());
        }
        let ty = normalize_param_object_type(&p.ty);
        if p.ty.trim().starts_with('&') {
            continue;
        }
        if is_trackable_object_type(&ty) {
            param_object_types.insert(p.name.clone(), ty);
        }
    }

    for line in &d.body_lines {
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
        if let Some((name, target)) = parse_alias_binding(stmt) {
            let resolved_target = resolve_alias_path(&target, &aliases);
            if coin_vars.contains(&target) || coin_vars.contains(&resolved_target) {
                coin_vars.insert(name.clone());
            }
            aliases.insert(name, target);
            continue;
        }
        if stmt.starts_with("let ") {
            for (id_var, owner_ty) in parse_struct_destructure_id_bindings(stmt) {
                id_owner_types.insert(id_var, owner_ty);
            }
            if let Some(name) = parse_let_binding_name(stmt) {
                aliases.remove(&name);
                local_object_types.remove(&name);
                minted_cap_vars.remove(&name);
                coin_vars.remove(&name);
                if let Some(ty) = parse_let_binding_type_annotation(stmt) {
                    if is_trackable_object_type(&ty) {
                        local_object_types.insert(name.clone(), ty);
                    }
                }
                if let Some((_, rhs)) = stmt.split_once('=') {
                    let rhs = rhs.trim();
                    let rhs_lower = rhs.to_ascii_lowercase();
                    let coin_like_rhs = rhs_lower.contains("coin::")
                        || rhs_lower.contains(".into_coin(")
                        || rhs_lower.contains(".split(")
                        || rhs_lower.contains(".join(");
                    if coin_like_rhs {
                        coin_vars.insert(name.clone());
                    }
                    if rhs.contains('{') && rhs.contains('}') {
                        if let Some(type_token) = rhs.split('{').next() {
                            let ty = normalize_param_object_type(type_token.trim());
                            if is_trackable_object_type(&ty) {
                                local_object_types.insert(name.clone(), ty.clone());
                                if is_cap_type(&ty)
                                    && (rhs.contains("object::new(")
                                        || rhs.contains("sui::object::new("))
                                {
                                    minted_cap_vars.insert(name.clone(), ty);
                                }
                            }
                        }
                    }
                }
            }
        }

        if stmt.contains(".delete(") {
            if let Some(obj_ty) = resolve_deleted_object_type(
                stmt,
                &aliases,
                &param_object_types,
                &local_object_types,
                &id_owner_types,
            ) {
                let label_ty = if obj_ty.contains("::") {
                    obj_ty
                } else {
                    qualify_type_for_module(&d.module_name, &obj_ty)
                };
                summary.add_potential(format!("object delete {}", label_ty));
            } else {
                summary.add_potential("object delete".to_string());
            }
        }

        let transfer_args = extract_namespace_args(stmt, "transfer::public_transfer(")
            .or_else(|| extract_namespace_args(stmt, "transfer::transfer("));
        if let Some(args) = transfer_args {
            let obj_arg = match args.first() {
                Some(v) => v.as_str(),
                None => continue,
            };
            let recipient_arg = match args.get(1) {
                Some(v) => v.as_str(),
                None => continue,
            };
            let Some((resolved_obj, obj_ty)) = resolve_object_var_and_type(
                obj_arg,
                &aliases,
                &param_object_types,
                &local_object_types,
            ) else {
                let obj_token = strip_ref_and_parens_text(obj_arg);
                if is_ident(obj_token) {
                    let resolved_token = resolve_alias_path(obj_token, &aliases);
                    if coin_vars.contains(obj_token) || coin_vars.contains(&resolved_token) {
                        continue;
                    }
                }
                summary.add_potential("ownership transfer object".to_string());
                continue;
            };
            let label_ty = if obj_ty.contains("::") {
                obj_ty.clone()
            } else {
                qualify_type_for_module(&d.module_name, &obj_ty)
            };
            let transfer_label = if is_cap_type(&obj_ty) {
                format!("cap transfer {}", label_ty)
            } else {
                format!("ownership transfer {}", label_ty)
            };
            if has_non_linear_flow {
                summary.add_potential(transfer_label);
            } else if let Some(recipient) =
                resolve_cap_transfer_recipient(recipient_arg, param_arg_values)
            {
                transfer_checks.push(OwnershipTransferCheck {
                    recipient,
                    object_type: label_ty.clone(),
                });
                summary.add_asserted(transfer_label);
            } else {
                summary.add_potential(transfer_label);
            }
            if is_cap_type(&obj_ty) && minted_cap_vars.contains_key(&resolved_obj) {
                let mint_label = format!("cap mint {}", label_ty);
                if has_non_linear_flow {
                    summary.add_potential(mint_label);
                } else {
                    summary.add_asserted(mint_label);
                }
            }
        }

        let share_args = extract_namespace_args(stmt, "transfer::share_object(")
            .or_else(|| extract_namespace_args(stmt, "transfer::public_share_object("));
        if let Some(args) = share_args {
            let obj_arg = match args.first() {
                Some(v) => v.as_str(),
                None => continue,
            };
            let Some((resolved_obj, obj_ty)) = resolve_object_var_and_type(
                obj_arg,
                &aliases,
                &param_object_types,
                &local_object_types,
            ) else {
                summary.add_potential("ownership share object".to_string());
                continue;
            };
            let label_ty = if obj_ty.contains("::") {
                obj_ty.clone()
            } else {
                qualify_type_for_module(&d.module_name, &obj_ty)
            };
            let share_label = if is_cap_type(&obj_ty) {
                format!("cap share {}", label_ty)
            } else {
                format!("ownership share {}", label_ty)
            };
            if has_non_linear_flow {
                summary.add_potential(share_label);
            } else {
                let type_key = type_key_from_type_name(&obj_ty);
                if preexisting_shared_type_keys.contains(&type_key) {
                    summary.add_potential(share_label);
                } else {
                    share_checks.push(OwnershipShareCheck {
                        object_type: label_ty.clone(),
                    });
                    summary.add_asserted(share_label);
                }
            }
            if is_cap_type(&obj_ty) && minted_cap_vars.contains_key(&resolved_obj) {
                let mint_label = format!("cap mint {}", label_ty);
                if has_non_linear_flow {
                    summary.add_potential(mint_label);
                } else {
                    summary.add_asserted(mint_label);
                }
            }
        }
    }

    for (_var, cap_ty) in minted_cap_vars {
        let label_ty = if cap_ty.contains("::") {
            cap_ty
        } else {
            qualify_type_for_module(&d.module_name, &cap_ty)
        };
        summary.add_potential(format!("cap mint {}", label_ty));
    }

    (transfer_checks, share_checks, summary)
}
