fn synthesize_guard_factory_args(
    factory: &FnDecl,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
) -> Option<Vec<String>> {
    let mut args = Vec::new();
    let factory_type_args = default_type_args_for_decl(factory);
    for p in &factory.params {
        let resolved_ty = resolved_param_ty(factory, p, &factory_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_coin_type(t) && !t.starts_with('&') {
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &factory.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            args.push(format!(
                "coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario))",
                coin_ty
            ));
        } else if is_vector_type(t) && !t.starts_with('&') {
            args.push(vector_literal_expr_for_type(t)?);
        } else if t == "u64" {
            args.push(default_u64_guard_arg_for_param(&p.name));
        } else if is_numeric_type(t) {
            args.push("1".to_string());
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            args.push("SUPER_USER".to_string());
        } else if is_id_type(t) {
            args.push("sui::object::id_from_address(OTHER)".to_string());
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else if !t.starts_with('&') {
            args.push(synthesize_value_expr_for_type(
                t,
                &factory.module_name,
                fn_lookup,
                Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                0,
            )?);
        } else {
            return None;
        }
    }
    Some(args)
}

fn synthesize_guard_factory_args_with_refs(
    factory: &FnDecl,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Option<GuardFactoryArgPlan> {
    let mut args = Vec::new();
    let mut prep_lines = Vec::new();
    let mut cleanup_lines = Vec::new();
    let factory_type_args = default_type_args_for_decl(factory);
    for (idx, p) in factory.params.iter().enumerate() {
        let resolved_ty = resolved_param_ty(factory, p, &factory_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
            continue;
        }
        if is_clock_type(t) {
            let var = format!("guard_factory_clock_{}_{}", idx, sanitize_ident(&p.name));
            if t.starts_with("&mut") {
                prep_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&mut {}", var));
            } else if t.starts_with('&') {
                prep_lines.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(format!("&{}", var));
            } else {
                prep_lines.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var
                ));
                args.push(var.clone());
            }
            cleanup_lines.push(format!("sui::clock::destroy_for_testing({});", var));
            continue;
        }
        if is_coin_type(t) {
            let var = format!("guard_factory_coin_{}_{}", idx, sanitize_ident(&p.name));
            let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &factory.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            prep_lines.push(format!(
                "let {}{} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
                maybe_mut, var, coin_ty
            ));
            if t.starts_with("&mut") {
                args.push(format!("&mut {}", var));
            } else if t.starts_with('&') {
                args.push(format!("&{}", var));
            } else {
                args.push(var.clone());
            }
            if t.starts_with('&') {
                cleanup_lines.push(format!("transfer::public_transfer({}, SUPER_USER);", var));
            }
            continue;
        }
        if t.starts_with('&') {
            let inner_ty = normalize_param_object_type(t);
            let var = format!("guard_factory_ref_{}_{}", idx, sanitize_ident(&p.name));
            if is_known_key_struct(&inner_ty, &factory.module_name, key_structs_by_module) {
                let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                prep_lines.push(format!(
                    "let {}{} = scenario.take_shared<{}>();",
                    maybe_mut,
                    var,
                    qualify_type_for_module(&factory.module_name, &inner_ty)
                ));
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", var));
                } else {
                    args.push(format!("&{}", var));
                }
                cleanup_lines.push(format!("test_scenario::return_shared({});", var));
            } else {
                let expr = synthesize_value_expr_for_type(
                    &inner_ty,
                    &factory.module_name,
                    fn_lookup,
                    Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                    0,
                )?;
                let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                prep_lines.push(format!("let {}{} = {};", maybe_mut, var, expr));
                if t.starts_with("&mut") {
                    args.push(format!("&mut {}", var));
                } else {
                    args.push(format!("&{}", var));
                }
                if should_transfer_call_return_with_keys(
                    &inner_ty,
                    &factory.module_name,
                    key_structs_by_module,
                ) {
                    cleanup_lines.push(cleanup_stmt_for_type(
                        &factory.module_name,
                        &inner_ty,
                        &var,
                        fn_lookup,
                        key_structs_by_module,
                        store_structs_by_module,
                    )?);
                }
            }
            continue;
        }

        if is_vector_type(t) {
            args.push(vector_literal_expr_for_type(t)?);
        } else if t == "u64" {
            args.push(default_u64_guard_arg_for_param(&p.name));
        } else if is_numeric_type(t) {
            args.push("1".to_string());
        } else if t == "bool" {
            args.push("false".to_string());
        } else if t == "address" {
            args.push("SUPER_USER".to_string());
        } else if is_id_type(t) {
            args.push("sui::object::id_from_address(OTHER)".to_string());
        } else if is_option_type(t) {
            args.push(option_none_expr_for_type(t));
        } else if is_string_type(t) {
            args.push("std::string::utf8(b\"tidewalker\")".to_string());
        } else {
            args.push(synthesize_value_expr_for_type(
                t,
                &factory.module_name,
                fn_lookup,
                Some(&format!("{}::{}", factory.module_name, factory.fn_name)),
                0,
            )?);
        }
    }
    Some(GuardFactoryArgPlan {
        args,
        prep_lines,
        cleanup_lines,
    })
}

fn pick_guard_factory_call_for_type(
    d: &FnDecl,
    type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> Option<GuardFactoryCallPlan> {
    let mut best: Option<(usize, GuardFactoryCallPlan)> = None;
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
        let Some(arg_plan) = synthesize_guard_factory_args_with_refs(
            f,
            fn_lookup,
            key_structs_by_module,
            store_structs_by_module,
        )
        else {
            continue;
        };
        let type_args = if has_unbound_type_params(f) {
            default_type_args_for_params(&f.type_params)
        } else {
            Vec::new()
        };
        let plan = GuardFactoryCallPlan {
            fn_name: f.fn_name.clone(),
            type_args,
            args: arg_plan.args,
            prep_lines: arg_plan.prep_lines,
            cleanup_lines: arg_plan.cleanup_lines,
        };
        let score = f.params.len();
        let candidate = (score, plan);
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some(candidate);
        }
    }
    best.map(|(_, plan)| plan)
}

fn pick_guard_shared_creator_plan(
    d: &FnDecl,
    module_fns: &std::collections::HashMap<String, FnDecl>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    _required_cap_type_keys: &std::collections::BTreeSet<String>,
    required_shared_type_keys: &std::collections::BTreeSet<String>,
) -> Option<GuardSharedCreatorPlan> {
    let mut candidates = module_fns
        .values()
        .filter(|f| {
            !f.is_test_only
                && (f.is_public || f.is_entry)
                && f.fn_name != d.fn_name
                && f.params.iter().any(|p| p.ty.contains("TxContext"))
                && fn_body_shares_object(&f.body_lines)
        })
        .collect::<Vec<_>>();
    candidates.sort_by_key(|f| f.params.len());
    let mut best: Option<(usize, GuardSharedCreatorPlan)> = None;
    for f in candidates {
        let shared_type_keys =
            infer_shared_type_keys_from_fn_body(&f.body_lines, &d.module_name, key_structs_by_module);
        if !required_shared_type_keys.is_empty()
            && !required_shared_type_keys
                .iter()
                .all(|k| shared_type_keys.contains(k))
        {
            continue;
        }
        if let Some(ret_ty) = f.return_ty.as_ref() {
            if !should_transfer_call_return_with_keys(ret_ty, &d.module_name, key_structs_by_module)
                && shared_type_keys.is_empty()
            {
                continue;
            }
        } else if shared_type_keys.is_empty() {
            continue;
        }
        let Some(arg_plan) = synthesize_guard_factory_args_with_refs(
            f,
            fn_lookup,
            key_structs_by_module,
            store_structs_by_module,
        )
        else {
            continue;
        };
        let type_args = default_type_args_for_decl(f);
        let provided_cap_type_keys =
            infer_creator_provided_cap_type_keys(d, f, &type_args, module_fns);
        let plan = GuardSharedCreatorPlan {
            fn_name: f.fn_name.clone(),
            args: arg_plan.args,
            type_args,
            return_ty: f.return_ty.clone(),
            shared_type_keys,
            provided_cap_type_keys,
            prep_lines: arg_plan.prep_lines,
            cleanup_lines: arg_plan.cleanup_lines,
        };
        let score = f.params.len();
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some((score, plan));
        }
    }
    best.map(|(_, plan)| plan)
}

fn pick_guard_owned_test_helper_for_type(
    d: &FnDecl,
    type_key: &str,
    module_fns: &std::collections::HashMap<String, FnDecl>,
    fn_lookup: &std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
) -> Option<GuardHelperCallPlan> {
    let wanted = normalize_helper_key(type_key);
    let mut best: Option<(usize, GuardHelperCallPlan)> = None;
    for f in module_fns.values() {
        if !f.is_test_only || f.fn_name == d.fn_name || f.fn_name == "init" {
            continue;
        }
        if !(f.fn_name.starts_with("create_") && f.fn_name.ends_with("_for_testing")) {
            continue;
        }
        if f.fn_name.starts_with("create_and_share_") {
            continue;
        }
        let helper_name_match = f
            .fn_name
            .strip_prefix("create_")
            .and_then(|x| x.strip_suffix("_for_testing"))
            .map(|x| normalize_helper_key(x) == wanted)
            .unwrap_or(false);
        let ret_type_match = f
            .return_ty
            .as_ref()
            .map(|r| type_key_from_type_name(r) == type_key)
            .unwrap_or(false);
        if !helper_name_match && !ret_type_match {
            continue;
        }
        let Some(args) = synthesize_guard_factory_args(f, fn_lookup) else {
            continue;
        };
        let type_args = if has_unbound_type_params(f) {
            default_type_args_for_params(&f.type_params)
        } else {
            Vec::new()
        };
        let plan = GuardHelperCallPlan {
            fn_name: f.fn_name.clone(),
            args,
            type_args,
        };
        let score = f.params.len();
        if best.as_ref().map(|b| score < b.0).unwrap_or(true) {
            best = Some((score, plan));
        }
    }
    best.map(|(_, plan)| plan)
}

fn build_common_call_plan(d: &FnDecl, env: &GuardEnv<'_>) -> Option<GuardCallPlan> {
    let mut lines_before_tx = Vec::new();
    let mut call_args = Vec::new();
    let mut cleanup = Vec::new();

    let mut param_runtime: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_arg_values: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();

    let mut object_needs: Vec<ObjectNeed> = Vec::new();
    let mut coin_needs: Vec<GuardCoinNeed> = Vec::new();
    let mut clock_cleanup_vars: Vec<String> = Vec::new();
    let default_type_args = default_type_args_for_params(&d.type_params);
    let empty_helpers = ModuleHelperCatalog::default();
    let module_helpers = env
        .helper_catalog
        .get(&d.module_name)
        .unwrap_or(&empty_helpers);
    let empty_bootstrap = ModuleBootstrapCatalog::default();
    let module_bootstrap = env
        .bootstrap_catalog
        .get(&d.module_name)
        .unwrap_or(&empty_bootstrap);
    let module_fns = env.fn_lookup.get(&d.module_name);
    let module_fn_fq = module_fn_label(d);

    for param in &d.params {
        let resolved_ty = concretize_type_params(&param.ty, &d.type_params, &default_type_args);
        let t = resolved_ty.trim();
        if t.contains("TxContext") {
            call_args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_clock_type(t) {
            let var_name = format!("clock_{}", sanitize_ident(&param.name));
            if t.starts_with("&mut") {
                lines_before_tx.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var_name
                ));
                call_args.push(format!("&mut {}", var_name));
                clock_cleanup_vars.push(var_name);
            } else if t.starts_with('&') {
                lines_before_tx.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var_name
                ));
                call_args.push(format!("&{}", var_name));
                clock_cleanup_vars.push(var_name);
            } else {
                lines_before_tx.push(format!(
                    "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var_name
                ));
                call_args.push(var_name);
            }
        } else if t == "address" {
            call_args.push("OTHER".to_string());
            param_arg_values.insert(param.name.clone(), "OTHER".to_string());
        } else if t == "u64" {
            let v = default_u64_guard_arg_for_param(&param.name);
            call_args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
        } else if is_numeric_type(t) {
            call_args.push("1".to_string());
            param_arg_values.insert(param.name.clone(), "1".to_string());
        } else if t == "bool" {
            call_args.push("false".to_string());
            param_arg_values.insert(param.name.clone(), "false".to_string());
        } else if is_id_type(t) {
            let v = default_id_arg_expr();
            call_args.push(v.clone());
            param_arg_values.insert(param.name.clone(), v);
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
            let coin_ty = coin_type_tag_from_coin_type_with_aliases(t, &d.module_use_aliases);
            if !is_qualified_type_tag(&coin_ty) {
                return None;
            }
            call_args.push(arg);
            coin_needs.push(GuardCoinNeed {
                var_name,
                coin_type: coin_ty,
                moved_on_call: !is_ref,
                needs_mut_binding: t.starts_with("&mut"),
            });
        } else {
            let should_try_constructor = {
                let norm = normalize_param_object_type(t);
                let type_key = type_key_from_type_name(&norm);
                let has_local_factory_for_type = module_fns
                    .map(|fns| {
                        fns.values().any(|f| {
                            f.return_ty
                                .as_ref()
                                .map(|r| type_key_from_type_name(r) == type_key)
                                .unwrap_or(false)
                        })
                    })
                    .unwrap_or(false);
                let avoid_constructor = is_cap_type(&norm)
                    || is_treasury_cap_type(&norm)
                    || module_helpers.shared_types.contains(&type_key)
                    || module_helpers.owned_types.contains(&type_key)
                    || module_bootstrap.init_shared_types.contains(&type_key)
                    || module_bootstrap.init_owned_types.contains(&type_key)
                    || has_local_factory_for_type;
                !avoid_constructor
            };
            if should_try_constructor {
                let expr = synthesize_value_expr_for_type(
                    t,
                    &d.module_name,
                    env.fn_lookup,
                    Some(&module_fn_fq),
                    0,
                )?;
                if t.starts_with('&') {
                    let var_name = format!("value_{}", sanitize_ident(&param.name));
                    let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                    lines_before_tx.push(format!("let {}{} = {};", maybe_mut, var_name, expr));
                    if t.starts_with("&mut") {
                        call_args.push(format!("&mut {}", var_name));
                    } else {
                        call_args.push(format!("&{}", var_name));
                    }
                } else {
                    call_args.push(expr);
                }
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
                let obj = ObjectNeed::from_resolved(param, normalize_param_object_type(t));
                call_args.push(obj.var_name.clone());
                param_runtime.insert(param.name.clone(), obj.var_name.clone());
                object_needs.push(obj);
            }
        }
    }
    let moved_object_vars_on_call: std::collections::HashSet<String> = object_needs
        .iter()
        .filter(|o| !o.is_ref)
        .map(|o| o.var_name.clone())
        .collect();

    let mut shared_objects: Vec<ObjectNeed> = Vec::new();
    let mut owned_objects: Vec<ObjectNeed> = Vec::new();
    let mut object_sources: std::collections::HashMap<String, GuardObjectProvisionSource> =
        std::collections::HashMap::new();
    let mut factory_calls: std::collections::HashMap<String, GuardFactoryCallPlan> =
        std::collections::HashMap::new();
    let mut owned_helper_calls: std::collections::HashMap<String, GuardHelperCallPlan> =
        std::collections::HashMap::new();
    let mut shared_creator_plan: Option<GuardSharedCreatorPlan> = None;
    let mut creator_non_cap_type_keys: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    let required_creator_shared_type_keys: std::collections::BTreeSet<String> = object_needs
        .iter()
        .filter(|o| o.is_ref && !is_cap_type(&o.type_name))
        .map(|o| o.type_key.clone())
        .collect();
    let needs_shared_non_cap_object = object_needs
        .iter()
        .any(|o| o.is_ref && !is_cap_type(&o.type_name));
    for obj in &object_needs {
        let owned_helper_plan = module_fns.and_then(|fns| {
            pick_guard_owned_test_helper_for_type(d, &obj.type_key, fns, env.fn_lookup)
        });
        if obj.is_ref && module_helpers.shared_types.contains(&obj.type_key) {
            shared_objects.push(obj.clone());
            object_sources.insert(
                obj.var_name.clone(),
                GuardObjectProvisionSource::SharedHelper,
            );
        } else if let Some(plan) = owned_helper_plan {
            owned_objects.push(obj.clone());
            object_sources.insert(
                obj.var_name.clone(),
                GuardObjectProvisionSource::OwnedHelper,
            );
            owned_helper_calls.insert(obj.var_name.clone(), plan);
        } else if module_helpers.owned_types.contains(&obj.type_key) {
            owned_objects.push(obj.clone());
            object_sources.insert(
                obj.var_name.clone(),
                GuardObjectProvisionSource::OwnedHelper,
            );
        } else if obj.is_ref
            && module_bootstrap.one_time_witness_init
            && module_bootstrap.init_shared_types.contains(&obj.type_key)
        {
            shared_objects.push(obj.clone());
            object_sources.insert(obj.var_name.clone(), GuardObjectProvisionSource::SharedInit);
        } else if module_bootstrap.one_time_witness_init
            && module_bootstrap.init_owned_types.contains(&obj.type_key)
        {
            owned_objects.push(obj.clone());
            object_sources.insert(obj.var_name.clone(), GuardObjectProvisionSource::OwnedInit);
        } else if obj.is_ref && is_cap_type(&obj.type_name) && needs_shared_non_cap_object {
            if shared_creator_plan.is_none() {
                shared_creator_plan = module_fns.and_then(|fns| {
                    pick_guard_shared_creator_plan(
                        d,
                        fns,
                        env.fn_lookup,
                        env.key_structs_by_module,
                        env.store_structs_by_module,
                        &std::collections::BTreeSet::new(),
                        &required_creator_shared_type_keys,
                    )
                });
            }
            let creator_supports_cap = shared_creator_plan
                .as_ref()
                .map(|p| p.provided_cap_type_keys.contains(&obj.type_key))
                .unwrap_or(false);
            if creator_supports_cap {
                owned_objects.push(obj.clone());
                object_sources.insert(
                    obj.var_name.clone(),
                    GuardObjectProvisionSource::OwnedCreatorSender,
                );
            } else if let Some(factory_plan) = module_fns.and_then(|fns| {
                pick_guard_factory_call_for_type(
                    d,
                    &obj.type_key,
                    fns,
                    env.fn_lookup,
                    env.key_structs_by_module,
                    env.store_structs_by_module,
                )
            }) {
                owned_objects.push(obj.clone());
                object_sources.insert(
                    obj.var_name.clone(),
                    GuardObjectProvisionSource::OwnedFactory,
                );
                factory_calls.insert(obj.var_name.clone(), factory_plan);
            } else {
                return None;
            }
        } else if let Some(factory_plan) = module_fns.and_then(|fns| {
            pick_guard_factory_call_for_type(
                d,
                &obj.type_key,
                fns,
                env.fn_lookup,
                env.key_structs_by_module,
                env.store_structs_by_module,
            )
        })
        {
            owned_objects.push(obj.clone());
            object_sources.insert(
                obj.var_name.clone(),
                GuardObjectProvisionSource::OwnedFactory,
            );
            factory_calls.insert(obj.var_name.clone(), factory_plan);
        } else {
            if shared_creator_plan.is_none() {
                shared_creator_plan = module_fns.and_then(|fns| {
                    pick_guard_shared_creator_plan(
                        d,
                        fns,
                        env.fn_lookup,
                        env.key_structs_by_module,
                        env.store_structs_by_module,
                        &std::collections::BTreeSet::new(),
                        &required_creator_shared_type_keys,
                    )
                });
            }
            if obj.is_ref && shared_creator_plan.is_some() {
                if is_cap_type(&obj.type_name) {
                    let creator_supports_cap = shared_creator_plan
                        .as_ref()
                        .map(|p| p.provided_cap_type_keys.contains(&obj.type_key))
                        .unwrap_or(false);
                    if creator_supports_cap {
                        owned_objects.push(obj.clone());
                        object_sources.insert(
                            obj.var_name.clone(),
                            GuardObjectProvisionSource::OwnedCreatorSender,
                        );
                    } else {
                        return None;
                    }
                } else {
                    let creator_supports_type = shared_creator_plan
                        .as_ref()
                        .map(|p| p.shared_type_keys.contains(&obj.type_key))
                        .unwrap_or(false);
                    if !creator_supports_type {
                        return None;
                    }
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
    let init_bootstrap_helper_name = module_fns.and_then(pick_init_bootstrap_helper_name);
    if needs_init_bootstrap && init_bootstrap_helper_name.is_none() {
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
            for prep in &plan.prep_lines {
                lines_before_tx.push(format!("    {}", prep));
            }
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
            let call_expr = format!("{}({})", call_path, plan.args.join(", "));
            if let Some(ret_ty) = plan.return_ty.as_ref() {
                if should_transfer_call_return_with_keys(
                    ret_ty,
                    &d.module_name,
                    env.key_structs_by_module,
                ) {
                    lines_before_tx.push(format!("    let seed_ret_obj = {};", call_expr));
                    let seed_cleanup = cleanup_stmt_for_type(
                        &d.module_name,
                        ret_ty,
                        "seed_ret_obj",
                        env.fn_lookup,
                        env.key_structs_by_module,
                        env.store_structs_by_module,
                    )?;
                    lines_before_tx.push(format!("    {}", seed_cleanup));
                } else {
                    lines_before_tx.push(format!("    {};", call_expr));
                }
            } else {
                lines_before_tx.push(format!("    {};", call_expr));
            }
            for cleanup in &plan.cleanup_lines {
                lines_before_tx.push(format!("    {}", cleanup));
            }
            lines_before_tx.push("};".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if needs_init_bootstrap {
        let bootstrap_name = init_bootstrap_helper_name?;
        lines_before_tx.push("{".to_string());
        lines_before_tx.push(format!(
            "    {}::{}(test_scenario::ctx(&mut scenario));",
            d.module_name, bootstrap_name
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
            "let {}{} = coin::mint_for_testing<{}>(1_000_000_000_000, test_scenario::ctx(&mut scenario));",
            maybe_mut, coin.var_name, coin.coin_type
        ));
    }
    for obj in &owned_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        match object_sources.get(&obj.var_name) {
            Some(GuardObjectProvisionSource::OwnedHelper) => {
                if let Some(plan) = owned_helper_calls.get(&obj.var_name) {
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
                    lines_before_tx.push(format!(
                        "let {}{} = {}({});",
                        maybe_mut,
                        obj.var_name,
                        call_path,
                        plan.args.join(", ")
                    ));
                } else {
                    lines_before_tx.push(format!(
                        "let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
                        maybe_mut, obj.var_name, d.module_name, obj.type_key
                    ));
                }
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
                let plan = factory_calls.get(&obj.var_name)?;
                for prep in &plan.prep_lines {
                    lines_before_tx.push(prep.clone());
                }
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
                lines_before_tx.push(format!(
                    "let {}{} = {}({});",
                    maybe_mut,
                    obj.var_name,
                    call_path,
                    plan.args.join(", ")
                ));
                for cleanup_line in &plan.cleanup_lines {
                    lines_before_tx.push(cleanup_line.clone());
                }
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
        if moved_object_vars_on_call.contains(&obj.var_name) {
            continue;
        }
        let cleanup_stmt = cleanup_stmt_for_type(
            &d.module_name,
            &obj.type_name,
            &obj.var_name,
            env.fn_lookup,
            env.key_structs_by_module,
            env.store_structs_by_module,
        )?;
        cleanup.push(cleanup_stmt);
    }
    for coin in &coin_needs {
        if !coin.moved_on_call {
            cleanup.push(format!(
                "transfer::public_transfer({}, SUPER_USER);",
                coin.var_name
            ));
        }
    }
    let mut seen_clock_cleanup: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for var in &clock_cleanup_vars {
        if seen_clock_cleanup.insert(var.clone()) {
            cleanup.push(format!("sui::clock::destroy_for_testing({});", var));
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

