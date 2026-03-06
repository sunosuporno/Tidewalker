pub(super) fn render_best_effort_test(
    d: &FnDecl,
    inputs: &RenderInputs<'_>,
) -> Option<Vec<String>> {
    let accessor_map = inputs.accessor_map;
    let option_accessor_map = inputs.option_accessor_map;
    let container_accessor_map = inputs.container_accessor_map;
    let bootstrap_catalog = inputs.bootstrap_catalog;
    let fn_lookup = inputs.fn_lookup;
    let key_structs_by_module = inputs.key_structs_by_module;
    let store_structs_by_module = inputs.store_structs_by_module;
    let numeric_effects = inputs.numeric_effects;
    let vector_effects = inputs.vector_effects;
    let coin_effects = inputs.coin_effects;
    let treasury_cap_effects = inputs.treasury_cap_effects;
    let coin_notes = inputs.coin_notes;
    let option_effects = inputs.option_effects;
    let string_effects = inputs.string_effects;
    let container_effects = inputs.container_effects;
    let deep_overflow_paths = inputs.deep_overflow_paths;
    if d.params
        .iter()
        .any(|p| p.name.to_ascii_lowercase().contains("public_inputs"))
    {
        return None;
    }
    if has_assert_on_mut_local(&d.body_lines, &d.params) {
        return None;
    }
    if requires_table_key_prestate(&d.body_lines) {
        return None;
    }
    let fq = format!("{}::{}", d.module_name, d.fn_name);
    let coin_mint_amount = per_call_coin_mint_amount(d, fn_lookup);
    let mut lines = Vec::new();
    lines.push("#[test]".to_string());
    lines.push(format!(
        "fun test_call_{}_{}() {{",
        d.module_name.split("::").last().unwrap_or("m"),
        d.fn_name
    ));
    lines.push("    let mut scenario = test_scenario::begin(SUPER_USER);".to_string());

    let mut args: Vec<String> = Vec::new();
    let mut param_runtime: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_coin_runtime: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut param_coin_is_ref: std::collections::HashMap<String, bool> =
        std::collections::HashMap::new();
    let mut param_arg_values: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut object_needs: Vec<ObjectNeed> = Vec::new();
    let mut coin_needs: Vec<CoinNeed> = Vec::new();
    let mut arg_setup_lines: Vec<String> = Vec::new();
    let mut arg_value_cleanup_lines: Vec<String> = Vec::new();
    let mut clock_cleanup_vars: Vec<String> = Vec::new();
    let mut deferred_id_params: Vec<(String, String)> = Vec::new();
    let mut unresolved_deferred_id_params: Vec<(String, String)> = Vec::new();
    let mut shared_chain_args: std::collections::HashMap<String, SharedChainArgSpec> =
        std::collections::HashMap::new();
    let default_type_args = default_type_args_for_params(&d.type_params);
    let prefer_advanced_clock = has_clock_ge_guard(&d.body_lines);

    for param in &d.params {
        let resolved_ty = concretize_type_params(&param.ty, &d.type_params, &default_type_args);
        let t = resolved_ty.trim();
        let reuse_key = chain_reuse_key(&param.name, t);
        if t.contains("TxContext") {
            args.push("test_scenario::ctx(&mut scenario)".to_string());
        } else if is_clock_type(t) {
            let var_name = format!("clock_{}", sanitize_ident(&param.name));
            if t.starts_with("&mut") {
                arg_setup_lines.push(format!(
                    "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                    var_name
                ));
                if prefer_advanced_clock {
                    arg_setup_lines.push(format!(
                        "sui::clock::set_for_testing(&mut {}, 1000000);",
                        var_name
                    ));
                }
                args.push(format!("&mut {}", var_name));
                clock_cleanup_vars.push(var_name);
            } else if t.starts_with('&') {
                if prefer_advanced_clock {
                    arg_setup_lines.push(format!(
                        "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                    arg_setup_lines.push(format!(
                        "sui::clock::set_for_testing(&mut {}, 1000000);",
                        var_name
                    ));
                } else {
                    arg_setup_lines.push(format!(
                        "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                }
                args.push(format!("&{}", var_name));
                clock_cleanup_vars.push(var_name);
            } else {
                if prefer_advanced_clock {
                    arg_setup_lines.push(format!(
                        "let mut {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                    arg_setup_lines.push(format!(
                        "sui::clock::set_for_testing(&mut {}, 1000000);",
                        var_name
                    ));
                } else {
                    arg_setup_lines.push(format!(
                        "let {} = sui::clock::create_for_testing(test_scenario::ctx(&mut scenario));",
                        var_name
                    ));
                }
                args.push(var_name);
            }
        } else if t == "address" {
            let v = "OTHER".to_string();
            args.push(v.clone());
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &v);
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "u64" {
            let v = choose_u64_arg_for_param_in_fn(&param.name, &d.body_lines);
            args.push(v.clone());
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &v);
            param_arg_values.insert(param.name.clone(), v);
        } else if is_numeric_type(t) {
            let v = "1".to_string();
            args.push(v.clone());
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &v);
            param_arg_values.insert(param.name.clone(), v);
        } else if t == "bool" {
            let v = "false".to_string();
            args.push(v.clone());
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &v);
            param_arg_values.insert(param.name.clone(), v);
        } else if is_id_type(t) {
            let placeholder = format!("__TW_ID_PARAM_{}__", sanitize_ident(&param.name));
            args.push(placeholder.clone());
            deferred_id_params.push((param.name.clone(), placeholder.clone()));
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &placeholder);
            param_arg_values.insert(param.name.clone(), placeholder);
        } else if is_option_type(t) {
            let expr = option_none_expr_for_type(t);
            args.push(expr.clone());
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &expr);
        } else if is_string_type(t) {
            let v = "std::string::utf8(b\"tidewalker\")".to_string();
            args.push(v.clone());
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &v);
        } else if is_vector_type(t) {
            let vec_expr = vector_literal_expr_for_type(t)?;
            record_shared_chain_arg_expr(&mut shared_chain_args, reuse_key.as_ref(), &vec_expr);
            if t.starts_with('&') {
                if let Some(key) = reuse_key.as_ref() {
                    args.push(materialize_existing_shared_chain_arg(
                        &mut shared_chain_args,
                        key,
                        t,
                        &mut arg_setup_lines,
                    )?);
                } else {
                    let var_name = format!("vec_{}", sanitize_ident(&param.name));
                    let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                    arg_setup_lines.push(format!("let {}{} = {};", maybe_mut, var_name, vec_expr));
                    if t.starts_with("&mut") {
                        args.push(format!("&mut {}", var_name));
                    } else {
                        args.push(format!("&{}", var_name));
                    }
                }
            } else {
                args.push(
                    reuse_key
                        .as_ref()
                        .and_then(|key| {
                            materialize_existing_shared_chain_arg(
                                &mut shared_chain_args,
                                key,
                                t,
                                &mut arg_setup_lines,
                            )
                        })
                        .unwrap_or(vec_expr),
                );
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
            args.push(arg);
            param_coin_runtime.insert(param.name.clone(), var_name.clone());
            param_coin_is_ref.insert(param.name.clone(), is_ref);
            coin_needs.push(CoinNeed {
                var_name,
                coin_type: coin_ty,
                moved_on_main_call: !is_ref,
                needs_mut_binding: t.starts_with("&mut"),
            });
        } else {
            let should_try_constructor = {
                let norm = normalize_param_object_type(t);
                let is_immut_ref = t.starts_with('&') && !t.starts_with("&mut");
                let is_key_struct = is_known_key_struct(&norm, &d.module_name, key_structs_by_module);
                let is_store_struct =
                    is_known_store_struct(&norm, &d.module_name, store_structs_by_module);
                let avoid_constructor = is_cap_type(&norm)
                    || is_treasury_cap_type(&norm)
                    || (is_key_struct && (!is_immut_ref || !is_store_struct));
                !avoid_constructor
            };
            if should_try_constructor {
                let expr =
                    synthesize_value_expr_for_type(t, &d.module_name, fn_lookup, Some(&fq), 0)?;
                if t.starts_with('&') {
                    let var_name = format!("value_{}", sanitize_ident(&param.name));
                    let maybe_mut = if t.starts_with("&mut") { "mut " } else { "" };
                    arg_setup_lines.push(format!("let {}{} = {};", maybe_mut, var_name, expr));
                    if t.starts_with("&mut") {
                        args.push(format!("&mut {}", var_name));
                    } else {
                        args.push(format!("&{}", var_name));
                        let norm = normalize_param_object_type(t);
                        if is_known_key_struct(&norm, &d.module_name, key_structs_by_module) {
                            if let Some(cleanup_stmt) = cleanup_stmt_for_type(
                                &d.module_name,
                                &norm,
                                &var_name,
                                fn_lookup,
                                key_structs_by_module,
                                store_structs_by_module,
                            ) {
                                arg_value_cleanup_lines.push(cleanup_stmt);
                            } else {
                                arg_value_cleanup_lines.push(format!(
                                    "transfer::transfer({}, SUPER_USER);",
                                    var_name
                                ));
                            }
                        }
                    }
                } else {
                    args.push(expr);
                }
            } else if t.starts_with('&') {
                let obj = ObjectNeed::from_resolved(param, normalize_param_object_type(t));
                if obj.is_mut {
                    args.push(format!("&mut {}", obj.var_name));
                } else {
                    args.push(format!("&{}", obj.var_name));
                }
                param_runtime.insert(param.name.clone(), obj.var_name.clone());
                object_needs.push(obj);
            } else {
                let obj = ObjectNeed::from_resolved(param, normalize_param_object_type(t));
                args.push(obj.var_name.clone());
                param_runtime.insert(param.name.clone(), obj.var_name.clone());
                object_needs.push(obj);
            }
        }
    }
    let moved_object_vars_on_main_call: std::collections::HashSet<String> = object_needs
        .iter()
        .filter(|o| !o.is_ref)
        .map(|o| o.var_name.clone())
        .collect();

    let mut shared_objects: Vec<ObjectNeed> = Vec::new();
    let mut owned_objects: Vec<ObjectNeed> = Vec::new();
    let mut object_sources: std::collections::HashMap<String, ObjectProvisionSource> =
        std::collections::HashMap::new();
    let mut owned_helper_calls: std::collections::HashMap<String, HelperCallPlan> =
        std::collections::HashMap::new();
    let mut shared_helper_calls: std::collections::HashMap<String, HelperCallPlan> =
        std::collections::HashMap::new();
    let mut factory_calls: std::collections::HashMap<String, FactoryCallPlan> =
        std::collections::HashMap::new();
    let mut shared_creator_plan: Option<SharedCreatorPlan> = None;
    let mut creator_non_cap_type_keys: std::collections::BTreeSet<String> =
        std::collections::BTreeSet::new();
    if !object_needs.is_empty() {
            let empty_bootstrap = ModuleBootstrapCatalog::default();
        let module_fns = fn_lookup.get(&d.module_name);
        let mut idx = 0usize;
        while idx < object_needs.len() {
            let obj = object_needs[idx].clone();
            let (provider_module, _) =
                split_type_module_and_base(&obj.type_name, &d.module_name);
            let provider_fns = fn_lookup.get(provider_module);
            let provider_bootstrap = bootstrap_catalog
                .get(provider_module)
                .unwrap_or(&empty_bootstrap);
            let needs_shared_non_cap_object = object_needs
                .iter()
                .any(|o| o.is_ref && !is_cap_type(&o.type_name));
            let required_creator_shared_type_keys: std::collections::BTreeSet<String> = object_needs
                .iter()
                .filter(|o| o.is_ref && !is_cap_type(&o.type_name))
                .map(|o| o.type_key.clone())
                .collect();
            let shared_helper_plan = provider_fns.and_then(|fns| {
                if obj.is_ref {
                    pick_shared_test_helper_for_type(
                        d,
                        &obj.type_key,
                        fns,
                        fn_lookup,
                        provider_module,
                        coin_mint_amount,
                    )
                } else {
                    None
                }
            });
            let owned_helper_plan = provider_fns.and_then(|fns| {
                pick_owned_test_helper_for_type(
                    d,
                    &obj.type_key,
                    fns,
                    fn_lookup,
                    provider_module,
                    coin_mint_amount,
                )
            });

            if let Some(plan) = shared_helper_plan {
                shared_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::SharedHelper);
                shared_helper_calls.insert(obj.var_name.clone(), plan);
            } else if let Some(plan) = owned_helper_plan {
                owned_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedHelper);
                owned_helper_calls.insert(obj.var_name.clone(), plan);
            } else if obj.is_ref && provider_bootstrap.init_shared_types.contains(&obj.type_key)
            {
                shared_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::SharedInit);
            } else if provider_bootstrap.init_owned_types.contains(&obj.type_key)
            {
                owned_objects.push(obj.clone());
                object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedInit);
            } else {
                let shared_helper = format!("create_and_share_{}_for_testing", obj.type_key);
                let owned_helper = format!("create_{}_for_testing", obj.type_key);
                let has_shared_helper = provider_fns
                    .map(|fns| fns.contains_key(&shared_helper))
                    .unwrap_or(false);
                let has_owned_helper = provider_fns
                    .map(|fns| fns.contains_key(&owned_helper))
                    .unwrap_or(false);
                if obj.is_ref && has_shared_helper {
                    shared_objects.push(obj.clone());
                    object_sources
                        .insert(obj.var_name.clone(), ObjectProvisionSource::SharedHelper);
                } else if has_owned_helper {
                    owned_objects.push(obj.clone());
                    object_sources.insert(obj.var_name.clone(), ObjectProvisionSource::OwnedHelper);
            } else if obj.is_ref && is_cap_type(&obj.type_name) && needs_shared_non_cap_object {
                if shared_creator_plan.is_none() {
                    shared_creator_plan = module_fns.and_then(|fns| {
                        pick_shared_creator_plan(
                            d,
                            fns,
                            fn_lookup,
                            key_structs_by_module,
                            store_structs_by_module,
                            &std::collections::BTreeSet::new(),
                            &required_creator_shared_type_keys,
                            coin_mint_amount,
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
                        ObjectProvisionSource::OwnedCreatorSender,
                    );
                } else if let Some(plan) = provider_fns.and_then(|fns| {
                    pick_factory_call_for_type(
                        d,
                        &obj.type_key,
                        fns,
                        fn_lookup,
                        key_structs_by_module,
                        coin_mint_amount,
                    )
                }) {
                    for (dep_ty, dep_mut) in &plan.shared_ref_deps {
                        upsert_object_need(&mut object_needs, dep_ty.clone(), *dep_mut, true);
                    }
                    owned_objects.push(obj.clone());
                    object_sources.insert(
                        obj.var_name.clone(),
                        match plan.provision_mode {
                            OwnedFactoryProvision::ReturnValue => ObjectProvisionSource::OwnedFactory,
                            OwnedFactoryProvision::SenderTransfer => {
                                ObjectProvisionSource::OwnedCreatorSender
                            }
                        },
                    );
                    factory_calls.insert(obj.var_name.clone(), plan);
                } else {
                    return None;
                }
            } else if let Some(plan) = provider_fns.and_then(|fns| {
                    pick_factory_call_for_type(
                        d,
                        &obj.type_key,
                        fns,
                        fn_lookup,
                        key_structs_by_module,
                        coin_mint_amount,
                    )
                }) {
                    for (dep_ty, dep_mut) in &plan.shared_ref_deps {
                        upsert_object_need(&mut object_needs, dep_ty.clone(), *dep_mut, true);
                    }
                    owned_objects.push(obj.clone());
                    object_sources.insert(
                        obj.var_name.clone(),
                        match plan.provision_mode {
                            OwnedFactoryProvision::ReturnValue => ObjectProvisionSource::OwnedFactory,
                            OwnedFactoryProvision::SenderTransfer => {
                                ObjectProvisionSource::OwnedCreatorSender
                            }
                        },
                    );
                    factory_calls.insert(obj.var_name.clone(), plan);
                } else {
                    if shared_creator_plan.is_none() {
                        shared_creator_plan = module_fns.and_then(|fns| {
                            pick_shared_creator_plan(
                                d,
                                fns,
                                fn_lookup,
                                key_structs_by_module,
                                store_structs_by_module,
                                &std::collections::BTreeSet::new(),
                                &required_creator_shared_type_keys,
                                coin_mint_amount,
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
                                    ObjectProvisionSource::OwnedCreatorSender,
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
                                // A single create_shared_* bootstrap can safely seed only one
                                // concrete shared object type in first-pass generation.
                                return None;
                            }
                            shared_objects.push(obj.clone());
                            object_sources
                                .insert(obj.var_name.clone(), ObjectProvisionSource::SharedCreator);
                        }
                    } else {
                        return None;
                    }
                }
            }
            idx += 1;
        }
    }
    let mut init_bootstrap_modules: Vec<String> = Vec::new();
    for obj in &object_needs {
        let is_init_sourced = matches!(
            object_sources.get(&obj.var_name),
            Some(ObjectProvisionSource::SharedInit | ObjectProvisionSource::OwnedInit)
        );
        if !is_init_sourced {
            continue;
        }
        let (provider_module, _) = split_type_module_and_base(&obj.type_name, &d.module_name);
        if !init_bootstrap_modules.iter().any(|m| m == provider_module) {
            init_bootstrap_modules.push(provider_module.to_string());
        }
    }
    let mut init_bootstrap_helper_by_module: std::collections::HashMap<String, &'static str> =
        std::collections::HashMap::new();
    for module in &init_bootstrap_modules {
        let helper_name = fn_lookup.get(module).and_then(pick_init_bootstrap_helper_name)?;
        init_bootstrap_helper_by_module.insert(module.clone(), helper_name);
    }

    let has_shared_objects = !shared_objects.is_empty();
    let preexisting_shared_type_keys: std::collections::HashSet<String> =
        shared_objects.iter().map(|o| o.type_key.clone()).collect();

    let mut needs_next_tx_before_use = false;
    if has_shared_objects {
        let helper_shared = shared_objects
            .iter()
            .filter(|obj| {
                matches!(
                    object_sources.get(&obj.var_name),
                    Some(ObjectProvisionSource::SharedHelper)
                )
            })
            .collect::<Vec<_>>();
        if !helper_shared.is_empty() {
            lines.push("    {".to_string());
            for obj in helper_shared {
                let plan = shared_helper_calls.get(&obj.var_name)?;
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
            }
            lines.push("    };".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if object_sources.values().any(|src| {
        matches!(
            src,
            ObjectProvisionSource::SharedCreator | ObjectProvisionSource::OwnedCreatorSender
        )
    }) {
        if let Some(plan) = &shared_creator_plan {
            lines.push("    {".to_string());
            for prep in &plan.prep_lines {
                lines.push(format!("        {}", prep));
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
                if let Some(tuple_types) = parse_top_level_tuple_types(ret_ty) {
                    let vars = tuple_types
                        .iter()
                        .enumerate()
                        .map(|(i, _)| format!("seed_ret_obj_{}", i))
                        .collect::<Vec<_>>();
                    lines.push(format!(
                        "        let ({}) = {};",
                        vars.join(", "),
                        call_expr
                    ));
                    for (i, member_ty) in tuple_types.iter().enumerate() {
                        if should_transfer_call_return_with_keys(
                            member_ty,
                            &d.module_name,
                            key_structs_by_module,
                        ) {
                            let seed_cleanup = cleanup_stmt_for_type(
                                &d.module_name,
                                member_ty,
                                &format!("seed_ret_obj_{}", i),
                                fn_lookup,
                                key_structs_by_module,
                                store_structs_by_module,
                            )?;
                            lines.push(format!("        {}", seed_cleanup));
                        }
                    }
                } else if should_transfer_call_return_with_keys(
                    ret_ty,
                    &d.module_name,
                    key_structs_by_module,
                ) {
                    lines.push(format!("        let seed_ret_obj = {};", call_expr));
                    let seed_cleanup = cleanup_stmt_for_type(
                        &d.module_name,
                        ret_ty,
                        "seed_ret_obj",
                        fn_lookup,
                        key_structs_by_module,
                        store_structs_by_module,
                    )?;
                    lines.push(format!("        {}", seed_cleanup));
                } else {
                    lines.push(format!("        {};", call_expr));
                }
            } else {
                lines.push(format!("        {};", call_expr));
            }
            for cleanup in &plan.cleanup_lines {
                lines.push(format!("        {}", cleanup));
            }
            lines.push("    };".to_string());
            needs_next_tx_before_use = true;
        }
    }
    if !init_bootstrap_modules.is_empty() {
        lines.push("    {".to_string());
        for module in &init_bootstrap_modules {
            let bootstrap_name = init_bootstrap_helper_by_module.get(module)?;
            lines.push(format!(
                "        {}::{}(test_scenario::ctx(&mut scenario));",
                module, bootstrap_name
            ));
        }
        lines.push("    };".to_string());
        needs_next_tx_before_use = true;
    }
    let mut sender_seeded_owned_vars: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for obj in &owned_objects {
        if !matches!(
            object_sources.get(&obj.var_name),
            Some(ObjectProvisionSource::OwnedCreatorSender)
        ) {
            continue;
        }
        let Some(plan) = factory_calls.get(&obj.var_name) else {
            continue;
        };
        if !matches!(plan.provision_mode, OwnedFactoryProvision::SenderTransfer) {
            continue;
        }
        // Sender-transfer producers are safest when executed in setup tx and consumed in the
        // next tx; avoid inline same-tx take_from_sender.
        if !plan.shared_ref_deps.is_empty() {
            continue;
        }
        lines.push("    {".to_string());
        for l in &plan.prep_lines {
            lines.push(format!("        {}", l));
        }
        let call_path = if plan.type_args.is_empty() {
            format!("{}::{}", plan.module_name, plan.fn_name)
        } else {
            format!(
                "{}::{}<{}>",
                plan.module_name,
                plan.fn_name,
                plan.type_args.join(", ")
            )
        };
        lines.push(format!("        {}({});", call_path, plan.args.join(", ")));
        for l in &plan.cleanup_lines {
            lines.push(format!("        {}", l));
        }
        lines.push("    };".to_string());
        sender_seeded_owned_vars.insert(obj.var_name.clone());
        needs_next_tx_before_use = true;
    }
    if needs_next_tx_before_use {
        lines.push("    test_scenario::next_tx(&mut scenario, SUPER_USER);".to_string());
    }

    let mut object_vars_by_type: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for obj in &object_needs {
        object_vars_by_type
            .entry(obj.type_key.clone())
            .or_insert_with(|| obj.var_name.clone());
    }
    if !deferred_id_params.is_empty() {
        let defer_id_resolution_to_requires = !d.requires.is_empty();
        let resolve_id_arg = |param_name: &str| -> Option<String> {
            if defer_id_resolution_to_requires && param_name.ends_with("_id") {
                return None;
            }
            let preferred = param_name.strip_suffix("_id").unwrap_or(param_name);
            if let Some(obj) = object_needs
                .iter()
                .find(|o| o.var_name.contains(preferred) || preferred.contains(&o.var_name))
            {
                return Some(format!("sui::object::id(&{})", obj.var_name));
            }
            if let Some(obj) = object_needs.first() {
                return Some(format!("sui::object::id(&{})", obj.var_name));
            }
            None
        };
        for (param_name, placeholder) in deferred_id_params.into_iter() {
            if let Some(resolved) = resolve_id_arg(&param_name) {
                for arg in &mut args {
                    if arg == &placeholder {
                        *arg = resolved.clone();
                    }
                }
                if let Some(v) = param_arg_values.get_mut(&param_name) {
                    *v = resolved;
                }
            } else {
                unresolved_deferred_id_params.push((param_name, placeholder));
            }
        }
    }
    let coin_vars_for_calls = coin_var_names(&coin_needs);
    let mut pre_coin_counter = 0usize;
    let mut requires_cleanup_coin_vars: Vec<String> = Vec::new();
    let mut requires_cleanup_clock_vars: Vec<String> = Vec::new();
    let mut requires_consumed_coin_vars: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut requires_extra_coin_slots: std::collections::HashMap<usize, String> =
        std::collections::HashMap::new();
    let mut requires_sender_obj_var_by_type_key: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut requires_sender_obj_ordered: Vec<(String, String)> = Vec::new();
    let mut requires_sender_obj_pending: Vec<(String, String)> = Vec::new();
    let mut requires_sender_obj_cleanup: Vec<String> = Vec::new();
    let mut requires_sender_obj_counter: usize = 0;
    let mut requires_chain: Vec<&FnDecl> = Vec::new();
    if let Some(module_fns) = fn_lookup.get(&d.module_name) {
        for req in &d.requires {
            if let Some(req_decl) = module_fns.get(req) {
                requires_chain.push(req_decl);
            }
        }
    }
    if !requires_chain.is_empty() {
        let mut requires_mut_type_keys: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for req_decl in &requires_chain {
            for p in &req_decl.params {
                let t = p.ty.trim();
                if !t.starts_with("&mut")
                    || t.contains("TxContext")
                    || is_clock_type(t)
                    || is_vector_type(t)
                    || is_coin_type(t)
                {
                    continue;
                }
                let key = type_key_from_type_name(&normalize_param_object_type(t));
                requires_mut_type_keys.insert(key);
            }
        }
        if !requires_mut_type_keys.is_empty() {
            for obj in &mut object_needs {
                if requires_mut_type_keys.contains(&obj.type_key) {
                    obj.is_mut = true;
                }
            }
            for obj in &mut shared_objects {
                if requires_mut_type_keys.contains(&obj.type_key) {
                    obj.is_mut = true;
                }
            }
            for obj in &mut owned_objects {
                if requires_mut_type_keys.contains(&obj.type_key) {
                    obj.is_mut = true;
                }
            }
        }
    }

    let (snapshot_before_lines, snapshot_after_lines, mut numeric_summary) =
        build_numeric_assertion_lines(
            d,
            numeric_effects,
            &param_runtime,
            &param_arg_values,
            accessor_map,
            deep_overflow_paths,
        );
    let (vector_before_lines, vector_after_lines, vector_summary) = build_vector_assertion_lines(
        d,
        vector_effects,
        &param_runtime,
        accessor_map,
        deep_overflow_paths,
    );
    let (coin_before_lines, coin_after_lines, coin_summary) = build_coin_assertion_lines(
        d,
        coin_effects,
        &param_coin_runtime,
        &param_coin_is_ref,
        &param_arg_values,
        deep_overflow_paths,
    );
    let (treasury_before_lines, treasury_after_lines, treasury_summary) =
        build_treasury_cap_assertion_lines(
            d,
            treasury_cap_effects,
            &param_runtime,
            &param_arg_values,
            deep_overflow_paths,
        );
    let (option_before_lines, option_after_lines, option_summary) = build_option_assertion_lines(
        d,
        option_effects,
        &param_runtime,
        option_accessor_map,
        deep_overflow_paths,
    );
    let (container_before_lines, container_after_lines, container_summary) =
        build_container_assertion_lines(
            d,
            container_effects,
            &param_runtime,
            &param_arg_values,
            container_accessor_map,
            deep_overflow_paths,
        );
    let (ownership_transfer_checks, ownership_share_checks, ownership_summary) =
        build_ownership_checks(d, &param_arg_values, &preexisting_shared_type_keys);
    let cap_auth_summary = build_cap_auth_summary(d);
    let string_summary = build_string_summary(string_effects, &param_runtime, deep_overflow_paths);
    for eff in string_effects {
        let operator_target = format!("operator {}.{}", eff.base_var, eff.field);
        numeric_summary.asserted.remove(&operator_target);
        numeric_summary.potential.remove(&operator_target);
    }
    let mut state_summary = StateChangeSummary::default();
    state_summary.merge(numeric_summary);
    state_summary.merge(vector_summary);
    state_summary.merge(coin_summary);
    state_summary.merge(treasury_summary);
    state_summary.merge(build_coin_note_summary(coin_notes));
    state_summary.merge(cap_auth_summary);
    state_summary.merge(ownership_summary);
    state_summary.merge(option_summary);
    state_summary.merge(container_summary);
    state_summary.merge(string_summary);
    state_summary.merge(build_deep_chain_summary(deep_overflow_paths));
    state_summary.merge(build_mut_param_fallback_summary(d, &state_summary));
    let summary_lines = render_state_change_summary_lines(&state_summary);

    lines.push("    {".to_string());
    for coin in &coin_needs {
        let maybe_mut = if coin.needs_mut_binding { "mut " } else { "" };
        lines.push(format!(
            "        let {}{} = coin::mint_for_testing<{}>({}, test_scenario::ctx(&mut scenario));",
            maybe_mut,
            coin.var_name,
            coin.coin_type,
            coin_mint_amount
        ));
    }
    for obj in &shared_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        let obj_ty = qualify_type_for_module(&d.module_name, &obj.type_name);
        lines.push(format!(
            "        let {}{} = scenario.take_shared<{}>();",
            maybe_mut, obj.var_name, obj_ty
        ));
    }
    for obj in &owned_objects {
        let maybe_mut = if obj.is_mut { "mut " } else { "" };
        match object_sources.get(&obj.var_name) {
            Some(ObjectProvisionSource::OwnedHelper) => {
                let plan = owned_helper_calls.get(&obj.var_name)?;
                let call_path = if plan.type_args.is_empty() {
                    format!("{}::{}", plan.module_name, plan.fn_name)
                } else {
                    format!(
                        "{}::{}<{}>",
                        plan.module_name,
                        plan.fn_name,
                        plan.type_args.join(", ")
                    )
                };
                lines.push(format!(
                    "        let {}{} = {}({});",
                    maybe_mut,
                    obj.var_name,
                    call_path,
                    plan.args.join(", ")
                ));
            }
            Some(ObjectProvisionSource::OwnedInit) => {
                lines.push(format!(
                    "        let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
                    maybe_mut,
                    obj.var_name,
                    qualify_type_for_module(&d.module_name, &obj.type_name)
                ));
            }
            Some(ObjectProvisionSource::OwnedFactory) => {
                let plan = factory_calls.get(&obj.var_name)?;
                for l in &plan.prep_lines {
                    lines.push(format!("        {}", l));
                }
                let mut resolved_args: Vec<String> = Vec::new();
                for arg in &plan.args {
                    let mut resolved = arg.clone();
                    for (dep_ty, _) in &plan.shared_ref_deps {
                        let dep_key = type_key_from_type_name(dep_ty);
                        let token =
                            format!("__TW_OBJ_BY_TYPE_{}__", sanitize_ident(&dep_key));
                        if resolved.contains(&token) {
                            let dep_var = object_vars_by_type.get(&dep_key)?;
                            resolved = resolved.replace(&token, dep_var);
                        }
                    }
                    resolved_args.push(resolved);
                }
                let call_path = if plan.type_args.is_empty() {
                    format!("{}::{}", plan.module_name, plan.fn_name)
                } else {
                    format!(
                        "{}::{}<{}>",
                        plan.module_name,
                        plan.fn_name,
                        plan.type_args.join(", ")
                    )
                };
                lines.push(format!(
                    "        let {}{} = {}({});",
                    maybe_mut,
                    obj.var_name,
                    call_path,
                    resolved_args.join(", ")
                ));
                for l in &plan.cleanup_lines {
                    lines.push(format!("        {}", l));
                }
            }
            Some(ObjectProvisionSource::OwnedCreatorSender) => {
                if !sender_seeded_owned_vars.contains(&obj.var_name) {
                    if let Some(plan) = factory_calls.get(&obj.var_name) {
                    let mut resolved_args: Vec<String> = Vec::new();
                    for arg in &plan.args {
                        let mut resolved = arg.clone();
                        for (dep_ty, _) in &plan.shared_ref_deps {
                            let dep_key = type_key_from_type_name(dep_ty);
                            let token = format!("__TW_OBJ_BY_TYPE_{}__", sanitize_ident(&dep_key));
                            if resolved.contains(&token) {
                                let dep_var = object_vars_by_type.get(&dep_key)?;
                                resolved = resolved.replace(&token, dep_var);
                            }
                        }
                        resolved_args.push(resolved);
                    }
                    for l in &plan.prep_lines {
                        lines.push(format!("        {}", l));
                    }
                    let call_path = if plan.type_args.is_empty() {
                        format!("{}::{}", plan.module_name, plan.fn_name)
                    } else {
                        format!(
                            "{}::{}<{}>",
                            plan.module_name,
                            plan.fn_name,
                            plan.type_args.join(", ")
                        )
                    };
                    lines.push(format!("        {}({});", call_path, resolved_args.join(", ")));
                    for l in &plan.cleanup_lines {
                        lines.push(format!("        {}", l));
                    }
                }
                }
                lines.push(format!(
                    "        let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
                    maybe_mut,
                    obj.var_name,
                    qualify_type_for_module(&d.module_name, &obj.type_name)
                ));
            }
            _ => {
                lines.push(format!(
                    "        let {}{} = {}::create_{}_for_testing(test_scenario::ctx(&mut scenario));",
                    maybe_mut, obj.var_name, d.module_name, obj.type_key
                ));
            }
        }
    }
    for l in &arg_setup_lines {
        lines.push(format!("        {}", l));
    }

    for (req_idx, req_decl) in requires_chain.iter().enumerate() {
        if let Some((req_args, pre_lines)) = synthesize_requires_call_args(
            req_decl,
            req_idx,
            &requires_chain,
            &object_vars_by_type,
            &mut shared_chain_args,
            fn_lookup,
            &coin_vars_for_calls,
            &mut requires_extra_coin_slots,
            &mut pre_coin_counter,
            &mut requires_cleanup_coin_vars,
            &mut requires_cleanup_clock_vars,
            &mut requires_consumed_coin_vars,
            coin_mint_amount,
        ) {
            for l in pre_lines {
                lines.push(format!("        {}", l));
            }
            let req_call_target = if has_unbound_type_params(req_decl) {
                let req_type_args = default_type_args_for_params(&req_decl.type_params);
                if req_type_args.is_empty() {
                    format!("{}::{}", d.module_name, req_decl.fn_name)
                } else {
                    format!(
                        "{}::{}<{}>",
                        d.module_name,
                        req_decl.fn_name,
                        req_type_args.join(", ")
                    )
                }
            } else {
                format!("{}::{}", d.module_name, req_decl.fn_name)
            };
            lines.push(format!(
                "        {}({});",
                req_call_target,
                req_args.join(", ")
            ));
            if let Some(module_fns) = fn_lookup.get(&req_decl.module_name) {
                let transferred_types = infer_transferred_object_types_for_fn(req_decl, module_fns);
                for ty in transferred_types {
                    let type_key = type_key_from_type_name(&ty);
                    let wants_type_for_unresolved_id = unresolved_deferred_id_params.iter().any(
                        |(param_name, _)| {
                            let preferred =
                                sanitize_ident(param_name.strip_suffix("_id").unwrap_or(param_name));
                            !preferred.is_empty()
                                && (type_key.contains(&preferred) || preferred.contains(&type_key))
                        },
                    );
                    if !wants_type_for_unresolved_id
                        || requires_sender_obj_var_by_type_key.contains_key(&type_key)
                    {
                        continue;
                    }
                    let var = format!("req_sender_obj_{}", requires_sender_obj_counter);
                    requires_sender_obj_counter += 1;
                    requires_sender_obj_var_by_type_key.insert(type_key.clone(), var.clone());
                    requires_sender_obj_ordered.push((type_key.clone(), var.clone()));
                    requires_sender_obj_pending.push((var.clone(), ty));
                    requires_sender_obj_cleanup.push(var);
                }
            }
        }
    }
    if !requires_sender_obj_pending.is_empty() {
        for obj in &shared_objects {
            lines.push(format!(
                "        test_scenario::return_shared({});",
                obj.var_name
            ));
        }
        for obj in &owned_objects {
            if moved_object_vars_on_main_call.contains(&obj.var_name) {
                continue;
            }
            let norm = normalize_param_object_type(&obj.type_name);
            if is_known_store_struct(&norm, &d.module_name, store_structs_by_module) {
                lines.push(format!(
                    "        transfer::public_transfer({}, SUPER_USER);",
                    obj.var_name
                ));
            } else {
                lines.push(format!(
                    "        transfer::transfer({}, SUPER_USER);",
                    obj.var_name
                ));
            }
        }
        lines.push("    };".to_string());
        lines.push("    test_scenario::next_tx(&mut scenario, SUPER_USER);".to_string());
        lines.push("    {".to_string());
        for obj in &shared_objects {
            let maybe_mut = if obj.is_mut { "mut " } else { "" };
            let obj_ty = qualify_type_for_module(&d.module_name, &obj.type_name);
            lines.push(format!(
                "        let {}{} = scenario.take_shared<{}>();",
                maybe_mut, obj.var_name, obj_ty
            ));
        }
        for obj in &owned_objects {
            if moved_object_vars_on_main_call.contains(&obj.var_name) {
                continue;
            }
            let maybe_mut = if obj.is_mut { "mut " } else { "" };
            lines.push(format!(
                "        let {}{} = test_scenario::take_from_sender<{}>(&scenario);",
                maybe_mut,
                obj.var_name,
                qualify_type_for_module(&d.module_name, &obj.type_name)
            ));
        }
        for (var, ty) in &requires_sender_obj_pending {
            lines.push(format!(
                "        let {} = test_scenario::take_from_sender<{}>(&scenario);",
                var,
                qualify_type_for_module(&d.module_name, ty)
            ));
        }
    }
    if !unresolved_deferred_id_params.is_empty() {
        for (param_name, placeholder) in unresolved_deferred_id_params {
            let preferred = sanitize_ident(param_name.strip_suffix("_id").unwrap_or(&param_name));
            let resolved = requires_sender_obj_ordered
                .iter()
                .rev()
                .find_map(|(type_key, var)| {
                    if !preferred.is_empty()
                        && (type_key.contains(&preferred) || preferred.contains(type_key))
                    {
                        Some(format!("sui::object::id(&{})", var))
                    } else {
                        None
                    }
                })
                .unwrap_or_else(default_id_arg_expr);
            for arg in &mut args {
                if arg == &placeholder {
                    *arg = resolved.clone();
                }
            }
            if let Some(v) = param_arg_values.get_mut(&param_name) {
                *v = resolved;
            }
        }
    }
    let auto_prestate_lines = synthesize_auto_prestate_calls(
        d,
        fn_lookup,
        &object_vars_by_type,
        &coin_vars_for_calls,
        &mut requires_extra_coin_slots,
        &mut pre_coin_counter,
        &mut requires_cleanup_coin_vars,
        &mut requires_cleanup_clock_vars,
        &mut requires_consumed_coin_vars,
        &param_arg_values,
    );
    for l in auto_prestate_lines {
        lines.push(format!("        {}", l));
    }
    for l in snapshot_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in vector_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in coin_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in treasury_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in option_before_lines {
        lines.push(format!("        {}", l));
    }
    for l in container_before_lines {
        lines.push(format!("        {}", l));
    }
    let call_target = if has_unbound_type_params(d) {
        if default_type_args.is_empty() {
            fq.clone()
        } else {
            format!("{}<{}>", fq, default_type_args.join(", "))
        }
    } else {
        fq.clone()
    };
    for l in rewrite_inline_object_id_args_for_call(d, &mut args) {
        lines.push(format!("        {}", l));
    }
    let call_expr = format!("{}({})", call_target, args.join(", "));
    if let Some(ret_ty) = d.return_ty.as_ref() {
        if let Some(tuple_types) = parse_top_level_tuple_types(ret_ty) {
            let vars = tuple_types
                .iter()
                .enumerate()
                .map(|(i, _)| format!("tw_ret_obj_{}", i))
                .collect::<Vec<_>>();
            lines.push(format!("        let ({}) = {};", vars.join(", "), call_expr));
            for (i, member_ty) in tuple_types.iter().enumerate() {
                if should_transfer_call_return_with_keys(
                    member_ty,
                    &d.module_name,
                    key_structs_by_module,
                ) {
                    let ret_cleanup = cleanup_stmt_for_type(
                        &d.module_name,
                        member_ty,
                        &format!("tw_ret_obj_{}", i),
                        fn_lookup,
                        key_structs_by_module,
                        store_structs_by_module,
                    )?;
                    lines.push(format!("        {}", ret_cleanup));
                }
            }
        } else if should_transfer_call_return_with_keys(ret_ty, &d.module_name, key_structs_by_module) {
            lines.push(format!("        let tw_ret_obj = {};", call_expr));
            let ret_cleanup = cleanup_stmt_for_type(
                &d.module_name,
                ret_ty,
                "tw_ret_obj",
                fn_lookup,
                key_structs_by_module,
                store_structs_by_module,
            )?;
            lines.push(format!("        {}", ret_cleanup));
        } else {
            lines.push(format!("        {};", call_expr));
        }
    } else {
        lines.push(format!("        {};", call_expr));
    }
    for l in snapshot_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in vector_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in coin_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in treasury_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in option_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in container_after_lines {
        lines.push(format!("        {}", l));
    }
    for l in summary_lines {
        lines.push(format!("        {}", l));
    }
    for cleanup in &arg_value_cleanup_lines {
        lines.push(format!("        {}", cleanup));
    }
    for obj in &shared_objects {
        lines.push(format!(
            "        test_scenario::return_shared({});",
            obj.var_name
        ));
    }
    for var in &requires_sender_obj_cleanup {
        lines.push(format!("        test_scenario::return_to_sender(&scenario, {});", var));
    }
    let mut seen_req_coin_cleanup: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for var in &requires_cleanup_coin_vars {
        if requires_consumed_coin_vars.contains(var) {
            continue;
        }
        if seen_req_coin_cleanup.insert(var.clone()) {
            lines.push(format!(
                "        transfer::public_transfer({}, SUPER_USER);",
                var
            ));
        }
    }
    let mut seen_req_clock_cleanup: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for var in &requires_cleanup_clock_vars {
        if seen_req_clock_cleanup.insert(var.clone()) {
            lines.push(format!("        sui::clock::destroy_for_testing({});", var));
        }
    }
    for obj in &owned_objects {
        if moved_object_vars_on_main_call.contains(&obj.var_name) {
            continue;
        }
        if is_coin_type(&obj.type_name)
            || is_treasury_cap_type(&obj.type_name)
            || is_known_key_struct(&obj.type_name, &d.module_name, key_structs_by_module)
        {
            let cleanup_stmt = cleanup_stmt_for_type(
                &d.module_name,
                &obj.type_name,
                &obj.var_name,
                fn_lookup,
                key_structs_by_module,
                store_structs_by_module,
            )?;
            lines.push(format!("        {}", cleanup_stmt));
        }
    }
    for coin in &coin_needs {
        if !coin.moved_on_main_call {
            lines.push(format!(
                "        transfer::public_transfer({}, SUPER_USER);",
                coin.var_name
            ));
        }
    }
    let mut seen_clock_cleanup: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for var in &clock_cleanup_vars {
        if seen_clock_cleanup.insert(var.clone()) {
            lines.push(format!("        sui::clock::destroy_for_testing({});", var));
        }
    }
    lines.push("    };".to_string());
    for (idx, check) in ownership_transfer_checks.iter().enumerate() {
        lines.push(format!(
            "    test_scenario::next_tx(&mut scenario, {});",
            check.recipient
        ));
        lines.push("    {".to_string());
        lines.push(format!(
            "        let moved_obj_{} = test_scenario::take_from_sender<{}>(&scenario);",
            idx, check.object_type
        ));
        lines.push(format!(
            "        test_scenario::return_to_sender(&scenario, moved_obj_{});",
            idx
        ));
        lines.push("    };".to_string());
    }
    for (idx, check) in ownership_share_checks.iter().enumerate() {
        lines.push("    test_scenario::next_tx(&mut scenario, SUPER_USER);".to_string());
        lines.push("    {".to_string());
        lines.push(format!(
            "        let shared_obj_{} = scenario.take_shared<{}>();",
            idx, check.object_type
        ));
        lines.push(format!(
            "        test_scenario::return_shared(shared_obj_{});",
            idx
        ));
        lines.push("    };".to_string());
    }

    // Ensure scenario is closed.
    lines.push("    test_scenario::end(scenario);".to_string());
    lines.push("}".to_string());
    Some(lines)
}
