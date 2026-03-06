use super::catalog::ModuleBootstrapCatalog;
use super::provision::{
    default_type_args_for_decl, fn_transfers_type_to_sender, infer_creator_provided_cap_type_keys,
    resolved_param_ty,
};
use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ObjectProvisionSource {
    SharedHelper,
    SharedInit,
    SharedCreator,
    OwnedHelper,
    OwnedInit,
    OwnedFactory,
    OwnedCreatorSender,
}

#[derive(Debug, Clone)]
struct SharedCreatorPlan {
    fn_name: String,
    args: Vec<String>,
    type_args: Vec<String>,
    return_ty: Option<String>,
    shared_type_keys: std::collections::HashSet<String>,
    provided_cap_type_keys: std::collections::HashSet<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
}

#[derive(Debug, Clone)]
struct HelperCallPlan {
    module_name: String,
    fn_name: String,
    args: Vec<String>,
    type_args: Vec<String>,
}

#[derive(Debug, Clone)]
struct FactoryArgPlan {
    args: Vec<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
}

#[derive(Debug, Clone)]
struct FactoryCallPlan {
    module_name: String,
    fn_name: String,
    provision_mode: OwnedFactoryProvision,
    type_args: Vec<String>,
    args: Vec<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
    shared_ref_deps: Vec<(String, bool)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OwnedFactoryProvision {
    ReturnValue,
    SenderTransfer,
}

#[derive(Debug, Clone)]
struct SharedChainArgSpec {
    expr: String,
    binding_var: Option<String>,
}


include!("orchestrator/helpers.rs");
include!("orchestrator/main_render.rs");
