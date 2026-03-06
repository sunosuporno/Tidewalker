use super::catalog::{ModuleBootstrapCatalog, ModuleHelperCatalog};
use super::expr_ast::{self, BinOp, Expr};
use super::provision::{
    default_type_args_for_decl, infer_creator_provided_cap_type_keys, resolved_param_ty,
};
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
    ObjectFieldSender {
        param_name: String,
        field_name: String,
        op: String,
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
    CoinValueObjectField {
        coin_param: String,
        object_param: String,
        field_name: String,
        op: String,
    },
    ParamClockTimestamp {
        param_name: String,
        clock_param: String,
        op: String,
    },
    ObjectFieldClockTimestamp {
        param_name: String,
        field_name: String,
        clock_param: String,
        op: String,
    },
    VectorLenConst {
        param_name: String,
        op: String,
        rhs_literal: String,
    },
    VectorLenParam {
        lhs_param: String,
        rhs_param: String,
        op: String,
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
    coin_type: String,
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
    return_ty: Option<String>,
    shared_type_keys: std::collections::HashSet<String>,
    provided_cap_type_keys: std::collections::HashSet<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
}

#[derive(Debug, Clone)]
struct GuardHelperCallPlan {
    fn_name: String,
    args: Vec<String>,
    type_args: Vec<String>,
}

#[derive(Debug, Clone)]
struct GuardFactoryArgPlan {
    args: Vec<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
}

#[derive(Debug, Clone)]
struct GuardFactoryCallPlan {
    fn_name: String,
    type_args: Vec<String>,
    args: Vec<String>,
    prep_lines: Vec<String>,
    cleanup_lines: Vec<String>,
}

type GuardCallPlan = (
    Vec<String>,
    Vec<String>,
    Vec<String>,
    std::collections::HashMap<String, String>,
    std::collections::HashMap<String, String>,
);

struct GuardEnv<'a> {
    helper_catalog: &'a std::collections::HashMap<String, ModuleHelperCatalog>,
    bootstrap_catalog: &'a std::collections::HashMap<String, ModuleBootstrapCatalog>,
    fn_lookup: &'a std::collections::HashMap<String, std::collections::HashMap<String, FnDecl>>,
    key_structs_by_module: &'a std::collections::HashMap<String, std::collections::HashSet<String>>,
    store_structs_by_module:
        &'a std::collections::HashMap<String, std::collections::HashSet<String>>,
}

include!("guards/detect.rs");
include!("guards/plan.rs");
include!("guards/render.rs");
