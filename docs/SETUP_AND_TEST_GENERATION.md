# Tidewalker: Setup & Test Generation — Cases and Strategy

This document captures the cases we need to handle across two phases—**test_only helper generation** (setup) and **test generation**—and what we explicitly do not attempt.

---

## 1. Test-only helper generation phase (setup)

**Goal:** Generate `#[test_only]` helpers so tests can create shared objects and admin caps (replicate or wrap init/factory behavior). We inject these into the protocol source (defining module) or emit alerts when we cannot.

### 1.1 Cases we cover today

| Case | Detection | Generation | Notes |
|------|-----------|------------|--------|
| **Shared object in `init(ctx)`** | Scan `fun init(...)` body for `transfer::share_object(var)`; map var → type via `let var = TypeName { ... }`. | Generate constructor + `share_object` in defining module. | Simple init only (no versioned/nested we can’t replicate). |
| **Cap in `init(ctx)`** | Same init body; look for `transfer::transfer(var, ctx.sender()` / `tx_context::sender`). | Generate constructor and return (or transfer to sender) in defining module. | |
| **Public struct only** | We only generate for types that are `public struct` with `key`/`store`. | If type is used in init but not public → **alert** (cannot generate). | |

### 1.2 Cases to add in setup phase

| Case | Detection | Generation | Notes |
|------|-----------|------------|--------|
| **Shared/cap in non-init functions** | Scan **all** functions (not just init) for `share_object(...)` and `transfer(..., sender(...))`; record (type, kind, function_name). | Do **not** replicate construction (too many deps). **Alert:** “Type X is created/shared in function F. Add a #[test_only] helper that calls F with test parameters, or implement manually.” | Optional later: thin wrapper that calls F for very simple signatures. |
| **Init with one-time witness** | Detect `fun init(_: WITNESS_TYPE, ctx: &mut TxContext)` (first param is one-time witness). | Generate `#[test_only] bootstrap_init_for_testing(ctx)` in the defining module. It creates witness via `sui::test_utils::create_one_time_witness<WITNESS_TYPE>()` and calls `init(witness, ctx)`. | No manual helper needed for simple OTW init bootstrap. |
| **Better alerts** | Already detect. | When we can’t generate: “Init too complex to replicate (versioned/nested); add a #[test_only] helper manually or call existing test API.” | Clear messaging only. |

### 1.3 What we do not generate in setup phase (admit defeat)

| Case | Reason |
|------|--------|
| **Complex init** | Construction uses `versioned::create`, nested modules, bags, vec_set, etc. We cannot reliably replicate this in generated Move. Dev must add a #[test_only] helper by hand in the defining module (copy-paste/simplify init or call internal helpers). |
| **Private struct** | Even if we parse the struct’s fields and know its “structure,” we do **not** know the **constraints** for each field (valid ranges, invariants, dependencies on other modules). Generating a struct literal with placeholder values (e.g. 0, object::new(ctx)) may break invariants or fail at runtime. We therefore do not auto-generate constructors for private structs; the dev writes the helper in the defining module with correct constraints. |

---

## 2. Test generation phase (later)

**Goal:** Generate the actual test code (scenario, transactions, assertions) that uses the setup and exercises the protocol.

### 2.1 Cases we can tackle in test generation

| Case | How test generation handles it |
|------|-------------------------------|
| **Shared/cap in non-init (1A)** | Generate **test code** that performs a transaction calling the existing function (e.g. `pool::create_pool(registry, tick_size, lot_size, …)`) with test arguments. No separate test_only helper needed; the generated test is the “helper” that invokes the API. |
| **One-time witness init (2)** | Test generation bootstraps init state by calling generated `bootstrap_init_for_testing(ctx)` first, then uses `take_shared` / `take_from_sender` for init-produced objects in later transaction steps. |
| **`new()` / factory functions** | Types created and returned (owned) by public factory functions (including `new(...)`) need no test_only helper. Test generation now detects such factories and emits direct `module::factory(...)` object provisioning when a required object type has no helper. |

So: **1A** and **`new()`** are addressed by **how we generate tests**; **one-time witness init** is handled by an auto-generated setup helper (`bootstrap_init_for_testing`) plus generated test calls.

---

## 3. Summary: who does what

| Scenario | Setup phase (helpers) | Test generation phase |
|----------|------------------------|-------------------------|
| Simple shared/cap in `init`, public struct | ✅ Generate helper in defining module | Use generated helper in tests |
| Shared/cap in non-init function | ⚠️ Alert; no construction generated | ✅ Generate test that calls that function |
| Init with one-time witness | ✅ Auto-generate `bootstrap_init_for_testing` in defining module | ✅ Generate test that calls bootstrap helper, then takes init-produced objects |
| **`new()` / owned-object factories** | Not needed (no helper) | ✅ Generate test that calls `module::new(ctx)` etc. when test needs the object |
| Complex init | ❌ Admit defeat; alert | Dev writes helper; tests use it |
| Private struct | ❌ Admit defeat (structure known but not field constraints); alert | Dev writes helper; tests use it |

---

## 4. Reference: how the dev writes helpers when we don’t generate

- **Non-init (e.g. Pool, Referral):** Test calls the real function with test params, or dev adds a thin `#[test_only]` wrapper that calls it with fixed defaults.
- **One-time witness:** Test simulates publish and uses the resulting objects, or dev adds a `#[test_only]` function that takes the witness and duplicates init body (using framework’s test witness).
- **Complex init / private struct:** Dev adds a `#[test_only]` function in the **defining module** that constructs the value with correct constraints (copy-paste/simplify init or use internal helpers).

---

*Last updated from design discussion using the Deepbook codebase as reference.*
