# Tidewalker

Tidewalker is a Rust CLI that bootstraps **Move (Sui)** tests from protocol code.

It does three things:
- runs static call analysis
- injects `#[test_only]` setup helpers where possible
- generates best-effort call tests with safe assertions and explicit fallback guidance

## Why it exists

Writing first-pass Move tests is repetitive: setup flows, shared object wiring, cap wiring, and basic state assertions.

Tidewalker automates this baseline so developers can focus on protocol-specific edge cases and invariants.

## Install

Prerequisites:
- `cargo` (Rust toolchain)
- `sui` CLI

Build:

```bash
cargo build --release
```

Run (from this repo):

```bash
cargo run -- <command> <path-to-move-package>
```

## Commands

### `run`

Run Move tests with trace logging:

```bash
cargo run -- run ../my_move_pkg
```

Static-only call analysis:

```bash
cargo run -- run ../my_move_pkg --static
```

### `generate-setup`

Inject setup helpers into protocol modules (best effort):

```bash
cargo run -- generate-setup ../my_move_pkg
```

### `generate`

Main flow. Runs setup injection first, then generates tests:

```bash
cargo run -- generate ../my_move_pkg
```

Output test file:
- `tests/tidewalker_generated_tests.move`

## Annotations

Tidewalker supports function preconditions/chaining via comment annotation:

```move
/// @tidewalker requires create_state, seed_liquidity
public fun do_action(...) { ... }
```

`generate` will execute required functions before the target function in generated tests when signatures can be synthesized safely.

## What Tidewalker asserts today

Safe subsets are auto-asserted; uncertain cases are downgraded to guidance comments.

- Numeric state transitions (`+`, `-`, `*`, `/`, `%`, direct set, changed-fallback)
- Vector length transitions for common ops (`push`, `pop`, `insert`, `remove`, `swap_remove`, safe `append`)
- Option presence transitions (`some`/`none`) when accessors can be resolved
- Coin flow subsets (`split`, `join`, safe `mint`/`burn`/transfer patterns)
- Treasury supply change subsets
- Ownership/cap lifecycle signals (safe subset)
- Container subsets (`table`, `vec_map`, `vec_set`, `bag`, `dynamic_field`)

For unsupported or uncertain paths, generated tests include:
- `potential_change`: fields/state likely to change
- `manual_asserts`: conditions Tidewalker detected but did not auto-verify

## Skips and diagnostics

When a function call cannot be synthesized safely, Tidewalker still emits useful diagnostics in the generated file:
- function skipped reason
- local potential state changes
- potential changes from called functions (bounded depth)
- local/manual assert conditions from code and calls

Read-only getter/view functions are intentionally omitted from this skipped diagnostics block.

## Architecture

Core files:
- `src/main.rs`: CLI entry (`run`, `generate-setup`, `generate`)
- `src/commands/run.rs`: trace-based test run logging
- `src/commands/setup.rs`: helper injection into Move sources
- `src/commands/generate.rs`: orchestration, synthesis, diagnostics

Generation internals:
- `src/commands/generate/parser.rs`: parsing and declaration extraction
- `src/commands/generate/expr_ast.rs`: expression normalization (AST-like)
- `src/commands/generate/chain.rs`: requires/call-chain handling
- `src/commands/generate/guards.rs`: assert/guard extraction
- `src/commands/generate/catalog.rs`: type/function cataloging
- `src/commands/generate/render/`: domain renderers (`numeric`, `vector`, `option`, `coin`, `ownership`, `container`, `summary`, `orchestrator`)

## Project docs

- Remaining roadmap: `docs/ROADMAP_REMAINING_WORK.md`
- Setup and generation strategy: `docs/SETUP_AND_TEST_GENERATION.md`

## Current boundaries

Tidewalker is designed for deterministic, safe automation first.

It is conservative for:
- complex control-flow-heavy mutations (`if/else` + loops + deep aliasing)
- advanced object/container variants beyond covered subsets
- business-logic-specific invariants requiring protocol domain context

In those cases it reports structured guidance instead of forcing unsafe assertions.
