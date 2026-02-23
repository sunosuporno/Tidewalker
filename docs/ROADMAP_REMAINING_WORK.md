# Tidewalker Roadmap: Remaining Work

Tidewalker is a Move (Sui) test bootstrap tool.
It generates baseline tests from protocol code, including state-change assertions where confidence is high.

## Current Coverage

1. Generates `#[test_only]` setup helpers for common shared-object and cap flows.
2. Generates best-effort tests for public and entry functions.
3. Tracks direct and chained state changes (bounded static depth).
4. Emits specific assertions when safe.
5. Emits `potential_change` comments when certainty is lower.

Implemented state categories today:

1. Numeric/operator state changes (`+`, `-`, `*`, `/`, `%`, direct set, generic changed).
2. Vector length-driven changes (`push`, `pop`, `insert`, `remove`, `swap_remove`, `append`).
3. Option presence changes (`some`, `none`, changed via accessor).
4. String mutation detection as `potential_change`.

## What's Left

### 1) Non-blockchain state types

1. Map/set-like containers:
   `table`, `object_table`, `vec_map`, `vec_set`, and equivalent helper APIs.
2. Struct-level/nested replacement flows:
   whole-struct rewrites and deeper field rewrite patterns.

### 2) Blockchain-specific state assertions

1. Treasury authority flows:
   `TreasuryCap<T>`-aware mint/burn permission checks and supply-control assertions.
2. Ownership transitions (owned/shared/wrapped/object movement).
3. Cap mint/transfer/burn lifecycle checks.
4. Object creation/destruction assertions by type and owner.

### 3) Function-level checks (assert/abort behavior)

1. Infer and generate expected abort tests from guards.
2. Generate success-path tests for assert preconditions.
3. Distinguish expected abort code vs unexpected failure.

### 4) Control-flow precision

1. Improve branch-aware reasoning (`if/else`).
2. Improve loop-aware reasoning.
3. Reduce false positives in mixed/complex mutation paths.

### 5) Confidence and DX

1. Add per-assert confidence tagging.
2. Improve summary readability for large functions.
3. Expand docs/examples against mature real-world protocols.

## Suggested Next Build Order

1. Map/set container support.
2. Coin/ownership/cap blockchain-state support.
3. Assert/abort expectation synthesis.
4. Control-flow precision improvements.
