# Tidewalker Roadmap: Remaining Work

Tidewalker is a Move (Sui) test bootstrap tool.
It generates baseline tests from protocol code, including state-change assertions where confidence is high and `potential_change` fallbacks where confidence is lower.

## Current Coverage (Implemented)

1. Test generation and setup
   - Generates best-effort tests for public and entry functions.
   - Generates/uses `#[test_only]` helpers for shared/owned object setup in common cases.
   - Supports `@tidewalker requires ...` precondition chains.

2. Chained static analysis
   - Tracks direct and bounded chained state changes (depth-limited static pass).
   - Emits conservative deep-chain `potential_change` when certainty drops.

3. Non-blockchain state categories
   - Numeric/operator state assertions (`+`, `-`, `*`, `/`, `%`, direct set, changed fallback).
   - Vector length-state assertions (`push`, `pop`, `insert`, `remove`, `swap_remove`, `append` where safe).
   - Option presence assertions (`some`/`none`) through detected bool accessors.
   - String mutation detection (currently reported as `potential_change`).
   - Map/set-like containers (safe subset):
     - `table`
     - `vec_map`
     - `vec_set`
     - `bag`
     - `dynamic_field`
     - with assert strategy: add/insert/remove -> `contains/exists` post-check when safe.

4. Blockchain-state categories
   - Coin value assertions for safe `split/join/mint/burn/transfer` patterns.
   - TreasuryCap total-supply assertions for safe mint/burn patterns.
   - Cap auth/ownership summaries and ownership checks (transfer/share/destroy signals in safe subset).
   - Staking-like flow detection is surfaced as `potential_change` note.

## What's Left

### 1) Function-level checks (primary remaining milestone)

1. Infer expected abort tests from guards (`assert!`/abort-like flows).
2. Generate explicit success-path guard tests.
3. Verify expected abort code vs unexpected failures.

### 2) Control-flow precision

1. Better branch-aware reasoning (`if/else`) for effect certainty.
2. Better loop-aware reasoning.
3. Fewer conservative fallbacks in mixed mutation paths.

### 3) Remaining state-model gaps

1. Additional container/object variants not in current safe subset (for example `object_table` style flows).
2. More precise nested/whole-struct rewrite reasoning.
3. Expand alias/key resolution so every safe `vec_map` remove-style flow can be asserted (currently some paths still downgrade to `potential_change`).

### 4) DX and maintainability

1. Keep extractor/parsing code as modular as render modules (ongoing refactor target).
2. Add optional confidence tags per assertion.
3. Expand protocol-based examples/docs for edge-case behavior.

## Suggested Next Build Order

1. Assert/abort expectation synthesis (function-level checks).
2. Control-flow precision improvements.
3. Remaining state-model gaps (`object_table`, deeper struct/key tracking).
4. DX + maintainability cleanup.
