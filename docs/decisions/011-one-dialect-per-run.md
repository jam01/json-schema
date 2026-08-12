---
date: 2026-08-12
---
# Should a referenced schema resource be evaluated under its own `$schema`'s dialect?
## Context and Problem Statement
The dialect is fixed for an entire validation run. `SchemaValidator.apply` always uses `ctx.config.dialect`, which is either what the caller passed in `Config` or — under `Config(resolveDialect = true)` — what `Dialect.tryDialect` resolved once, from the **root** schema's `$schema`.

JSON Schema Core § 8.1.1 puts `$schema` at the root of a *schema resource* and has it declare the dialect for that resource; `$ref` resolution crosses resource boundaries. So a referenced resource should be evaluated under the dialect its own `$schema` names, not the referrer's:

```json
{ "$id": "https://example/referrer",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "properties": { "addr": { "$ref": "https://example/strict" } } }
```
```json
{ "$id": "https://example/strict",
  "$schema": "https://example/meta/format-assertion-true",
  "format": "ipv4" }
```

Here `format` in the referenced resource is evaluated as an annotation, because the referrer's dialect says so. The same applies to any custom `$vocabulary` — a referenced resource that drops `Metadata`, or adds a user-supplied vocabulary, gets the referrer's set.

`optional/cross-draft.json` is the suite's case for this, and it is excluded.

## Decision Drivers
* Conformance, weighed against what anyone will actually hit.
* `Config(resolveDialect = true)` is already opt-in, so a change confined to it cannot affect callers on the default path.
* This library implements 2020-12 semantics only, and README says so.
* Keep the annotation-dependency machinery ([decision-002](002-dynamic-deps.md) through [decision-004](004-invalid-dyn-deps.md)) intact — it assumes a single vocabulary set across a traversal.

## Considered Options
* (A) Keep one dialect per run; document the deviation.
* (B) Resolve per schema resource, using the vocabularies already implemented, behind `resolveDialect`.
* (C) (B) plus historic-draft vocabularies, i.e. real cross-draft support.

## Decision Outcome
Chosen option: **(A)** for now. **(B)** is tracked as [issue #18](https://github.com/jam01/json-schema/issues/18) and is the right eventual shape. **(C)** is rejected.

The limitation is stated in README § `$ref` and the registry, and `optional/cross-draft.json` stays in `TestSuiteTest.NotSupportedOptional` with the reasoning recorded there.

### Consequences
* A `$ref` crossing into a resource whose meta-schema declares a different `$vocabulary` is evaluated under the referrer's dialect. Under `Config.Default` — a fixed dialect, no resolution — this is invisible, which is the overwhelming majority of use.
* `optional/cross-draft.json` cannot pass regardless of (B), because it needs (C). Excluding it is not a decision deferred by this ADR.
* Deferring (B) leaves the resolution point in one place, `SchemaValidator.apply`, which keeps the eventual change contained.
* Nothing else in the ecosystem appears to implement per-resource dialect resolution, so there is no reference behaviour to match and no user demand pulling on it. That is the main reason this is (A) and not (B) today: it is correctness for its own sake, and the cost is not in the lookup.

### What (B) actually costs
Not the lookup — `Dialect.tryDialect` already does the `$vocabulary` work, `ObjectSchema` already tracks its base URI, and resource roots are identifiable (the document root, plus any schema carrying `$id`). That part is plumbing and a per-base cache. The cost is in the interactions:

* **`$dynamicRef`/`$dynamicAnchor`** resolve against the dynamic scope, which crosses resources. Which resource's dialect governs a dynamic resolution needs its own answer.
* **`unevaluatedItems`/`unevaluatedProperties`** consume annotations produced by sibling and subschema keywords. Once neighbouring resources can run under different vocabulary sets, an annotation may be produced under one and consumed under another — including where the producing vocabulary is not in the consumer's dialect at all. That is squarely in the machinery [decision-003](003-annotation-dyn-deps.md) and [decision-004](004-invalid-dyn-deps.md) set up, and it is where the work would live.

## Pros and Cons of the Options
### (A) One dialect per run
* Status quo; zero risk; the annotation machinery keeps its single-vocabulary-set assumption.
* A documented spec deviation, in a place a reader will find it.
* Silently wrong for the custom-`$vocabulary`-across-`$ref` case — no error, just the wrong keyword semantics.

### (B) Per-resource resolution, 2020-12 vocabularies only
* Correct for every case reachable with the vocabularies that exist, including the custom-`$vocabulary` example above and format assertion differing across a `$ref`.
* Confined to `resolveDialect = true`, so the blast radius is callers who already opted into `$schema`-driven resolution.
* Forces an answer on dynamic-scope and annotation-flow questions that are currently trivial because there is one dialect.
* Buys conformance nobody has asked for.

### (C) Per-resource plus historic-draft vocabularies
* The only option that closes `optional/cross-draft.json`.
* A different order of magnitude: 2019-09 `items`-as-tuple-validation, `additionalItems`, `definitions`, `dependencies`, `$recursiveRef` — a second full vocabulary set and the keyword semantics behind it.
* Contradicts README's "earlier drafts are not supported", which is a deliberate scope choice, not an omission.

## More Information
* `shared/src/main/scala/io/github/jam01/json_schema/package.scala` — `validator(...)` resolves the dialect once, here.
* `shared/src/main/scala/io/github/jam01/json_schema/SchemaValidator.scala` — `apply` applies `ctx.config.dialect` unconditionally; this is the seam (B) would change.
* `shared/src/main/scala/io/github/jam01/json_schema/Dialect.scala` — `tryDialect` already builds a dialect from a meta-schema's `$vocabulary`.
* `shared/src/test/scala/io/github/jam01/json_schema/TestSuiteTest.scala` — `NotSupportedOptional` carries the `cross-draft.json` reasoning.
* README § `$ref` and the registry — the user-facing statement.
* [issue #18](https://github.com/jam01/json-schema/issues/18).
