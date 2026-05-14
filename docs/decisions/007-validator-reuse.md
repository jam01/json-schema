---
date: 2026-05-13
---
# How should the validator handle sequential reuse and document its concurrency contract?
## Context and Problem Statement
`json_schema.validator(schema, config, registry)` returns a `Visitor[?, OutputUnit]` backed by a single `DefaultContext` that holds per-traversal mutable state: an instance-location stack, an `_pointer` cache, and two maps tracking annotation `dependents`/`dependencies` for keywords like `unevaluatedProperties`/`unevaluatedItems`. The contract was undocumented and, in practice, the visitor was effectively single-shot for two reasons:

1. **`ffast` failures left location state dirty.** When validation fails at the root under `ffast=true`, the top-level visitor throws `ValidationException` from inside `FFastObjectSchemaValidator.compose`. The `tracker.pop` calls in `PointerDelegate.visitValue` that would have unwound the un-traversed remainder of the instance never run, so `insloc` retained the giveup-point tokens. A subsequent `.transform(...)` on the same visitor would prefix every instance-location with those tokens, silently corrupting results.
2. **`onScopeEnd` removed `dependents` for every ended scope.** Vocab constructors (notably `Unevaluated`) register dependants once, and lazily-resolved `$ref` paths add more on first traversal. Removing them by scope at end-of-scope meant the second traversal had no dependant tracking, so e.g. `unevaluatedProperties:false` falsely rejected previously-known-evaluated keys.

Separately, the root unit's `insLoc` on an `ffast` failure captured `ctx.instanceLoc` at throw time — i.e. wherever traversal gave up — instead of the root, which is what the successful path produces. Reuse aside, this was an internal inconsistency.

No documentation told users about any of this. Concurrent use was simply not supported and not stated.

## Decision Drivers
* Allow callers to amortize validator construction across many instances (the common shape: "validate a thousand JSON payloads against one schema").
* Make the concurrency contract explicit.
* Minimize blast radius — do not redesign the Context API in this change.
* Preserve the existing single-threaded, sequential-call performance.

## Considered Options
* (A) Document the visitor as single-shot. No code changes.
* (B) Sequential reuse via a `reset()` invoked at end of root scope. Document not-thread-safe.
* (C) Full thread-safety: move per-traversal state out of `DefaultContext` into a state value carried as a parameter through visit methods. Vocab visit signatures change.

## Decision Outcome
Chosen options: **(A) + (B)**.

`DefaultContext.reset()` clears `insloc`, `_pointer`, and `dependencies` (but **not** `dependents` — see below). It is called from `onScopeEnd` when the ending scope is `JsonPointer.Root`, which covers both the success path and the throw-from-`compose` path because `onScopeEnd` runs before the `ValidationException` is constructed.

`FFastObjectSchemaValidator.compose` was also amended so that, at the root scope (`dynParent.isEmpty`), the composed unit's `insLoc` is unconditionally `JsonPointer.Root` instead of `ctx.instanceLoc`. Under `ffast`, the latter would otherwise point at the give-up location when compose runs mid-traversal during exception propagation.

The scaladoc on `validator(...)` now documents the visitor as **not thread-safe** but **safe for repeated sequential `.transform(...)` calls**, including after `ValidationException`.

### Consequences
* Single-shot users see no change. Visitor construction is unchanged.
* Reuse users (the typical batch-validation case) now work correctly — including after `ffast` failures — without needing to rebuild the validator.
* `dependents` is no longer pruned per-scope; it is bounded by the schema's structure (number of `Unevaluated` sub-schemas plus those reachable via `$ref`), not by traversal count, so the memory cost is fixed for a given schema.
* `onScopeEnd` for the root scope now also runs `reset()`, which is cheap (one stack clear, one push, one var write, one map clear).
* Concurrent use across threads remains unsupported. A future change (Option C) would be a substantial API refactor and should be its own decision.

## Pros and Cons of the Options
### (A) Document single-shot only
* Cheapest. No code change.
* Forces every caller into the boilerplate `validator(...)` per instance — wastes the immutable vocab tree construction work on every call.
* Leaves the latent `insLoc`-on-failure inconsistency unfixed.

### (B) `reset()` at end of root scope
* Small, surgical change. Two files touched in production code.
* Fixes a real correctness bug (`unevaluatedProperties` across reuse) that the test suite happened not to exercise — every test-suite case builds a fresh validator.
* No throughput cost for single-shot users; one cheap reset call at end of each transform.
* Does not address concurrent use.

### (C) Full thread-safety
* Required to allow concurrent validation against the same schema from many threads.
* Touches every Vocab — they currently capture `ctx` at construction; a per-traversal state would need to be threaded through visit methods or stored on the per-call anonymous `ArrVisitor`/`ObjVisitor` returned by `visitArray`/`visitObject`.
* Annotation-dependency mechanism specifically relies on a Context shared across all sub-vocabs to mediate `offerAnnotation`/`getDependenciesFor` — that would need redesign (e.g. per-traversal state object passed alongside `ctx`).
* Out of scope for this change; warrants its own ADR.

## More Information
* `shared/src/main/scala/io/github/jam01/json_schema/Context.scala` — `reset()` is defined here, and `onScopeEnd` no longer removes `dependents`.
* `shared/src/main/scala/io/github/jam01/json_schema/SchemaValidator.scala` — root-scope `insLoc` fix in `FFastObjectSchemaValidator.compose`.
* `shared/src/main/scala/io/github/jam01/json_schema/package.scala` — scaladoc on `validator(...)` documents the contract.
* `shared/src/test/scala/io/github/jam01/json_schema/ValidatorReuseTest.scala` — regression coverage for: `ffast` failure → success, `ffast` failure → failure, repeated transforms involving `unevaluatedProperties`, and non-`ffast` reuse.
