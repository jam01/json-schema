---
date: 2026-08-12
---
# How should a JSON Pointer that lands outside a recognized subschema be resolved?
## Context and Problem Statement
`SchemaR` compiles a value into a `Schema` only when the keyword holding it is one it recognizes as schema-bearing — `items`, `properties`, `allOf` and the rest of the list in its `subVisitor`. Everything else goes to `LiteralVisitor` and stays a raw `Value`. That is the right default: an unknown keyword's object value is arbitrary JSON, and eagerly compiling it would both waste work and throw on data that was never meant to be a schema.

But JSON Schema Core § Fragment Identifiers says a JSON Pointer fragment resolves against the schema *resource* as plain JSON. Nothing restricts a pointer to locations the parser happened to recognize, so all of these are legal and each names a subschema:

```json
{ "unknown-keyword": {"type": "integer"},
  "properties": {"bar": {"$ref": "#/unknown-keyword"}} }
```
```json
{ "$id": "/base", "examples": [{"type": "string"}], "$ref": "#/examples/0" }
```

`ObjSchema.schBy0` walked the pointer and then did `res.asInstanceOf[Schema]`, so both threw `ClassCastException` — an unhandled crash, and one no caller could reasonably be catching for. `optional/refOfUnknownKeyword.json` covers exactly this and was failing.

## Decision Drivers
* Spec conformance: the suite file is a real gap, not an edge case someone invented.
* Do not compile arbitrary JSON eagerly at parse time — an unknown keyword's value may be anything.
* A resolved subschema must behave like one that was recognized up front: its `location` and `base` have to come out the same, or `$ref`s inside it resolve against the wrong base.
* Failure modes should be typed. A pointer landing on a string is a user error, and should report like every other bad reference.

## Considered Options
* (A) Reject — report `SchemaRetrievalException` for any pointer target the parser did not compile.
* (B) Compile every object during parse, whatever keyword holds it.
* (C) Wrap the raw `Obj` in an `ObjectSchema` on demand, reusing its existing children.
* (D) Compile the raw subtree through `SchemaR` on demand.

## Decision Outcome
Chosen option: **(D)**.

`schBy0` matches on what the pointer walk produced. An already-compiled `Schema` is returned as-is; `True`/`False` become `TrueSchema`/`FalseSchema`; an `Obj` is replayed through `SchemaR` via a new package-private `SchemaR.subschema(docbase, parent, prel)`; anything else — a string, a number, an array element — throws `SchemaRetrievalException`, the same exception a pointer to a missing key already produced.

`SchemaR.subschema` reuses `SchemaR`'s existing private constructor, which already takes a `parent` and a relative pointer, so the compiled subtree gets the same lexical anchoring it would have had if its keyword had been recognized.

Option (C) is what an earlier iteration did, and it is subtly insufficient — see below.

### Consequences
* `optional/refOfUnknownKeyword.json` passes, including the `examples` case.
* **`ObjectSchema`'s accessors do not parse.** They assume `SchemaR` already turned every schema-position child into a `Schema`; `getSchemaObjectOpt` bottoms out in `Value.sch`, which throws `IllegalStateException("Expected Schema")` on a raw `Obj`. That is why (C) is not enough: it converts one node and leaves the children raw, so a referenced subschema containing `properties`, `items`, `allOf` — any applicator — crashes. Only leaf subschemas work, which is all the suite file happens to contain.
* A nested `$ref` inside such a subschema worked even under (C), but incidentally: `Core` reads `$ref` as a *string* and resolves it through the `Registry`, never touching `.sch`. Not evidence that (C) was sound.
* **`SchemaR.subschema` takes no registry.** With a `parent` set, `SchemaR` neither registers the schema it builds nor flushes the `$id`/`$anchor`s it collects, so a subschema compiled this way is reachable only through the pointer that produced it — never by its own `$id` or `$anchor`. That is exactly its status before it was compiled at all, and `$ref`s *inside* it still resolve normally against the `Registry` handed to `json_schema.validator`.
* **A literal reached by pointer does not establish a schema resource**, so `$id` inside one carries no identity: `base` and `location` ignore it, and the whole compiled subtree inherits that, not just its outermost node. `getId` still reads it.

  This closes a hole rather than adding a restriction. `SchemaR` was already declining to register these, while `ObjectSchema.base` went on honouring `$id` — so

  ```json
  {"$id": "https://ex/root",
   "unknown": {"$id": "https://ex/sub", "$defs": {"a": {}}, "$ref": "#/$defs/a"},
   "$ref": "#/unknown"}
  ```

  resolved its inner `$ref` against a base no `Registry` had heard of and died with `NoSuchElementException: Unavailable schema https://ex/sub#/$defs/a` — not even the `SchemaRetrievalException` a caller would be catching for. It now resolves against the enclosing resource, which is registered.

  Core § 9.4.2 leaves this undefined, and names this exact situation as the reason: nested subschemas under unrecognized keywords "would be subject to the processing rules for `$id`", which is why "having a reference target in such an unrecognized structure cannot be reliably implemented". Registering instead is the branch § 9.4.2 says cannot be done reliably — it needs a parse-time decision about which unknown-keyword objects are resources — and it would only half-work anyway: an external `$ref` to `https://ex/sub` would resolve only if something had already pointed into that literal first, making resolution order-dependent. Not establishing a resource is the reading that never mints an unresolvable URI.
* **Observable:** schemas inside such a literal report an `absoluteKeywordLocation` under the enclosing resource (`https://ex/root#/unknown/…`) rather than under the phantom one. `ObjectSchema.equals`/`hashCode` include the flag, since two schemas with identical content resolve references differently if one honours `$id` and the other does not.
* Compilation is memoized per location on the enclosing schema. `$ref` resolution is per-reference and per-`Core`-instance, so without it every reference into one literal re-ran the subtree through `SchemaR` and produced a distinct `Schema` graph for a single location; a self-referential literal did it once per level of recursion, leaving `guardDepth` as the only bound on repeated compilation.
* The exception type for a pointer to a scalar changed from `ClassCastException` to `SchemaRetrievalException`. A caller catching the former was catching an accident.

## Pros and Cons of the Options
### (A) Reject
* Trivial, and honest about what the parser knows.
* Fails a conformance case the suite explicitly covers. The spec is unambiguous that the pointer is legal.

### (B) Compile eagerly during parse
* No on-demand path at all; every location is already a `Schema`.
* Wrong. `{"x-metadata": {"type": "internal"}}` is not a subschema, and compiling it means `SchemaR` decides that arbitrary user JSON must parse as a schema.
* Pays the cost for every unknown keyword in every schema, to serve a rare reference.

### (C) Wrap the `Obj` on demand
* One line, and it clears the suite file.
* One node deep. Every child stays a raw `Obj`, so any applicator inside the referenced literal throws `IllegalStateException`. The failure is invisible until someone writes a `$ref` to a non-trivial subschema, and the suite does not.

### (D) Compile the subtree on demand
* Produces a subschema indistinguishable from one the parser recognized, at any depth.
* Reuses `SchemaR` rather than reimplementing which keywords are schema-bearing — there is exactly one such list and this does not add a second.
* Costs a `MutableRegistry` allocation per call that is never written to, which is the price of `SchemaR`'s current constructor shape.
* Nested `$id`/`$anchor` inside the compiled subtree are not registered, and — per the above — establish nothing either.

## More Information
* `shared/src/main/scala/io/github/jam01/json_schema/ObjSchema.scala` — `schBy0`'s match on the pointer target.
* `shared/src/main/scala/io/github/jam01/json_schema/SchemaR.scala` — `SchemaR.subschema`, and the `parent`/`prel` constructor parameters it reuses.
* `shared/src/test/scala/io/github/jam01/json_schema/SchemaRetrievalExceptionTest.scala` — object/boolean/scalar targets, a referenced subschema carrying `properties` and `allOf`, the memoization, and `$id` carrying no identity.
* [Core § 9.4.2](https://json-schema.org/draft/2020-12/json-schema-core.html), *References to Possible Non-Schemas* — why this is undefined territory. The suite keeps `refOfUnknownKeyword.json` under `optional/`, and none of its cases puts an `$id` inside the referenced literal.
* `shared/src/test/resources/test-suite/tests/draft2020-12/optional/refOfUnknownKeyword.json` — the conformance cases.
