---
date: 2026-08-12
---
# Open review findings, branch `bowtie`

Triage of a `/code-review` pass over `bowtie` (commits `31cfad8`..`ca8d06f`). Items are removed as
they are fixed or dismissed, not struck through. Convert to issues if that turns out to be the
better home.

**Verified** means reproduced or refuted here by running, not just read.

## Settled — do not re-litigate

### `multipleOf` was never a denial of service — **verified**

The review reported `{"multipleOf": 7}` against `1234567890123456789e100000000` taking 237
seconds. It did not reproduce. `java.math.BigDecimal.remainder` *without* a `MathContext` is that
slow — measured at 575 ms for `e=1000000`, growing superlinearly — but the library never called
it. `scala.math.BigDecimal`'s `%` carries `DECIMAL128`, which abandoned the division as "Division
impossible" in ~0.2 ms, and `isMultiple` read that as "not a multiple"; the reported case answered
correctly in 153 ms including parse and JIT warm-up.

The real question was whether that bail-out was ever the *wrong* answer, since "Division
impossible" is a statement about representing the quotient rather than about divisibility. No
counterexample was ever found: roughly 8,000 random cases plus every adversarial shape that five
separate theories predicted would break it — quotients past `DECIMAL128`'s 34 digits, quotients
unrepresentable at the preferred scale, divisor unscaled values not dividing the dividend's — all
answered correctly.

It is moot now regardless: `Validation.divides` computes divisibility directly, by modular
arithmetic on the unscaled values, so nothing depends on that invariant and no power of ten is
ever materialized. `MultipleOfTest` checks it against the slow-but-obvious formulation, including
mantissas longer than `DECIMAL128` can hold.

## Number precision (`178cb34`, `5196a84`)

Two items resolved: README § Numbers no longer claims `1e400` saturates to `Float64(Infinity)`,
and the `Dec128` → `Decimal` rename stands as-is — no API is promised yet, so no deprecated alias
is owed.

- **`Num.toBigDecimal` is a non-exhaustive match on an unsealed class.** `Value` is sealed but
  `Num` is not, so a downstream `class MyNum extends Num` reaches it and gets `MatchError` rather
  than the documented exception. Seal `Num` (it has exactly four intended cases) or add a default
  arm.
- **`decOf` no longer distinguishes anything.** `BigDecimal.apply(x: BigInt)` is `exact(x)`, which
  never rounds, so `decOf(i)` and `BigDecimal(i)` are value-identical and differ only in a carried
  `MathContext` that `compareTo` does not consult. `compareTo` spells the same conversion both
  ways, which reads as load-bearing and is not. Unify or delete.
- **`sigDigits` takes a `decIndex` it never reads.** The body derives everything from `expIndex`
  and skips `'.'` implicitly.
- **Orphaned scaladoc in `Validation.scala`.** The `valueEquals` equality-contract doc sits above
  `canonical`, so `valueEquals` is undocumented and the doc attaches to the wrong method.
- **`uniqueItems` short-circuit fixed in `visitArray` but not `visitObject`.** `visitObject` still
  reads `(uniqueItems.isEmpty || !uniqueItems.get)`. `uniqueItems` cannot constrain an object at
  all, so the condition should not mention it — as written, `{"uniqueItems": true}` plus an object
  instance materializes the whole object through `LiteralVisitor` for nothing.

## Regex-adjacent (`31cfad8`)

The Scala.js `u` flag stands: it is the more correct behaviour, and no API is promised yet. The
discarded `CompiledPattern.toString` is fixed — `matchedPatternSchs` no longer carries the string.

- **`FormatAssertion` catches an `UnsupportedOperationException` nothing can throw.** The comment
  attributes it to "scala native implementation"; there is no `native/` source set, and both
  `isValidPattern`s catch their own platform exception internally. A dead arm that would now only
  mask a real bug.

## Pointer resolution (`4049895`)

Memoization is in: `ObjSchema.compiledLiteral` compiles a raw literal once per location, pinned by
`SchemaRetrievalExceptionTest.a_literal_is_compiled_once_per_location`.

- **A literal carrying its own `$id` becomes an unreachable resource — verified.** `SchemaR` skips
  `reg.addOne` and the ids/anchors flush when `parent` is set, but `ObjectSchema.base` still
  honours `getId`, so references *inside* the literal resolve against a base the registry never
  learned. Both of these fail:

  ```json
  {"$id": "https://ex/root",
   "unknown": {"$id": "https://ex/sub", "$defs": {"a": {"type": "integer"}}, "$ref": "#/$defs/a"},
   "$ref": "#/unknown"}
  ```

  → `NoSuchElementException: Unavailable schema https://ex/sub#/$defs/a` (and the `$anchor`
  spelling gives `...#aa`). Note the exception type: the review said `SchemaRetrievalException`,
  but it surfaces from the registry, so callers catching the documented type miss it.

  Either register the compiled subschema under its `$id` and flush its anchors, or decide that a
  literal reached by pointer does not establish a resource and stop honouring `$id` for it. The
  first matches Core § Fragment Identifiers; the second is simpler. Until then it is a gap
  documented only in [decision-010](decisions/010-pointer-into-non-schema.md), not in README.
