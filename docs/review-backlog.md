---
date: 2026-08-12
---
# Open review findings, branch `bowtie`

Triage of a `/code-review` pass over `bowtie` (commits `31cfad8`..`ca8d06f`). Everything here is
either unactioned or deliberately deferred; items are removed as they are fixed or dismissed, not
struck through. Convert to issues if that turns out to be the better home.

**Verified** means reproduced or refuted here by running, not just read.

## Refuted — do not re-litigate

### `multipleOf` is not a denial of service — **verified**

The review reported `{"multipleOf": 7}` against `1234567890123456789e100000000` taking 237
seconds, and README § Numbers being wrong to promise sub-millisecond. It does not reproduce.

`java.math.BigDecimal.remainder` *without* a `MathContext` is indeed catastrophic — measured here
at 575 ms for `e=1000000`, growing superlinearly — but the library never calls it.
`scala.math.BigDecimal`'s `%` carries `DECIMAL128`, so the call raises
`ArithmeticException("Division impossible")` in ~0.2 ms, and `Validation.isMultiple` already maps
that to "not a multiple". The reported case completes in 153 ms including schema parse and JIT
warm-up, and answers correctly.

Whether that bail-out is ever the *wrong* answer was the real question, and it is checked: 8,010
cases — hand-picked adversarial shapes plus a seeded random sweep, half of them exact multiples by
construction — agree with an oracle that computes divisibility by modular arithmetic on the
unscaled values. Zero mismatches. `MultipleOfTest` keeps that sweep, and the timeouts there are
what stop a future change from reaching the slow path.

Residual, and the reason this is written down rather than deleted: the bail-out's correctness
rests on an invariant established by testing, not by proof — that a "Division impossible" from
`remainder` under `DECIMAL128` implies non-divisibility. Replacing `mod` with the oracle's
algorithm outright would remove the dependency on `scala.math.BigDecimal`'s choice of
`MathContext` and close the theoretical slow path for good, at the cost of touching working
number code. Deferred pending a decision.

## Number precision (`178cb34`, `5196a84`)

- **README § Numbers contradicts the shipped behaviour — verified.** README:299 says "`1e400`
  still overflows to `Float64(Infinity)`". `numOf`'s guard is
  `case Some(d) if d.isFinite && Math.abs(d) >= MIN_NORMAL`, and `"1e400".toDoubleOption` gives
  `Infinity`, so it falls through to `BigDecimal(s)` and becomes `Decimal`.
  `DecimalPrecisionTest:82-85` asserts exactly that. Doc-only fix.
- **`Dec128` → `Decimal` is a source-breaking public rename.** The case class (so also its
  extractor and the `Conversion[BigDecimal, Num]`) and `Value.dec128` → `Value.decimal`, with no
  deprecated alias. A `type Dec128 = Decimal` plus a `@deprecated def dec128` for one release
  would make it non-breaking. Worth deciding before 0.3.1 ships.
- **`Num.toBigDecimal` is a non-exhaustive match on an unsealed class.** `Value` is sealed but
  `Num` is not, so a downstream `class MyNum extends Num` reaches it and gets `MatchError` rather
  than the documented exception. Seal `Num` (it has exactly four intended cases) or add a default
  arm.
- **`decOf` no longer distinguishes anything.** `BigDecimal.apply(x: BigInt)` is `exact(x)`, which
  never rounds, so `decOf(i)` and `BigDecimal(i)` are value-identical and differ only in a carried
  `MathContext` that neither `compareTo` nor `%` consults. `compareTo` and `mod` each spell the
  same conversion both ways, which reads as load-bearing and is not. Unify or delete.
- **`sigDigits` takes a `decIndex` it never reads.** The body derives everything from `expIndex`
  and skips `'.'` implicitly.
- **Orphaned scaladoc in `Validation.scala`.** The `valueEquals` equality-contract doc sits above
  `canonical`, so `valueEquals` is undocumented and the doc attaches to the wrong method.
- **`uniqueItems` short-circuit fixed in `visitArray` but not `visitObject`.** `visitObject:155`
  still reads `(uniqueItems.isEmpty || !uniqueItems.get)`. `uniqueItems` cannot constrain an
  object at all, so the condition should not mention it — as written, `{"uniqueItems": true}` plus
  an object instance materializes the whole object through `LiteralVisitor` for nothing.

## Regex-adjacent (`31cfad8`, and this session's work)

- **`Applicator:369` stringifies a `CompiledPattern` — verified.** The `patternProperties` key type
  changed from `Regex` to `CompiledPattern`, but `.map((rgx, v) => (rgx.toString(), v))` stayed,
  so the tuple's first element is now `...RegexSupport$JavaCompiledPattern@1f2e3d` instead of the
  pattern source. Line 346 discards it, so nothing observable breaks today — but it is a per-key
  allocation of a meaningless value, and any future use of that element (an error message, an
  annotation location) silently gets garbage. Drop the `String` from the tuple, or give
  `CompiledPattern` a `toString` returning the source pattern.
- **The Scala.js `u` flag is an unannounced behaviour change for JS users.** The old shared
  implementation compiled through Scala.js's `java.util.regex` emulation, with no `u`. The JS
  target now rejects a bare `}` or `]`, `\-`, `\ `, `\101`, `\1` with no group, `[a-d[x-z]]` and
  `[a-z&&[b]]` at validator-construction time — patterns that worked in 0.3.0 and that the JVM
  target still accepts. README documents the divergence as JVM over-acceptance but never frames it
  as a JS regression; it belongs in the 0.3.1 release notes.
- **`FormatAssertion:64` catches a `UnsupportedOperationException` nothing can throw.** The comment
  attributes it to "scala native implementation"; there is no `native/` source set. Both
  `isValidPattern`s catch their own platform exception internally. Dead arm that would now only
  mask a real bug.

## Pointer resolution (`4049895`)

- **`schBy0` recompiles the pointer target on every resolution.** `DefaultContext.getSch` calls it
  per `#/...` reference and `Core._refVis` is per-`Core`-instance, so N `$ref`s into one raw
  literal run `SchemaW.transform` plus a fresh `SchemaR` over the whole subtree N times and build N
  distinct `ObjectSchema` graphs for one location. A self-referential literal recompiles once per
  level until `guardDepth` trips, which makes the depth guard the only bound on repeated
  compilation. A cache on the enclosing `ObjectSchema` keyed by pointer is the cheap fix.
- **A subschema compiled by `schBy0` honours its own `$id` but is never registered under it.**
  `SchemaR` skips `reg.addOne` and the ids/anchors flush when `parent` is set, yet
  `ObjectSchema.base` still resolves `getId`, so a `$ref` or `$anchor` inside the literal resolves
  against a base no registry entry matches and raises `SchemaRetrievalException`. Noted in the
  scaladoc; absent from README's limitations, and [decision-010](decisions/010-pointer-into-non-schema.md)
  is the only place a user would find it.
