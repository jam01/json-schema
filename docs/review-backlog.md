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

Closed out. README no longer claims `1e400` saturates; the `Dec128` → `Decimal` rename stands, as
no API is promised yet; `Num` is sealed, so `toBigDecimal`'s match is exhaustive by construction;
`decOf` is gone, since `BigDecimal(BigInt)` attaches a context sized to the value and never
rounds; `sigDigits` no longer takes the `decIndex` it never read; the equality-contract scaladoc
sits on `valueEquals` again; and `uniqueItems` no longer has a say in whether an object instance
is materialized, which it cannot constrain.

## Regex-adjacent (`31cfad8`)

The Scala.js `u` flag stands: it is the more correct behaviour, and no API is promised yet. The
discarded `CompiledPattern.toString` is fixed — `matchedPatternSchs` no longer carries the string.

- **`FormatAssertion` catches an `UnsupportedOperationException` nothing can throw.** The comment
  attributes it to "scala native implementation"; there is no `native/` source set, and both
  `isValidPattern`s catch their own platform exception internally. A dead arm that would now only
  mask a real bug.

## Pointer resolution (`4049895`)

Both items resolved. Compilation is memoized per location, and a literal reached by pointer no
longer establishes a schema resource, so `$id` inside one carries no identity and references
inside it resolve against the enclosing resource. Rationale and the Core § 9.4.2 reading are in
[decision-010](decisions/010-pointer-into-non-schema.md).
