---
date: 2026-08-12
---
# How much numeric precision should schema literals and instance data carry?
## Context and Problem Statement
`Validation.numOf` maps a JSON number literal to the narrowest exact representation — `Long`, `Double`, `BigInt` or `BigDecimal` — and `LiteralVisitor` wraps that in the `Value` ADT's `Int64`/`Float64`/`Int128`/`Dec128`. The integer branch, `s.toLongOption.getOrElse(BigInt(s))`, is sound because `toLongOption` returns `None` when the literal does not fit. The decimal branch, `s.toDoubleOption.getOrElse(BigDecimal(s))`, is not: `Double.parseDouble` never fails. It rounds silently past ~15-17 significant digits, saturates to an infinity past ~1.8e308, and flushes to zero below ~4.9e-324 — each reported as a successful parse. The `BigDecimal` fallback was therefore dead code, and every decimal literal in a schema or an instance was silently rounded regardless of magnitude.

Two further constraints sat on top of that:

1. **`Int128`/`Dec128` self-validated a 128-bit / IEEE-754 Decimal128 bound in their constructors**, throwing a raw `IllegalArgumentException`. For decimals the check was unreachable precisely because `numOf` never produced a `BigDecimal`; for integers it fired, including on *instance* numbers nested in an array or object, which `Validation.visitArray`/`visitObject` collect through the same `LiteralVisitor` for deep `const`/`enum`/`uniqueItems` comparison.
2. **`Validation.decOf` capped `BigInt` → `BigDecimal` conversion at Decimal128**, throwing `ArithmeticException("Decimal128 overflow")`. That path is reached by any mixed-width comparison, so `{"minimum": 1.5}`, `{"const": 1}` or `{"enum": [1, 2]}` against a 60-digit instance integer crashed rather than answering.

Separately, `uniqueItems` detects duplicates with a `HashSet[Value]`, i.e. by case-class equality, so `Int64(1)` and `Float64(1.0)` were distinct keys — while `const`/`enum` compare through the numeric-aware `valueEquals` in the same file.

RFC 8259 places no limit on JSON numbers. JSON Schema Core notes that implementations SHOULD support at least IEEE-754 double and warns that going beyond risks interoperability, and the official test suite files arbitrary precision under `optional/bignum.json` — so rounding is a conforming choice and this is a decision, not a compliance obligation.

## Decision Drivers
* The library's headline claim is end-to-end conformance against the official suite, tracked publicly on bowtie.report.
* An ordinary schema must never crash. `{"minimum": 1.5}` answering with an `ArithmeticException` is worse than any precision question.
* Do not reject schemas that every other validator accepts. A long decimal under `examples`, `default` or an `x-` keyword is not a correctness problem — nothing compares it.
* Keep the `Double` fast path for ordinary decimals like `3.14`; precision must not cost the common case.
* Number handling is a streaming-parse concern; whatever is chosen has to be decidable from the literal's text as it arrives, without a second pass.

## Considered Options
* (A) Round everything to `Double`, as the code effectively did and as ajv, python-jsonschema and Jackson-by-default do.
* (B) Bound schema literals to 128 bits / Decimal128 and leave instance data unbounded.
* (C) Arbitrary precision on both sides, no bound anywhere.
* (D) (C) plus a configurable resource guard on instance data.

## Decision Outcome
Chosen option: **(C)**, with **(D)** left open and unimplemented.

`numOf` routes on significant-digit count, and then verifies that `Double` actually holds the value:

```scala
if (digits > DoubleSafeDigits) BigDecimal(s)          // more precision than Double round-trips
else s.toDoubleOption match
  case Some(d) if digits == 0 => d                    // the literal is zero, whatever its exponent
  case Some(d) if d.isFinite && Math.abs(d) >= java.lang.Double.MIN_NORMAL => d
  case _ => BigDecimal(s)
```

`DoubleSafeDigits` is 15, the most decimal digits `Double` is guaranteed to round-trip. The `MIN_NORMAL` floor rules out saturation to an infinity, flush-to-zero, and subnormals — where `Double` loses precision well before 15 digits. `sigDigits` returns 0 for an all-zero mantissa, which is how a genuine zero is told apart from underflow.

The 128-bit/Decimal128 bound is removed: `SchemaR.checkAnchor`, `Int128.exceedsAnchor`, `Dec128.exceedsAnchor` and `SchemaCompileException` are gone, and `decOf` converts exactly. `Dec128` is renamed `Decimal` because the name asserted a bound it no longer has; `Int128` keeps its name as "an integer wider than `Long`".

For `uniqueItems`, elements are canonicalized before hashing — whole numbers to `Int64` where they fit, everything else to `Decimal`, recursively through arrays and objects — rather than comparing every pair with `valueEquals`.

### Consequences
* Comparisons are exact at any width and mix widths freely. `{"exclusiveMaximum": 3.0000000000000000001}` distinguishes `3.00000000000000000005` from `3.0000000000000000002`.
* `1e400` is `Decimal(1E+400)`, not `Float64(Infinity)`. `type: integer` accepts it (it is one) and `maximum` compares it instead of throwing `NumberFormatException` converting `Infinity` to a `BigDecimal`.
* `optional/bignum.json`'s two "float comparison with high precision" cases pass, so `TestSuiteTest.NotSupportedOptionalCases` is empty.
* **Breaking for the `Value` ADT's readers.** A decimal literal of 16+ significant digits now arrives as `Decimal` where it used to be `Float64`, so `.float64` throws `IllegalStateException` on input it used to handle. `Num.toBigDecimal` and `Value.num` were added to widen whichever case holds the number; they are the accessor to reach for when reading a number out of a schema.
* **This exceeds what the spec requires.** A schema written against this library — one relying on a 30-digit `maximum` comparing exactly — may not behave the same on a validator that rounds.
* The `Value` ADT no longer enforces any invariant about magnitude, so a schema built programmatically can hold any number. Nothing downstream needs the invariant, but it is no longer a type-level guarantee.
* Cost scales with the *length of the JSON text*, not with a value's magnitude: `BigDecimal` keeps scale separate from the significand, so exponent notation is free. Measured — `{"multipleOf": 7}` against a 300,000-digit integer takes ~1.4s, while `1234567890123456789e100000000` (a 30-byte literal) takes ~0.4ms. The only lever is payload size, and the mitigation is the ordinary one: bound the request body. Documented in README § Numbers.
* If a guard is ever wanted, option (D) belongs on instance data — the untrusted side — as a `Config` knob beside `maxDepth`, surfacing as a failing `OutputUnit` rather than a thrown exception, which is how `maxDepth` already behaves.

## Pros and Cons of the Options
### (A) Round everything to `Double`
* What most of the field does; maximum interoperability, no surprises when porting a schema elsewhere.
* Conforming — arbitrary precision is `optional/` in the suite.
* Silently wrong for any schema that means what it wrote: `{"const": 1.0000000000000000001}` matches `1.0000000000000000002`.
* Was never an explicit choice here — it was the accidental behavior of a dead fallback branch.

### (B) Bound schema literals, leave instances unbounded
* The status quo ante, once the precision fix made it reachable.
* Guards the *trusted* side. A schema is authored; instance data is what arrives from outside. Backwards as a defence.
* Rejects previously-valid schemas, including a long decimal in `examples`, `default` or an `x-` keyword, where nothing ever compares the number.
* The two limits disagreed: `checkAnchor` admitted 128 bits (~39 digits) while `decOf` capped comparisons at 34, leaving a window where a schema compiled and then threw on every instance.
* Not load-bearing for determinism. The concern behind a bound attaches to `MathContext`, not magnitude — `BigDecimal.divide` under an unlimited context can throw on a non-terminating expansion, but `mod` goes through `%`/`divideToIntegralValue`, which is exact and always terminating, and `compareTo` never consults a `MathContext`. Measured: removing the `decOf` cap changed no timing on any of sixteen probes, and `multipleOf` against a 300,000-digit integer was already exact *with* the bound in place, because `BigDecimal(BigInt)` never rounded.

### (C) Arbitrary precision everywhere
* Matches the spec's model of `integer`/`number` and the library's conformance claim.
* Removes an entire class of crash from ordinary schemas.
* Costs nothing measurable, and nothing at all on the `Double` fast path.
* Unbounded work on untrusted input — but that was already true for integers, which have gone through `BigInt` since the beginning.
* Leaves `Int128` a historical name.

### (D) (C) plus a configurable instance-side guard
* The version that actually defends the untrusted side, and consistent with `maxDepth`'s precedent.
* Nobody has asked. Adding a `Config` knob and an `OutputUnit` shape for it is a feature, not a patch, and the input-length bound above is a real mitigation in the meantime.

### On `uniqueItems` equality
* Canonicalizing keeps the keyword O(n) and reuses the existing `HashSet`; comparing every pair with `valueEquals` would make it quadratic on an instance-supplied array.
* Whole numbers canonicalize to `Int64` where they fit, so the overwhelmingly common array-of-integers case allocates nothing.
* The official suite does not cover this: its case is named "numbers are unique if mathematically unequal" but the data is `[1.0, 1.0, 1]`, whose first pair collides on representation alone, so the answer came out right without ever comparing `1.0` to `1`.

## More Information
* `shared/src/main/scala/io/github/jam01/json_schema/vocab/Validation.scala` — `numOf`, `sigDigits`, `decOf`, `canonical`, and the `uniqueItems` branch of `visitArray`.
* `shared/src/main/scala/io/github/jam01/json_schema/Schema.scala` — `Int64`/`Float64`/`Int128`/`Decimal`, `Num.toBigDecimal`, `Value.num`.
* `shared/src/main/scala/io/github/jam01/json_schema/Visitors.scala` — `LiteralVisitor` maps `numOf`'s result to the ADT.
* `shared/src/test/scala/io/github/jam01/json_schema/DecimalPrecisionTest.scala` — precision, magnitude, zero and subnormal handling.
* `shared/src/test/scala/io/github/jam01/json_schema/EnumConstEqualityTest.scala` — numeric equality for `uniqueItems`, and `uniqueItems: false`.
* README § Numbers — the user-facing statement, including the cost profile.
* Supersedes the unstated bound `Int128`/`Dec128` carried in their constructors from their introduction until this change.
