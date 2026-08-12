---
date: 2026-08-12
---
# How should ECMA-262 regex semantics be provided on each platform?
## Context and Problem Statement
JSON Schema specifies `pattern`, `patternProperties` and `format: regex` against **ECMA-262** regular expressions. Neither target's default engine is one:

* On Scala.js the platform has a real ECMA-262 engine, but `scala.util.matching.Regex` routes through Scala.js's `java.util.regex` emulation instead of reaching it directly.
* On the JVM, `java.util.regex.Pattern` is a different language. It diverges in ways that are bounded and well-documented, but real.

The shared implementation compiled with `new Regex(s).unanchored` and, on `PatternSyntaxException`, retried with the `(?U)` embedded flag. That retry shipped in 0.3.0 as the fix for `\p{Letter}` and **never worked**: verified against JDK 25, `\p{Letter}` throws `Unknown character property name {Letter}` both plain and under `(?U)`. Java recognizes Unicode `General_Category` short codes (`\p{L}`) and `Is`-prefixed aliases, never ECMA-262's long-form names. The bug shipped undetected because `optional/ecmascript-regex.json` sits below the depth `TestSuiteTest`'s `Files.walk(..., 1)` reached, so nothing ran it.

## Decision Drivers
* Conformance against the official suite, which is the library's headline claim.
* One library, two targets. A heavy JVM-only dependency is weight the Scala.js artifact cannot use and cannot shed.
* `format: regex` must answer for the same language `pattern` compiles, or the two contradict each other.
* Divergences that produce a *wrong match* are worse than ones that produce an error — they are invisible.

## Considered Options
* (A) One shared implementation over `scala.util.matching.Regex`, as before.
* (B) Per-platform: the native `RegExp` on Scala.js, a translation table on the JVM.
* (C) Embed an ECMA-262 engine on the JVM — Graal's TRegex, or Joni in ECMAScript syntax mode.
* (D) Write an ECMA-262 engine.

## Decision Outcome
Chosen option: **(B)**, with **(C)** deferred to a dedicated spike rather than rejected.

`RegexSupport` follows the pattern already established by `Idn`: a `shared/` interface, `CompiledPattern`, with a `jvm/` and a `js/` implementation.

Scala.js constructs `new js.RegExp(s, "u")` and calls `.test`. The `u` flag is not optional — without it `\p{…}` does not error, it *silently fails to match*, and the suite requires `\p{Letter}` and `\p{digit}` to work. `v` (ES2024) was measured and is strictly more restrictive than `u`, not less.

The JVM translates the divergences the official suite exercises, each verified directly against a real JDK:

1. `\p{…}` long-form `General_Category` names, via the alias table from the Unicode standard (`\p{Letter}` → `\p{L}`).
2. `\c` + lowercase letter — ECMA-262 computes the control code as `charCode & 0x1F`, Java as `charCode ^ 0x40`; the two agree only for uppercase, so the letter is upper-cased first.
3. `\s`/`\S` — ECMA-262's whitespace set is fixed and wider than Java's even under `UNICODE_CHARACTER_CLASS`, notably including `﻿`, which Unicode's own `White_Space` property excludes.

Property names that need `UNICODE_CHARACTER_CLASS` keep the compile-plain-then-retry-`(?U)` approach, so the majority of patterns stay on the zero-flag path — `(?U)` also broadens `\d`/`\w` beyond ECMA-262, where both are always ASCII-only.

`format: regex` routes through `RegexSupport.isValidPattern`, so it answers for the *translated* form. A pattern is valid `format: regex` exactly when `pattern` would accept it.

### Consequences
* **The JVM target is an approximation, not a conformant engine**, and README § Regular expressions says so with the error budget spelled out. Known and untranslated, all JVM-only:

  | construct | ECMA-262 | JVM behaviour | severity |
  | --- | --- | --- | --- |
  | `$` / `^` | end / start of input | also match around a trailing newline | silent wrong match |
  | `\v` | vertical tab only | any vertical whitespace, incl. `\n` | silent wrong match |
  | `\S` inside `[…]` | excludes NBSP, `﻿` | matches them | silent wrong match |
  | `\Q…\E`, `\A`, `\z`, `\Z`, `\G`, `\h`, `\R`, `\X`, `a*+`, `[a-z&&[b]]`, `\p{Is…}` | invalid | accepted by `format: regex` | wrong answer |

* **The suite is not a safety net here.** Of those seven divergences it exercises one. Its own `$`-versus-trailing-newline fixture cannot fail on any engine: the data is `"abc\\n"`, a literal backslash followed by `n`. Divergences have to be enumerated deliberately, which is what the spike is for.
* Scala.js is the conformant target, so the parity gap runs the *opposite* way from `Idn`'s. It is documented under § Regular expressions rather than § Scala.js limitations, which is about the JS artifact falling short.
* `u` is also *stricter* than default ECMA-262, so a few legacy-but-tolerated spellings (`a{,3}`, a bare `}` or `]`, `\-`) are a syntax error on Scala.js while the JVM accepts them. A try-`u`-then-no-flag retry mirroring the JVM's plain-then-`(?U)` retry would close this in about three lines; deliberately not taken yet.
* `\S` inside a `[…]` class is the one member of the `\s`/`\S` work left untranslated — a negated set is not expressible as a union there. It keeps Java's `\S`, which, being the complement of Java's narrower `\s`, is *wider* than ECMA-262's.
* `translateAliases` and `translateControlEscapes` use plain regex replacement and are not backslash-run aware, unlike `translateWhitespaceEscapes` and the `\a` check. A literal backslash followed by `p{Letter}` would be rewritten. No conforming ECMA-262 pattern reaches it.
* The `js` smoke test now carries regex canaries, so both targets are checked to agree on the cases the JVM had to patch.

## Pros and Cons of the Options
### (A) One shared implementation
* Simplest; one file, no platform split.
* Wrong on both targets. Scala.js never reaches its own engine, and the JVM's divergences are unaddressed — the `(?U)` retry that was supposed to cover them provably does not.

### (B) Per-platform, translate on the JVM
* Scala.js becomes exactly conformant for free, by deleting code rather than adding it.
* No new dependency on either target.
* Scoped to what the suite exercises, which is honest but leaves a known, written-down error budget.
* The translation table grows reactively, one suite failure at a time — the reason this keeps being revisited, and the reason the spike exists.
* Two implementations to keep in step; the smoke test is the only cross-check.

### (C) Embed an ECMA-262 engine on the JVM
* The only option that actually ends the divergence list.
* A heavy JVM-only dependency for a cross-platform library, and Scala.js would still take path (B)'s zero-cost route — so the two targets diverge structurally instead of behaviourally.
* Deferred, not rejected. The spike is to establish: whether a maintained ECMA-262 → `java.util.regex` translation exists to adopt instead of hand-rolling; whether an embeddable engine is cheap enough; and whether there is a de-facto compatibility bound other implementations converge on, so the remainder can be declared out of scope rather than discovered one at a time.

### (D) Write an engine
* Total control, total conformance.
* Wildly disproportionate to a validator's regex needs.

## More Information
* `shared/src/main/scala/io/github/jam01/json_schema/vocab/CompiledPattern.scala` — the interface; `matches` is substring/unanchored, matching how `pattern` is specified.
* `jvm/src/main/scala/io/github/jam01/json_schema/vocab/RegexSupport.scala` — the translation table, and an `OPEN` block carrying the spike's two questions.
* `js/src/main/scala/io/github/jam01/json_schema/vocab/RegexSupport.scala` — native `RegExp` under `u`.
* `shared/src/test/scala/io/github/jam01/json_schema/RegexSupportTest.scala` — the translated constructs, plus `format: regex` in both directions.
* `js/src/test/scala/io/github/jam01/json_schema/Smoke.scala` — cross-platform canaries.
* README § Regular expressions — the user-facing error budget.
* Follows the platform-split pattern set by `Idn`; see [decision-006](006-scalajs-testing.md) for how the JS side is exercised at all.
