---
date: 2026-08-12
amended: 2026-08-13
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
* (B) Per-platform: the native `RegExp` on Scala.js, a translation on the JVM.
* (C) Embed an ECMA-262 engine on the JVM — Graal's TRegex, Joni in ECMAScript syntax mode, or Rhino.
* (C′) Adopt an existing ECMA-262 → `java.util.regex` translation rather than writing one.
* (D) Write an ECMA-262 engine.

## Decision Outcome
Chosen option: **(B)**, with (C) and (C′) measured and rejected rather than deferred.

`RegexSupport` follows the pattern already established by `Idn`: a `shared/` interface, `CompiledPattern`, with a `jvm/` and a `js/` implementation.

Scala.js constructs `new js.RegExp(s, "u")` and calls `.test`. The `u` flag is not optional — without it `\p{…}` does not error, it *silently fails to match*, and the suite requires `\p{Letter}` and `\p{digit}` to work. `v` (ES2024) was measured and is strictly more restrictive than `u`, not less.

### Which ECMA-262 — `u` is the dialect (amended 2026-08-13)

ECMA-262 is **two mutually exclusive grammars**, and "the ECMA-262 dialect" does not pick between them. Under `u`, matching is by code point and `\p{…}`/`\u{…}` are syntax; without it, Annex B's legacy spellings are live — octal escapes (`\101`), identity escapes of any character (`\-`, `\ `), a bare `}` or `]` — and property escapes do not exist at all. You cannot have both.

`u` is the one this library targets, on both targets:

* Property escapes are a suite requirement, and they exist only under `u`. That already decided Scala.js; leaving the JVM aimed elsewhere gave it a dialect that was *neither* — `\p{…}` from `u` mode, Annex B leniency from the other, and `\101` read as neither (see below).
* The conformance harness has always compared against Node under `u` and called it the reference, so the baseline was already `u` while the JVM implementation was not.
* Annex B is legacy web compatibility. The suite exercises no octal escape and no backreference, and `u`/`v` are where the language is going.

So `translate` rejects what `u` rejects: legacy octal (`\101`, `\0` followed by a digit), a `\<digits>` naming a group the pattern does not define, an identity escape outside SyntaxCharacter/`/` (`\-`, `\ `, `\%`, and `java.util.regex`'s own `\a`, `\z`, `\Q…\E` and friends), a literal `}` or `]`, and a `{` opening no quantifier.

The octal case is why this matters beyond tidiness. `\101` is octal `A` without `u` and a syntax error with it, but `java.util.regex` reads `\1` as a backreference and spells octal `\0101` — so `^\101$` compiled, referenced a group that does not exist, and **matched nothing at all**. Three engines, three answers, and the JVM's was silent.

The JVM rewrites the pattern in a single escape- and class-aware scan. Scanning rather than replacing is what makes the rewrites safe: each construct is rewritten only where it actually has that meaning, so a literal backslash followed by `p{Letter}` or `cc` stays literal. `translate` covers `$`, `.`, `\v`, `\s`/`\S`, `\p{…}` names, `\c<letter>`, `\0`, `\u{…}`, `[\b]`, `[]`, `[^]`, and `[`/`&&` inside a class; the file's scaladoc carries the per-construct reasoning and README § Regular expressions carries the user-facing budget.

Two consequences of scanning are worth calling out:

* **No `UNICODE_CHARACTER_CLASS`, ever.** The previous compile-plain-then-retry-`(?U)` strategy was not just redundant, it was wrong: the flag applies to the whole pattern, so switching it on to recognize one property name also widened `\d`/`\w`/`\b` elsewhere in that pattern, and in ECMA-262 those are always ASCII-only. `{"pattern": "^\\p{digit}\\d$"}` matched `"4৪"`. Mapping each property name explicitly removes the need for the flag.
* **`java.util.regex`-only constructs are rejected**, not passed through. `\Q…\E`, `\A`, `\z`, `\Z`, `\G`, `\h`, `\H`, `\R`, `\X`, `\a`, `\e`, `\N{…}`, possessive quantifiers and the non-ECMA-262 group forms all raise `PatternSyntaxException`. This is what makes `format: regex` answer for the language `pattern` compiles. It is a behavioral change: a schema relying on Java regex syntax now fails at `validator` construction instead of silently getting Java semantics.

`&&` inside a class is *not* rejected, because `[a&&b]` is valid ECMA-262 — three literal characters. Left alone `java.util.regex` reads it as an intersection and matches nothing, so both `&` and a nested `[` are escaped into literals instead.

### Consequences
* **The JVM target is exact within the portable subset Core § 6.4 recommends** — literals, `[abc]`, `[a-z]`, `[^abc]`, `+ * ?` and lazy forms, `{x}`/`{x,y}`/`{x,}`, `^`, `$`, `(…)`, `|` — and an approximation outside it. That subset is the de-facto interoperability bound; see *More Information*.
* What is left is in README § Regular expressions, and is only what `java.util.regex` cannot express: 38 ECMA-262 binary properties and `\p{Script_Extensions=…}` with no equivalent, non-alphanumeric group names, and forward references to a later group. **All of them are errors; none is a wrong answer.** The forward reference is valid ECMA-262 and is rejected anyway, because the alternative is a pattern that compiles and then cannot match — the harness recorded it as the one remaining `mismatch`, and that category is now empty.
* Near-miss substitutions were deliberately not made. `\p{IsHex_Digit}` and `Script=` exist for `Hex_Digit` and `Script_Extensions=`, and are *different sets* — taking them would trade a visible error for an invisible wrong answer.
* Scala.js is the conformant target, so the parity gap runs the *opposite* way from `Idn`'s. It is documented under § Regular expressions rather than § Scala.js limitations, which is about the JS artifact falling short.
* `u` is *stricter* than the Annex B grammar, so a few legacy-but-tolerated spellings (a bare `}` or `]`, `\-`, `\101`) are a syntax error — on both targets, since the JVM scan now rejects them too. A schema relying on one fails at `validator` construction rather than getting Java's reading of it. `a{,3}` was already rejected by `java.util.regex` and is now rejected by the scan, with the same verdict on both targets.
* The emoji binary properties need a JDK 21 runtime. That is this library's `-release` floor, so they are always available; on an older runtime they would be rejected, not mismatched.
* **The suite is not a safety net here.** Of the divergences enumerated it exercises a handful, and its own `$`-versus-trailing-newline fixture cannot fail on any engine: the data is `"abc\\n"`, a literal backslash followed by `n`. `RegexSupportTest` pins each decision directly, and the `js` smoke test carries the same cases as cross-platform canaries so both targets are checked to agree.

## Pros and Cons of the Options
### (A) One shared implementation
* Simplest; one file, no platform split.
* Wrong on both targets. Scala.js never reaches its own engine, and the JVM's divergences are unaddressed — the `(?U)` retry that was supposed to cover them provably does not.

### (B) Per-platform, translate on the JVM
* Scala.js becomes exactly conformant for free, by deleting code rather than adding it.
* No new dependency on either target, and the fastest of the options measured.
* ~200 lines of scanner to own, and a property-name table that tracks Unicode.
* Two implementations to keep in step; the smoke test is the cross-check.

### (C) Embed an ECMA-262 engine on the JVM
Measured on JDK 25 against Node 22, one process each:

| candidate | closure | conformance | throughput |
| --- | --- | --- | --- |
| `java.util.regex` + (B) | none | 310/320 corpus cases | 2.81 M ops/s |
| Rhino 1.9.1 | 1.6 MiB | 13/14 probes; no binary properties beyond `gc`/`Script` | 0.99 M ops/s |
| Joni, ECMAScript syntax | ~2 MiB | networknt documents known newline and anchor bugs — the two worst divergences here | 1.81 M ops/s (per networknt) |
| GraalJS / TRegex | ~35–50 MiB | exact | 0.36 M ops/s (per networknt) |

* The only option that ends the divergence list outright is GraalJS, at roughly fifty times the artifact's own size and eight times slower.
* Rhino is the interesting one and was close: 1.6 MiB, and correct on `$`/newline, `\p{Letter}`, `\p{digit}`, `\s`/BOM, `\S`/NBSP, `\c`+lowercase, `\v`, `[^]`, `.`/NEL, `\u{…}` and `Script=`. Rejected on three grounds — it is a JVM-only dependency for a cross-platform library, it is 2.8× slower, and its compiled patterns are thread-affine: matching from a thread that has not called `Context.enter()` throws `NullPointerException`, so `CompiledPattern.matches` would need a Context lifecycle it does not have today.
* Joni's ECMAScript dialect is not maintained upstream from Oniguruma, and its documented failure modes are precisely the cases this decision exists to fix.

### (C′) Adopt an existing translation
* One exists and is maintained: Nashorn's `RegExpScanner` (`org.openjdk.nashorn:nashorn-core`, 2.0 MiB). It is a real recursive-descent ECMA-262 parser and it arrives independently at the same `\s`-expansion this implementation uses.
* It is **ES5.1-era and has no `\p{…}` support at all** — under it `\p{Letter}` degrades to the literal `p{Letter}`, regressing the suite cases that motivated this work. It also leaves `$`-versus-trailing-newline unfixed, and its `\S`-inside-a-negated-class rewrite (`&&[…]`) is wrong; measured, it fails 5 of 20 probes. Java supports nested classes, so a nested negated class is correct in both positive and negated classes, and that is what is used here.
* Worth reading as corroboration; not worth depending on.

### (D) Write an engine
* Total control, total conformance.
* Wildly disproportionate to a validator's regex needs.

## More Information
* `shared/src/main/scala/io/github/jam01/json_schema/vocab/CompiledPattern.scala` — the interface; `matches` is substring/unanchored, matching how `pattern` is specified.
* `jvm/src/main/scala/io/github/jam01/json_schema/vocab/RegexSupport.scala` — the scan, the property tables, and the per-construct reasoning.
* `js/src/main/scala/io/github/jam01/json_schema/vocab/RegexSupport.scala` — native `RegExp` under `u`.
* `shared/src/test/scala/io/github/jam01/json_schema/RegexSupportTest.scala` — one test per row of the README table.
* `js/src/test/scala/io/github/jam01/json_schema/Smoke.scala` — the same cases as cross-platform canaries.
* `src/tools/ecma262-regex/` — the harness the numbers above come from: ~4,400 cases run through the compiled `RegexSupport` and compared against Node, with the divergence set checked in as `divergences.tsv` so a change to it has to be reviewed. Not wired into the build; run it after touching the translation.
* README § Regular expressions — the user-facing error budget.
* [Core § 6.4](https://json-schema.org/draft/2020-12/json-schema-core.html) defines the portable subset schema authors should keep to. That subset, not full ECMA-262, is what the ecosystem converges on: spec issue [#816](https://github.com/json-schema-org/json-schema-spec/issues/816) describes the status quo as everyone using their native engine and hoping authors stay inside it, and the suite keeps ECMA-262 regex semantics in `optional/`.
* networknt, the most widely used JVM validator, [defaults to raw `java.util.regex`](https://github.com/networknt/json-schema-validator/blob/master/doc/ecma-262.md) with no translation at all, and says so in its own [compatibility doc](https://github.com/networknt/json-schema-validator/blob/master/doc/compatibility.md). ECMA-262 conformance there is opt-in behind a 50 MiB dependency.
* Follows the platform-split pattern set by `Idn`; see [decision-006](006-scalajs-testing.md) for how the JS side is exercised at all.
