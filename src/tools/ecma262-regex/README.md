# ECMA-262 conformance harness

JSON Schema specifies `pattern`, `patternProperties` and `format: regex` against ECMA-262.
Scala.js hands those to the platform's own `RegExp` and is exact; the JVM has no ECMA-262 engine,
so `jvm/…/vocab/RegexSupport.scala` rewrites patterns into `java.util.regex` before compiling.

This measures that rewrite against a real ECMA-262 engine, and fails when the set of known
divergences moves. It is the breadth check behind the error budget in README § Regular
expressions; `RegexSupportTest` pins the individual decisions. See
[decision-009](../../../docs/decisions/009-ecma-262-regex.md) for why translation was chosen over
embedding an engine.

```bash
mvn -pl jvm compile                          # RunShipped reads the compiled classes
python3 src/tools/ecma262-regex/check.py
```

Not part of the build — nothing in CI runs it. Run it after touching `RegexSupport`.

## What it does

`corpus.py` generates ~4,400 cases covering the portable subset Core § 6.4 recommends, anchors,
escapes, character-class context, `java.util.regex`-only constructs, legacy spellings, groups and
backreferences, astral code points, every ECMA-262 property escape spelling, and every shorthand
class swept over the characters where the two dialects could disagree.

Patterns and inputs are carried as space-separated UTF-16 code units in hex, so Python, Node and
the JVM decode them identically and no escaping convention has to survive three languages.

`check.py` runs the corpus through the **compiled** `RegexSupport` — via reflection, so it
measures what ships rather than a copy — and compares against two checked-in baselines:

| file | what it holds |
| --- | --- |
| `reference.tsv` | ECMA-262 truth: Node's `RegExp` under the `u` flag, per case |
| `divergences.tsv` | every case where this target knowingly differs, with the reason |

It also asserts that `format: regex` and `pattern` agree — a string must be valid `format: regex`
exactly when `pattern` compiles it.

## Reading divergences.tsv

One line per divergent pattern, except mismatches, which are per case:

- **rejected** — a valid ECMA-262 pattern `java.util.regex` cannot express. An error, not a wrong
  answer, and `format: regex` reports it invalid too. Mostly the ECMA-262 binary properties with
  no Java equivalent.
- **accepted** — a pattern ECMA-262 rejects under `u` that this target takes, with the meaning
  ECMA-262 gives it without `u`.
- **mismatch** — both engines compile it and they match differently. The only silent kind, and
  the one to look hardest at.

Every line should be reflected in README § Regular expressions. When a change is intended:

```bash
python3 src/tools/ecma262-regex/check.py --refresh   # needs node; rewrites both baselines
```

then review the diff and update the README table to match.

## Requirements

JDK for `RunShipped` (single-file source launch, no build step) and the Scala library jars, which
`check.py` finds in `~/.m2` after any build. Node is needed only for `--refresh`; it is already a
build prerequisite, since the `js` module links with sjsld and runs its smoke test under Node.
