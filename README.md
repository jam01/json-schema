# json-schema_3 — JSON Schema 2020-12 validator for Scala 3

A push-style validator for JSON-like structures, built on [upickle](https://github.com/com-lihaoyi/upickle)'s
`Visitor` framework. Cross-platform (JVM and Scala.js); validates the instance as it's parsed,
without building an instance AST. Passes the official
[JSON Schema Test Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite) for draft
2020-12 end-to-end (mandatory + optional `format`), with regression assertions on output shape.

## Install

JVM:
```scala
// sbt
libraryDependencies += "io.github.jam01" %% "json-schema" % "0.3.0"
// Mill
ivy"io.github.jam01::json-schema::0.3.0"
```
```xml
<!-- Maven -->
<dependency>
  <groupId>io.github.jam01</groupId>
  <artifactId>json-schema_3</artifactId>
  <version>0.3.0</version>
</dependency>
```

Scala.js:
```scala
// sbt
libraryDependencies += "io.github.jam01" %%% "json-schema" % "0.3.0"
// Mill
ivy"io.github.jam01::json-schema::0.3.0"
```
```xml
<!-- Maven -->
<dependency>
  <groupId>io.github.jam01</groupId>
  <artifactId>json-schema_sjs1_3</artifactId>
  <version>0.3.0</version>
</dependency>
```

`ujson` is not a direct dependency — bring your own upickle `Transformer`. Examples below use `ujson`.

## How it compares

The Scala 3 / JVM + Scala.js story is the obvious uniqueness, but the design choices below are
worth considering against any modern validator:

| Capability                                            | this lib                       | networknt (JVM) | ajv (JS)                | jsonschema-rs (Rust) | python-jsonschema |
|-------------------------------------------------------|--------------------------------|-----------------|-------------------------|----------------------|-------------------|
| Draft 2020-12, official test suite                    | ✅                              | ✅              | ✅                      | ✅                   | ✅                |
| All four spec output formats                          | ✅ Flag / Basic / Detailed / Verbose | partial         | own format (not spec-aligned) | partial              | partial           |
| Annotations as first-class output + dependency API    | ✅ AllowList, `findAnnotatingUnits`, vocab-level | partial         | not in standard output  | partial              | partial           |
| Correct `unevaluated*` across `$ref` / applicators / `if`/`then`/`else` | ✅          | ✅              | ✅                      | ✅                   | partial           |
| Push-style streaming (no instance AST built)          | ✅                              | ❌ (Jackson tree) | ❌                      | ❌ (`serde_json::Value`) | ❌            |
| Cross-platform native (single library, multiple targets) | ✅ JVM + Scala.js           | ❌ JVM only     | ❌ JS only              | ❌ Rust only         | ❌ Python only    |
| Vocabulary extension at the spec-vocabulary level     | ✅ public `Vocab`/`VocabFactory` | keyword-level   | keyword-level (different model) | keyword-level | ❌                |
| Scala 3 native                                        | ✅                              | n/a             | n/a                     | n/a                  | n/a               |

What this lib does **not** claim against the field: adoption (networknt and ajv are widely
deployed); measured throughput (jsonschema-rs and pre-compiled ajv are very fast; this library
has no published benchmarks yet); and breadth of supported drafts (2020-12 only — most listed
alternatives support draft-04 through 2020-12).

Where it lands: if you want a Scala 3 native validator that runs on both JVM and the browser,
treats annotations as part of the contract, and lets you stream large instances or plug in your
own vocabularies — this is the most aligned option available.

## Quick start

```scala
import io.github.jam01.json_schema.{Schema, OutputUnit}
import io.github.jam01.json_schema as js

val sch: Schema = js.from(ujson.Readable, ujson.Readable.fromString("""{"type":"string"}"""))
val r:  OutputUnit = sch.validate(ujson.Readable, ujson.Readable.fromString(""""hello""""))

assert(r.vvalid)
```

`sch.validate` builds a one-shot validator and applies it. For repeated validation against the
same schema, build the validator once (next section).

## Reusable validator

```scala
import io.github.jam01.json_schema.{Schema, OutputUnit, Config}
import io.github.jam01.json_schema as js
import upickle.core.Visitor

val sch: Schema = js.from(ujson.Readable, ujson.Readable.fromString("""{"type":"string"}"""))
val v:   Visitor[?, OutputUnit] = js.validator(sch)

val r1 = ujson.Str("foo").transform(v)
val r2 = ujson.Str("bar").transform(v)
```

The returned visitor is **not thread-safe**, but **is safe for repeated sequential
`.transform(...)` calls** — including after a `ValidationException` (the per-traversal state
resets at the end of each root scope).

Under the default `Config(ffast = true)`, a failed validation throws `ValidationException`;
the wrapped `OutputUnit` is on `e.result`. Disable by passing `Config(ffast = false)` to get the
unit back through the normal return.

## `$ref` and the registry

Schemas referenced via `$ref` are resolved through a `Registry`. To validate a schema that
references others, populate one `MutableRegistry` with every schema and pass it to both `from`
and the validator:

```scala
import io.github.jam01.json_schema.{MutableRegistry, Config}
import io.github.jam01.json_schema as js

val reg = new MutableRegistry
val userSch  = js.from(ujson.Readable, ujson.Readable.fromString(userSchemaJson),  registry = reg)
val orderSch = js.from(ujson.Readable, ujson.Readable.fromString(orderSchemaJson), registry = reg)

val v = js.validator(orderSch, Config.Default, registry = reg)
```

`Registry` is read-only; `MutableRegistry extends Registry`. Pre-populate it once, share it
across validators.

**One dialect per validation run.** The dialect is fixed for the whole run: either the one in
`Config`, or — with `Config(resolveDialect = true)` — the one resolved from the *root* schema's
`$schema`. Per JSON Schema Core § Schema References, a referenced schema resource's dialect is
determined by that resource's own `$schema`, not inherited from whoever referenced it, so a
`$ref` that crosses into a resource with a different `$schema` is still evaluated under the
referrer's dialect here. In practice this only shows up when a `$ref`'d resource declares a
different `$vocabulary` — for example one that turns format assertion on while the referrer has it
off. Referencing a resource written for an *earlier draft* is a separate matter and is out of
scope regardless: only 2020-12 keyword semantics are implemented.

## Output formats

Selected via `Config(format = …)`. Mirrors the four formats described in
[JSON Schema 2020-12 §12.4](https://json-schema.org/draft/2020-12/json-schema-core#section-12.4).

| Format     | Shape                                                             |
|------------|-------------------------------------------------------------------|
| `Flag`     | Single root unit with `valid` only. Cheapest. **Default.**        |
| `Basic`    | Single root unit; `details` is a *flat* list of keyword-level units. |
| `Detailed` | Hierarchical, retains only error units and annotated successes.   |
| `Verbose`  | Hierarchical, retains every unit.                                 |

Example. Schema `{"properties":{"name":{"type":"string","minLength":3}}}` against
`{"name":"ab"}`, rendered with `OutputUnitW`:

`Flag`:
```json
{ "valid": false, "keywordLocation": "", "instanceLocation": "" }
```

`Detailed`:
```json
{
  "valid": false, "keywordLocation": "", "instanceLocation": "",
  "details": [{
    "valid": false, "keywordLocation": "/properties", "instanceLocation": "",
    "details": [{
      "valid": false, "keywordLocation": "/properties/name", "instanceLocation": "/name",
      "details": [{
        "valid": false, "keywordLocation": "/properties/name/minLength", "instanceLocation": "/name",
        "error": "String length 2 is less than minimum 3"
      }]
    }]
  }]
}
```

`Basic` produces the same set of keyword units but flat at the root.
`Verbose` additionally retains every successful unit. Use `OutputUnitW.transform(unit, ujson.StringRenderer())`
to serialize.

## Annotations

Beyond pass/fail, JSON Schema defines an **annotation** mechanism: keywords that succeed can attach a value to
their evaluation site, observable downstream by the user *and* by sibling keywords. This is what
makes `unevaluatedItems` / `unevaluatedProperties` correct in the first place, and what powers things like
form generation, doc rendering, or routing decisions driven by schema metadata.

This library treats annotations as first-class. Specifically:

- **Spec-compliant emission across all four output formats** — `Detailed` retains successful units that
  carry annotations; `Verbose` retains every successful unit; `Basic` flattens annotated units at the root;
  `Flag` drops them. See [JSON Schema 2020-12 §7.7](https://json-schema.org/draft/2020-12/json-schema-core#section-7.7).
- **Correct cross-keyword propagation** — `unevaluatedItems` and `unevaluatedProperties` see the
  annotations produced by sibling `properties` / `patternProperties` / `additionalProperties` /
  `prefixItems` / `items` / `contains`, including across `$ref`, `allOf`/`anyOf`/`oneOf`, and
  `if`/`then`/`else` branches.
- **Pruning on invalidated branches** — when a `oneOf` arm fails, an `if` selects the other branch,
  or an applicator otherwise discards a result, the annotations produced by the discarded subtree
  are pruned before downstream keywords consume them. This is the kind of thing many implementations
  get wrong.
- **`AllowList` for tuning what lands in the output** — `Config(allowList = …)`. Choices: `KeepAll`,
  `DropAll` (default), `Keep(Set[String])`, `Drop(Set[String])`. Useful when you want, say, `title` /
  `description` / `x-*` for downstream tooling but not the `prefixItems` index annotations.
- **`OutputUnit.findAnnotatingUnits(insLoc, keyword)`** for extracting annotations from a result tree
  without re-traversing it yourself.

Custom vocabularies plug into the same mechanism: emit an annotation via `mkUnit(..., annotation = …)`,
and declare a dependency on others via `ctx.registerDependant(…)` + `ctx.getDependenciesFor(…)`. See
the `Unevaluated` vocab for the canonical pattern.

## Format assertion

`format` is annotation-only by default. To make it assert, opt in via `Dialect.FormatAssertion`:

```scala
import io.github.jam01.json_schema.{Config, Dialect}
import io.github.jam01.json_schema as js

val v = js.validator(sch, Config(dialect = Dialect.FormatAssertion))
```

Asserted formats: `date-time`, `date`, `time`, `duration`, `email`, `idn-email`, `hostname`,
`idn-hostname`, `ipv4`, `ipv6`, `uuid`, `uri`, `uri-reference`, `iri`, `iri-reference`,
`uri-template`, `json-pointer`, `relative-json-pointer`, `regex`.

Known limitations:
- `duration` does not reject `P…W` combined with non-week units (the ISO 8601 ambiguity is left
  to upstream `java.time`).
- `idn-hostname` / `idn-email` are fully compliant on JVM but only best-effort on Scala.js — see *Scala.js limitations* below.
- `regex` rejects a few patterns that are valid ECMA-262 but have no `java.util.regex`
  equivalent — see *Regular expressions* below.

## Regular expressions

`pattern`, `patternProperties` and `format: regex` are specified against **ECMA-262** regular
expressions. Scala.js hands them to the platform's own `RegExp`, so they are exact there. The JVM
has no ECMA-262 engine available, so patterns are rewritten into `java.util.regex` before
compiling.

**Within the portable subset the spec recommends** — literal characters, `[abc]`, `[a-z]`,
`[^abc]`, `+ * ?` and their lazy forms, `{x}`, `{x,y}`, `{x,}`, `^`, `$`, `(…)` and `|` — the two
targets behave identically. Beyond it, the JVM target is an approximation, and this is its budget.

Rewritten, so they mean what ECMA-262 says:

| construct | would otherwise be |
| --- | --- |
| `$` | end of *line*: `^abc$` matching `"abc\n"` |
| `.` | also excluding U+0085 NEXT LINE |
| `\v` | any vertical whitespace, including `\n` |
| `\s`, `\S`, including inside `[…]` | a narrower set: no U+FEFF, and `\S` over-matching NBSP |
| `\p{…}` long-form category names, `\p{General_Category=…}`, 15 binary properties | unknown, or — for `\p{Alpha}`, `\p{Lower}`, `\p{Upper}`, `\p{space}` — silently ASCII-only |
| `\c` + lowercase letter | a different control code |
| `\0`, `\u{…}`, `[\b]`, `[]`, `[^]` | a syntax error |
| `[` and `&&` inside `[…]` | a nested class and a class intersection |

`\d`, `\w` and `\b` stay ASCII-only as ECMA-262 requires, including in patterns that also use a
Unicode property.

Not rewritten — the remaining differences, all JVM-only:

| construct | ECMA-262 | this library on JVM |
| --- | --- | --- |
| `\p{Math}`, `\p{ID_Start}`, `\p{Dash}` and 35 other binary properties | valid | rejected — `java.util.regex` has no equivalent, and the near misses are different sets |
| `\p{Script_Extensions=…}` | valid | rejected — `Script=` of the same name is a different set |
| a group name that is not alphanumeric, e.g. `(?<$x>a)` | valid | rejected by `java.util.regex` |
| a forward reference to a later group, e.g. `(\2)(a)` | matches empty | does not match |
| a bare `}` or `]`, `\-`, `\ `, `a{,3}`, `\101`, `[a-z&&[b]]` | a syntax error under `u` | accepted, with the meaning ECMA-262 gives them without `u` |

The first three are errors rather than wrong answers: a rejected pattern throws from `validator`,
and `format: regex` reports it invalid. Only the forward reference is a silent wrong match.
`format: regex` and `pattern` always agree — a string is valid `format: regex` exactly when
`pattern` will compile it.

The last row is the one place the JVM accepts *more* than ECMA-262: Scala.js compiles patterns
in Unicode mode, which is stricter than ECMA-262 without it, so those spellings are a syntax
error there while the JVM takes them.

Constructs that are `java.util.regex` and not ECMA-262 — `\Q…\E`, `\A`, `\z`, `\Z`, `\G`, `\h`,
`\R`, `\X`, `\a`, `\e`, `\N{…}`, `a*+`, `(?i)`, `(?>…)`, `\p{Is…}` — are rejected on both targets.

## Numbers

Numbers are arbitrary precision, on both sides of a validation. A JSON number literal — in a schema
or in an instance — is parsed to the narrowest representation that holds it exactly:

| literal | `Value` case | backed by |
| --- | --- | --- |
| an integer that fits `Long` | `Int64` | `Long` |
| a decimal of ≤ 15 significant digits | `Float64` | `Double` |
| a wider integer | `Int128` | `BigInt` |
| a finer decimal | `Decimal` | `BigDecimal` |

`Int128` is a historical name — it is not capped at 128 bits. Every comparison
(`maximum`, `minimum`, `exclusive*`, `const`, `enum`, `uniqueItems`, `multipleOf`) is exact at any
width, and mixes widths freely, so `{"minimum": 1.5}` gives the right answer against a 500-digit
integer. This goes beyond what the spec requires — `optional/bignum.json` in the official suite is
optional precisely because rounding to a double is a conforming choice — so a schema written for
this library may not port to a validator that rounds.

The 15-significant-digit cutoff is where `Double` stops round-tripping decimals exactly. Below it
you get the `Double` fast path; above it, exactness. `1e400` still overflows to `Float64(Infinity)`,
since its mantissa is one digit.

**Cost, if you validate untrusted input.** Arbitrary precision means the work scales with the
*length of the JSON text*, not with the value's magnitude — exponent notation is free, because
`BigDecimal` keeps the scale separate from the significand. `{"multipleOf": 7}` against
`1234567890123456789e100000000` (a 30-byte literal) returns in well under a millisecond, while the
same keyword against a literal 300,000 digits long takes ~1.4s. So the only lever is payload size,
and the mitigation is the ordinary one: bound the request body. There is no `Config` knob for this
today — if you want one, open an issue and say what shape it should take.

## Streaming

The validator pushes the instance through a `Visitor` without ever building an instance-side AST.
Any upickle `Transformer`/`Readable` source works — including a `java.io.InputStream`:

```scala
import io.github.jam01.json_schema as js
import java.nio.file.{Files, Paths}

val v  = js.validator(sch)
val in = Files.newInputStream(Paths.get("large.json"))
try ujson.InputStreamParser.transform(in, v)
finally in.close()
```

Under `ffast = true` (default), an invalid path short-circuits as soon as the failing element is
seen; the parser does not pull further bytes from the source. (See
`shared/src/test/scala/.../StreamingTest.scala` for the regression that proves it.)

The same applies to any non-streaming `Readable` — there is no intermediate reification just for
validation; only a handful of keywords (`contains`, `const`, `enum`) buffer locally and only as
much as the keyword needs.

## Custom vocabularies

A vocabulary is a set of keywords with a single `VocabFactory` companion. To add one: extend
`VocabBase`, override the `visit*` methods for the JSON node types your keyword applies to,
declare the factory, and put it in a `Dialect`.

`VocabBase` provides `Nil`-returning defaults for every `visit*` method, so a string-only
keyword needs to override only `visitString`:

```scala
import io.github.jam01.json_schema.*

final class StartsWith(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]])
    extends VocabBase(schema, ctx, path, dynParent) {

  private val prefix: String = schema.getString("startsWith").get

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val valid = s.toString.startsWith(prefix)
    Seq(mkUnit(valid, "startsWith",
      error = if (valid) null else s"""string does not start with "$prefix""""))
  }
}

object StartsWith extends VocabFactory[StartsWith] {
  override def uri: String = "https://example.com/vocab/starts-with"
  override def shouldApply(schema: ObjectSchema): Boolean = schema.value.contains("startsWith")
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]) =
    new StartsWith(schema, ctx, path, dynParent)
}
```

Wire it into a dialect and validate:

```scala
import io.github.jam01.json_schema.{Config, Dialect, Uri}
import io.github.jam01.json_schema as js

val customDialect = Dialect(
  Uri("https://example.com/dialect"),
  Dialect.FullSpec.vocabularies :+ StartsWith)

val sch = js.from(ujson.Readable, ujson.Readable.fromString("""{"startsWith":"user-"}"""))
val r   = ujson.Str("user-42").transform(js.validator(sch, Config(dialect = customDialect)))
assert(r.vvalid)
```

Tips when writing a vocab:
- The built-in `vocab/Format.scala` and `vocab/Metadata.scala` are the smallest reference
  implementations and useful as templates.
- Use `mkUnit` for one-shot units and `accumulate(buff, …)` when emitting several from the same
  scope; both already respect the configured `OutputFormat` and `AllowList`.
- Annotation-dependent keywords (think `unevaluatedItems`) coordinate through the `Context` —
  call `ctx.registerDependant(…)` in the constructor and `ctx.getDependenciesFor(…)` at
  `visitEnd`. See `vocab/Unevaluated.scala` for the pattern.

## Scala.js limitations

The `_sjs1_3` artifact aims for feature parity with the JVM artifact, with one documented
exception:

- **`format: idn-hostname` and `format: idn-email`** are validated *structurally* on Scala.js
  (label length, character category, hyphen placement, total length) but not against the full
  IDNA 2008 / RFC 5892 tables, Punycode (`xn--…`) decoding, or RFC 5893 Bidi rules. The JVM
  target uses `com.networknt`'s RFC 5892 implementation for full conformance. If you need that,
  validate on the JVM. See `js/src/main/scala/.../vocab/Idn.scala` for the precise list of
  checks performed.

## Status

- Implements [JSON Schema 2020-12](https://json-schema.org/draft/2020-12). Earlier drafts are
  not supported.
- Pre-1.0 — the user-facing API may change between minor versions.
- The full JSON Schema Test Suite for draft 2020-12 (mandatory + optional `format`) runs in CI.

## License

[Apache-2.0](LICENSE).
