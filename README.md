# json-schema_3 — JSON Schema 2020-12 validator for Scala 3

A push-style validator for JSON-like structures, built on [upickle](https://github.com/com-lihaoyi/upickle)'s
`Visitor` framework. Cross-platform (JVM and Scala.js); validates the instance as it's parsed,
without building an instance AST.

## Install

JVM:
```scala
// sbt
libraryDependencies += "io.github.jam01" %% "json-schema" % "0.2.0"
// Mill
ivy"io.github.jam01::json-schema::0.2.0"
```
```xml
<!-- Maven -->
<dependency>
  <groupId>io.github.jam01</groupId>
  <artifactId>json-schema_3</artifactId>
  <version>0.2.0</version>
</dependency>
```

Scala.js:
```scala
// sbt
libraryDependencies += "io.github.jam01" %%% "json-schema" % "0.2.0"
// Mill
ivy"io.github.jam01::json-schema::0.2.0"
```
```xml
<!-- Maven -->
<dependency>
  <groupId>io.github.jam01</groupId>
  <artifactId>json-schema_sjs1_3</artifactId>
  <version>0.2.0</version>
</dependency>
```

`ujson` is not a direct dependency — bring your own upickle `Transformer`. Examples below use `ujson`.

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

```scala
import io.github.jam01.json_schema.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

final class StartsWith(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]])
    extends VocabBase(schema, ctx, path, dynParent) {

  private val prefix: String = schema.getString("startsWith").get

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val valid = s.toString.startsWith(prefix)
    Seq(mkUnit(valid, "startsWith",
      error = if (valid) null else s"""string does not start with "$prefix""""))
  }

  // Keyword applies only to strings; everything else is a no-op.
  override def visitNull(index: Int):    Seq[OutputUnit] = Nil
  override def visitTrue(index: Int):    Seq[OutputUnit] = Nil
  override def visitFalse(index: Int):   Seq[OutputUnit] = Nil
  override def visitInt64(i: Long, index: Int):   Seq[OutputUnit] = Nil
  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = Nil
  override def visitFloat64StringParts(s: CharSequence, dec: Int, exp: Int, index: Int): Seq[OutputUnit] = Nil
  override def visitArray(length: Int, index: Int):  ArrVisitor[Nothing, Seq[OutputUnit]] = NoArr
  override def visitObject(length: Int, index: Int): ObjVisitor[Nothing, Seq[OutputUnit]] = NoObj

  private val NoArr = new ArrVisitor[Any, Seq[OutputUnit]] {
    def subVisitor: Visitor[?, ?] = NoOpVisitor
    def visitValue(v: Any, index: Int): Unit = ()
    def visitEnd(index: Int): Seq[OutputUnit] = Nil
  }
  private val NoObj = new ObjVisitor[Any, Seq[OutputUnit]] {
    def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    def visitKeyValue(v: Any): Unit = ()
    def subVisitor: Visitor[?, ?] = NoOpVisitor
    def visitValue(v: Any, index: Int): Unit = ()
    def visitEnd(index: Int): Seq[OutputUnit] = Nil
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
