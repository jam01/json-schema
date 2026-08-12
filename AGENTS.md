# AGENTS.md

Guidance for AI coding agents working in this repository.

## Project

Scala 3 JSON Schema validator (draft 2020-12) built on `upickle.core.Visitor`. Published as `io.github.jam01:json-schema_3` (JVM) and `io.github.jam01:json-schema_sjs1_3` (Scala.js).

## Build & test

This is a Maven build using **Polyglot YAML** (`pom.yaml`, not `pom.xml`) via the `io.takari.polyglot:polyglot-yaml` extension declared in `.mvn/extensions.xml`. Maven auto-loads the extension, so the normal `mvn` CLI works — there is no separate command.

Requires **JDK 25** (CI uses Oracle 25); compiler `release` is `21`. The full test suite lives under `shared/src/test/` and runs in the `jvm` module; the `js` module runs a smaller Node-executed smoke test (see *Scala.js testing* below and `docs/decisions/006-scalajs-testing.md`).

The repo includes the official **JSON Schema Test Suite as a git submodule** at `shared/src/test/resources/test-suite`. Initialize it before running tests:

```bash
git submodule update --init --recursive
mvn -V --no-transfer-progress --batch-mode clean verify
```

Run a single test class or method (JVM module only — `surefire` doesn't run inside the JS module):

```bash
mvn -pl jvm test -Dtest=ObjectSchemaValidatorTest
mvn -pl jvm test -Dtest=TestSuiteTest#optional_format
```

Useful flags:
- `-pl jvm` / `-pl js` — restrict to one module (the root is an aggregator only).
- `-P release` — sources + scaladoc + GPG sign + Sonatype Central Portal publish via `central-publishing-maven-plugin` (used by `.github/workflows/release.yaml`).
- `mvn license:format -pl '.'` — re-apply the Apache-2.0 license header (`src/build/license-header.txt`).

## Architecture

### Pipeline

The library is a **push-style streaming validator**: the JSON instance is pushed through a `Visitor` and validation results stream out, without ever materializing the instance as a full AST. The two public entry points in `shared/.../json_schema/package.scala` are:

- `json_schema.from(reader, readable, ...)` — parses a JSON document into a `Schema` by transforming it through `SchemaR` (the schema reader visitor).
- `json_schema.validator(schema, config, registry)` — returns a `Visitor[?, OutputUnit]` you push the instance through (typically via `ujson.transform`).

`Schema#validate(reader, readable, ...)` wraps both. `SchemaValidator.apply` wraps the validator in a `PointerDelegate` that tracks the instance location during traversal.

### Schema ADT

`Schema.scala` defines the schema ADT and the broader `Value` ADT (`Str`, `Obj`, `Arr`, `Int64`, `Float64`, `Int128`, `Decimal`, `Bool`, `Null`). Numbers are arbitrary precision on both sides of a validation and carry no magnitude or precision bound — `Int128` is a historical name, and `Num.toBigDecimal`/`Value.num` widen whichever case holds a number (see `docs/decisions/008-number-precision.md`). A `Schema` is either:
- `BooleanSchema` (`TrueSchema` / `FalseSchema`), or
- `ObjectSchema` — a JSON object backed by a `collection.Map[String, Value]` plus `docbase: Uri`, `parent: Option[ObjectSchema]`, and `prel` (relative pointer from parent). `ObjectSchema.equals`/`hashCode` deliberately ignore `parent` to avoid cycles, since children reference back to it (see comment in `Schema.scala`).

`ObjSchema.scala` provides accessor helpers (`getMetaSchema`, `getVocabularies`, etc.). Those accessors do **not** parse — they assume `SchemaR` already turned every schema-position child into a `Schema`, which is why `schBy0` compiles a pointer target through `SchemaR` rather than wrapping it (`docs/decisions/010-pointer-into-non-schema.md`). `Uri` and `JsonPointer` are first-class types used pervasively.

### Vocabularies (the keyword implementations)

Validation logic is split into **vocabularies**, each implementing a `Vocab[T]` (which itself is a `JsonVisitor[T, Seq[OutputUnit]]`). Built-in vocabs live in `shared/.../json_schema/vocab/`: `Core`, `Validation`, `Applicator`, `Unevaluated`, `Format`, `FormatAssertion`, `Metadata`, `Content`. Two pieces those vocabs depend on are platform-specific (see `CompiledPattern`/`Idn` in `shared/` for the interfaces they implement): `RegexSupport` (`pattern`/`patternProperties`/`format: regex`) — the JVM version (`jvm/src/main/.../vocab/RegexSupport.scala`) hand-patches `java.util.regex.Pattern`'s divergences from ECMA-262 (`\p{Letter}`-style long-form Unicode aliases, `\s`/`\S`'s whitespace set, `\c<letter>` case sensitivity); the Scala.js version (`js/src/main/.../vocab/RegexSupport.scala`) forwards straight to the native `RegExp` engine under the `u` flag, which needs no patching since it already implements ECMA-262. The JVM side is a deliberate approximation with a written-down error budget — see `docs/decisions/009-ecma-262-regex.md` and README § Regular expressions before touching the translation table. `Idn.scala` (IDN hostname/email): the JVM version (`jvm/src/main/.../vocab/Idn.scala`) wraps `com.networknt`'s RFC 5892 utility for full IDNA 2008 conformance; the Scala.js version (`js/src/main/.../vocab/Idn.scala`) is a best-effort structural validator with documented gaps — no IDNA 2008 character eligibility, no Punycode decoding, no Bidi (see Scala.js limitations below).

### Scala.js limitations

`format: idn-hostname` / `format: idn-email` are validated structurally on Scala.js but not against the IDNA 2008 tables. Documented in README. The Scala.js `Idn.isHostname` enforces label/total length, hyphen placement, and category-based char checks (`Character.isLetterOrDigit` + dots/hyphens). Code paths that need strict conformance must run on JVM.

### Scala.js testing

The `js` module is linked to a single `main.js` and run under Node at the `test` phase. This is a **smoke test only** — the full suite is JVM-only because most tests use `java.nio.file` and JUnit 5 (which doesn't run on Scala.js).

- `src/build/sjsld/` — small Maven module producing `sjsld.jar`, a Scala.js linker driver around `org.scala-js:scalajs-linker_2.13`. Built first in the reactor; uber-jar bundled via `maven-shade-plugin`.
- `js/src/test/scala/.../Smoke.scala` — plain `main` with hand-rolled `check(...)` calls covering platform-divergent code paths (regex, `java.time`, `Idn`) plus a few smoke happy/sad paths. No test framework — JUnit 5 doesn't work on Scala.js and a JS-compatible framework would need its own linker/runner.
- `js/pom.yaml` disables the parent's `shared-test-sources` execution (those tests are JVM-only) and runs `exec-maven-plugin` twice: link → node.
- CI installs Node via `actions/setup-node@v4`.

Anything in shared production code that references a Java 15+ `CharSequence` method, `java.security.SecureRandom`, `java.nio.file`, etc. will fail at the **link** step on JS — so the linker doubles as a static check for accidental JVM-only API use.

A `Dialect` (`Dialect.scala`) bundles a set of `VocabFactory` instances under a meta-schema URI. Three presets: `Dialect.Basic` (no format/metadata/content), `Dialect.FormatAssertion`, `Dialect.FullSpec`. `Dialect.tryDialect` derives a dialect from `$schema` + `$vocabulary` in a meta-schema, falling back to `Basic`. One dialect is resolved for a whole validation run, not per schema resource — a documented spec deviation, see `docs/decisions/011-one-dialect-per-run.md`.

`VocabBase` (extend this for new vocabs) provides `mkUnit`, `accumulate`, `accumulateVec`, and `ffastChild` helpers that respect the configured `OutputFormat`, offer annotations to the `Context`, and short-circuit on `ffast` by throwing `InvalidVectorException`.

### How keyword sub-schemas compose

`SchemaValidator.apply` filters dialect vocabs to those applicable to the schema, then composes them with either `FFastObjectSchemaValidator` (the `ffast` path, which throws `InvalidVectorException` / `ValidationException` to short-circuit) or `MapCompositeVisitor` (full-validation path). Composite visitors fan a single instance node out to N delegates (`CompositeVisitor` / `MapCompositeVisitor` in `Visitors.scala`). This is the pattern used by both multi-vocab dispatch and applicator keywords (`allOf`, `anyOf`, `oneOf`).

### Dynamic dependencies & annotation flow

Some keywords depend on others (e.g. `else` on `if`, `unevaluatedItems` on `items`/`properties`). The chosen strategy (see `docs/decisions/002-dynamic-deps.md`, `003-annotation-dyn-deps.md`, `004-invalid-dyn-deps.md`) is **always compute, then resolve via annotations**:

- Each vocab `offerAnnotation(loc, value)` through the `Context` when a keyword produces an annotation.
- Dependent keywords call `ctx.registerDependant(schLocation, kwLocation, predicate)` up front; `getDependenciesFor(kwLocation)` later returns the matching annotations.
- `Context.notifyInvalid(...)` (and `onVocabResults` / `onScopeEnd` in `ContextExtension`) prune dependencies that came from branches later found invalid (e.g. discarded `if`/`then`/`else` paths).
- `DefaultContext` in `Context.scala` is the only `Context` implementation; treat the `ContextExtension` API (`ext.onVocabResults`, `ext.onScopeEnd`) as **internal**, called from `VocabBase`/`SchemaValidator`, not from vocab keyword code.

### Output and configuration

- `OutputFormat` (in `OutputUnit.scala`) controls result shape: `Flag` (single bool), `Basic` (flat keyword list under one root), `Detailed` (hierarchical, errors + annotated successes), `Verbose` (hierarchical, retains everything). The format also controls whether successful units are accumulated (annotation propagation) or dropped — Verbose overrides `retainsValidUnannotated = true`.
- `Config` bundles `dialect`, `format`, `ffast` (default `true`), `allowList` (annotation filter, default `DropAll`), `maxDepth` (default `32`, guards infinite `$ref` recursion — see `SchemaValidator.guardDepth`).
- `Registry` looks up schemas by `Uri`. Used to resolve `$ref` / `$dynamicRef`, meta-schemas, and remotes. `DefaultContext.getDynSch` walks the dynamic scope by climbing `Vocab.dynParent`.

### Module layout summary

- `pom.yaml` (root, aggregator) → modules `jvm`, `src/build/sjsld`, `js` (in reactor order; sjsld must build before `js` consumes it).
- `src/build/pom.yaml` — the **parent POM** for `jvm` and `js` (compiler config, scala-maven-plugin, license header, surefire, flatten). `sjsld` is standalone, does not inherit.
- `shared/src/main/scala` — added as an extra source root to both modules via `build-helper-maven-plugin`. Likewise `shared/src/test/scala` and `shared/src/test/resources`.
- `jvm/src/main/scala` / `js/src/main/scala` — platform-specific overrides (`vocab/Idn.scala`, `vocab/RegexSupport.scala`).

## Checking bowtie.report failures

[bowtie.report](https://bowtie.report/#/implementations/scala-json-schema) tracks this implementation (id `scala-json-schema`) against the official test suite. The page is a client-rendered SPA — fetching the URL directly only returns the empty shell. The underlying data is plain NDJSON, fetched per dialect:

```bash
curl -s https://bowtie.report/draft2020-12.json -o /tmp/draft2020-12.json
```

Format: line 0 is `{"implementations": {...}, ...}` (metadata, keyed by impl id). Every other line is either a test case (`{"seq", "case": {"description", "schema", "tests": [{"description", "instance", "valid"}, ...]}}`) or a per-implementation result (`{"seq", "implementation", "expected": [...], "results": [{"valid": ...}, ...]}`, one entry per test aligned by index) — join case and result lines on `seq`, then filter results on `implementation == "scala-json-schema"` and compare `expected` vs `results[i].valid` to find failures.

**Gotcha when reproducing a failure locally**: `TestSuiteTest` reads each fixture file once into a `Value` tree (via `LiteralVisitor`, which preserves number representation) and replays extracted `schema`/`data` sub-nodes through `SchemaW`. That replay is value-lossless, but it does not reproduce *which* visitor callback a real parse would invoke — e.g. a `Float64` replays through `visitFloat64`, where `ujson`'s parser would have called `visitFloat64StringParts`. To faithfully reproduce a reported failure, drive the schema/instance through `ujson.Readable.fromString(rawJsonText).transform(...)` directly instead — see `EnumConstEqualityTest.scala` for the pattern. (Before `f5582e8` the loader used `ujson.read(...)`, whose `Double`-only AST silently collapsed number representation; that is fixed, but the direct-parse advice still stands.)

## Conventions worth knowing

- All source files carry the Apache-2.0 header from `src/build/license-header.txt`; `mvn license:format -pl '.'` rewrites them. CI fails on missing headers (`license:check` in the root `pom.yaml`).
- `Schema` and `Value` are `sealed` ADTs in a single file — see comment block in `Schema.scala` linking to discussions on multi-file ADTs. Keep new cases co-located.
- The `equals`/`hashCode` of `ObjectSchema` intentionally ignores `parent` to avoid cycles. Don't "fix" this without reading the rationale.
- `package.scala` is the user-facing API surface. Keep public-API additions there.
- Tests rely on the submodule being initialized; CI does this explicitly.
