---
date: 2026-05-12
---
# How to validate the Scala.js artifact at runtime?
## Context and Problem Statement
Per [decision-005](005-build-tool.md), Maven was chosen for the JVM and Scala.js artifacts knowing there was "no obvious way to test Scala.js running in a JavaScript environment". The consequence was real: the `js` module compiled to `.sjsir` and surefire (which scans `.class` and JUnit annotations) silently ran zero tests. CI green meant "JS sources compile"; nothing exercised the produced code under a JS engine. Platform-specific divergence — `java.util.regex.Pattern` semantics, `scala-java-time`, the platform-specific `Idn` implementation — could regress unnoticed.

How do we get _some_ runtime signal on the Scala.js artifact within the existing Maven build, without bringing in sbt for tests?

## Decision Drivers
* Catch link-time failures from accidental JVM-only API use in `shared/` sources
* Catch obvious runtime divergence on the JS engine (regex, `java.time`, `Idn`)
* Stay within Maven; do not bifurcate the build into sbt-for-tests + Maven-for-publish
* Minimize new moving parts and external dependencies
* Keep CI runtime cheap

## Considered Options
* Adopt sbt (or Mill) for tests only, keep Maven for publish
* `scalajs-maven-plugin` (third-party) + a Scala.js test framework with a Node runner
* In-house Scala.js linker driver + plain `main` smoke test under Node
* Skip JS testing entirely; rely on JVM tests

## Decision Outcome
Chosen options: "In-house Scala.js linker driver" + "plain `main` smoke test under Node".

A small Maven module (`src/build/sjsld/`) packages a shaded uber-jar around `org.scala-js:scalajs-linker_2.13`. The `js` module dumps its test-runtime classpath, then `exec-maven-plugin` runs `java -jar sjsld.jar` to link a single `main.js` from `.sjsir` on that classpath, and runs it under Node. The smoke test (`js/src/test/scala/.../Smoke.scala`) is a plain object with a `main` method and hand-rolled `check(...)` assertions; it targets platform-divergent code paths plus a few happy/sad spot checks.

### Consequences
* Runtime validation of the Scala.js artifact in CI, every push.
* The link step doubles as a static check: any shared production code that references a JS-unsupported API (e.g. `java.security.SecureRandom`, `java.nio.file`, Java 15+ `CharSequence` methods) fails the build at link.
* Smoke test is intentionally minimal — broader behavior remains JVM-only. Most existing tests rely on `java.nio.file` / JUnit 5 (no JS-compatible runner) and so cannot move to JS without rewriting.
* No test framework on JS — using one would require its own JS-compatible runner and linker integration, dwarfing the smoke harness itself.
* New build artifact (`sjsld.jar`) must be installed into the reactor before the `js` module's `test` phase. Aggregator orders modules `sjsld → jvm → js`.
* CI gains a Node dependency (`actions/setup-node`).
* Pure Maven; no sbt.

## Pros and Cons of the Options
### Adopt sbt (or Mill) for tests only
* Idiomatic for the Scala/Scala.js ecosystem; canonical test framework support out of the box.
* Splits the build into two tools; contributors need to learn both.
* Conflicts with [decision-005](005-build-tool.md)'s "quickest path to distribution" driver.

### `scalajs-maven-plugin` + JS test framework
* Less code than rolling our own linker driver.
* The plugin is third-party and not officially supported by the Scala.js team.
* Still requires a JS-compatible test framework with a Node runner; framework + linker integration is more weight than a smoke harness needs.

### In-house linker driver + plain main smoke test
* ~80 lines of Scala wrapping `StandardImpl.linker`; uses only public Scala.js linker APIs.
* No test framework; assertions are a `check(cond, msg)` helper.
* Linker driver is reusable for any future "link this Scala.js main and run it" need.
* Forfeits a real test framework's ergonomics (per-test reporting, fixtures, `@BeforeAll`); acceptable at the current scope.

### Skip JS testing
* Status quo before this decision.
* Silent regressions on JS, including outright incorrect behavior (the `Idn` stub previously returned `true` for any string).

## More Information
* `src/build/sjsld/src/main/scala/io/github/jam01/json_schema/build/Sjsld.scala` — the linker driver. Args: `<outDir> <mainClass> <ir-path|@classpath-file>...`. `ModuleKind.NoModule` so Node can execute `main.js` directly.
* `js/src/test/scala/io/github/jam01/json_schema/Smoke.scala` — the smoke test entry point.
* `js/pom.yaml` disables the parent's `shared-test-sources` execution (those tests are JVM-only) and runs `exec-maven-plugin` twice in the `test` phase: link → node.
* The `org.scala-js:scalajs-test-bridge_2.13` dep declared in `js/pom.yaml` is dead weight under this design; it was originally there in anticipation of a real test runner. Kept for now until JS testing scope expands or contracts.
