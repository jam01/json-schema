/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import io.github.jam01.json_schema
import org.junit.jupiter.api.{Assertions, Disabled}
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}
import ujson.StringRenderer
import upickle.core.Visitor

import java.nio.file.{Files, Path, Paths}
import scala.util.Using

class TestSuiteTest {
  @ParameterizedTest
  @MethodSource(value = Array("args_provider"))
  def test_suite(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, vis: Visitor[?, OutputUnit]): Unit = {
    val res = try { data.transform(vis) } catch
      case exc: ValidationException => exc.result
    //println(OutputUnitW.transform(res, StringRenderer()).toString)
    Assertions.assertEquals(valid, res.vvalid, path + ": " + desc + ": " + tdesc)
  }

  @ParameterizedTest
  @MethodSource(value = Array("args_provider_format"))
  def optional_format(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, vis: Visitor[?, OutputUnit]): Unit = {
    val res = try { data.transform(vis) } catch
      case exc: ValidationException => exc.result
    //println(OutputUnitW.transform(res, StringRenderer()).toString)
    Assertions.assertEquals(valid, res.vvalid, path + ": " + desc + ": " + tdesc)
  }

  /**
   * Runs the rest of `optional/` (i.e. everything but `optional/format/`, which has its own
   * `args_provider_format`/`optional_format` above). Previously unexercised by `mvn test` at
   * all — `args_provider`'s `Files.walk(..., 1)` only reaches `draft2020-12/` at depth 1, so
   * files nested under `optional/` were silently skipped. That gap is why a broken `\p{Letter}`
   * fix shipped in 0.3.0 undetected: nothing here ran `optional/ecmascript-regex.json`. See
   * `NotSupportedOptional` for files with known, pre-existing (not regressions) failures that
   * are excluded until root-caused separately.
   */
  @ParameterizedTest
  @MethodSource(value = Array("args_provider_optional"))
  def optional_suite(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, vis: Visitor[?, OutputUnit]): Unit = {
    val res = try { data.transform(vis) } catch
      case exc: ValidationException => exc.result
    Assertions.assertEquals(valid, res.vvalid, path + ": " + desc + ": " + tdesc)
  }

  /**
   * Runs every invalid test from the suite under `Detailed` format (`ffast=false`) and asserts the result tree
   * carries diagnostic info: at least one unit in the tree has a non-null `error` string. Default
   * `Flag` + `ffast=true` strips errors entirely, so without this we'd silently accept empty error trees from
   * any vocab.
   */
  @ParameterizedTest
  @MethodSource(value = Array("args_provider_invalid_detailed"))
  def error_shape(path: String, desc: String, tdesc: String, data: ujson.Value, vis: Visitor[?, OutputUnit]): Unit = {
    val res = try { data.transform(vis) } catch
      case exc: ValidationException => exc.result
    val label = path + ": " + desc + ": " + tdesc
    Assertions.assertFalse(res.vvalid, "expected invalid: " + label)
    Assertions.assertTrue(TestSuiteTest.hasError(res), "no error in result for: " + label)
  }
}

object TestSuiteTest {
  val NotSupported: Seq[String] = Seq.empty
  val NotSupportedTests: Seq[String] = Seq.empty
  val NotSupportedFormat: Seq[String] = Seq.empty
  val NotSupportedFormatTests: Seq[String] = Seq("weeks cannot be combined with other units")

  // optional/*.json files (besides optional/format/, covered separately) with known pre-existing
  // failures/errors, not regressions from this change — root-cause deferred, see the handoff plan.
  val NotSupportedOptional: Seq[String] = Seq(
    "dependencies-compatibility.json", // multiple assertion failures
    "bignum.json", // maximum/minimum integer comparison fails for very large/negative numbers
    "format-assertion.json", // custom-metaschema format-assertion:true/false cases fail
    "refOfUnknownKeyword.json", // Obj cannot be cast to Schema when $ref targets a non-schema keyword location
    "cross-draft.json", // harness/registry only carries 2020-12 remotes; can't resolve draft2019-09 cross-refs
  )

  // Individual test-case descriptions (within otherwise-included optional/*.json files) skipped
  // by args_provider_optional only.
  val NotSupportedOptionalTests: Seq[String] = Seq(
    // ecmascript-regex.json's schema uses the plain draft2020-12 $schema, which resolves to
    // Dialect.FullSpec (format is annotation-only there) — so "format": "regex" is never
    // asserted here, unlike optional/format/*.json which args_provider_format forces under
    // Dialect.FormatAssertion. Covered directly (under the right dialect) by
    // RegexSupportTest.format_regex_rejects_java_only_bell_escape.
    "when used as a pattern",
  )

  // Files whose invalid cases don't currently produce an error in the result tree under Detailed format.
  val NotSupportedErrorShape: Seq[String] = Seq.empty

  // Test-case descriptions to skip in `error_shape` only — same shape as NotSupportedErrorShape but
  // for cases that live in a file whose other cases do produce keyword-level errors.
  val NotSupportedErrorShapeTests: Seq[String] = Seq.empty

  // The official test suite expects `remotes/<rel>` to be reachable at `http://localhost:1234/<rel>`.
  // See test-suite/README.md § "Additional Assumptions".
  private val RemotesBase: Uri = Uri("http://localhost:1234/")

  val Registry: MutableRegistry = {
    val builder = new MutableRegistry

    // load remotes — walk recursively, register under their canonical localhost URIs
    val remotesRoot = resource("test-suite/remotes/")
    Using(Files.walk(remotesRoot)) { remotes =>
      remotes.filter(Files.isRegularFile(_))
        .filter(p => p.getFileName.toString.endsWith(".json"))
        .forEach(p => {
          val rel = remotesRoot.relativize(p).toString.replace('\\', '/')
          ujson.read(ujson.Readable.fromPath(p)).transform(SchemaR(RemotesBase.resolve(rel), registry = builder))
        })
    }

    // load meta-schemas — each carries its own absolute `$id`, so docbase URI doesn't matter
    Using(Files.walk(resource("meta/"), 1)) { meta =>
      meta.filter(Files.isRegularFile(_))
        .forEach(p => {
          ujson.read(ujson.Readable.fromPath(p)).transform(SchemaR(Uri("file:" + p.toString), registry = builder))
        })
    }

    builder
  }

  def args_provider: java.util.List[Arguments] = {
    val args = new java.util.ArrayList[Arguments]()
    Using(Files.walk(resource("test-suite/tests/draft2020-12/"), 1)) { tests =>
        tests.filter(Files.isRegularFile(_))
          .filter(p => !NotSupported.contains(p.getFileName.toString))
          //.peek(println)
          .forEach(p => {
            args_provider(p).stream()
              .filter(args => !NotSupportedTests.contains(args.get()(1)))
              .forEach(args0 => args.add(args0))
          })
    }
    //args.addAll(args_provider(resource("test-suite/tests/draft2020-12/defs.json")))

    args
  }

  def args_provider_format: java.util.List[Arguments] = {
    val args = new java.util.ArrayList[Arguments]()
    Using(Files.walk(resource("test-suite/tests/draft2020-12/optional/format/"), 1)) { tests =>
        tests.filter(Files.isRegularFile(_))
          .filter(p => !NotSupportedFormat.contains(p.getFileName.toString))
          //.peek(println)
          .forEach(p => {
            args_provider(p, Dialect.FormatAssertion).stream()
              .filter(args => !NotSupportedFormatTests.contains(args.get()(2)))
              .forEach(args0 => args.add(args0))
          })
    }
    //args.addAll(args_provider(resource("test-suite/tests/draft2020-12/defs.json")))

    args
  }

  def args_provider_optional: java.util.List[Arguments] = {
    val args = new java.util.ArrayList[Arguments]()
    Using(Files.walk(resource("test-suite/tests/draft2020-12/optional/"), 1)) { tests =>
        tests.filter(Files.isRegularFile(_)) // excludes the format/ subdirectory, covered by args_provider_format
          .filter(p => !NotSupportedOptional.contains(p.getFileName.toString))
          .forEach(p => {
            args_provider(p).stream()
              .filter(args => !NotSupportedTests.contains(args.get()(1)))
              .filter(args => !NotSupportedOptionalTests.contains(args.get()(2)))
              .forEach(args0 => args.add(args0))
          })
    }
    args
  }

  def args_provider_invalid_detailed: java.util.List[Arguments] = {
    val args = new java.util.ArrayList[Arguments]()
    Using(Files.walk(resource("test-suite/tests/draft2020-12/"), 1)) { tests =>
      tests.filter(Files.isRegularFile(_))
        .filter(p => !NotSupported.contains(p.getFileName.toString))
        .filter(p => !NotSupportedErrorShape.contains(p.getFileName.toString))
        .forEach(p => {
          args_provider(p, errorShape = true).stream()
            .filter(args => !NotSupportedTests.contains(args.get()(1)))
            .filter(args => !NotSupportedErrorShapeTests.contains(args.get()(1)))
            .filter(args => !args.get()(4).asInstanceOf[java.lang.Boolean])  // invalid cases only
            .forEach(args0 => args.add(Arguments.of(args0.get()(0), args0.get()(1), args0.get()(2), args0.get()(3), args0.get()(5))))
        })
    }
    args
  }

  /** True if any unit in the tree carries a non-null `error` string. */
  private def hasError(u: OutputUnit): Boolean =
    (!u.vvalid && u.error != null) || u.details.exists(hasError)

  def args_provider(path: Path, dial0: Dialect = null, errorShape: Boolean = false): java.util.List[Arguments] = {
    val suite = ujson.read(ujson.Readable.fromPath(path)).arr
    val args = new java.util.ArrayList[Arguments]()

    suite.foreach { testcase =>
      testcase.obj.get("tests").get.arr.foreach(test => {
        val sch = testcase.obj.get("schema").get.transform(SchemaR(registry = Registry))
        val dial = Dialect.tryDialect(sch, registry = Registry).getOrElse(Dialect.Basic)
        val cfg =
          if (errorShape) Config(if (dial0 != null) dial0 else dial, format = OutputFormat.Detailed, ffast = false)
          else Config(if (dial0 != null) dial0 else dial)

        args.add(Arguments.of(
          resource("test-suite/tests/draft2020-12/").relativize(path).toString,
          testcase.obj.get("description").get.str,
          test.obj.get("description").get.str,
          test.obj.get("data").get,
          test.obj.get("valid").get.bool,
          json_schema.validator(sch, cfg, Registry)))
      })
    }

    args
  }

  def resource(s: String): Path = Paths.get(getClass.getClassLoader.getResource(s).toURI)
}
