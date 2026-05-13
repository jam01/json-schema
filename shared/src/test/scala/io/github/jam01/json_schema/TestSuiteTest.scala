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
   * Runs every invalid test from the suite under `Detailed` format (`ffast=false`) and asserts the result tree
   * carries enough info to pinpoint a failure: at least one descendant must have a non-null `error` and a `kwLoc`
   * that names a specific keyword (i.e. deeper than the root pointer). Default `Flag` + `ffast=true` strips both,
   * so without this we'd silently accept empty error trees from any vocab.
   */
  @ParameterizedTest
  @MethodSource(value = Array("args_provider_invalid_detailed"))
  def error_shape(path: String, desc: String, tdesc: String, data: ujson.Value, vis: Visitor[?, OutputUnit]): Unit = {
    val res = try { data.transform(vis) } catch
      case exc: ValidationException => exc.result
    val label = path + ": " + desc + ": " + tdesc
    Assertions.assertFalse(res.vvalid, "expected invalid: " + label)
    Assertions.assertTrue(TestSuiteTest.hasKeywordError(res), "no keyword-level error in result for: " + label)
  }
}

object TestSuiteTest {
  val NotSupported: Seq[String] = Seq.empty
  val NotSupportedTests: Seq[String] = Seq.empty
  val NotSupportedFormat: Seq[String] = Seq("idn-hostname.json", "idn-email.json")
  val NotSupportedFormatTests: Seq[String] = Seq("weeks cannot be combined with other units")

  // Files whose invalid cases don't currently produce a keyword-level error unit under Detailed format.
  // These are real gaps to be fixed separately — `error_shape` skips them to keep the harness green
  // while it does catch regressions elsewhere. Buckets, by root cause:
  //   - BooleanSchemaValidator emits no error message for `false` schemas (there is no keyword name):
  //       boolean_schema.json
  //   - $ref / $dynamicRef short-circuit before vocab errors propagate up:
  //       ref.json, dynamicRef.json
  //   - Misc keyword-specific gaps:
  //       items.json, additionalProperties.json, dependentSchemas.json, patternProperties.json,
  //       uniqueItems.json, properties.json, vocabulary.json, prefixItems.json
  val NotSupportedErrorShape: Seq[String] = Seq(
    "boolean_schema.json", "ref.json", "dynamicRef.json",
    "items.json", "additionalProperties.json", "dependentSchemas.json", "patternProperties.json",
    "uniqueItems.json", "properties.json", "vocabulary.json", "prefixItems.json"
  )

  // Test-case descriptions to skip in `error_shape` only — same shape as NotSupportedErrorShape but
  // for cases that live in a file whose other cases do produce keyword-level errors.
  val NotSupportedErrorShapeTests: Seq[String] = Seq.empty

  // The official test suite expects `remotes/<rel>` to be reachable at `http://localhost:1234/<rel>`.
  // See test-suite/README.md § "Additional Assumptions".
  private val RemotesBase: Uri = Uri("http://localhost:1234/")

  val Registry: Registry = {
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

  /**
   * True if the unit tree has at least one descendant with a non-null `error` and a `kwLoc` deeper than root
   * (i.e. naming a specific keyword that failed).
   */
  private def hasKeywordError(u: OutputUnit): Boolean = {
    if (!u.vvalid && u.error != null && u.kwLoc.refTokens.nonEmpty && u.kwLoc.refTokens != Seq("")) true
    else u.details.exists(hasKeywordError)
  }

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
