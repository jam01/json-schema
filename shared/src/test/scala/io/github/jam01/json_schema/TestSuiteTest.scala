package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}

import java.nio.file.{Files, Path, Paths}
import java.util.*
import scala.collection.mutable
import scala.util.Using

class TestSuiteTest {
  @ParameterizedTest
  @MethodSource(value = Array("args_provider"))
  def test(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, sch: Schema, ctx: Context): Unit = {
    val res = data.transform(SchemaValidator.of(sch, ctx = ctx))
    Assertions.assertEquals(valid, res.valid, path + ": " + desc + ": " + tdesc)
  }
}

object TestSuiteTest {
  val NotSupported: Seq[String] = Seq("content.json",
    "default.json",
    "id.json",
    "infinite-loop-detection.json",
    "patternProperties.json",
    "refRemote.json",
    "unevaluatedItems.json",
    "unevaluatedProperties.json",
    "vocabulary.json")

  val Registry: mutable.Map[Uri, Schema] = {
    val res = mutable.Map[Uri, Schema]()

    // load remotes
    Using(Files.walk(resource("test-suite/remotes/draft2020-12/"), 1)) { remotes =>
      remotes.filter(Files.isRegularFile(_))
        .forEach(p => {
          //println(p.toString)
          ujson.read(ujson.Readable.fromPath(p)).transform(SchemaR(Uri.of("file:" + p.toString), reg = res))
        })
    }

    // load meta-schemas
    Using(Files.walk(resource("meta/"), 1)) { meta =>
      meta.filter(Files.isRegularFile(_))
        .forEach(p => {
          //println(p.toString)
          ujson.read(ujson.Readable.fromPath(p)).transform(SchemaR(Uri.of("file:" + p.toString), reg = res))
        })
    }

    res
  }

  def args_provider: List[Arguments] = {
    val args = new ArrayList[Arguments]()
    Using(Files.walk(resource("test-suite/tests/draft2020-12/"), 1)) { tests =>
        tests.filter(Files.isRegularFile(_))
          .filter(p => !NotSupported.contains(p.getFileName.toString))
          //.peek(println
          .forEach(p => args.addAll(args_provider(p)))
    }

    args
  }

  def args_provider(path: Path): List[Arguments] = {
    val suite = ujson.read(ujson.Readable.fromPath(path)).arr
    val args = new ArrayList[Arguments]()

    suite.foreach { testcase =>
      testcase.obj.get("tests").get.arr.foreach { test =>
        val reg = Registry
        args.add(Arguments.of(
          resource("test-suite/tests/draft2020-12/").relativize(path).toString,
          testcase.obj.get("description").get.str,
          test.obj.get("description").get.str,
          test.obj.get("data").get,
          test.obj.get("valid").get.bool,
          testcase.obj.get("schema").get.transform(SchemaR(Uri.of("urn:uuid:" + UUID.randomUUID().toString), reg = reg)),
          Context(mutable.Stack(""), reg)))
      }
    }

    args
  }

  def resource(s: String): Path = Paths.get(getClass.getClassLoader.getResource(s).toURI)
}
