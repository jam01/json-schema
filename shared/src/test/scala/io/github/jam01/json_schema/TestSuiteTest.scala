package io.github.jam01.json_schema

import io.github.jam01.json_schema
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}
import ujson.StringRenderer
import upickle.core.Visitor

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.Using

class TestSuiteTest {
  @ParameterizedTest
  @MethodSource(value = Array("args_provider"))
  def test(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, vis: Visitor[?, OutputUnit]): Unit = {

    val res = try { data.transform(vis) } catch
      case exc: ValidationException => exc.result
    //println(OutputUnitW.transform(res, StringRenderer()).toString)
    Assertions.assertEquals(valid, res.vvalid, path + ": " + desc + ": " + tdesc)
  }
}

object TestSuiteTest {
  val NotSupported: Seq[String] = Seq("refRemote.json")

  val Registry: mutable.Map[Uri, Schema] = {
    val builder = mutable.Map[Uri, Schema]()

    // load remotes
    Using(Files.walk(resource("test-suite/remotes/draft2020-12/"), 1)) { remotes =>
      remotes.filter(Files.isRegularFile(_))
        .forEach(p => {
          //println(p.toString)
          ujson.read(ujson.Readable.fromPath(p)).transform(SchemaR(Uri("file:" + p.toString), registry = builder))
        })
    }

    // load meta-schemas
    Using(Files.walk(resource("meta/"), 1)) { meta =>
      meta.filter(Files.isRegularFile(_))
        .forEach(p => {
          //println(p.toString)
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
          .forEach(p => args.addAll(args_provider(p)))
    }
    //args.addAll(args_provider(resource("test-suite/tests/draft2020-12/defs.json")))

    args
  }

  def args_provider(path: Path): java.util.List[Arguments] = {
    val suite = ujson.read(ujson.Readable.fromPath(path)).arr
    val args = new java.util.ArrayList[Arguments]()

    suite.foreach { testcase =>
      testcase.obj.get("tests").get.arr.foreach(test => {
        val sch = testcase.obj.get("schema").get.transform(SchemaR(registry = Registry))
        val dial = Dialect.tryDialect(sch, registry = Registry).getOrElse(Dialect._2020_12_Basic)

        args.add(Arguments.of(
          resource("test-suite/tests/draft2020-12/").relativize(path).toString,
          testcase.obj.get("description").get.str,
          test.obj.get("description").get.str,
          test.obj.get("data").get,
          test.obj.get("valid").get.bool,
          json_schema.validator(sch, Config(dial), Registry)))
      })
    }

    args
  }

  def resource(s: String): Path = Paths.get(getClass.getClassLoader.getResource(s).toURI)
}
