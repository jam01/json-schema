package io.github.jam01.json_schema

import io.github.jam01.json_schema
import io.github.jam01.json_schema.TestSuiteTest.Registry
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}
import ujson.StringRenderer

import java.nio.file.{Files, Path, Paths}
import java.util.*
import scala.collection.mutable
import scala.util.Using

class TestSuiteTest {
  @ParameterizedTest
  @MethodSource(value = Array("args_provider"))
  def test(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, sch: Schema): Unit = {

    val dial = json_schema.tryDialect(sch, TestSuiteTest.Registry).getOrElse(Dialect._2020_12)
    val res = data.transform(json_schema.validator(sch, Registry, Config(dial)))
    //println(OutputUnitW.transform(res, StringRenderer()).toString)
    Assertions.assertEquals(valid, res.vvalid, path + ": " + desc + ": " + tdesc)
  }
}

object TestSuiteTest {
  val NotSupported: Seq[String] = Seq("refRemote.json")

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

  def args_provider: java.util.List[Arguments] = {
    val args = new java.util.ArrayList[Arguments]()
    Using(Files.walk(resource("test-suite/tests/draft2020-12/"), 1)) { tests =>
        tests.filter(Files.isRegularFile(_))
          .filter(p => !NotSupported.contains(p.getFileName.toString))
          //.peek(println
          .forEach(p => args.addAll(args_provider(p)))
    }

    args
  }

  def args_provider(path: Path): java.util.List[Arguments] = {
    val suite = ujson.read(ujson.Readable.fromPath(path)).arr
    val args = new java.util.ArrayList[Arguments]()

    suite.foreach { testcase =>
      testcase.obj.get("tests").get.arr.foreach { test =>
        args.add(Arguments.of(
          resource("test-suite/tests/draft2020-12/").relativize(path).toString,
          testcase.obj.get("description").get.str,
          test.obj.get("description").get.str,
          test.obj.get("data").get,
          test.obj.get("valid").get.bool,
          testcase.obj.get("schema").get.transform(SchemaR(reg = Registry))))
      }
    }

    args
  }

  def resource(s: String): Path = Paths.get(getClass.getClassLoader.getResource(s).toURI)
}
