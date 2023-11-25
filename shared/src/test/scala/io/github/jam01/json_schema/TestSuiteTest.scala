package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}

import java.nio.file.Paths
import java.util
import scala.collection.mutable

class TestSuiteTest {

  @ParameterizedTest
  @MethodSource(value = Array("args_provider"))
  def test(path: String, desc: String, tdesc: String, data: ujson.Value, valid: Boolean, sch: Schema, ctx: Context): Unit = {
    val res = data.transform(SchemaValidator.of(sch, ctx = ctx))
    Assertions.assertEquals(valid, res, path + ": " + desc + ": " + tdesc)
  }
}

object TestSuiteTest {
  def args_provider: java.util.List[Arguments] = {
    val args = new util.ArrayList[Arguments]()
    args.addAll(args_provider("test-suite/tests/draft2020-12/anchor.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/boolean_schema.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/defs.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/exclusiveMaximum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/exclusiveMinimum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/format.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/items.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maximum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maxItems.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maxLength.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maxProperties.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minimum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minItems.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minLength.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minProperties.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/multipleOf.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/pattern.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/properties.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/ref.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/required.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/type.json"))

    args
  }

  def args_provider(path: String): java.util.List[Arguments] = {
    val suite = ujson.read(ujson.Readable.fromPath(Paths.get(getClass.getClassLoader.getResource(path).toURI))).arr
    val args = new util.ArrayList[Arguments]()

    suite.foreach { testcase =>
      testcase.obj.get("tests").get.arr.foreach { test =>
        val reg = mutable.Map[String, Schema]()
        args.add(Arguments.of(
          path,
          testcase.obj.get("description").get.str,
          test.obj.get("description").get.str,
          test.obj.get("data").get,
          test.obj.get("valid").get.bool,
          testcase.obj.get("schema").get.transform(SchemaR("test", reg = reg)),
          Context(mutable.Stack(""), reg)))
      }
    }

    args
  }
}
