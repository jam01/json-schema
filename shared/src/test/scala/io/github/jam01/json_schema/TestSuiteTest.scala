package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}

import java.nio.file.{Files, Paths}
import java.util.*
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
  val registry: mutable.Map[Uri, Schema] = {
    val res = mutable.Map[Uri, Schema]()

    // load remotes
    val remotes = Files.walk(Paths.get(getClass.getClassLoader.getResource("test-suite/remotes/draft2020-12/").toURI))
    try {
      remotes.filter(Files.isRegularFile(_))
        .forEach(p => {
          //println(p.toString)
          ujson.read(ujson.Readable.fromPath(p)).transform(SchemaR("file:" + p.toString, reg = res))
        })
    } finally if (remotes != null) remotes.close()

    // load meta-schema
    val meta = Files.walk(Paths.get(getClass.getClassLoader.getResource("meta/").toURI))
    try {
      meta.filter(Files.isRegularFile(_))
        .forEach(p => {
          //println(p.toString)
          ujson.read(ujson.Readable.fromPath(p)).transform(SchemaR("file:" + p.toString, reg = res))
        })
    } finally if (meta != null) meta.close()

    res
  }

  def args_provider: List[Arguments] = {
    val args = new ArrayList[Arguments]()
    args.addAll(args_provider("test-suite/tests/draft2020-12/additionalProperties.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/allOf.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/anchor.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/anyOf.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/boolean_schema.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/const.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/contains.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/defs.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/dynamicRef.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/enum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/exclusiveMaximum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/exclusiveMinimum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/format.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/if-then-else.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/items.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maxContains.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maxItems.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maxLength.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maxProperties.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/maximum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minContains.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minItems.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minLength.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minProperties.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/minimum.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/multipleOf.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/not.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/oneOf.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/pattern.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/prefixItems.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/properties.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/ref.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/required.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/type.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/uniqueItems.json"))
    args.addAll(args_provider("test-suite/tests/draft2020-12/unknownKeyword.json"))

    args
  }

  def args_provider(path: String): List[Arguments] = {
    val suite = ujson.read(ujson.Readable.fromPath(Paths.get(getClass.getClassLoader.getResource(path).toURI))).arr
    val args = new ArrayList[Arguments]()

    suite.foreach { testcase =>
      testcase.obj.get("tests").get.arr.foreach { test =>
        val reg = registry
        args.add(Arguments.of(
          path,
          testcase.obj.get("description").get.str,
          test.obj.get("description").get.str,
          test.obj.get("data").get,
          test.obj.get("valid").get.bool,
          testcase.obj.get("schema").get.transform(SchemaR("urn:uuid:" + UUID.randomUUID().toString, reg = reg)),
          Context(mutable.Stack(""), reg)))
      }
    }

    args
  }
}
