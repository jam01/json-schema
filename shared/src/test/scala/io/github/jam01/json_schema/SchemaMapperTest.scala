package io.github.jam01.json_schema

import org.junit.jupiter.api.{Assertions, Disabled, Test}
import ujson.StringParser
import upickle.core.LinkedHashMap
//import upickle.default.*

class SchemaMapperTest {
  val lhm: LinkedHashMap[String, Value] = LinkedHashMap(Seq(
    "type" -> Str("string"),
    "pattern" -> Str(".*"),
    "maxLength" -> Num(16L),
    "minLength" -> Num(3L),
  ))
  val strSch: ObjectSchema = ObjectSchema(lhm, Uri.of("mem://test"))

  val lhm2: LinkedHashMap[String, Value] = LinkedHashMap(Seq(
    "type" -> Str("array"),
    "maxItems" -> Num(4L),
    "minItems" -> Num(2L),
    "items" -> strSch
  ))
  val arrSch: ObjectSchema = ObjectSchema(lhm2, Uri.of("mem://test"))


  val lhm4: LinkedHashMap[String, Value] = LinkedHashMap(Seq(
    "type" -> Str("object"),
    "maxProperties" -> Num(2L),
    "minProperties" -> Num(1L),
    "properties" -> Obj(LinkedHashMap(Seq(
      "foo" -> strSch,
      "arr" -> arrSch,
      "obj" -> ObjectSchema(LinkedHashMap(Seq(
        "type" -> Str("object"),
        "maxProperties" -> Num(1L))), Uri.of("mem://test"))
    ))),
    "required" -> Arr(Str("foo"))
  ))
  val objSch: ObjectSchema = ObjectSchema(lhm4, Uri.of("mem://test"))

  @Test
  @Disabled
  def dis(): Unit = {
//    val m = upickle.core.LinkedHashMap[String, String]()
//    m.put("", "")
//    println(
//      upickle.default.write(m)
//    )
  }

  @Test
  def _true(): Unit = {
    val jsonStr = "true"
    val sch = StringParser.transform(jsonStr, new SchemaR("(test)"))
    Assertions.assertEquals(TrueSchema, sch)
  }

  @Test
  def _false(): Unit = {
    val jsonStr = "false"
    val sch = StringParser.transform(jsonStr, new SchemaR("(test)"))
    Assertions.assertEquals(FalseSchema, sch)
  }

  @Test
  def main(): Unit = {
    val jsonStr = """{
                    |  "allOf": [
                    |    {
                    |      "classRelation": "is-a",
                    |      "$ref": "classes/base.json"
                    |    },
                    |    {
                    |      "$ref": "fields/common.json"
                    |    }
                    |  ],
                    |  "properties": {
                    |    "foo": {
                    |      "classRelation": "has-a",
                    |      "$ref": "classes/foo.json"
                    |    },
                    |    "date": {
                    |      "$ref": "types/dateStruct.json"
                    |    }
                    |  }
                    |}""".stripMargin
    val sch = StringParser.transform(jsonStr, new SchemaR("(test)"))
    println()
  }
}


