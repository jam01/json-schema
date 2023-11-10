package io.github.jam01.json_schema

import org.junit.jupiter.api.{Assertions, Disabled, Test}
import ujson.StringParser
//import upickle.default.*

class SchemaMapper {
  val lhm: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "string",
    "pattern" -> ".*",
    "maxLength" -> 16,
    "minLength" -> 3,
  )
  val strSch: ObjectSchema = ObjectSchema(lhm, "mem://test")

  val lhm2: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "array",
    "maxItems" -> 4,
    "minItems" -> 2,
    "items" -> strSch
  )
  val arrSch: ObjectSchema = ObjectSchema(lhm2, "mem://test")


  val lhm4: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "object",
    "maxProperties" -> 2,
    "minProperties" -> 1,
    "properties" -> LinkedHashMap(
      "foo" -> strSch,
      "arr" -> arrSch,
      "obj" -> ObjectSchema(LinkedHashMap(
        "type" -> "object",
        "maxProperties" -> 1), "mem://test")
    ),
    "required" -> Seq("foo")
  )
  val objSch: ObjectSchema = ObjectSchema(lhm4, "mem://test")

  @Test
  @Disabled
  def main(): Unit = {
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
    Assertions.assertEquals(True, sch)
  }

  @Test
  def _false(): Unit = {
    val jsonStr = "false"
    val sch = StringParser.transform(jsonStr, new SchemaR("(test)"))
    Assertions.assertEquals(False, sch)
  }
}


