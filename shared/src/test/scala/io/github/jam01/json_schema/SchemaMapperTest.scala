/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.{Assertions, Test}
import ujson.StringParser

class SchemaMapperTest {
  val uri: Uri = Uri("(test)")

  @Test
  def _true(): Unit = {
    val jsonStr = "true"
    val sch = StringParser.transform(jsonStr, SchemaR(uri))
    Assertions.assertEquals(TrueSchema, sch)
  }

  @Test
  def _false(): Unit = {
    val jsonStr = "false"
    val sch = StringParser.transform(jsonStr, SchemaR(uri))
    Assertions.assertEquals(FalseSchema, sch)
  }

  @Test
  def main(): Unit = {
    val jsonStr = """{
                    |  "type": "object",
                    |  "maxProperties": 2,
                    |  "minProperties": 1,
                    |  "required": [
                    |    "foo"
                    |  ],
                    |  "properties": {
                    |    "foo": {
                    |      "type": "string",
                    |      "pattern": ".*",
                    |      "maxLength": 16,
                    |      "minLength": 3
                    |    },
                    |    "arr": {
                    |      "type": "array",
                    |      "maxItems": 4,
                    |      "minItems": 2,
                    |      "items": {
                    |        "type": "number"
                    |      }
                    |    },
                    |    "obj": {
                    |      "type": "object",
                    |      "maxProperties": 1
                    |    }
                    |  }
                    |}""".stripMargin

    val oschmap = LinkedHashMapFactory(
      "type" -> Str("object"),
      "maxProperties" -> Num(2L),
      "minProperties" -> Num(1L),
      "required" -> Arr(Str("foo")))
    val osch: ObjectSchema = ObjectSchema(oschmap, uri)

    val arrschmap = LinkedHashMapFactory(
      "type" -> Str("array"),
      "maxItems" -> Num(4L),
      "minItems" -> Num(2L))
    val arrsch: ObjectSchema = new ObjectSchema(arrschmap, uri, Some(osch), Some("/properties/arr"))

    arrschmap.addOne("items" -> new ObjectSchema(LinkedHashMapFactory(
      "type" -> Str("number")), uri, Some(arrsch), Some("/items")))

    oschmap.addOne("properties" -> Obj(
      "foo" -> new ObjectSchema(LinkedHashMapFactory(
        "type" -> Str("string"),
        "pattern" -> Str(".*"),
        "maxLength" -> Num(16L),
        "minLength" -> Num(3L)), uri, Some(osch), Some("/properties/foo")),
      "arr" -> arrsch,
      "obj" -> new ObjectSchema(LinkedHashMapFactory(
        "type" -> Str("object"),
        "maxProperties" -> Num(1L)), uri, Some(osch), Some("/properties/obj"))
    ))

    val sch = StringParser.transform(jsonStr, SchemaR(uri))
    Assertions.assertEquals(osch, sch)
  }
}


