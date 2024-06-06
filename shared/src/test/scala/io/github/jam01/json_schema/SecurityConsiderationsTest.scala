package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertThrows, assertTrue}
import org.junit.jupiter.api.Test

import scala.collection.mutable

class SecurityConsiderationsTest {

  @Test
  def main(): Unit = {
    val reg: mutable.Map[Uri, Schema] = mutable.Map()
    val sch = ujson.Readable.fromString(
      """{
        |  "$ref": "#/$defs/a",
        |  "$defs": {
        |    "a": { "$ref": "#/$defs/b" },
        |    "b": { "$ref": "#/$defs/a" }
        |  }
        |}""".stripMargin).transform(SchemaR(Uri("mem://test"), reg))

    val ex1 = assertThrows(classOf[IllegalStateException],
      () => Transformer.transform(Null, SchemaValidator.of(sch, DefaultContext(reg), JsonPointer.Root, None)))

    assertTrue(ex1.getMessage.contains("depth limit exceeded"))
  }
}
