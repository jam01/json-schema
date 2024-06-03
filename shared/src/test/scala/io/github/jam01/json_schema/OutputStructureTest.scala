package io.github.jam01.json_schema

import io.github.jam01.json_schema.OutputStructureTest.*
import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.parallel.Resources
import org.skyscreamer.jsonassert.JSONAssert
import ujson.StringRenderer

import java.nio.file.{Files, Paths}
import java.util.UUID
import scala.collection.mutable
import scala.language.implicitConversions

class OutputStructureTest {
  @Test
  def main(): Unit = {
    val base = Uri.of("mem://test")

    val osch = ObjectSchema(LinkedHashMapFactory(
      "type" -> "object",
      "$ref" -> "str"), base)
    osch.value.addOne(
      "items" -> new ObjectSchema(LinkedHashMapFactory("type" -> "number", "maximum" -> 3), base, Some(osch), Some("/items")))
    val r = ujson.Readable
      .fromString("""["", "", 1, 2, 3, "", 4, 5]""")
      .transform(io.github.jam01.json_schema.validator(osch, Map(Uri.of("mem://test/str") -> RefSch3)))

    val res =  OutputUnitW.transform(r, StringRenderer()).toString
    //println(res)
    JSONAssert.assertEquals(resourceAsString("output/main.json"), res, true)
  }
}

object OutputStructureTest {
  val refBase: Uri = Uri.of("mem://ref")
  val RefSch3: ObjectSchema = ObjectSchema(LinkedHashMapFactory("type" -> Str("string")), refBase)
  RefSch3.value.addOne("items" -> new ObjectSchema(LinkedHashMapFactory("type" -> "number", "minimum" -> 5), refBase, Some(RefSch3), Some("/items")))

  def resourceAsString(s: String): String =
    Files.readString(Paths.get(getClass.getClassLoader.getResource(s).toURI))
}
