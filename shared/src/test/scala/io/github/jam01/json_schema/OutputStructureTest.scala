package io.github.jam01.json_schema

import io.github.jam01.json_schema
import io.github.jam01.json_schema.OutputStructureTest.*
import org.junit.jupiter.api.Test
import org.skyscreamer.jsonassert.JSONAssert
import ujson.StringRenderer
import upickle.core.LinkedHashMap

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions

class OutputStructureTest {
  @Test
  def main(): Unit = {
    val base = Uri("mem://test")

    val oschmap: LinkedHashMap[String, Value] = LinkedHashMapFactory(
      "type" -> "object",
      "$ref" -> "str")
    val osch = ObjectSchema(oschmap, base)
    oschmap.addOne(
      "items" -> new ObjectSchema(LinkedHashMapFactory("type" -> "number", "maximum" -> 3), base, Some(osch), Some("/items")))
    val r = ujson.Readable
      .fromString("""["", "", 1, 2, 3, "", 4, 5]""")
      .transform(json_schema.validator(osch, Config(format = OutputFormat.Detailed, ffast = false), Map(Uri("mem://test/str") -> RefSch3)))

    val res =  OutputUnitW.transform(r, StringRenderer()).toString
    //println(res)
    JSONAssert.assertEquals(resourceAsString("output/main.json"), res, true)
  }
}

object OutputStructureTest {
  val refBase: Uri = Uri("mem://ref")
  private val refsch3map: LinkedHashMap[String, Value] = LinkedHashMapFactory("type" -> Str("string"))
  val RefSch3: ObjectSchema = ObjectSchema(refsch3map, refBase)
  refsch3map.addOne("items" -> new ObjectSchema(LinkedHashMapFactory("type" -> "number", "minimum" -> 5), refBase, Some(RefSch3), Some("/items")))

  def resourceAsString(s: String): String =
    Files.readString(Paths.get(getClass.getClassLoader.getResource(s).toURI))
}
