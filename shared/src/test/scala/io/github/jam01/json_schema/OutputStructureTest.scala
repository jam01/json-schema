package io.github.jam01.json_schema

import io.github.jam01.json_schema.OutputStructureTest.*
import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test
import ujson.StringRenderer

import scala.collection.mutable

class OutputStructureTest {
  @Test
  def output(): Unit = {
    val ctx = Context(mutable.Stack(""), Map(Uri.of("mem://test/str") -> RefSch3))
    val r = ujson.Readable
      .fromString("""["", "", 1, 2, 3, "", 4, 5]""")
      .transform(PointerDelegate(ctx, ObjectSchemaValidator.of(ObjectSchema(LinkedHashMapFactory(
        "type" -> "object", 
        "$ref" -> "str", 
        "items" -> ObjectSchema(LinkedHashMapFactory("type" -> "number", "maximum" -> 3), TestUri, None, None)
      ), TestUri, None, None), ctx)))

    println(OutputUnitTransformer.transform(r, StringRenderer()))
  }
}

object OutputStructureTest {
  val TestUri: Uri = Uri.of("mem://test")
  val RefSch3: ObjectSchema = ObjectSchema(LinkedHashMapFactory("type" -> Str("string"),
    "items" -> ObjectSchema(LinkedHashMapFactory("type" -> "number", "minimum" -> 5), TestUri, None, None)), TestUri)
}
