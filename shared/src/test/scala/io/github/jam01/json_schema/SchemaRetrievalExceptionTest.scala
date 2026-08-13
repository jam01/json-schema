/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertNotSame, assertSame, assertThrows, assertTrue}
import org.junit.jupiter.api.Test

class SchemaRetrievalExceptionTest {
  // Regression: SchemaRetrievalException used to be a path-dependent inner class of `Schema`,
  // so `TrueSchema.SchemaRetrievalException` and `FalseSchema.SchemaRetrievalException` were
  // distinct nominal types — pattern matching across schema instances would have missed.
  @Test def caught_uniformly_across_schema_instances(): Unit = {
    val osch: Schema = ujson.Readable.fromString("""{"type":"string"}""").transform(SchemaR())
    val bsch: Schema = TrueSchema

    val e1 = assertThrows(classOf[SchemaRetrievalException], () =>
      osch.schBy(JsonPointer("/does/not/exist")))
    val e2 = assertThrows(classOf[SchemaRetrievalException], () =>
      bsch.schBy(JsonPointer("/anything")))

    // Both are the same top-level class; a single catch site handles both.
    assertEquals(e1.getClass, e2.getClass)
    assertTrue(e1.getClass.getName.endsWith(".SchemaRetrievalException"),
      s"expected top-level SchemaRetrievalException, got ${e1.getClass.getName}")
  }

  // Per Core § Fragment Identifiers a JSON Pointer resolves against the resource as plain JSON, so
  // a pointer can land on a value the parser never recognized as a subschema. An object or boolean
  // found that way is a valid subschema and is compiled on the spot; a scalar is not, and reports
  // the same SchemaRetrievalException as a pointer that goes nowhere (it used to be a raw
  // ClassCastException, which no caller could reasonably be catching for).
  @Test def pointer_into_an_unrecognized_keyword(): Unit = {
    val sch = ujson.Readable.fromString(
      """{"unknown": {"type": "integer"}, "flag": true, "off": false, "scalar": "hello",
        | "arr": [1, 2]}""".stripMargin).transform(SchemaR())

    assertTrue(sch.schBy(JsonPointer("/unknown")).isInstanceOf[ObjectSchema], "object is a subschema")
    assertEquals(TrueSchema, sch.schBy(JsonPointer("/flag")), "true is a subschema")
    assertEquals(FalseSchema, sch.schBy(JsonPointer("/off")), "false is a subschema")

    assertThrows(classOf[SchemaRetrievalException], () => sch.schBy(JsonPointer("/scalar")))
    assertThrows(classOf[SchemaRetrievalException], () => sch.schBy(JsonPointer("/arr/0")))
  }

  // The literal is compiled through SchemaR, not merely wrapped in an ObjectSchema, so keywords
  // nested inside it are real subschemas too. Wrapping only converted the outermost node, leaving
  // every child a raw Obj, and any applicator reaching for one got "Expected Schema" instead.
  @Test def pointer_target_is_compiled_all_the_way_down(): Unit = {
    val registry = new MutableRegistry
    val sch = ujson.Readable.fromString(
      """{"$id": "https://ex/deep",
        | "unknown": {"properties": {"a": {"type": "integer"}},
        |             "allOf": [{"required": ["a"]}]},
        | "$ref": "#/unknown"}""".stripMargin)
      .transform(SchemaR(Uri("https://ex/deep"), registry))

    val v = validator(sch, Config(Dialect.Basic), registry)
    def isValid(json: String): Boolean = {
      val res = try ujson.Readable.fromString(json).transform(v)
      catch { case e: ValidationException => e.result }
      res.vvalid
    }

    assertTrue(isValid("""{"a": 1}"""), "satisfies the nested properties and allOf")
    assertFalse(isValid("""{"a": "no"}"""), "nested properties must still apply")
    assertFalse(isValid("""{"b": 1}"""), "nested allOf/required must still apply")
  }

  // Resolution is per-reference and per-Core-instance, so without memoization every `$ref` into
  // the same literal ran the subtree back through SchemaR and produced a fresh Schema graph for
  // one location — and a self-referential literal did it once per level of recursion, leaving the
  // depth guard as the only bound on repeated compilation.
  @Test def a_literal_is_compiled_once_per_location(): Unit = {
    val sch = ujson.Readable.fromString(
      """{"unknown": {"properties": {"a": {"type": "integer"}}},
        | "other": {"type": "string"}}""".stripMargin).transform(SchemaR())

    val first = sch.schBy(JsonPointer("/unknown"))
    assertSame(first, sch.schBy(JsonPointer("/unknown")), "same location, same compiled schema")
    assertSame(first, sch.schBy(JsonPointer("/unknown")), "and again")
    assertNotSame(first, sch.schBy(JsonPointer("/other")), "a different location is its own schema")

    // a nested literal is cached on the schema that compiled it, not on the root
    val nested = first.schBy(JsonPointer("/properties/a"))
    assertSame(nested, first.schBy(JsonPointer("/properties/a")))
  }
}
