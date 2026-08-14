/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertNotSame, assertSame, assertThrows, assertTrue}
import org.junit.jupiter.api.Test

class SchemaRetrievalExceptionTest {
  // SchemaRetrievalException is a single top-level class, not path-dependent on the Schema
  // instance that throws it, so `TrueSchema` and `FalseSchema` report the same exception type
  // and a single catch site handles both.
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
  // the same SchemaRetrievalException as a pointer that goes nowhere.
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

  // RFC 6901 § 4 spells an array index as "0" or a digit sequence with no leading zero, and gives
  // "-" the element after the last. Anything else addresses nothing, and § 5 makes a pointer that
  // addresses nothing an error — the same SchemaRetrievalException every other unresolvable
  // reference reports, rather than the token parse escaping as a NumberFormatException or an
  // IndexOutOfBoundsException.
  @Test def array_index_tokens_in_a_pointer(): Unit = {
    val sch = ujson.Readable.fromString("""{"unknown": [{"type": "integer"}, {"type": "string"}]}""")
      .transform(SchemaR())

    assertTrue(sch.schBy(JsonPointer("/unknown/0")).isInstanceOf[ObjectSchema], "an index in range resolves")
    assertTrue(sch.schBy(JsonPointer("/unknown/1")).isInstanceOf[ObjectSchema], "and so does the last")

    assertThrows(classOf[SchemaRetrievalException], () => sch.schBy(JsonPointer("/unknown/x")))
    assertThrows(classOf[SchemaRetrievalException], () => sch.schBy(JsonPointer("/unknown/-1")))
    assertThrows(classOf[SchemaRetrievalException], () => sch.schBy(JsonPointer("/unknown/9")))
    assertThrows(classOf[SchemaRetrievalException], () => { sch.schBy(JsonPointer("/unknown/99999999999")); () },
      "wider than Int, so it addresses nothing")
    assertThrows(classOf[SchemaRetrievalException], () => { sch.schBy(JsonPointer("/unknown/01")); () },
      "a leading zero is not an index")
    assertThrows(classOf[SchemaRetrievalException], () => { sch.schBy(JsonPointer("/unknown/-")); () },
      "'-' is the element after the last, which no array has")
    assertThrows(classOf[SchemaRetrievalException], () => { sch.schBy(JsonPointer("/unknown/١")); () },
      "a non-ASCII decimal digit is not an index")
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

  // A literal reached by pointer is not a schema resource: SchemaR neither registers it nor
  // flushes the `$id`/`$anchor`s under it, so honouring `$id` would derive a base no Registry has
  // heard of and every reference inside it would resolve to a URI nothing can retrieve. Core
  // § 9.4.2 leaves this undefined; of the two readings this is the one that cannot mint an
  // unresolvable URI. See decision-010.
  @Test def an_id_inside_a_pointed_at_literal_carries_no_identity(): Unit = {
    val registry = new MutableRegistry
    val sch = ujson.Readable.fromString(
      """{"$id": "https://ex/root",
        | "unknown": {"$id": "https://ex/sub", "$defs": {"a": {"type": "integer"}},
        |             "properties": {"p": {"type": "string"}}},
        | "$ref": "#/unknown"}""".stripMargin)
      .transform(SchemaR(Uri("https://ex/root"), registry))

    val literal = sch.schBy(JsonPointer("/unknown")).asInstanceOf[ObjectSchema]
    assertEquals(Some("https://ex/sub"), literal.getId, "$id is still readable")
    assertEquals(Uri("https://ex/root"), literal.base, "but does not establish a base")
    assertEquals("https://ex/root#/unknown", literal.location.toString, "nor an identity")

    // and the whole subtree follows, not just the outermost node
    val nested = literal.schBy(JsonPointer("/properties/p")).asInstanceOf[ObjectSchema]
    assertEquals(Uri("https://ex/root"), nested.base, "nested schemas keep the enclosing base")

    // an ordinary embedded resource is unaffected
    val embedded = ujson.Readable.fromString(
      """{"$id": "https://ex/root",
        | "properties": {"p": {"$id": "https://ex/embedded", "type": "integer"}}}""".stripMargin)
      .transform(SchemaR(Uri("https://ex/root"), new MutableRegistry))
      .schBy(JsonPointer("/properties/p")).asInstanceOf[ObjectSchema]
    assertEquals(Uri("https://ex/embedded"), embedded.base, "$id in a schema position still counts")
  }

  // The reference inside the literal now resolves against the enclosing resource, which is
  // registered, so a bad pointer reports itself as one instead of as a missing resource.
  @Test def a_reference_inside_a_pointed_at_literal_resolves_against_the_enclosing_resource(): Unit = {
    val registry = new MutableRegistry
    val sch = ujson.Readable.fromString(
      """{"$id": "https://ex/root",
        | "$defs": {"a": {"type": "integer"}},
        | "unknown": {"$id": "https://ex/sub", "$ref": "#/$defs/a"},
        | "$ref": "#/unknown"}""".stripMargin)
      .transform(SchemaR(Uri("https://ex/root"), registry))

    val v = validator(sch, Config(Dialect.Basic), registry)
    def isValid(json: String): Boolean = {
      val res = try ujson.Readable.fromString(json).transform(v)
      catch { case e: ValidationException => e.result }
      res.vvalid
    }

    assertTrue(isValid("1"), "#/$defs/a resolves against https://ex/root, which is registered")
    assertFalse(isValid("\"no\""), "and still applies")
  }

  // A pointer can cross into a nested embedded resource (its own `$id`) on the way to a literal
  // further down. The literal's base must be that nested resource's, not the resource the pointer
  // started from, so a `$ref` inside it resolves relative to what actually encloses it.
  @Test def a_reference_inside_a_literal_past_a_nested_resource_resolves_against_that_resource(): Unit = {
    val registry = new MutableRegistry
    val sch = ujson.Readable.fromString(
      """{"$id": "https://ex/root",
        | "properties": {"p": {"$id": "https://ex/nested",
        |                       "$defs": {"a": {"type": "integer"}},
        |                       "unknown": {"$ref": "#/$defs/a"}}},
        | "$ref": "#/properties/p/unknown"}""".stripMargin)
      .transform(SchemaR(Uri("https://ex/root"), registry))

    val literal = sch.schBy(JsonPointer("/properties/p/unknown")).asInstanceOf[ObjectSchema]
    assertEquals(Uri("https://ex/nested"), literal.base, "base is the nested resource the pointer crossed, not the root")

    val v = validator(sch, Config(Dialect.Basic), registry)
    def isValid(json: String): Boolean = {
      val res = try ujson.Readable.fromString(json).transform(v)
      catch { case e: ValidationException => e.result }
      res.vvalid
    }

    assertTrue(isValid("1"), "#/$defs/a resolves against https://ex/nested, which has it")
    assertFalse(isValid("\"no\""), "and still applies")
  }
}
