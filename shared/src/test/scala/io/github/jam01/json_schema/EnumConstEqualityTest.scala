/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test

/**
 * Regression coverage for `const`/`enum` equality of numbers nested inside arrays/objects.
 *
 * These cases must be driven through [[ujson.Readable.fromString]] (a genuine streaming parse)
 * rather than [[ujson.read]] followed by a second `.transform` on an extracted sub-node — the
 * latter is what [[TestSuiteTest]] does for fixture files, and going through that intermediate
 * `ujson.Value` AST collapses the distinction between e.g. `0` and `0.0`, silently hiding this
 * class of bug (both sides end up compared as the same representation either way).
 */
class EnumConstEqualityTest {
  private def mkValidator(schemaJson: String) = {
    val sch = ujson.Readable.fromString(schemaJson).transform(SchemaR())
    validator(sch, Config(Dialect.Basic))
  }

  private def isValid(v: upickle.core.Visitor[?, OutputUnit], instanceJson: String): Boolean = {
    val res = try ujson.Readable.fromString(instanceJson).transform(v)
    catch { case e: ValidationException => e.result }
    res.vvalid
  }

  @Test def enum_array_matches_int_and_float_representation_of_same_number(): Unit = {
    val v = mkValidator("""{"enum": [[0]]}""")
    assertTrue(isValid(v, "[0]"), "[0] should match enum entry [0]")
    assertTrue(isValid(v, "[0.0]"), "[0.0] should match enum entry [0] (same numeric value)")
    assertFalse(isValid(v, "[false]"), "[false] must not match enum entry [0]")
  }

  @Test def enum_object_matches_int_and_float_representation_of_same_number(): Unit = {
    val v = mkValidator("""{"enum": [6, "foo", [], true, {"foo": 12}]}""")
    assertTrue(isValid(v, """{"foo": 12}"""), "{\"foo\": 12} should match enum entry {\"foo\": 12}")
    assertTrue(isValid(v, """{"foo": 12.0}"""), "{\"foo\": 12.0} should match enum entry {\"foo\": 12} (same numeric value)")
    assertFalse(isValid(v, """{"foo": false}"""), "{\"foo\": false} must not match enum entry {\"foo\": 12}")
    assertFalse(isValid(v, """{"foo": 12, "boo": 42}"""), "extra property must not match")
  }

  @Test def enum_array_matches_int_and_float_representation_of_same_number_1(): Unit = {
    val v = mkValidator("""{"enum": [[1]]}""")
    assertTrue(isValid(v, "[1]"), "[1] should match enum entry [1]")
    assertTrue(isValid(v, "[1.0]"), "[1.0] should match enum entry [1] (same numeric value)")
    assertFalse(isValid(v, "[true]"), "[true] must not match enum entry [1]")
  }

  @Test def const_array_and_object_match_across_numeric_representation(): Unit = {
    val arrV = mkValidator("""{"const": [1]}""")
    assertTrue(isValid(arrV, "[1]"))
    assertTrue(isValid(arrV, "[1.0]"))
    assertFalse(isValid(arrV, "[true]"))

    val objV = mkValidator("""{"const": {"a": 1}}""")
    assertTrue(isValid(objV, """{"a": 1}"""))
    assertTrue(isValid(objV, """{"a": 1.0}"""))
    assertFalse(isValid(objV, """{"a": true}"""))
  }

  @Test def enum_and_const_of_non_numeric_type_do_not_throw_for_number_instance(): Unit = {
    // Regression guard: const/enum entries that aren't numbers must not blow up (nor match)
    // when the instance being checked is a number.
    val constV = mkValidator("""{"const": "foo"}""")
    assertFalse(isValid(constV, "42"))

    val enumV = mkValidator("""{"enum": ["foo", true, null]}""")
    assertFalse(isValid(enumV, "42"))
  }

  @Test def unique_items_compares_bignums_beyond_the_128_bit_anchor(): Unit = {
    // Regression guard: Int128/Dec128's anchor binds *schema*-embedded literals only (enforced by
    // SchemaR.checkAnchor at compile time) - it must not reject an *instance* number nested in an
    // array under uniqueItems (or const/enum), which LiteralVisitor also builds Int128/Dec128 for
    // (see Schema.scala's Int128/Dec128 scaladoc). uniqueItems needs no schema-side literal, so it
    // isolates the instance-only path (unlike const/enum, whose own value is itself a schema
    // literal, correctly anchored). Before this fix, this threw IllegalArgumentException instead
    // of comparing.
    val v = mkValidator("""{"uniqueItems": true}""")
    val bignum = "1" * 60
    val otherBignum = "2" * 60
    assertFalse(isValid(v, s"[$bignum, $bignum]"), "duplicate bignums beyond 128 bits should compare equal, not throw")
    assertTrue(isValid(v, s"[$bignum, $otherBignum]"), "distinct bignums beyond 128 bits should compare unequal, not throw")
  }
}
