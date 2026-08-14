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

  @Test def unique_items_compares_numbers_by_value_not_representation(): Unit = {
    // Spec: two instances are equal if they are both numbers with the same mathematical value.
    // uniqueItems detects duplicates with a HashSet, so it needs numbers canonicalized first -
    // otherwise Int64(1) and Float64(1.0) are distinct keys and this reported `unique`. The
    // official suite's case for this is [1.0, 1.0, 1], whose first pair collides on
    // representation alone, so it passes either way and does not cover the mixed comparison.
    val v = mkValidator("""{"uniqueItems": true}""")
    assertFalse(isValid(v, "[1, 1.0]"), "1 and 1.0 are the same number")
    assertFalse(isValid(v, "[0, 0.0]"), "0 and 0.0 are the same number")
    assertFalse(isValid(v, "[0, -0.0]"), "0 and -0.0 are the same number")
    assertFalse(isValid(v, "[1, 1e0]"), "1 and 1e0 are the same number")
    assertFalse(isValid(v, "[1.5, 1.50]"), "1.5 and 1.50 are the same number")
    assertFalse(isValid(v, "[[1], [1.0]]"), "nested in arrays too")
    assertFalse(isValid(v, """[{"a": 1}, {"a": 1.0}]"""), "nested in objects too")
    assertFalse(isValid(v, s"[1, ${"1" * 40}, ${"1" * 40}.0]"), "and past Long/Double range")

    // Distinctness that must survive: booleans are not numbers, and unequal numbers stay unequal.
    assertTrue(isValid(v, "[1, 2]"))
    assertTrue(isValid(v, "[0, false]"), "false is not equal to zero")
    assertTrue(isValid(v, "[1, true]"), "true is not equal to one")
    assertTrue(isValid(v, "[1.5, 1.6]"))
  }

  @Test def unique_items_false_imposes_nothing_even_alongside_const(): Unit = {
    // `uniqueItems: false` must impose nothing, even when const/enum on the same schema already
    // trigger array collection for their own comparison.
    val v = mkValidator("""{"const": [1, 1], "uniqueItems": false}""")
    assertTrue(isValid(v, "[1, 1]"), "uniqueItems:false must not reject a repeated element")
    assertFalse(isValid(v, "[1, 2]"), "const must still apply")
  }

  @Test def unique_items_compares_bignums_wider_than_128_bits(): Unit = {
    // Regression guard: numbers nested inside an array or object are collected by LiteralVisitor
    // into Int128/Decimal for deep uniqueItems/const/enum comparison, and those carry no magnitude
    // bound (see Schema.scala's Int128 scaladoc). uniqueItems is the case with no schema-side
    // literal at all, so it isolates the instance-only path, which must compare bignums rather
    // than throw building an Int128.
    val v = mkValidator("""{"uniqueItems": true}""")
    val bignum = "1" * 60
    val otherBignum = "2" * 60
    assertFalse(isValid(v, s"[$bignum, $bignum]"), "duplicate bignums beyond 128 bits should compare equal, not throw")
    assertTrue(isValid(v, s"[$bignum, $otherBignum]"), "distinct bignums beyond 128 bits should compare unequal, not throw")
  }
}
