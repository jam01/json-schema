/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test

/**
 * Regression coverage for the `Validation.numOf` decimal-precision fix: a schema/instance decimal
 * literal with more significant digits than `Double` can hold (~15-17) used to be silently
 * rounded, because `String.toDoubleOption` never signals precision loss the way `toLongOption`
 * does for integers. Must be driven through [[ujson.Readable.fromString]] (a genuine streaming
 * parse) - see [[EnumConstEqualityTest]]'s header comment for why.
 */
class DecimalPrecisionTest {
  private def mkValidator(schemaJson: String) = {
    val sch = ujson.Readable.fromString(schemaJson).transform(SchemaR())
    validator(sch, Config(Dialect.Basic))
  }

  private def isValid(v: upickle.core.Visitor[?, OutputUnit], instanceJson: String): Boolean = {
    val res = try ujson.Readable.fromString(instanceJson).transform(v)
    catch { case e: ValidationException => e.result }
    res.vvalid
  }

  @Test def ordinary_decimal_literals_still_use_the_fast_double_path(): Unit = {
    // No behavior change intended for typical decimals - just a regression guard.
    val v = mkValidator("""{"exclusiveMaximum": 3.15}""")
    assertTrue(isValid(v, "3.14"))
    assertFalse(isValid(v, "3.15"))
    assertFalse(isValid(v, "3.16"))
  }

  @Test def schema_literal_beyond_double_precision_compares_exactly(): Unit = {
    // NB: the literal must carry a decimal point to exercise the fix. numOf routes a literal with
    // neither '.' nor exponent through `toLongOption.getOrElse(BigInt(s))`, which was already exact
    // and is untouched by this fix - so an integer literal here would pass with or without it.
    // All three instances collapse to the same Double (1.2345678901234568E32), so pre-fix the
    // "just under" case compared equal to the boundary rather than less than it.
    val v = mkValidator("""{"exclusiveMaximum": 123456789012345678901234567890123.4}""")
    assertTrue(isValid(v, "123456789012345678901234567890123.3"), "just under the boundary")
    assertFalse(isValid(v, "123456789012345678901234567890123.4"), "exactly at the boundary (exclusive)")
    assertFalse(isValid(v, "123456789012345678901234567890123.5"), "just over the boundary")
  }

  @Test def bounds_keywords_compare_decimals_past_double_precision(): Unit = {
    // 20 significant digits either side; every literal here rounds to exactly 3.0 as a Double, so
    // pre-fix `exclusiveMaximum` saw 3.0 < 3.0 and rejected the value it should have accepted.
    val v = mkValidator("""{"exclusiveMaximum": 3.0000000000000000001}""")
    assertTrue(isValid(v, "3.00000000000000000005"), "strictly less than the maximum")
    assertFalse(isValid(v, "3.0000000000000000001"), "equal to the maximum (exclusive)")
    assertFalse(isValid(v, "3.0000000000000000002"), "greater than the maximum")
  }

  @Test def const_and_enum_distinguish_decimals_past_double_precision(): Unit = {
    // Both literals round to the same Double, so pre-fix const/enum reported a match on a value
    // that isn't the constant.
    val const = mkValidator("""{"const": 1.0000000000000000001}""")
    assertTrue(isValid(const, "1.0000000000000000001"), "the constant itself")
    assertFalse(isValid(const, "1.0000000000000000002"), "a different value, equal only as a Double")

    val enuum = mkValidator("""{"enum": [1.0000000000000000001]}""")
    assertTrue(isValid(enuum, "1.0000000000000000001"), "the enumerated value itself")
    assertFalse(isValid(enuum, "1.0000000000000000002"), "a different value, equal only as a Double")
  }

  @Test def unique_items_distinguishes_decimals_past_double_precision(): Unit = {
    // Instance-only path (no schema literal involved): two distinct decimals that share a Double
    // are unique, and two spellings of the same value are not.
    val v = mkValidator("""{"uniqueItems": true}""")
    assertTrue(isValid(v, "[1.0000000000000000001, 1.0000000000000000002]"), "distinct past Double precision")
    assertFalse(isValid(v, "[1.0000000000000000001, 1.0000000000000000001]"), "genuinely duplicated")
  }

  @Test def exponent_literal_past_double_range_keeps_its_magnitude(): Unit = {
    // Double.parseDouble reports overflow as a successful parse of Infinity, so a short-mantissa
    // literal like 1e400 used to become Float64(Infinity): `type: integer` said false (Infinity
    // isn't whole) and every comparison died converting Infinity to a BigDecimal.
    assertTrue(isValid(mkValidator("""{"type": "integer"}"""), "1e400"), "1e400 is an integer")
    assertFalse(isValid(mkValidator("""{"maximum": 2}"""), "1e400"), "and is greater than 2")
    assertTrue(isValid(mkValidator("""{"minimum": 2}"""), "1e400"), "and greater than the minimum")
    assertTrue(isValid(mkValidator("""{"multipleOf": 2}"""), "1e400"), "and an even number")
    // Underflow is the same trap in the other direction: 1e-400 parses as a successful 0.0.
    assertFalse(isValid(mkValidator("""{"exclusiveMinimum": 0}"""), "0"), "zero is not above zero")
    assertTrue(isValid(mkValidator("""{"exclusiveMinimum": 0}"""), "1e-400"), "but 1e-400 is")
  }

  @Test def zero_literals_stay_on_the_double_path(): Unit = {
    // The mantissa-is-all-zeros case has to be told apart from underflow-to-zero; these are
    // genuinely zero and must not be promoted.
    val v = mkValidator("""{"const": 0}""")
    for (zero <- Seq("0", "0.0", "-0.0", "0e100", "0.000e-100"))
      assertTrue(isValid(v, zero), s"$zero is zero")
  }

  @Test def exponent_literal_with_a_long_mantissa_keeps_its_magnitude(): Unit = {
    // A literal whose mantissa exceeds Double's precision no longer overflows to Infinity, so it
    // keeps both its integrality and its magnitude. Pre-fix `type: integer` rejected it (Infinity
    // is not whole) and `maximum` blew up converting Infinity to a BigDecimal.
    assertTrue(isValid(mkValidator("""{"type": "integer"}"""), "1234567890123456789e400"),
      "a large power-of-ten multiple is still an integer")
    assertFalse(isValid(mkValidator("""{"maximum": 2}"""), "1234567890123456789e400"),
      "and still compares as greater than 2")
  }

  @Test def schema_literal_of_any_precision_compiles_and_compares(): Unit = {
    // 35 significant digits, straight out of optional/bignum.json - one past what Decimal128 can
    // represent. There is no magnitude bound on schema literals: this must compile and compare
    // exactly, not throw and not round. (It used to throw; see README § Numbers.)
    val v = mkValidator("""{"exclusiveMaximum": 972783798187987123879878123.18878137}""")
    assertTrue(isValid(v, "972783798187987123879878123.18878136"), "just under the boundary")
    assertFalse(isValid(v, "972783798187987123879878123.18878137"), "exactly at the boundary (exclusive)")
    assertFalse(isValid(v, "972783798187987123879878123.188781371"), "just over the boundary")
  }

  @Test def instance_numbers_of_any_precision_compare_against_ordinary_schemas(): Unit = {
    // The schema side here is as plain as it gets - a Long or a Double literal - and the instance
    // is a 60-digit integer, i.e. wider than Decimal128. Mixed-width comparison used to convert
    // through a Decimal128-capped helper and threw ArithmeticException on all but the last.
    val big = "1" * 59 + "2"
    assertTrue(isValid(mkValidator("""{"minimum": 1.5}"""), big), "60-digit integer is above 1.5")
    assertFalse(isValid(mkValidator("""{"exclusiveMaximum": 1.5}"""), big), "and not below it")
    assertFalse(isValid(mkValidator("""{"const": 1}"""), big), "and is not the constant 1")
    assertFalse(isValid(mkValidator("""{"enum": [1, 2]}"""), big), "and is not in the enumeration")
    assertFalse(isValid(mkValidator("""{"multipleOf": 3}"""), big), "and is not a multiple of 3")
  }

  @Test def instance_decimal_data_stays_arbitrary_precision_uncapped(): Unit = {
    // A decimal instance value with far more than 34 significant digits must validate and compare
    // correctly, not be capped or rejected.
    val v = mkValidator("""{"type": "number"}""")
    assertTrue(isValid(v, "1." + ("1" * 60)))

    val cmp = mkValidator("""{"minimum": 0}""")
    assertTrue(isValid(cmp, "1." + ("1" * 60)))
  }
}
