/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertFalse, assertThrows, assertTrue}
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
    // 972783798187987123879878123.18878137 has 35 significant digits - one over Dec128's 34-digit
    // (Decimal128) anchor, so this specific pair is one of TestSuiteTest's
    // NotSupportedOptionalCases. Use a 34-sig-digit literal instead, which fits the anchor, and
    // assert the comparison is exact rather than Double-rounded - before the numOf fix, both sides
    // silently rounded to the same Double and this comparison passed for the wrong reason.
    val v = mkValidator("""{"exclusiveMaximum": 1234567890123456789012345678901234}""")
    assertTrue(isValid(v, "1234567890123456789012345678901233"), "just under the boundary")
    assertFalse(isValid(v, "1234567890123456789012345678901234"), "exactly at the boundary (exclusive)")
    assertFalse(isValid(v, "1234567890123456789012345678901235"), "just over the boundary")
  }

  @Test def schema_literal_beyond_the_128_bit_anchor_fails_to_compile(): Unit = {
    // 35 significant digits - one over Dec128's 34-digit anchor (see SchemaR.checkAnchor /
    // Schema.scala's Dec128 scaladoc). Compiling the schema itself must fail, not silently round.
    assertThrows(classOf[SchemaCompileException], () =>
      mkValidator("""{"exclusiveMaximum": 972783798187987123879878123.18878137}"""))
  }

  @Test def instance_decimal_data_stays_arbitrary_precision_uncapped(): Unit = {
    // Instance data never goes through Int128/Dec128's anchor (SchemaR.checkAnchor only runs on
    // schema literals) - a decimal instance value with far more than 34 significant digits must
    // still validate/compare correctly, not be capped or rejected.
    val v = mkValidator("""{"type": "number"}""")
    assertTrue(isValid(v, "1." + ("1" * 60)))

    val cmp = mkValidator("""{"minimum": 0}""")
    assertTrue(isValid(cmp, "1." + ("1" * 60)))
  }
}
