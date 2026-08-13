/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.{Test, Timeout}

import java.util.concurrent.TimeUnit
import scala.util.Random

/**
 * `multipleOf` across the whole number model, including magnitudes that exist only because
 * numbers carry no exponent bound.
 *
 * The timeouts are the point of two of these. `1e100000000` is a thirty-byte literal whose
 * `BigDecimal` scale is -100000000, so any formulation that materializes the value before
 * dividing inflates it into a hundred-million-digit integer: `java.math.BigDecimal.remainder`
 * with no `MathContext` does exactly that, and takes minutes. `Validation.divides` never builds
 * a power of ten, and these keep it that way.
 *
 * [[differential_against_exact_arithmetic]] is the correctness half, checking the implementation
 * against the slow formulation it deliberately avoids. The sweep includes mantissas longer than
 * `DECIMAL128` can hold, since that is where a `BigDecimal.remainder` formulation stops being
 * decisive.
 */
class MultipleOfTest {
  private def isMultiple(instance: String, multipleOf: String): Boolean = {
    val sch = ujson.Readable.fromString(s"""{"multipleOf": $multipleOf}""").transform(SchemaR())
    val v = validator(sch, Config(Dialect.Basic))
    val res = try ujson.Readable.fromString(instance).transform(v)
    catch { case e: ValidationException => e.result }
    res.vvalid
  }

  /**
   * Exact divisibility the slow, obvious way: `java.math.BigDecimal.remainder` with no
   * `MathContext`, which materializes the values and is therefore always decisive. Deliberately
   * not the implementation's algorithm — an oracle that shared its reasoning would only confirm
   * that the code agrees with itself. The exponents in the sweep below stay small enough that
   * materializing is cheap.
   */
  private def exactlyDivides(xs: String, ys: String): Boolean = {
    val x = new java.math.BigDecimal(xs)
    val y = new java.math.BigDecimal(ys)
    y.signum != 0 && x.remainder(y).signum == 0
  }

  @Test def small_integers(): Unit = {
    assertTrue(isMultiple("9", "3"))
    assertFalse(isMultiple("10", "3"))
    assertTrue(isMultiple("0", "3"))
    assertTrue(isMultiple("-9", "3"))
    assertFalse(isMultiple("-10", "3"))
  }

  @Test def decimals(): Unit = {
    assertTrue(isMultiple("0.0075", "0.0001"))
    assertFalse(isMultiple("0.00751", "0.0001"))
    assertTrue(isMultiple("1.5", "0.5"))
    assertFalse(isMultiple("1.5", "0.4"))
    assertTrue(isMultiple("4.5", "1.5"))
  }

  @Test def mixed_widths(): Unit = {
    assertTrue(isMultiple("1e40", "2"))
    assertTrue(isMultiple("7e40", "7"))
    assertFalse(isMultiple("1e40", "3"))
    assertTrue(isMultiple("12345678901234567890123456789012345678901234567890", "2"))
    assertFalse(isMultiple("12345678901234567890123456789012345678901234567891", "2"))
    assertTrue(isMultiple("1000000000000000000000000000000000000000", "1e10"))
    // an exact multiple whose quotient cannot be written at scale(x) - scale(y)
    assertTrue(isMultiple("1234567890123456789e1", "2"))
    assertTrue(isMultiple("12345678901234567890123456789012345678901234567890.5", "0.5"))
  }

  @Timeout(value = 10, unit = TimeUnit.SECONDS)
  @Test def enormous_exponent_is_not_a_denial_of_service(): Unit = {
    assertFalse(isMultiple("1234567890123456789e100000000", "7"))
    assertTrue(isMultiple("1234567890123456789e100000000", "1e100000000"))
    assertTrue(isMultiple("2e100000000", "2"))
    assertTrue(isMultiple("2e100000000", "1e99999999"))
    assertFalse(isMultiple("3e100000000", "7"))
  }

  @Timeout(value = 10, unit = TimeUnit.SECONDS)
  @Test def enormous_negative_exponent_is_not_a_denial_of_service(): Unit = {
    assertTrue(isMultiple("1e-100000000", "1e-100000000"))
    assertFalse(isMultiple("1e-100000000", "7"))
    assertTrue(isMultiple("0", "1e-100000000"))
    assertFalse(isMultiple("3e-100000000", "1e-99999999"))
  }

  @Timeout(value = 60, unit = TimeUnit.SECONDS)
  @Test def differential_against_exact_arithmetic(): Unit = {
    val rnd = new Random(20260812) // fixed, so a failure is reproducible
    val cases = collection.mutable.ArrayBuffer(
      ("1234567890123456789e40", "7"),
      ("1234567890123456789e1", "2"), ("7e40", "7"), ("1e40", "3"),
      ("0.0075", "0.0001"), ("1.5", "0.5"), ("2e1", "4"), ("1e-40", "1e-41"),
      ("12345678901234567890123456789012345678901234567890.5", "0.5"))

    def literal(): String = {
      // sometimes longer than DECIMAL128's 34 digits, which is where a remainder-based
      // formulation gives up rather than answering
      val mantissa = ('1' + rnd.nextInt(9)).toChar.toString +
        (1 to rnd.nextInt(if (rnd.nextInt(4) == 0) 45 else 22))
          .map(_ => ('0' + rnd.nextInt(10)).toChar).mkString
      val withPoint =
        if (rnd.nextBoolean() && mantissa.length > 1) mantissa.take(1) + "." + mantissa.drop(1)
        else mantissa
      val exponent = if (rnd.nextBoolean()) "e" + (rnd.nextInt(80) - 40) else ""
      (if (rnd.nextBoolean()) "-" else "") + withPoint + exponent
    }

    for (_ <- 0 until 2000) {
      cases += ((literal(), literal()))
      // and a pair that is an exact multiple by construction, so the sweep is not all negatives
      val divisor = BigInt(1 + rnd.nextInt(999))
      val quotient = BigInt(1 + rnd.nextInt(999999))
      cases += (((divisor * quotient).toString + "e" + rnd.nextInt(30), divisor.toString))
    }

    for ((instance, multiple) <- cases)
      assertEquals(exactlyDivides(instance, multiple), isMultiple(instance, multiple),
        s"$instance multipleOf $multiple")
  }
}
