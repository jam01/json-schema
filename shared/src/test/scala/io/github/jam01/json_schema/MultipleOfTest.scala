/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.{Test, Timeout}

import java.math.BigInteger
import java.util.concurrent.TimeUnit
import scala.util.Random

/**
 * `multipleOf` across the whole number model, including magnitudes that exist only because
 * numbers carry no exponent bound.
 *
 * Two hazards are pinned here. The first is cost: `1e100000000` is a thirty-byte literal whose
 * `BigDecimal` scale is -100000000, so any implementation that materializes the value to divide
 * it inflates that into a hundred-million-digit integer — `java.math.BigDecimal.remainder`
 * without a `MathContext` does exactly that, and takes minutes. `scala.math.BigDecimal`'s `%`
 * carries `DECIMAL128`, which bails out instead, and [[Validation.isMultiple]] reads that bail-out
 * as "not a multiple"; the timeouts below are what keeps a future change from quietly reaching
 * the slow path.
 *
 * The second is whether that bail-out is ever the *wrong* answer. [[differential_against_exact_arithmetic]]
 * checks it against an oracle that computes divisibility a deliberately different way — modular
 * arithmetic on the unscaled values, which never materializes a power of ten — so the two share
 * no machinery.
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
   * Exact divisibility, by modular arithmetic on the unscaled values. Both operands are
   * `unscaled * 10^-scale`, so `x` is a multiple of `y` exactly when `10^d * ux` is divisible by
   * `uy`, and `10^d mod uy` is a `modPow` away — no power of ten is ever built.
   */
  private def exactlyDivides(xs: String, ys: String): Boolean = {
    val x = new java.math.BigDecimal(xs)
    val y = new java.math.BigDecimal(ys)
    if (y.signum == 0) false
    else if (x.signum == 0) true
    else {
      val ux = x.unscaledValue.abs
      val uy = y.unscaledValue.abs
      val d = y.scale.toLong - x.scale.toLong
      if (d >= 0)
        ux.mod(uy).multiply(BigInteger.TEN.modPow(BigInteger.valueOf(d), uy)).mod(uy).signum == 0
      else {
        val k = -d
        // uy * 10^k can only divide ux if 10^k <= ux, and log10(ux) < bitLength * 0.302
        if (k > ux.bitLength.toLong * 302 / 1000 + 1) false
        else ux.remainder(uy.multiply(BigInteger.TEN.pow(k.toInt))).signum == 0
      }
    }
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
      ("1234567890123456789e100000000", "7"), ("1234567890123456789e40", "7"),
      ("1234567890123456789e1", "2"), ("7e40", "7"), ("1e40", "3"),
      ("0.0075", "0.0001"), ("1.5", "0.5"), ("2e1", "4"), ("1e-40", "1e-41"),
      ("12345678901234567890123456789012345678901234567890.5", "0.5"))

    def literal(): String = {
      val mantissa = ('1' + rnd.nextInt(9)).toChar.toString +
        (1 to rnd.nextInt(22)).map(_ => ('0' + rnd.nextInt(10)).toChar).mkString
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
