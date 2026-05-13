/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test

class FormatAssertionTest {
  private def validate(format: String, value: String): Boolean = {
    val sch = ujson.Readable
      .fromString(s"""{"format":"$format"}""")
      .transform(SchemaR())
    try ujson.Str(value).transform(validator(sch, Config(dialect = Dialect.FormatAssertion))).vvalid
    catch { case e: ValidationException => e.result.vvalid }
  }

  // email — quoted-string local part
  @Test def email_quoted_space_valid(): Unit =
    assertTrue(validate("email", "\"joe bloggs\"@example.com"))

  @Test def email_quoted_escaped_dquote_valid(): Unit =
    assertTrue(validate("email", "\"a\\\"b\"@example.com"))

  @Test def email_quoted_unescaped_dquote_invalid(): Unit =
    assertFalse(validate("email", "\"a\"b\"@example.com"))

  // regression: dangling-else made the backslash-at-end check unreachable, so a
  // trailing `\"` (which escapes the closing quote, leaving the string unterminated)
  // was silently accepted.
  @Test def email_quoted_unescaped_trailing_backslash_invalid(): Unit =
    assertFalse(validate("email", "\"abc\\\"@example.com"))

  @Test def email_quoted_escaped_trailing_backslash_valid(): Unit =
    assertTrue(validate("email", "\"abc\\\\\"@example.com"))

  // regression: i18n branch (non-ASCII preceded by backslash) was also unreachable.
  @Test def idn_email_quoted_backslash_before_nonascii_invalid(): Unit =
    assertFalse(validate("idn-email", "\"\\é\"@example.com"))

  @Test def idn_email_quoted_bare_nonascii_valid(): Unit =
    assertTrue(validate("idn-email", "\"é\"@example.com"))
}
