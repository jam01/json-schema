/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test

/**
 * Regression coverage for `\p{...}` Unicode property escapes in `pattern`/`patternProperties`.
 *
 * The official test suite's `optional/ecmascript-regex.json` covers this too, but that file
 * lived under `optional/` (not `draft2020-12/` at depth 1), so [[TestSuiteTest]] never actually
 * exercised it — a fix here shipped in 0.3.0 (`\p{Letter}` retried with `UNICODE_CHARACTER_CLASS`)
 * that looked plausible but never worked, since Java only recognizes Unicode `General_Category`
 * short codes (`\p{L}`) or `Is`-prefixed aliases, never ECMA-262's long-form names like `Letter`.
 * These tests pin the fix directly, independent of that suite-coverage gap.
 */
class RegexSupportTest {
  private def mkValidator(schemaJson: String, dialect: Dialect = Dialect.Basic) = {
    val sch = ujson.Readable.fromString(schemaJson).transform(SchemaR())
    validator(sch, Config(dialect))
  }

  private def isValid(v: upickle.core.Visitor[?, OutputUnit], instanceJson: String): Boolean = {
    val res = try ujson.Readable.fromString(instanceJson).transform(v)
    catch { case e: ValidationException => e.result }
    res.vvalid
  }

  @Test def pattern_matches_long_form_unicode_general_category_alias(): Unit = {
    val v = mkValidator("""{"pattern": "\\p{Letter}cole"}""")
    assertTrue(isValid(v, "\"Nicole\""), "Nicole should match \\p{Letter}cole")
    assertFalse(isValid(v, "\"7cole\""), "7cole should not match \\p{Letter}cole")
  }

  @Test def pattern_properties_matches_long_form_unicode_general_category_alias(): Unit = {
    val v = mkValidator("""{"patternProperties": {"\\p{Letter}cole": {"type": "integer"}}}""")
    assertTrue(isValid(v, """{"Nicole": 12}"""), "property matching \\p{Letter}cole should validate against the schema")
    assertFalse(isValid(v, """{"Nicole": "bad"}"""), "property matching \\p{Letter}cole should still enforce its schema")
  }

  @Test def pattern_matches_unicode_digit_property(): Unit = {
    val v = mkValidator("""{"pattern": "\\p{digit}"}""")
    assertTrue(isValid(v, "\"1\""), "1 should match \\p{digit}")
    assertFalse(isValid(v, "\"a\""), "a should not match \\p{digit}")
  }

  @Test def pattern_without_property_escapes_is_unaffected(): Unit = {
    val v = mkValidator("""{"pattern": "^[a-z]+$"}""")
    assertTrue(isValid(v, "\"abc\""))
    assertFalse(isValid(v, "\"ABC\""))
  }

  @Test def pattern_s_matches_ecma_262_whitespace_set(): Unit = {
    val v = mkValidator("""{"pattern": "^\\s$"}""")
    assertTrue(isValid(v, "\" \""), "ASCII space should match \\s")
    assertTrue(isValid(v, "\"\\u00a0\""), "latin-1 non-breaking-space should match \\s")
    assertTrue(isValid(v, "\"\\ufeff\""), "zero-width whitespace (BOM) should match \\s")
    assertTrue(isValid(v, "\"\\u2029\""), "paragraph separator should match \\s")
    assertTrue(isValid(v, "\"\\u2003\""), "EM SPACE should match \\s")
    assertFalse(isValid(v, "\"a\""), "a should not match \\s")
  }

  @Test def pattern_cap_s_matches_everything_but_ecma_262_whitespace(): Unit = {
    val v = mkValidator("""{"pattern": "^\\S$"}""")
    assertFalse(isValid(v, "\"\\u00a0\""), "latin-1 non-breaking-space should not match \\S")
    assertFalse(isValid(v, "\"\\ufeff\""), "zero-width whitespace (BOM) should not match \\S")
    assertFalse(isValid(v, "\"\\u2029\""), "paragraph separator should not match \\S")
    assertTrue(isValid(v, "\"a\""), "a should match \\S")
  }

  @Test def pattern_matches_control_letter_escape_case_insensitively(): Unit = {
    val v = mkValidator("""{"pattern": "^\\cc$"}""")
    assertTrue(isValid(v, "\"\\u0003\""), "\\cc should match control-C (0x03), matching ECMA-262's case-insensitive rule")
  }

  @Test def format_regex_rejects_java_only_bell_escape(): Unit = {
    val v = mkValidator("""{"format": "regex"}""", Dialect.FormatAssertion)
    assertFalse(isValid(v, "\"\\\\a\""), "\\a is not a valid ECMA-262 regex escape")
  }
}
