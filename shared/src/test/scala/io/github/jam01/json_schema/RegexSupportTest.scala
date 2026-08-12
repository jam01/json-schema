/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertFalse, assertThrows, assertTrue}
import org.junit.jupiter.api.Test

/**
 * Pins the JVM target's ECMA-262 translation, one test per row of the table in
 * README § Regular expressions.
 *
 * These are all cases where `java.util.regex` diverges from ECMA-262, so on Scala.js — which
 * hands patterns to the platform's own `RegExp` — they hold for free. The official suite reaches
 * only a handful of them: `optional/ecmascript-regex.json` covers `\p{...}`, `\c`, `\s`/`\S` and
 * the ASCII-only `\d`/`\w`, and its own `$`-versus-trailing-newline fixture cannot fail on any
 * engine, since the data is `"abc\\n"` — a literal backslash followed by `n`. Every expectation
 * below was verified against a real ECMA-262 engine (Node) before being asserted here.
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

  /** True if `instanceJson` matches `pattern`, which is given as it would appear in JSON. */
  private def matches(pattern: String, instanceJson: String): Boolean =
    isValid(mkValidator(s"""{"pattern": "$pattern"}"""), instanceJson)

  /** True if `patternJson` is a valid ECMA-262 pattern per `format: regex`. */
  private def isValidPattern(patternJson: String): Boolean =
    isValid(mkValidator("""{"format": "regex"}""", Dialect.FormatAssertion), s""""$patternJson"""")

  // ---------------------------------------------------------------- anchors

  @Test def dollar_does_not_match_before_a_trailing_line_terminator(): Unit = {
    assertTrue(matches("^abc$", "\"abc\""), "^abc$ should match abc")
    assertFalse(matches("^abc$", "\"abc\\n\""), "$ is end of input, not end of line")
    assertFalse(matches("^abc$", "\"abc\\r\\n\""), "$ should not match before a trailing CRLF")
    assertFalse(matches("^abc$", "\"abc\\u2028\""), "$ should not match before LINE SEPARATOR")
    assertFalse(matches("c$", "\"abc\\n\""), "an unanchored $ is still end of input")
    assertFalse(matches("^$", "\"\\n\""), "^$ should not match a lone newline")
  }

  @Test def caret_matches_only_at_the_start_of_input(): Unit = {
    assertFalse(matches("^abc", "\"\\nabc\""), "^ is start of input, not start of line")
    assertTrue(matches("^abc", "\"abcd\""), "^ should match at the start of input")
  }

  // ---------------------------------------------------------------- character classes

  @Test def dot_excludes_only_the_ecma_262_line_terminators(): Unit = {
    assertFalse(matches("^.$", "\"\\n\""), ". should not match a line feed")
    assertFalse(matches("^.$", "\"\\u2029\""), ". should not match PARAGRAPH SEPARATOR")
    assertTrue(matches("^.$", "\"\\u0085\""), "NEXT LINE is not an ECMA-262 line terminator")
    assertTrue(matches("^.$", "\"a\""), ". should match an ordinary character")
  }

  @Test def v_escape_is_the_vertical_tab_only(): Unit = {
    assertTrue(matches("^\\\\v$", "\"\\u000b\""), "\\v should match a vertical tab")
    assertFalse(matches("^\\\\v$", "\"\\n\""), "\\v should not match a line feed")
    assertFalse(matches("^\\\\v$", "\"\\f\""), "\\v should not match a form feed")
    assertFalse(matches("^[^\\\\v]$", "\"\\u000b\""), "[^\\v] should exclude the vertical tab")
    assertTrue(matches("^[^\\\\v]$", "\"\\n\""), "[^\\v] should still admit a line feed")
  }

  @Test def s_matches_ecma_262_whitespace_set(): Unit = {
    assertTrue(matches("^\\\\s$", "\" \""), "ASCII space should match \\s")
    assertTrue(matches("^\\\\s$", "\"\\u00a0\""), "latin-1 non-breaking-space should match \\s")
    assertTrue(matches("^\\\\s$", "\"\\ufeff\""), "zero-width whitespace (BOM) should match \\s")
    assertTrue(matches("^\\\\s$", "\"\\u2029\""), "paragraph separator should match \\s")
    assertTrue(matches("^\\\\s$", "\"\\u2003\""), "EM SPACE should match \\s")
    assertFalse(matches("^\\\\s$", "\"a\""), "a should not match \\s")
  }

  @Test def cap_s_matches_everything_but_ecma_262_whitespace(): Unit = {
    assertFalse(matches("^\\\\S$", "\"\\u00a0\""), "latin-1 non-breaking-space should not match \\S")
    assertFalse(matches("^\\\\S$", "\"\\ufeff\""), "zero-width whitespace (BOM) should not match \\S")
    assertFalse(matches("^\\\\S$", "\"\\u2029\""), "paragraph separator should not match \\S")
    assertTrue(matches("^\\\\S$", "\"a\""), "a should match \\S")
  }

  /** A negated set isn't expressible by union, so `\S` in a class becomes a nested class. */
  @Test def cap_s_inside_a_class_keeps_the_ecma_262_whitespace_set(): Unit = {
    assertFalse(matches("^[\\\\S]$", "\"\\u00a0\""), "[\\S] should exclude non-breaking-space")
    assertFalse(matches("^[a\\\\S]$", "\"\\u3000\""), "[a\\S] should exclude IDEOGRAPHIC SPACE")
    assertTrue(matches("^[a\\\\S]$", "\"b\""), "[a\\S] should still match an ordinary character")
    assertTrue(matches("^[^\\\\S]$", "\"\\ufeff\""), "[^\\S] should match zero-width whitespace")
    assertTrue(matches("^[^a\\\\S]$", "\"\\u2003\""), "[^a\\S] should match EM SPACE")
    assertFalse(matches("^[^a\\\\S]$", "\"a\""), "[^a\\S] should exclude a")
  }

  @Test def d_and_w_stay_ascii_alongside_a_unicode_property(): Unit = {
    // \p{digit} is Nd, which is Unicode; \d and \w are ASCII-only in ECMA-262 always. Compiling
    // the pattern under UNICODE_CHARACTER_CLASS to recognize the former would widen the latter.
    assertTrue(matches("^\\\\p{digit}\\\\d$", "\"44\""), "\\d should match an ASCII digit")
    assertFalse(matches("^\\\\p{digit}\\\\d$", "\"4\\u09ea\""), "\\d should not match a Bengali digit")
    assertTrue(matches("^\\\\p{digit}\\\\D$", "\"4\\u09ea\""), "\\D should match a Bengali digit")
    assertFalse(matches("^\\\\p{digit}\\\\w$", "\"4\\u00e9\""), "\\w should not match e-acute")
  }

  @Test def empty_classes_are_legal(): Unit = {
    assertFalse(matches("^[]$", "\"a\""), "[] should never match")
    assertFalse(matches("[]", "\"\""), "[] should never match, even the empty string")
    assertTrue(matches("^[^]$", "\"a\""), "[^] should match anything")
    assertTrue(matches("^[^]$", "\"\\n\""), "[^] should match a line terminator too")
  }

  @Test def class_metacharacters_java_has_and_ecma_262_does_not(): Unit = {
    assertTrue(matches("^[a[]$", "\"[\""), "[ inside a class is a literal, not a nested class")
    assertTrue(matches("^[a&&b]$", "\"&\""), "&& is two literals, not a class intersection")
    assertTrue(matches("^[a&&b]$", "\"a\""), "[a&&b] should match a")
    assertFalse(matches("^[a&&b]$", "\"c\""), "[a&&b] should not match c")
  }

  // ---------------------------------------------------------------- escapes

  @Test def control_letter_escape_is_case_insensitive(): Unit = {
    assertTrue(matches("^\\\\cC$", "\"\\u0003\""), "\\cC should match control-C")
    assertTrue(matches("^\\\\cc$", "\"\\u0003\""), "\\cc should match control-C too")
  }

  @Test def escapes_java_spells_differently(): Unit = {
    assertTrue(matches("^\\\\0$", "\"\\u0000\""), "\\0 should match NUL")
    assertTrue(matches("^[\\\\b]$", "\"\\u0008\""), "\\b inside a class is a backspace")
    assertTrue(matches("^\\\\u{1F600}$", "\"\\ud83d\\ude00\""), "\\u{...} should match its code point")
  }

  /** A literal backslash is not the start of an escape; only a real escape is rewritten. */
  @Test def escapes_are_only_rewritten_where_they_are_escapes(): Unit = {
    assertTrue(matches("^\\\\\\\\cc$", "\"\\\\cc\""), "\\\\cc is a backslash then cc")
    assertTrue(matches("^\\\\\\\\s$", "\"\\\\s\""), "\\\\s is a backslash then s")
    assertFalse(matches("^\\\\\\\\cc$", "\"\\u0003\""), "\\\\cc is not the control-C escape")
  }

  // ---------------------------------------------------------------- property escapes

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
    assertTrue(isValid(v, "\"\\u09ea\""), "a Bengali digit should match \\p{digit}")
    assertFalse(isValid(v, "\"a\""), "a should not match \\p{digit}")
  }

  @Test def general_category_in_its_long_spelling(): Unit = {
    assertTrue(matches("^\\\\p{General_Category=Letter}$", "\"a\""), "General_Category=Letter should match a")
    assertFalse(matches("^\\\\p{General_Category=Letter}$", "\"1\""), "General_Category=Letter should not match 1")
    assertTrue(matches("^\\\\p{gc=Nd}$", "\"1\""), "gc=Nd should match a digit")
  }

  /** These collide with POSIX class names java.util.regex reads as ASCII-only. */
  @Test def binary_properties_are_unicode_not_ascii(): Unit = {
    assertTrue(matches("^\\\\p{Alpha}$", "\"\\u00e9\""), "\\p{Alpha} should match e-acute")
    assertTrue(matches("^\\\\p{Lower}$", "\"\\u00e9\""), "\\p{Lower} should match e-acute")
    assertTrue(matches("^\\\\p{Upper}$", "\"\\u00c9\""), "\\p{Upper} should match E-acute")
    assertTrue(matches("^\\\\p{space}$", "\"\\u2003\""), "\\p{space} should match EM SPACE")
    assertTrue(matches("^\\\\p{White_Space}$", "\"\\u00a0\""), "\\p{White_Space} should match NBSP")
  }

  @Test def script_and_any_properties(): Unit = {
    assertTrue(matches("^\\\\p{Script=Greek}$", "\"\\u03b1\""), "Script=Greek should match alpha")
    assertFalse(matches("^\\\\p{Script=Greek}$", "\"a\""), "Script=Greek should not match a")
    assertTrue(matches("^\\\\p{Any}$", "\"a\""), "\\p{Any} should match anything")
    assertFalse(matches("^\\\\P{Any}$", "\"a\""), "\\P{Any} should match nothing")
  }

  // ---------------------------------------------------------------- format: regex

  @Test def format_regex_accepts_patterns_that_only_compile_after_translation(): Unit = {
    // `format: regex` asks whether the string is a valid *ECMA-262* pattern, so it has to answer
    // for the translated form: these are all rejected by java.util.regex as written.
    assertTrue(isValidPattern("\\\\p{Letter}cole"), "\\p{Letter} is a valid ECMA-262 pattern")
    assertTrue(isValidPattern("^\\\\p{digit}+$"), "\\p{digit} is accepted by ECMA-262 engines")
    assertTrue(isValidPattern("^\\\\cc$"), "\\c + lowercase letter is a valid ECMA-262 control escape")
    assertTrue(isValidPattern("^[]$"), "[] is a valid ECMA-262 class")
    assertTrue(isValidPattern("^\\\\p{Emoji}$"), "\\p{Emoji} is a valid ECMA-262 property")
    assertFalse(isValidPattern("["), "an unbalanced class is still invalid")
  }

  @Test def format_regex_rejects_java_only_constructs(): Unit = {
    assertFalse(isValidPattern("\\\\a"), "\\a is not a valid ECMA-262 escape")
    assertFalse(isValidPattern("\\\\Qa+b\\\\E"), "\\Q...\\E is not ECMA-262")
    assertFalse(isValidPattern("\\\\Aabc"), "\\A is not ECMA-262")
    assertFalse(isValidPattern("abc\\\\z"), "\\z is not ECMA-262")
    assertFalse(isValidPattern("\\\\h"), "\\h is not ECMA-262")
    assertFalse(isValidPattern("\\\\R"), "\\R is not ECMA-262")
    assertFalse(isValidPattern("\\\\X"), "\\X is not ECMA-262")
    assertFalse(isValidPattern("^a*+$"), "possessive quantifiers are not ECMA-262")
    assertFalse(isValidPattern("(?i)abc"), "inline flags are not ECMA-262")
    assertFalse(isValidPattern("(?>a+)b"), "atomic groups are not ECMA-262")
    assertFalse(isValidPattern("^\\\\p{IsLatin}$"), "\\p{Is...} is a java.util.regex spelling")
  }

  /** No java.util.regex equivalent, so they are reported invalid rather than approximated. */
  @Test def format_regex_rejects_properties_java_cannot_express(): Unit = {
    assertFalse(isValidPattern("^\\\\p{Math}$"), "\\p{Math} has no java.util.regex equivalent")
    assertFalse(isValidPattern("^\\\\p{ID_Start}$"), "\\p{ID_Start} has no java.util.regex equivalent")
    assertFalse(isValidPattern("^\\\\p{Script_Extensions=Greek}$"), "Script_Extensions has no equivalent")
  }

  @Test def format_regex_and_pattern_agree(): Unit = {
    // Whatever `format: regex` rejects, `pattern` must refuse to compile, and vice versa.
    assertThrows(classOf[java.util.regex.PatternSyntaxException],
      () => { mkValidator("""{"pattern": "\\Qa+b\\E"}"""); () },
      "a pattern format: regex calls invalid must not compile")
    assertTrue(isValid(mkValidator("""{"pattern": "\\p{Emoji}"}"""), "\"\\ud83d\\ude00\""),
      "a pattern format: regex calls valid must compile")
  }

  @Test def pattern_without_property_escapes_is_unaffected(): Unit = {
    val v = mkValidator("""{"pattern": "^[a-z]+$"}""")
    assertTrue(isValid(v, "\"abc\""))
    assertFalse(isValid(v, "\"ABC\""))
  }
}
