/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

import java.util.regex.PatternSyntaxException
import scala.scalajs.js

/**
 * Scala.js regex compilation for the `pattern` and `patternProperties` keywords (and validity
 * checking for `format: regex`), backed directly by the native `RegExp` engine under the `u`
 * (Unicode) flag.
 *
 * JSON Schema specifies ECMA-262 regex semantics, and on this target the engine actually
 * executing the pattern already *is* an ECMA-262 engine — so unlike the JVM target's
 * `RegexSupport` (which has to patch several `java.util.regex.Pattern` divergences from
 * ECMA-262 by hand, e.g. `\p{Letter}`, `\s`/`\S`, `\c<letter>`), no translation is needed here at
 * all. The `u` flag specifically is what makes `\p{...}` Unicode property escapes valid syntax
 * in the first place (without it they're a `SyntaxError`) and is what the JVM-side fixes assume
 * patterns are compiled under; every `pattern`/`patternProperties` string in the official test
 * suite was confirmed (via a real Node process) to compile under `u` before relying on this.
 *
 * `RegExp#test` is inherently substring/unanchored, matching how JSON Schema's `pattern` keyword
 * is specified.
 */
private[vocab] object RegexSupport {
  private final class JsCompiledPattern(rgx: js.RegExp) extends CompiledPattern {
    def matches(s: CharSequence): Boolean = rgx.test(s.toString)
  }

  def compilePattern(s: String): CompiledPattern = new JsCompiledPattern(rgxOf(s))

  /** True if `s` is a syntactically valid ECMA-262 pattern, for `format: regex`. */
  def isValidPattern(s: String): Boolean =
    try { rgxOf(s); true }
    catch { case _: PatternSyntaxException => false }

  /**
   * `js.RegExp` reports a bad pattern as a JS `SyntaxError`, which arrives here as a
   * `js.JavaScriptException`. Both targets are compiled against by the same cross-platform code,
   * so an invalid `pattern` has to surface as the `PatternSyntaxException` the JVM target throws.
   */
  private def rgxOf(s: String): js.RegExp =
    try new js.RegExp(s, "u")
    catch case e: js.JavaScriptException => throw new PatternSyntaxException(e.getMessage, s, -1)
}
