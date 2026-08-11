/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

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

  def compilePattern(s: String): CompiledPattern = new JsCompiledPattern(new js.RegExp(s, "u"))

  /** True if `s` is a syntactically valid ECMA-262 pattern, for `format: regex`. */
  def isValidPattern(s: String): Boolean =
    try { new js.RegExp(s, "u"); true }
    catch { case _: js.JavaScriptException => false }
}
