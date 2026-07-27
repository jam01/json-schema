/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

import scala.util.matching.Regex

/**
 * Shared regex compilation for the `pattern` and `patternProperties` keywords.
 *
 * JSON Schema specifies ECMA-262 regex semantics, under which `\p{...}` Unicode property
 * escapes (e.g. `\p{Letter}`) are valid. Java's `java.util.regex.Pattern` only recognizes
 * `\p{...}` Unicode property names when compiled with `UNICODE_CHARACTER_CLASS`, so that flag
 * is applied here rather than at each call site, via the `(?U)` embedded flag expression —
 * `scala.util.matching.Regex`'s public constructor only accepts a pattern string, not a
 * pre-built `java.util.regex.Pattern`, so the flag can't be passed programmatically.
 */
private[vocab] object RegexSupport {
  def compilePattern(s: String): Regex = new Regex(s"(?U)$s").unanchored
}
