/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

import scala.util.matching.Regex
import java.util.regex.PatternSyntaxException

/**
 * Shared regex compilation for the `pattern` and `patternProperties` keywords.
 *
 * JSON Schema specifies ECMA-262 regex semantics, under which `\p{...}` Unicode property
 * escapes (e.g. `\p{Letter}`) are valid. Some of those names (e.g. `\p{L}`, `\p{IsAlphabetic}`)
 * are already recognized by `java.util.regex.Pattern` with no flags; others (e.g. `\p{Letter}`)
 * are only recognized when compiled with `UNICODE_CHARACTER_CLASS`. Rather than guess which
 * names need it, compile plain first and only retry with the flag — via the `(?U)` embedded
 * flag expression, since `scala.util.matching.Regex`'s public constructor only accepts a
 * pattern string, not a pre-built `java.util.regex.Pattern` — if that fails. This keeps the
 * overwhelming majority of patterns on the plain (zero-flag) path, since `UNICODE_CHARACTER_CLASS`
 * also broadens `\d`/`\w`/`\s`/`\b` semantics, and on Scala.js additionally requires an ES2018+
 * linker target.
 */
private[vocab] object RegexSupport {
  def compilePattern(s: String): Regex = {
    try new Regex(s).unanchored
    catch {
      case _: PatternSyntaxException => new Regex(s"(?U)$s").unanchored
    }
  }
}
