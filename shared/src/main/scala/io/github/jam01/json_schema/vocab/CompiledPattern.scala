/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

/**
 * A compiled `pattern`/`patternProperties` regex, abstracting over the platform-specific engine
 * `RegexSupport.compilePattern` compiles against (`java.util.regex.Pattern` on the JVM, the
 * native `RegExp` engine on Scala.js — see `RegexSupport`'s per-platform implementations).
 *
 * `matches` is substring/unanchored, matching how JSON Schema's `pattern` keyword is specified
 * (equivalent to ECMA-262's `RegExp.prototype.test`, not a whole-string match).
 */
private[vocab] trait CompiledPattern {
  def matches(s: CharSequence): Boolean
}
