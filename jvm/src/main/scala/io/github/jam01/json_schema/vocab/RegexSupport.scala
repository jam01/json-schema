/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

import scala.util.matching.Regex
import java.util.regex.PatternSyntaxException

/**
 * JVM regex compilation for the `pattern` and `patternProperties` keywords (and validity
 * checking for `format: regex`), backed by `java.util.regex.Pattern`.
 *
 * JSON Schema specifies ECMA-262 regex semantics, which `java.util.regex.Pattern` diverges from
 * in a few well-defined, bounded ways. Each divergence below is translated away from the pattern
 * string before compiling, rather than attempting a general ECMA-262 engine — translation keeps
 * the fix scoped to what the official test suite actually exercises (verified directly against a
 * real JDK, `java.util.regex.Pattern`). Contrast the Scala.js target's `RegexSupport`, which
 * forwards straight to the native `RegExp` engine and needs none of this, since that engine
 * already implements ECMA-262 exactly.
 *
 *  1. `\p{...}` Unicode property escapes: ECMA-262 allows long-form `General_Category` names
 *     (e.g. `\p{Letter}`), but Java only recognizes short codes (`\p{L}`) or `Is`-prefixed
 *     aliases (`\p{IsLetter}`) — never bare long-form names. Translated via the standard alias
 *     table from the Unicode standard / ECMA-262 spec
 *     (https://tc39.es/ecma262/#table-unicode-general-category-values).
 *  2. `\c<letter>` control-letter escapes: ECMA-262 computes the control code via
 *     `charCode & 0x1F` (case-insensitive), Java via `charCode ^ 0x40` (effectively
 *     uppercase-only) — the two agree only for uppercase letters. Lowercase letters after `\c`
 *     are upper-cased before compiling so Java computes the same code ECMA-262 would.
 *  3. `\s`/`\S`: ECMA-262's whitespace class (WhiteSpace + LineTerminator) is a fixed, wider set
 *     than what Java's `\s` matches even with `UNICODE_CHARACTER_CLASS` — notably it includes
 *     `﻿` (zero-width no-break space), which Unicode's own `White_Space` property excludes.
 *     Both are expanded to ECMA-262's exact set — tab/LF/VT/FF/CR, `\p{Z}` (Unicode `Separator`,
 *     i.e. `Zs`+`Zl`+`Zp`, covering space, NBSP, EM SPACE and the line/paragraph separators), and
 *     `﻿` — as an explicit Java character class, or, for a `\s` already inside a `[...]`
 *     class, as the bare member list to union in. The one case left untranslated is `\S` *inside*
 *     a `[...]` class: a negated set isn't expressible as a union there. It keeps Java's `\S`,
 *     which — being the complement of Java's narrower `\s` — is *wider* than ECMA-262's, so e.g.
 *     `[\S]` over-matches NBSP and `﻿`. No pattern in the official suite exercises it.
 *
 * Separately, some property names are recognized only under `UNICODE_CHARACTER_CLASS`, and others
 * are recognized either way but match ASCII-only semantics without it. Rather than sort out which
 * is which per name, compile plain first and only retry with the flag — via the `(?U)` embedded
 * flag expression, since `scala.util.matching.Regex`'s public constructor only accepts a pattern
 * string, not a pre-built `java.util.regex.Pattern` — if that fails. `\p{digit}` is an example of
 * the first kind: plain compilation throws `Unknown character property name {digit}` (Java's POSIX
 * spelling is the capitalized `\p{Digit}`, which is ASCII-only), and the `(?U)` retry both compiles
 * it and gives it the Unicode semantics the suite's Bengali-digit case expects. Keeping the retry
 * second means the overwhelming majority of patterns stay on the plain path, which matters because
 * `UNICODE_CHARACTER_CLASS` also broadens `\d`/`\w` beyond ECMA-262 (both are ASCII-only in
 * ECMA-262, always).
 *
 * OPEN (deferred to a dedicated spike, do not re-litigate piecemeal): this translation table grew
 * reactively, one suite failure at a time. Two questions to settle in one pass — (a) is there a
 * known, maintained ECMA-262 → `java.util.regex` translation to adopt instead of hand-rolling, or
 * an embeddable ECMA-262 engine cheap enough to depend on (Graal's TRegex, Joni in ECMAScript
 * syntax mode) that other JVM validators have converged on; and (b) is there a de-facto accepted
 * compatibility bound implementations agree to stop at, so the remaining divergences can be
 * declared out of scope rather than discovered one at a time. Until then the rule is: translate
 * only what the official suite exercises, and document what is left.
 */
private[vocab] object RegexSupport {
  // ECMA-262 long-form Unicode General_Category alias -> Java short code.
  private val GeneralCategoryAliases: Map[String, String] = Map(
    "Cased_Letter" -> "LC", "Close_Punctuation" -> "Pe", "Connector_Punctuation" -> "Pc",
    "Control" -> "Cc", "Currency_Symbol" -> "Sc", "Dash_Punctuation" -> "Pd",
    "Decimal_Number" -> "Nd", "Enclosing_Mark" -> "Me", "Final_Punctuation" -> "Pf",
    "Format" -> "Cf", "Initial_Punctuation" -> "Pi", "Letter" -> "L", "Letter_Number" -> "Nl",
    "Line_Separator" -> "Zl", "Lowercase_Letter" -> "Ll", "Mark" -> "M", "Math_Symbol" -> "Sm",
    "Modifier_Letter" -> "Lm", "Modifier_Symbol" -> "Sk", "Nonspacing_Mark" -> "Mn",
    "Number" -> "N", "Open_Punctuation" -> "Ps", "Other" -> "C", "Other_Letter" -> "Lo",
    "Other_Number" -> "No", "Other_Punctuation" -> "Po", "Other_Symbol" -> "So",
    "Paragraph_Separator" -> "Zp", "Private_Use" -> "Co", "Punctuation" -> "P",
    "Separator" -> "Z", "Space_Separator" -> "Zs", "Spacing_Mark" -> "Mc",
    "Surrogate" -> "Cs", "Symbol" -> "S", "Titlecase_Letter" -> "Lt",
    "Unassigned" -> "Cn", "Uppercase_Letter" -> "Lu",
  )

  private val PropertyEscape: Regex = raw"\\([pP])\{([A-Za-z_]+)\}".r

  private def translateAliases(s: String): String =
    PropertyEscape.replaceAllIn(s, m =>
      Regex.quoteReplacement(GeneralCategoryAliases.get(m.group(2)).fold(m.matched)(short => s"\\${m.group(1)}{$short}")))

  private val ControlEscape: Regex = raw"\\c([a-zA-Z])".r

  private def translateControlEscapes(s: String): String =
    ControlEscape.replaceAllIn(s, m => Regex.quoteReplacement("\\c" + m.group(1).toUpperCase))

  // tab, LF, VT, FF, CR, Unicode Separator (Zs+Zl+Zp), ZWNBSP — exactly ECMA-262's \s set.
  private val EcmaWhitespaceMembers = raw"\t\n\x0B\f\r\p{Z}\uFEFF"

  /**
   * Expands standalone (not already inside a `[...]` class) `\s`/`\S` to an explicit class with
   * ECMA-262's exact whitespace set. `\s`/`\S` found nested inside a `[...]` class are left as-is
   * — translating a negated class there isn't expressible via simple union, and no test in the
   * official suite exercises that nesting — so they keep Java's narrower (but not wrong, just
   * incomplete) semantics.
   */
  private def translateWhitespaceEscapes(s: String): String = {
    val sb = new StringBuilder(s.length)
    var inClass = false
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c == '\\' && i + 1 < s.length) {
        s.charAt(i + 1) match {
          case 's' => sb.append(if (inClass) EcmaWhitespaceMembers else s"[$EcmaWhitespaceMembers]")
          case 'S' if !inClass => sb.append(s"[^$EcmaWhitespaceMembers]")
          case other => sb.append(c).append(other)
        }
        i += 2
      } else {
        if (c == '[' && !inClass) inClass = true
        else if (c == ']' && inClass) inClass = false
        sb.append(c)
        i += 1
      }
    }
    sb.result()
  }

  private def translate(s: String): String =
    translateWhitespaceEscapes(translateControlEscapes(translateAliases(s)))

  private final class JavaCompiledPattern(rgx: Regex) extends CompiledPattern {
    def matches(s: CharSequence): Boolean = rgx.matches(s)
  }

  def compilePattern(s: String): CompiledPattern = {
    val translated = translate(s)
    val rgx = try new Regex(translated).unanchored
      catch {
        case _: PatternSyntaxException => new Regex(s"(?U)$translated").unanchored
      }
    new JavaCompiledPattern(rgx)
  }

  // `\a` (alert/bell) is a Java-recognized escape with no ECMA-262 equivalent — under the
  // Unicode-mode semantics `\p{...}` support above already commits this implementation to, it's
  // a JS SyntaxError, but `java.util.regex.Pattern` compiles it without complaint. Checked
  // heuristically (odd number of preceding backslashes) rather than via a full escape-aware scan,
  // matching this file's existing bounded-not-exhaustive approach.
  private val JavaOnlyBellEscape: Regex = raw"(?<!\\)(?:\\\\)*\\a".r

  /** True if `s` is a syntactically valid ECMA-262 pattern, for `format: regex`. */
  def isValidPattern(s: String): Boolean = {
    if (JavaOnlyBellEscape.findFirstIn(s).isDefined) false
    else {
      val translated = translate(s)
      try { new Regex(translated); true }
      catch {
        case _: PatternSyntaxException =>
          try { new Regex(s"(?U)$translated"); true }
          catch { case _: PatternSyntaxException => false }
      }
    }
  }
}
