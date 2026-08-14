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
 * JSON Schema specifies ECMA-262 regex semantics, which `java.util.regex.Pattern` is not. The
 * pattern is rewritten into the equivalent `java.util.regex` construct before compiling, rather
 * than embedding an ECMA-262 engine — see [[https://github.com/jam01/json-schema/blob/main/docs/decisions/009-ecma-262-regex.md decision-009]]
 * for the alternatives measured and rejected. Contrast the Scala.js target's `RegexSupport`,
 * which forwards straight to the native `RegExp` engine and needs none of this.
 *
 * The dialect targeted is ECMA-262 **under the `u` flag**, which is the one Scala.js compiles and
 * the one the suite's `\p{...}` cases require — the two modes of the ECMA-262 grammar are mutually
 * exclusive, and Annex B's legacy spellings are what `u` gives up to get property escapes.
 *
 * [[translate]] is a single escape- and class-aware scan, which is what makes the rewrites below
 * safe: a construct is only rewritten where it actually has that meaning, so a literal backslash
 * followed by `p{Letter}` or `cc` stays literal. It rewrites:
 *
 *  1. `$` to `\z`. `java.util.regex`'s `$` also matches *before* a final line terminator, so
 *     `^abc$` matches `"abc\n"`; ECMA-262's matches at the very end of input only. JSON Schema
 *     patterns never carry flags, so multiline `$` never applies. (`^` needs no rewrite: without
 *     `MULTILINE` it already means start-of-input, exactly as in ECMA-262.)
 *  2. `.` to an explicit class. Both dialects exclude the line terminators from `.`, but
 *     `java.util.regex`'s set additionally contains U+0085 NEXT LINE, which ECMA-262's does not.
 *  3. `\v` to `\x0B`. In ECMA-262 it is the vertical tab; in `java.util.regex` it is a class of
 *     every vertical whitespace character, including `\n`.
 *  4. `\s`/`\S` to ECMA-262's exact set — tab/LF/VT/FF/CR, `\p{Z}` (`Zs`+`Zl`+`Zp`, covering
 *     space, NBSP, EM SPACE and the line/paragraph separators) and `\uFEFF`, which Unicode's own
 *     `White_Space` property excludes so no `java.util.regex` class spells it. `\S` becomes a
 *     nested negated class, which unions correctly inside a positive class (`[a\S]`) and, being a
 *     single member, negates correctly inside a negated one (`[^a\S]`).
 *  5. `\p{...}` property names, via [[property]].
 *  6. `\c<letter>` to the code point it denotes. ECMA-262 computes it as `charCode & 0x1F`,
 *     `java.util.regex` as `charCode ^ 0x40`; the two agree only for uppercase letters.
 *  7. `\0`, `\u{...}` and `[\b]` to the `java.util.regex` spellings of the same code points
 *     (`\x00`, `\x{...}`, `\x08`), none of which `java.util.regex` accepts as written.
 *  8. `[]` and `[^]` — legal in ECMA-262, where they never and always match respectively, and a
 *     syntax error in `java.util.regex` — to an empty and a universal class.
 *  9. `[` and `&` inside a class to literals. ECMA-262 has neither nested classes nor `&&`
 *     intersection, so `[a&&b]` is the three characters `a`, `&`, `b`; left alone,
 *     `java.util.regex` reads it as an intersection and matches nothing.
 *
 * Constructs that `java.util.regex` accepts and ECMA-262 under `u` does not are rejected with a
 * `PatternSyntaxException`:
 *
 *  - any `\<char>` that is not a defined escape. Under `u` an identity escape may only take a
 *    SyntaxCharacter or `/` (and `-` inside a class), so this covers `java.util.regex`'s own
 *    `\a \e \A \z \Z \G \h \H \R \X \N{...} \Q...\E` and equally `\-`, `\ ` and `\%`.
 *  - `\0` followed by a digit, and any `\<digits>` naming a group the pattern does not define
 *    before it. Both are legacy octal escapes without `u`; `java.util.regex` reads them as
 *    backreferences and then never matches, which is a wrong answer rather than an error.
 *  - a literal `]` or `}`, and a `{` opening no quantifier — punctuation under `u`, characters to
 *    `java.util.regex`.
 *  - possessive quantifiers (`a*+`) and the non-ECMA-262 group forms (`(?i)`, `(?x)`, `(?>...)`).
 *  - a character-class range with `\d \D \w \W \s \S` or a `\p{...}`/`\P{...}` property escape as
 *    either endpoint (`[\d-z]`, `[a-\s]`). `java.util.regex` accepts these as the union of the
 *    class and the two characters flanking the `-`, silently discarding the intended range.
 *
 * Rejecting rather than passing them through is what lets `format: regex` answer for the same
 * language `pattern` compiles: a string is valid `format: regex` exactly when `pattern` accepts it.
 *
 * The whole scan runs without `UNICODE_CHARACTER_CLASS`. That flag is a property of the entire
 * pattern, so switching it on to recognize one property name also widens `\d`/`\w`/`\b` in the
 * rest of it — and in ECMA-262 those are always ASCII-only. Mapping each property name explicitly
 * keeps the two independent.
 *
 * What is left is written down in README § Regular expressions, and is now entirely constructs
 * `java.util.regex` cannot express: the ECMA-262 binary properties and `\p{Script_Extensions=...}`
 * with no equivalent, non-alphanumeric group names, and forward references. All are rejected, so
 * every remaining divergence is an error rather than a silent wrong match.
 */
private[vocab] object RegexSupport {
  /**
   * ECMA-262 `General_Category` values — long names, short codes, and the additional value
   * aliases Unicode's `PropertyValueAliases.txt` defines — to the short code `java.util.regex`
   * knows. `java.util.regex` recognizes short codes and `Is`-prefixed aliases, never bare
   * long-form names like `Letter`.
   */
  private val GeneralCategories: Map[String, String] = Map(
    "Cased_Letter" -> "LC", "Close_Punctuation" -> "Pe", "Connector_Punctuation" -> "Pc",
    "Control" -> "Cc", "cntrl" -> "Cc", "Currency_Symbol" -> "Sc", "Dash_Punctuation" -> "Pd",
    "Decimal_Number" -> "Nd", "digit" -> "Nd", "Enclosing_Mark" -> "Me",
    "Final_Punctuation" -> "Pf", "Format" -> "Cf", "Initial_Punctuation" -> "Pi",
    "Letter" -> "L", "Letter_Number" -> "Nl", "Line_Separator" -> "Zl",
    "Lowercase_Letter" -> "Ll", "Mark" -> "M", "Combining_Mark" -> "M", "Math_Symbol" -> "Sm",
    "Modifier_Letter" -> "Lm", "Modifier_Symbol" -> "Sk", "Nonspacing_Mark" -> "Mn",
    "Number" -> "N", "Open_Punctuation" -> "Ps", "Other" -> "C", "Other_Letter" -> "Lo",
    "Other_Number" -> "No", "Other_Punctuation" -> "Po", "Other_Symbol" -> "So",
    "Paragraph_Separator" -> "Zp", "Private_Use" -> "Co", "Punctuation" -> "P", "punct" -> "P",
    "Separator" -> "Z", "Space_Separator" -> "Zs", "Spacing_Mark" -> "Mc",
    "Surrogate" -> "Cs", "Symbol" -> "S", "Titlecase_Letter" -> "Lt",
    "Unassigned" -> "Cn", "Uppercase_Letter" -> "Lu",
  ) ++ "L LC Lu Ll Lt Lm Lo M Mn Mc Me N Nd Nl No P Pc Pd Ps Pe Pi Pf Po S Sm Sc Sk So Z Zs Zl Zp C Cc Cf Cs Co Cn"
    .split(' ').map(short => short -> short)

  /**
   * The ECMA-262 binary property names, and their aliases, that `java.util.regex` can express
   * exactly. Several of these — `Alpha`, `Lower`, `Upper`, `space` — collide with POSIX class
   * names `java.util.regex` reads as ASCII-only, so leaving them alone is a silent wrong match
   * rather than an error. The emoji properties require a JDK 21 runtime, which is this library's
   * floor. ECMA-262's remaining binary properties have no `java.util.regex` equivalent and are
   * rejected; approximating them with a near-miss class would trade an error for a wrong answer.
   */
  private val BinaryProperties: Map[String, String] = Map(
    "ASCII" -> "ASCII",
    "Alphabetic" -> "IsAlphabetic", "Alpha" -> "IsAlphabetic",
    "Assigned" -> "IsAssigned",
    "Emoji" -> "IsEmoji",
    "Emoji_Component" -> "IsEmoji_Component", "EComp" -> "IsEmoji_Component",
    "Emoji_Modifier" -> "IsEmoji_Modifier", "EMod" -> "IsEmoji_Modifier",
    "Emoji_Modifier_Base" -> "IsEmoji_Modifier_Base", "EBase" -> "IsEmoji_Modifier_Base",
    "Emoji_Presentation" -> "IsEmoji_Presentation", "EPres" -> "IsEmoji_Presentation",
    "Extended_Pictographic" -> "IsExtended_Pictographic", "ExtPict" -> "IsExtended_Pictographic",
    "Ideographic" -> "IsIdeographic", "Ideo" -> "IsIdeographic",
    "Join_Control" -> "IsJoin_Control", "Join_C" -> "IsJoin_Control",
    "Lowercase" -> "IsLowercase", "Lower" -> "IsLowercase",
    "Noncharacter_Code_Point" -> "IsNoncharacter_Code_Point", "NChar" -> "IsNoncharacter_Code_Point",
    "Uppercase" -> "IsUppercase", "Upper" -> "IsUppercase",
    "White_Space" -> "IsWhite_Space", "space" -> "IsWhite_Space",
  )

  /**
   * Escapes ECMA-262 defines, beyond the ones [[translate]] rewrites case by case. `\b` is the
   * word boundary here; inside a class it is the backspace and is rewritten before this is reached.
   */
  private val KnownEscapes = "dDwWfnrtbBkxu"

  /**
   * ECMA-262's SyntaxCharacter. Under `u` these, plus `/`, are the only characters an identity
   * escape may take - `\-` inside a class is the one addition, and every other `\<char>` is a
   * syntax error rather than the character itself.
   */
  private val SyntaxCharacters = "^$\\.*+?()[]{}|"

  /** ECMA-262's `\s`: tab, LF, VT, FF, CR, Unicode `Separator` and ZWNBSP. */
  private val Whitespace = raw"\t\n\x0B\f\r\p{Z}\uFEFF"

  /** ECMA-262's `.`: anything but the four LineTerminator code points. */
  private val Dot = raw"[^\n\r\u2028\u2029]"

  private val AnyCodePoint = raw"\x00-\x{10FFFF}"

  private def isAsciiLetter(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  // ECMA-262's DecimalDigit is ASCII, so `Char.isDigit` would be too broad here.
  private def isAsciiDigit(c: Char): Boolean = c >= '0' && c <= '9'

  /**
   * The `java.util.regex` class body for an ECMA-262 property name, or `None` where no equivalent
   * exists. `Script=`/`sc=` pass through, which `java.util.regex` understands as written;
   * `Script_Extensions=` has no equivalent, and the plain script of the same name is a different
   * set.
   */
  private def property(name: String): Option[String] = name.indexOf('=') match {
    case -1 => GeneralCategories.get(name).orElse(BinaryProperties.get(name))
    case eq =>
      val (key, value) = (name.substring(0, eq), name.substring(eq + 1))
      if (key == "General_Category" || key == "gc") GeneralCategories.get(value)
      else if (key == "Script" || key == "sc") Some(name)
      else None
  }

  /**
   * The index just past a `{n}`, `{n,}` or `{n,m}` quantifier opening at `at`, or -1 if the brace
   * opens no quantifier - which under `u` is a syntax error rather than a literal brace.
   */
  private def quantifierEnd(s: String, at: Int): Int = {
    def digits(from: Int): Int = {
      var i = from
      while (i < s.length && isAsciiDigit(s.charAt(i))) i += 1
      i
    }

    var i = digits(at + 1)
    if (i == at + 1) return -1 // {} or {,3}: ECMA-262 requires the lower bound
    if (i < s.length && s.charAt(i) == ',') i = digits(i + 1)
    if (i < s.length && s.charAt(i) == '}') i + 1 else -1
  }

  /** Rewrites an ECMA-262 pattern into the equivalent `java.util.regex` one. */
  private def translate(s: String): String = {
    def invalid(desc: String, at: Int): Nothing = throw new PatternSyntaxException(desc, s, at)

    val sb = new StringBuilder(s.length + 16)
    var inClass = false
    var i = 0
    var groups = 0    // capturing groups opened so far
    var badRef = 0    // the first `\N` naming none of them, and where it was
    var badRefAt = -1

    // Within a class, \d \D \w \W \s \S and \p{...}/\P{...} denote a set of code points rather
    // than one, so ECMA-262 forbids using any of them as either endpoint of a `-` range
    // (`[\d-z]`, `[a-\s]`); `java.util.regex` accepts both without complaint.
    var atClassStart = false        // true until the class's first atom is emitted
    var lastAtomClassEscape = false // whether the most recently emitted atom was one of the above
    def isClassEscapeStart(at: Int): Boolean =
      at + 1 < s.length && s.charAt(at) == '\\' && (
        "dDwWsS".indexOf(s.charAt(at + 1)) >= 0 ||
          ((s.charAt(at + 1) == 'p' || s.charAt(at + 1) == 'P') &&
            at + 2 < s.length && s.charAt(at + 2) == '{'))

    while (i < s.length) {
      val c = s.charAt(i)

      if (c == '\\') {
        if (i + 1 == s.length) invalid("Trailing backslash", i)
        val esc = s.charAt(i + 1)
        val at = i
        i += 2

        esc match {
          case 's' => sb.append(if (inClass) Whitespace else s"[$Whitespace]")
          case 'S' => sb.append(s"[^$Whitespace]")
          case 'v' => sb.append(raw"\x0B")
          case 'b' if inClass => sb.append(raw"\x08")            // in a class, ECMA-262's backspace
          case '0' if i == s.length || !isAsciiDigit(s.charAt(i)) => sb.append(raw"\x00")
          case '0' => invalid("\\0 followed by a digit is a legacy octal escape", at)

          // A DecimalEscape is a backreference, never an octal escape: the octal spellings are
          // Annex B, which `u` withdraws. `java.util.regex` reads `\1` as a backreference too, so
          // this passes through - but only once the group it names is known to exist.
          case _ if esc >= '1' && esc <= '9' =>
            if (inClass) invalid(s"\\$esc is not a character class escape", at)
            var j = i
            while (j < s.length && isAsciiDigit(s.charAt(j))) j += 1
            val digits = s.substring(i - 1, j)
            i = j
            val num = digits.toIntOption.getOrElse(Int.MaxValue)
            if (num > groups && badRefAt < 0) { badRef = num; badRefAt = at }
            sb.append('\\').append(digits)

          case 'p' | 'P' =>
            if (i == s.length || s.charAt(i) != '{') invalid(s"\\$esc without a {name}", at)
            val close = s.indexOf('}', i)
            if (close < 0) invalid(s"Unclosed \\$esc{", at)
            val name = s.substring(i + 1, close)
            i = close + 1
            if (name == "Any") sb.append(if (esc == 'p') s"[$AnyCodePoint]" else s"[^$AnyCodePoint]")
            else property(name) match {
              case Some(body) => sb.append('\\').append(esc).append('{').append(body).append('}')
              case None => invalid(s"\\$esc{$name} has no java.util.regex equivalent", at)
            }

          case 'u' if i < s.length && s.charAt(i) == '{' =>
            val close = s.indexOf('}', i)
            if (close < 0) invalid("Unclosed \\u{", at)
            sb.append(raw"\x{").append(s.substring(i + 1, close)).append('}')
            i = close + 1

          case 'c' if i < s.length && isAsciiLetter(s.charAt(i)) =>
            sb.append("\\x%02X".format(s.charAt(i) % 32))
            i += 1
          case 'c' => invalid("\\c must be followed by a control letter", at)

          case _ if KnownEscapes.indexOf(esc.toInt) >= 0 => sb.append('\\').append(esc)
          case '-' if inClass => sb.append(raw"\-")
          case _ if SyntaxCharacters.indexOf(esc.toInt) >= 0 || esc == '/' => sb.append('\\').append(esc)
          case _ => invalid(s"\\$esc is not an ECMA-262 escape", at)
        }

        if (inClass) {
          lastAtomClassEscape = "dDwWsSpP".indexOf(esc.toInt) >= 0
          atClassStart = false
        }
      } else if (inClass) {
        c match {
          case ']' => inClass = false; sb.append(']')
          case '[' => sb.append(raw"\[")     // ECMA-262 has no nested classes
          case '&' => sb.append(raw"\&")     // nor `&&` intersection
          case '-' =>
            val leading = atClassStart
            val trailing = i + 1 < s.length && s.charAt(i + 1) == ']'
            if (!leading && !trailing && (lastAtomClassEscape || isClassEscapeStart(i + 1)))
              invalid("A character class range cannot have \\d, \\w, \\s or a Unicode property escape as an endpoint", i)
            sb.append('-')
          case _ => sb.append(c)
        }
        atClassStart = false
        lastAtomClassEscape = false
        i += 1
      } else {
        c match {
          case '[' =>
            if (i + 1 < s.length && s.charAt(i + 1) == ']') {
              sb.append(s"[^$AnyCodePoint]"); i += 2
            } else if (i + 2 < s.length && s.charAt(i + 1) == '^' && s.charAt(i + 2) == ']') {
              sb.append(s"[$AnyCodePoint]"); i += 3
            } else {
              inClass = true
              atClassStart = true
              lastAtomClassEscape = false
              sb.append('[')
              i += 1
              if (i < s.length && s.charAt(i) == '^') { sb.append('^'); i += 1 }
            }

          case '$' => sb.append(raw"\z"); i += 1
          case '.' => sb.append(Dot); i += 1

          // Under `u` a brace or a bracket is punctuation, not a character: one that closes
          // nothing, and one that opens no quantifier, are both syntax errors - where
          // `java.util.regex` takes the closers as literals.
          case ']' => invalid("A literal ] must be escaped", i)
          case '}' => invalid("A literal } must be escaped", i)

          case '{' =>
            val end = quantifierEnd(s, i)
            if (end < 0) invalid("{ opens no quantifier", i)
            sb.append(s.substring(i, end))
            i = end
            if (i < s.length && s.charAt(i) == '+')
              invalid("Possessive quantifiers are not ECMA-262", i)

          case '*' | '+' | '?' =>
            sb.append(c)
            i += 1
            if (i < s.length && s.charAt(i) == '+')
              invalid("Possessive quantifiers are not ECMA-262", i)

          case '(' =>
            sb.append(c)
            i += 1
            if (i < s.length && s.charAt(i) == '?') {
              val kind = if (i + 1 < s.length) s.charAt(i + 1) else ' '
              val after = if (i + 2 < s.length) s.charAt(i + 2) else ' '
              val lookbehind = kind == '<' && (after == '=' || after == '!')
              val named = kind == '<' && !lookbehind &&
                (after.isLetter || after == '_' || after == '$')
              if (!(kind == ':' || kind == '=' || kind == '!' || lookbehind || named))
                invalid(s"(?$kind is not an ECMA-262 group", i)
              if (named) groups += 1
            } else groups += 1

          case _ => sb.append(c); i += 1
        }
      }
    }

    if (inClass) invalid("Unclosed character class", s.length)
    // Deferred to here because a group the reference names may still have been ahead of it, which
    // separates the two verdicts: ECMA-262 permits a forward reference and `java.util.regex`
    // cannot express one, while a reference to a group the pattern never defines is invalid under
    // `u` (and an octal escape only under Annex B, which `u` withdraws).
    if (badRefAt >= 0)
      if (badRef <= groups) invalid(s"\\$badRef is a forward reference, which java.util.regex cannot express", badRefAt)
      else invalid(s"\\$badRef is a backreference to a group that does not exist", badRefAt)
    sb.result()
  }

  private final class JavaCompiledPattern(rgx: Regex) extends CompiledPattern {
    def matches(s: CharSequence): Boolean = rgx.matches(s)
  }

  def compilePattern(s: String): CompiledPattern =
    new JavaCompiledPattern(new Regex(translate(s)).unanchored)

  /** True if `s` is a syntactically valid ECMA-262 pattern, for `format: regex`. */
  def isValidPattern(s: String): Boolean =
    try { new Regex(translate(s)); true }
    catch { case _: PatternSyntaxException => false }
}
