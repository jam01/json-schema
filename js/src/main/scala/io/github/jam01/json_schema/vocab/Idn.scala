/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema.vocab

/**
 * Best-effort `idn-hostname` validator for Scala.js.
 *
 * The JVM target delegates to `com.networknt.schema.utils.RFC5892`, which validates against
 * IDNA 2008 + the full Unicode database. Porting that to Scala.js would require shipping the UCD,
 * so this implementation does *structural* validation only:
 *
 *  - total length 1..253 characters
 *  - each label is 1..63 characters and does not start or end with `-`
 *  - non-separator characters must be Unicode letters or digits, or `-`
 *  - no trailing dot
 *
 * NOT validated (silently accepted):
 *
 *  - IDNA 2008 character eligibility per RFC 5892 (e.g. discouraged scripts, contextual rules)
 *  - Punycode (`xn--…`) labels are not decoded; only ACE syntax is sanity-checked structurally
 *  - Bidi rules (RFC 5893)
 *
 * Label length is measured in UTF-16 code units; the RFC's 63-octet limit applies to the
 * Punycode-encoded form, which we don't compute. Sufficiently long Unicode labels may slip
 * through on JS where the JVM would reject them.
 *
 * For full conformance, use the JVM target.
 */
object Idn {
  def isHostname(s: String): Boolean = {
    if (s.isEmpty || s.length > 253) return false

    val len = s.length
    var labelLen = 0
    var labelStart = 0
    var prevHyphen = false
    var i = 0

    while (i < len) {
      val cp = s.codePointAt(i)
      val w = Character.charCount(cp)

      if (cp == '.') {
        if (labelLen == 0) return false  // empty label / leading or consecutive dot
        if (prevHyphen) return false     // label ends with '-'
        labelLen = 0
        labelStart = i + 1
        prevHyphen = false
      } else {
        labelLen += w
        if (labelLen > 63) return false
        if (cp == '-') {
          if (i == labelStart) return false // label starts with '-'
          prevHyphen = true
        } else if (Character.isLetterOrDigit(cp)) {
          prevHyphen = false
        } else {
          return false
        }
      }
      i += w
    }

    if (labelLen == 0) return false   // trailing dot
    if (prevHyphen) return false      // last label ends with '-'
    true
  }
}
