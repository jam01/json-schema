/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

/**
 * Scala.js smoke test executed under Node after linking with sjsld.
 *
 * Plain `main` + hand-rolled checks rather than a test framework: JUnit 5 (used by the rest of
 * the suite) doesn't run on Scala.js, and pulling in a JS-compatible framework would require its
 * own linker/runner wiring. This is intentionally minimal — broader behavior is covered by the
 * JVM suite. Cases here target platform divergence between JVM and JS:
 *
 *   - `RegexSupport` — platform-specific implementations (JVM patches `java.util.regex.Pattern`
 *     divergences from ECMA-262 by hand; Scala.js forwards straight to the native `RegExp` engine)
 *   - `java.time.*` via scala-java-time
 *   - `Idn.isHostname` — platform-specific implementations
 *
 * Exit code is non-zero if any check fails.
 */
object Smoke {
  private var checks = 0
  private var failures = 0

  private def check(cond: Boolean, msg: String): Unit = {
    checks += 1
    if (!cond) {
      System.err.println(s"FAIL: $msg")
      failures += 1
    }
  }

  private def run(schemaJson: String, instance: ujson.Value, cfg: Config): OutputUnit = {
    // share a registry across from() and validator() so internal $ref to local $defs can resolve.
    val reg = new MutableRegistry
    val sch = from(ujson.Readable, ujson.Readable.fromString(schemaJson), registry = reg)
    try instance.transform(validator(sch, cfg, reg))
    catch { case exc: ValidationException => exc.result } // ffast=true throws on failure
  }

  private def basic(schemaJson: String, instance: ujson.Value): OutputUnit =
    run(schemaJson, instance, Config.Default)

  private def fmt(schemaJson: String, instance: ujson.Value): OutputUnit =
    run(schemaJson, instance, Config(dialect = Dialect.FormatAssertion))

  def main(args: Array[String]): Unit = {
    println("=== json-schema Scala.js smoke test ===")

    // basic type validation
    check( basic("""{"type":"string"}""", ujson.Str("hi")).vvalid,           "type:string accepts string")
    check(!basic("""{"type":"string"}""", ujson.Num(1)).vvalid,              "type:string rejects number")
    check( basic("""{"type":"integer"}""", ujson.Num(42)).vvalid,            "type:integer accepts integer")

    // number constraints
    check( basic("""{"type":"integer","multipleOf":3}""", ujson.Num(9)).vvalid,  "multipleOf:3 accepts 9")
    check(!basic("""{"type":"integer","multipleOf":3}""", ujson.Num(10)).vvalid, "multipleOf:3 rejects 10")

    // pattern: exercises RegexSupport's native js.RegExp backend
    check( basic("""{"type":"string","pattern":"^[a-z]+$"}""", ujson.Str("abc")).vvalid,  "pattern accepts match")
    check(!basic("""{"type":"string","pattern":"^[a-z]+$"}""", ujson.Str("ABC")).vvalid,  "pattern rejects non-match")

    // Canary that native js.RegExp (compiled under the "u" flag) gives the same ECMA-262 results
    // here as the JVM's hand-patched java.util.regex.Pattern does for the same cases.
    check( basic("""{"pattern":"\\p{Letter}cole"}""", ujson.Str("Nicole")).vvalid, "\\p{Letter} alias matches Nicole")
    check(!basic("""{"pattern":"\\p{Letter}cole"}""", ujson.Str("7cole")).vvalid,  "\\p{Letter} alias rejects 7cole")
    check( basic("""{"pattern":"^\\s$"}""", ujson.Str(" ")).vvalid,          "\\s matches latin-1 non-breaking-space")
    check( basic("""{"pattern":"^\\s$"}""", ujson.Str("﻿")).vvalid,          "\\s matches zero-width whitespace (BOM)")
    check(!basic("""{"pattern":"^\\S$"}""", ujson.Str(" ")).vvalid,          "\\S rejects latin-1 non-breaking-space")
    check( basic("""{"pattern":"^\\cc$"}""", ujson.Str("")).vvalid,         "\\cc matches control-C case-insensitively")
    check(!basic("""{"pattern":"^[\\S]$"}""", ujson.Str(" ")).vvalid,        "[\\S] rejects latin-1 non-breaking-space")
    check(!basic("""{"pattern":"^abc$"}""", ujson.Str("abc\n")).vvalid,      "$ rejects a trailing newline")
    check( basic("""{"pattern":"^.$"}""", ujson.Str("")).vvalid,             ". matches NEXT LINE")
    check(!basic("""{"pattern":"^\\v$"}""", ujson.Str("\n")).vvalid,         "\\v rejects a line feed")
    check(!basic("""{"pattern":"^[]$"}""", ujson.Str("a")).vvalid,           "[] never matches")
    check( basic("""{"pattern":"^[^]$"}""", ujson.Str("a")).vvalid,          "[^] matches anything")
    check( basic("""{"pattern":"^[a&&b]$"}""", ujson.Str("&")).vvalid,       "&& in a class is two literals")
    check( basic("""{"pattern":"^\\p{General_Category=Letter}$"}""", ujson.Str("a")).vvalid, "General_Category=Letter matches a")
    check( basic("""{"pattern":"^\\p{Alpha}$"}""", ujson.Str("é")).vvalid,   "\\p{Alpha} is Unicode, not ASCII")

    // format assertions
    check( fmt("""{"format":"date"}""",     ujson.Str("2024-01-15")).vvalid,      "format:date accepts valid")
    check(!fmt("""{"format":"date"}""",     ujson.Str("not-a-date")).vvalid,      "format:date rejects garbage")
    check( fmt("""{"format":"ipv4"}""",     ujson.Str("192.168.0.1")).vvalid,     "format:ipv4 accepts valid")
    check(!fmt("""{"format":"ipv4"}""",     ujson.Str("999.999.999.999")).vvalid, "format:ipv4 rejects invalid")
    check( fmt("""{"format":"hostname"}""", ujson.Str("example.com")).vvalid,     "format:hostname accepts valid")
    check(!fmt("""{"format":"hostname"}""", ujson.Str("hello world")).vvalid,     "format:hostname rejects space")

    // format:regex — RegexSupport.isValidPattern rejecting java.util.regex-only constructs
    check( fmt("""{"format":"regex"}""", ujson.Str("^[a-z]+$")).vvalid, "format:regex accepts valid pattern")
    check(!fmt("""{"format":"regex"}""", ujson.Str("\\a")).vvalid,      "format:regex rejects \\a (Java-only escape)")
    check(!fmt("""{"format":"regex"}""", ujson.Str("\\Qa+b\\E")).vvalid, "format:regex rejects \\Q...\\E (Java-only)")
    check( fmt("""{"format":"regex"}""", ujson.Str("^[]$")).vvalid,     "format:regex accepts the empty class")

    // An uncompilable `pattern` reports itself the same way on both targets: java.util.regex's
    // PatternSyntaxException, not the JS engine's own SyntaxError.
    check(
      try { basic("""{"pattern":"("}""", ujson.Str("a")); false }
      catch { case _: java.util.regex.PatternSyntaxException => true },
      "an invalid pattern throws PatternSyntaxException")

    // Annex B spellings, which the `u` dialect withdraws. Native RegExp refuses them here; the
    // canary is that the JVM's translation refuses the same ones rather than taking Java's reading.
    check(!fmt("""{"format":"regex"}""", ujson.Str("^\\101$")).vvalid, "format:regex rejects a legacy octal escape")
    check(!fmt("""{"format":"regex"}""", ujson.Str("^\\1$")).vvalid,   "format:regex rejects a dangling backreference")
    check(!fmt("""{"format":"regex"}""", ujson.Str("^}$")).vvalid,     "format:regex rejects an unescaped }")
    check(!fmt("""{"format":"regex"}""", ujson.Str("^\\-$")).vvalid,   "format:regex rejects the \\- identity escape")
    check( fmt("""{"format":"regex"}""", ujson.Str("^(a)\\1$")).vvalid, "format:regex accepts a real backreference")

    // idn-hostname: canary for the platform-specific Idn implementation
    check( fmt("""{"format":"idn-hostname"}""", ujson.Str("example.com")).vvalid, "format:idn-hostname accepts valid")
    check(!fmt("""{"format":"idn-hostname"}""", ujson.Str("hello world")).vvalid, "format:idn-hostname rejects space")

    // $ref to local $defs
    val refSchema = """{"$defs":{"pos":{"type":"integer","minimum":1}},"$ref":"#/$defs/pos"}"""
    check( basic(refSchema, ujson.Num(5)).vvalid, "$ref to $defs accepts 5")
    check(!basic(refSchema, ujson.Num(0)).vvalid, "$ref to $defs rejects 0")

    // output unit shape on failure
    val r = basic("""{"type":"integer"}""", ujson.Str("not-an-int"))
    check(!r.vvalid,         "non-int as integer is invalid")
    check(r.insLoc != null,  "invalid result carries instance location")

    println(s"\n$checks checks, $failures failed")
    if (failures > 0) throw new AssertionError(s"$failures of $checks smoke checks failed")
  }
}
