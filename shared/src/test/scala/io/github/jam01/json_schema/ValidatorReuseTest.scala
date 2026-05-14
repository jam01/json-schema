/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test

class ValidatorReuseTest {
  private def mkValidator(schemaJson: String, cfg: Config = Config.Default) = {
    val sch = ujson.Readable.fromString(schemaJson).transform(SchemaR())
    validator(sch, cfg)
  }

  private def run(v: upickle.core.Visitor[?, OutputUnit], instance: ujson.Value): OutputUnit =
    try instance.transform(v)
    catch { case e: ValidationException => e.result }

  // After a ffast failure on `[5, "bad"]`, the next transform must not see the leftover "1"
  // instance-location token from the un-traversed remainder of the failed traversal.
  @Test def ffast_failure_then_success_reports_correct_insloc(): Unit = {
    val v = mkValidator("""{"type":"integer"}""", Config(format = OutputFormat.Basic, ffast = true))

    val r1 = run(v, ujson.Str("nope"))
    assertFalse(r1.vvalid, "first transform should fail")
    assertEquals("", r1.insLoc.toString, "root failure should have root insLoc")

    val r2 = run(v, ujson.Num(42))
    assertTrue(r2.vvalid, "second transform should succeed on the reused validator")
    assertEquals("", r2.insLoc.toString, "successful root result should have root insLoc")
  }

  @Test def ffast_failure_then_failure_reports_correct_insloc(): Unit = {
    val v = mkValidator("""{"items":{"type":"integer"}}""", Config(format = OutputFormat.Basic, ffast = true))

    val r1 = run(v, ujson.Arr(1, "bad", 3))
    assertFalse(r1.vvalid)
    assertEquals("", r1.insLoc.toString, "root unit of an array failure carries the root insLoc")

    // Reused — failure on a fresh instance must not inherit the prior "/1" token.
    val r2 = run(v, ujson.Arr("alpha"))
    assertFalse(r2.vvalid)
    assertEquals("", r2.insLoc.toString, "root unit of the second array failure must still be at root, not /1/0 or similar")
  }

  @Test def repeated_success_does_not_accumulate_annotations(): Unit = {
    // unevaluatedProperties annotation collection would leak across transforms if the dependency
    // maps weren't cleared between root scopes.
    val v = mkValidator(
      """{"properties":{"a":{"type":"integer"}},"unevaluatedProperties":false}""",
      Config(format = OutputFormat.Basic, ffast = false, allowList = AllowList.KeepAll))

    val r1 = run(v, ujson.Obj("a" -> 1))
    assertTrue(r1.vvalid, "first transform should validate")

    // If `dependents`/`dependencies` weren't reset, the second transform's `unevaluatedProperties`
    // could observe stale annotation entries from the first.
    val r2 = run(v, ujson.Obj("a" -> 2))
    assertTrue(r2.vvalid, "second identical transform should also validate")

    val r3 = run(v, ujson.Obj("a" -> 3, "extra" -> 4))
    assertFalse(r3.vvalid, "third transform with an unevaluated property must fail")
  }

  @Test def non_ffast_reuse_works(): Unit = {
    val v = mkValidator("""{"type":"integer"}""", Config(format = OutputFormat.Basic, ffast = false))

    val r1 = run(v, ujson.Str("nope"))
    assertFalse(r1.vvalid)

    val r2 = run(v, ujson.Num(7))
    assertTrue(r2.vvalid)
  }
}
