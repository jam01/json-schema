/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertNull, assertTrue}
import org.junit.jupiter.api.Test

class BasicOutputTest {
  private def run(schemaJson: String, instance: ujson.Value, cfg: Config = Config(format = OutputFormat.Basic, ffast = false)): OutputUnit = {
    val sch = ujson.Readable.fromString(schemaJson).transform(SchemaR())
    try instance.transform(validator(sch, cfg))
    catch { case e: ValidationException => e.result }
  }

  // The Basic output's invariant: the root unit's `details` is flat — no entry has its own `details`.
  private def assertFlat(root: OutputUnit): Unit =
    root.details.foreach(u => assertTrue(u.details.isEmpty,
      s"Basic root.details must be flat, but child at ${u.kwLoc} had ${u.details.size} nested entries"))

  @Test def invalid_keyword_appears_in_flat_details(): Unit = {
    val r = run("""{"type":"integer","minimum":10}""", ujson.Num(3))
    assertFalse(r.vvalid)
    assertFlat(r)
    // The /minimum keyword failure must surface as a flat entry with the error string
    val minUnit = r.details.find(u => u.kwLoc.toString == "/minimum")
    assertTrue(minUnit.isDefined, "expected /minimum unit in flat details")
    assertEquals(false, minUnit.get.valid)
    assertTrue(minUnit.get.error != null && minUnit.get.error.toString.toLowerCase.contains("minimum"))
  }

  @Test def applicator_branches_flatten_to_root(): Unit = {
    // anyOf with no match — all three branches are failures and must surface flat at root
    val r = run("""{"anyOf":[{"type":"string"},{"type":"boolean"},false]}""", ujson.Num(5))
    assertFalse(r.vvalid)
    assertFlat(r)
    val kws = r.details.map(_.kwLoc.toString).toSet
    assertTrue(kws.contains("/anyOf"), s"expected /anyOf in flat keys, got: $kws")
    assertTrue(kws.contains("/anyOf/0"), s"expected /anyOf/0 in flat keys, got: $kws")
    assertTrue(kws.contains("/anyOf/1"), s"expected /anyOf/1 in flat keys, got: $kws")
    assertTrue(kws.contains("/anyOf/2"), s"expected /anyOf/2 in flat keys, got: $kws")
  }

  @Test def nested_keyword_failures_lift_to_root(): Unit = {
    // /properties/foo/minimum should appear at the root, not nested inside /properties
    val r = run("""{"properties":{"foo":{"type":"integer","minimum":10}}}""", ujson.Obj("foo" -> 3))
    assertFalse(r.vvalid)
    assertFlat(r)
    val kws = r.details.map(_.kwLoc.toString).toSet
    assertTrue(kws.exists(_.endsWith("/properties/foo/minimum")),
      s"expected a unit whose kwLoc ends in /properties/foo/minimum, got: $kws")
  }

  @Test def valid_root_with_no_annotations_has_empty_details(): Unit = {
    val r = run("""{"type":"integer"}""", ujson.Num(5))
    assertTrue(r.vvalid)
    assertEquals(Nil, r.details)
    assertNull(r.error)
  }

  @Test def valid_root_keeps_only_annotated_units(): Unit = {
    val r = run("""{"prefixItems":[true,true,true]}""",
      ujson.Arr(0, 1, 2),
      Config(format = OutputFormat.Basic, ffast = false, allowList = AllowList.KeepAll))
    assertTrue(r.vvalid)
    assertFlat(r)
    // Every entry in details should carry an annotation; none should be a bare valid marker
    r.details.foreach(u => assertTrue(u.annotation != null,
      s"valid Basic details should only contain annotated units, but ${u.kwLoc} had none"))
    assertTrue(r.details.exists(u => u.kwLoc.toString == "/prefixItems"),
      "expected /prefixItems annotation in flat details")
  }

  @Test def boolean_false_schema_emits_root_error(): Unit = {
    val r = run("""false""", ujson.Num(1))
    assertFalse(r.vvalid)
    assertTrue(r.error != null, "false-schema rejection should surface as root-level error")
  }
}
