/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test

import java.io.{ByteArrayInputStream, InputStream}

class StreamingTest {
  // Asserts the validator does not reify the instance: it consumes the InputStream incrementally
  // and, under `ffast`, stops as soon as the result is decided — the byte counter proves that
  // the un-traversed remainder is never pulled.

  private class CountingInputStream(in: InputStream) extends InputStream {
    var count: Long = 0L
    override def read(): Int = {
      val b = in.read()
      if (b >= 0) count += 1
      b
    }
    override def read(buf: Array[Byte], off: Int, len: Int): Int = {
      val n = in.read(buf, off, len)
      if (n > 0) count += n
      n
    }
  }

  // A JSON array whose first element is an integer and the rest are large strings, totalling well
  // over 1 MiB. Schema `{"items":{"type":"string"}}` fails immediately on element 0.
  private def largeArrayBytes: Array[Byte] = {
    val item = "\"" + ("a" * 1024) + "\""
    val sb = new StringBuilder
    sb.append("[42")
    var i = 0
    while (i < 2000) { sb.append(','); sb.append(item); i += 1 }
    sb.append(']')
    sb.result().getBytes("UTF-8")
  }

  private def run(schemaJson: String, in: InputStream, cfg: Config): OutputUnit = {
    val sch = ujson.Readable.fromString(schemaJson).transform(SchemaR())
    try ujson.InputStreamParser.transform(in, validator(sch, cfg))
    catch { case e: ValidationException => e.result }
  }

  @Test def ffast_stops_reading_after_first_failure(): Unit = {
    val bytes = largeArrayBytes
    val ctr = new CountingInputStream(new ByteArrayInputStream(bytes))

    val r = run("""{"items":{"type":"string"}}""", ctr, Config(ffast = true))

    assertFalse(r.vvalid, "validation must fail on the int at index 0")
    // The parser only needs to read `[42` (3 bytes) before the first item is pushed. Allow generous
    // headroom for parser buffering; the point is that consumption is bounded — not proportional to
    // the input size.
    assertTrue(ctr.count < 1024,
      s"expected the validator to short-circuit before draining the stream, but ${ctr.count} bytes were read out of ${bytes.length}")
  }

  @Test def non_ffast_traverses_entire_stream(): Unit = {
    // Same input shape but every element is valid — confirms full streaming traversal works
    // end-to-end. Pairs with the previous test: low count there is meaningful because high count
    // here proves the counter and full traversal are wired correctly.
    val item = "\"" + ("a" * 1024) + "\""
    val sb = new StringBuilder
    sb.append("[\"head\"")
    var i = 0
    while (i < 2000) { sb.append(','); sb.append(item); i += 1 }
    sb.append(']')
    val bytes = sb.result().getBytes("UTF-8")

    val ctr = new CountingInputStream(new ByteArrayInputStream(bytes))
    val r = run("""{"items":{"type":"string"}}""", ctr, Config(ffast = false))

    assertTrue(r.vvalid, "all-string array against items:string must validate")
    assertEquals(bytes.length.toLong, ctr.count,
      s"expected the parser to consume the full stream under ffast=false")
  }
}
