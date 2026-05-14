/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertEquals, assertThrows, assertTrue}
import org.junit.jupiter.api.Test

class SchemaRetrievalExceptionTest {
  // Regression: SchemaRetrievalException used to be a path-dependent inner class of `Schema`,
  // so `TrueSchema.SchemaRetrievalException` and `FalseSchema.SchemaRetrievalException` were
  // distinct nominal types — pattern matching across schema instances would have missed.
  @Test def caught_uniformly_across_schema_instances(): Unit = {
    val osch: Schema = ujson.Readable.fromString("""{"type":"string"}""").transform(SchemaR())
    val bsch: Schema = TrueSchema

    val e1 = assertThrows(classOf[SchemaRetrievalException], () =>
      osch.schBy(JsonPointer("/does/not/exist")))
    val e2 = assertThrows(classOf[SchemaRetrievalException], () =>
      bsch.schBy(JsonPointer("/anything")))

    // Both are the same top-level class; a single catch site handles both.
    assertEquals(e1.getClass, e2.getClass)
    assertTrue(e1.getClass.getName.endsWith(".SchemaRetrievalException"),
      s"expected top-level SchemaRetrievalException, got ${e1.getClass.getName}")
  }
}
