/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertThrows, assertTrue}
import org.junit.jupiter.api.Test

import java.nio.file.Paths
import scala.collection.mutable

class SecurityConsiderationsTest {

  @Test
  def main(): Unit = {
    val reg = new MutableRegistry
    val sch = ujson.Readable
      .fromPath(Paths.get(getClass.getClassLoader.getResource("simple-tests/recursive.json").toURI))
      .transform(SchemaR(registry = reg))

    val ex1 = assertThrows(classOf[IllegalStateException],
      () => SchemaW.transform(Null, SchemaValidator(sch, DefaultContext(reg), JsonPointer.Root, None)))

    assertTrue(ex1.getMessage.contains("Depth limit exceeded"))
  }
}
