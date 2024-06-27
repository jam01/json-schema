/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import org.junit.jupiter.api.Test
import upickle.core.NoOpVisitor

import scala.language.implicitConversions

class SchemaWTest {
  val arr: Arr =
    Arr(0, 1, "bloop",
      3, null, 5.5d,
      Arr("a", "b", "c"),
      Obj(
        "foo7" -> "bar",
        "arr" -> Arr(null, null)
      )
    )
  
  @Test
  def transformer(): Unit = {
    SchemaW.transform(arr, NoOpVisitor)
  }
}
