package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertThrows, assertTrue}
import org.junit.jupiter.api.Test
import upickle.core.NoOpVisitor

class TransformerTest {
  val obj: Arr =
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
    Transformer.transform(obj, NoOpVisitor)
  }
}
