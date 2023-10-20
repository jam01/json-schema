package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertThrows, assertTrue}
import org.junit.jupiter.api.Test
import upickle.core.NoOpVisitor

class TransformerTest {
  val obj: Seq[Any] =
    Seq(0, 1, "bloop",
      3, null, 5.5d,
      Seq("a", "b", "c"),
      Map(
        "foo7" -> "bar",
        "arr" -> Seq(null, null)
      )
    )
  
  @Test
  def transformer(): Unit = {
    Transformer.transform(obj, NoOpVisitor)
  }

  @Test
  def transformer_invalid_map_key(): Unit = {
    val ex = assertThrows(classOf[ClassCastException], () => Transformer.transform(Map("valid" -> 1, 2 -> "invalid"), NoOpVisitor))
    assertTrue(ex.getMessage.contains("class java.lang.Integer cannot be cast to class java.lang.CharSequence"))
  }

  @Test
  def transformer_invalid_type(): Unit = {
    val ex = assertThrows(classOf[IllegalArgumentException], () => Transformer.transform(Map("valid" -> 1, "invalid" -> new Object()), NoOpVisitor))
    assertTrue(ex.getMessage.contains("unsupported type java.lang.Object"))
  }
}
