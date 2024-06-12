package io.github.jam01.json_schema

import org.junit.jupiter.api.Assertions.{assertThrows, assertTrue}
import org.junit.jupiter.api.Test

import java.nio.file.Paths
import scala.collection.mutable

class SecurityConsiderationsTest {

  @Test
  def main(): Unit = {
    val reg: mutable.Map[Uri, Schema] = mutable.Map()
    val sch = ujson.Readable
      .fromPath(Paths.get(getClass.getClassLoader.getResource("simple-tests/recursive.json").toURI))
      .transform(SchemaR(registry = reg))

    val ex1 = assertThrows(classOf[IllegalStateException],
      () => Transformer.transform(Null, SchemaValidator.of(sch, DefaultContext(reg), JsonPointer.Root, None)))

    assertTrue(ex1.getMessage.contains("Depth limit exceeded"))
  }
}
