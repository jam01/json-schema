package io.github.jam01.json_schema

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertThrows, assertTrue}

class ObjectSchemaValidatorTest {
  @Test
  def valid_str(): Unit = {
    assertTrue(ObjectSchemaVisitor().visitString("valid", -1))
  }

  @Test
  def invalid_str(): Unit = {
    assertFalse(ObjectSchemaVisitor().visitString("12345678901234567", -1))
  }
  
  @Test
  def test(): Unit = {
//    println(java.lang.Long.toUnsignedString(Long.MinValue))
//    println(java.lang.Long.toUnsignedString(Long.MaxValue))
    val lng = java.lang.Long.parseUnsignedLong("9223372036854775807")

    System.out.println(lng) // 10000000000000000000
    System.out.println(java.lang.Long.toUnsignedString(lng)) // 10000000000000000000
    println(lng > Long.MaxValue)
    println(lng < 0)

  }
}
