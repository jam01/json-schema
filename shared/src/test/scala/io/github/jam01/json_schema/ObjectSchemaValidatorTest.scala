package io.github.jam01.json_schema

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertThrows, assertTrue}

import scala.language.implicitConversions

class ObjectSchemaValidatorTest {
  val lhm: LinkedHashMap[String, Any] = LinkedHashMap.empty
  val strSch: ObjectSchema = ObjectSchema(lhm)

  lhm.put("type", "string")
  lhm.put("pattern", ".*")
  lhm.put("maxLength", 16)
  lhm.put("minLength", 4)

  val lhm2: LinkedHashMap[String, Any] = LinkedHashMap.empty
  val arrSch: ObjectSchema = ObjectSchema(lhm2)
  lhm2.put("maxItems", 4)
  lhm2.put("minItems", 2)
  lhm2.put("items", strSch)

  @Test
  def valid_str(): Unit = {
    assertTrue(ObjectSchemaVisitor(strSch).visitString("valid", -1))
  }

  @Test
  def invalid_str(): Unit = {
    assertFalse(ObjectSchemaVisitor(strSch).visitString("12345678901234567", -1))
  }

  @Test
  def valid_arr(): Unit = {
    val r = ujson.Readable.fromString("""["valid", "valid2", "valid3"]""").transform(ObjectSchemaVisitor(arrSch))
    assertTrue(r)
  }

  @Test
  def invalid_arr_length(): Unit = {
    val r = ujson.Readable.fromString("""["valid", "valid2", "valid3", "valid4", "valid5"]""").transform(ObjectSchemaVisitor(arrSch))
    assertFalse(r)
  }

  @Test
  def invalid_arr_subsch(): Unit = {
    val r = ujson.Readable.fromString("""["valid", "12345678901234567", "valid3"]""").transform(ObjectSchemaVisitor(arrSch))
    assertFalse(r)
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

  @Test
  def lhms(): Unit = {
    val x: LinkedHashMap[String, Any] = LinkedHashMap.from(LinkedHashMap.empty)
    println("")

//    x.getString("")

//    val y: IterableOnce[(String, String)] = null
//    y.

//    val j = scala.collection.mutable.LinkedHashMap[String, ujson.Value]()
//    j.value.put("n", ujson.Str(""))
//    ujson.Obj.from(j)
  }
}
