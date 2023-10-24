package io.github.jam01.json_schema

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertThrows, assertTrue}

import scala.collection.mutable

class ObjectSchemaValidatorTest {
  val lhm: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "string",
    "pattern" -> ".*",
    "maxLength" -> 16,
    "minLength" -> 4,
  )
  val strSch: ObjectSchema = ObjectSchema(lhm, "mem://test")

  val lhm2: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "array",
    "maxItems" -> 4,
    "minItems" -> 2,
    "items" -> strSch
  )
  val arrSch: ObjectSchema = ObjectSchema(lhm2, "mem://test")

  @Test
  def valid_str(): Unit = {
    assertTrue(ObjectSchemaValidator(strSch).visitString("valid", -1))
  }

  @Test
  def invalid_str(): Unit = {
    assertFalse(ObjectSchemaValidator(strSch).visitString("12345678901234567", -1))
  }

  @Test
  def valid_arr(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator(arrSch))
    assertTrue(r)
  }

  @Test
  def invalid_arr_length(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3", "valid4", "valid5"]""")
      .transform(ObjectSchemaValidator(arrSch))
    assertFalse(r)
  }

  @Test
  def invalid_arr_subsch(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "12345678901234567", "valid3"]""")
      .transform(ObjectSchemaValidator(arrSch))
    assertFalse(r)
  }

  @Test
  def invalid_arr_ref(): Unit = {
    val refSch = ObjectSchema(LinkedHashMap("type" -> "string"), "mem://test")
    val ctx = Context(mutable.Stack.empty, mutable.Stack.empty, Map("str" -> refSch))
    val lhm3 = LinkedHashMap(lhm2).addOne("$ref" -> "str")
    val arrSchRef = ObjectSchema(lhm3, "mem://test")

    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchRef, ctx))
    assertFalse(r)
  }

//  @Test
//  def test(): Unit = {
////    println(java.lang.Long.toUnsignedString(Long.MinValue))
////    println(java.lang.Long.toUnsignedString(Long.MaxValue))
//    val lng = java.lang.Long.parseUnsignedLong("9223372036854775807")
//
//    System.out.println(lng) // 10000000000000000000
//    System.out.println(java.lang.Long.toUnsignedString(lng)) // 10000000000000000000
//    println(lng > Long.MaxValue)
//    println(lng < 0)
//
//
//
//  }
//
////  @Test
//  def lhms(): Unit = {
//    val prev: LinkedHashMap[Int, Any] = LinkedHashMap().addOne((23, "bloop"))
//    val x: LinkedHashMap[_, Any] = LinkedHashMap.from(prev)
//    println("")
//
////    x.getString("")
//
////    val y: IterableOnce[(String, String)] = null
////    y.
//
//    val j = scala.collection.mutable.LinkedHashMap[String, ujson.Value]()
//    j.value.put("n", ujson.Str(""))
//    ujson.Obj.from(j)
////    j.transform(null.asInstanceOf[Visitor[_,_]])
//
//    val y: mutable.LinkedHashMap[Int, Int] = mutable.LinkedHashMap.empty
//    y.addOne(21, 12)
//    y.asInstanceOf[mutable.LinkedHashMap[String, ujson.Value]].value.arr
//  }
}
