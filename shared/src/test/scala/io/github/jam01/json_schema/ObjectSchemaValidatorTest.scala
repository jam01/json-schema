package io.github.jam01.json_schema

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertThrows, assertTrue}

import scala.collection.mutable

class ObjectSchemaValidatorTest {
  val lhm: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "string",
    "pattern" -> ".*",
    "maxLength" -> 16,
    "minLength" -> 3,
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
  def invalid_str(): Unit = { // string too long
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
  def invalid_arr_length(): Unit = { // arr too long
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3", "valid4", "invalid5"]""")
      .transform(ObjectSchemaValidator(arrSch))
    assertFalse(r)
  }

  @Test
  def invalid_arr_items(): Unit = { // 2nd string too long
    val r = ujson.Readable
      .fromString("""["valid", "12345678901234567", "valid3"]""")
      .transform(ObjectSchemaValidator(arrSch))
    assertFalse(r)
  }

  @Test
  def valid_nest_arr_items(): Unit = {
    val lhm3 = lhm2.clone().addOne("items" -> LinkedHashMap("type" -> collection.IndexedSeq("string", "array")))
    val arrSchNest = ObjectSchema(lhm3, "mem://test")

    val r = ujson.Readable
      .fromString("""["valid", ["valid", "valid2", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchNest))
    assertTrue(r)
  }

  @Test
  def invalid_nest_arr_items(): Unit = { // nested arr fails items validation, its 2nd string too long
    val lhm3 = lhm2.clone().addOne("items" -> LinkedHashMap(
      "type" -> collection.IndexedSeq("string", "array"),
      "items" -> strSch
    ))
    val arrSchNest = ObjectSchema(lhm3, "mem://test")

    val r = ujson.Readable
      .fromString("""["valid", ["valid", "12345678901234567", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchNest))
    assertFalse(r)
  }

  @Test
  def invalid_arr_ref(): Unit = { // base schema dictates arr, but $ref dictates string
    val refSch = ObjectSchema(LinkedHashMap("type" -> "string"), "mem://test")
    val ctx = Context(mutable.Stack.empty, mutable.Stack.empty, Map("str" -> refSch))
    val lhm3 = lhm2.clone().addOne("$ref" -> "str")
    val arrSchRef = ObjectSchema(lhm3, "mem://test")

    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchRef, ctx))
    assertFalse(r)
  }

  val lhm4: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "object",
    "maxProperties" -> 2,
    "minProperties" -> 1,
    "properties" -> LinkedHashMap(
      "foo" -> strSch,
      "arr" -> arrSch,
      "obj" -> ObjectSchema(LinkedHashMap(
        "type" -> "object",
        "maxProperties" -> 1), "mem://test")
    ),
    "required" -> Seq("foo")
  )
  val objSch: ObjectSchema = ObjectSchema(lhm4, "mem://test")

  @Test
  def valid_obj(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(ObjectSchemaValidator(objSch))
    assertTrue(r)
  }

  @Test
  def invalid_obj_required(): Unit = { // foo prop required
    val r = ujson.Readable
      .fromString("""{"nfoo": "bar"}""")
      .transform(ObjectSchemaValidator(objSch))
    assertFalse(r)
  }

  @Test
  def invalid_obj_props(): Unit = { // foo prop must be string
    val r = ujson.Readable
      .fromString("""{"foo": null}""")
      .transform(ObjectSchemaValidator(objSch))
    assertFalse(r)
  }


  @Test
  def invalid_obj_length(): Unit = { // obj too long
    val r = ujson.Readable
      .fromString("""{"foo": "null", "arr": [], "null": null}""")
      .transform(ObjectSchemaValidator(objSch))
    assertFalse(r)
  }

  @Test
  def valid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar"}}""")
      .transform(ObjectSchemaValidator(objSch))
    assertTrue(r)
  }

  @Test
  def invalid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar", "null": null}}""")
      .transform(ObjectSchemaValidator(objSch))
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
