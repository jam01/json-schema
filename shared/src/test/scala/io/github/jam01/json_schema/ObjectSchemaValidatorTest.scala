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
  val strSch: ObjectSchema = ObjectSchema(lhm, Uri.of("mem://test"))

  @Test
  def valid_str(): Unit = {
    assertTrue(ObjectSchemaValidator(strSch).visitString("valid", -1))
  }

  @Test
  def invalid_str(): Unit = { // string too long
    assertFalse(ObjectSchemaValidator(strSch).visitString("12345678901234567", -1))
  }


  val lhm2: LinkedHashMap[String, Any] = LinkedHashMap(
    "type" -> "array",
    "maxItems" -> 4,
    "minItems" -> 2,
    "items" -> strSch
  )
  val arrSch: ObjectSchema = ObjectSchema(lhm2, Uri.of("mem://test"))

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
    val lhm3 = lhm2.clone().addOne("items" -> ObjectSchema(LinkedHashMap("type" -> collection.IndexedSeq("string", "array")), Uri.of("")))
    val arrSchNest = ObjectSchema(lhm3, Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", ["valid", "valid2", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchNest))
    assertTrue(r)
  }

  @Test
  def invalid_nest_arr_items(): Unit = { // nested arr fails items validation, its 2nd string too long
    val lhm3 = lhm2.clone().addOne("items" -> ObjectSchema(LinkedHashMap(
      "type" -> collection.IndexedSeq("string", "array"),
      "items" -> strSch
    ), Uri.of("")))
    val arrSchNest = ObjectSchema(lhm3, Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", ["valid", "12345678901234567", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchNest))
    assertFalse(r)
  }

  @Test
  def valid_arr_ref(): Unit = {
    val refSch = ObjectSchema(LinkedHashMap("type" -> "array", "minItems" -> 1), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/str") -> refSch))
    val lhm3 = lhm2.clone().addOne("$ref" -> "str")
    val arrSchRef = ObjectSchema(lhm3, Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchRef, ctx = ctx))
    assertTrue(r)
  }

  @Test
  def invalid_arr_ref(): Unit = { // base schema dictates arr, but $ref dictates string
    val refSch = ObjectSchema(LinkedHashMap("type" -> "string"), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/str") -> refSch))
    val lhm3 = lhm2.clone().addOne("$ref" -> "str")
    val arrSchRef = ObjectSchema(lhm3, Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator(arrSchRef, ctx = ctx))
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
        "maxProperties" -> 1), Uri.of("mem://test"))
    ),
    "required" -> Seq("foo")
  )
  val objSch: ObjectSchema = ObjectSchema(lhm4, Uri.of("mem://test"))

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

  @Test
  def valid_obj_ref(): Unit = {
    val refSch = ObjectSchema(LinkedHashMap("required" -> Seq("null")), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/nullreq") -> refSch))
    val lhm3 = lhm4.clone().addOne("$ref" -> "nullreq")
    val objSchRef = ObjectSchema(lhm3, Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""{"foo": "bar", "null": null}""")
      .transform(ObjectSchemaValidator(objSchRef, ctx = ctx))
    assertTrue(r)
  }

  @Test
  def invalid_obj_ref(): Unit = { // base schema dictates obj, but $ref dictates string
    val refSch = ObjectSchema(LinkedHashMap("type" -> "string"), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/str") -> refSch))
    val lhm3 = lhm4.clone().addOne("$ref" -> "str")
    val objSchRef = ObjectSchema(lhm3, Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(ObjectSchemaValidator(objSchRef, ctx = ctx))
    assertFalse(r)
  }
}
