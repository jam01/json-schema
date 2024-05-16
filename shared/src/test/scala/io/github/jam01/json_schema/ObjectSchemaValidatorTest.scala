package io.github.jam01.json_schema

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertThrows, assertTrue}
import upickle.core.LinkedHashMap

import scala.collection.mutable

class ObjectSchemaValidatorTest {
  val lhm: LinkedHashMap[String, Value] = LinkedHashMap(Seq(
    "type" -> Str("string"),
    "pattern" -> Str(".*"),
    "maxLength" -> Num(16L),
    "minLength" -> Num(3L)
  ))
  val strSch: ObjectSchema = ObjectSchema(lhm, Uri.of("mem://test"))

  @Test
  def valid_str(): Unit = {
    assertTrue(ObjectSchemaValidator.validate(strSch).visitString("valid", -1))
  }

  @Test
  def invalid_str(): Unit = { // string too long
    assertFalse(ObjectSchemaValidator.validate(strSch).visitString("12345678901234567", -1))
  }


  val lhm2: LinkedHashMap[String, Value] = LinkedHashMap(Seq(
    "type" -> Str("array"),
    "maxItems" -> Num(4L),
    "minItems" -> Num(2L),
    "items" -> strSch
  ))
  val arrSch: ObjectSchema = ObjectSchema(lhm2, Uri.of("mem://test"))

  @Test
  def valid_arr(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator.validate(arrSch))
    assertTrue(r)
  }

  @Test
  def invalid_arr_length(): Unit = { // arr too long
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3", "valid4", "invalid5"]""")
      .transform(ObjectSchemaValidator.validate(arrSch))
    assertFalse(r)
  }

  @Test
  def invalid_arr_items(): Unit = { // 2nd string too long
    val r = ujson.Readable
      .fromString("""["valid", "12345678901234567", "valid3"]""")
      .transform(ObjectSchemaValidator.validate(arrSch))
    assertFalse(r)
  }

  @Test
  def valid_nest_arr_items(): Unit = {
    val lhm3 = lhm2.clone().addOne("items" -> ObjectSchema(LinkedHashMap(Seq("type" -> Arr(Str("string"), Str("array")))), Uri.of("")))
    val arrSchNest = ObjectSchema(LinkedHashMap(lhm3), Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", ["valid", "valid2", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator.validate(arrSchNest))
    assertTrue(r)
  }

  @Test
  def invalid_nest_arr_items(): Unit = { // nested arr fails items validation, its 2nd string too long
    val lhm3 = lhm2.clone().addOne("items" -> ObjectSchema(LinkedHashMap(Seq(
      "type" -> Arr(Str("string"), Str("array")),
      "items" -> strSch
    )), Uri.of("")))
    val arrSchNest = ObjectSchema(LinkedHashMap(lhm3), Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", ["valid", "12345678901234567", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator.validate(arrSchNest))
    assertFalse(r)
  }

  @Test
  def valid_arr_ref(): Unit = {
    val refSch = ObjectSchema(LinkedHashMap(Seq("type" -> Str("array"), "minItems" -> Num(1L))), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/str") -> refSch))
    val lhm3 = lhm2.clone().addOne("$ref" -> Str("str"))
    val arrSchRef = ObjectSchema(LinkedHashMap(lhm3), Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator.validate(arrSchRef, ctx = ctx))
    assertTrue(r)
  }

  @Test
  def invalid_arr_ref(): Unit = { // base schema dictates arr, but $ref dictates string
    val refSch = ObjectSchema(LinkedHashMap(Seq("type" -> Str("string"))), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/str") -> refSch))
    val lhm3 = lhm2.clone().addOne("$ref" -> Str("str"))
    val arrSchRef = ObjectSchema(LinkedHashMap(lhm3), Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator.validate(arrSchRef, ctx = ctx))
    assertFalse(r)
  }


  val lhm4: LinkedHashMap[String, Value] = LinkedHashMap(Seq(
    "type" -> Str("object"),
    "maxProperties" -> Num(2L),
    "minProperties" -> Num(1L),
    "properties" -> Obj(LinkedHashMap(Seq(
      "foo" -> strSch,
      "arr" -> arrSch,
      "obj" -> ObjectSchema(LinkedHashMap(Seq(
        "type" -> Str("object"),
        "maxProperties" -> Num(1L))), Uri.of("mem://test"))
    ))),
    "required" -> Arr(Str("foo"))
  ))
  val objSch: ObjectSchema = ObjectSchema(LinkedHashMap(lhm4), Uri.of("mem://test"))

  @Test
  def valid_obj(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(ObjectSchemaValidator.validate(objSch))
    assertTrue(r)
  }

  @Test
  def invalid_obj_required(): Unit = { // foo prop required
    val r = ujson.Readable
      .fromString("""{"nfoo": "bar"}""")
      .transform(ObjectSchemaValidator.validate(objSch))
    assertFalse(r)
  }

  @Test
  def invalid_obj_props(): Unit = { // foo prop must be string
    val r = ujson.Readable
      .fromString("""{"foo": null}""")
      .transform(ObjectSchemaValidator.validate(objSch))
    assertFalse(r)
  }


  @Test
  def invalid_obj_length(): Unit = { // obj too long
    val r = ujson.Readable
      .fromString("""{"foo": "null", "arr": [], "null": null}""")
      .transform(ObjectSchemaValidator.validate(objSch))
    assertFalse(r)
  }

  @Test
  def valid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar"}}""")
      .transform(ObjectSchemaValidator.validate(objSch))
    assertTrue(r)
  }

  @Test
  def invalid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar", "null": null}}""")
      .transform(ObjectSchemaValidator.validate(objSch))
    assertFalse(r)
  }

  @Test
  def valid_obj_ref(): Unit = {
    val refSch = ObjectSchema(LinkedHashMap(Seq("required" -> Arr(Str("null")))), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/nullreq") -> refSch))
    val lhm3 = lhm4.clone().addOne("$ref" -> Str("nullreq"))
    val objSchRef = ObjectSchema(LinkedHashMap(lhm3), Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""{"foo": "bar", "null": null}""")
      .transform(ObjectSchemaValidator.validate(objSchRef, ctx = ctx))
    assertTrue(r)
  }

  @Test
  def invalid_obj_ref(): Unit = { // base schema dictates obj, but $ref dictates string
    val refSch = ObjectSchema(LinkedHashMap(Seq("type" -> Str("string"))), Uri.of("mem://test"))
    val ctx = Context(mutable.Stack.empty, Map(Uri.of("mem://test/str") -> refSch))
    val lhm3 = lhm4.clone().addOne("$ref" -> Str("str"))
    val objSchRef = ObjectSchema(LinkedHashMap(lhm3), Uri.of("mem://test"))

    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(ObjectSchemaValidator.validate(objSchRef, ctx = ctx))
    assertFalse(r)
  }
}
