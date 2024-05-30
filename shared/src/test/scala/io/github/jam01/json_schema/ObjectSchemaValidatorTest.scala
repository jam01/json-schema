package io.github.jam01.json_schema

import io.github.jam01.json_schema.ObjectSchemaValidatorTest.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}

import scala.collection.mutable

class ObjectSchemaValidatorTest {
  @Test
  def valid_str(): Unit = {
    val unit = ObjectSchemaValidator.of(StrSch).visitString("valid", -1)
    assertTrue(unit.vvalid)
  }

  @Test
  def invalid_str(): Unit = { // string too long
    val unit = ObjectSchemaValidator.of(StrSch).visitString("12345678901234567", -1)
    assertFalse(unit.vvalid)
  }

  @Test
  def valid_arr(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator.of(ArrSch))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_arr_length(): Unit = { // arr too long
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3", "valid4", "invalid5"]""")
      .transform(ObjectSchemaValidator.of(ArrSch))
    assertFalse(r.vvalid)
  }

  @Test
  def invalid_arr_items(): Unit = { // 2nd string too long
    val r = ujson.Readable
      .fromString("""["valid", "12345678901234567", "valid3"]""")
      .transform(ObjectSchemaValidator.of(ArrSch))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_nest_arr_items(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", ["valid", "valid2", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator.of(ArrSchNest0))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_nest_arr_items(): Unit = { // nested arr fails items validation, its 2nd string too long
    val r = ujson.Readable
      .fromString("""["valid", ["valid", "12345678901234567", "valid3"], "valid3"]""")
      .transform(ObjectSchemaValidator.of(ArrSchNest1))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_arr_ref(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator.of(ArrRefSch, Context(mutable.Stack(""), Map(Uri.of("mem://test/str") -> RefSch0))))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_arr_ref(): Unit = { // base schema dictates arr, but $ref dictates string
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(ObjectSchemaValidator.of(ArrRefSch, Context(mutable.Stack(""), Map(Uri.of("mem://test/str") -> RefSch1))))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_obj(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(ObjectSchemaValidator.of(ObjSch))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_obj_required(): Unit = { // foo prop required
    val r = ujson.Readable
      .fromString("""{"nfoo": "bar"}""")
      .transform(ObjectSchemaValidator.of(ObjSch))
    assertFalse(r.vvalid)
  }

  @Test
  def invalid_obj_props(): Unit = { // foo prop must be string
    val r = ujson.Readable
      .fromString("""{"foo": null}""")
      .transform(ObjectSchemaValidator.of(ObjSch))
    assertFalse(r.vvalid)
  }

  @Test
  def invalid_obj_length(): Unit = { // obj too long
    val r = ujson.Readable
      .fromString("""{"foo": "null", "arr": [], "null": null}""")
      .transform(ObjectSchemaValidator.of(ObjSch))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar"}}""")
      .transform(ObjectSchemaValidator.of(ObjSch))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar", "null": null}}""")
      .transform(ObjectSchemaValidator.of(ObjSch))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_obj_ref(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "bar", "null": null}""")
      .transform(ObjectSchemaValidator.of(ObjRefSch0, Context(mutable.Stack(""), Map(Uri.of("mem://test/nullreq") -> RefSch2))))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_obj_ref(): Unit = { // base schema dictates obj, but $ref dictates string
    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(ObjectSchemaValidator.of(ObjRefSch1, Context(mutable.Stack(""), Map(Uri.of("mem://test/str") -> RefSch3))))
    assertFalse(r.vvalid)
  }
}

object ObjectSchemaValidatorTest {
  val TestUri: Uri = Uri.of("mem://test")
  val StrSch: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("string"),
    "pattern" -> Str(".*"),
    "maxLength" -> Num(16L),
    "minLength" -> Num(3L)), TestUri)
  val ArrSch: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("array"),
    "maxItems" -> Num(4L),
    "minItems" -> Num(2L),
    "items" -> StrSch), TestUri)
  val ArrSchNest0: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("array"),
    "maxItems" -> Num(4L),
    "minItems" -> Num(2L),
    "items" -> ObjectSchema(LinkedHashMapFactory("type" -> Arr("string", "array")), TestUri)), TestUri)
  val ArrSchNest1: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("array"),
    "maxItems" -> Num(4L),
    "minItems" -> Num(2L),
    "items" -> ObjectSchema(LinkedHashMapFactory(
      "type" -> Arr("string", "array"),
      "items" -> StrSch), TestUri)), TestUri)
  val ArrRefSch: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("array"),
    "maxItems" -> Num(4L),
    "minItems" -> Num(2L),
    "items" -> StrSch,
    "$ref" -> "str"), TestUri)
  val RefSch0: ObjectSchema = ObjectSchema(LinkedHashMapFactory("type" -> Str("array"), "minItems" -> Num(1L)), TestUri)
  val RefSch1: ObjectSchema = ObjectSchema(LinkedHashMapFactory("type" -> Str("string")), TestUri)
  val ObjSch: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("object"),
    "maxProperties" -> Num(2L),
    "minProperties" -> Num(1L),
    "properties" -> Obj(
      "foo" -> StrSch,
      "arr" -> ArrSch,
      "obj" -> ObjectSchema(LinkedHashMapFactory(
        "type" -> Str("object"),
        "maxProperties" -> Num(1L)), TestUri)),
    "required" -> Arr(Str("foo"))), TestUri)
  val ObjRefSch0: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("object"),
    "maxProperties" -> Num(2L),
    "minProperties" -> Num(1L),
    "properties" -> Obj(
      "foo" -> StrSch,
      "arr" -> ArrSch,
      "obj" -> ObjectSchema(LinkedHashMapFactory(
        "type" -> Str("object"),
        "maxProperties" -> Num(1L)), TestUri)),
    "required" -> Arr(Str("foo")),
    "$ref" -> "nullreq"), TestUri)
  val ObjRefSch1: ObjectSchema = ObjectSchema(LinkedHashMapFactory(
    "type" -> Str("object"),
    "maxProperties" -> Num(2L),
    "minProperties" -> Num(1L),
    "properties" -> Obj(
      "foo" -> StrSch,
      "arr" -> ArrSch,
      "obj" -> ObjectSchema(LinkedHashMapFactory(
        "type" -> Str("object"),
        "maxProperties" -> Num(1L)), TestUri)),
    "required" -> Arr(Str("foo")),
    "$ref" -> "str"), TestUri)
  val RefSch2: ObjectSchema = ObjectSchema(LinkedHashMapFactory("required" -> Arr(Str("null"))), TestUri)
  val RefSch3: ObjectSchema = ObjectSchema(LinkedHashMapFactory("type" -> Str("string")), TestUri)
}
