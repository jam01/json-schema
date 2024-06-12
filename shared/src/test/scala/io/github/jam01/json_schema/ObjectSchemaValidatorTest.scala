package io.github.jam01.json_schema

import io.github.jam01.json_schema.ObjectSchemaValidatorTest.*
import io.github.jam01.json_schema.TestSuiteTest.getClass
import org.junit.jupiter.api.Assertions.{assertFalse, assertTrue}
import org.junit.jupiter.api.{Disabled, Test}
import upickle.core.Visitor

import java.nio.file.Paths
import scala.language.implicitConversions

class ObjectSchemaValidatorTest {
  val schemas: collection.Map[String, ObjectSchema] = ujson.Readable
    .fromPath(Paths.get(getClass.getClassLoader.getResource("simple-tests/main.json").toURI))
    .transform(SchemaR(docbase = Uri("mem://test"))).asInstanceOf[ObjectSchema]
    .value("$defs").obj.asInstanceOf[collection.Map[String, ObjectSchema]]

  @Test
  def valid_str(): Unit = {
    val unit = mkValidator(schemas("strSch")).visitString("valid", -1)
    assertTrue(unit.vvalid)
  }

  @Test
  def invalid_str(): Unit = { // string too long
    val unit = mkValidator(schemas("strSch")).visitString("12345678901234567", -1)
    assertFalse(unit.vvalid)
  }

  @Test
  def valid_arr(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(mkValidator(schemas("arrSch")))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_arr_length(): Unit = { // arr too long
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3", "valid4", "invalid5"]""")
      .transform(mkValidator(schemas("arrSch")))
    assertFalse(r.vvalid)
  }

  @Test
  def invalid_arr_items(): Unit = { // 2nd string too long
    val r = ujson.Readable
      .fromString("""["valid", "12345678901234567", "valid3"]""")
      .transform(mkValidator(schemas("arrSch")))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_nest_arr_items(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", ["valid", "valid2", "valid3"], "valid3"]""")
      .transform(mkValidator(schemas("arrSchNest0")))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_nest_arr_items(): Unit = { // nested arr fails items validation, its 2nd string too long
    val r = ujson.Readable
      .fromString("""["valid", ["valid", "12345678901234567", "valid3"], "valid3"]""")
      .transform(mkValidator(schemas("arrSchNest1")))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_arr_ref(): Unit = {
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(mkValidator(schemas("arrRefSch"), DefaultContext(Map(Uri("mem://test/str") -> schemas("refSch0")))))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_arr_ref(): Unit = { // base schema dictates arr, but $ref dictates string
    val r = ujson.Readable
      .fromString("""["valid", "valid2", "valid3"]""")
      .transform(mkValidator(schemas("arrRefSch"), DefaultContext(Map(Uri("mem://test/str") -> schemas("refSch1")), Config(ffast = false))))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_obj(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(mkValidator(schemas("objSch")))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_obj_required(): Unit = { // foo prop required
    val r = ujson.Readable
      .fromString("""{"nfoo": "bar"}""")
      .transform(mkValidator(schemas("objSch")))
    assertFalse(r.vvalid)
  }

  @Test
  def invalid_obj_props(): Unit = { // foo prop must be string
    val r = ujson.Readable
      .fromString("""{"foo": null}""")
      .transform(mkValidator(schemas("objSch")))
    assertFalse(r.vvalid)
  }

  @Test
  def invalid_obj_length(): Unit = { // obj too long
    val r = ujson.Readable
      .fromString("""{"foo": "null", "arr": [], "null": null}""")
      .transform(mkValidator(schemas("objSch")))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar"}}""")
      .transform(mkValidator(schemas("objSch")))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_nest_obj_props(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "null", "obj": {"nesfoo": "nesbar", "null": null}}""")
      .transform(mkValidator(schemas("objSch")))
    assertFalse(r.vvalid)
  }

  @Test
  def valid_obj_ref(): Unit = {
    val r = ujson.Readable
      .fromString("""{"foo": "bar", "null": null}""")
      .transform(mkValidator(schemas("objRefSch0"), DefaultContext(Map(Uri("mem://test/nullreq") -> schemas("refSch2")))))
    assertTrue(r.vvalid)
  }

  @Test
  def invalid_obj_ref(): Unit = { // base schema dictates obj, but $ref dictates string
    val r = ujson.Readable
      .fromString("""{"foo": "bar"}""")
      .transform(mkValidator(schemas("objRefSch1"), DefaultContext(Map(Uri("mem://test/str") -> schemas("refSch1")), Config(ffast = false))))
    assertFalse(r.vvalid)
  }

  @Test @Disabled
  def uneval(): Unit = {
    val sch =
    """{
      |  "items": { "items": {
      |      "unevaluatedProperties": false,
      |      "properties": { "ffoo": true, "bar": true },
      |      "dependentSchemas": {
      |        "ffoo": { "properties": { "foo": true }},
      |        "bar": { "properties": { "foo": false }}
      |      }
      |    }
      |  }
      |}""".stripMargin

    val r = ujson.Readable
      .fromString("""[[
                    |  { "foo": null, "ffoo": null },
                    |  { "foo": null, "bar": null }
                    |]]""".stripMargin)
      .transform(mkValidator(ujson.Readable.fromString(sch).transform(SchemaR()).asInstanceOf[ObjectSchema]))
    assertFalse(r.vvalid)
  }
}

object ObjectSchemaValidatorTest {
  def mkValidator(osch: ObjectSchema, ctx: Context = DefaultContext(Map.empty[Uri, Schema], Config(ffast = false)),
                  path: JsonPointer = JsonPointer.Root, dynParent: Option[VocabBase] = None): Visitor[?, OutputUnit] = {
    SchemaValidator.of(osch, ctx, path, dynParent)
  }
}
