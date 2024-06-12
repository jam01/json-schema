package io.github.jam01.json_schema

import io.github.jam01.json_schema
import io.github.jam01.json_schema.OutputStructureTest.*
import org.junit.jupiter.api.{Assertions, Test}
import org.skyscreamer.jsonassert.JSONAssert
import ujson.StringRenderer

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions

class OutputStructureTest {
  val simpleSchs: collection.Map[String, ObjectSchema] = ujson.Readable
    .fromString(resourceAsString("output/simple-sch.json"))
    .transform(SchemaR(docbase = Uri("mem://test"))).asInstanceOf[ObjectSchema]
    .value("$defs").obj.asInstanceOf[collection.Map[String, ObjectSchema]]

  val valSch: Schema = ujson.Readable
    .fromString(resourceAsString("output/validation-sch.json"))
    .transform(SchemaR())
  val applSch0: Schema = ujson.Readable
    .fromString(resourceAsString("output/applicator-sch0.json"))
    .transform(SchemaR())
  val applSch1: Schema = ujson.Readable
    .fromString(resourceAsString("output/applicator-sch1.json"))
    .transform(SchemaR())
  val applSch2: Schema = ujson.Readable
    .fromString(resourceAsString("output/applicator-sch2.json"))
    .transform(SchemaR())
  val applSch3: Schema = ujson.Readable
    .fromString(resourceAsString("output/applicator-sch3.json"))
    .transform(SchemaR())

  @Test
  def simple(): Unit = {
    val r = ujson.Readable
      .fromString("""["", "", 1, 2, 3, "", 4, 5]""")
      .transform(json_schema.validator(simpleSchs("simple"),
        Config(format = OutputFormat.Detailed, ffast = false),
        Map(Uri("mem://test/ref") -> simpleSchs("refSch0"))))

    val res =  OutputUnitW.transform(r, StringRenderer()).toString
    //println(res)
    JSONAssert.assertEquals(resourceAsString("output/simple-output.json"), res, true)
  }

  @Test
  def validation(): Unit = {
    // note: missing min/maxContains b/c they're implemented in Applicator
    testOutput(valSch, ujson.Str(""), "output/validation/str0.json") // const, minLength, pattern
    testOutput(valSch, ujson.Str("12345"), "output/validation/str1.json") // maxLength, enum
    testOutput(valSch, ujson.Num(5), "output/validation/num0.json") // type, multipleOf, minimum, exMinimum
    testOutput(valSch, ujson.Num(20), "output/validation/num1.json") // exMaximum
    testOutput(valSch, ujson.Num(200), "output/validation/num2.json") // maximum
    testOutput(valSch, ujson.Arr(1), "output/validation/arr0.json") // minItems
    testOutput(valSch, ujson.Arr(1, 2, 2, 4, 5, 6), "output/validation/arr1.json") // maxItems, unique
    testOutput(valSch, ujson.Obj("bar" -> 0), "output/validation/obj0.json") // minProps, required, depReq
    testOutput(valSch, ujson.Obj("foo" -> 0, "foo1" -> 1, "foo2" -> 2), "output/validation/obj1.json") // maxProps
  }

  @Test
  def applicator(): Unit = {
    // TODO: propertyNames should have verbose details?
    testOutput(applSch0, ujson.Num(1), "output/applicator/num0.json") // then
    testOutput(applSch0, ujson.Arr(1, 2, ujson.Null), "output/applicator/arr0.json") // else, preItems, items, contains
    testOutput(applSch0, ujson.Obj("12345" -> 0, "foo" -> 1, "bar" -> 2), "output/applicator/obj0.json") // propNames, props, patternProps, addlProps, depSchs
    testOutput(applSch1, ujson.Null, "output/applicator/null0.json") // not, allof, oneof, anyof
    testOutput(applSch2, ujson.Null, "output/applicator/null1.json") // not, oneof, anyof
    testOutput(applSch3, ujson.Null, "output/applicator/null2.json") // oneOf
  }

  @Test
  def annotations(): Unit = {
    val res =json_schema
      .from(ujson.Readable, ujson.Readable.fromString("""{"prefixItems":[true, true, true]}"""))
      .validate(ujson.Value, ujson.Arr(0, 1, 2), Config(format = OutputFormat.Detailed, allowList = AllowList.KeepAll))
    Assertions.assertEquals(2L, res.details.head.annotation.value)
  }
}

object OutputStructureTest {
  def testOutput(sch: Schema, sub: ujson.Value, path: String): Unit = {
    val flag = sub.transform(json_schema.validator(sch, Config(format = OutputFormat.Flag, ffast = false)))
    val fres = OutputUnitW.transform(flag, StringRenderer()).toString
    //println(fres)

    val detailed = sub.transform(json_schema.validator(sch, Config(format = OutputFormat.Detailed, ffast = false)))
    val dres = OutputUnitW.transform(detailed, StringRenderer()).toString
    //println(dres)

    val verbose = sub.transform(json_schema.validator(sch, Config(format = OutputFormat.Verbose, ffast = false)))
    val vres = OutputUnitW.transform(verbose, StringRenderer()).toString
    //println(vres)

    val res = Seq(fres, dres, vres).mkString("[", ",", "]")
    JSONAssert.assertEquals(resourceAsString(path), res, true)
  }

  def resourceAsString(s: String): String =
    Files.readString(Paths.get(getClass.getClassLoader.getResource(s).toURI))
}
