package io.github.jam01.json_schema

import scala.collection.mutable

/**
 * A Validator that applies a Vocabulary using a given Schema
 *
 * @param schema    schema to apply
 * @param ctx       validation context
 * @param path      the path followed to the given schema
 * @param dynParent dynamic scope parent validator
 */
abstract class VocabValidator(val schema: ObjectSchema,
                              val ctx: Context = Context.Empty,
                              val path: JsonPointer = JsonPointer(),
                              val dynParent: Option[VocabValidator] = None) extends JsonVisitor[_, collection.Seq[OutputUnit]] {
  def valid(kw: String): OutputUnit = {
    OutputUnit(true, Some(path.appended(kw)), None, Some(ctx.insPtr))
  }

  def invalid(kw: String, err: String): OutputUnit = {
    OutputUnit(false, Some(path.appended(kw)), None, Some(ctx.insPtr), Some(err))
  }
  
  def validate(kw: String, err: String, isValid: Boolean): OutputUnit = {
    if (isValid) { if (false /* verbose */ ) (); valid(kw) }
    else invalid(kw, err)
  }
  
  def validate(kw: String, err: String, units: mutable.ArrayBuffer[OutputUnit], isValid: Boolean): mutable.ArrayBuffer[OutputUnit] = {
    if (isValid) { if (false /* verbose */) units.addOne(valid(kw)); units }
    else units.addOne(invalid(kw, err))
  }
  
  def addUnit(units: mutable.ArrayBuffer[OutputUnit], unit: OutputUnit): mutable.ArrayBuffer[OutputUnit] = {
    if (unit.valid) { if (false /* verbose */ ) units.addOne(unit); units }
    else units.addOne(unit)
  }
}
