package io.github.jam01.json_schema

import scala.collection.mutable

/**
 * A base class for implementing validators using a given Schema. 
 * 
 * Implementations can validate whole vocabularies or single keywords only, as it returns a sequence of OutputUnits  
 *
 * @param schema    schema to apply
 * @param ctx       validation context
 * @param path      the path followed to the given schema
 * @param dynParent dynamic scope parent validator
 */
abstract class BaseValidator(val schema: ObjectSchema,
                             val ctx: Context = Context.Empty,
                             val path: JsonPointer = JsonPointer(),
                             val dynParent: Option[BaseValidator] = None) extends JsonVisitor[_, collection.Seq[OutputUnit]] {
  def valid(kw: String): OutputUnit = {
    OutputUnit(true, Some(path.appended(kw)), None, Some(ctx.currentLoc))
  }

  def invalid(kw: String, err: String): OutputUnit = {
    OutputUnit(false, Some(path.appended(kw)), None, Some(ctx.currentLoc), Some(err))
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
