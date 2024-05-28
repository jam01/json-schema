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
                             val dynParent: Option[BaseValidator] = None) extends JsonVisitor[?, collection.Seq[OutputUnit]] {
  // TODO: should these be in Context instead?
  def unitOf(isValid: Boolean, kw: String, err: String): OutputUnit = {
    if (isValid) OutputUnit(true, Some(path.appended(kw)), None, Some(ctx.currentLoc)) 
    else OutputUnit(false, Some(path.appended(kw)), None, Some(ctx.currentLoc), Some(err))
  }
  
  def unitOf(isValid: Boolean, kw: String,
             err: Option[String], errs: collection.Seq[OutputUnit],
             annot: Option[Value], annots: collection.Seq[OutputUnit]): OutputUnit = {
    if (isValid) OutputUnit(true, Some(path.appended(kw)), None, Some(ctx.currentLoc),
      annotation = if (ctx.isVerbose || ctx.mode == Mode.Annotation) annot else None,
      annotations = if (ctx.isVerbose) errs.appendedAll(annots) else if (ctx.mode == Mode.Annotation) annots.filter(a => a.hasAnnotations) else Nil)
    else OutputUnit(false, Some(path.appended(kw)), None, Some(ctx.currentLoc), err, errs)
  }
  
  def addUnit(units: mutable.Buffer[OutputUnit], unit: OutputUnit): mutable.Buffer[OutputUnit] = {
    if (unit.valid) {
      if (ctx.isVerbose || (ctx.mode == Mode.Annotation && unit.hasAnnotations)) units.addOne(unit)
      units
    } else units.addOne(unit)
  }
}
