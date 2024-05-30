package io.github.jam01.json_schema

import io.github.jam01.json_schema.vocab.Core

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
                             val ctx: Context,
                             val path: JsonPointer,
                             val dynParent: Option[BaseValidator]) extends JsonVisitor[?, collection.Seq[OutputUnit]] {
  // TODO: should these be in Context instead?
  def unitOf(isValid: Boolean, kw: String, err: String): OutputUnit = {
    val abs = if (hasRef) Some(schema.location.appendedFragment(s"/$kw")) else None
    if (isValid) OutputUnit(true, path.appended(kw), abs, ctx.currentLoc)
    else OutputUnit(false, path.appended(kw), abs, ctx.currentLoc, Some(err))
  }

  def unitOf(isValid: Boolean, kw: String,
             err: Option[String], errs: collection.Seq[OutputUnit],
             annot: Option[Value], annots: collection.Seq[OutputUnit]): OutputUnit = {
    val abs = if (hasRef) Some(schema.location.appendedFragment(s"/$kw")) else None
    if (isValid) OutputUnit(true, path.appended(kw), abs, ctx.currentLoc,
      annotation = if (ctx.isVerbose || ctx.mode == Mode.Annotation) annot else None,
      annotations = if (ctx.isVerbose) errs.appendedAll(annots) else if (ctx.mode == Mode.Annotation) annots.filter(a => a.hasAnnotations) else Nil)
    else OutputUnit(false, path.appended(kw), abs, ctx.currentLoc, err, errs)
  }
  
  def addUnit(units: mutable.Buffer[OutputUnit], unit: OutputUnit): mutable.Buffer[OutputUnit] = {
    if (unit.vvalid) {
      if (ctx.isVerbose || (ctx.mode == Mode.Annotation && unit.hasAnnotations)) units.addOne(unit)
      units
    } else units.addOne(unit)
  }

  private def hasRef: Boolean = {
    path.refTokens.exists(s => Core._Ref == s || Core._DynRef == s)
  }
}
