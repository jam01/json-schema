package io.github.jam01.json_schema

import upickle.core.{Transformer, Visitor}

/**
 * JSON Schema validation output unit
 *
 * @param valid       indicates overall validation success or failure
 * @param kwLoc       relative location of the validating keyword that follows the validation path
 * @param absKwLoc    absolute, dereferenced location of the validating keyword
 * @param insLoc      location of the JSON value evaluated within the instance
 * @param error       produced by a failed validation
 * @param errors      produced by a failed validation
 * @param annotation  produced by a successful validation
 * @param annotations produced by a successful validation
 */
sealed class OutputUnit(val valid: Boolean,
                        val kwLoc: JsonPointer | Null,
                        val absKwLoc: Option[Uri] | Null,
                        val insLoc: JsonPointer | Null,
                        val error: Option[String] = None,
                        val errors: collection.Seq[OutputUnit] = Nil,
                        val annotation: Option[Value] = None, // TODO: should allow schema?
                        val annotations: collection.Seq[OutputUnit] = Nil) { // TODO: consider T | Null

  def hasAnnotations: Boolean = annotation.nonEmpty ||
    (annotations.nonEmpty && annotations.exists(u => u.hasAnnotations))

  def vvalid: Boolean = valid
}

/**
 * Special informational OutputUnit that has no direct effect on the overall validation, e.g.: results of the <i>if</>
 * keyword.
 */
final class InfoUnit(_valid: Boolean, _kwLoc: JsonPointer, _absKwLoc: Option[Uri], _insLoc: JsonPointer,
                          _error: Option[String] = None, _errors: collection.Seq[OutputUnit] = Nil,
                          _annotation: Option[Value] = None, _annotations: collection.Seq[OutputUnit] = Nil) extends
  OutputUnit(_valid, _kwLoc, _absKwLoc, _insLoc, _error, _errors, _annotation, _annotations) {
  override val vvalid: Boolean = true
}

object OutputUnit {
  /**
   * Transform the given OutputUnit into an informational one.
   *
   * @param unit the unit to transform
   * @return the [[InfoUnit]] equivalent
   */
  def info(unit: OutputUnit): OutputUnit = InfoUnit(unit.valid, unit.kwLoc, unit.absKwLoc, unit.insLoc,
    unit.error, unit.errors, unit.annotation, unit.annotations)
}

/**
 * A writer of OutputUnits
 */
object OutputUnitW extends upickle.core.Transformer[OutputUnit] {
  override def transform[T](j: OutputUnit, f: Visitor[?, T]): T = {
    val ov = f.visitObject(-1, true, -1).narrow
    ov.visitKeyValue(ov.visitKey(-1).visitString("valid", -1))
    ov.visitValue(if (j.valid) ov.subVisitor.visitTrue(-1) else ov.subVisitor.visitFalse(-1), -1)

    ov.visitKeyValue(ov.visitKey(-1).visitString("keywordLocation", -1))
    ov.visitValue(ov.subVisitor.visitString(j.kwLoc.toString, -1), -1)

    j.absKwLoc.foreach(kw => {
      ov.visitKeyValue(ov.visitKey(-1).visitString("absoluteKeywordLocation", -1))
      ov.visitValue(ov.subVisitor.visitString(kw.toString, -1), -1)
    })

    ov.visitKeyValue(ov.visitKey(-1).visitString("instanceLocation", -1))
    ov.visitValue(ov.subVisitor.visitString(j.insLoc.toString, -1), -1)

    j.error.foreach(err => {
      ov.visitKeyValue(ov.visitKey(-1).visitString("error", -1))
      ov.visitValue(ov.subVisitor.visitString(err, -1), -1)
    })

    if (j.errors.nonEmpty) {
      ov.visitKeyValue(ov.visitKey(-1).visitString("errors", -1))
      val av = ov.subVisitor.visitArray(j.errors.size, -1).narrow
      for (item <- j.errors) ov.visitValue(transform(item, ov.subVisitor), -1)
      av.visitEnd(-1)
    }

    j.annotation.foreach(ann => {
      ov.visitKeyValue(ov.visitKey(-1).visitString("annotation", -1))
      Transformer.transform(ann, ov.subVisitor)
    })

    if (j.annotations.nonEmpty) {
      ov.visitKeyValue(ov.visitKey(-1).visitString("annotations", -1))
      val av = ov.subVisitor.visitArray(j.errors.size, -1).narrow
      for (item <- j.annotations) ov.visitValue(transform(item, ov.subVisitor), -1)
      av.visitEnd(-1)
    }

    ov.visitEnd(-1)
  }
}

/**
 * JSON Schema validation output format
 */
sealed abstract class OutputFormat {
  def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit
}

object OutputFormat {
  val Flag: OutputFormat = new OutputFormat {
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }
  }

  val Basic: OutputFormat = new OutputFormat{
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }
  }

  val Detailed: OutputFormat = new OutputFormat {
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      val (annots, errs) = units.partition(_.vvalid)
      if (errs.nonEmpty)
        OutputUnit(false, path, None, ctx.instanceLoc, errors = errs)
      else
        OutputUnit(true, path, None, ctx.instanceLoc, annotations = annots)
    }
  }

  val Verbose: OutputFormat = new OutputFormat{
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }
  }
}
