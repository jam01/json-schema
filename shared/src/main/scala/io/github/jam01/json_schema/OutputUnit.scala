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
                        val kwLoc: JsonPointer,
                        val absKwLoc: Uri | Null = null,
                        val insLoc: JsonPointer,
                        val error: String | Null = null,
                        val annotation: Value | Null = null,
                        val details: collection.Seq[OutputUnit] = Nil) {

  def hasAnnotations: Boolean = annotation != null ||
    (details.nonEmpty && details.exists(u => u.hasAnnotations))

  def vvalid: Boolean = valid
}

/**
 * Special informational OutputUnit that has no direct effect on the overall validation, e.g.: results of the <i>if</>
 * keyword.
 */
final class InfoUnit(seed: OutputUnit) extends
  OutputUnit(seed.valid, seed.kwLoc, seed.absKwLoc, seed.insLoc, seed.error, seed.annotation, seed.details) {
  override val vvalid: Boolean = true
}

object OutputUnit {
  /**
   * Transform the given OutputUnit into an informational one.
   *
   * @param unit the unit to transform
   * @return the [[InfoUnit]] equivalent
   */
  def info(unit: OutputUnit): OutputUnit = InfoUnit(unit)
}

/**
 * A writer of OutputUnits
 */
object OutputUnitW extends upickle.core.Transformer[OutputUnit] {
  override def transform[T](unit: OutputUnit, f: Visitor[?, T]): T = {
    val ov = f.visitObject(-1, true, -1).narrow
    ov.visitKeyValue(ov.visitKey(-1).visitString("valid", -1))
    ov.visitValue(if (unit.valid) ov.subVisitor.visitTrue(-1) else ov.subVisitor.visitFalse(-1), -1)

    ov.visitKeyValue(ov.visitKey(-1).visitString("keywordLocation", -1))
    ov.visitValue(ov.subVisitor.visitString(unit.kwLoc.toString, -1), -1)

    if (unit.absKwLoc != null) {
      ov.visitKeyValue(ov.visitKey(-1).visitString("absoluteKeywordLocation", -1))
      ov.visitValue(ov.subVisitor.visitString(unit.absKwLoc.toString, -1), -1)
    }

    ov.visitKeyValue(ov.visitKey(-1).visitString("instanceLocation", -1))
    ov.visitValue(ov.subVisitor.visitString(unit.insLoc.toString, -1), -1)

    if (unit.error != null) {
      ov.visitKeyValue(ov.visitKey(-1).visitString("error", -1))
      ov.visitValue(ov.subVisitor.visitString(unit.error, -1), -1)
    }

    if (unit.annotation != null) {
      ov.visitKeyValue(ov.visitKey(-1).visitString("annotation", -1))
      Transformer.transform(unit.annotation, ov.subVisitor)
    }

    if (unit.details.nonEmpty) {
      ov.visitKeyValue(ov.visitKey(-1).visitString("details", -1))
      val av = ov.subVisitor.visitArray(unit.details.size, -1).narrow
      for (item <- unit.details) ov.visitValue(transform(item, ov.subVisitor), -1)
      av.visitEnd(-1)
    }

    ov.visitEnd(-1)
  }
}

/**
 * JSON Schema validation output format
 */
sealed abstract class OutputFormat {
  def compose(path: JsonPointer, units: collection.Seq[OutputUnit], insLoc: JsonPointer): OutputUnit =
    val (valid, invalid) = units.partition(_.vvalid)
    make(invalid.isEmpty, path, null, insLoc, null, invalid, null, valid)

  def make(isValid: Boolean,
           kwLoc: JsonPointer,
           absKwLoc: Uri | Null = null,
           insLoc: JsonPointer,
           error: String | Null = null,
           errors: collection.Seq[OutputUnit] = Nil,
           annotation: Value | Null = null,
           verbose: collection.Seq[OutputUnit] = Nil): OutputUnit
}

object OutputFormat {
  val Flag: OutputFormat = new OutputFormat {
    override def compose(path: JsonPointer, units: collection.Seq[OutputUnit], insLoc: JsonPointer): OutputUnit =
      OutputUnit(units.forall(_.vvalid), path, null, insLoc)

    override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: collection.Seq[OutputUnit], annotation: Value | Null, verbose: collection.Seq[OutputUnit]): OutputUnit =
      OutputUnit(isValid, kwLoc, null, insLoc)
  }

  val Basic: OutputFormat = new OutputFormat{
    override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: collection.Seq[OutputUnit], annotation: Value | Null, verbose: collection.Seq[OutputUnit]): OutputUnit =
      ???
  }

  val Detailed: OutputFormat = new OutputFormat {
    override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: collection.Seq[OutputUnit], annotation: Value | Null, verbose: collection.Seq[OutputUnit]): OutputUnit =
      if (isValid) OutputUnit(true, kwLoc, absKwLoc, insLoc, null, annotation, verbose.filter(_.hasAnnotations))
      else OutputUnit(false, kwLoc, absKwLoc, insLoc, error, annotation, errors)

  }

  val Verbose: OutputFormat = new OutputFormat {
    override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: collection.Seq[OutputUnit], annotation: Value | Null, verbose: collection.Seq[OutputUnit]): OutputUnit =
      if (isValid) OutputUnit(true, kwLoc, absKwLoc, insLoc, null, annotation, verbose.filter(_.hasAnnotations))
      else OutputUnit(false, kwLoc, absKwLoc, insLoc, error, annotation, errors ++: verbose.filter(_.hasAnnotations))
  }
}
