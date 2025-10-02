/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import upickle.core.{Transformer, Visitor}

import scala.collection.mutable

/**
 * JSON Schema validation output unit.
 *
 * @param valid       indicates overall validation success or failure
 * @param kwLoc       relative location of the validating keyword that follows the validation path
 * @param absKwLoc    absolute, dereferenced location of the validating keyword
 * @param insLoc      location of the JSON value evaluated within the instance
 * @param error       produced by a failed validation
 * @param annotation  produced by a successful validation
 * @param details     subschema(s) validation results
 */
sealed class OutputUnit(val valid: Boolean,
                        val kwLoc: JsonPointer,
                        val absKwLoc: Uri | Null = null,
                        val insLoc: JsonPointer,
                        val error: String | Null = null,
                        val annotation: Value | Null = null,
                        val details: Seq[OutputUnit] = Nil) {

  /**
   * Whether the OutputUnit has annotations.
   */
  val hasAnnotations: Boolean = annotation != null ||
    (details.nonEmpty && details.exists(u => u.hasAnnotations))

  /**
   * Whether the OutputUnit is valid for validation purposes.
   * @see [[InfoUnit]]
   */
  def vvalid: Boolean = valid

  /**
   * Finds all OutputUnits within the given hierarchy that:
   * 1. Match the specified instance location (insLoc).
   * 2. Contain annotations.
   * 3. Match the specified keyword filter.
   *
   * @param instanceLocation The JsonPointer location for which units are being searched.
   * @param keyword The relative keyword location to filter by.
   * @return A sequence of OutputUnits that match the instance location and keyword filter.
   */
  def findAnnotatingUnits(instanceLocation: JsonPointer, keyword: String): Seq[OutputUnit] = {
    val matchingUnits = scala.collection.mutable.ListBuffer[OutputUnit]()

    // Check if the current unit matches the instance location, has an annotation, and matches the keyword filter
    if (this.insLoc == instanceLocation && this.annotation != null && this.kwLoc.refTokens.last == keyword)
      matchingUnits += this

    for (childUnit <- this.details)  // Recursively search through the details of the unit
      matchingUnits ++= childUnit.findAnnotatingUnits(instanceLocation, keyword)

    matchingUnits.toSeq
  }
}

/**
 * Special informational OutputUnit that has no direct effect on the overall validation, e.g.: results of the `if`
 * keyword.
 */
final class InfoUnit(seed: OutputUnit) extends
  OutputUnit(seed.valid, seed.kwLoc, seed.absKwLoc, seed.insLoc, seed.error, seed.annotation, seed.details) {
  override val vvalid: Boolean = true
}

object OutputUnit {
  /**
   * Informational equivalent to the given OutputUnit.
   *
   * @param unit the unit to transform
   * @return the [[InfoUnit]] equivalent
   */
  def info(unit: OutputUnit): OutputUnit = InfoUnit(unit)
}

/**
 * OutputUnit writer.
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
      SchemaW.transform(unit.annotation, ov.subVisitor)
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
 * JSON Schema validation output format.
 */
abstract class OutputFormat {
  /**
   * Compose a schema OutputUnit according to this output format.
   *
   * @param path the schema path
   * @param units the keyword output units
   * @param insLoc the instance location for which the results apply
   * @return the resulting OutputUnit
   */
  def compose(path: JsonPointer, units: Seq[OutputUnit], insLoc: JsonPointer): OutputUnit =
    val (valid, invalid) = units.partition(_.vvalid)
    make(invalid.isEmpty, path, null, insLoc, null, invalid, null, valid)

  /**
   * Create a keyword OutputUnit according to this output format.
   *
   * @param isValid whether the unit is valid
   * @param kwLoc the keyword location
   * @param absKwLoc the keyword absolute location
   * @param insLoc the instance location for this unit
   * @param error the keyword error message
   * @param errors the keyword error sub output units
   * @param annotation the keyword annotation
   * @param verbose the keyword verbose sub output units
   * @return the resulting OutputUnit
   */
  def make(isValid: Boolean,
           kwLoc: JsonPointer,
           absKwLoc: Uri | Null = null,
           insLoc: JsonPointer,
           error: String | Null = null,
           errors: Seq[OutputUnit] = Nil,
           annotation: Value | Null = null,
           verbose: Seq[OutputUnit] = Nil): OutputUnit

  /**
   * Accumulate the given unit in the results, according to this output format.
   *
   * @param results accumulated units
   * @param unit the unit to accumulate
   * @return the resulting collection
   */
  def accumulate(results: mutable.Growable[OutputUnit], unit: OutputUnit): mutable.Growable[OutputUnit] = {
    if (!unit.vvalid) results.addOne(unit)
    else results
  }

  /**
   * Accumulate the unit-to-be in the results, according to this output format.
   *
   * This is equivalent to `fmt.accumulate(results, fmt.make(...))` but where the unit may not be constructed if it
   * would not be added to the results, for example if it is valid and this format would discard it.
   *
   * @param results accumulated units
   * @param isValid whether the unit is valid
   * @param kwLoc the keyword location
   * @param absKwLoc the keyword absolute location
   * @param insLoc the instance location for this unit
   * @param error the keyword error message
   * @param errors the keyword error sub output units
   * @param annotation the keyword annotation
   * @param verbose the keyword verbose sub output units
   * @return the resulting collection
   */
  def accumulate(results: mutable.Growable[OutputUnit],
                 isValid: Boolean,
                 kwLoc: JsonPointer,
                 absKwLoc: Uri | Null = null,
                 insLoc: JsonPointer,
                 error: String | Null = null,
                 errors: Seq[OutputUnit] = Nil,
                 annotation: Value | Null = null,
                 verbose: Seq[OutputUnit] = Nil): mutable.Growable[OutputUnit] = {
    if (!isValid || annotation != null) results.addOne(make(isValid, kwLoc, absKwLoc, insLoc, error, errors, annotation, verbose))
    else results
  }
}

object OutputFormat {
  /**
   * An output format that results in a single OutputUnit for the entire instance being validated.
   */
  val Flag: OutputFormat = new OutputFormat {
    override def compose(path: JsonPointer, units: Seq[OutputUnit], insLoc: JsonPointer): OutputUnit =
      OutputUnit(units.forall(_.vvalid), path, null, insLoc)

    inline override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: Seq[OutputUnit], annotation: Value | Null, verbose: Seq[OutputUnit]): OutputUnit =
      OutputUnit(isValid, kwLoc, null, insLoc)
  }

  val Basic: OutputFormat = new OutputFormat {
    override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: Seq[OutputUnit], annotation: Value | Null, verbose: Seq[OutputUnit]): OutputUnit =
      ???
  }

  /**
   * A hierarchical output format resembling the schema's structure, which retains only errors results and filtered annotations.
   */
  val Detailed: OutputFormat = new OutputFormat {
    inline override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: Seq[OutputUnit], annotation: Value | Null, verbose: Seq[OutputUnit]): OutputUnit =
      if (isValid) OutputUnit(true, kwLoc, absKwLoc, insLoc, null, annotation, verbose.filter(_.hasAnnotations))
      else OutputUnit(false, kwLoc, absKwLoc, insLoc, error, null, errors)

    inline override def accumulate(results: mutable.Growable[OutputUnit], unit: OutputUnit): mutable.Growable[OutputUnit] =
      if (!unit.vvalid) results.addOne(unit)
      else if (unit.hasAnnotations) results.addOne(unit)
      else results
  }

  /**
   * A hierarchical output format resembling the schema's structure, which retains all results and filtered annotations.
   */
  val Verbose: OutputFormat = new OutputFormat {
    inline override def make(isValid: Boolean, kwLoc: JsonPointer, absKwLoc: Uri | Null, insLoc: JsonPointer, error: String | Null, errors: Seq[OutputUnit], annotation: Value | Null, verbose: Seq[OutputUnit]): OutputUnit =
      if (isValid) OutputUnit(true, kwLoc, absKwLoc, insLoc, null, annotation, errors ++: verbose)
      else OutputUnit(false, kwLoc, absKwLoc, insLoc, error, null, errors ++: verbose)

    inline override def accumulate(results: mutable.Growable[OutputUnit], unit: OutputUnit): mutable.Growable[OutputUnit] = {
      results.addOne(unit)
    }

    inline override def accumulate(results: mutable.Growable[OutputUnit],
                            isValid: Boolean,
                            kwLoc: JsonPointer,
                            absKwLoc: Uri | Null = null,
                            insLoc: JsonPointer,
                            error: String | Null = null,
                            errors: Seq[OutputUnit] = Nil,
                            annotation: Value | Null = null,
                            verbose: Seq[OutputUnit] = Nil): mutable.Growable[OutputUnit] = {
      results.addOne(make(isValid, kwLoc, absKwLoc, insLoc, error, errors, annotation, verbose))
    }
  }
}
