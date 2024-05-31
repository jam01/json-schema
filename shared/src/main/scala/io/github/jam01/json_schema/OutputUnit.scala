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
                        val absKwLoc: Option[Uri],
                        val insLoc: JsonPointer,
                        val error: Option[String] = None,
                        val errors: collection.Seq[OutputUnit] = Nil,
                        val annotation: Option[Value] = None, // TODO: should allow schema?
                        val annotations: collection.Seq[OutputUnit] = Nil) { // TODO: consider T | Null

  def hasAnnotations: Boolean = annotation.nonEmpty ||
    (annotations.nonEmpty && annotations.forall(u => u.hasAnnotations))

  def vvalid: Boolean = valid
}

final class InfoUnit(_valid: Boolean,
                          _kwLoc: JsonPointer,
                          _absKwLoc: Option[Uri],
                          _insLoc: JsonPointer,
                          _error: Option[String] = None,
                          _errors: collection.Seq[OutputUnit] = Nil,
                          _annotation: Option[Value] = None,
                          _annotations: collection.Seq[OutputUnit] = Nil) extends OutputUnit(_valid, _kwLoc, _absKwLoc, _insLoc, _error, _errors, _annotation, _annotations) {
  override val vvalid: Boolean = true
}

object OutputUnit {
  def info(u: OutputUnit): OutputUnit = InfoUnit(u.valid, u.kwLoc, u.absKwLoc, u.insLoc,
    u.error, u.errors, u.annotation, u.annotations)
}

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
