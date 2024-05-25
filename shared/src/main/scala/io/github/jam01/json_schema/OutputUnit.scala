package io.github.jam01.json_schema

import upickle.core.{Transformer, Visitor}

/**
 * JSON Schema validation output unit
 *
 * @param valid indicates overall validation success or failure
 * @param kwLoc relative location of the validating keyword that follows the validation path
 * @param absKwLoc absolute, dereferenced location of the validating keyword
 * @param insLoc location of the JSON value evaluated within the instance
 * @param error produced by a failed validation
 * @param errors produced by a failed validation
 * @param annotation produced by a successful validation
 * @param annotations produced by a successful validation
 */
final case class OutputUnit(valid: Boolean,
                      kwLoc: Option[JsonPointer] = None,
                      absKwLoc: Option[JsonPointer] = None,
                      insLoc: Option[JsonPointer] = None,
                      error: Option[String] = None,
                      errors: collection.Seq[OutputUnit] = Nil,
                      annotation: Option[Any] = None,
                      annotations: collection.Seq[OutputUnit] = Nil) { // TODO: consider T | Null
}

object OutputUnitTransformer extends upickle.core.Transformer[OutputUnit] {
  override def transform[T](j: OutputUnit, f: Visitor[?, T]): T = {
    val ov = f.visitObject(-1, true, -1).narrow
    ov.visitKeyValue(ov.visitKey(-1).visitString("valid", -1))
    ov.visitValue(if (j.valid) ov.subVisitor.visitTrue(-1) else ov.subVisitor.visitFalse(-1), -1)

    j.kwLoc.foreach(kw => {
      ov.visitKeyValue(ov.visitKey(-1).visitString("keywordLocation", -1))
      ov.visitValue(ov.subVisitor.visitString(kw.toString, -1), -1)      
    })
    
    j.insLoc.foreach(iloc => {
      ov.visitKeyValue(ov.visitKey(-1).visitString("instanceLocation", -1))
      ov.visitValue(ov.subVisitor.visitString(iloc.toString, -1), -1)      
    })
    
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
    
    ov.visitEnd(-1)
  }
}
