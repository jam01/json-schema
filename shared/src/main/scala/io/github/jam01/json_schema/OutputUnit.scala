package io.github.jam01.json_schema

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
                      annotations: collection.Seq[OutputUnit] = Nil) {

  def not(): OutputUnit = {
    if (valid) {
      OutputUnit(false, kwLoc, absKwLoc, insLoc, None, Seq(this), None, Nil)
    } else {
      OutputUnit(true, kwLoc, absKwLoc, insLoc, None, Nil, None, Seq(this))
    }
  }
}
