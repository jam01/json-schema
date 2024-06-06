package io.github.jam01.json_schema

/**
 * A JSON Pointer
 *
 * Used to identify specific values within a JSON document
 *
 * @see <a href=https://json-schema.org/draft/2020-12/json-schema-core#name-fragment-identifiers>JSON Schema § Fragment Identifiers</a>
 * @see <a href=https://www.rfc-editor.org/rfc/rfc6901.html>JSON Pointer</a>
 *
 * @param refTokens reference tokens for this pointer
 */
final case class JsonPointer(refTokens: Seq[String] = Seq("")) {
  if (refTokens.isEmpty) throw new IllegalArgumentException("Invalid JSON Pointer")

  def appended(refToks: String*): JsonPointer = JsonPointer(refTokens.appendedAll(refToks))

  def appended(other: JsonPointer): JsonPointer = JsonPointer(refTokens.appendedAll(other.refTokens.tail))

  def isRelativeTo(other: JsonPointer): Boolean = {
    if (other.refTokens.size < refTokens.size) false
    else refTokens.lazyZip(other.refTokens).forall((pstr, ostr) => pstr == ostr) // perf: extra LazyZip2 object overhead
  }

  override def toString: String = { // may need to be encoded
    refTokens.iterator
      .map(_.replace("~", "~0")
        .replace("/", "~1"))
      .mkString("/")
  }
}

object JsonPointer {
  val Root: JsonPointer = JsonPointer()
  
  def apply(s: String): JsonPointer = { // assumes string is already decoded (usually through URI's func)
    JsonPointer(s.split("/", -1)
      .toIndexedSeq
      .map(_.replace("~0", "~")
        .replace("~1", "/") // TODO: replace w/regex?
      ))
  }
}
