package io.github.jam01.json_schema

import scala.collection.mutable.ListBuffer

/**
 * A JSON Pointer
 *
 * Used to identify specific values within a JSON document
 *
 * @see <a href=https://json-schema.org/draft/2020-12/json-schema-core#name-fragment-identifiers>JSON Schema § Fragment Identifiers</a>
 * @see <a href=https://www.rfc-editor.org/rfc/rfc6901.html>JSON Pointer</a>
 * @param refTokens reference tokens for this pointer
 */
final case class JsonPointer(refTokens: Seq[String] = Seq("")) {
  if (refTokens.isEmpty) throw new IllegalArgumentException("Invalid JSON Pointer")

  def appended(refToks: String*): JsonPointer = JsonPointer(refTokens.appendedAll(refToks))

  def isRelativeTo(other: JsonPointer): Boolean = {
    if (other.refTokens.size < refTokens.size) false
    else refTokens.lazyZip(other.refTokens).forall((pstr, ostr) => pstr == ostr) // perf: extra LazyZip2 object overhead
  }

  override def toString: String = { // may need to be encoded
    refTokens.iterator
      .map(_.replace("~", "~0").replace("/", "~1")) // perf: replace w/regex?
      .mkString("/")
  }
}

object JsonPointer {
  val Root: JsonPointer = JsonPointer()
  def apply(s: String): JsonPointer = {
    if (s.isEmpty) return Root
    if (s.charAt(0) != '/') throw new IllegalArgumentException(s"Invalid JSON Poitner '$s'")

    val refs = ListBuffer("")
    var i = 1; var valid = true
    var currRef = 1
    while (i < s.length() - 1 && valid) {
      val c = s.charAt(i)
      if (c == '/') { refs.addOne(escape(s.substring(currRef, i))); currRef = i + 1 }
      else if (c == '~' && (s.charAt(i + 1) != '0' && s.charAt(i + 1) != '1')) valid = false
      i += 1
    }

    if (i == s.length() - 1 && valid && s.charAt(i) != '~') {
      if (s.charAt(i) == '/') { refs.addOne(escape(s.substring(currRef, i))); refs.addOne("") }
      else refs.addOne(escape(s.substring(currRef)))
      JsonPointer(refs.toList)
    }
    else throw new IllegalArgumentException(s"Invalid JSON Pointer '$s'")
  }

  private def escape(ref: String): String = {
    ref.replace("~0", "~").replace("~1", "/") // perf: replace w/regex?
  }
}
