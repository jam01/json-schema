package io.github.jam01.json_schema

import scala.collection.mutable.ListBuffer

/**
 * A JSON Pointer, used to identify values within a JSON document.
 *
 * @see <a href=https://json-schema.org/draft/2020-12/json-schema-core#name-fragment-identifiers>JSON Schema § Fragment Identifiers</a>
 * @see <a href=https://www.rfc-editor.org/rfc/rfc6901.html>JSON Pointer</a>
 * @param refTokens reference tokens for this pointer
 */
final case class JsonPointer private(refTokens: Seq[String]) {
  /**
   * A copy of this JsonPointer with the given tokens appended
   * @param refToks tokens to append
   * @return the new JsonPointer
   */
  def appended(refToks: String*): JsonPointer = JsonPointer(refTokens.appendedAll(refToks))

  /**
   * Whether this JsonPointer is the hierarchical ancestor of the given JsonPointer.
   *
   * For example, `/a/b` is ancestor of `/a/b/c/d`, but not the inverse.
   * @param other the other JsonPointer
   * @return true if this is an ancestor, otherwise false
   */
  def isAncestorOf(other: JsonPointer): Boolean = {
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
  val Root: JsonPointer = new JsonPointer(Seq(""))
  
  def apply(refTokens: Seq[String]): JsonPointer =
    if (refTokens.isEmpty) throw new IllegalArgumentException("Invalid JSON Pointer")
    if (refTokens.length == 1 && refTokens.head.isEmpty) return Root
    new JsonPointer(refTokens)
    
  def apply(s: String): JsonPointer = {
    if (s.isEmpty) return Root
    if (s.charAt(0) != '/') throw new IllegalArgumentException(s"Invalid JSON Pointer '$s'")

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
      return JsonPointer(refs.toList)
    }
    
    throw new IllegalArgumentException(s"Invalid JSON Pointer '$s'")
  }

  private def escape(ref: String): String = {
    ref.replace("~0", "~").replace("~1", "/") // perf: replace w/regex?
  }
}
