package io.github.jam01.json_schema

final case class JsonPointer(refTokens: Seq[String] = Seq("")) {
  if (refTokens.isEmpty) throw new IllegalArgumentException("invalid JSON Pointer")

  def appended(refToks: String*): JsonPointer = JsonPointer(refTokens.appendedAll(refToks))

  def appended(other: JsonPointer): JsonPointer = JsonPointer(refTokens.appendedAll(other.refTokens.tail))

  def isRelative(other: JsonPointer): Boolean = {
    if (other.refTokens.size < refTokens.size) false
    else refTokens.lazyZip(other.refTokens).forall((pstr, ostr) => pstr == ostr)
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
