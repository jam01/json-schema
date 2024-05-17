package io.github.jam01.json_schema

import java.net.URLDecoder
import java.nio.charset.Charset

final case class JsonPointer(refTokens: Seq[String] = Seq("")) {
  if (refTokens.isEmpty) throw new IllegalArgumentException("invalid JSON Pointer")

  def appended(refToks: String*): JsonPointer = JsonPointer(refTokens.appendedAll(refToks))

  override def toString: String = JsonPointer.toString(refTokens)
}

object JsonPointer {
  val Empty: JsonPointer = JsonPointer()
  
  def apply(s: String): JsonPointer = {
    JsonPointer(URLDecoder.decode(s, Charset.defaultCharset()).split("/", -1)
      .map(_.replace("~0", "~")
        .replace("~1", "/") // TODO: replace w/regex?
      ))
  }

  // TODO: do we need this? public?
  def strValueOf(it: collection.IterableOnce[String]): String = {
    val itr = it.iterator
    if (!itr.hasNext) throw new IllegalArgumentException("invalid JSON Pointer")
    toString(itr)
  }

  private def toString(it: collection.IterableOnce[String]): String = {
    it.iterator
      .map(_.replace("~", "~0")
        .replace("/", "~1"))
      .mkString("/")
  }
}
