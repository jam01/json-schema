package io.github.jam01.json_schema

import java.net.URLDecoder
import java.nio.charset.Charset
import scala.collection.immutable.ArraySeq

final case class JsonPointer(refTokens: Seq[String] = Seq("")) {
  if (refTokens.isEmpty) throw new IllegalArgumentException("invalid JSON Pointer")

  def appended(refToks: String*): JsonPointer = JsonPointer(refTokens.appendedAll(refToks))

  def isRelative(other: JsonPointer): Boolean = {
    if (other.refTokens.size < refTokens.size) false
    else refTokens.lazyZip(other.refTokens).forall((pstr, ostr) => pstr == ostr)
  }

  override def toString: String = {
    refTokens.iterator
      .map(_.replace("~", "~0")
        .replace("/", "~1"))
      .mkString("/")
  }
}

object JsonPointer {
  val Empty: JsonPointer = JsonPointer()
  
  def apply(s: String): JsonPointer = {
    JsonPointer(URLDecoder.decode(s, Charset.defaultCharset()).split("/", -1)
      .toIndexedSeq
      .map(_.replace("~0", "~")
        .replace("~1", "/") // TODO: replace w/regex?
      ))
  }

  private def toString(it: collection.IterableOnce[String]): String = {
    it.iterator
      .map(_.replace("~", "~0")
        .replace("/", "~1"))
      .mkString("/")
  }
}
