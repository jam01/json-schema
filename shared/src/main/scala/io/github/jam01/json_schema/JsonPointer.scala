package io.github.jam01.json_schema

import scala.collection.immutable

case class JsonPointer(refTokens: immutable.Seq[String]) {
  if (refTokens.isEmpty) throw new IllegalArgumentException("invalid JSON Pointer")
  // TODO: replace w/regex?
  def apply(s: String): JsonPointer = {
    JsonPointer(
      s.split('/')
        .map(_.replace("~0", "~").replace("~1", "/"))
    )
  }

  override def toString: String =
    refTokens
      .map(_.replace("~", "~0").replace("/", "~1"))
      .mkString("/")
}
