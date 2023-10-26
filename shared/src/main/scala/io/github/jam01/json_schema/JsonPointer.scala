package io.github.jam01.json_schema

import scala.collection.immutable

case class JsonPointer(refTokens: immutable.Seq[String]) {
  if (refTokens.isEmpty) throw new IllegalArgumentException("invalid JSON Pointer")

  def apply(s: String): JsonPointer = {
    JsonPointer(s.split('/'))
  }

  override def toString: String =
    refTokens.mkString("/") // TODO: escape 
}
