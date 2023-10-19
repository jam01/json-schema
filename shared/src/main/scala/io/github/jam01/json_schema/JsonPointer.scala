package io.github.jam01.json_schema

import scala.collection.immutable

case class JsonPointer(refTokens: immutable.Seq[String]) {
  def apply(s: String): JsonPointer = {
    JsonPointer(s.split('/'))
  }
}
