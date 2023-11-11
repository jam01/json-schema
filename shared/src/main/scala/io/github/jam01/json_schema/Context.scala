package io.github.jam01.json_schema

import scala.collection.{immutable, mutable}

case class Context(insloc: mutable.Stack[String], // TODO: consider making coll.Seq
                   reg: immutable.Map[String, Schema]) {
  def insPtr: String = JsonPointer.strValueOf(insloc.reverseIterator)
}

object Context {
  def empty: Context = Context(mutable.Stack(""),
    immutable.Map.empty[String, Schema])
}
