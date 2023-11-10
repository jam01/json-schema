package io.github.jam01.json_schema

import scala.collection.{immutable, mutable}

case class Context(insloc: mutable.Stack[String], // TODO: consider making coll.Seq
                   schloc: mutable.Stack[String],
                   reg: immutable.Map[String, Schema]) {
  def insPtr: String = JsonPointer.strValueOf(insloc.reverseIterator)

  def schPtr: String = JsonPointer.strValueOf(schloc.reverseIterator)

  def withSchLoc(loc: String): Context = Context(insloc, schloc.clone().push(loc), reg)
}

object Context {
  def empty: Context = Context(mutable.Stack.empty[String],
    mutable.Stack.empty[String],
    immutable.Map.empty[String, Schema])
}
