package io.github.jam01.json_schema

import scala.collection.mutable

case class Context(insloc: mutable.Stack[String], // TODO: consider making coll.IndexedSeq
                   schloc: mutable.Stack[String],
                   reg: Map[String, Schema])

object Context {
  def empty: Context = Context(mutable.Stack.empty[String],
    mutable.Stack.empty[String],
    Map.empty[String, Schema])
}
