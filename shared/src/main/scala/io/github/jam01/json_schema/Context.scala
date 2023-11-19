package io.github.jam01.json_schema

import scala.collection.{immutable, mutable}

// TODO: consider making a record 
case class Context(insloc: mutable.Stack[String], // TODO: consider making coll.Seq
                   reg: collection.Map[String, Schema]) {
  def insPtr: String = JsonPointer.strValueOf(insloc.reverseIterator)

  def getSch(s: String): Option[Schema] = {
    val ptr = s.lastIndexOf("#/")
    if (ptr >= 0)
      reg.get(s.substring(0, ptr)).map(_.schBy(JsonPointer(s.substring(ptr + 1))))
    else
      reg.get(s)
  }
}

object Context {
  def empty: Context = Context(mutable.Stack(""),
    immutable.Map.empty[String, Schema])
}
