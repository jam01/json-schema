package io.github.jam01.json_schema

import scala.collection.{immutable, mutable}

// TODO: consider making a record 
final case class Context(insloc: mutable.Stack[String], // TODO: consider making coll.Seq
                   reg: collection.Map[Uri, Schema]) {
  def insPtr: String = JsonPointer.strValueOf(insloc.reverseIterator)

  def getSch(s: Uri): Option[Schema] = {
    val ptr = s.toString.lastIndexOf("#/")
    if (ptr == -1)
      reg.get(s).orElse(reg.get(Uri.of(s.toString, true)))
    else
      reg.get(Uri.of(s.toString.substring(0, ptr)))
        .map(sch => sch.schBy(JsonPointer(s.toString.substring(ptr + 1))))
  }

  def getDynSch(loc: Uri, current: VocabValidator): Option[Schema] = {
    if (loc.toString.contains("#/")) return getSch(Uri.of(loc.toString, false))

    val sch0 = getSch(loc)
    if (sch0.isEmpty) return getSch(Uri.of(loc.toString, false)) // trying w/o dynamic

    val dynScope = mutable.ArrayBuffer(current.schema)
    var head = current
    while (head.dynParent.nonEmpty) {
      dynScope.addOne(head.dynParent.get.schema)
      head = head.dynParent.get
    }

    val anchor = "#" + loc.uri.getRawFragment
    dynScope.reverseIterator
      .map(osch => osch.getBase.resolve(anchor, true))
      .find(dref => reg.contains(dref))
      .flatMap(dref => reg.get(dref))
  }
}

object Context {
  def Empty: Context = Context(mutable.Stack(""),
    immutable.Map.empty[Uri, Schema])

  // TODO: should be int?
  object OutputStructure {
    val Flag: Byte = 0
    val Basic: Byte = 1
    val Detailed: Byte = 2
    val Verbose: Byte = 3
  }
  object Mode {
    val Assertion: Byte = 0
    val Annotation: Byte = 1
    val Both: Byte = 2
  }
}
