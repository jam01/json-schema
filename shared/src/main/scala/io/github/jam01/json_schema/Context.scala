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

  def getDynSch(s: Uri, oschval: ObjectSchemaValidator): Option[Schema] = {
    if (s.toString.contains("#/")) return getSch(Uri.of(s.toString, false))

    val sch0 = getSch(s)
    if (sch0.isEmpty) return getSch(Uri.of(s.toString, false)) // trying w/o dynamic

    val dynScope = mutable.ArrayBuffer(oschval.schema)
    var oschval0 = oschval
    while (oschval0.dynParent.nonEmpty) {
      dynScope.addOne(oschval0.dynParent.get.schema)
      oschval0 = oschval0.dynParent.get
    }

    val anchor = "#" + s.uri.getRawFragment
    dynScope.reverseIterator
      .map(osch => osch.getBase.resolve(anchor, true))
      .find(dref => reg.contains(dref))
      .flatMap(dref => reg.get(dref))
  }
}

object Context {
  def empty: Context = Context(mutable.Stack(""),
    immutable.Map.empty[Uri, Schema])
}
