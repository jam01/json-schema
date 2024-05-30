package io.github.jam01.json_schema

import scala.collection.{immutable, mutable}

case class Context(insloc: mutable.Stack[String], // TODO: consider making coll.Seq -- need to make a factory-type of class
                   reg: collection.Map[Uri, Schema],
                   mode: Mode = Mode.Annotation,
                   struct: OutputStructure = OutputStructure.Detailed,
                   ffast: Boolean = false,
                   allowAnnot: Seq[String] = Nil) {
  def currentLoc: JsonPointer = JsonPointer(insloc.reverseIterator.toSeq)

  def getSchOrThrow(s: Uri): Schema =
    getSch(s) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"Unavailable schema $s")

  def getSch(s: Uri): Option[Schema] = {
    val ptr = s.toString.lastIndexOf("#/")
    if (ptr == -1)
      reg.get(s).orElse(reg.get(s.asDyn))
    else
      reg.get(s.withoutFragment)
        .map(sch => sch.schBy(JsonPointer(s.uri.getFragment))) // using decoded fragment as map values would be unencoded
  }

  def getDynSchOrThrow(loc: Uri, current: BaseValidator): Schema =
    getDynSch(loc, current) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"Unavailable schema $loc")

  def getDynSch(loc: Uri, current: BaseValidator): Option[Schema] = {
    if (loc.toString.contains("#/")) return getSch(loc.asStatic)

    val sch0 = getSch(loc)
    if (sch0.isEmpty) return getSch(loc.asStatic) // trying w/o dynamic

    val dynScope = mutable.ArrayBuffer(current.schema)
    var head = current
    while (head.dynParent.nonEmpty) {
      dynScope.addOne(head.dynParent.get.schema)
      head = head.dynParent.get
    }

    dynScope.reverseIterator
      .map(osch => osch.base.withFragment(loc.uri.getFragment, true))
      .find(dref => reg.contains(dref))
      .flatMap(dref => reg.get(dref))
  }

  def isVerbose: Boolean = struct == OutputStructure.Verbose

  val annots: mutable.Buffer[OutputUnit] = mutable.ArrayBuffer()
  def add(u: collection.Seq[OutputUnit]): collection.Seq[OutputUnit] = { annots.addAll(u); u }
  def clear(): Unit = annots.clear()
}

object Context {
  val Empty: Context = Context(mutable.Stack(""),
    immutable.Map.empty[Uri, Schema])
}

sealed abstract class OutputStructure {
  def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit
}

object OutputStructure {
  val Flag: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }

  val Basic: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }

  val Detailed: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      val (annots, errs) = units.partition(_.vvalid)
      if (errs.nonEmpty)
        OutputUnit(false, path, None, ctx.currentLoc, errors = errs)
      else
        OutputUnit(true, path, None, ctx.currentLoc, annotations = annots)
    }

  val Verbose: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      Detailed.compose(path, units, ctx)
    }
}

enum Mode {
  case Assertion, Annotation
}
