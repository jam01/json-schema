package io.github.jam01.json_schema

import scala.collection.{immutable, mutable}

case class Context(insloc: mutable.Stack[String], // TODO: consider making coll.Seq -- need to make a factory-type of class
                   reg: collection.Map[Uri, Schema],
                   mode: Mode = Mode.Assertion,
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
      reg.get(s).orElse(reg.get(Uri.of(s.toString, true)))
    else
      reg.get(Uri.of(s.toString.substring(0, ptr)))
        .map(sch => sch.schBy(JsonPointer(s.toString.substring(ptr + 1))))
  }

  def getDynSchOrThrow(loc: Uri, current: BaseValidator): Schema =
    getDynSch(loc, current) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"Unavailable schema $loc")

  def getDynSch(loc: Uri, current: BaseValidator): Option[Schema] = {
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
      OutputUnit(units.map(_.valid).forall(identity))
    }

  val Basic: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }

  val Detailed: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      val (annots, errs) = units.partition(_.valid)
      if (errs.nonEmpty)
        OutputUnit(false, Some(path), None, Some(ctx.currentLoc), errors = errs)
      else
        OutputUnit(true, Some(path), None, Some(ctx.currentLoc), annotations = annots)
    }

  val Verbose: OutputStructure = new OutputStructure:
    override def compose(path: JsonPointer, units: Seq[OutputUnit], ctx: Context): OutputUnit = {
      ???
    }
}

enum Mode {
  case Assertion, Annotation
}
