package io.github.jam01.json_schema

import scala.collection.mutable

trait Context {
  def config: Config
  def isVerbose: Boolean = config.struct == OutputStructure.Verbose
  def currentLoc: JsonPointer

  def getSch(s: Uri): Option[Schema]
  def getDynSch(loc: Uri, current: Vocab[?]): Option[Schema]
  def getSchOrThrow(s: Uri): Schema =
    getSch(s) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"Unavailable schema $s")
  def getDynSchOrThrow(loc: Uri, current: Vocab[?]): Schema =
    getDynSch(loc, current) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"Unavailable schema $loc")

  def internal(dynpath: JsonPointer): collection.Seq[OutputUnit]
  def add(dynpath: JsonPointer, units: collection.Seq[OutputUnit]): collection.Seq[OutputUnit]
  def clear(dynpath: JsonPointer): Unit
}

case class SimpleContext(private val reg: collection.Map[Uri, Schema],
                         config: Config = Config.Default) extends Context with Tracker {

  private val insloc = mutable.Stack[String]("")
  private var _pointer = JsonPointer.Root
  override def push(ref: String): Unit = {
    insloc.push(ref)
    _pointer = JsonPointer(insloc.reverseIterator.toSeq)
  }
  override def pop: String = {
    val ref = insloc.pop
    _pointer = JsonPointer(insloc.reverseIterator.toSeq)
    ref
  }
  override def currentLoc: JsonPointer = _pointer
  override def pointer: JsonPointer = _pointer

  def getSch(s: Uri): Option[Schema] = {
    val ptr = s.toString.lastIndexOf("#/")
    if (ptr == -1)
      reg.get(s).orElse(reg.get(s.asDyn))
    else
      reg.get(s.withoutFragment)
        .map(sch => sch.schBy(JsonPointer(s.uri.getFragment))) // using decoded fragment as map values would be unencoded
  }

  def getDynSch(loc: Uri, current: Vocab[?]): Option[Schema] = {
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

  private val int: mutable.Map[JsonPointer, mutable.Buffer[OutputUnit]] = mutable.Map()
  override def internal(dynpath: JsonPointer): collection.Seq[OutputUnit] = int.getOrElse(dynpath, Nil)
  def add(dynpath: JsonPointer, units: collection.Seq[OutputUnit]): collection.Seq[OutputUnit] = {
    if (units.isEmpty) return units

    val appended = int.getOrElse(dynpath, mutable.ArrayBuffer()).addAll(units)
    int.put(dynpath, appended)
    units
  }

  def clear(dynpath: JsonPointer): Unit = int.remove(dynpath)
}

object SimpleContext {
  val Empty: SimpleContext = SimpleContext(Map.empty[Uri, Schema], Config.Default)
}
