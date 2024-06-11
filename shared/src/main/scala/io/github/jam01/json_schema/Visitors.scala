package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, LinkedHashMap, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CompositeVisitor[-T, +J](delegates: Seq[Visitor[T, J]]) extends JsonVisitor[Seq[T], Seq[J]] {
  override def visitNull(index: Int): Seq[J] = delegates.map(_.visitNull(index))
  override def visitFalse(index: Int): Seq[J] = delegates.map(_.visitFalse(index))
  override def visitTrue(index: Int): Seq[J] = delegates.map(_.visitTrue(index))
  override def visitFloat64(d: Double, index: Int): Seq[J] = delegates.map(_.visitFloat64(d, index))
  override def visitInt64(i: Long, index: Int): Seq[J] = delegates.map(_.visitInt64(i, index))
  override def visitString(s: CharSequence, index: Int): Seq[J] = delegates.map(_.visitString(s, index))
  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], Seq[J]] =
    new CompositeArrVisitor[T, J](delegates.map(_.visitArray(length, index)))
  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[T], Seq[J]] =
    new CompositeObjVisitor[T, J](delegates.map(_.visitObject(length, true, index)))
}

class CompositeArrVisitor[-T, +J](val delArrVis: Seq[ArrVisitor[T, J]]) extends ArrVisitor[Seq[T], Seq[J]] {
  override def subVisitor: Visitor[?, ?] = new CompositeVisitor(delArrVis.map(_.subVisitor))

  override def visitValue(v: Seq[T], index: Int): Unit =
    delArrVis.lazyZip(v).foreach((v, o) => v.visitValue(o, index)) // perf: extra LazyZip2 object overhead

  override def visitEnd(index: Int): Seq[J] = delArrVis.map(_.visitEnd(index))
}

class CompositeObjVisitor[-T, +J](val delObjVis: Seq[ObjVisitor[T, J]]) extends ObjVisitor[Seq[T], Seq[J]] {
  override def visitKey(index: Int): Visitor[?, ?] = new CompositeVisitor(delObjVis.map(_.visitKey(index)))

  override def visitKeyValue(v: Any): Unit =
    delObjVis.lazyZip(v.asInstanceOf[Seq[?]]).foreach((v, o) => v.visitKeyValue(o))

  override def subVisitor: Visitor[?, ?] = new CompositeVisitor(delObjVis.map(_.subVisitor))

  override def visitValue(v: Seq[T], index: Int): Unit =
    delObjVis.lazyZip(v).foreach((v, o) => v.visitValue(o, index))

  override def visitEnd(index: Int): Seq[J] = delObjVis.map(_.visitEnd(index))
}


class MapCompositeVisitor[-T, +J, Z](delegates: Seq[Visitor[T, J]], f: Seq[J] => Z) extends JsonVisitor[Seq[T], Z] {
  override def visitNull(index: Int): Z = f(delegates.map(_.visitNull(index)))

  override def visitFalse(index: Int): Z = f(delegates.map(_.visitFalse(index)))

  override def visitTrue(index: Int): Z = f(delegates.map(_.visitTrue(index)))

  override def visitInt64(i: Long, index: Int): Z = f(delegates.map(_.visitInt64(i, index)))

  override def visitFloat64(d: Double, index: Int): Z = f(delegates.map(_.visitFloat64(d, index)))

  override def visitString(s: CharSequence, index: Int): Z = f(delegates.map(_.visitString(s, index)))

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], Z] =
    new MapCompositeArrContext[T, J, Z](delegates.map(_.visitArray(length, index)), f)

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[T], Z] =
    new MapCompositeObjContext[T, J, Z](delegates.map(_.visitObject(length, true, index)), f)
}

class MapCompositeArrContext[-T, +J, Z](val delegates: Seq[ArrVisitor[T, J]], f: Seq[J] => Z) extends ArrVisitor[Seq[T], Z] {
  override def subVisitor: Visitor[?, ?] = new CompositeVisitor(delegates.map(_.subVisitor))

  override def visitValue(v: Seq[T], index: Int): Unit =
    delegates.lazyZip(v).foreach((v, o) => v.visitValue(o, index))

  override def visitEnd(index: Int): Z = f(delegates.map(_.visitEnd(index)))
}

class MapCompositeObjContext[-T, +J, Z](val delegates: Seq[ObjVisitor[T, J]], f: Seq[J] => Z) extends ObjVisitor[Seq[T], Z] {
  override def visitKey(index: Int): Visitor[?, ?] =
    new CompositeVisitor(delegates.map(_.visitKey(index)))

  override def visitKeyValue(v: Any): Unit =
    delegates.lazyZip(v.asInstanceOf[Seq[?]]).foreach((v, o) => v.visitKeyValue(o))

  override def subVisitor: Visitor[?, ?] = new CompositeVisitor(delegates.map(_.subVisitor))

  override def visitValue(v: Seq[T], index: Int): Unit =
    delegates.lazyZip(v).foreach((v, o) => v.visitValue(o, index))

  override def visitEnd(index: Int): Z = f(delegates.map(_.visitEnd(index)))
}


object LiteralVisitor extends JsonVisitor[Value, Value] {
  override def visitNull(index: Int): Value = Null
  override def visitFalse(index: Int): Value = False
  override def visitTrue(index: Int): Value = True
  override def visitFloat64(d: Double, index: Int): Value = Num(d)
  override def visitInt64(i: Long, index: Int): Value = Num(i)
  override def visitString(s: CharSequence, index: Int): Value = Str(s.toString)
  override def visitObject(length: Int, index: Int): ObjVisitor[Value, Obj] = new CollectObjVisitor(LiteralVisitor, length, index)
  override def visitArray(length: Int, index: Int): ArrVisitor[Value, Arr] = new CollectArrVisitor(LiteralVisitor, length, index)
}

class CollectObjVisitor(val vis: Visitor[Value, Value], length: Int, index: Int) extends ObjVisitor[Value, Obj] {
  def this(vis: Visitor[Value, Value]) = this(vis, -1, -1)
  
  private val map = Map.newBuilder[String, Value]
  protected var _key: String = "?"
  override def visitKey(index: Int): Visitor[?, ?] = StringVisitor
  override def visitKeyValue(v: Any): Unit = _key = v.asInstanceOf[String]
  override def subVisitor: Visitor[?, ?] = vis
  override def visitValue(v: Value, index: Int): Unit = map.addOne(_key, v)
  override def visitEnd(index: Int): Obj = Obj(map.result())
}

class CollectArrVisitor(val vis: Visitor[Value, Value], length: Int, index: Int) extends ArrVisitor[Value, Arr] {
  def this(vis: Visitor[Value, Value]) = this(vis, -1, -1)

  private val buff = new ListBuffer[Value]
  override def subVisitor: Visitor[?, ?] = vis
  override def visitValue(v: Value, index: Int): Unit = buff.addOne(v)
  override def visitEnd(index: Int): Arr = Arr(buff.result())
}


object StringVisitor extends SimpleVisitor[Any, String] {
  def expectedMsg = "Expected string"

  override def visitString(s: CharSequence, index: Int): String = s.toString
}
