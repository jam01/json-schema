package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, LinkedHashMap, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable

class CompositeVisitor[-T, +J](delegates: Visitor[T, J]*) extends JsonVisitor[Seq[T], Seq[J]] {
  override def visitNull(index: Int): Seq[J] = delegates.map(_.visitNull(index))
  override def visitFalse(index: Int): Seq[J] = delegates.map(_.visitFalse(index))
  override def visitTrue(index: Int): Seq[J] = delegates.map(_.visitTrue(index))
  override def visitFloat64(d: Double, index: Int): Seq[J] = delegates.map(_.visitFloat64(d, index))
  override def visitInt64(i: Long, index: Int): Seq[J] = delegates.map(_.visitInt64(i, index))
  override def visitString(s: CharSequence, index: Int): Seq[J] = delegates.map(_.visitString(s, index))
  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], Seq[J]] =
    new CompositeArrVisitor[T, J](delegates.map(_.visitArray(length, index))*)
  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[T], Seq[J]] =
    new CompositeObjVisitor[T, J](delegates.map(_.visitObject(length, true, index))*)
}

class CompositeArrVisitor[-T, +J](protected val delArrVis: ArrVisitor[T, J]*) extends ArrVisitor[Seq[T], Seq[J]] {
  override def subVisitor: Visitor[?, ?] =
    new CompositeVisitor(delArrVis.map(_.subVisitor)*)

  override def visitValue(v: Seq[T], index: Int): Unit =
    delArrVis.view.lazyZip(v).foreach((v, o) => v.visitValue(o, index))

  override def visitEnd(index: Int): Seq[J] = delArrVis.map(_.visitEnd(index))
}

class CompositeObjVisitor[-T, +J](protected val delObjVis: ObjVisitor[T, J]*) extends ObjVisitor[Seq[T], Seq[J]] {
  override def visitKey(index: Int): Visitor[?, ?] =
    new CompositeVisitor(delObjVis.map(_.visitKey(index))*)

  override def visitKeyValue(v: Any): Unit =
    delObjVis.view.lazyZip(v.asInstanceOf[Seq[?]]).foreach((v, o) => v.visitKeyValue(o))

  override def subVisitor: Visitor[?, ?] =
    new CompositeVisitor(delObjVis.map(_.subVisitor)*)

  override def visitValue(v: Seq[T], index: Int): Unit =
    delObjVis.view.lazyZip(v).foreach((v, o) => v.visitValue(o, index))

  override def visitEnd(index: Int): Seq[J] = delObjVis.map(_.visitEnd(index))
}


class CompositeVisitorReducer[-T, +J](reducer: Seq[J] => J, delegates: Visitor[T, J]*) extends JsonVisitor[Seq[T], J] {
  override def visitNull(index: Int): J = reducer(delegates.map(_.visitNull(index)))

  override def visitFalse(index: Int): J = reducer(delegates.map(_.visitFalse(index)))

  override def visitTrue(index: Int): J = reducer(delegates.map(_.visitTrue(index)))

  override def visitInt64(i: Long, index: Int): J = reducer(delegates.map(_.visitInt64(i, index)))

  override def visitFloat64(d: Double, index: Int): J = reducer(delegates.map(_.visitFloat64(d, index)))

  override def visitString(s: CharSequence, index: Int): J = reducer(delegates.map(_.visitString(s, index)))

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], J] =
    new CompositeArrVisitorReducer[T, J](reducer, delegates.map(_.visitArray(length, index))*)

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[T], J] =
    new CompositeObjVisitorReducer[T, J](reducer, delegates.map(_.visitObject(length, true, index))*)
}

class CompositeArrVisitorReducer[-T, +J](reducer: Seq[J] => J, delArrVis: ArrVisitor[T, J]*) extends ArrVisitor[Seq[T], J] {
  override def subVisitor: Visitor[?, ?] = new CompositeVisitor(delArrVis.map(_.subVisitor)*)

  override def visitValue(v: Seq[T], index: Int): Unit =
    delArrVis.view.lazyZip(v).foreach((v, o) => v.visitValue(o, index))

  override def visitEnd(index: Int): J = reducer(delArrVis.map(_.visitEnd(index)))
}

class CompositeObjVisitorReducer[-T, +J](reducer: Seq[J] => J, delObjVis: ObjVisitor[T, J]*) extends ObjVisitor[Seq[T], J] {

  override def visitKey(index: Int): Visitor[?, ?] =
    new CompositeVisitor(delObjVis.map(_.visitKey(index))*)

  override def visitKeyValue(v: Any): Unit =
    delObjVis.view.lazyZip(v.asInstanceOf[Seq[?]]).foreach((v, o) => v.visitKeyValue(o))

  override def subVisitor: Visitor[?, ?] = new CompositeVisitor(delObjVis.map(_.subVisitor)*)

  override def visitValue(v: Seq[T], index: Int): Unit =
    delObjVis.view.lazyZip(v).foreach((v, o) => v.visitValue(o, index))

  override def visitEnd(index: Int): J = reducer(delObjVis.map(_.visitEnd(index)))
}


object LiteralVisitor extends JsonVisitor[Value, Value] {
  override def visitNull(index: Int): Value = Null
  override def visitFalse(index: Int): Value = False
  override def visitTrue(index: Int): Value = True
  override def visitFloat64(d: Double, index: Int): Value = Num(d)
  override def visitInt64(i: Long, index: Int): Value = Num(i)
  override def visitString(s: CharSequence, index: Int): Value = Str(s.toString)
  override def visitObject(length: Int, index: Int): ObjVisitor[Value, Obj] = new CollectObjVisitor(LiteralVisitor)
  override def visitArray(length: Int, index: Int): ArrVisitor[Value, Arr] = new CollectArrVisitor(LiteralVisitor)
}

class CollectObjVisitor(protected val vis: Visitor[Value, Value]) extends ObjVisitor[Value, Obj] {
  val lhm: LinkedHashMap[String, Value] = LinkedHashMap()
  var k: String = "?"

  override def visitKey(index: Int): Visitor[?, ?] = StringVisitor
  override def visitKeyValue(v: Any): Unit = k = v.asInstanceOf[String]
  override def subVisitor: Visitor[?, ?] = vis
  override def visitValue(v: Value, index: Int): Unit = lhm.addOne(k, v)
  override def visitEnd(index: Int): Obj = Obj(lhm)
}

class CollectArrVisitor(protected val vis: Visitor[Value, Value]) extends ArrVisitor[Value, Arr] {
  val arr: mutable.ArrayBuffer[Value] = mutable.ArrayBuffer()

  override def subVisitor: Visitor[?, ?] = vis
  override def visitValue(v: Value, index: Int): Unit = arr.append(v)
  override def visitEnd(index: Int): Arr = Arr(arr)
}


object StringVisitor extends SimpleVisitor[Nothing, String] {
  def expectedMsg = "expected string"

  override def visitString(s: CharSequence, index: Int): String = s.toString
}


class PeekCompositeVisitor[-T, +J](peek: J => J, eff: () => Unit, delegates: Visitor[T, J]*)
  extends JsonVisitor[Seq[T], Seq[J]] {
  def visitNull(index: Int): Seq[J] = {
    val u = delegates.map(d => peek(d.visitNull(index))); eff.apply(); u }
  def visitFalse(index: Int): Seq[J] = {
    val u = delegates.map(d => peek(d.visitFalse(index))); eff.apply(); u }
  def visitTrue(index: Int): Seq[J] = {
    val u = delegates.map(d => peek(d.visitTrue(index))); eff.apply(); u }
  def visitFloat64(d: Double, index: Int): Seq[J] = {
    val u = delegates.map(dv => peek(dv.visitFloat64(d, index))); eff.apply(); u }
  def visitInt64(i: Long, index: Int): Seq[J] = {
    val u = delegates.map(d => peek(d.visitInt64(i, index))); eff.apply(); u }
  def visitString(s: CharSequence, index: Int): Seq[J] = {
    val u = delegates.map(d => peek(d.visitString(s, index))); eff.apply(); u }

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], Seq[J]] =
    new CompositeArrVisitor[T, J](delegates.map(_.visitArray(length, index)) *) {
      override def visitEnd(index: Int): Seq[J] = {
        val u = delArrVis.map(d => peek(d.visitEnd(index))); eff.apply(); u }
    }

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[T], Seq[J]] =
    new CompositeObjVisitor[T, J](delegates.map(_.visitObject(length, true, index)) *) {
      override def visitEnd(index: Int): Seq[J] = {
        val u = delObjVis.map(d => peek(d.visitEnd(index))); eff.apply(); u }
    }
}
