package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

class CompositeValidator[-T, +J](delegates: Visitor[T, J]*) extends JsonVisitor[Seq[_], Seq[J]] {
  override def visitNull(index: Int): Seq[J] = delegates.map(_.visitNull(index))

  override def visitFalse(index: Int): Seq[J] = delegates.map(_.visitFalse(index))

  override def visitTrue(index: Int): Seq[J] = delegates.map(_.visitTrue(index))

  override def visitFloat64(d: Double, index: Int): Seq[J] = delegates.map(_.visitFloat64(d, index))

  override def visitInt64(i: Long, index: Int): Seq[J] = delegates.map(_.visitInt64(i, index))

  override def visitString(s: CharSequence, index: Int): Seq[J] = delegates.map(_.visitString(s, index))

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[_], Seq[J]] =
    new CompositeArrVisitor[T, J](delegates.map(_.visitArray(length, index)): _*)

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[_], Seq[J]] = ???
}

class CompositeArrVisitor[-T, +J](delArrVis: ArrVisitor[T, J]*) extends ArrVisitor[Seq[_], Seq[J]] {
  // unsure why ArrVisitor[Seq[T], Seq[J]] doesn't work when delArrVis if Arr[_, J]
  //  .../json_schema/ObjectSchemaValidator.scala:99:78

  //  Found:    (delegArrVis : Seq[upickle.core.ArrVisitor[?, Boolean]])
  //  Required: Seq[upickle.core.ArrVisitor[Any, Boolean]] |
  //    Array[? <: upickle.core.ArrVisitor[Any, Boolean]]
  //  new MapArrContext[Seq[_], Seq[Boolean], Boolean](new CompositeArrVisitor(delegArrVis: _*), _.forall(identity)) {

  override def subVisitor: Visitor[_, _] =
    new CompositeValidator(delArrVis.map(_.subVisitor): _*)

  override def visitValue(v: Seq[_], index: Int): Unit = {
    var i = 0
    while (i < delArrVis.length) {
      delArrVis(i).visitValue(v(i).asInstanceOf[T], index)
      i = i + 1
    }
  }

  override def visitEnd(index: Int): Seq[J] = delArrVis.map(_.visitEnd(index))
}


class DynDelegateArrVisitor[-T, +J](delegate: ArrVisitor[T, J]) extends ArrVisitor[Any, J] {
  override def subVisitor: Visitor[_, _] = delegate.subVisitor
  override def visitValue(v: Any, index: Int): Unit = delegate.visitValue(v.asInstanceOf[T], index)
  override def visitEnd(index: Int): J = delegate.visitEnd(index)
}
