package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

class CompositeVisitorReducer[-T, +J](reducer: Seq[J] => J, delegates: Visitor[T, J]*) extends JsonVisitor[Seq[T], J] {
  override def visitNull(index: Int): J = reducer(delegates.map(_.visitNull(index)))

  override def visitFalse(index: Int): J = reducer(delegates.map(_.visitFalse(index)))

  override def visitTrue(index: Int): J = reducer(delegates.map(_.visitTrue(index)))

  override def visitFloat64(d: Double, index: Int): J = reducer(delegates.map(_.visitFloat64(d, index)))

  override def visitInt64(i: Long, index: Int): J = reducer(delegates.map(_.visitInt64(i, index)))

  override def visitString(s: CharSequence, index: Int): J = reducer(delegates.map(_.visitString(s, index)))

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], J] =
    new CompositeArrVisitorReducer[T, J](reducer, delegates.map(_.visitArray(length, index)): _*)

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[T], J] = ???
}


class CompositeArrVisitorReducer[-T, +J](reducer: Seq[J] => J, delArrVis: ArrVisitor[T, J]*) extends ArrVisitor[Seq[T], J] {
  override def subVisitor: Visitor[_, _] = new CompositeValidator[Any, Any](delArrVis.map(_.subVisitor.asInstanceOf): _*)

  override def visitValue(v: Seq[T], index: Int): Unit = {
    var i = 0
    while (i < delArrVis.length) {
      delArrVis(i).visitValue(v(i), index)
      i = i + 1
    }
  }

  override def visitEnd(index: Int): J = reducer(delArrVis.map(_.visitEnd(index)))
}