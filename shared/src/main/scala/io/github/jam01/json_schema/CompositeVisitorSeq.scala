package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

class CompositorVisitor[-T, +J](delegates: Visitor[T, J]*) extends JsonVisitor[Seq[T], Seq[J]] {
  override def visitNull(index: Int): Seq[J] = delegates.map(_.visitNull(index))

  override def visitFalse(index: Int): Seq[J] = delegates.map(_.visitFalse(index))

  override def visitTrue(index: Int): Seq[J] = delegates.map(_.visitTrue(index))

  override def visitFloat64(d: Double, index: Int): Seq[J] = delegates.map(_.visitFloat64(d, index))

  override def visitInt64(i: Long, index: Int): Seq[J] = delegates.map(_.visitInt64(i, index))

  override def visitString(s: CharSequence, index: Int): Seq[J] = delegates.map(_.visitString(s, index))

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[T], Seq[J]] = new ArrVisitor[Seq[T], Seq[J]] {
    private val delArrVis: Seq[ArrVisitor[T, J]] = delegates.map(_.visitArray(length, index))

    override def subVisitor: Visitor[_, _] = new CompositorVisitor[Any, Any](delArrVis.map(_.subVisitor.asInstanceOf): _*)

    override def visitValue(v: Seq[T], index: Int): Unit = {
      var i = 0
      while (i < delArrVis.length) {
        delArrVis(i).visitValue(v(i), index)
        i = i + 1
      }
    }

    override def visitEnd(index: Int): Seq[J] = delArrVis.map(_.visitEnd(index))
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Any, Seq[J]] = ???
}

class CompositeArrVisitor[-T, +J](delArrVis: ArrVisitor[T, J]*) extends ArrVisitor[Seq[T], Seq[J]] {
  override def subVisitor: Visitor[_, _] = new CompositorVisitor[Any, Any](delArrVis.map(_.subVisitor.asInstanceOf): _*)

  override def visitValue(v: Seq[T], index: Int): Unit = {
    var i = 0
    while (i < delArrVis.length) {
      delArrVis(i).visitValue(v(i), index)
      i = i + 1
    }
  }

  override def visitEnd(index: Int): Seq[J] = delArrVis.map(_.visitEnd(index))
}
