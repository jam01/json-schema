package io.github.jam01.json_schema

import upickle.core.Visitor.Delegate
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

// TODO: can pass the Stack directly 
class PointerDelegate[T, V](delegate: Visitor[T, V], ctx: Context) extends Delegate[T, V](delegate) {
  override def visitArray(length: Int, index: Int): ArrVisitor[T, V] = new ArrVisitor[T, V] {
    val arrVis: ArrVisitor[T, V] = delegate.visitArray(length, index)
    private var nextIdx = 0

    override def subVisitor: Visitor[_, _] = {
      ctx.insloc.push(String.valueOf(nextIdx))
      new PointerDelegate(arrVis.subVisitor, ctx)
    }

    override def visitValue(v: T, index: Int): Unit = {
      arrVis.visitValue(v, index)
      ctx.insloc.pop
      nextIdx += 1
    }

    override def visitEnd(index: Int): V = { arrVis.visitEnd(index) }
  }

  override def visitObject(length: Int, jsonablekeys: Boolean, index: Int): ObjVisitor[T, V] = new ObjVisitor[T, V] {
    val objVis: ObjVisitor[T, V] = delegate.visitObject(length, jsonablekeys, index)

    override def visitKey(index: Int): Visitor[_, _] = new SimpleVisitor[Nothing, Any] {
      override def expectedMsg: String = "expected string"

      override def visitString(s: CharSequence, index: Int): Any = {
        ctx.insloc.push(s.toString)
        objVis.visitKey(index).visitString(s, index)
      }
    }

    override def visitKeyValue(v: Any): Unit = objVis.visitKeyValue(v)

    override def subVisitor: Visitor[_, _] = new PointerDelegate(objVis.subVisitor, ctx)

    override def visitValue(v: T, index: Int): Unit = { objVis.visitValue(v, index); ctx.insloc.pop }

    override def visitEnd(index: Int): V = { objVis.visitEnd(index) }
  }
}
