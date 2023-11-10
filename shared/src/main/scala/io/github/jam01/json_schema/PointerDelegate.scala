package io.github.jam01.json_schema

import upickle.core.Visitor.Delegate
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable

class PointerDelegate[T, V](delegate: Visitor[T, V], ctx: Context) extends Delegate[T, V](delegate) {
  override def visitArray(length: Int, index: Int): ArrVisitor[T, V] = new ArrVisitor[T, V] {
    val arrVis: ArrVisitor[T, V] = delegate.visitArray(length, index)
    private var nextIdx = 0

    override def subVisitor: Visitor[_, _] = {
      val res = new PointerDelegate(arrVis.subVisitor, ctx)
      ctx.insloc.push(String.valueOf(nextIdx))
      res
    }

    override def visitValue(v: T, index: Int): Unit = {
      arrVis.visitValue(v, index)
      ctx.insloc.pop
      nextIdx += 1
    }

    override def visitEnd(index: Int): V = { ctx.insloc.pop; arrVis.visitEnd(index) }
  }

  override def visitObject(length: Int, jsonablekeys: Boolean, index: Int): ObjVisitor[T, V] = new ObjVisitor[T, V] {
    val objVis: ObjVisitor[T, V] = delegate.visitObject(length, jsonablekeys, index)

    override def visitKey(index: Int): Visitor[_, _] = objVis.visitKey(index)

    override def visitKeyValue(v: Any): Unit = {
      ctx.insloc.push(v.asInstanceOf[String])
      objVis.visitKeyValue(v)
    }

    override def subVisitor: Visitor[_, _] = new PointerDelegate(objVis.subVisitor, ctx)

    override def visitValue(v: T, index: Int): Unit = { ctx.insloc.pop; objVis.visitValue(v, index) }

    override def visitEnd(index: Int): V = { ctx.insloc.pop; objVis.visitEnd(index) }
  }
}
