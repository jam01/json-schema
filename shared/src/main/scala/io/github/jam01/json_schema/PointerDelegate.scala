package io.github.jam01.json_schema

import upickle.core.Visitor.Delegate
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.collection.mutable

/**
 * A delegating visitor that keeps track of the current node visited as a JSON Pointer.
 *
 * For arrays, the child location will be set before fwding the subVisitor invocation. For objects, the entry location
 * will be set before fwding visitKey invocation.
 *
 * @param delegate the visitor to fwd nodes to
 * @param tracker the [[Tracker]] to receive reference tokens changes
 */
class PointerDelegate[T, V](tracker: Tracker, delegate: Visitor[T, V]) extends Delegate[T, V](delegate) {
  override def visitArray(length: Int, index: Int): ArrVisitor[T, V] = new ArrVisitor[T, V] {
    val arrVis: ArrVisitor[T, V] = delegate.visitArray(length, index)
    private var nextIdx = 0

    override def subVisitor: Visitor[?, ?] = {
      tracker.push(String.valueOf(nextIdx))
      new PointerDelegate(tracker, arrVis.subVisitor)
    }

    override def visitValue(v: T, index: Int): Unit = {
      arrVis.visitValue(v, index)
      tracker.pop
      nextIdx += 1
    }

    override def visitEnd(index: Int): V = { arrVis.visitEnd(index) }
  }

  override def visitObject(length: Int, jsonablekeys: Boolean, index: Int): ObjVisitor[T, V] = new ObjVisitor[T, V] {
    val objVis: ObjVisitor[T, V] = delegate.visitObject(length, jsonablekeys, index)

    override def visitKey(index: Int): Visitor[?, ?] = new SimpleVisitor[Nothing, Any] {
      override def expectedMsg: String = "expected string"

      override def visitString(s: CharSequence, index: Int): Any = {
        tracker.push(s.toString)
        objVis.visitKey(index).visitString(s, index)
      }
    }

    override def visitKeyValue(v: Any): Unit = objVis.visitKeyValue(v)

    override def subVisitor: Visitor[?, ?] = new PointerDelegate(tracker, objVis.subVisitor)

    override def visitValue(v: T, index: Int): Unit = { objVis.visitValue(v, index); tracker.pop }

    override def visitEnd(index: Int): V = { objVis.visitEnd(index) }
  }
}

trait Tracker {
  def push(ref: String): Unit
  def pop: String
}
