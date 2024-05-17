package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema._
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable

class Core(schema: ObjectSchema, 
           ctx: Context = Context.empty, 
           schloc: JsonPointer = JsonPointer(), 
           dynParent: Option[VocabValidator] = None) extends VocabValidator(schema, ctx, schloc, dynParent) {

  private val _refVis: Option[JsonVisitor[_, Boolean]] = schema.getRef
    .map(s => ctx.getSch(s) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"unavailable schema $s"))
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("$ref"), Some(this)))
  private val _dynRefVis: Option[JsonVisitor[_, Boolean]] = schema.getDynRef
    .map(s => ctx.getDynSch(s, this) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"unavailable schema $s"))
    .map(sch => SchemaValidator.of(sch, ctx, schloc.appended("$dynamicRef"), Some(this)))

  override def visitNull(index: Int): Boolean = {
    _refVis.forall(_.visitNull(index)) &&
      _dynRefVis.forall(_.visitNull(index))
  }

  override def visitFalse(index: Int): Boolean = {
    _refVis.forall(_.visitFalse(index)) &&
      _dynRefVis.forall(_.visitFalse(index))
  }

  override def visitTrue(index: Int): Boolean = {
    _refVis.forall(_.visitTrue(index)) &&
      _dynRefVis.forall(_.visitTrue(index))
  }

  override def visitInt64(l: Long, index: Int): Boolean = {
    _refVis.forall(_.visitInt64(l, index)) &&
      _dynRefVis.forall(_.visitInt64(l, index))
  }

  override def visitFloat64(d: Double, index: Int): Boolean = {
    _refVis.forall(_.visitFloat64(d, index)) &&
      _dynRefVis.forall(_.visitFloat64(d, index))
  }

  override def visitString(s: CharSequence, index: Int): Boolean = {
    _refVis.forall(_.visitString(s, index)) &&
      _dynRefVis.forall(_.visitString(s, index))
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[_, Boolean] = {
    val insVisitors = mutable.ArrayBuffer[ArrVisitor[_, Boolean]]()
    _refVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))

    if (insVisitors.length == 1) insVisitors.head
    else new CompositeArrVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[_, Boolean] = {
    val insVisitors = mutable.ArrayBuffer.empty[ObjVisitor[_, Boolean]]
    _refVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, index)))

    if (insVisitors.length == 1) insVisitors.head
    else new CompositeObjVisitorReducer(_.forall(identity), insVisitors.toSeq: _*)
  }
}
