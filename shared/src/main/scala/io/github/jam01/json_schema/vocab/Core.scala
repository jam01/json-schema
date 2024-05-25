package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Core(schema: ObjectSchema,
           ctx: Context = Context.Empty,
           path: JsonPointer = JsonPointer(),
           dynParent: Option[BaseValidator] = None) extends BaseValidator(schema, ctx, path, dynParent) {

  private val _refVis: Option[Visitor[?, OutputUnit]] = schema.getRef
    .map(s => ctx.getSch(s) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"unavailable schema $s")) // TODO: add ctx.getSchOrThrow
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Core._Ref), Some(this)))
  private val _dynRefVis: Option[Visitor[?, OutputUnit]] = schema.getDynRef
    .map(s => ctx.getDynSch(s, this) match
      case Some(sch) => sch
      case None => throw new IllegalArgumentException(s"unavailable schema $s"))
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Core._DynRef), Some(this)))

  override def visitNull(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.map(rv => rv.visitNull(index)).foreach(u => addUnit(units, u))
    _dynRefVis.map(drv => drv.visitNull(index)).foreach(u => addUnit(units, u))
    units
  }

  override def visitFalse(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.map(rv => rv.visitFalse(index)).foreach(u => addUnit(units, u))
    _dynRefVis.map(drv => drv.visitFalse(index)).foreach(u => addUnit(units, u))
    units
  }

  override def visitTrue(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.map(rv => rv.visitTrue(index)).foreach(u => addUnit(units, u))
    _dynRefVis.map(drv => drv.visitTrue(index)).foreach(u => addUnit(units, u))
    units
  }

  override def visitInt64(l: Long, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.map(rv => rv.visitInt64(l, index)).foreach(u => addUnit(units, u))
    _dynRefVis.map(drv => drv.visitInt64(l, index)).foreach(u => addUnit(units, u))
    units
  }

  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.map(rv => rv.visitFloat64(d, index)).foreach(u => addUnit(units, u))
    _dynRefVis.map(drv => drv.visitFloat64(d, index)).foreach(u => addUnit(units, u))
    units
  }

  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.map(rv => rv.visitString(s, index)).foreach(u => addUnit(units, u))
    _dynRefVis.map(drv => drv.visitString(s, index)).foreach(u => addUnit(units, u))
    units
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[Nothing], collection.Seq[OutputUnit]] = {
    val insVisitors: mutable.ArrayBuffer[ArrVisitor[Nothing, OutputUnit]] = new ArrayBuffer(2)
    _refVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))

    new CompositeArrVisitor(insVisitors.toSeq*) // Vis[Seq[Nothing], Seq[OUnit]]
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[Nothing], collection.Seq[OutputUnit]] = {
    val insVisitors: mutable.ArrayBuffer[ObjVisitor[Nothing, OutputUnit]] = new ArrayBuffer(2)
    _refVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))

    new CompositeObjVisitor(insVisitors.toSeq*) // Vis[Seq[Nothing], Seq[OUnit]]
  }
}

object Core {
  private val _Ref = "$ref"
  private val _DynRef = "$dynamicRef"
}
