package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Core.*
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Core private(schema: ObjectSchema,
                   ctx: Context,
                   path: JsonPointer,
                   dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {

  private val _refVis: Option[Visitor[?, OutputUnit]] = schema.getRef
    .map(s => ctx.getSchOrThrow(s))
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(_Ref), Some(this)))
  private val _dynRefVis: Option[Visitor[?, OutputUnit]] = schema.getDynRef
    .map(s => ctx.getDynSchOrThrow(s, this))
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(_DynRef), Some(this)))

  override def visitNull(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.foreach(v => addUnit(units, v.visitNull(index)))
    _dynRefVis.foreach(v => addUnit(units, v.visitNull(index)))
    units
  }

  override def visitFalse(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.foreach(v => addUnit(units, v.visitFalse(index)))
    _dynRefVis.foreach(v => addUnit(units, v.visitFalse(index)))
    units
  }

  override def visitTrue(index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.foreach(v => addUnit(units, v.visitTrue(index)))
    _dynRefVis.foreach(v => addUnit(units, v.visitTrue(index)))
    units
  }

  override def visitInt64(l: Long, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    _dynRefVis.foreach(v => addUnit(units, v.visitInt64(l, index)))
    units
  }

  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    _dynRefVis.foreach(v => addUnit(units, v.visitFloat64(d, index)))
    units
  }

  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = {
    val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer(2) // perf: should be re-used?

    _refVis.foreach(v => addUnit(units, v.visitString(s, index)))
    _dynRefVis.foreach(v => addUnit(units, v.visitString(s, index)))
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

object Core extends VocabBaseFactory {
  val _Ref = "$ref"
  val _DynRef = "$dynamicRef"
  val Keys: Seq[String] = Seq(_Ref, _DynRef)

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/core"

  override def from(schema: ObjectSchema,
                    ctx: Context,
                    path: JsonPointer,
                    dynParent: Option[Vocab[?]]): VocabBase = new Core(schema, ctx, path, dynParent)

  override def appliesTo(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
}
