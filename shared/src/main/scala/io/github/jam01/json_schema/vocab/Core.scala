package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Core.*
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

final class Core private(schema: ObjectSchema,
                   ctx: Context,
                   path: JsonPointer,
                   dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {

  // these are made lazy to avoid infinite-loops of recursive schemas such as 2020-12 meta schema
  private lazy val _refVis: Option[Visitor[?, OutputUnit]] = schema.getRef
    .map(ref => ctx.getSchOrThrow(ref))
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(_Ref), Some(this)))
  private lazy val _dynRefVis: Option[Visitor[?, OutputUnit]] = schema.getDynRef
    .map(dynref => ctx.getDynSchOrThrow(dynref, this))
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(_DynRef), Some(this)))

  override def visitNull(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    accumulateOpt(buff, _refVis.map(v => v.visitNull(index))) &&
      accumulateOpt(buff, _dynRefVis.map(v => v.visitNull(index)))
    buff.result()
  }

  override def visitFalse(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    accumulateOpt(buff, _refVis.map(v => v.visitFalse(index))) &&
      accumulateOpt(buff, _dynRefVis.map(v => v.visitFalse(index)))
    buff.result()
  }

  override def visitTrue(index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    accumulateOpt(buff, _refVis.map(v => v.visitTrue(index))) &&
      accumulateOpt(buff, _dynRefVis.map(v => v.visitTrue(index)))
    buff.result()
  }

  override def visitInt64(num: Long, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    accumulateOpt(buff, _refVis.map(v => v.visitInt64(num, index))) &&
      accumulateOpt(buff, _dynRefVis.map(v => v.visitInt64(num, index)))
    buff.result()
  }

  override def visitFloat64(num: Double, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    accumulateOpt(buff, _refVis.map(v => v.visitFloat64(num, index))) &&
      accumulateOpt(buff, _dynRefVis.map(v => v.visitFloat64(num, index)))
    buff.result()
  }

  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    accumulateOpt(buff, _refVis.map(v => v.visitString(s, index))) &&
      accumulateOpt(buff, _dynRefVis.map(v => v.visitString(s, index)))
    buff.result()
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[Seq[Nothing], Seq[OutputUnit]] = {
    val insVisitors = new ListBuffer[ArrVisitor[Nothing, OutputUnit]]
    _refVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitArray(length, index)))
    new CompositeArrVisitor(insVisitors.toSeq) // Vis[Seq[Nothing], Seq[OUnit]]
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Seq[Nothing], Seq[OutputUnit]] = {
    val insVisitors = new ListBuffer[ObjVisitor[Nothing, OutputUnit]]
    _refVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    _dynRefVis.foreach(vis => insVisitors.addOne(vis.visitObject(length, true, index)))
    new CompositeObjVisitor(insVisitors.toSeq) // Vis[Seq[Nothing], Seq[OUnit]]
  }
}

object Core extends VocabBaseFactory {
  val _Ref = "$ref"
  val _DynRef = "$dynamicRef"
  val Keys: Seq[String] = Seq(_Ref, _DynRef)

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/core"

  override def create(schema: ObjectSchema,
                      ctx: Context,
                      path: JsonPointer,
                      dynParent: Option[Vocab[?]]): VocabBase = new Core(schema, ctx, path, dynParent)

  override def shouldApply(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
}
