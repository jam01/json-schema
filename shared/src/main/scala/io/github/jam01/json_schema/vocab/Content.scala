package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Content.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

final class Content private(schema: ObjectSchema,
                           ctx: Context,
                           path: JsonPointer,
                           dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {
  
  private val result = Seq(mkUnit(true, ContentKw, annotation = schema.get(ContentKw).get))
    // perf: could not compute if gonna be dropped

  override def visitNull(index: Int): Seq[OutputUnit] = result
  override def visitFalse(index: Int): Seq[OutputUnit] = result
  override def visitTrue(index: Int): Seq[OutputUnit] = result
  override def visitInt64(l: Long, index: Int): Seq[OutputUnit] = result
  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = result
  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = result
  override def visitArray(length: Int, index: Int): ArrVisitor[?, Seq[OutputUnit]] = constArrVis
  private val constArrVis = new ArrVisitor[Any, Seq[OutputUnit]] {
    override def subVisitor: Visitor[_, _] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = result
  }
  override def visitObject(length: Int, index: Int): ObjVisitor[?, Seq[OutputUnit]] = constObjVis
  private val constObjVis = new ObjVisitor[Any, Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[_, _] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[_, _] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = result
  }
}

object Content extends VocabFactory[Content] {
  val ContentKw: String = "content"

  override def uri: String = "https://json-schema.org/draft/2020-12/vocab/content"
  override def shouldApply(schema: ObjectSchema): Boolean = schema.value.contains(ContentKw)
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): Content = 
    new Content(schema, ctx, path, dynParent)
}
