package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Metadata.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

import scala.collection.mutable.ListBuffer

final class Metadata private(schema: ObjectSchema,
                           ctx: Context,
                           path: JsonPointer,
                           dynParent: Option[Vocab[?]]) extends VocabBase(schema, ctx, path, dynParent) {
  
  private def result(): Seq[OutputUnit] = {
    val buff = new ListBuffer[OutputUnit]
    schema.get(Title).forall(title => accumulate(buff, true, Title, annotation = title))
    schema.get(Description).forall(desc => accumulate(buff, true, Description, annotation = desc))
    schema.get(Default).forall(defa => accumulate(buff, true, Default, annotation = defa))
    schema.get(Deprecated).forall(depr => accumulate(buff, true, Deprecated, annotation = depr))
    schema.get(ReadOnly).forall(r => accumulate(buff, true, ReadOnly, annotation = r))
    schema.get(WriteOnly).forall(w => accumulate(buff, true, WriteOnly, annotation = w))
    schema.get(Examples).forall(ex => accumulate(buff, true, Examples, annotation = ex))
    buff.result()
  }

  override def visitNull(index: Int): Seq[OutputUnit] = result()
  override def visitFalse(index: Int): Seq[OutputUnit] = result()
  override def visitTrue(index: Int): Seq[OutputUnit] = result()
  override def visitFloat64(d: Double, index: Int): Seq[OutputUnit] = result()
  override def visitInt64(i: Long, index: Int): Seq[OutputUnit] = result()
  override def visitString(s: CharSequence, index: Int): Seq[OutputUnit] = result()
  override def visitArray(length: Int, index: Int): ArrVisitor[?, Seq[OutputUnit]] = constArrVis
  private val constArrVis = new ArrVisitor[Any, Seq[OutputUnit]] {
    override def subVisitor: Visitor[_, _] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = result()
  }
  override def visitObject(length: Int, index: Int): ObjVisitor[?, Seq[OutputUnit]] = constObjVis
  private val constObjVis = new ObjVisitor[Any, Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[_, _] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[_, _] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): Seq[OutputUnit] = result()
  }
}

object Metadata extends VocabFactory[Metadata] {
  val Title: String = "title"
  val Description: String = "description"
  val Default: String = "default"
  val Deprecated: String = "deprecated"
  val ReadOnly: String = "readOnly"
  val WriteOnly: String = "writeOnly"
  val Examples: String = "examples"

  val Keys: Set[String] = Set(Title, Description, Default, Deprecated, ReadOnly, WriteOnly, Examples)

  override def uri: String = "https://json-schema.org/draft/2020-12/meta/meta-data"
  override def shouldApply(schema: ObjectSchema): Boolean = Keys.exists(schema.value.contains)
  override def create(schema: ObjectSchema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): Metadata =
    new Metadata(schema, ctx, path, dynParent)
}
