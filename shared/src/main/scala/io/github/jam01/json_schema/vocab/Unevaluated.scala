package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import io.github.jam01.json_schema.vocab.Unevaluated.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Unevaluated(schema: ObjectSchema,
                  ctx: Context = Context.Empty,
                  path: JsonPointer = JsonPointer(),
                  dynParent: Option[BaseValidator] = None) extends BaseValidator(schema, ctx, path, dynParent) {
  private val itemsVis: Option[ArrVisitor[OutputUnit, OutputUnit]] = schema.getSchemaOpt(UnevaluatedItems)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(UnevaluatedItems), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer

      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = addUnit(units, u)
      override def visitEnd(index: Int): OutputUnit = and(UnevaluatedItems, units)
    })
  private val propsVis: Option[ObjVisitor[OutputUnit, OutputUnit]] = schema.getSchemaOpt(UnevaluatedProperties)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(UnevaluatedProperties), Some(this)))
    .map(schValidator => new ObjVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer
      private var subsch = true

      override def visitKey(index: Int): Visitor[?, ?] = throw new UnsupportedOperationException("Should not be invoked")
      override def visitKeyValue(v: Any): Unit = throw new UnsupportedOperationException("Should not be invoked")
      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = addUnit(units, u)
      override def visitEnd(index: Int): OutputUnit = and(UnevaluatedProperties, units)
    })


  override def visitNull(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitFalse(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitTrue(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitInt64(i: Long, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitArray(length: Int, index: Int): ArrVisitor[Any, collection.Seq[OutputUnit]] = new ArrVisitor[Any, collection.Seq[OutputUnit]] {
    val childVisitor: ArrVisitor[?, ?] = itemsVis.getOrElse(NoOpVisitor.visitArray(length, index))
    override def subVisitor: Visitor[?, ?] = childVisitor.subVisitor
    override def visitValue(v: Any, index: Int): Unit = childVisitor.narrow.visitValue(v, index)
    override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
//      if (itemsVis.isEmpty) Nil
//      else {
//        Seq(itemsVis.get.visitEnd(index))
//      }
      Nil
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[Any, collection.Seq[OutputUnit]] = new ObjVisitor[Any, collection.Seq[OutputUnit]] {
    val childVisitor: ObjVisitor[?, ?] = propsVis.getOrElse(NoOpVisitor.visitObject(length, true, index))
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = childVisitor.narrow.visitValue(v, index)
    override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
//      if (propsVis.isEmpty) Nil
//      else {
//        Seq(propsVis.get.visitEnd(index))
//      }
      Nil
    }
  }


  /* helper methods */
  private def and(kw: String, units: collection.Seq[OutputUnit], ann: Option[Value] = None): OutputUnit = {
    val (annots, errs) = units.partition(_.valid)
    unitOf(errs.isEmpty, kw, None, errs, None, annots)
  }
}

object Unevaluated {
  val UnevaluatedItems = "unevaluatedItems"
  val UnevaluatedProperties = "unevaluatedProperties"
}
