package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Unevaluated(schema: ObjectSchema,
                  ctx: Context = Context.Empty,
                  path: JsonPointer = JsonPointer(),
                  dynParent: Option[BaseValidator] = None) extends BaseValidator(schema, ctx, path, dynParent) {
  private val itemsVis: Option[ArrVisitor[OutputUnit, OutputUnit]] = schema.getSchemaOpt(Unevaluated.UnevaluatedItems)
    .map(sch => SchemaValidator.of(sch, ctx, path.appended(Unevaluated.UnevaluatedItems), Some(this)))
    .map(schValidator => new ArrVisitor[OutputUnit, OutputUnit] {
      private val units: mutable.ArrayBuffer[OutputUnit] = new ArrayBuffer

      override def subVisitor: Visitor[?, ?] = schValidator
      override def visitValue(u: OutputUnit, index: Int): Unit = units.addOne(u)
      override def visitEnd(index: Int): OutputUnit = and(path.appended(Unevaluated.UnevaluatedItems), units)
    })


  override def visitNull(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitFalse(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitTrue(index: Int): collection.Seq[OutputUnit] = Nil
  override def visitInt64(i: Long, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitFloat64(d: Double, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitString(s: CharSequence, index: Int): collection.Seq[OutputUnit] = Nil
  override def visitArray(length: Int, index: Int): ArrVisitor[?, collection.Seq[OutputUnit]] = new ArrVisitor[Any, collection.Seq[OutputUnit]] {
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
      Nil
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[?, collection.Seq[OutputUnit]] = new ObjVisitor[Any, collection.Seq[OutputUnit]] {
    override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
    override def visitKeyValue(v: Any): Unit = ()
    override def subVisitor: Visitor[?, ?] = NoOpVisitor
    override def visitValue(v: Any, index: Int): Unit = ()
    override def visitEnd(index: Int): collection.Seq[OutputUnit] = {
      println()
      Nil
    }
  }


  /* helper methods */
  private def and(kw: JsonPointer, units: collection.Seq[OutputUnit]): OutputUnit = { // TODO: if verbose?
    if (units.map(_.valid).forall(identity)) {
      OutputUnit(true, Some(kw), None, Some(ctx.currentLoc), None, units.filter(_.valid), None, Nil)
    } else {
      OutputUnit(false, Some(kw), None, Some(ctx.currentLoc), None, units.filterNot(_.valid), None, Nil)
    }
  }
}

object Unevaluated {
  val UnevaluatedItems = "unevaluatedItems"
}
