package io.github.jam01.json_schema

import upickle.core.Visitor.MapReader
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

object SchemaValidator {
  def of(sch: Schema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): Visitor[?, OutputUnit] = {
    sch match
      case BooleanSchema(bool) => new BooleanSchemaValidator(bool, ctx, path)
      case osch: ObjectSchema =>
        val vocabs = ctx.config.dialect.vocabularies
          .filter(v => v.appliesTo(osch))
          .map(v => v.from(osch, ctx, path, dynParent))

        val comp: JsonVisitor[Seq[Nothing], Seq[collection.Seq[OutputUnit]]] =
//          new TapCompositeVisitor(units => (), vocabs*)
          new TapCompositeVisitor(units => ctx.publish(path, units), vocabs*)

        new MapReader[Seq[Nothing], Seq[collection.Seq[OutputUnit]], OutputUnit](comp) {
          override def mapNonNullsFunction(unitss: Seq[collection.Seq[OutputUnit]]): OutputUnit =
            val res = ctx.config.struct.compose(path, unitss.flatten, ctx)
            ctx.endDynScope(path, res)
            res
        }
  }
}

final class BooleanSchemaValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends JsonVisitor[OutputUnit, OutputUnit] {
  override def visitNull(index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
  override def visitFalse(index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
  override def visitTrue(index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
  override def visitFloat64(d: Double, index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
  override def visitInt64(i: Long, index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
  override def visitString(s: CharSequence, index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
  override def visitArray(length: Int, index: Int): ArrVisitor[OutputUnit, OutputUnit] = new BooleanArrValidator(bool, ctx, path)
  override def visitObject(length: Int, index: Int): ObjVisitor[OutputUnit, OutputUnit] = new BooleanObjValidator(bool, ctx, path)
}

final class BooleanArrValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ArrVisitor[Any, OutputUnit] {
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
}

final class BooleanObjValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ObjVisitor[Any, OutputUnit] {
  override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
  override def visitKeyValue(v: Any): Unit = ()
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, path, None, ctx.currentLoc)
}
