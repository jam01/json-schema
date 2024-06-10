package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

object SchemaValidator {
  def of(sch: Schema, ctx: Context, path: JsonPointer, dynParent: Option[Vocab[?]]): Visitor[?, OutputUnit] = {
    sch match
      case BooleanSchema(bool) => new BooleanSchemaValidator(bool, ctx, path)
      case osch: ObjectSchema =>
        val vocabs: Seq[Vocab[?]] = ctx.config.dialect.vocabularies
          .filter(vocabfact => vocabfact.shouldApply(osch))
          .map(vocabfact => vocabfact.create(osch, ctx, path, dynParent))

        new JsonVisitor[Seq[Nothing], OutputUnit] {
          private def compose(units: collection.Seq[OutputUnit]): OutputUnit = {
            val result = ctx.config.format.compose(path, units, ctx.instanceLoc)
            ctx.onScopeEnd(path, result)
            result
          }

          private def compose(f: Vocab[?] => collection.Seq[OutputUnit]): OutputUnit = {
            var res0: collection.Seq[OutputUnit] = Nil
            val it = vocabs.iterator
            var continue = true
            while (it.hasNext && continue) {
              val res1 = f(it.next())
              ctx.onVocabResults(path, res1)
              res0 = res0 :++ res1
              if (ctx.config.ffast && res1.exists(u => !u.vvalid)) continue = false
            }

            compose(res0)
          }

          def visitNull(index: Int): OutputUnit = compose(v => v.visitNull(index))
          def visitFalse(index: Int): OutputUnit = compose(v => v.visitFalse(index))
          def visitTrue(index: Int): OutputUnit = compose(v => v.visitTrue(index))
          def visitFloat64(d: Double, index: Int): OutputUnit = compose(v => v.visitFloat64(d, index))
          def visitInt64(i: Long, index: Int): OutputUnit = compose(v => v.visitInt64(i, index))
          def visitString(s: CharSequence, index: Int): OutputUnit = compose(v => v.visitString(s, index))
          override def visitArray(length: Int, index: Int): ArrVisitor[Seq[Nothing], OutputUnit] =
            new MapCompositeArrContext[Nothing, collection.Seq[OutputUnit], OutputUnit](vocabs.map(_.visitArray(length, index)), units => compose(units.flatten))
          override def visitObject(length: Int, index: Int): ObjVisitor[Seq[Nothing], OutputUnit] =
            new MapCompositeObjContext[Nothing, collection.Seq[OutputUnit], OutputUnit](vocabs.map(_.visitObject(length, index)), units => compose(units.flatten))
        }
  }
}

final class BooleanSchemaValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends JsonVisitor[OutputUnit, OutputUnit] {
  override def visitNull(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitFalse(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitTrue(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitFloat64(d: Double, index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitInt64(i: Long, index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitString(s: CharSequence, index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
  override def visitArray(length: Int, index: Int): ArrVisitor[OutputUnit, OutputUnit] = new BooleanArrValidator(bool, ctx, path)
  override def visitObject(length: Int, index: Int): ObjVisitor[OutputUnit, OutputUnit] = new BooleanObjValidator(bool, ctx, path)
}

final class BooleanArrValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ArrVisitor[Any, OutputUnit] {
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
}

final class BooleanObjValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ObjVisitor[Any, OutputUnit] {
  override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
  override def visitKeyValue(v: Any): Unit = ()
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, path, null, ctx.instanceLoc)
}
