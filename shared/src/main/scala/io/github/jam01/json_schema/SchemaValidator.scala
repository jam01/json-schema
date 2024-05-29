package io.github.jam01.json_schema

import io.github.jam01.json_schema.vocab.Core
import upickle.core.Visitor.MapReader
import upickle.core.{ArrVisitor, NoOpVisitor, ObjVisitor, Visitor}

object SchemaValidator {
  def of(sch: Schema,
         ctx: Context = Context.Empty,
         path: JsonPointer = JsonPointer(),
         dynParent: Option[BaseValidator] = None): Visitor[?, OutputUnit] = {
    sch match
      case bs: BooleanSchema => BooleanSchemaValidator.of(bs, ctx, path)
      case os: ObjectSchema => ObjectSchemaValidator.of(os, ctx, path, dynParent)
  }
}

object BooleanSchemaValidator {
  def of(bsch: BooleanSchema, ctx: Context = Context.Empty, path: JsonPointer = JsonPointer()): BooleanSchemaValidator =
    bsch match
      case TrueSchema => new BooleanSchemaValidator(true, ctx, path)
      case FalseSchema => new BooleanSchemaValidator(false, ctx, path)
}

final class BooleanSchemaValidator(bool: Boolean, ctx: Context = Context.Empty, path: JsonPointer = JsonPointer()) extends JsonVisitor[OutputUnit, OutputUnit] {
  override def visitNull(index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
  override def visitFalse(index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
  override def visitTrue(index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
  override def visitFloat64(d: Double, index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
  override def visitInt64(i: Long, index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
  override def visitString(s: CharSequence, index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
  override def visitArray(length: Int, index: Int): ArrVisitor[OutputUnit, OutputUnit] = new BooleanArrValidator(bool, ctx, path)
  override def visitObject(length: Int, index: Int): ObjVisitor[OutputUnit, OutputUnit] = new BooleanObjValidator(bool, ctx, path)
}

final class BooleanArrValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ArrVisitor[Any, OutputUnit] {
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
}

final class BooleanObjValidator(bool: Boolean, ctx: Context, path: JsonPointer) extends ObjVisitor[Any, OutputUnit] {
  override def visitKey(index: Int): Visitor[?, ?] = NoOpVisitor
  override def visitKeyValue(v: Any): Unit = ()
  override def subVisitor: Visitor[?, ?] = NoOpVisitor
  override def visitValue(v: Any, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool, Some(path), None, Some(ctx.currentLoc))
}

object ObjectSchemaValidator {
  def of(schema: ObjectSchema, ctx: Context = Context.Empty, path: JsonPointer = JsonPointer(),
         dynParent: Option[BaseValidator] = None): Visitor[?, OutputUnit] = {

    // TODO: selective vocabs 
    val comp: JsonVisitor[Seq[Nothing], Seq[collection.Seq[OutputUnit]]] =
      new PeekCompositeVisitor(u => ctx.add(u), () => ctx.clear(), // registers/clears sibling annotations
        vocab.Core(schema, ctx, path, dynParent), // Vis[Seq[Nothing], Seq[OUnit]]
        vocab.Applicator(schema, ctx, path, dynParent), // Vis[Any, Seq[OUnit]]
        vocab.Validation(schema, ctx, path, dynParent), // Vis[Nothing, Seq[OUnit]]
        vocab.Unevaluated(schema, ctx, path, dynParent)
      )

    new MapReader[Seq[Nothing], Seq[collection.Seq[OutputUnit]], OutputUnit](comp) {
      override def mapNonNullsFunction(v: Seq[collection.Seq[OutputUnit]]): OutputUnit =
        ctx.struct.compose(path, v.flatten, ctx)
    }
  }
}
