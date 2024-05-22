package io.github.jam01.json_schema

import io.github.jam01.json_schema.vocab.Core
import upickle.core.Visitor.MapReader
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

object SchemaValidator {
  def of(sch: Schema,
         ctx: Context = Context.Empty,
         path: JsonPointer = JsonPointer(),
         dynParent: Option[BaseValidator] = None): Visitor[_, OutputUnit] = {
    sch match
      case bs: BooleanSchema => BooleanSchemaValidator.of(bs)
      case os: ObjectSchema => ObjectSchemaValidator.of(os, ctx, path, dynParent)
  }
}

object BooleanSchemaValidator {
  val True: BooleanSchemaValidator = new BooleanSchemaValidator(true) {
    private val TrueArrValidator = new BooleanArrValidator(true) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaValidator.True
    }

    private val TrueObjValidator = new BooleanObjValidator(true) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaValidator.True
      override def visitKey(index: Int): Visitor[_, _] = BooleanSchemaValidator.True
    }

    override def visitArray(length: Int, index: Int): ArrVisitor[OutputUnit, OutputUnit] = TrueArrValidator
    override def visitObject(length: Int, index: Int): ObjVisitor[OutputUnit, OutputUnit] = TrueObjValidator
  }

  val False: BooleanSchemaValidator = new BooleanSchemaValidator(false) {
    private val FalseArrVisitor = new BooleanArrValidator(false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaValidator.False
    }

    private val FalseObjVisitor = new BooleanObjValidator(false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaValidator.False
      override def visitKey(index: Int): Visitor[_, _] = BooleanSchemaValidator.False
    }

    override def visitArray(length: Int, index: Int): ArrVisitor[OutputUnit, OutputUnit] = FalseArrVisitor
    override def visitObject(length: Int, index: Int): ObjVisitor[OutputUnit, OutputUnit] = FalseObjVisitor
  }

  def of(bsch: BooleanSchema): BooleanSchemaValidator =
    bsch match
      case TrueSchema => True
      case FalseSchema => False
}

private abstract class BooleanSchemaValidator(bool: Boolean) extends JsonVisitor[OutputUnit, OutputUnit] {
  override def visitNull(index: Int): OutputUnit = OutputUnit(bool)
  override def visitFalse(index: Int): OutputUnit = OutputUnit(bool)
  override def visitTrue(index: Int): OutputUnit = OutputUnit(bool)
  override def visitFloat64(d: Double, index: Int): OutputUnit = OutputUnit(bool)
  override def visitInt64(i: Long, index: Int): OutputUnit = OutputUnit(bool)
  override def visitString(s: CharSequence, index: Int): OutputUnit = OutputUnit(bool)
}

private abstract class BooleanObjValidator(bool: Boolean) extends ObjVisitor[OutputUnit, OutputUnit] {
  override def visitKeyValue(v: Any): Unit = ()
  override def visitValue(v: OutputUnit, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool)
}

private abstract class BooleanArrValidator(bool: Boolean) extends ArrVisitor[OutputUnit, OutputUnit] {
  override def visitValue(v: OutputUnit, index: Int): Unit = ()
  override def visitEnd(index: Int): OutputUnit = OutputUnit(bool)
}

object ObjectSchemaValidator {
  def of(schema: ObjectSchema, ctx: Context = Context.Empty, path: JsonPointer = JsonPointer(),
         dynParent: Option[BaseValidator] = None): Visitor[_, OutputUnit] = {

    // TODO: selective vocabs 
    val comp: JsonVisitor[Seq[Any], Seq[collection.Seq[OutputUnit]]] = 
      new CompositeVisitor(vocab.Core(schema, ctx, path, dynParent),
        vocab.Applicator(schema, ctx, path, dynParent),
        vocab.Validation(schema, ctx, path, dynParent))

    new MapReader[Seq[Any], Seq[collection.Seq[OutputUnit]], OutputUnit](comp) {
      override def mapNonNullsFunction(v: Seq[collection.Seq[OutputUnit]]): OutputUnit = {
        val units = v.flatten
        if (units.map(_.valid).forall(identity)) {
          OutputUnit(true, Some(path), None, Some(ctx.currentLoc), None, Nil, None, Nil)
        } else {
          OutputUnit(false, Some(path), None, Some(ctx.currentLoc), None, units.filterNot(_.valid), None, Nil)
        }
      }
    }
  }
}
