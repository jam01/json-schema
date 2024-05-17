package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

object SchemaValidator {
  def of(sch: Schema,
         ctx: Context = Context.empty,
         path: JsonPointer = JsonPointer(),
         dynParent: Option[VocabValidator] = None): JsonVisitor[_, Boolean] = {
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

    override def visitArray(length: Int, index: Int): ArrVisitor[Boolean, Boolean] = TrueArrValidator
    override def visitObject(length: Int, index: Int): ObjVisitor[Boolean, Boolean] = TrueObjValidator
  }

  val False: BooleanSchemaValidator = new BooleanSchemaValidator(false) {
    private val FalseArrVisitor = new BooleanArrValidator(false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaValidator.False
    }

    private val FalseObjVisitor = new BooleanObjValidator(false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaValidator.False
      override def visitKey(index: Int): Visitor[_, _] = BooleanSchemaValidator.False
    }

    override def visitArray(length: Int, index: Int): ArrVisitor[Boolean, Boolean] = FalseArrVisitor
    override def visitObject(length: Int, index: Int): ObjVisitor[Boolean, Boolean] = FalseObjVisitor
  }

  def of(bsch: BooleanSchema): BooleanSchemaValidator =
    bsch match
      case TrueSchema => True
      case FalseSchema => False
}

private abstract class BooleanSchemaValidator(bool: Boolean) extends JsonVisitor[Boolean, Boolean] {
  override def visitNull(index: Int): Boolean = bool
  override def visitFalse(index: Int): Boolean = bool
  override def visitTrue(index: Int): Boolean = bool
  override def visitFloat64(d: Double, index: Int): Boolean = bool
  override def visitInt64(i: Long, index: Int): Boolean = bool
  override def visitString(s: CharSequence, index: Int): Boolean = bool
}

private abstract class BooleanObjValidator(bool: Boolean) extends ObjVisitor[Boolean, Boolean] {
  override def visitKeyValue(v: Any): Unit = ()
  override def visitValue(v: Boolean, index: Int): Unit = ()
  override def visitEnd(index: Int): Boolean = bool
}

private abstract class BooleanArrValidator(bool: Boolean) extends ArrVisitor[Boolean, Boolean] {
  override def visitValue(v: Boolean, index: Int): Unit = ()
  override def visitEnd(index: Int): Boolean = bool
}

object ObjectSchemaValidator {
  def of(schema: ObjectSchema, ctx: Context = Context.empty, schloc: JsonPointer = JsonPointer(), dynParent: Option[VocabValidator] = None): JsonVisitor[_, Boolean] = {

    new CompositeVisitorReducer(_.forall(identity), Seq(vocab.Core(schema, ctx, schloc, dynParent),
      vocab.Applicator(schema, ctx, schloc, dynParent),
      vocab.Validation(schema, ctx, schloc, dynParent),
      vocab.Format(schema, ctx, schloc, dynParent)): _*)
  }
}
