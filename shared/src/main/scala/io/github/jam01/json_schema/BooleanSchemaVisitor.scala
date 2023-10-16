package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

object BooleanSchemaVisitor {  
  val True: BooleanSchemaVisitor = new BooleanSchemaVisitor(true) {
    private val TrueArrVisitor = new BooleanArrVisitor(true) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.True
    }

    private val TrueObjVisitor = new BooleanObjVisitor(true) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.True
      override def visitKey(index: Int): Visitor[_, _] = BooleanSchemaVisitor.True
    }

    override def visitArray(length: Int, index: Int): ArrVisitor[Boolean, Boolean] = TrueArrVisitor
    override def visitObject(length: Int, index: Int): ObjVisitor[Boolean, Boolean] = TrueObjVisitor
  }

  val False: BooleanSchemaVisitor = new BooleanSchemaVisitor(true) {
    private val FalseArrVisitor = new BooleanArrVisitor(false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.False
    }

    private val FalseObjVisitor = new BooleanObjVisitor(false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.False
      override def visitKey(index: Int): Visitor[_, _] = BooleanSchemaVisitor.False
    }

    override def visitArray(length: Int, index: Int): ArrVisitor[Boolean, Boolean] = FalseArrVisitor
    override def visitObject(length: Int, index: Int): ObjVisitor[Boolean, Boolean] = FalseObjVisitor
  }
  
  def of(sch: BooleanSchema): BooleanSchemaVisitor =
    if (sch == BooleanSchema.True) True else False
}

private abstract class BooleanSchemaVisitor(bool: Boolean) extends JsonVisitor[Boolean, Boolean] {
  override def visitNull(index: Int): Boolean = bool

  override def visitFalse(index: Int): Boolean = bool

  override def visitTrue(index: Int): Boolean = bool

  override def visitFloat64(d: Double, index: Int): Boolean = bool

  override def visitInt64(i: Long, index: Int): Boolean = bool

  override def visitString(s: CharSequence, index: Int): Boolean = bool
}

private abstract class BooleanObjVisitor(bool: Boolean) extends ObjVisitor[Boolean, Boolean] {

  override def visitKeyValue(v: Any): Unit = ()

  override def visitValue(v: Boolean, index: Int): Unit = ()

  override def visitEnd(index: Int): Boolean = bool
}

private abstract class BooleanArrVisitor(bool: Boolean) extends ArrVisitor[Boolean, Boolean] {
  override def visitValue(v: Boolean, index: Int): Unit = ()

  override def visitEnd(index: Int): Boolean = bool
}