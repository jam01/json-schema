package io.github.jam01.json_schema

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

object BooleanSchemaVisitor {
  val True: BooleanSchemaVisitor[Any] = new BooleanSchemaVisitor[Any](true) {
    private val TrueArrVisitor = new BooleanArrVisitor[Any](true) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.True
    }

    private val TrueObjVisitor = new BooleanObjVisitor[Any](true) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.True
      override def visitKey(index: Int): Visitor[_, _] = BooleanSchemaVisitor.True
    }

    override def visitArray(length: Int, index: Int): ArrVisitor[Any, Boolean] = TrueArrVisitor
    override def visitObject(length: Int, index: Int): ObjVisitor[Any, Boolean] = TrueObjVisitor
  }

  val False: BooleanSchemaVisitor[Any] = new BooleanSchemaVisitor[Any](true) {
    private val FalseArrVisitor = new BooleanArrVisitor[Any](false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.False
    }

    private val FalseObjVisitor = new BooleanObjVisitor[Any](false) {
      override def subVisitor: Visitor[_, _] = BooleanSchemaVisitor.False
      override def visitKey(index: Int): Visitor[_, _] = BooleanSchemaVisitor.False
    }

    override def visitArray(length: Int, index: Int): ArrVisitor[Any, Boolean] = FalseArrVisitor
    override def visitObject(length: Int, index: Int): ObjVisitor[Any, Boolean] = FalseObjVisitor
  }
}

private abstract class BooleanSchemaVisitor[T](bool: Boolean) extends JSONVisitor[T, Boolean] {
  override def visitNull(index: Int): Boolean = bool

  override def visitFalse(index: Int): Boolean = bool

  override def visitTrue(index: Int): Boolean = bool

  override def visitFloat64(d: Double, index: Int): Boolean = bool

  override def visitInt64(i: Long, index: Int): Boolean = bool

  override def visitString(s: CharSequence, index: Int): Boolean = bool
}

private abstract class BooleanObjVisitor[T](bool: Boolean) extends ObjVisitor[T, Boolean] {

  override def visitKeyValue(v: Any): Unit = ()

  override def visitValue(v: T, index: Int): Unit = ()

  override def visitEnd(index: Int): Boolean = bool
}

private abstract class BooleanArrVisitor[T](bool: Boolean) extends ArrVisitor[T, Boolean] {
  override def visitValue(v: T, index: Int): Unit = ()

  override def visitEnd(index: Int): Boolean = bool
}