package io.github.jam01.json_schema

import upickle.core.{Abort, ObjVisitor, Visitor}

trait JsonVisitor[-T, +J] extends Visitor[T, J] {
  def expectedMsg = "expected string, integer, number, object, array, or null;"

  override def visitUInt64(i: Long, index: Int): J = {
    if (i < 0) visitString(java.lang.Long.toUnsignedString(i), index)
    else visitInt64(i, index)
  }

  override def visitInt32(i: Int, index: Int): J =
    visitInt64(i, index)

  override def visitFloat32(f: Float, index: Int): J =
    visitFloat64(f, index)

  override def visitFloat64String(s: String, index: Int): J =
    visitFloat64(java.lang.Double.valueOf(s), index)

  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J =
    visitFloat64(java.lang.Double.valueOf(s.asInstanceOf[String]), index)

  override def visitChar(s: Char, index: Int): J = visitString(s.toString, index)

  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[T, J] = {
    if (jsonableKeys) visitObject(length, index)
    else throw Abort("Object keys must be Strings")
  }

  def visitObject(length: Int, index: Int): ObjVisitor[T, J]

  override def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int): J =
    throw Abort(expectedMsg + " got binary")

  override def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): J =
    throw Abort(expectedMsg + " got Ext")
}
