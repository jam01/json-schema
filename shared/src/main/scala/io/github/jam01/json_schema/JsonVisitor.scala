/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import upickle.core.{Abort, ObjVisitor, Visitor}

/**
 * A [[Visitor]] specialized to work with JSON types. Forwards the not-JSON-related methods to their JSON equivalents.
 */
trait JsonVisitor[-T, +J] extends Visitor[T, J] {
  protected def expectedMsg = "Expected null, boolean, number, string, object or array"
  override def visitUInt64(i: Long, index: Int): J = {
    if (i < 0) visitFloat64StringParts(java.lang.Long.toUnsignedString(i), -1, -1, index)
    else visitInt64(i, index)
  }

  override def visitInt32(i: Int, index: Int): J =
    visitInt64(i, index)

  override def visitFloat32(f: Float, index: Int): J =
    visitFloat64(f, index)

  override def visitFloat64String(s: String, index: Int): J = {
    visitFloat64StringParts(s, s.indexOf('.'), s.indexWhere(c => (c | 0x20) == 'e'), index)
  }

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
