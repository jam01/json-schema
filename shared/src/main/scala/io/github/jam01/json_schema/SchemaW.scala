/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

import upickle.core.Visitor

/**
 * JSON Schema writer.
 */
object SchemaW extends upickle.core.Transformer[Value] {
  override def transform[T](o: Value, v: Visitor[?, T]): T = { // see ujson.Value#transform
    o match
      case null | Null => v.visitNull(-1)
      case Bool(bool) => if (bool) v.visitTrue(-1) else v.visitFalse(-1)
      case num: Num => num match
        case Int64(l) => v.visitInt64(l, -1)
        case Float64(d) => v.visitFloat64(d, -1)
        case Int128(i) => v.visitFloat64StringParts(i.toString(), -1, -1, -1)
        case Dec128(d) => v.visitFloat64String(d.toString(), -1)
      case Str(str) => v.visitString(str, -1)
      case Arr(arr) => val ctx = v.visitArray(arr.size, -1).narrow
        for (item <- arr) ctx.visitValue(transform(item, ctx.subVisitor), -1)
        ctx.visitEnd(-1)
      case Obj(map) => transformObj(map, v)
      case sch: Schema => sch match
        case BooleanSchema(bool) => if (bool) v.visitTrue(-1) else v.visitFalse(-1)
        case ObjectSchema(map) => transformObj(map, v)
  }

  private def transformObj[T](obj: collection.Map[String, Value], v: Visitor[?, T]) = {
    val ctx = v.visitObject(obj.size, true, -1).narrow
    for (kv <- obj) {
      val keyVisitor = ctx.visitKey(-1)
      ctx.visitKeyValue(keyVisitor.visitString(kv._1, -1))
      ctx.visitValue(transform(kv._2, ctx.subVisitor), -1)
    }
    ctx.visitEnd(-1)
  }
}
