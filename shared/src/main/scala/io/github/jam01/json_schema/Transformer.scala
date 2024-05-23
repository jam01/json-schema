package io.github.jam01.json_schema

import upickle.core.{LinkedHashMap, Visitor}

object Transformer extends upickle.core.Transformer[Value] {
  override def transform[T](o: Value, v: Visitor[?, T]): T = { // see ujson.Value#transform
    o match
      case null | Null => v.visitNull(-1)
      case Bool(bool) => if (bool) v.visitTrue(-1) else v.visitFalse(-1)
      case Num(num) => num match
        case l: Long => v.visitInt64(l, -1)
        case d: Double => v.visitFloat64(d, -1)
      case Str(str) => v.visitString(str, -1)
      case Arr(arr) => val ctx = v.visitArray(arr.size, -1).narrow
        for (item <- arr) ctx.visitValue(transform(item, ctx.subVisitor), -1)
        ctx.visitEnd(-1)
      case Obj(obj) => transformObj(obj, v)
      case sch: Schema => sch match
        case bsch: BooleanSchema => if (bsch.value) v.visitTrue(-1) else v.visitFalse(-1)
        case ObjectSchema(obj, _, _, _) => transformObj(obj, v)
  }

  private def transformObj[T](obj: LinkedHashMap[String, Value], v: Visitor[?, T]) = {
    val ctx = v.visitObject(obj.size, true, -1).narrow
    for (kv <- obj) {
      val keyVisitor = ctx.visitKey(-1)
      ctx.visitKeyValue(keyVisitor.visitString(kv._1, -1))
      ctx.visitValue(transform(kv._2, ctx.subVisitor), -1)
    }
    ctx.visitEnd(-1)
  }
}
