package io.github.jam01.json_schema

import upickle.core.Visitor

// TODO: consider a custom JSON Schema AST
object Transformer {
  def transform[T](o: Any, v: Visitor[_, T]): T = { // see ujson.Value#transform
    o match
      case null => v.visitNull(-1)
      case s: String => v.visitString(s, -1)
      case l: Int => v.visitInt32(l, -1)
      case l: Long => v.visitInt64(l, -1)
      case d: Float => v.visitFloat32(d, -1)
      case d: Double => v.visitFloat64(d, -1)
      case obj: collection.Map[String, Any] =>  { // consider specialized StringMap[V]-like
        val ctx = v.visitObject(obj.size, true, -1).narrow
        for (kv <- obj) {
          val keyVisitor = ctx.visitKey(-1)
          ctx.visitKeyValue(keyVisitor.visitString(kv._1, -1))
          ctx.visitValue(transform(kv._2, ctx.subVisitor), -1)
        }
        ctx.visitEnd(-1)
      }
      case arr: collection.Seq[Any] => {
        val ctx = v.visitArray(arr.size, -1).narrow
        for (item <- arr) ctx.visitValue(transform(item, ctx.subVisitor), -1)
        ctx.visitEnd(-1)
      }
      case x: Any => throw new IllegalArgumentException(s"unsupported type ${x.getClass.getName}")
  }
}
