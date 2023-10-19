package io.github.jam01.json_schema

trait Schema { }

enum BooleanSchema extends Schema {
  case True, False
}

object BooleanSchema {
  def of(bool: Boolean): BooleanSchema = if (bool) True else False
}

object Schema {
  def getSchemaByPointer(sch: Schema, ptr: JsonPointer): Any = {
    if (ptr.refTokens.isEmpty) return sch

    sch match
      case schema: BooleanSchema => throw new IllegalStateException("cannot evaluate a JSON pointer against a boolean schema")
      case objSch: ObjectSchema => objSch.getSchemaByPointer(ptr)
  }
}
