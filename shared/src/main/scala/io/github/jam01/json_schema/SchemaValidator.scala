package io.github.jam01.json_schema

object SchemaValidator {
  def of(sch: Schema, schLoc: JsonPointer, ctx: Context): JsonVisitor[_, Boolean] = {
    sch match
      case bs: BooleanSchema => BooleanSchemaValidator.of(bs)
      case os: ObjectSchema => ObjectSchemaValidator(os, schLoc, ctx)
  }
}
