package io.github.jam01.json_schema

object SchemaValidator {
  def from(sch: Schema, ctx: Context): JsonVisitor[Boolean, Boolean] = {
    sch match
      case bs: BooleanSchema => BooleanSchemaVisitor.of(bs)
      case os: ObjectSchema => ObjectSchemaValidator(os, ctx)
  }
}
