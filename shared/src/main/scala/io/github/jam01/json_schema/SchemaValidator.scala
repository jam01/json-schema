package io.github.jam01.json_schema

object SchemaValidator {
  def of(sch: Schema, 
         schLoc: JsonPointer = JsonPointer(), 
         ctx: Context = Context.empty, 
         dynParent: Option[ObjectSchemaValidator] = None): JsonVisitor[_, Boolean] = {
    sch match
      case bs: BooleanSchema => BooleanSchemaValidator.of(bs)
      case os: ObjectSchema => ObjectSchemaValidator(os, schLoc, ctx, dynParent)
  }
}
