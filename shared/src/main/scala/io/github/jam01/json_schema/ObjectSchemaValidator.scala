package io.github.jam01.json_schema

abstract class ObjectSchemaValidator(val schema: ObjectSchema,
                                     val schloc: JsonPointer = JsonPointer(),
                                     val ctx: Context = Context.empty,
                                     val dynParent: Option[ObjectSchemaValidator] = None)
  extends JsonVisitor[_, Boolean] {
}

object ObjectSchemaValidator {
  def validate(schema: ObjectSchema,
               schloc: JsonPointer = JsonPointer(),
               ctx: Context = Context.empty,
               dynParent: Option[ObjectSchemaValidator] = None): JsonVisitor[_, Boolean] = {

    new CompositeVisitorReducer(_.forall(identity), Seq(vocab.Core(schema, schloc, ctx, dynParent),
      vocab.ApplicatorValidator(schema, schloc, ctx, dynParent),
      vocab.ValidationValidator(schema, schloc, ctx, dynParent),
      vocab.Format(schema, schloc, ctx, dynParent)): _*)
  }
}
