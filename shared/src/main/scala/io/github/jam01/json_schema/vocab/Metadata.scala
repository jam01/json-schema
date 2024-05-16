package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema._

abstract class Metadata(schema: ObjectSchema,
                        schloc: JsonPointer = JsonPointer(),
                        ctx: Context = Context.empty,
                        dynParent: Option[ObjectSchemaValidator] = None) 
  extends ObjectSchemaValidator(schema, schloc, ctx, dynParent) {
}
