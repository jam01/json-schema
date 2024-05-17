package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*

abstract class Unevaluated(schema: ObjectSchema,
                           ctx: Context = Context.empty,
                           schloc: JsonPointer = JsonPointer(),
                           dynParent: Option[VocabValidator] = None) extends VocabValidator(schema, ctx, schloc, dynParent) {
}
