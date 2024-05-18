package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*

abstract class Unevaluated(schema: ObjectSchema,
                           ctx: Context = Context.Empty,
                           path: JsonPointer = JsonPointer(),
                           dynParent: Option[VocabValidator] = None) extends VocabValidator(schema, ctx, path, dynParent) {
}
