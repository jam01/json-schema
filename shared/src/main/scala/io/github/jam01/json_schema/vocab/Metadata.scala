package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema.*

abstract class Metadata(schema: ObjectSchema,
                        ctx: Context,
                        path: JsonPointer,
                        dynParent: Option[VocabBase]) extends VocabBase(schema, ctx, path, dynParent) {
}
