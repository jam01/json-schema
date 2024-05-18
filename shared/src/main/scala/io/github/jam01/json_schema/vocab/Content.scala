package io.github.jam01.json_schema.vocab

import io.github.jam01.json_schema._

abstract class Content(schema: ObjectSchema,
                       ctx: Context = Context.Empty,
                       path: JsonPointer = JsonPointer(),
                       dynParent: Option[VocabValidator] = None) extends VocabValidator(schema, ctx, path, dynParent) {
  }
