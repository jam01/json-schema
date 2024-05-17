package io.github.jam01.json_schema

/**
 * A Validator that applies a Vocabulary using a given Schema
 *
 * @param schema schema to apply
 * @param ctx validation context
 * @param path the path followed to the given schema
 * @param dynParent dynamic scope parent validator
 */
abstract class VocabValidator(val schema: ObjectSchema,
                              val ctx: Context = Context.empty,
                              val path: JsonPointer = JsonPointer(),
                              val dynParent: Option[VocabValidator] = None)
  extends JsonVisitor[_, Boolean]
